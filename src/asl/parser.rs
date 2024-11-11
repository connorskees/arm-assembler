use core::str;

use crate::{
    AstIf, AstIfClause, BitAccess, Case, CaseClause, Expr, Field, InstructionDefinition,
    InstructionEncoding, InstructionSet, Stmt, When, WhenBody, WhenClause,
};

use super::lexer::{AslLexer, Token};

pub struct AslParser<'a> {
    tokens: AslLexer<'a>,
}

impl<'a> AslParser<'a> {
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            tokens: AslLexer::new(buffer),
        }
    }

    pub fn parse_toplevel_case(buffer: &'a [u8]) -> anyhow::Result<Case<'a>> {
        let mut parser = Self::new(buffer);

        parser.expect_token(Token::Decode)?;
        parser.expect_token(Token::A64)?;

        parser.expect_token(Token::Indent)?;

        let case = parser.parse_case()?;

        parser.expect_token(Token::Deindent)?;

        Ok(case)
    }

    fn consume_if_exists(&mut self, token: Token<'a>) -> bool {
        if self.tokens.peek() == Some(token) {
            self.tokens.next();
            true
        } else {
            false
        }
    }

    #[track_caller]
    fn expect_token(&mut self, token: Token<'a>) -> anyhow::Result<()> {
        let actual = self.tokens.next();

        if actual.as_ref() != Some(&token) {
            self.tokens.debug_print();
            panic!("expected {token:?}, found {actual:?}");
        }

        Ok(())
    }

    fn parse_u8(&mut self) -> anyhow::Result<u8> {
        let Some(Token::IntegerLit(int)) = self.tokens.next() else {
            todo!()
        };

        Ok(u8::from_str_radix(int, 10)?)
    }

    fn parse_bit_access(&mut self) -> anyhow::Result<BitAccess> {
        let start = self.parse_u8()?;
        self.expect_token(Token::BitSlice)?;
        let len = self.parse_u8()?;
        Ok(BitAccess { start, len })
    }

    fn expect_binary_integer(&mut self) -> anyhow::Result<&'a str> {
        match self.tokens.next() {
            Some(Token::BinaryInteger(str)) => Ok(str),
            _ => todo!(),
        }
    }

    #[track_caller]
    fn expect_ident(&mut self) -> anyhow::Result<&'a str> {
        match self.tokens.next() {
            Some(Token::Ident(str)) => Ok(str),
            tok => todo!("expected ident: {tok:?}"),
        }
    }

    fn parse_when_body_single_line(&mut self) -> anyhow::Result<WhenBody<'a>> {
        Ok(match self.tokens.next() {
            Some(Token::Encoding) => {
                let mut first = self.expect_ident()?;
                self.tokens.cursor += 4;
                let mut second = self.expect_ident()?;

                if first.starts_with("aarch64") {
                    std::mem::swap(&mut first, &mut second);
                }

                WhenBody::Encoding {
                    mnemonic: first,
                    name: second,
                }
            }
            Some(Token::Unallocated) => WhenBody::Unallocated,
            Some(Token::Unpredictable) => WhenBody::Unpredictable,
            _ => todo!(),
        })
    }

    fn parse_field(&mut self) -> anyhow::Result<Field<'a>> {
        self.expect_token(Token::Field)?;
        let name = self.expect_ident()?;
        let bit_access = self.parse_bit_access()?;

        Ok(Field { name, bit_access })
    }

    fn parse_when_body_multiline(&mut self) -> anyhow::Result<WhenBody<'a>> {
        let mut fields = Vec::new();

        while self.tokens.peek().is_some_and(|tok| tok == Token::Field) {
            fields.push(self.parse_field()?);
        }

        let case = self.parse_case()?;

        Ok(WhenBody::Case {
            fields,
            case: Box::new(case),
        })
    }

    fn parse_when_clause(&mut self) -> anyhow::Result<WhenClause<'a>> {
        Ok(match self.tokens.peek() {
            Some(Token::Underscore) => {
                self.tokens.next();
                WhenClause::Any
            }
            Some(Token::Bang) => {
                self.tokens.next();
                WhenClause::Not(self.expect_binary_integer()?)
            }
            Some(Token::BinaryInteger(int)) => {
                self.tokens.next();
                WhenClause::Concrete(int)
            }
            tok => {
                self.tokens.debug_print();
                todo!("{tok:?}")
            }
        })
    }

    fn parse_when(&mut self) -> anyhow::Result<When<'a>> {
        self.expect_token(Token::When)?;
        self.expect_token(Token::OpenParen)?;

        let mut clauses = Vec::new();

        let has_clauses = !self.consume_if_exists(Token::CloseParen);

        if has_clauses {
            loop {
                clauses.push(self.parse_when_clause()?);

                match self.tokens.next() {
                    Some(Token::Comma) => continue,
                    Some(Token::CloseParen) => break,
                    _ => todo!(),
                }
            }
        }

        self.expect_token(Token::Arrow)?;

        let body = if self.consume_if_exists(Token::Indent) {
            let b = self.parse_when_body_multiline()?;
            self.expect_token(Token::Deindent)?;
            b
        } else {
            self.parse_when_body_single_line()?
        };

        Ok(When { clauses, body })
    }

    fn parse_case_clause(&mut self) -> anyhow::Result<CaseClause<'a>> {
        Ok(match self.tokens.peek() {
            Some(Token::IntegerLit(..)) => CaseClause::BitAccess(self.parse_bit_access().unwrap()),
            Some(Token::Ident(ident)) => {
                self.tokens.next();
                CaseClause::Field(ident)
            }
            _ => todo!(),
        })
    }

    fn parse_case(&mut self) -> anyhow::Result<Case<'a>> {
        self.expect_token(Token::Case)?;
        self.expect_token(Token::OpenParen)?;

        let mut clauses = Vec::new();

        let has_clauses = !self.consume_if_exists(Token::CloseParen);

        if has_clauses {
            loop {
                clauses.push(self.parse_case_clause()?);

                match self.tokens.next() {
                    Some(Token::Comma) => continue,
                    Some(Token::CloseParen) => break,
                    tok => todo!("{tok:?}"),
                }
            }
        }

        self.expect_token(Token::Of)?;
        self.expect_token(Token::Indent)?;

        let mut when = Vec::new();

        while !self.consume_if_exists(Token::Deindent) {
            when.push(self.parse_when()?);
        }

        Ok(Case { clauses, when })
    }

    fn expect_instruction_set(&mut self) -> anyhow::Result<InstructionSet> {
        Ok(match self.tokens.next() {
            Some(Token::A64) => InstructionSet::A64,
            Some(Token::A32) => InstructionSet::A32,
            Some(Token::T32) => InstructionSet::T32,
            Some(Token::T16) => InstructionSet::T16,
            tok => todo!("expected instruction set: {tok:?}"),
        })
    }

    fn parse_block(&mut self) -> anyhow::Result<Vec<Token<'a>>> {
        let mut toks = Vec::new();
        let mut count = 1;

        self.expect_token(Token::Indent)?;

        while let Some(tok) = self.tokens.next() {
            match tok {
                Token::Indent => count += 1,
                Token::Deindent if count == 1 => break,
                Token::Deindent => count -= 1,
                _ => {}
            }

            toks.push(tok);
        }

        Ok(toks)
    }

    fn parse_instruction_encoding(&mut self) -> anyhow::Result<InstructionEncoding<'a>> {
        self.expect_token(Token::Encoding)?;
        let name = self.expect_ident()?;

        self.expect_token(Token::Indent)?;
        self.expect_token(Token::InstructionSet)?;

        let inst_set = self.expect_instruction_set()?;

        let mut fields = Vec::new();

        while self.consume_if_exists(Token::Field) {
            let name = self.expect_ident()?;
            let bit_access = self.parse_bit_access()?;

            fields.push(Field { name, bit_access });
        }

        self.expect_token(Token::Opcode)?;

        let opcode = self.expect_binary_integer()?;

        self.expect_token(Token::Guard)?;

        let guard = self.tokens.tokens_until_newline();

        let mut unpredictable_unless = Vec::new();

        while self.consume_if_exists(Token::UnpredictableUnless) {
            let body = self.tokens.tokens_until_newline();

            unpredictable_unless.push(body);
        }

        self.expect_token(Token::Decode)?;

        let decode = self.parse_block()?;

        self.expect_token(Token::Deindent)?;

        Ok(InstructionEncoding {
            name,
            inst_set,
            fields,
            opcode,
            guard,
            unpredictable_unless,
            decode,
        })
    }

    pub fn parse_instruction_file(
        buffer: &'a [u8],
    ) -> anyhow::Result<Vec<InstructionDefinition<'a>>> {
        let mut parser = Self::new(buffer);

        let mut insts = Vec::new();

        while parser.tokens.peek().is_some() {
            insts.push(parser.parse_instruction()?);
        }

        Ok(insts)
    }

    fn parse_expr(&mut self) -> anyhow::Result<Expr<'a>> {
        todo!()
    }

    fn parse_stmt_block(&mut self) -> anyhow::Result<Vec<Stmt<'a>>> {
        if self.consume_if_exists(Token::OpenCurlyBrace) {
            todo!()
        } else {
            todo!()
        }
    }

    fn parse_if_stmt(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::If)?;

        let cond = self.parse_expr()?;

        let mut if_clauses = Vec::new();

        self.expect_token(Token::Then)?;

        if_clauses.push(AstIfClause {
            cond,
            body: self.parse_stmt_block()?,
        });

        while self.consume_if_exists(Token::ElseIf) {
            let cond = self.parse_expr()?;
            self.expect_token(Token::Then)?;
            if_clauses.push(AstIfClause {
                cond,
                body: self.parse_stmt_block()?,
            });
        }

        let else_clause = if self.consume_if_exists(Token::Else) {
            Some(self.parse_stmt_block()?)
        } else {
            None
        };

        Ok(Stmt::If(AstIf {
            if_clauses,
            else_clause,
        }))
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt<'a>> {
        match self.tokens.peek() {
            Some(Token::If) => self.parse_if_stmt(),
            Some(Token::For) => todo!(),
            Some(Token::Ident(..)) => todo!(),
            Some(Token::IntegerKeyword) => todo!(),
            Some(Token::Boolean) => todo!(),
            Some(Token::Case) => todo!(),
            Some(Token::Assert) => todo!(),
            Some(Token::Return) => todo!(),
            Some(Token::Constant) => todo!(),
            Some(Token::Array) => todo!(),
            Some(Token::Undefined) => todo!(),
            _ => todo!(),
        }
    }

    fn parse_instruction(&mut self) -> anyhow::Result<InstructionDefinition<'a>> {
        self.expect_token(Token::Instruction)?;

        let name = self.expect_ident()?;

        let mut encodings = Vec::new();

        self.expect_token(Token::Indent)?;

        while self.tokens.peek().is_some_and(|tok| tok == Token::Encoding) {
            encodings.push(self.parse_instruction_encoding()?);
        }

        let postdecode = if self.consume_if_exists(Token::PostDecode) {
            self.parse_block()?
        } else {
            Vec::new()
        };

        self.expect_token(Token::Execute)?;
        let conditional = self.consume_if_exists(Token::Conditional);

        let execute = if self.tokens.peek() == Some(Token::Indent) {
            self.parse_block()?
        } else {
            Vec::new()
        };

        self.expect_token(Token::Deindent)?;

        Ok(InstructionDefinition {
            name,
            conditional,
            encodings,
            postdecode,
            execute,
        })
    }
}
