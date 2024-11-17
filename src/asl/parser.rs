use core::str;

use super::arch_decode::*;

use crate::{
    AstCase, AstFor, AstIf, AstIfClause, AstWhen, AstWhile, BinOp, Bits, Expr,
    InstructionDefinition, InstructionEncoding, InstructionSet, Stmt, UnaryOp, VariableType,
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

        Ok(u8::try_from(int)?)
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

    fn expect_integer(&mut self) -> anyhow::Result<u64> {
        match self.tokens.next() {
            Some(Token::IntegerLit(val)) => Ok(val),
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

        while !self.tokens.at_end() && !self.consume_if_exists(Token::Deindent) {
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

    fn parse_instruction_encoding(&mut self) -> anyhow::Result<InstructionEncoding<'a>> {
        self.expect_token(Token::Encoding)?;
        let name = self.expect_ident()?;

        self.expect_token(Token::Indent)?;
        self.expect_token(Token::InstructionSet)?;

        let inst_set = self.expect_instruction_set()?;

        let mut fields = Vec::new();

        while !self.tokens.at_end() && self.consume_if_exists(Token::Field) {
            let name = self.expect_ident()?;
            let bit_access = self.parse_bit_access()?;

            fields.push(Field { name, bit_access });
        }

        self.expect_token(Token::Opcode)?;

        let opcode = self.expect_binary_integer()?;

        self.expect_token(Token::Guard)?;

        let guard = self.parse_expr()?;

        let mut unpredictable_unless = Vec::new();

        while !self.tokens.at_end() && self.consume_if_exists(Token::UnpredictableUnless) {
            let body = self.parse_expr()?;
            // let body = self.tokens.tokens_until_newline();

            unpredictable_unless.push(body);
        }

        self.expect_token(Token::Decode)?;

        let decode = self.parse_stmt_block()?;

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

    fn parse_single_expr(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut val = match self.tokens.peek() {
            Some(Token::True) => {
                self.tokens.next();
                Expr::True
            }
            Some(Token::False) => {
                self.tokens.next();
                Expr::False
            }
            Some(Token::OpenParen) => {
                self.tokens.next();
                let value = self.parse_expr()?;
                let expr = if self.consume_if_exists(Token::Comma) {
                    let mut values = vec![value];
                    values.append(&mut self.parse_comma_separated(Self::parse_expr)?);
                    Expr::Tuple(values)
                } else {
                    value
                };
                self.expect_token(Token::CloseParen)?;
                expr
            }
            Some(Token::Ident(..)) => self.parse_ident_expr()?,
            Some(Token::IntegerLit(val)) => {
                self.tokens.next();
                Expr::IntegerLit(val)
            }
            Some(Token::BinaryInteger(val)) => {
                self.tokens.next();
                Expr::BinaryInteger(val)
            }
            Some(Token::Bang | Token::BitwiseNot) => {
                self.tokens.next();
                Expr::UnaryOp(UnaryOp::BitwiseNot, Box::new(self.parse_single_expr()?))
            }
            Some(Token::Minus) => {
                self.tokens.next();
                match self.tokens.peek() {
                    Some(Token::Comma | Token::CloseParen) => Expr::Variable("-"),
                    _ => Expr::UnaryOp(UnaryOp::Neg, Box::new(self.parse_single_expr()?)),
                }
            }
            Some(Token::OpenCurlyBrace) => self.parse_array_literal()?,
            Some(Token::Bits) => self.parse_bits_unknown()?,
            Some(Token::If) => self.parse_ternary()?,
            Some(Token::OpenSquareBrace) => self.parse_field_array()?,
            Some(Token::IntegerKeyword) => self.parse_integer_unknown()?,
            tok => {
                self.tokens.debug_print();
                todo!("unimplemented single expr: {tok:?}")
            }
        };

        while self.tokens.peek() == Some(Token::OpenSquareBrace) {
            val = self.parse_index_expr(val)?;
        }

        Ok(val)
    }

    fn parse_comma_separated<T>(
        &mut self,
        func: impl Fn(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<Vec<T>> {
        let mut vals = Vec::new();

        if self.tokens.peek().is_some_and(Self::token_is_expr_end) {
            return Ok(vals);
        }

        self.tokens.ignore_indent = true;

        vals.push(func(self)?);

        while self.consume_if_exists(Token::Comma) {
            vals.push(func(self)?);
        }

        self.tokens.ignore_indent = false;

        Ok(vals)
    }

    fn parse_field_array(&mut self) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::OpenSquareBrace)?;

        let mut args = Vec::new();

        if !self.consume_if_exists(Token::CloseSquareBrace) {
            args = self.parse_comma_separated(Self::parse_expr)?;
            self.expect_token(Token::CloseSquareBrace)?;
        }

        Ok(Expr::FieldArray(args))
    }

    fn parse_ternary(&mut self) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::If)?;
        let cond = Box::new(self.parse_expr()?);
        self.expect_token(Token::Then)?;
        let if_true = Box::new(self.parse_expr()?);
        self.expect_token(Token::Else)?;
        let else_clause = Box::new(self.parse_expr()?);
        Ok(Expr::Ternary {
            cond,
            if_true,
            else_clause,
        })
    }

    fn parse_bits_unknown(&mut self) -> anyhow::Result<Expr<'a>> {
        let bits = self.parse_bits()?;

        self.expect_token(Token::Unknown)?;

        Ok(Expr::BitsUnknown(Box::new(bits)))
    }

    fn parse_integer_unknown(&mut self) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::IntegerKeyword)?;
        self.expect_token(Token::Unknown)?;

        Ok(Expr::IntegerUnknown)
    }

    fn parse_array_literal(&mut self) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::OpenCurlyBrace)?;

        let mut vals = Vec::new();

        while !self.tokens.at_end() && !self.consume_if_exists(Token::CloseCurlyBrace) {
            vals.push(self.parse_expr()?);
            // todo: better handling of comma separated values
            self.consume_if_exists(Token::Comma);
        }

        Ok(Expr::Array(vals))
    }

    fn binop_for_token(tok: Token<'a>) -> Option<BinOp> {
        Some(match tok {
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub,
            Token::Mul => BinOp::Mul,
            Token::Div => BinOp::Div,
            Token::Mod => BinOp::Mod,
            Token::Xor => BinOp::Xor,
            Token::Pow => BinOp::Pow,
            Token::BitwiseAnd => BinOp::BitwiseAnd,
            Token::LogicalAnd => BinOp::LogicalAnd,
            Token::LogicalOr => BinOp::LogicalOr,
            Token::BitwiseOr => BinOp::BitwiseOr,
            Token::ShiftLeft => BinOp::ShiftLeft,
            Token::ShiftRight => BinOp::ShiftRight,
            Token::DoubleEq => BinOp::Eq,
            Token::NotEq => BinOp::Ne,
            Token::GreaterThan => BinOp::Gt,
            Token::GreaterThanEqual => BinOp::Gte,
            Token::LessThan => BinOp::Lt,
            Token::LessThanEqual => BinOp::Lte,
            Token::Dot => BinOp::Dot,
            Token::In => BinOp::In,
            Token::Colon => BinOp::Slice,
            Token::SingleEq => BinOp::Assignment,
            _ => return None,
        })
    }

    fn token_is_expr_end(tok: Token<'a>) -> bool {
        matches!(
            tok,
            Token::CloseParen
                | Token::CloseSquareBrace
                | Token::CloseCurlyBrace
                | Token::SemiColon
                | Token::Indent
                | Token::Deindent
                | Token::Comma
                | Token::To
                | Token::Of
                | Token::Else
                | Token::For
                | Token::While
                | Token::Do
                | Token::UnpredictableUnless
                | Token::Decode
                | Token::Then
        )
    }

    fn parse_expr(&mut self) -> anyhow::Result<Expr<'a>> {
        let lhs = self.parse_single_expr()?;

        if let Some(binop) = self.tokens.peek().and_then(Self::binop_for_token) {
            self.tokens.next();
            return Ok(Expr::BinOp(
                Box::new(lhs),
                binop,
                Box::new(self.parse_expr()?),
            ));
        }

        if self.tokens.peek().is_some_and(Self::token_is_expr_end) {
            return Ok(lhs);
        }

        Ok(match self.tokens.peek() {
            None => lhs,
            tok => {
                self.tokens.debug_print();
                todo!("expr: {tok:?}")
            }
        })
    }

    fn parse_stmt_block(&mut self) -> anyhow::Result<Vec<Stmt<'a>>> {
        if !self.consume_if_exists(Token::Indent) {
            let stmt = if let Some(stmt) = self.parse_single_stmt()? {
                vec![stmt]
            } else {
                Vec::new()
            };
            self.consume_if_exists(Token::SemiColon);
            return Ok(stmt);
        }

        let mut stmts = Vec::new();

        while !self.tokens.at_end() && !self.consume_if_exists(Token::Deindent) {
            if let Some(stmt) = self.parse_single_stmt()? {
                stmts.push(stmt);
            }
            self.consume_if_exists(Token::SemiColon);
        }

        Ok(stmts)
    }

    fn parse_for_stmt(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::For)?;

        let var = self.expect_ident()?;

        self.expect_token(Token::SingleEq)?;

        let init = self.parse_expr()?;

        self.expect_token(Token::To)?;

        let to = self.parse_expr()?;

        let body = self.parse_stmt_block()?;

        Ok(Stmt::For(AstFor {
            var,
            init,
            to,
            body,
        }))
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

        while !self.tokens.at_end() && self.consume_if_exists(Token::ElseIf) {
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

    fn parse_assert(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Assert)?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Assert(expr))
    }

    fn parse_return(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Return)?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Return(expr))
    }

    fn parse_func_call(&mut self, name: &'a str) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::OpenParen)?;
        let args = self.parse_comma_separated(Self::parse_expr)?;
        self.expect_token(Token::CloseParen)?;
        Ok(Expr::FnCall { name, args })
    }

    fn parse_index_expr(&mut self, val: Expr<'a>) -> anyhow::Result<Expr<'a>> {
        self.expect_token(Token::OpenSquareBrace)?;

        let mut args = Vec::new();

        if !self.consume_if_exists(Token::CloseSquareBrace) {
            while !self.tokens.at_end() && !self.consume_if_exists(Token::CloseSquareBrace) {
                args.push(self.parse_expr()?);
                self.consume_if_exists(Token::Comma);
            }
        }

        let expr = Expr::Index(Box::new(val), args);

        if self
            .tokens
            .peek()
            .is_some_and(|t| t == Token::OpenSquareBrace)
        {
            return self.parse_index_expr(expr);
        }

        Ok(expr)
    }

    fn parse_ident_expr(&mut self) -> anyhow::Result<Expr<'a>> {
        let ident = self.expect_ident()?;

        if self.tokens.peek().is_some_and(Self::token_is_expr_end) {
            return Ok(Expr::Variable(ident));
        }

        if self.tokens.peek().and_then(Self::binop_for_token).is_some() {
            return Ok(Expr::Variable(ident));
        }

        match self.tokens.peek() {
            Some(Token::OpenParen) => self.parse_func_call(ident),
            Some(Token::OpenSquareBrace) => self.parse_index_expr(Expr::Variable(ident)),
            Some(Token::Ident(..) | Token::Undefined) => Ok(Expr::Variable(ident)),
            tok => {
                self.tokens.debug_print();
                todo!("ident expr: {tok:?}")
            }
        }
    }

    fn parse_bits(&mut self) -> anyhow::Result<Bits<'a>> {
        self.expect_token(Token::Bits)?;
        self.expect_token(Token::OpenParen)?;
        let n = self.parse_expr()?;
        self.expect_token(Token::CloseParen)?;
        Ok(Bits { n })
    }

    fn expect_variable_type(&mut self) -> anyhow::Result<VariableType<'a>> {
        Ok(match self.tokens.peek() {
            Some(Token::IntegerKeyword) => {
                self.tokens.next();
                VariableType::Integer
            }
            Some(Token::Boolean) => {
                self.tokens.next();
                VariableType::Boolean
            }
            Some(Token::Bit) => {
                self.tokens.next();
                VariableType::Bit
            }
            Some(Token::Bits) => VariableType::Bits(self.parse_bits()?),
            Some(Token::Ident(v)) => VariableType::Ident(v),
            _ => todo!(),
        })
    }

    fn parse_declaration(&mut self) -> anyhow::Result<Stmt<'a>> {
        let ty = self.expect_variable_type()?;
        let mut idents = vec![self.expect_ident()?];
        let rhs = if self.consume_if_exists(Token::SingleEq) {
            Some(self.parse_expr()?)
        } else if self.consume_if_exists(Token::Comma) {
            idents.append(&mut self.parse_comma_separated(Self::expect_ident)?);
            self.consume_if_exists(Token::SemiColon);
            None
        } else {
            self.consume_if_exists(Token::SemiColon);
            None
        };

        Ok(Stmt::Declaration(ty, idents, rhs))
    }

    fn parse_when_stmt(&mut self) -> anyhow::Result<AstWhen<'a>> {
        self.expect_token(Token::When)?;

        let clause = self.parse_single_expr()?;
        let body = if self.tokens.peek() == Some(Token::When) {
            Vec::new()
        } else {
            let mut b = Vec::new();
            let has_indent = self.consume_if_exists(Token::Indent);
            while !self.tokens.at_end()
                && self.tokens.peek() != Some(Token::When)
                && self.tokens.peek() != Some(Token::Deindent)
                && self.tokens.peek() != Some(Token::Otherwise)
            {
                if let Some(v) = self.parse_single_stmt()? {
                    b.push(v);
                }
            }

            if has_indent {
                self.expect_token(Token::Deindent)?;
            }

            b
            // self.parse_stmt_block()?
        };

        Ok(AstWhen { clause, body })
    }

    fn parse_otherwise(&mut self) -> anyhow::Result<Vec<Stmt<'a>>> {
        self.expect_token(Token::Otherwise)?;
        if self.tokens.peek() == Some(Token::Indent) {
            self.parse_stmt_block()
        } else {
            let mut block = Vec::new();

            while !self.tokens.at_end() && self.tokens.peek() != Some(Token::Deindent) {
                if let Some(stmt) = self.parse_single_stmt()? {
                    block.push(stmt);
                }
                self.consume_if_exists(Token::SemiColon);
            }
            Ok(block)
        }
    }

    fn parse_see(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::See)?;
        let v = self.parse_expr()?;
        Ok(Stmt::See(v))
    }

    fn parse_case_stmt(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Case)?;

        let clause = self.parse_expr()?;

        self.expect_token(Token::Of)?;
        self.expect_token(Token::Indent)?;

        let mut when = Vec::new();

        while self.tokens.peek() == Some(Token::When) {
            when.push(self.parse_when_stmt()?);
        }

        let otherwise = if self.tokens.peek() == Some(Token::Otherwise) {
            Some(self.parse_otherwise()?)
        } else {
            None
        };

        self.expect_token(Token::Deindent)?;

        Ok(Stmt::Case(AstCase {
            clause,
            when,
            otherwise,
        }))
    }

    fn parse_single_stmt(&mut self) -> anyhow::Result<Option<Stmt<'a>>> {
        let val = match self.tokens.peek() {
            Some(Token::If) => self.parse_if_stmt(),
            Some(Token::For) => self.parse_for_stmt(),
            Some(
                Token::IntegerKeyword
                | Token::Boolean
                | Token::Bits
                | Token::Bit
                // todo: parse types correctly
                | Token::Ident("FPRounding")
                | Token::Ident("AccType")
                | Token::Ident("SystemHintOp")
                | Token::Ident("CompareOp")
                | Token::Ident("MemAtomicOp")
                | Token::Ident("BranchType")
                | Token::Ident("SVECmp")
                | Token::Ident("MemOp")
                | Token::Ident("FPMaxMinOp")
                | Token::Ident("CountOp")
                | Token::Ident("ExtendType")
                | Token::Ident("ShiftType")
                | Token::Ident("ImmediateOp")
                | Token::Ident("LogicalOp")
                | Token::Ident("FPUnaryOp")
                | Token::Ident("ReduceOp")
                | Token::Ident("FPConvOp")
                | Token::Ident("PSTATEField")
                | Token::Ident("MoveWideOp")
                | Token::Ident("VBitOp")
                | Token::Ident("Constraint"),
            ) => self.parse_declaration(),
            Some(Token::Ident(..)) => Ok(Stmt::Expr(self.parse_expr()?)),
            Some(Token::Case) => self.parse_case_stmt(),
            Some(Token::Assert) => self.parse_assert(),
            Some(Token::Return) => self.parse_return(),
            Some(Token::Constant) => self.parse_const_decl(),
            Some(Token::Array) => self.parse_array_decl(),
            Some(Token::Undefined | Token::Unpredictable) => {
                self.tokens.next();
                Ok(Stmt::Expr(Expr::Undefined))
            }
            Some(Token::OpenParen) => self.parse_tuple_assignment(),
            Some(Token::OpenSquareBrace) => self.parse_array_assignment(),
            Some(Token::See) => self.parse_see(),
            Some(Token::SemiColon) | None => return Ok(None),
            Some(Token::Minus) => {
                // todo: can this be less of a special case? anywhere this breaks down?
                self.tokens.next();
                self.expect_token(Token::SingleEq)?;
                let val = self.parse_expr()?;
                self.consume_if_exists(Token::SemiColon);
                Ok(Stmt::Expr(Expr::BinOp(
                    Box::new(Expr::Variable("-")),
                    BinOp::Assignment,
                    Box::new(val),
                )))
            }
            Some(Token::While) => self.parse_while(),
            Some(Token::Enumeration) => self.parse_enum(),
            tok => {
                self.tokens.debug_print();
                todo!("{tok:?}")
            }
        };

        self.consume_if_exists(Token::SemiColon);

        Ok(Some(val?))
    }

    fn parse_enum(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Enumeration)?;
        let name = self.expect_ident()?;
        self.expect_token(Token::OpenCurlyBrace)?;
        let variants = self.parse_comma_separated(Self::expect_ident)?;
        self.expect_token(Token::CloseCurlyBrace)?;
        self.expect_token(Token::SemiColon)?;
        Ok(Stmt::Enum { name, variants })
    }

    fn parse_while(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::While)?;
        let cond = self.parse_expr()?;
        self.expect_token(Token::Do)?;
        let body = self.parse_stmt_block()?;
        Ok(Stmt::While(AstWhile { cond, body }))
    }

    fn parse_tuple(&mut self) -> anyhow::Result<Vec<Expr<'a>>> {
        self.expect_token(Token::OpenParen)?;
        let vals = self.parse_comma_separated(Self::parse_expr)?;
        self.expect_token(Token::CloseParen)?;
        Ok(vals)
    }

    fn parse_array_decl(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Array)?;
        self.expect_token(Token::OpenSquareBrace)?;
        let start = self.expect_integer()?;
        self.expect_token(Token::Dot)?;
        self.expect_token(Token::Dot)?;
        let end = self.expect_integer()?;
        self.expect_token(Token::CloseSquareBrace)?;
        self.expect_token(Token::Of)?;
        let of = self.parse_bits()?;
        let name = self.expect_ident()?;

        Ok(Stmt::ArrayDecl {
            start,
            end,
            of,
            name,
        })
    }

    fn parse_const_decl(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::Constant)?;
        let ty = self.expect_variable_type()?;
        let name = self.expect_ident()?;
        self.expect_token(Token::SingleEq)?;
        let val = self.parse_expr()?;
        self.expect_token(Token::SemiColon)?;

        Ok(Stmt::ConstDecl { ty, name, val })
    }

    fn parse_tuple_assignment(&mut self) -> anyhow::Result<Stmt<'a>> {
        let vars = self.parse_tuple()?;

        self.expect_token(Token::SingleEq)?;

        let val = self.parse_expr()?;

        Ok(Stmt::TupleAssignment { vars, val })
    }

    fn parse_array_assignment(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.expect_token(Token::OpenSquareBrace)?;
        let vars = self.parse_comma_separated(Self::parse_expr)?;
        self.expect_token(Token::CloseSquareBrace)?;

        self.expect_token(Token::SingleEq)?;

        let val = self.parse_expr()?;

        Ok(Stmt::TupleAssignment { vars, val })
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
            self.parse_stmt_block()?
        } else {
            Vec::new()
        };

        self.expect_token(Token::Execute)?;
        let conditional = self.consume_if_exists(Token::Conditional);

        let execute = if self.tokens.peek() == Some(Token::Indent) {
            self.parse_stmt_block()?
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

#[cfg(test)]
mod test {
    use crate::{BinOp, Expr, Stmt};

    use super::AslParser;

    #[test]
    fn parses_add() {
        let mut parser = AslParser::new(b"1 + 2 + 3");
        dbg!(parser.parse_expr().unwrap());

        let mut parser = AslParser::new(b"1 + a + 3");
        dbg!(parser.parse_expr().unwrap());
    }

    #[test]
    fn parses_index_expr() {
        let mut parser = AslParser::new(b"a[1][2]");
        dbg!(parser.parse_expr().unwrap());

        let mut parser = AslParser::new(b"1 + a[1][2] + 3");
        dbg!(parser.parse_expr().unwrap());
    }

    #[test]
    fn parses_slice_expr() {
        let mut parser = AslParser::new(b"a[1][2:3]");
        dbg!(parser.parse_expr().unwrap());
    }

    #[test]
    fn parses_func_call_expr() {
        let mut parser = AslParser::new(b"foo();");
        dbg!(parser.parse_expr().unwrap());
    }

    #[test]
    fn parses_assignment_expr() {
        let mut parser = AslParser::new(b"\n  a = 1;\n  b = 2;");
        assert_eq!(
            parser.parse_stmt_block().unwrap(),
            vec![
                Stmt::Expr(Expr::BinOp(
                    Box::new(Expr::Variable("a")),
                    BinOp::Assignment,
                    Box::new(Expr::IntegerLit(1))
                )),
                Stmt::Expr(Expr::BinOp(
                    Box::new(Expr::Variable("b")),
                    BinOp::Assignment,
                    Box::new(Expr::IntegerLit(2))
                )),
            ]
        );
    }
}
