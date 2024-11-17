#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    /// [a-zA-Z_]
    Ident(&'a str),
    /// +:
    BitSlice,
    /// +
    Plus,
    /// =>
    Arrow,
    /// =
    SingleEq,
    /// ==
    DoubleEq,
    /// !=
    NotEq,
    /// >>
    ShiftRight,
    /// >
    GreaterThan,
    /// >=
    GreaterThanEqual,
    /// <<
    ShiftLeft,
    /// <
    LessThan,
    /// <=
    LessThanEqual,
    /// &&
    LogicalAnd,
    /// AND
    BitwiseAnd,
    /// ||
    LogicalOr,
    /// OR
    BitwiseOr,
    /// NOT
    BitwiseNot,
    /// _
    Underscore,
    /// *
    Mul,
    /// EOR
    Xor,
    /// ^
    Pow,
    /// / or DIV
    // todo: i think one is for reals and one is integer?
    Div,
    /// otherwise
    Otherwise,
    /// while
    While,
    /// do
    Do,
    /// (
    OpenParen,
    /// )
    CloseParen,
    ///
    Indent,
    ///
    Deindent,
    /// {
    OpenCurlyBrace,
    /// }
    CloseCurlyBrace,
    /// [
    OpenSquareBrace,
    /// ]
    CloseSquareBrace,
    /// !
    Bang,
    /// [0-9]+
    IntegerLit(u64),
    /// '[01x]+'
    BinaryInteger(&'a str),
    /// ,
    Comma,
    /// ;
    SemiColon,
    /// :
    Colon,
    /// -
    Minus,
    /// .
    Dot,
    /// case
    Case,
    /// when
    When,
    /// of
    Of,
    /// __decode
    Decode,
    /// __field
    Field,
    /// __encoding
    Encoding,
    /// A64
    A64,
    /// A32
    A32,
    /// T32
    T32,
    /// T16
    T16,
    /// __UNALLOCATED
    Unallocated,
    /// __UNPREDICTABLE
    Unpredictable,
    /// __instruction
    Instruction,
    /// __instruction_set
    InstructionSet,
    /// __unpredictable_unless
    UnpredictableUnless,
    /// __opcode
    Opcode,
    /// __guard
    Guard,
    /// __execute
    Execute,
    /// __conditional
    Conditional,
    /// __postdecode
    PostDecode,
    /// if
    If,
    /// elsif
    ElseIf,
    /// else
    Else,
    /// then
    Then,
    /// IN
    In,
    /// UNDEFINED
    Undefined,
    /// integer
    IntegerKeyword,
    /// boolean
    Boolean,
    /// TRUE
    True,
    /// FALSE
    False,
    /// for
    For,
    /// to
    To,
    /// SEE
    See,
    /// assert
    Assert,
    /// return
    Return,
    /// constant
    Constant,
    /// MOD
    Mod,
    /// array
    Array,
    /// bits(N)
    Bits,
    /// bit
    Bit,
    /// UNKNOWN
    Unknown,
    /// enumeration
    Enumeration,
}

pub struct AslLexer<'a> {
    buffer: &'a [u8],
    pub cursor: usize,

    pub ignore_indent: bool,

    indent_stack: Vec<usize>,
    pending_curlies: u32,
}

impl<'a> AslLexer<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            ignore_indent: false,
            cursor: 0,
            pending_curlies: 0,
            indent_stack: vec![0],
        }
    }

    pub fn debug_print(&self) {
        println!(
            "{}",
            std::str::from_utf8(&self.buffer[self.cursor.saturating_sub(50)..self.cursor]).unwrap()
        );
    }

    fn peek_byte(&self) -> Option<u8> {
        self.buffer.get(self.cursor).copied()
    }

    fn peek_n_bytes(&self, n: usize) -> Option<u8> {
        self.buffer.get(self.cursor + n).copied()
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.buffer
            .get(self.cursor)
            .copied()
            .inspect(|_| self.cursor += 1)
    }

    fn lex_eq(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b'>') => {
                self.next_byte();
                Token::Arrow
            }
            Some(b'=') => {
                self.next_byte();
                Token::DoubleEq
            }
            // todo: more things can cause single eq, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') => Token::SingleEq,
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_bang(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b'=') => {
                self.next_byte();
                Token::NotEq
            }
            // todo: more things can cause single eq, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'(' | b'\'') => Token::Bang,
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_gt(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b'>') => {
                self.next_byte();
                Token::ShiftRight
            }
            Some(b'=') => {
                self.next_byte();
                Token::GreaterThanEqual
            }
            // todo: more things can cause plus, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') => Token::GreaterThan,
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_lt(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b'<') => {
                self.next_byte();
                Token::ShiftLeft
            }
            Some(b'=') => {
                self.next_byte();
                Token::LessThanEqual
            }
            // todo: more things can cause plus, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') => Token::LessThan,
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_ampersand(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b'&') => {
                self.next_byte();
                Token::LogicalAnd
            }
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_plus(&mut self) -> Token<'a> {
        match self.peek_byte() {
            Some(b':') => {
                self.next_byte();
                Token::BitSlice
            }
            // todo: more things can cause plus, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') => Token::Plus,
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_forward_slash(&mut self) -> Option<Token<'a>> {
        match self.peek_byte() {
            Some(b'/') => {
                self.cursor -= 1;
                self.skip_comment();
                return self.next();
            }
            // todo: more things can cause plus, but i want to avoid a default
            // case for now to catch any bugs
            Some(b' ' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') => Some(Token::Div),
            tok => todo!("{:?}", tok.map(|t| t as char)),
        }
    }

    fn lex_bar(&mut self) -> Token<'a> {
        assert_eq!(self.next_byte(), Some(b'|'));
        Token::LogicalOr
    }

    fn lex_underscore(&mut self) -> Token<'a> {
        if self
            .peek_byte()
            .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
        {
            return self.lex_ident();
        }

        Token::Underscore
    }

    fn lex_integer(&mut self) -> Token<'a> {
        self.cursor -= 1;
        let mut start = self.cursor;

        let mut radix = 10;

        if self.peek_byte() == Some(b'0') && self.peek_n_bytes(1) == Some(b'x') {
            self.next_byte();
            self.next_byte();
            start = self.cursor;
            while self.peek_byte().is_some_and(|b| b.is_ascii_hexdigit()) {
                self.next_byte();
            }

            radix = 16;
        } else {
            while self.peek_byte().is_some_and(|b| b.is_ascii_digit()) {
                self.next_byte();
            }
        }

        let end = self.cursor;

        let s = std::str::from_utf8(&self.buffer[start..end]).unwrap();

        Token::IntegerLit(u64::from_str_radix(s, radix).unwrap())
    }

    fn lex_ident(&mut self) -> Token<'a> {
        self.cursor -= 1;
        let start = self.cursor;

        while self
            .peek_byte()
            .is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_')
        {
            self.next_byte();
        }

        let end = self.cursor;

        match &self.buffer[start..end] {
            b"case" => Token::Case,
            b"when" => Token::When,
            b"of" => Token::Of,
            b"__field" => Token::Field,
            b"__decode" => Token::Decode,
            b"__encoding" => Token::Encoding,
            b"A64" => Token::A64,
            b"A32" => Token::A32,
            b"T32" => Token::T32,
            b"T16" => Token::T16,
            b"__UNALLOCATED" => Token::Unallocated,
            b"__UNPREDICTABLE" | b"UNPREDICTABLE" => Token::Unpredictable,
            b"__instruction" => Token::Instruction,
            b"__instruction_set" => Token::InstructionSet,
            b"__unpredictable_unless" => Token::UnpredictableUnless,
            b"__opcode" => Token::Opcode,
            b"__guard" => Token::Guard,
            b"__execute" => Token::Execute,
            b"__conditional" => Token::Conditional,
            b"__postdecode" => Token::PostDecode,
            b"if" => Token::If,
            b"then" => Token::Then,
            b"IN" => Token::In,
            b"UNDEFINED" => Token::Undefined,
            b"integer" => Token::IntegerKeyword,
            b"boolean" => Token::Boolean,
            b"TRUE" => Token::True,
            b"FALSE" => Token::False,
            b"for" => Token::For,
            b"to" => Token::To,
            b"SEE" => Token::See,
            b"EOR" => Token::Xor,
            b"elsif" => Token::ElseIf,
            b"else" => Token::Else,
            b"assert" => Token::Assert,
            b"return" => Token::Return,
            b"constant" => Token::Constant,
            b"MOD" => Token::Mod,
            b"bits" => Token::Bits,
            b"bit" => Token::Bit,
            b"array" => Token::Array,
            b"AND" => Token::BitwiseAnd,
            b"OR" => Token::BitwiseOr,
            b"NOT" => Token::BitwiseNot,
            b"UNKNOWN" => Token::Unknown,
            b"DIV" => Token::Div,
            b"otherwise" => Token::Otherwise,
            b"while" => Token::While,
            b"do" => Token::Do,
            b"enumeration" => Token::Enumeration,
            val => Token::Ident(std::str::from_utf8(val).unwrap()),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_byte().is_some_and(|b| b == b' ') {
            self.next_byte();
        }
    }

    fn at_start_of_comment(&self) -> bool {
        self.peek_byte().is_some_and(|b| b == b'/')
            && self.peek_n_bytes(1).is_some_and(|b| b == b'/')
    }

    fn skip_comment(&mut self) {
        assert!(self.at_start_of_comment());
        while self.peek_byte().is_some_and(|b| b != b'\n') {
            self.next_byte();
        }
    }

    fn read_indent(&mut self) -> Option<Token<'a>> {
        let mut indent = 0;

        while self.peek_byte().is_some_and(|b| b == b' ') {
            indent += 1;
            self.next_byte();
        }

        if self.ignore_indent {
            return None;
        }

        if self.peek_byte().is_some_and(|b| b == b'\n') {
            // indent == 0 &&
            return None;
        }

        let current_indent = self.indent_stack.last().copied().unwrap();

        if indent == current_indent {
            return None;
        }

        if indent > current_indent {
            self.indent_stack.push(indent);
            return Some(Token::Indent);
        }

        while let Some(current_indent) = self.indent_stack.last().copied() {
            if current_indent <= indent {
                break;
            }

            self.pending_curlies += 1;
            self.indent_stack.pop();
        }

        None
    }

    fn lex_quoted_string(&mut self) -> Token<'a> {
        let start = self.cursor;

        while self.peek_byte().is_some_and(|b| !matches!(b, b'\"')) {
            self.next_byte();
        }

        let s = std::str::from_utf8(&self.buffer[start..self.cursor]).unwrap();

        assert_eq!(self.next_byte(), Some(b'\"'));

        Token::BinaryInteger(s)
    }

    fn lex_binary_string(&mut self) -> Token<'a> {
        let start = self.cursor;

        while self
            .peek_byte()
            .is_some_and(|b| matches!(b, b'0' | b'1' | b'x' | b' '))
        {
            self.next_byte();
        }

        assert_eq!(self.next_byte(), Some(b'\''));

        let s = std::str::from_utf8(&self.buffer[start..self.cursor]).unwrap();

        Token::BinaryInteger(s)
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        if self.pending_curlies > 0 {
            self.pending_curlies -= 1;
            return Some(Token::Deindent);
        }

        Some(match self.next_byte()? {
            b'\n' => {
                let indent_tok = self.read_indent();

                if indent_tok.is_some() {
                    return indent_tok;
                }

                return self.next();
            }
            b'/' => return self.lex_forward_slash(),
            b' ' => {
                self.skip_whitespace();
                return self.next();
            }
            b'!' => self.lex_bang(),
            b'+' => self.lex_plus(),
            b'=' => self.lex_eq(),
            b'_' => self.lex_underscore(),
            b'(' => Token::OpenParen,
            b')' => Token::CloseParen,
            b'[' => Token::OpenSquareBrace,
            b']' => Token::CloseSquareBrace,
            b',' => Token::Comma,
            b';' => Token::SemiColon,
            b':' => Token::Colon,
            b'-' => Token::Minus,
            b'*' => Token::Mul,
            b'^' => Token::Pow,
            b'{' => Token::OpenCurlyBrace,
            b'}' => Token::CloseCurlyBrace,
            b'.' => Token::Dot,
            b'|' => self.lex_bar(),
            b'>' => self.lex_gt(),
            b'<' => self.lex_lt(),
            b'&' => self.lex_ampersand(),
            b'0'..=b'9' => self.lex_integer(),
            b'a'..=b'z' | b'A'..=b'Z' => self.lex_ident(),
            b'\'' => self.lex_binary_string(),
            b'"' => self.lex_quoted_string(),
            tok => {
                self.debug_print();
                todo!("{:?}", tok as char)
            }
        })
    }

    pub fn at_end(&self) -> bool {
        self.cursor >= self.buffer.len()
    }

    pub fn peek(&mut self) -> Option<Token<'a>> {
        let start = self.cursor;
        let indent = self.indent_stack.clone();
        let pending_curlies = self.pending_curlies;
        let tok = self.next();
        self.cursor = start;
        self.indent_stack = indent;
        self.pending_curlies = pending_curlies;
        tok
    }
}
