use super::arch_decode::Field;

#[derive(Debug)]
pub struct InstructionDefinition<'a> {
    pub name: &'a str,
    pub conditional: bool,
    pub encodings: Vec<InstructionEncoding<'a>>,
    pub postdecode: Vec<Stmt<'a>>,
    pub execute: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct InstructionEncoding<'a> {
    pub name: &'a str,
    pub inst_set: InstructionSet,
    pub fields: Vec<Field<'a>>,
    pub opcode: &'a str,
    pub guard: Expr<'a>,
    pub unpredictable_unless: Vec<Expr<'a>>,
    pub decode: Vec<Stmt<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionSet {
    A64,
    A32,
    /// 32-bit thumb
    T32,
    T16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Assignment,
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte,
    LogicalAnd,
    BitwiseAnd,
    LogicalOr,
    BitwiseOr,
    In,
    ShiftLeft,
    ShiftRight,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Xor,
    Mod,
    /// a.b, todo: rename field access or something?
    Dot,
    /// :
    Slice,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    BitwiseNot,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<'a> {
    BinOp(Box<Self>, BinOp, Box<Self>),
    UnaryOp(UnaryOp, Box<Self>),
    FnCall {
        name: &'a str,
        args: Vec<Self>,
    },
    Undefined,
    Index(Box<Self>, Vec<Self>),
    BinaryInteger(&'a str),
    IntegerLit(u64),
    Paren(Box<Self>),
    Array(Vec<Self>),
    Variable(&'a str),
    True,
    False,
    Tuple(Vec<Expr<'a>>),
    BitsUnknown(Box<Bits<'a>>),
    Ternary {
        cond: Box<Expr<'a>>,
        if_true: Box<Expr<'a>>,
        else_clause: Box<Expr<'a>>,
    },
    FieldArray(Vec<Expr<'a>>),
    IntegerUnknown,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Bits<'a> {
    pub n: Expr<'a>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VariableType<'a> {
    Integer,
    Boolean,
    Bit,
    Bits(Bits<'a>),
    Ident(&'a str),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt<'a> {
    If(AstIf<'a>),
    For(AstFor<'a>),
    FnDecl,
    Case(AstCase<'a>),
    Assert(Expr<'a>),
    Return(Expr<'a>),
    Expr(Expr<'a>),
    See(Expr<'a>),
    Declaration(VariableType<'a>, Vec<&'a str>, Option<Expr<'a>>),
    TupleAssignment {
        vars: Vec<Expr<'a>>,
        val: Expr<'a>,
    },
    ArrayDecl {
        start: u64,
        end: u64,
        of: Bits<'a>,
        name: &'a str,
    },
    ConstDecl {
        ty: VariableType<'a>,
        name: &'a str,
        val: Expr<'a>,
    },
    While(AstWhile<'a>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstIf<'a> {
    pub if_clauses: Vec<AstIfClause<'a>>,
    pub else_clause: Option<Vec<Stmt<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstIfClause<'a> {
    pub cond: Expr<'a>,
    pub body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstFor<'a> {
    pub var: &'a str,
    pub init: Expr<'a>,
    pub to: Expr<'a>,
    pub body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstCase<'a> {
    pub(crate) clause: Expr<'a>,
    pub(crate) when: Vec<AstWhen<'a>>,
    pub(crate) otherwise: Option<Vec<Stmt<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstWhen<'a> {
    pub(crate) clause: Expr<'a>,
    pub(crate) body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstWhile<'a> {
    pub cond: Expr<'a>,
    pub body: Vec<Stmt<'a>>,
}
