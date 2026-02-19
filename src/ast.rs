/// AST (Abstract Syntax Tree) for AILang
/// Every node maps directly to a spec construct.
/// Flat structure — no deeply nested trees.

/// A complete AILang program
#[derive(Debug)]
pub struct Program {
    pub uses: Vec<UseDecl>,
    pub types: Vec<TypeDecl>,
    pub enums: Vec<EnumDecl>,
    pub consts: Vec<ConstDecl>,
    pub functions: Vec<FnDecl>,
    pub tests: Vec<TestDecl>,
    pub error_handlers: Vec<ErrHandler>,
    pub entry: Option<EntryBlock>,
}

/// #use path {names}
#[derive(Debug)]
pub struct UseDecl {
    pub path: String,
    pub names: Option<Vec<String>>,
    pub line: usize,
}

/// #type Name
///   field :Type
#[derive(Debug)]
pub struct TypeDecl {
    pub name: String,
    pub fields: Vec<Field>,
    pub line: usize,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub ty: AiType,
}

/// #enum Name
///   Variant :Type :Type
#[derive(Debug)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub line: usize,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<AiType>,
}

/// #const name :Type = value
#[derive(Debug)]
pub struct ConstDecl {
    pub name: String,
    pub ty: AiType,
    pub value: Expr,
    pub line: usize,
}

/// #fn name :RetType param:Type param:Type
///   body
#[derive(Debug)]
pub struct FnDecl {
    pub name: String,
    pub return_type: AiType,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: AiType,
}

/// #entry
///   body
#[derive(Debug)]
pub struct EntryBlock {
    pub body: Vec<Stmt>,
    pub line: usize,
}

/// #test name
///   body
#[derive(Debug)]
pub struct TestDecl {
    pub name: String,
    pub body: Vec<Stmt>,
    pub line: usize,
}

/// #err fn_name
///   retry 3 1000
///   fallback "value"
#[derive(Debug)]
pub struct ErrHandler {
    pub fn_name: String,
    pub retry_count: Option<i64>,
    pub retry_delay_ms: Option<i64>,
    pub fallback: Option<Expr>,
    pub line: usize,
}

/// Statements inside blocks
#[derive(Debug, Clone)]
pub enum Stmt {
    /// vN :Type = expr
    Bind {
        name: String,
        ty: AiType,
        value: Expr,
    },
    /// = expr
    Return {
        value: Expr,
    },
    /// > expr
    Emit {
        value: Expr,
    },
    /// Side-effect: log, each, assert, send, store_set, etc.
    Effect {
        expr: Expr,
    },
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    IntLit(i64),
    /// Float literal
    FloatLit(f64),
    /// Boolean literal
    BoolLit(bool),
    /// Text literal
    TextLit(String),
    /// Null
    NullLit,
    /// Variable reference
    Var(String),
    /// Field access: val.field
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    /// List literal: [1 2 3]
    ListLit(Vec<Expr>),
    /// Map literal: {"key" 42}
    MapLit(Vec<(Expr, Expr)>),
    /// Tuple literal: (1 "hello" true)
    TupleLit(Vec<Expr>),
    /// Prefix binary op: + a b
    BinOp {
        op: BinOpKind,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Prefix unary op: neg a, not a
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<Expr>,
    },
    /// call fn_name arg1 arg2
    Call {
        name: String,
        args: Vec<Expr>,
    },
    /// select cond then else
    Select {
        cond: Box<Expr>,
        then_val: Box<Expr>,
        else_val: Box<Expr>,
    },
    /// cond cond1 val1 cond2 val2 ... default
    Cond {
        branches: Vec<(Box<Expr>, Box<Expr>)>,
        default: Box<Expr>,
    },
    /// match val
    ///   Pattern => expr
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    /// map fn list
    MapIter {
        func: Box<Expr>,
        list: Box<Expr>,
    },
    /// filter fn list
    FilterIter {
        func: Box<Expr>,
        list: Box<Expr>,
    },
    /// fold list init fn
    FoldIter {
        list: Box<Expr>,
        init: Box<Expr>,
        func: Box<Expr>,
    },
    /// each list fn
    EachIter {
        list: Box<Expr>,
        func: Box<Expr>,
    },
    /// zip list_a list_b
    ZipIter {
        list_a: Box<Expr>,
        list_b: Box<Expr>,
    },
    /// flatmap fn list
    FlatMapIter {
        func: Box<Expr>,
        list: Box<Expr>,
    },
    /// cast Type val
    Cast {
        target: AiType,
        value: Box<Expr>,
    },
    /// try expr
    TryExpr {
        value: Box<Expr>,
    },
    /// unwrap expr default
    Unwrap {
        value: Box<Expr>,
        default: Box<Expr>,
    },
    /// ok expr
    OkWrap(Box<Expr>),
    /// tool "name" {params}
    ToolCall {
        name: Box<Expr>,
        params: Box<Expr>,
    },
    /// log "level" "msg" args...
    Log {
        level: Box<Expr>,
        message: Box<Expr>,
        args: Vec<Expr>,
    },
    /// assert expr
    Assert {
        value: Box<Expr>,
    },
    /// Type constructor: TypeName val1 val2
    Construct {
        type_name: String,
        args: Vec<Expr>,
    },
    /// Inline lambda: (fn a:Type => expr)
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },
    /// Error propagation: expr ?
    Propagate(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    /// Literal value
    Literal(Expr),
    /// Enum variant: Circle r
    Variant {
        name: String,
        bindings: Vec<String>,
    },
    /// Wildcard: _
    Wildcard,
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Band,
    Bor,
    Bxor,
    Shl,
    Shr,
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Neg,
    Not,
    Bnot,
}

/// AILang type system
#[derive(Debug, Clone)]
pub enum AiType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Text,
    Byte,
    Void,
    Any,
    List(Box<AiType>),
    Map(Box<AiType>, Box<AiType>),
    Tuple(Vec<AiType>),
    Optional(Box<AiType>),
    Result(Box<AiType>),
    Function(Vec<AiType>, Box<AiType>),
    Named(String),
}
