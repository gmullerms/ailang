/// Token types for AILang
/// Every meaningful unit of the language maps to exactly one token kind.

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Sigils (block markers)
    Fn,       // #fn
    Type,     // #type
    Enum,     // #enum
    Const,    // #const
    Use,      // #use
    Entry,    // #entry
    Test,     // #test
    Err,      // #err
    Extern,   // #extern

    // Keywords
    Call,     // call
    Select,   // select
    Cond,     // cond
    Match,    // match
    Map,      // map
    Filter,   // filter
    Fold,     // fold
    Each,     // each
    FlatMap,  // flatmap
    Zip,      // zip
    Async,    // async
    Await,    // await
    Par,      // par
    Try,      // try
    Unwrap,   // unwrap
    Ok,       // ok
    Assert,   // assert
    Log,      // log
    Tool,     // tool
    Prompt,   // prompt
    Cast,     // cast
    Typeof,   // typeof
    Is,       // is
    True,     // true
    False,    // false
    Null,     // null
    Panic,    // panic
    Error,    // error
    Retry,    // retry
    Fallback, // fallback
    Send,     // send
    Recv,     // recv
    Chan,     // chan
    Pipe,     // |>
    Arrow,    // ->
    FatArrow, // =>
    Question, // ?

    // Operators
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Percent,  // %
    Eq,       // ==
    Neq,      // !=
    Lt,       // <
    Gt,       // >
    Lte,      // <=
    Gte,      // >=
    And,      // and
    Or,       // or
    Not,      // not
    Band,     // band
    Bor,      // bor
    Bxor,     // bxor
    Bnot,     // bnot
    Shl,      // shl
    Shr,      // shr
    Neg,      // neg

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrack,   // [
    RBrack,   // ]
    LBrace,   // {
    RBrace,   // }

    // Punctuation
    Colon,    // :
    Assign,   // =
    Return,   // = at start of statement (contextual)
    Emit,     // >  at start of statement (contextual)
    Underscore, // _
    Dot,      // .
    Comment,  // --

    // Literals
    IntLit(i64),
    FloatLit(f64),
    BoolLit(bool),
    TextLit(String),
    NullLit,

    // Identifiers
    Ident(String),

    // Structure
    Newline,
    Indent,   // 2 spaces at start of line
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, col: usize) -> Self {
        Token { kind, line, col }
    }
}
