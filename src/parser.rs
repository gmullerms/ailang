/// Parser for AILang
/// Transforms a flat token stream into an AST.
/// Sigil-driven: each #sigil starts a top-level block.

use crate::ast::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut program = Program {
            uses: Vec::new(),
            types: Vec::new(),
            enums: Vec::new(),
            consts: Vec::new(),
            functions: Vec::new(),
            tests: Vec::new(),
            error_handlers: Vec::new(),
            entry: None,
        };

        self.skip_newlines();

        while !self.is_at_end() {
            match &self.peek().kind {
                TokenKind::Use => program.uses.push(self.parse_use()?),
                TokenKind::Type => program.types.push(self.parse_type_decl()?),
                TokenKind::Enum => program.enums.push(self.parse_enum_decl()?),
                TokenKind::Const => program.consts.push(self.parse_const()?),
                TokenKind::Fn => program.functions.push(self.parse_fn()?),
                TokenKind::Test => program.tests.push(self.parse_test()?),
                TokenKind::Err => program.error_handlers.push(self.parse_err_handler()?),
                TokenKind::Entry => {
                    program.entry = Some(self.parse_entry()?);
                }
                TokenKind::Eof => break,
                _ => {
                    let tok = self.peek();
                    return Err(self.error(&format!(
                        "expected block sigil (#fn, #type, etc.), got {:?}",
                        tok.kind
                    )));
                }
            }
            self.skip_newlines();
        }

        Ok(program)
    }

    // --- Top-level block parsers ---

    fn parse_use(&mut self) -> Result<UseDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Use)?;
        let path = self.expect_ident()?;

        let names = if self.check(&TokenKind::LBrace) {
            self.advance();
            let mut names = Vec::new();
            while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
                names.push(self.expect_ident()?);
            }
            self.expect(TokenKind::RBrace)?;
            Some(names)
        } else {
            None
        };

        self.expect_newline_or_eof()?;
        Ok(UseDecl { path, names, line })
    }

    fn parse_type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Type)?;
        let name = self.expect_ident()?;
        self.expect_newline_or_eof()?;

        let mut fields = Vec::new();
        while self.check(&TokenKind::Indent) {
            self.advance(); // consume indent
            let field_name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push(Field {
                name: field_name,
                ty,
            });
            self.expect_newline_or_eof()?;
        }

        Ok(TypeDecl { name, fields, line })
    }

    fn parse_enum_decl(&mut self) -> Result<EnumDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Enum)?;
        let name = self.expect_ident()?;
        self.expect_newline_or_eof()?;

        let mut variants = Vec::new();
        while self.check(&TokenKind::Indent) {
            self.advance();
            let variant_name = self.expect_ident()?;
            let mut fields = Vec::new();
            while self.check(&TokenKind::Colon) {
                self.advance();
                fields.push(self.parse_type()?);
            }
            variants.push(EnumVariant {
                name: variant_name,
                fields,
            });
            self.expect_newline_or_eof()?;
        }

        Ok(EnumDecl {
            name,
            variants,
            line,
        })
    }

    fn parse_const(&mut self) -> Result<ConstDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Const)?;
        let name = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Assign)?;
        let value = self.parse_expr()?;
        self.expect_newline_or_eof()?;
        Ok(ConstDecl {
            name,
            ty,
            value,
            line,
        })
    }

    fn parse_fn(&mut self) -> Result<FnDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Fn)?;
        let name = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        let mut params = Vec::new();
        while self.check_ident() {
            let param_name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let param_type = self.parse_type()?;
            params.push(Param {
                name: param_name,
                ty: param_type,
            });
        }

        self.expect_newline_or_eof()?;
        let body = self.parse_block_body()?;

        Ok(FnDecl {
            name,
            return_type,
            params,
            body,
            line,
        })
    }

    fn parse_entry(&mut self) -> Result<EntryBlock, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Entry)?;
        self.expect_newline_or_eof()?;
        let body = self.parse_block_body()?;
        Ok(EntryBlock { body, line })
    }

    fn parse_test(&mut self) -> Result<TestDecl, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Test)?;
        let name = self.expect_ident()?;
        self.expect_newline_or_eof()?;
        let body = self.parse_block_body()?;
        Ok(TestDecl { name, body, line })
    }

    fn parse_err_handler(&mut self) -> Result<ErrHandler, ParseError> {
        let line = self.peek().line;
        self.expect(TokenKind::Err)?;
        let fn_name = self.expect_ident()?;
        self.expect_newline_or_eof()?;

        let mut handler = ErrHandler {
            fn_name,
            retry_count: None,
            retry_delay_ms: None,
            fallback: None,
            line,
        };

        while self.check(&TokenKind::Indent) {
            self.advance();
            match &self.peek().kind {
                TokenKind::Retry => {
                    self.advance();
                    if let TokenKind::IntLit(n) = self.peek().kind {
                        handler.retry_count = Some(n);
                        self.advance();
                    }
                    if let TokenKind::IntLit(n) = self.peek().kind {
                        handler.retry_delay_ms = Some(n);
                        self.advance();
                    }
                }
                TokenKind::Fallback => {
                    self.advance();
                    handler.fallback = Some(self.parse_expr()?);
                }
                _ => {
                    return Err(self.error("expected 'retry' or 'fallback' in error handler"));
                }
            }
            self.expect_newline_or_eof()?;
        }

        Ok(handler)
    }

    // --- Block body parsing ---

    fn parse_block_body(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();

        while self.check(&TokenKind::Indent) {
            self.advance(); // consume indent
            stmts.push(self.parse_stmt()?);
            self.expect_newline_or_eof()?;
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match &self.peek().kind {
            // Return: = expr
            TokenKind::Assign => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Stmt::Return { value })
            }
            // Emit: > expr (only when > is at start of statement)
            TokenKind::Gt if self.is_stmt_start() => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Stmt::Emit { value })
            }
            // Bind: vN :Type = expr
            TokenKind::Ident(name) if name.starts_with('v') && name[1..].chars().all(|c| c.is_ascii_digit()) => {
                let name = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?;
                self.expect(TokenKind::Assign)?;
                let value = self.parse_expr()?;
                Ok(Stmt::Bind { name, ty, value })
            }
            // Effect statements: log, each, assert, send, store_set
            TokenKind::Log | TokenKind::Each | TokenKind::Assert | TokenKind::Send => {
                let expr = self.parse_expr()?;
                Ok(Stmt::Effect { expr })
            }
            _ => {
                let expr = self.parse_expr()?;
                Ok(Stmt::Effect { expr })
            }
        }
    }

    // --- Expression parsing ---

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match &self.peek().kind {
            // Prefix binary operators
            TokenKind::Plus => self.parse_binop(BinOpKind::Add),
            TokenKind::Minus => self.parse_binop(BinOpKind::Sub),
            TokenKind::Star => self.parse_binop(BinOpKind::Mul),
            TokenKind::Slash => self.parse_binop(BinOpKind::Div),
            TokenKind::Percent => self.parse_binop(BinOpKind::Mod),
            TokenKind::Eq => self.parse_binop(BinOpKind::Eq),
            TokenKind::Neq => self.parse_binop(BinOpKind::Neq),
            TokenKind::Lt => self.parse_binop(BinOpKind::Lt),
            TokenKind::Gt if !self.is_stmt_start() => self.parse_binop(BinOpKind::Gt),
            TokenKind::Lte => self.parse_binop(BinOpKind::Lte),
            TokenKind::Gte => self.parse_binop(BinOpKind::Gte),
            TokenKind::And => self.parse_binop(BinOpKind::And),
            TokenKind::Or => self.parse_binop(BinOpKind::Or),
            TokenKind::Band => self.parse_binop(BinOpKind::Band),
            TokenKind::Bor => self.parse_binop(BinOpKind::Bor),
            TokenKind::Bxor => self.parse_binop(BinOpKind::Bxor),
            TokenKind::Shl => self.parse_binop(BinOpKind::Shl),
            TokenKind::Shr => self.parse_binop(BinOpKind::Shr),

            // Unary operators
            TokenKind::Neg => self.parse_unaryop(UnaryOpKind::Neg),
            TokenKind::Not => self.parse_unaryop(UnaryOpKind::Not),
            TokenKind::Bnot => self.parse_unaryop(UnaryOpKind::Bnot),

            // Call
            TokenKind::Call => self.parse_call(),

            // Select
            TokenKind::Select => self.parse_select(),

            // Match
            TokenKind::Match => self.parse_match(),

            // Iteration
            TokenKind::Map => self.parse_map_iter(),
            TokenKind::Filter => self.parse_filter_iter(),
            TokenKind::Fold => self.parse_fold_iter(),
            TokenKind::Each => self.parse_each_iter(),

            // Cast
            TokenKind::Cast => self.parse_cast(),

            // Try
            TokenKind::Try => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Expr::TryExpr {
                    value: Box::new(value),
                })
            }

            // Unwrap
            TokenKind::Unwrap => {
                self.advance();
                let value = self.parse_expr()?;
                let default = self.parse_expr()?;
                Ok(Expr::Unwrap {
                    value: Box::new(value),
                    default: Box::new(default),
                })
            }

            // Ok wrap
            TokenKind::Ok => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Expr::OkWrap(Box::new(value)))
            }

            // Tool call
            TokenKind::Tool => {
                self.advance();
                let name = self.parse_atom()?;
                let params = self.parse_atom()?;
                Ok(Expr::ToolCall {
                    name: Box::new(name),
                    params: Box::new(params),
                })
            }

            // Log
            TokenKind::Log => {
                self.advance();
                let level = self.parse_atom()?;
                let message = self.parse_atom()?;
                let mut args = Vec::new();
                while !self.check_expr_end() {
                    args.push(self.parse_atom()?);
                }
                Ok(Expr::Log {
                    level: Box::new(level),
                    message: Box::new(message),
                    args,
                })
            }

            // Assert
            TokenKind::Assert => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Expr::Assert {
                    value: Box::new(value),
                })
            }

            // Literals and atoms
            _ => self.parse_atom(),
        }
    }

    fn parse_binop(&mut self, op: BinOpKind) -> Result<Expr, ParseError> {
        self.advance(); // consume operator
        let left = self.parse_expr()?;
        let right = self.parse_expr()?;
        Ok(Expr::BinOp {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_unaryop(&mut self, op: UnaryOpKind) -> Result<Expr, ParseError> {
        self.advance();
        let operand = self.parse_expr()?;
        Ok(Expr::UnaryOp {
            op,
            operand: Box::new(operand),
        })
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        self.advance(); // consume 'call'
        let name = self.expect_ident()?;
        let mut args = Vec::new();
        while !self.check_expr_end() {
            args.push(self.parse_atom()?);
        }
        Ok(Expr::Call { name, args })
    }

    fn parse_select(&mut self) -> Result<Expr, ParseError> {
        self.advance(); // consume 'select'
        let cond = self.parse_expr()?;
        let then_val = self.parse_expr()?;
        let else_val = self.parse_expr()?;
        Ok(Expr::Select {
            cond: Box::new(cond),
            then_val: Box::new(then_val),
            else_val: Box::new(else_val),
        })
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        self.advance(); // consume 'match'
        let value = self.parse_atom()?;
        self.expect_newline_or_eof()?;

        let mut arms = Vec::new();
        while self.check(&TokenKind::Indent) {
            self.advance();
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::FatArrow)?;
            let body = self.parse_expr()?;
            arms.push(MatchArm { pattern, body });
            if !self.is_at_end() && self.check(&TokenKind::Newline) {
                self.advance();
            }
        }

        Ok(Expr::Match {
            value: Box::new(value),
            arms,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.peek().kind {
            TokenKind::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            TokenKind::IntLit(n) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Literal(Expr::IntLit(n)))
            }
            TokenKind::TextLit(s) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Expr::TextLit(s)))
            }
            TokenKind::BoolLit(b) => {
                let b = *b;
                self.advance();
                Ok(Pattern::Literal(Expr::BoolLit(b)))
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                // Check if this is an enum variant with bindings
                let mut bindings = Vec::new();
                while self.check_ident() && !self.check(&TokenKind::FatArrow) {
                    bindings.push(self.expect_ident()?);
                }
                Ok(Pattern::Variant { name, bindings })
            }
            _ => Err(self.error("expected pattern")),
        }
    }

    fn parse_map_iter(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let func = self.parse_atom()?;
        let list = self.parse_atom()?;
        Ok(Expr::MapIter {
            func: Box::new(func),
            list: Box::new(list),
        })
    }

    fn parse_filter_iter(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let func = self.parse_atom()?;
        let list = self.parse_atom()?;
        Ok(Expr::FilterIter {
            func: Box::new(func),
            list: Box::new(list),
        })
    }

    fn parse_fold_iter(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let list = self.parse_atom()?;
        let init = self.parse_atom()?;
        let func = self.parse_atom()?;
        Ok(Expr::FoldIter {
            list: Box::new(list),
            init: Box::new(init),
            func: Box::new(func),
        })
    }

    fn parse_each_iter(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let list = self.parse_atom()?;
        let func = self.parse_atom()?;
        Ok(Expr::EachIter {
            list: Box::new(list),
            func: Box::new(func),
        })
    }

    fn parse_cast(&mut self) -> Result<Expr, ParseError> {
        self.advance();
        let target = self.parse_type()?;
        let value = self.parse_atom()?;
        Ok(Expr::Cast {
            target,
            value: Box::new(value),
        })
    }

    /// Parse an atomic expression (literals, variables, bracketed constructs)
    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        match &self.peek().kind {
            TokenKind::IntLit(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::IntLit(n))
            }
            TokenKind::FloatLit(f) => {
                let f = *f;
                self.advance();
                Ok(Expr::FloatLit(f))
            }
            TokenKind::BoolLit(b) => {
                let b = *b;
                self.advance();
                Ok(Expr::BoolLit(b))
            }
            TokenKind::TextLit(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::TextLit(s))
            }
            TokenKind::NullLit => {
                self.advance();
                Ok(Expr::NullLit)
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                // Check for field access: name.field
                if self.check(&TokenKind::Dot) {
                    self.advance();
                    let field = self.expect_ident()?;
                    Ok(Expr::FieldAccess {
                        object: Box::new(Expr::Var(name)),
                        field,
                    })
                } else {
                    Ok(Expr::Var(name))
                }
            }
            // List literal: [1 2 3]
            TokenKind::LBrack => {
                self.advance();
                let mut elements = Vec::new();
                while !self.check(&TokenKind::RBrack) && !self.is_at_end() {
                    elements.push(self.parse_atom()?);
                }
                self.expect(TokenKind::RBrack)?;
                Ok(Expr::ListLit(elements))
            }
            // Map literal: {"key" 42 "key2" 99}
            TokenKind::LBrace => {
                self.advance();
                let mut pairs = Vec::new();
                while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
                    let key = self.parse_atom()?;
                    let value = self.parse_atom()?;
                    pairs.push((key, value));
                }
                self.expect(TokenKind::RBrace)?;
                Ok(Expr::MapLit(pairs))
            }
            // Tuple or lambda: (...)
            TokenKind::LParen => {
                self.advance();
                // Check if this is a lambda: (fn params => body)
                if self.check(&TokenKind::Fn)
                    || matches!(&self.peek().kind, TokenKind::Ident(s) if s == "fn")
                {
                    self.advance();
                    let mut params = Vec::new();
                    while !self.check(&TokenKind::FatArrow) && !self.is_at_end() {
                        let pname = self.expect_ident()?;
                        self.expect(TokenKind::Colon)?;
                        let pty = self.parse_type()?;
                        params.push(Param {
                            name: pname,
                            ty: pty,
                        });
                    }
                    self.expect(TokenKind::FatArrow)?;
                    let body = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(Expr::Lambda {
                        params,
                        body: Box::new(body),
                    })
                } else if self.is_expr_start() {
                    // Grouped expression: (call foo bar), (+ a b), (select ...)
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(expr)
                } else {
                    // Tuple literal: (1 2 3)
                    let mut elements = Vec::new();
                    while !self.check(&TokenKind::RParen) && !self.is_at_end() {
                        elements.push(self.parse_atom()?);
                    }
                    self.expect(TokenKind::RParen)?;
                    Ok(Expr::TupleLit(elements))
                }
            }
            TokenKind::Panic => {
                self.advance();
                Ok(Expr::Var("panic".to_string()))
            }
            _ => {
                let tok = self.peek();
                Err(self.error(&format!("expected expression, got {:?}", tok.kind)))
            }
        }
    }

    // --- Type parsing ---

    pub fn parse_type(&mut self) -> Result<AiType, ParseError> {
        match &self.peek().kind {
            // Optional: ?Type
            TokenKind::Question => {
                self.advance();
                let inner = self.parse_type()?;
                Ok(AiType::Optional(Box::new(inner)))
            }
            // List: [Type]
            TokenKind::LBrack => {
                self.advance();
                let inner = self.parse_type()?;
                self.expect(TokenKind::RBrack)?;
                Ok(AiType::List(Box::new(inner)))
            }
            // Map: {K:V}
            TokenKind::LBrace => {
                self.advance();
                let key = self.parse_type()?;
                self.expect(TokenKind::Colon)?;
                let val = self.parse_type()?;
                self.expect(TokenKind::RBrace)?;
                Ok(AiType::Map(Box::new(key), Box::new(val)))
            }
            // Tuple or Function type: (T1 T2 -> T3) or (T1 T2)
            TokenKind::LParen => {
                self.advance();
                let mut types = Vec::new();
                while !self.check(&TokenKind::RParen)
                    && !self.check(&TokenKind::Arrow)
                    && !self.is_at_end()
                {
                    types.push(self.parse_type()?);
                }
                if self.check(&TokenKind::Arrow) {
                    self.advance();
                    let ret = self.parse_type()?;
                    self.expect(TokenKind::RParen)?;
                    Ok(AiType::Function(types, Box::new(ret)))
                } else {
                    self.expect(TokenKind::RParen)?;
                    Ok(AiType::Tuple(types))
                }
            }
            TokenKind::Ident(name) => {
                let ty = match name.as_str() {
                    "i32" => AiType::I32,
                    "i64" => AiType::I64,
                    "f32" => AiType::F32,
                    "f64" => AiType::F64,
                    "bool" => AiType::Bool,
                    "text" => AiType::Text,
                    "byte" => AiType::Byte,
                    "void" => AiType::Void,
                    "any" => AiType::Any,
                    other => AiType::Named(other.to_string()),
                };
                self.advance();
                Ok(ty)
            }
            // Result type prefix: !Type
            // We handle this by checking if the previous token context expects it
            _ => {
                let tok = self.peek();
                Err(self.error(&format!("expected type, got {:?}", tok.kind)))
            }
        }
    }

    // --- Helpers ---

    fn peek(&self) -> &Token {
        &self.tokens[self.pos.min(self.tokens.len() - 1)]
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos.min(self.tokens.len() - 1)];
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn check_ident(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Ident(_))
    }

    fn check_line_end(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Eof | TokenKind::Indent
        )
    }

    fn check_expr_end(&self) -> bool {
        // Stop consuming args at line end OR closing delimiters
        matches!(
            self.peek().kind,
            TokenKind::Newline
                | TokenKind::Eof
                | TokenKind::Indent
                | TokenKind::RParen
                | TokenKind::RBrack
                | TokenKind::RBrace
        )
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || matches!(self.peek().kind, TokenKind::Eof)
    }

    fn is_expr_start(&self) -> bool {
        // Check if current token starts a compound expression (not just an atom)
        matches!(
            self.peek().kind,
            TokenKind::Call
                | TokenKind::Select
                | TokenKind::Match
                | TokenKind::Map
                | TokenKind::Filter
                | TokenKind::Fold
                | TokenKind::Each
                | TokenKind::FlatMap
                | TokenKind::Cast
                | TokenKind::Try
                | TokenKind::Unwrap
                | TokenKind::Ok
                | TokenKind::Tool
                | TokenKind::Log
                | TokenKind::Assert
                | TokenKind::Neg
                | TokenKind::Not
                | TokenKind::Bnot
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::Neq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Lte
                | TokenKind::Gte
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Band
                | TokenKind::Bor
                | TokenKind::Bxor
                | TokenKind::Shl
                | TokenKind::Shr
                | TokenKind::Async
                | TokenKind::Await
        )
    }

    fn is_stmt_start(&self) -> bool {
        // Check if we're at the start of a statement (after indent)
        self.pos > 0
            && matches!(
                self.tokens[self.pos - 1].kind,
                TokenKind::Indent | TokenKind::Newline
            )
    }

    fn expect(&mut self, expected: TokenKind) -> Result<&Token, ParseError> {
        if self.check(&expected) {
            Ok(self.advance())
        } else {
            Err(self.error(&format!(
                "expected {:?}, got {:?}",
                expected,
                self.peek().kind
            )))
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match &self.peek().kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => Err(self.error(&format!("expected identifier, got {:?}", self.peek().kind))),
        }
    }

    fn expect_newline_or_eof(&mut self) -> Result<(), ParseError> {
        self.skip_newlines();
        Ok(())
    }

    fn skip_newlines(&mut self) {
        while !self.is_at_end() && matches!(self.peek().kind, TokenKind::Newline) {
            self.advance();
        }
    }

    fn error(&self, message: &str) -> ParseError {
        let tok = self.peek();
        ParseError {
            message: message.to_string(),
            line: tok.line,
            col: tok.col,
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parse error at {}:{}: {}",
            self.line, self.col, self.message
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    /// Helper: lex + parse source, return the Program
    fn parse_source(source: &str) -> Program {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parser failed")
    }

    /// Helper: lex + parse, expect failure
    fn parse_source_err(source: &str) -> ParseError {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect_err("expected parse error")
    }

    // -------------------------------------------------------
    // 1. Parsing a simple function
    // -------------------------------------------------------
    #[test]
    fn test_parse_simple_function() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b";
        let prog = parse_source(src);

        assert_eq!(prog.functions.len(), 1);
        let f = &prog.functions[0];
        assert_eq!(f.name, "add");
        assert_eq!(f.params.len(), 2);
        assert_eq!(f.params[0].name, "a");
        assert_eq!(f.params[1].name, "b");

        // Return type should be i32
        assert!(matches!(f.return_type, AiType::I32));

        // Body should have exactly one statement (a return)
        assert_eq!(f.body.len(), 1);
        assert!(matches!(f.body[0], Stmt::Return { .. }));
    }

    // -------------------------------------------------------
    // 2. Parsing select expression
    // -------------------------------------------------------
    #[test]
    fn test_parse_select_expression() {
        let src = "#fn f :i32 x:i32\n  = select (> x 0) x (neg x)";
        let prog = parse_source(src);

        assert_eq!(prog.functions.len(), 1);
        let f = &prog.functions[0];
        assert_eq!(f.body.len(), 1);

        if let Stmt::Return { value } = &f.body[0] {
            assert!(matches!(value, Expr::Select { .. }));
            if let Expr::Select { cond, then_val, else_val } = value {
                assert!(matches!(cond.as_ref(), Expr::BinOp { op: BinOpKind::Gt, .. }));
                assert!(matches!(then_val.as_ref(), Expr::Var(ref name) if name == "x"));
                assert!(matches!(else_val.as_ref(), Expr::UnaryOp { op: UnaryOpKind::Neg, .. }));
            }
        } else {
            panic!("expected return statement");
        }
    }

    // -------------------------------------------------------
    // 3. Parsing list literal
    // -------------------------------------------------------
    #[test]
    fn test_parse_list_literal() {
        let src = "#fn f :[i32]\n  = [1 2 3]";
        let prog = parse_source(src);

        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::ListLit(elements) = value {
                assert_eq!(elements.len(), 3);
                assert!(matches!(elements[0], Expr::IntLit(1)));
                assert!(matches!(elements[1], Expr::IntLit(2)));
                assert!(matches!(elements[2], Expr::IntLit(3)));
            } else {
                panic!("expected ListLit, got {:?}", value);
            }
        } else {
            panic!("expected return statement");
        }
    }

    // -------------------------------------------------------
    // 4. Parsing map literal
    // -------------------------------------------------------
    #[test]
    fn test_parse_map_literal() {
        let src = "#fn f :any\n  = {\"key\" 42}";
        let prog = parse_source(src);

        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::MapLit(pairs) = value {
                assert_eq!(pairs.len(), 1);
                assert!(matches!(&pairs[0].0, Expr::TextLit(ref s) if s == "key"));
                assert!(matches!(pairs[0].1, Expr::IntLit(42)));
            } else {
                panic!("expected MapLit, got {:?}", value);
            }
        } else {
            panic!("expected return statement");
        }
    }

    // -------------------------------------------------------
    // 5. Parsing lambda
    // -------------------------------------------------------
    #[test]
    fn test_parse_lambda() {
        let src = "#fn f :i32\n  = (fn x:i32 => * x x)";
        let prog = parse_source(src);

        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::Lambda { params, body } = value {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name, "x");
                assert!(matches!(params[0].ty, AiType::I32));
                assert!(matches!(body.as_ref(), Expr::BinOp { op: BinOpKind::Mul, .. }));
            } else {
                panic!("expected Lambda, got {:?}", value);
            }
        } else {
            panic!("expected return statement");
        }
    }

    // -------------------------------------------------------
    // 6. Parsing call with grouped args
    // -------------------------------------------------------
    #[test]
    fn test_parse_call_grouped_args() {
        let src = "#fn f :i32 a:i32\n  = call foo (+ a 1)";
        let prog = parse_source(src);

        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::Call { name, args } = value {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], Expr::BinOp { op: BinOpKind::Add, .. }));
            } else {
                panic!("expected Call, got {:?}", value);
            }
        } else {
            panic!("expected return statement");
        }
    }

    // -------------------------------------------------------
    // 7. Parse error on invalid input
    // -------------------------------------------------------
    #[test]
    fn test_parse_error_invalid_input() {
        let err = parse_source_err("42");
        assert!(err.message.contains("expected"));
    }

    #[test]
    fn test_parse_error_missing_return_type() {
        let err = parse_source_err("#fn foo");
        assert!(err.message.contains("expected"));
    }

    // -------------------------------------------------------
    // Additional parser tests
    // -------------------------------------------------------
    #[test]
    fn test_parse_test_block() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#test add_basic\n  v0 :i32 = call add 2 3\n  assert == v0 5";
        let prog = parse_source(src);

        assert_eq!(prog.functions.len(), 1);
        assert_eq!(prog.tests.len(), 1);
        assert_eq!(prog.tests[0].name, "add_basic");
        assert_eq!(prog.tests[0].body.len(), 2);
    }

    #[test]
    fn test_parse_entry_block() {
        let src = "#entry\n  = 0";
        let prog = parse_source(src);
        assert!(prog.entry.is_some());
    }

    #[test]
    fn test_parse_const() {
        let src = "#const PI :f64 = 3.14";
        let prog = parse_source(src);
        assert_eq!(prog.consts.len(), 1);
        assert_eq!(prog.consts[0].name, "PI");
        assert!(matches!(prog.consts[0].ty, AiType::F64));
    }

    #[test]
    fn test_parse_multiple_params() {
        let src = "#fn f :i32 a:i32 b:i32 c:text\n  = a";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        assert_eq!(f.params.len(), 3);
        assert_eq!(f.params[2].name, "c");
        assert!(matches!(f.params[2].ty, AiType::Text));
    }

    #[test]
    fn test_parse_empty_list() {
        let src = "#fn f :[i32]\n  = []";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::ListLit(elems) = value {
                assert_eq!(elems.len(), 0);
            } else {
                panic!("expected empty list literal");
            }
        } else {
            panic!("expected return");
        }
    }

    #[test]
    fn test_parse_bind_statement() {
        let src = "#fn f :i32\n  v0 :i32 = 42\n  = v0";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        assert_eq!(f.body.len(), 2);
        assert!(matches!(&f.body[0], Stmt::Bind { name, .. } if name == "v0"));
        assert!(matches!(&f.body[1], Stmt::Return { .. }));
    }

    #[test]
    fn test_parse_nested_select() {
        let src = "#fn f :i32 x:i32\n  = select (> x 10) 1 (select (> x 0) 0 -1)";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::Select { else_val, .. } = value {
                assert!(matches!(else_val.as_ref(), Expr::Select { .. }));
            } else {
                panic!("expected select");
            }
        } else {
            panic!("expected return");
        }
    }

    #[test]
    fn test_parse_map_iter() {
        let src = "#fn f :[i32] xs:[i32]\n  = map (fn x:i32 => + x 1) xs";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            assert!(matches!(value, Expr::MapIter { .. }));
        } else {
            panic!("expected return with MapIter");
        }
    }

    #[test]
    fn test_parse_fold_iter() {
        let src = "#fn f :i32 xs:[i32]\n  = fold xs 0 (fn a:i32 b:i32 => + a b)";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            assert!(matches!(value, Expr::FoldIter { .. }));
        } else {
            panic!("expected return with FoldIter");
        }
    }

    #[test]
    fn test_parse_filter_iter() {
        let src = "#fn f :[i32] xs:[i32]\n  = filter (fn x:i32 => > x 0) xs";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            assert!(matches!(value, Expr::FilterIter { .. }));
        } else {
            panic!("expected return with FilterIter");
        }
    }

    #[test]
    fn test_parse_cast() {
        let src = "#fn f :text x:i32\n  = cast text x";
        let prog = parse_source(src);
        let f = &prog.functions[0];
        if let Stmt::Return { value } = &f.body[0] {
            if let Expr::Cast { target, .. } = value {
                assert!(matches!(target, AiType::Text));
            } else {
                panic!("expected Cast");
            }
        } else {
            panic!("expected return");
        }
    }
}
