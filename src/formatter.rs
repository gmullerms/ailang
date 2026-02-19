/// Canonical formatter for AILang
/// Parses source to AST, then pretty-prints in canonical form.
///
/// Formatting rules:
/// 1. SSA variable renaming: intermediate variables become v0, v1, v2, ... per block
/// 2. 2-space indentation for all statements inside blocks
/// 3. Single space between tokens, no trailing whitespace, no extra blank lines
/// 4. One blank line between top-level blocks
/// 5. Sigils at column 0
/// 6. Block ordering: #use, #const, #type, #enum, #fn, #test, #err, #entry
///
/// Limitation: comments are NOT preserved (they are stripped during lexing/parsing).

use std::collections::HashMap;

use crate::ast::*;

/// Format an entire program AST back to canonical source text.
pub fn format_program(program: &Program) -> String {
    let mut blocks: Vec<String> = Vec::new();

    // 1. #use declarations
    for u in &program.uses {
        blocks.push(format_use(u));
    }

    // 2. #extern blocks
    for ext in &program.externs {
        blocks.push(format_extern(ext));
    }

    // 3. #const declarations
    for c in &program.consts {
        blocks.push(format_const(c));
    }

    // 4. #type declarations
    for t in &program.types {
        blocks.push(format_type_decl(t));
    }

    // 5. #enum declarations
    for e in &program.enums {
        blocks.push(format_enum_decl(e));
    }

    // 6. #fn declarations
    for f in &program.functions {
        blocks.push(format_fn(f));
    }

    // 7. #test blocks
    for t in &program.tests {
        blocks.push(format_test(t));
    }

    // 8. #err handlers
    for e in &program.error_handlers {
        blocks.push(format_err_handler(e));
    }

    // 9. #entry block (last)
    if let Some(entry) = &program.entry {
        blocks.push(format_entry(entry));
    }

    // Join blocks with exactly one blank line between them
    let result = blocks.join("\n\n");
    // Ensure file ends with a single newline
    if result.is_empty() {
        String::new()
    } else {
        format!("{}\n", result)
    }
}

// ---------------------------------------------------------------------------
// Top-level block formatters
// ---------------------------------------------------------------------------

fn format_use(u: &UseDecl) -> String {
    match &u.names {
        Some(names) => {
            let names_str = names.join(" ");
            format!("#use \"{}\" {{{}}}", escape_string(&u.path), names_str)
        }
        None => format!("#use \"{}\"", escape_string(&u.path)),
    }
}

fn format_extern(ext: &ExternBlock) -> String {
    let mut lines = vec![format!("#extern \"{}\"", escape_string(&ext.lib_name))];
    for f in &ext.functions {
        let mut sig = format!("  {} :{}", f.name, format_type(&f.return_type));
        for p in &f.params {
            sig.push_str(&format!(" {}:{}", p.name, format_type(&p.ty)));
        }
        lines.push(sig);
    }
    lines.join("\n")
}

fn format_const(c: &ConstDecl) -> String {
    let renames = HashMap::new();
    format!(
        "#const {} :{} = {}",
        c.name,
        format_type(&c.ty),
        format_expr(&c.value, &renames)
    )
}

fn format_type_decl(t: &TypeDecl) -> String {
    let mut lines = vec![format!("#type {}", t.name)];
    for field in &t.fields {
        lines.push(format!("  {} :{}", field.name, format_type(&field.ty)));
    }
    lines.join("\n")
}

fn format_enum_decl(e: &EnumDecl) -> String {
    let mut lines = vec![format!("#enum {}", e.name)];
    for variant in &e.variants {
        if variant.fields.is_empty() {
            lines.push(format!("  {}", variant.name));
        } else {
            let types: Vec<String> = variant.fields.iter().map(|t| format!(":{}", format_type(t))).collect();
            lines.push(format!("  {} {}", variant.name, types.join(" ")));
        }
    }
    lines.join("\n")
}

fn format_fn(f: &FnDecl) -> String {
    // Header: #fn name :RetType param1:Type1 param2:Type2
    let mut header = format!("#fn {} :{}", f.name, format_type(&f.return_type));
    for p in &f.params {
        header.push_str(&format!(" {}:{}", p.name, format_type(&p.ty)));
    }

    // Body with SSA renaming
    let param_names: Vec<String> = f.params.iter().map(|p| p.name.clone()).collect();
    let body = format_block(&f.body, &param_names);

    if body.is_empty() {
        header
    } else {
        format!("{}\n{}", header, body)
    }
}

fn format_test(t: &TestDecl) -> String {
    let header = format!("#test {}", t.name);
    let body = format_block(&t.body, &[]);
    if body.is_empty() {
        header
    } else {
        format!("{}\n{}", header, body)
    }
}

fn format_err_handler(e: &ErrHandler) -> String {
    let mut lines = vec![format!("#err {}", e.fn_name)];
    if e.retry_count.is_some() || e.retry_delay_ms.is_some() {
        let count = e.retry_count.unwrap_or(0);
        let delay = e.retry_delay_ms.unwrap_or(0);
        lines.push(format!("  retry {} {}", count, delay));
    }
    if let Some(ref fallback) = e.fallback {
        let renames = HashMap::new();
        lines.push(format!("  fallback {}", format_expr(fallback, &renames)));
    }
    lines.join("\n")
}

fn format_entry(entry: &EntryBlock) -> String {
    let header = "#entry".to_string();
    let body = format_block(&entry.body, &[]);
    if body.is_empty() {
        header
    } else {
        format!("{}\n{}", header, body)
    }
}

// ---------------------------------------------------------------------------
// Block body formatting with SSA renaming
// ---------------------------------------------------------------------------

/// Format a block body (list of statements) with SSA variable renaming.
/// `param_names` are the parameter names that should NOT be renamed.
/// Returns the formatted block body (indented lines joined by newlines).
fn format_block(body: &[Stmt], param_names: &[String]) -> String {
    let mut renames: HashMap<String, String> = HashMap::new();
    let mut counter: usize = 0;
    let mut lines: Vec<String> = Vec::new();

    for stmt in body {
        lines.push(format_stmt(stmt, &mut renames, &mut counter, param_names));
    }

    lines.join("\n")
}

/// Format a single statement, updating the rename map for Bind statements.
fn format_stmt(
    stmt: &Stmt,
    renames: &mut HashMap<String, String>,
    counter: &mut usize,
    _param_names: &[String],
) -> String {
    match stmt {
        Stmt::Bind { name, ty, value } => {
            // Assign SSA name
            let new_name = format!("v{}", counter);
            *counter += 1;
            // Only add to renames if the original name differs
            if name != &new_name {
                renames.insert(name.clone(), new_name.clone());
            }
            format!(
                "  {} :{} = {}",
                new_name,
                format_type(ty),
                format_expr(value, renames)
            )
        }
        Stmt::Return { value } => {
            format!("  = {}", format_expr(value, renames))
        }
        Stmt::Emit { value } => {
            format!("  > {}", format_expr(value, renames))
        }
        Stmt::Effect { expr } => {
            format!("  {}", format_expr(expr, renames))
        }
    }
}

// ---------------------------------------------------------------------------
// Type formatting
// ---------------------------------------------------------------------------

/// Format an AiType to its canonical source representation.
fn format_type(ty: &AiType) -> String {
    match ty {
        AiType::I32 => "i32".to_string(),
        AiType::I64 => "i64".to_string(),
        AiType::F32 => "f32".to_string(),
        AiType::F64 => "f64".to_string(),
        AiType::Bool => "bool".to_string(),
        AiType::Text => "text".to_string(),
        AiType::Byte => "byte".to_string(),
        AiType::Void => "void".to_string(),
        AiType::Any => "any".to_string(),
        AiType::List(inner) => format!("[{}]", format_type(inner)),
        AiType::Map(k, v) => format!("{{{}:{}}}", format_type(k), format_type(v)),
        AiType::Tuple(types) => {
            let parts: Vec<String> = types.iter().map(|t| format_type(t)).collect();
            format!("({})", parts.join(" "))
        }
        AiType::Optional(inner) => format!("?{}", format_type(inner)),
        AiType::Result(inner) => format!("!{}", format_type(inner)),
        AiType::Function(params, ret) => {
            let parts: Vec<String> = params.iter().map(|t| format_type(t)).collect();
            format!("({} -> {})", parts.join(" "), format_type(ret))
        }
        AiType::Named(name) => name.clone(),
    }
}

// ---------------------------------------------------------------------------
// Expression formatting
// ---------------------------------------------------------------------------

/// Format an expression, applying variable renames from the rename map.
fn format_expr(expr: &Expr, renames: &HashMap<String, String>) -> String {
    match expr {
        Expr::IntLit(n) => format!("{}", n),
        Expr::FloatLit(f) => format_float(*f),
        Expr::BoolLit(b) => format!("{}", b),
        Expr::TextLit(s) => format!("\"{}\"", escape_string(s)),
        Expr::NullLit => "null".to_string(),

        Expr::Var(name) => {
            if let Some(renamed) = renames.get(name) {
                renamed.clone()
            } else {
                name.clone()
            }
        }

        Expr::FieldAccess { object, field } => {
            format!("{}.{}", format_expr(object, renames), field)
        }

        Expr::ListLit(elements) => {
            let parts: Vec<String> = elements.iter().map(|e| format_atom(e, renames)).collect();
            format!("[{}]", parts.join(" "))
        }

        Expr::MapLit(pairs) => {
            let parts: Vec<String> = pairs
                .iter()
                .flat_map(|(k, v)| vec![format_atom(k, renames), format_atom(v, renames)])
                .collect();
            format!("{{{}}}", parts.join(" "))
        }

        Expr::TupleLit(elements) => {
            let parts: Vec<String> = elements.iter().map(|e| format_atom(e, renames)).collect();
            format!("({})", parts.join(" "))
        }

        Expr::BinOp { op, left, right } => {
            format!(
                "{} {} {}",
                format_binop_kind(op),
                format_arg(left, renames),
                format_arg(right, renames)
            )
        }

        Expr::UnaryOp { op, operand } => {
            format!(
                "{} {}",
                format_unaryop_kind(op),
                format_arg(operand, renames)
            )
        }

        Expr::Call { name, args } => {
            if args.is_empty() {
                format!("call {}", name)
            } else {
                let parts: Vec<String> = args.iter().map(|a| format_atom(a, renames)).collect();
                format!("call {} {}", name, parts.join(" "))
            }
        }

        Expr::Select { cond, then_val, else_val } => {
            format!(
                "select {} {} {}",
                format_arg(cond, renames),
                format_arg(then_val, renames),
                format_arg(else_val, renames)
            )
        }

        Expr::Cond { branches, default } => {
            let mut parts = Vec::new();
            for (cond, val) in branches {
                parts.push(format_arg(cond, renames));
                parts.push(format_arg(val, renames));
            }
            parts.push(format_arg(default, renames));
            format!("cond {}", parts.join(" "))
        }

        Expr::Match { value, arms } => {
            let mut lines = vec![format!("match {}", format_atom(value, renames))];
            for arm in arms {
                let pattern_str = format_pattern(&arm.pattern, renames);
                let body_str = format_expr(&arm.body, renames);
                lines.push(format!("  {} => {}", pattern_str, body_str));
            }
            lines.join("\n")
        }

        Expr::MapIter { func, list } => {
            format!("map {} {}", format_atom(func, renames), format_atom(list, renames))
        }

        Expr::FilterIter { func, list } => {
            format!("filter {} {}", format_atom(func, renames), format_atom(list, renames))
        }

        Expr::FoldIter { list, init, func } => {
            format!(
                "fold {} {} {}",
                format_atom(list, renames),
                format_atom(init, renames),
                format_atom(func, renames)
            )
        }

        Expr::EachIter { list, func } => {
            format!("each {} {}", format_atom(list, renames), format_atom(func, renames))
        }

        Expr::ZipIter { list_a, list_b } => {
            format!("zip {} {}", format_atom(list_a, renames), format_atom(list_b, renames))
        }

        Expr::FlatMapIter { func, list } => {
            format!("flatmap {} {}", format_atom(func, renames), format_atom(list, renames))
        }

        Expr::Cast { target, value } => {
            format!("cast {} {}", format_type(target), format_atom(value, renames))
        }

        Expr::TryExpr { value } => {
            format!("try {}", format_expr(value, renames))
        }

        Expr::Unwrap { value, default } => {
            format!(
                "unwrap {} {}",
                format_arg(value, renames),
                format_arg(default, renames)
            )
        }

        Expr::OkWrap(value) => {
            format!("ok {}", format_arg(value, renames))
        }

        Expr::ToolCall { name, params } => {
            format!("tool {} {}", format_atom(name, renames), format_atom(params, renames))
        }

        Expr::Log { level, message, args } => {
            let mut parts = vec![
                "log".to_string(),
                format_atom(level, renames),
                format_atom(message, renames),
            ];
            for arg in args {
                parts.push(format_atom(arg, renames));
            }
            parts.join(" ")
        }

        Expr::Assert { value } => {
            format!("assert {}", format_expr(value, renames))
        }

        Expr::Construct { type_name, args } => {
            if args.is_empty() {
                type_name.clone()
            } else {
                let parts: Vec<String> = args.iter().map(|a| format_atom(a, renames)).collect();
                format!("{} {}", type_name, parts.join(" "))
            }
        }

        Expr::Lambda { params, body } => {
            let params_str: Vec<String> = params
                .iter()
                .map(|p| format!("{}:{}", p.name, format_type(&p.ty)))
                .collect();
            format!(
                "(fn {} => {})",
                params_str.join(" "),
                format_expr(body, renames)
            )
        }

        Expr::Propagate(inner) => {
            format!("{}?", format_atom(inner, renames))
        }
    }
}

/// Format an expression as an "atom" for use as an argument.
/// Compound expressions get wrapped in parentheses, while atomic
/// expressions (literals, variables, list/map/tuple literals) do not.
fn format_atom(expr: &Expr, renames: &HashMap<String, String>) -> String {
    if needs_grouping(expr) {
        format!("({})", format_expr(expr, renames))
    } else {
        format_expr(expr, renames)
    }
}

/// Format an expression as an argument to a prefix operator or keyword.
/// Same as format_atom -- compound sub-expressions need grouping.
fn format_arg(expr: &Expr, renames: &HashMap<String, String>) -> String {
    format_atom(expr, renames)
}

/// Determine if an expression needs to be wrapped in parentheses when used
/// as an argument to another expression.
fn needs_grouping(expr: &Expr) -> bool {
    match expr {
        // Atomic / self-delimiting -- no grouping needed
        Expr::IntLit(_)
        | Expr::FloatLit(_)
        | Expr::BoolLit(_)
        | Expr::TextLit(_)
        | Expr::NullLit
        | Expr::Var(_)
        | Expr::ListLit(_)
        | Expr::MapLit(_)
        | Expr::TupleLit(_)
        | Expr::FieldAccess { .. }
        | Expr::Lambda { .. } => false,

        // Propagate wraps with ? postfix and is already atomic-ish
        // (its inner expr is already format_atom'd)
        Expr::Propagate(_) => false,

        // Everything else is compound and needs grouping when used as argument
        _ => true,
    }
}

/// Format a match pattern.
fn format_pattern(pattern: &Pattern, renames: &HashMap<String, String>) -> String {
    match pattern {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Literal(expr) => format_expr(expr, renames),
        Pattern::Variant { name, bindings } => {
            if bindings.is_empty() {
                name.clone()
            } else {
                format!("{} {}", name, bindings.join(" "))
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Operator formatting
// ---------------------------------------------------------------------------

fn format_binop_kind(op: &BinOpKind) -> &'static str {
    match op {
        BinOpKind::Add => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Mod => "%",
        BinOpKind::Eq => "==",
        BinOpKind::Neq => "!=",
        BinOpKind::Lt => "<",
        BinOpKind::Gt => ">",
        BinOpKind::Lte => "<=",
        BinOpKind::Gte => ">=",
        BinOpKind::And => "and",
        BinOpKind::Or => "or",
        BinOpKind::Band => "band",
        BinOpKind::Bor => "bor",
        BinOpKind::Bxor => "bxor",
        BinOpKind::Shl => "shl",
        BinOpKind::Shr => "shr",
    }
}

fn format_unaryop_kind(op: &UnaryOpKind) -> &'static str {
    match op {
        UnaryOpKind::Neg => "neg",
        UnaryOpKind::Not => "not",
        UnaryOpKind::Bnot => "bnot",
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Format a float value, ensuring it always has a decimal point.
fn format_float(f: f64) -> String {
    let s = format!("{}", f);
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

/// Escape special characters in a string for source output.
/// Note: braces `{` and `}` are NOT escaped because the AILang lexer
/// accepts them unescaped in string literals. They are only special
/// as escape sequences (`\{`, `\}`), not as raw characters.
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            other => result.push(other),
        }
    }
    result
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    /// Helper: lex + parse source, return the Program
    fn parse_source(source: &str) -> Program {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parser failed")
    }

    /// Helper: parse and format a source string
    fn format_source(source: &str) -> String {
        let program = parse_source(source);
        format_program(&program)
    }

    // ---------------------------------------------------------------
    // Type formatting
    // ---------------------------------------------------------------

    #[test]
    fn test_format_primitive_types() {
        assert_eq!(format_type(&AiType::I32), "i32");
        assert_eq!(format_type(&AiType::I64), "i64");
        assert_eq!(format_type(&AiType::F32), "f32");
        assert_eq!(format_type(&AiType::F64), "f64");
        assert_eq!(format_type(&AiType::Bool), "bool");
        assert_eq!(format_type(&AiType::Text), "text");
        assert_eq!(format_type(&AiType::Byte), "byte");
        assert_eq!(format_type(&AiType::Void), "void");
        assert_eq!(format_type(&AiType::Any), "any");
    }

    #[test]
    fn test_format_composite_types() {
        assert_eq!(
            format_type(&AiType::List(Box::new(AiType::I32))),
            "[i32]"
        );
        assert_eq!(
            format_type(&AiType::Map(Box::new(AiType::Text), Box::new(AiType::I32))),
            "{text:i32}"
        );
        assert_eq!(
            format_type(&AiType::Optional(Box::new(AiType::Text))),
            "?text"
        );
        assert_eq!(
            format_type(&AiType::Tuple(vec![AiType::I32, AiType::Text])),
            "(i32 text)"
        );
        assert_eq!(
            format_type(&AiType::Function(vec![AiType::I32, AiType::I32], Box::new(AiType::Bool))),
            "(i32 i32 -> bool)"
        );
        assert_eq!(
            format_type(&AiType::Named("MyType".to_string())),
            "MyType"
        );
    }

    // ---------------------------------------------------------------
    // Expression formatting
    // ---------------------------------------------------------------

    #[test]
    fn test_format_literals() {
        let renames = HashMap::new();
        assert_eq!(format_expr(&Expr::IntLit(42), &renames), "42");
        assert_eq!(format_expr(&Expr::IntLit(-7), &renames), "-7");
        assert_eq!(format_expr(&Expr::FloatLit(3.14), &renames), "3.14");
        assert_eq!(format_expr(&Expr::BoolLit(true), &renames), "true");
        assert_eq!(format_expr(&Expr::BoolLit(false), &renames), "false");
        assert_eq!(
            format_expr(&Expr::TextLit("hello".to_string()), &renames),
            "\"hello\""
        );
        assert_eq!(format_expr(&Expr::NullLit, &renames), "null");
    }

    #[test]
    fn test_format_var_with_rename() {
        let mut renames = HashMap::new();
        renames.insert("myvar".to_string(), "v0".to_string());
        assert_eq!(format_expr(&Expr::Var("myvar".to_string()), &renames), "v0");
        assert_eq!(
            format_expr(&Expr::Var("other".to_string()), &renames),
            "other"
        );
    }

    #[test]
    fn test_format_binop() {
        let renames = HashMap::new();
        let expr = Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(Expr::Var("a".to_string())),
            right: Box::new(Expr::Var("b".to_string())),
        };
        assert_eq!(format_expr(&expr, &renames), "+ a b");
    }

    #[test]
    fn test_format_nested_binop_grouping() {
        let renames = HashMap::new();
        let inner = Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(Expr::Var("a".to_string())),
            right: Box::new(Expr::IntLit(1)),
        };
        let outer = Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(inner),
            right: Box::new(Expr::Var("b".to_string())),
        };
        assert_eq!(format_expr(&outer, &renames), "* (+ a 1) b");
    }

    #[test]
    fn test_format_call_no_args() {
        let renames = HashMap::new();
        let expr = Expr::Call {
            name: "foo".to_string(),
            args: vec![],
        };
        assert_eq!(format_expr(&expr, &renames), "call foo");
    }

    #[test]
    fn test_format_call_with_args() {
        let renames = HashMap::new();
        let expr = Expr::Call {
            name: "add".to_string(),
            args: vec![Expr::IntLit(2), Expr::IntLit(3)],
        };
        assert_eq!(format_expr(&expr, &renames), "call add 2 3");
    }

    #[test]
    fn test_format_select() {
        let renames = HashMap::new();
        let expr = Expr::Select {
            cond: Box::new(Expr::BoolLit(true)),
            then_val: Box::new(Expr::IntLit(1)),
            else_val: Box::new(Expr::IntLit(0)),
        };
        assert_eq!(format_expr(&expr, &renames), "select true 1 0");
    }

    #[test]
    fn test_format_lambda() {
        let renames = HashMap::new();
        let expr = Expr::Lambda {
            params: vec![Param {
                name: "x".to_string(),
                ty: AiType::I32,
            }],
            body: Box::new(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::Var("x".to_string())),
                right: Box::new(Expr::Var("x".to_string())),
            }),
        };
        assert_eq!(format_expr(&expr, &renames), "(fn x:i32 => * x x)");
    }

    #[test]
    fn test_format_list_lit() {
        let renames = HashMap::new();
        let expr = Expr::ListLit(vec![Expr::IntLit(1), Expr::IntLit(2), Expr::IntLit(3)]);
        assert_eq!(format_expr(&expr, &renames), "[1 2 3]");
    }

    #[test]
    fn test_format_map_lit() {
        let renames = HashMap::new();
        let expr = Expr::MapLit(vec![(
            Expr::TextLit("key".to_string()),
            Expr::IntLit(42),
        )]);
        assert_eq!(format_expr(&expr, &renames), "{\"key\" 42}");
    }

    #[test]
    fn test_format_propagate() {
        let renames = HashMap::new();
        let expr = Expr::Propagate(Box::new(Expr::Call {
            name: "safe_divide".to_string(),
            args: vec![Expr::Var("x".to_string()), Expr::FloatLit(0.0)],
        }));
        assert_eq!(
            format_expr(&expr, &renames),
            "(call safe_divide x 0.0)?"
        );
    }

    #[test]
    fn test_format_cond() {
        let renames = HashMap::new();
        let expr = Expr::Cond {
            branches: vec![
                (
                    Box::new(Expr::BinOp {
                        op: BinOpKind::Eq,
                        left: Box::new(Expr::Var("x".to_string())),
                        right: Box::new(Expr::IntLit(0)),
                    }),
                    Box::new(Expr::TextLit("zero".to_string())),
                ),
            ],
            default: Box::new(Expr::TextLit("other".to_string())),
        };
        assert_eq!(
            format_expr(&expr, &renames),
            "cond (== x 0) \"zero\" \"other\""
        );
    }

    // ---------------------------------------------------------------
    // SSA renaming
    // ---------------------------------------------------------------

    #[test]
    fn test_ssa_renaming_basic() {
        let src = "#fn add :i32 a:i32 b:i32\n  v0 :i32 = + a b\n  = v0";
        let result = format_source(src);
        assert!(result.contains("  v0 :i32 = + a b"));
        assert!(result.contains("  = v0"));
    }

    #[test]
    fn test_ssa_renaming_preserves_params() {
        // Parameters keep their original names
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b";
        let result = format_source(src);
        assert!(result.contains("= + a b"));
    }

    #[test]
    fn test_ssa_renaming_multiple_binds() {
        let src = "#fn f :i32 x:i32\n  v0 :i32 = + x 1\n  v1 :i32 = * v0 2\n  = v1";
        let result = format_source(src);
        assert!(result.contains("  v0 :i32 = + x 1"));
        assert!(result.contains("  v1 :i32 = * v0 2"));
        assert!(result.contains("  = v1"));
    }

    // ---------------------------------------------------------------
    // Block ordering
    // ---------------------------------------------------------------

    #[test]
    fn test_block_ordering() {
        // Source has entry before fn -- output should reorder
        let src = "#entry\n  = 0\n\n#fn add :i32 a:i32 b:i32\n  = + a b";
        let result = format_source(src);
        let fn_pos = result.find("#fn").unwrap();
        let entry_pos = result.find("#entry").unwrap();
        assert!(
            fn_pos < entry_pos,
            "Expected #fn before #entry, but #fn at {} and #entry at {}",
            fn_pos,
            entry_pos
        );
    }

    #[test]
    fn test_block_ordering_use_first() {
        let src = "#fn f :i32\n  = 1\n\n#use \"foo\"";
        let result = format_source(src);
        let use_pos = result.find("#use").unwrap();
        let fn_pos = result.find("#fn").unwrap();
        assert!(
            use_pos < fn_pos,
            "Expected #use before #fn"
        );
    }

    #[test]
    fn test_block_ordering_const_before_fn() {
        let src = "#fn f :i32\n  = PI\n\n#const PI :f64 = 3.14";
        let result = format_source(src);
        let const_pos = result.find("#const").unwrap();
        let fn_pos = result.find("#fn").unwrap();
        assert!(
            const_pos < fn_pos,
            "Expected #const before #fn"
        );
    }

    // ---------------------------------------------------------------
    // End-to-end formatting
    // ---------------------------------------------------------------

    #[test]
    fn test_format_simple_function() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b";
        let result = format_source(src);
        assert_eq!(result, "#fn add :i32 a:i32 b:i32\n  = + a b\n");
    }

    #[test]
    fn test_format_function_with_binds() {
        let src = "#fn f :i32 x:i32\n  v0 :i32 = + x 1\n  v1 :i32 = * v0 2\n  = v1";
        let result = format_source(src);
        assert_eq!(
            result,
            "#fn f :i32 x:i32\n  v0 :i32 = + x 1\n  v1 :i32 = * v0 2\n  = v1\n"
        );
    }

    #[test]
    fn test_format_const_block() {
        let src = "#const PI :f64 = 3.14";
        let result = format_source(src);
        assert_eq!(result, "#const PI :f64 = 3.14\n");
    }

    #[test]
    fn test_format_test_block() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#test add_basic\n  v0 :i32 = call add 2 3\n  assert == v0 5";
        let result = format_source(src);
        assert!(result.contains("#fn add :i32 a:i32 b:i32\n  = + a b"));
        assert!(result.contains("#test add_basic\n  v0 :i32 = call add 2 3\n  assert == v0 5"));
    }

    #[test]
    fn test_format_err_handler() {
        let src = "#fn f :i32\n  = 1\n\n#err f\n  fallback -1";
        let result = format_source(src);
        assert!(result.contains("#err f\n  fallback -1"));
    }

    #[test]
    fn test_format_entry_block() {
        let src = "#entry\n  = 0";
        let result = format_source(src);
        assert_eq!(result, "#entry\n  = 0\n");
    }

    #[test]
    fn test_format_use_without_names() {
        let src = "#use \"math_helpers\"";
        let result = format_source(src);
        assert_eq!(result, "#use \"math_helpers\"\n");
    }

    #[test]
    fn test_format_use_with_names() {
        let src = "#use \"std/math\" {PI pow_i clamp_i}";
        let result = format_source(src);
        assert_eq!(result, "#use \"std/math\" {PI pow_i clamp_i}\n");
    }

    #[test]
    fn test_format_emit() {
        let src = "#entry\n  > \"hello\"";
        let result = format_source(src);
        assert_eq!(result, "#entry\n  > \"hello\"\n");
    }

    #[test]
    fn test_format_lambda_as_arg() {
        let src = "#fn f :[i32] xs:[i32]\n  = map (fn x:i32 => + x 1) xs";
        let result = format_source(src);
        assert!(result.contains("map (fn x:i32 => + x 1) xs"));
    }

    #[test]
    fn test_format_select_with_compound_args() {
        let src = "#fn f :i32 x:i32\n  = select (> x 0) x (neg x)";
        let result = format_source(src);
        assert!(result.contains("select (> x 0) x (neg x)"));
    }

    #[test]
    fn test_format_blank_line_between_blocks() {
        let src = "#fn a :i32\n  = 1\n\n#fn b :i32\n  = 2";
        let result = format_source(src);
        assert!(result.contains("#fn a :i32\n  = 1\n\n#fn b :i32\n  = 2"));
    }

    #[test]
    fn test_format_string_escaping() {
        let renames = HashMap::new();
        let expr = Expr::TextLit("line\nbreak".to_string());
        assert_eq!(format_expr(&expr, &renames), "\"line\\nbreak\"");
    }

    #[test]
    fn test_format_fold_iter() {
        let src = "#fn f :i32 xs:[i32]\n  = fold xs 0 (fn a:i32 b:i32 => + a b)";
        let result = format_source(src);
        assert!(result.contains("fold xs 0 (fn a:i32 b:i32 => + a b)"));
    }

    #[test]
    fn test_format_cast() {
        let src = "#fn f :text x:i32\n  = cast text x";
        let result = format_source(src);
        assert!(result.contains("cast text x"));
    }

    #[test]
    fn test_format_error_propagation() {
        let src = "#fn f :f64 x:f64\n  v0 :f64 = (call safe_divide x 0.0)?\n  = + v0 1.0";
        let result = format_source(src);
        assert!(result.contains("(call safe_divide x 0.0)?"));
    }

    #[test]
    fn test_format_grouped_call_in_args() {
        let src = "#fn f :i32 a:i32\n  = call foo (+ a 1)";
        let result = format_source(src);
        assert!(result.contains("call foo (+ a 1)"));
    }

    // ---------------------------------------------------------------
    // Idempotency: format(format(x)) == format(x)
    // ---------------------------------------------------------------

    #[test]
    fn test_format_idempotent() {
        let src = "#fn add :i32 a:i32 b:i32\n  v0 :i32 = + a b\n  = v0\n\n#test add_basic\n  v0 :i32 = call add 2 3\n  assert == v0 5\n\n#entry\n  v0 :i32 = call add 1 2\n  = v0";
        let first = format_source(src);
        let second = format_source(&first);
        assert_eq!(first, second, "Formatter should be idempotent");
    }

    #[test]
    fn test_format_idempotent_complex() {
        let src = "#use \"std/math\" {PI}\n\n#const MAX :i32 = 100\n\n#fn compute :i32 x:i32\n  v0 :i32 = + x 1\n  v1 :i32 = * v0 2\n  = v1\n\n#test compute_basic\n  v0 :i32 = call compute 5\n  assert == v0 12\n\n#entry\n  v0 :i32 = call compute 10\n  = v0";
        let first = format_source(src);
        let second = format_source(&first);
        assert_eq!(first, second, "Formatter should be idempotent");
    }

    // ---------------------------------------------------------------
    // Needs-grouping tests
    // ---------------------------------------------------------------

    #[test]
    fn test_needs_grouping() {
        assert!(!needs_grouping(&Expr::IntLit(1)));
        assert!(!needs_grouping(&Expr::Var("x".to_string())));
        assert!(!needs_grouping(&Expr::TextLit("hi".to_string())));
        assert!(!needs_grouping(&Expr::ListLit(vec![])));
        assert!(!needs_grouping(&Expr::MapLit(vec![])));
        assert!(!needs_grouping(&Expr::BoolLit(true)));
        assert!(!needs_grouping(&Expr::NullLit));

        assert!(needs_grouping(&Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(Expr::IntLit(1)),
            right: Box::new(Expr::IntLit(2)),
        }));
        assert!(needs_grouping(&Expr::Call {
            name: "foo".to_string(),
            args: vec![],
        }));
        assert!(needs_grouping(&Expr::Select {
            cond: Box::new(Expr::BoolLit(true)),
            then_val: Box::new(Expr::IntLit(1)),
            else_val: Box::new(Expr::IntLit(0)),
        }));
    }

    // ---------------------------------------------------------------
    // Float formatting edge cases
    // ---------------------------------------------------------------

    #[test]
    fn test_format_float_values() {
        assert_eq!(format_float(3.14), "3.14");
        assert_eq!(format_float(0.0), "0.0");
        assert_eq!(format_float(-1.5), "-1.5");
        assert_eq!(format_float(1.0), "1.0");
    }

    // ---------------------------------------------------------------
    // String escaping
    // ---------------------------------------------------------------

    #[test]
    fn test_escape_string() {
        assert_eq!(escape_string("hello"), "hello");
        assert_eq!(escape_string("line\nbreak"), "line\\nbreak");
        assert_eq!(escape_string("tab\there"), "tab\\there");
        assert_eq!(escape_string("quote\"here"), "quote\\\"here");
        assert_eq!(escape_string("back\\slash"), "back\\\\slash");
        // Braces are NOT escaped (they are valid unescaped in AILang strings)
        assert_eq!(escape_string("{braces}"), "{braces}");
    }
}
