/// Static analysis warnings for AILang
/// Walks the AST and collects warnings for problematic patterns.
/// Currently checks for usage of the `any` type in type annotations.

use crate::ast::*;

/// Check for `any` type usage across the entire program and return warning messages.
pub fn check_any_warnings(program: &Program) -> Vec<String> {
    let mut warnings = Vec::new();

    // Check const declarations
    for c in &program.consts {
        if contains_any(&c.ty) {
            warnings.push(format!(
                "warning: use of :any type in const '{}' (line {})",
                c.name, c.line
            ));
        }
        walk_expr_for_any(&c.value, &format!("<const {}>", c.name), c.line, &mut warnings);
    }

    // Check function declarations
    for f in &program.functions {
        // Check return type
        if contains_any(&f.return_type) {
            warnings.push(format!(
                "warning: use of :any type in function '{}' return type (line {})",
                f.name, f.line
            ));
        }

        // Check parameter types
        for param in &f.params {
            if contains_any(&param.ty) {
                warnings.push(format!(
                    "warning: use of :any type in parameter '{}' of function '{}' (line {})",
                    param.name, f.name, f.line
                ));
            }
        }

        // Walk function body for binds, casts, lambdas
        walk_body_for_any(&f.body, &f.name, f.line, &mut warnings);
    }

    // Check test declarations
    for t in &program.tests {
        let context = format!("<test {}>", t.name);
        walk_body_for_any(&t.body, &context, t.line, &mut warnings);
    }

    // Check entry block
    if let Some(entry) = &program.entry {
        walk_body_for_any(&entry.body, "<entry>", entry.line, &mut warnings);
    }

    // Check extern declarations
    for ext in &program.externs {
        for ext_fn in &ext.functions {
            if contains_any(&ext_fn.return_type) {
                warnings.push(format!(
                    "warning: use of :any type in extern function '{}' return type (line {})",
                    ext_fn.name, ext_fn.line
                ));
            }
            for param in &ext_fn.params {
                if contains_any(&param.ty) {
                    warnings.push(format!(
                        "warning: use of :any type in parameter '{}' of extern function '{}' (line {})",
                        param.name, ext_fn.name, ext_fn.line
                    ));
                }
            }
        }
    }

    // Check type declarations (field types)
    for td in &program.types {
        for field in &td.fields {
            if contains_any(&field.ty) {
                warnings.push(format!(
                    "warning: use of :any type in field '{}' of type '{}' (line {})",
                    field.name, td.name, td.line
                ));
            }
        }
    }

    // Check enum declarations (variant field types)
    for ed in &program.enums {
        for variant in &ed.variants {
            for field_ty in &variant.fields {
                if contains_any(field_ty) {
                    warnings.push(format!(
                        "warning: use of :any type in variant '{}' of enum '{}' (line {})",
                        variant.name, ed.name, ed.line
                    ));
                }
            }
        }
    }

    warnings
}

/// Recursively check whether an `AiType` contains `Any` at any nesting level.
fn contains_any(ty: &AiType) -> bool {
    match ty {
        AiType::Any => true,
        AiType::List(inner) => contains_any(inner),
        AiType::Map(k, v) => contains_any(k) || contains_any(v),
        AiType::Tuple(items) => items.iter().any(contains_any),
        AiType::Optional(inner) => contains_any(inner),
        AiType::Result(inner) => contains_any(inner),
        AiType::Function(params, ret) => {
            params.iter().any(contains_any) || contains_any(ret)
        }
        // Primitive types and named types are fine
        AiType::I32
        | AiType::I64
        | AiType::F32
        | AiType::F64
        | AiType::Bool
        | AiType::Text
        | AiType::Byte
        | AiType::Void
        | AiType::Named(_) => false,
    }
}

/// Walk a list of statements looking for `any` type usage in binds, casts, and lambdas.
fn walk_body_for_any(body: &[Stmt], fn_name: &str, fn_line: usize, warnings: &mut Vec<String>) {
    for stmt in body {
        match stmt {
            Stmt::Bind { name, ty, value } => {
                if contains_any(ty) {
                    warnings.push(format!(
                        "warning: use of :any type in bind '{}' in function '{}' (line {})",
                        name, fn_name, fn_line
                    ));
                }
                walk_expr_for_any(value, fn_name, fn_line, warnings);
            }
            Stmt::Return { value } => {
                walk_expr_for_any(value, fn_name, fn_line, warnings);
            }
            Stmt::Emit { value } => {
                walk_expr_for_any(value, fn_name, fn_line, warnings);
            }
            Stmt::Effect { expr } => {
                walk_expr_for_any(expr, fn_name, fn_line, warnings);
            }
        }
    }
}

/// Walk an expression recursively looking for `Cast` and `Lambda` nodes containing `any`.
fn walk_expr_for_any(expr: &Expr, fn_name: &str, fn_line: usize, warnings: &mut Vec<String>) {
    match expr {
        Expr::Cast { target, value } => {
            if contains_any(target) {
                warnings.push(format!(
                    "warning: use of :any type in cast in function '{}' (line {})",
                    fn_name, fn_line
                ));
            }
            walk_expr_for_any(value, fn_name, fn_line, warnings);
        }
        Expr::Lambda { params, body } => {
            for param in params {
                if contains_any(&param.ty) {
                    warnings.push(format!(
                        "warning: use of :any type in lambda parameter '{}' in function '{}' (line {})",
                        param.name, fn_name, fn_line
                    ));
                }
            }
            walk_expr_for_any(body, fn_name, fn_line, warnings);
        }
        // Recurse into sub-expressions
        Expr::BinOp { left, right, .. } => {
            walk_expr_for_any(left, fn_name, fn_line, warnings);
            walk_expr_for_any(right, fn_name, fn_line, warnings);
        }
        Expr::UnaryOp { operand, .. } => {
            walk_expr_for_any(operand, fn_name, fn_line, warnings);
        }
        Expr::Call { args, .. } => {
            for arg in args {
                walk_expr_for_any(arg, fn_name, fn_line, warnings);
            }
        }
        Expr::Select { cond, then_val, else_val } => {
            walk_expr_for_any(cond, fn_name, fn_line, warnings);
            walk_expr_for_any(then_val, fn_name, fn_line, warnings);
            walk_expr_for_any(else_val, fn_name, fn_line, warnings);
        }
        Expr::Cond { branches, default } => {
            for (cond, val) in branches {
                walk_expr_for_any(cond, fn_name, fn_line, warnings);
                walk_expr_for_any(val, fn_name, fn_line, warnings);
            }
            walk_expr_for_any(default, fn_name, fn_line, warnings);
        }
        Expr::Match { value, arms } => {
            walk_expr_for_any(value, fn_name, fn_line, warnings);
            for arm in arms {
                walk_expr_for_any(&arm.body, fn_name, fn_line, warnings);
            }
        }
        Expr::MapIter { func, list } => {
            walk_expr_for_any(func, fn_name, fn_line, warnings);
            walk_expr_for_any(list, fn_name, fn_line, warnings);
        }
        Expr::FilterIter { func, list } => {
            walk_expr_for_any(func, fn_name, fn_line, warnings);
            walk_expr_for_any(list, fn_name, fn_line, warnings);
        }
        Expr::FoldIter { list, init, func } => {
            walk_expr_for_any(list, fn_name, fn_line, warnings);
            walk_expr_for_any(init, fn_name, fn_line, warnings);
            walk_expr_for_any(func, fn_name, fn_line, warnings);
        }
        Expr::EachIter { list, func } => {
            walk_expr_for_any(list, fn_name, fn_line, warnings);
            walk_expr_for_any(func, fn_name, fn_line, warnings);
        }
        Expr::ZipIter { list_a, list_b } => {
            walk_expr_for_any(list_a, fn_name, fn_line, warnings);
            walk_expr_for_any(list_b, fn_name, fn_line, warnings);
        }
        Expr::FlatMapIter { func, list } => {
            walk_expr_for_any(func, fn_name, fn_line, warnings);
            walk_expr_for_any(list, fn_name, fn_line, warnings);
        }
        Expr::TryExpr { value } => {
            walk_expr_for_any(value, fn_name, fn_line, warnings);
        }
        Expr::Unwrap { value, default } => {
            walk_expr_for_any(value, fn_name, fn_line, warnings);
            walk_expr_for_any(default, fn_name, fn_line, warnings);
        }
        Expr::OkWrap(inner) => {
            walk_expr_for_any(inner, fn_name, fn_line, warnings);
        }
        Expr::ToolCall { name, params } => {
            walk_expr_for_any(name, fn_name, fn_line, warnings);
            walk_expr_for_any(params, fn_name, fn_line, warnings);
        }
        Expr::Log { level, message, args } => {
            walk_expr_for_any(level, fn_name, fn_line, warnings);
            walk_expr_for_any(message, fn_name, fn_line, warnings);
            for arg in args {
                walk_expr_for_any(arg, fn_name, fn_line, warnings);
            }
        }
        Expr::Assert { value } => {
            walk_expr_for_any(value, fn_name, fn_line, warnings);
        }
        Expr::Construct { args, .. } => {
            for arg in args {
                walk_expr_for_any(arg, fn_name, fn_line, warnings);
            }
        }
        Expr::Propagate(inner) => {
            walk_expr_for_any(inner, fn_name, fn_line, warnings);
        }
        Expr::FieldAccess { object, .. } => {
            walk_expr_for_any(object, fn_name, fn_line, warnings);
        }
        Expr::ListLit(items) => {
            for item in items {
                walk_expr_for_any(item, fn_name, fn_line, warnings);
            }
        }
        Expr::MapLit(pairs) => {
            for (k, v) in pairs {
                walk_expr_for_any(k, fn_name, fn_line, warnings);
                walk_expr_for_any(v, fn_name, fn_line, warnings);
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                walk_expr_for_any(item, fn_name, fn_line, warnings);
            }
        }
        // Leaf expressions — no sub-expressions to check
        Expr::IntLit(_)
        | Expr::FloatLit(_)
        | Expr::BoolLit(_)
        | Expr::TextLit(_)
        | Expr::NullLit
        | Expr::Var(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_program() -> Program {
        Program {
            uses: vec![],
            externs: vec![],
            types: vec![],
            enums: vec![],
            consts: vec![],
            functions: vec![],
            tests: vec![],
            error_handlers: vec![],
            entry: None,
        }
    }

    #[test]
    fn test_contains_any_direct() {
        assert!(contains_any(&AiType::Any));
    }

    #[test]
    fn test_contains_any_nested_list() {
        assert!(contains_any(&AiType::List(Box::new(AiType::Any))));
    }

    #[test]
    fn test_contains_any_nested_map() {
        assert!(contains_any(&AiType::Map(
            Box::new(AiType::Text),
            Box::new(AiType::Any),
        )));
        assert!(contains_any(&AiType::Map(
            Box::new(AiType::Any),
            Box::new(AiType::I32),
        )));
    }

    #[test]
    fn test_contains_any_nested_tuple() {
        assert!(contains_any(&AiType::Tuple(vec![
            AiType::I32,
            AiType::Any,
            AiType::Bool,
        ])));
    }

    #[test]
    fn test_contains_any_nested_optional() {
        assert!(contains_any(&AiType::Optional(Box::new(AiType::Any))));
    }

    #[test]
    fn test_contains_any_nested_result() {
        assert!(contains_any(&AiType::Result(Box::new(AiType::Any))));
    }

    #[test]
    fn test_contains_any_nested_function() {
        assert!(contains_any(&AiType::Function(
            vec![AiType::I32, AiType::Any],
            Box::new(AiType::Bool),
        )));
        assert!(contains_any(&AiType::Function(
            vec![AiType::I32],
            Box::new(AiType::Any),
        )));
    }

    #[test]
    fn test_no_any_in_primitives() {
        assert!(!contains_any(&AiType::I32));
        assert!(!contains_any(&AiType::I64));
        assert!(!contains_any(&AiType::F32));
        assert!(!contains_any(&AiType::F64));
        assert!(!contains_any(&AiType::Bool));
        assert!(!contains_any(&AiType::Text));
        assert!(!contains_any(&AiType::Byte));
        assert!(!contains_any(&AiType::Void));
        assert!(!contains_any(&AiType::Named("Foo".to_string())));
    }

    #[test]
    fn test_no_any_nested() {
        assert!(!contains_any(&AiType::List(Box::new(AiType::I32))));
        assert!(!contains_any(&AiType::Map(
            Box::new(AiType::Text),
            Box::new(AiType::I32),
        )));
    }

    #[test]
    fn test_warn_function_return_type() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "process".to_string(),
            return_type: AiType::Any,
            params: vec![],
            body: vec![Stmt::Return {
                value: Expr::IntLit(0),
            }],
            line: 5,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("function 'process' return type"));
        assert!(warnings[0].contains("line 5"));
    }

    #[test]
    fn test_warn_function_param() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "transform".to_string(),
            return_type: AiType::I32,
            params: vec![Param {
                name: "x".to_string(),
                ty: AiType::Any,
            }],
            body: vec![Stmt::Return {
                value: Expr::Var("x".to_string()),
            }],
            line: 3,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("parameter 'x' of function 'transform'"));
        assert!(warnings[0].contains("line 3"));
    }

    #[test]
    fn test_warn_bind_any() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "compute".to_string(),
            return_type: AiType::I32,
            params: vec![],
            body: vec![
                Stmt::Bind {
                    name: "v0".to_string(),
                    ty: AiType::Any,
                    value: Expr::IntLit(42),
                },
                Stmt::Return {
                    value: Expr::Var("v0".to_string()),
                },
            ],
            line: 10,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("bind 'v0' in function 'compute'"));
        assert!(warnings[0].contains("line 10"));
    }

    #[test]
    fn test_warn_const_any() {
        let mut prog = empty_program();
        prog.consts.push(ConstDecl {
            name: "MY_CONST".to_string(),
            ty: AiType::Any,
            value: Expr::TextLit("hello".to_string()),
            line: 2,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("const 'MY_CONST'"));
        assert!(warnings[0].contains("line 2"));
    }

    #[test]
    fn test_warn_cast_any() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "convert".to_string(),
            return_type: AiType::I32,
            params: vec![],
            body: vec![Stmt::Return {
                value: Expr::Cast {
                    target: AiType::Any,
                    value: Box::new(Expr::IntLit(5)),
                },
            }],
            line: 7,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("cast in function 'convert'"));
        assert!(warnings[0].contains("line 7"));
    }

    #[test]
    fn test_warn_lambda_param_any() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "higher_order".to_string(),
            return_type: AiType::I32,
            params: vec![],
            body: vec![Stmt::Return {
                value: Expr::Lambda {
                    params: vec![Param {
                        name: "x".to_string(),
                        ty: AiType::Any,
                    }],
                    body: Box::new(Expr::Var("x".to_string())),
                },
            }],
            line: 12,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("lambda parameter 'x' in function 'higher_order'"));
        assert!(warnings[0].contains("line 12"));
    }

    #[test]
    fn test_warn_nested_any_in_list() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "list_fn".to_string(),
            return_type: AiType::List(Box::new(AiType::Any)),
            params: vec![],
            body: vec![Stmt::Return {
                value: Expr::ListLit(vec![]),
            }],
            line: 1,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("function 'list_fn' return type"));
    }

    #[test]
    fn test_no_warnings_for_typed_program() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "add".to_string(),
            return_type: AiType::I32,
            params: vec![
                Param {
                    name: "a".to_string(),
                    ty: AiType::I32,
                },
                Param {
                    name: "b".to_string(),
                    ty: AiType::I32,
                },
            ],
            body: vec![Stmt::Return {
                value: Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::Var("a".to_string())),
                    right: Box::new(Expr::Var("b".to_string())),
                },
            }],
            line: 1,
        });

        let warnings = check_any_warnings(&prog);
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_multiple_warnings() {
        let mut prog = empty_program();
        prog.functions.push(FnDecl {
            name: "process_data".to_string(),
            return_type: AiType::Any,
            params: vec![Param {
                name: "x".to_string(),
                ty: AiType::Any,
            }],
            body: vec![
                Stmt::Bind {
                    name: "v0".to_string(),
                    ty: AiType::Any,
                    value: Expr::Var("x".to_string()),
                },
                Stmt::Return {
                    value: Expr::Var("v0".to_string()),
                },
            ],
            line: 5,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 3);
        // Should have warnings for: return type, param, and bind
        assert!(warnings.iter().any(|w| w.contains("return type")));
        assert!(warnings.iter().any(|w| w.contains("parameter 'x'")));
        assert!(warnings.iter().any(|w| w.contains("bind 'v0'")));
    }

    #[test]
    fn test_warn_entry_block() {
        let mut prog = empty_program();
        prog.entry = Some(EntryBlock {
            body: vec![Stmt::Bind {
                name: "v0".to_string(),
                ty: AiType::Any,
                value: Expr::IntLit(1),
            }],
            line: 20,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("bind 'v0' in function '<entry>'"));
        assert!(warnings[0].contains("line 20"));
    }

    #[test]
    fn test_warn_test_block() {
        let mut prog = empty_program();
        prog.tests.push(TestDecl {
            name: "my_test".to_string(),
            body: vec![Stmt::Bind {
                name: "v0".to_string(),
                ty: AiType::Any,
                value: Expr::IntLit(1),
            }],
            line: 15,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("bind 'v0' in function '<test my_test>'"));
        assert!(warnings[0].contains("line 15"));
    }

    #[test]
    fn test_warn_type_decl_field() {
        let mut prog = empty_program();
        prog.types.push(TypeDecl {
            name: "MyStruct".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    ty: AiType::I32,
                },
                Field {
                    name: "data".to_string(),
                    ty: AiType::Any,
                },
            ],
            line: 3,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("field 'data' of type 'MyStruct'"));
        assert!(warnings[0].contains("line 3"));
    }

    #[test]
    fn test_warn_enum_variant_any() {
        let mut prog = empty_program();
        prog.enums.push(EnumDecl {
            name: "MyEnum".to_string(),
            variants: vec![EnumVariant {
                name: "Wrapper".to_string(),
                fields: vec![AiType::Any],
            }],
            line: 4,
        });

        let warnings = check_any_warnings(&prog);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("variant 'Wrapper' of enum 'MyEnum'"));
        assert!(warnings[0].contains("line 4"));
    }

    #[test]
    fn test_deeply_nested_any() {
        // :[[{text any}]]
        let ty = AiType::List(Box::new(AiType::List(Box::new(AiType::Map(
            Box::new(AiType::Text),
            Box::new(AiType::Any),
        )))));
        assert!(contains_any(&ty));
    }
}
