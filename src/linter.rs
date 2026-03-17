/// Static linter for AILang
/// Detects common anti-patterns that parse correctly but cause runtime errors.
/// Designed to catch the mistakes LLMs make most often.
///
/// Note: Many anti-patterns (e.g. `call map`, multi-line statements, `#` comments)
/// are already caught at parse time. This linter focuses on patterns that parse
/// successfully but fail or misbehave at runtime.

use crate::ast::*;

/// A lint warning with location info.
#[derive(Debug)]
pub struct LintWarning {
    pub rule: &'static str,
    pub message: String,
    pub function: String,
    pub line: usize,
}

impl std::fmt::Display for LintWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[lint:{}] in '{}' (line {}): {}",
            self.rule, self.function, self.line, self.message
        )
    }
}

/// Run all lint checks on a program and return warnings.
pub fn lint(program: &Program) -> Vec<LintWarning> {
    let mut warnings = Vec::new();

    for f in &program.functions {
        check_function(f, &mut warnings);
    }

    if let Some(entry) = &program.entry {
        check_body(&entry.body, "<entry>", entry.line, None, &mut warnings);
    }

    for t in &program.tests {
        let ctx = format!("<test {}>", t.name);
        check_body(&t.body, &ctx, t.line, None, &mut warnings);
    }

    warnings
}

fn check_function(f: &FnDecl, warnings: &mut Vec<LintWarning>) {
    check_body(&f.body, &f.name, f.line, Some(&f.name), warnings);
}

/// Check a block body for lint issues.
/// `self_fn` is Some(name) if we're inside a named function (for recursion detection).
fn check_body(
    body: &[Stmt],
    fn_name: &str,
    fn_line: usize,
    self_fn: Option<&str>,
    warnings: &mut Vec<LintWarning>,
) {
    for stmt in body {
        if let Stmt::Bind { value, .. } = stmt {
            // Check for recursive call in bind (not inside select/cond lazy branches)
            if let Some(self_name) = self_fn {
                if expr_has_unguarded_call(value, self_name) {
                    warnings.push(LintWarning {
                        rule: "recursive-call-in-bind",
                        message: format!(
                            "recursive call to '{}' in bind — will execute eagerly. Move inside select/cond branch.",
                            self_name
                        ),
                        function: fn_name.to_string(),
                        line: fn_line,
                    });
                }
            }
        }
    }
}

/// Check if an expression contains an unguarded call to `target_fn`.
/// "Unguarded" means NOT inside a select/cond/match branch (which are lazy).
/// Returns true if a direct recursive call is found outside lazy branches.
fn expr_has_unguarded_call(expr: &Expr, target_fn: &str) -> bool {
    match expr {
        Expr::Call { name, args } => {
            if name == target_fn {
                return true;
            }
            args.iter().any(|a| expr_has_unguarded_call(a, target_fn))
        }
        // select/cond/match branches are LAZY — recursive calls inside them are safe.
        // But the condition itself is eagerly evaluated.
        Expr::Select { cond, .. } => expr_has_unguarded_call(cond, target_fn),
        Expr::Cond { branches, .. } => {
            // Only the conditions are eager; values and default are lazy
            branches
                .iter()
                .any(|(c, _)| expr_has_unguarded_call(c, target_fn))
        }
        Expr::Match { value, .. } => expr_has_unguarded_call(value, target_fn),
        // Everything else: recurse into sub-expressions (all eager)
        Expr::BinOp { left, right, .. } => {
            expr_has_unguarded_call(left, target_fn)
                || expr_has_unguarded_call(right, target_fn)
        }
        Expr::UnaryOp { operand, .. } => expr_has_unguarded_call(operand, target_fn),
        Expr::MapIter { func, list }
        | Expr::FilterIter { func, list }
        | Expr::FlatMapIter { func, list } => {
            expr_has_unguarded_call(func, target_fn)
                || expr_has_unguarded_call(list, target_fn)
        }
        Expr::FoldIter { list, init, func } => {
            expr_has_unguarded_call(list, target_fn)
                || expr_has_unguarded_call(init, target_fn)
                || expr_has_unguarded_call(func, target_fn)
        }
        Expr::EachIter { list, func } => {
            expr_has_unguarded_call(list, target_fn)
                || expr_has_unguarded_call(func, target_fn)
        }
        Expr::ZipIter { list_a, list_b } => {
            expr_has_unguarded_call(list_a, target_fn)
                || expr_has_unguarded_call(list_b, target_fn)
        }
        Expr::Lambda { body, .. } => expr_has_unguarded_call(body, target_fn),
        Expr::TryExpr { value } => expr_has_unguarded_call(value, target_fn),
        Expr::Unwrap { value, default } => {
            expr_has_unguarded_call(value, target_fn)
                || expr_has_unguarded_call(default, target_fn)
        }
        Expr::OkWrap(inner) | Expr::Propagate(inner) => {
            expr_has_unguarded_call(inner, target_fn)
        }
        Expr::Cast { value, .. } => expr_has_unguarded_call(value, target_fn),
        Expr::Log { level, message, args } => {
            expr_has_unguarded_call(level, target_fn)
                || expr_has_unguarded_call(message, target_fn)
                || args.iter().any(|a| expr_has_unguarded_call(a, target_fn))
        }
        Expr::Assert { value } => expr_has_unguarded_call(value, target_fn),
        Expr::ToolCall { name, params } => {
            expr_has_unguarded_call(name, target_fn)
                || expr_has_unguarded_call(params, target_fn)
        }
        Expr::Construct { args, .. } => {
            args.iter().any(|a| expr_has_unguarded_call(a, target_fn))
        }
        Expr::FieldAccess { object, .. } => expr_has_unguarded_call(object, target_fn),
        Expr::ListLit(items) | Expr::TupleLit(items) => {
            items.iter().any(|i| expr_has_unguarded_call(i, target_fn))
        }
        Expr::MapLit(pairs) => pairs.iter().any(|(k, v)| {
            expr_has_unguarded_call(k, target_fn) || expr_has_unguarded_call(v, target_fn)
        }),
        // Leaf expressions
        Expr::IntLit(_) | Expr::FloatLit(_) | Expr::BoolLit(_) | Expr::TextLit(_)
        | Expr::NullLit | Expr::Var(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_program(source: &str) -> Program {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parser failed")
    }

    fn lint_source(source: &str) -> Vec<LintWarning> {
        let program = parse_program(source);
        lint(&program)
    }

    // -------------------------------------------------------
    // recursive-call-in-bind: recursion outside lazy branches
    // -------------------------------------------------------

    #[test]
    fn test_recursive_call_in_bind() {
        let warnings = lint_source(
            "#fn countdown :i32 n:i32\n  v0 :i32 = call countdown (- n 1)\n  = select (== n 0) 0 v0\n",
        );
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].rule, "recursive-call-in-bind");
        assert!(warnings[0].message.contains("countdown"));
    }

    #[test]
    fn test_no_warning_recursive_in_select() {
        let warnings = lint_source(
            "#fn countdown :i32 n:i32\n  = select (== n 0) 0 (call countdown (- n 1))\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_no_warning_recursive_in_cond() {
        let warnings = lint_source(
            "#fn classify :i32 n:i32\n  = cond (== n 0) 0 (> n 0) (call classify (- n 1)) -1\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_recursive_in_bind_nested_op() {
        // v0 = + acc (call sum ...) — recursive call inside a binop in a bind
        let warnings = lint_source(
            "#fn sum :i32 n:i32 acc:i32\n  v0 :i32 = + acc (call sum (- n 1) acc)\n  = select (== n 0) acc v0\n",
        );
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].rule, "recursive-call-in-bind");
    }

    #[test]
    fn test_no_warning_non_recursive_call_in_bind() {
        // Calling a DIFFERENT function in a bind is fine
        let warnings = lint_source(
            "#fn helper :i32 x:i32\n  = + x 1\n\n#fn main_fn :i32 n:i32\n  v0 :i32 = call helper n\n  = v0\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_recursive_in_bind_detected() {
        // Recursive call in the bind — even if later used in a select
        let warnings = lint_source(
            "#fn bad :bool n:i32\n  v0 :bool = call bad (- n 1)\n  = select v0 true false\n",
        );
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].rule, "recursive-call-in-bind");
    }

    #[test]
    fn test_clean_program_no_warnings() {
        let warnings = lint_source(
            "#fn fact :i32 n:i32\n  = select (<= n 1) 1 (* n (call fact (- n 1)))\n\n#test fact_5\n  assert == (call fact 5) 120\n\n#entry\n  v0 :i32 = call fact 10\n  = v0\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_no_warning_correct_builtin_call() {
        let warnings = lint_source(
            "#fn f :i32 nums:[i32]\n  v0 :i32 = call len nums\n  = v0\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_no_warning_correct_keyword_usage() {
        let warnings = lint_source(
            "#fn f :[i32] nums:[i32]\n  = map (fn x:i32 => * x 2) nums\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_recursive_in_select_branch_safe() {
        // Both branches of select contain recursive calls — this is fine (lazy)
        let warnings = lint_source(
            "#fn search :i32 lst:[i32] target:i32 i:i32\n  v0 :bool = >= i (call len lst)\n  = select v0 -1 (call search lst target (+ i 1))\n",
        );
        assert!(warnings.is_empty());
    }

    #[test]
    fn test_multiple_recursive_binds() {
        // Two separate binds, each with a recursive call
        let warnings = lint_source(
            "#fn bad :i32 n:i32\n  v0 :i32 = call bad (- n 1)\n  v1 :i32 = call bad (- n 2)\n  = select (== n 0) 0 (+ v0 v1)\n",
        );
        assert_eq!(warnings.len(), 2);
    }

    #[test]
    fn test_recursive_in_cond_condition_warns() {
        // Recursive call in a cond CONDITION (eager) — should warn
        // The bind has: select where cond contains the recursive call in the condition
        let warnings = lint_source(
            "#fn bad :i32 n:i32\n  v0 :i32 = call bad (- n 1)\n  = cond (== n 0) 0 (== v0 1) 2 3\n",
        );
        assert_eq!(warnings.len(), 1);
    }
}
