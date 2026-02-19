/// Interpreter for AILang
/// Tree-walking interpreter that executes the AST directly.
/// Immutable environments with scope chaining.

use crate::ast::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// Runtime values
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(String),
    Null,
    List(Vec<Value>),
    Map(Vec<(Value, Value)>),
    Tuple(Vec<Value>),
    Ok(Box<Value>),
    Err(String),
    Void,
    /// A reference to a named function
    FnRef(String),
    /// A lambda/closure
    Lambda {
        params: Vec<Param>,
        body: Expr,
        env: Env,
    },
}

impl Value {
    pub fn type_name(&self) -> &str {
        match self {
            Value::Int(_) => "i32",
            Value::Float(_) => "f64",
            Value::Bool(_) => "bool",
            Value::Text(_) => "text",
            Value::Null => "null",
            Value::List(_) => "list",
            Value::Map(_) => "map",
            Value::Tuple(_) => "tuple",
            Value::Ok(_) => "ok",
            Value::Err(_) => "err",
            Value::Void => "void",
            Value::FnRef(_) => "fn",
            Value::Lambda { .. } => "lambda",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Null => false,
            Value::Text(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            _ => true,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Text(s) => write!(f, "{}", s),
            Value::Null => write!(f, "null"),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Map(pairs) => {
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{} {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Ok(v) => write!(f, "ok({})", v),
            Value::Err(msg) => write!(f, "err({})", msg),
            Value::Void => write!(f, "void"),
            Value::FnRef(name) => write!(f, "<fn {}>", name),
            Value::Lambda { .. } => write!(f, "<lambda>"),
        }
    }
}

/// Environment: immutable bindings with scope chain
#[derive(Debug, Clone)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn child(&self) -> Self {
        Env {
            bindings: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(name)))
    }
}

/// Control flow signal
enum Signal {
    Return(Value),
    Emit(Value),
    Continue,
    /// Tail call optimization: instead of recursing, return the call info
    TailCall { name: String, args: Vec<Value> },
}

pub struct Interpreter {
    functions: HashMap<String, FnDecl>,
    type_defs: HashMap<String, TypeDecl>,
    constants: HashMap<String, Value>,
    error_handlers: HashMap<String, ErrHandler>,
    /// Tracks canonical paths of modules currently being loaded (for circular import detection)
    loading_modules: HashSet<PathBuf>,
    /// The base directory used to resolve std library imports
    std_dir: Option<PathBuf>,
}

impl Interpreter {
    pub fn new() -> Self {
        // Determine the std library directory: look relative to the executable
        let std_dir = std::env::current_exe()
            .ok()
            .and_then(|exe| exe.parent().map(|p| p.join("std")))
            .filter(|p| p.is_dir())
            .or_else(|| {
                // Fallback: look in the current working directory
                std::env::current_dir().ok().map(|p| p.join("std"))
            });

        Interpreter {
            functions: HashMap::new(),
            type_defs: HashMap::new(),
            constants: HashMap::new(),
            error_handlers: HashMap::new(),
            loading_modules: HashSet::new(),
            std_dir,
        }
    }

    /// Register declarations from a program fragment into the interpreter.
    /// Used by the REPL to accumulate function/type/const definitions across inputs.
    pub fn register_program(&mut self, program: Program, env: &mut Env) -> Result<(), RuntimeError> {
        for c in &program.consts {
            let val = self.eval_expr(&c.value, &Env::new())?;
            self.constants.insert(c.name.clone(), val.clone());
            env.set(c.name.clone(), val);
        }

        for t in program.types {
            self.type_defs.insert(t.name.clone(), t);
        }

        for f in program.functions {
            self.functions.insert(f.name.clone(), f);
        }

        for eh in program.error_handlers {
            self.error_handlers.insert(eh.fn_name.clone(), eh);
        }

        // Run tests if any
        for test in &program.tests {
            self.run_test(test)?;
        }

        // Run entry block if present
        if let Some(entry) = &program.entry {
            // Add constants to env
            for (name, val) in &self.constants {
                if env.get(name).is_none() {
                    env.set(name.clone(), val.clone());
                }
            }
            self.exec_body(&entry.body, env)?;
        }

        Ok(())
    }

    /// Evaluate a single statement in the given environment.
    /// Used by the REPL for evaluating individual lines.
    pub fn eval_stmt(&self, stmt: &Stmt, env: &mut Env) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Bind { name, value, .. } => {
                let val = self.eval_expr(value, env)?;
                env.set(name.clone(), val);
                Ok(Value::Void)
            }
            Stmt::Return { value } => self.eval_expr(value, env),
            Stmt::Emit { value } => {
                let val = self.eval_expr(value, env)?;
                println!("{}", val);
                Ok(val)
            }
            Stmt::Effect { expr } => {
                let val = self.eval_expr(expr, env)?;
                Ok(val)
            }
        }
    }

    pub fn run(&mut self, program: Program, test_only: bool) -> Result<Value, RuntimeError> {
        self.run_with_path(program, test_only, None)
    }

    pub fn run_with_path(
        &mut self,
        program: Program,
        test_only: bool,
        source_path: Option<&Path>,
    ) -> Result<Value, RuntimeError> {
        // Process #use declarations first
        if !program.uses.is_empty() {
            let base_dir = source_path
                .and_then(|p| p.parent())
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

            for use_decl in &program.uses {
                self.load_module(&use_decl.path, &use_decl.names, &base_dir, use_decl.line)?;
            }
        }

        // Register all declarations
        for c in &program.consts {
            let val = self.eval_expr(&c.value, &Env::new())?;
            self.constants.insert(c.name.clone(), val);
        }

        for t in program.types {
            self.type_defs.insert(t.name.clone(), t);
        }

        for f in program.functions {
            self.functions.insert(f.name.clone(), f);
        }

        // Register error handlers
        for eh in program.error_handlers {
            self.error_handlers.insert(eh.fn_name.clone(), eh);
        }

        // Run tests if any
        for test in &program.tests {
            self.run_test(test)?;
        }

        if test_only {
            return Ok(Value::Void);
        }

        // Run entry point
        if let Some(entry) = &program.entry {
            let mut env = Env::new();
            // Add constants to env
            for (name, val) in &self.constants {
                env.set(name.clone(), val.clone());
            }
            self.exec_body(&entry.body, &mut env)
        } else {
            Ok(Value::Void)
        }
    }

    /// Resolve a module path to a file system path.
    /// - "math" resolves to `<base_dir>/math.ai`
    /// - "std/math" resolves to `<std_dir>/math.ai` (standard library) or `<base_dir>/std/math.ai`
    fn resolve_module_path(&self, module_path: &str, base_dir: &Path) -> Result<PathBuf, RuntimeError> {
        // Check if this is a std library path
        if module_path.starts_with("std/") {
            let relative = &module_path["std/".len()..];
            let file_name = format!("{}.ai", relative);

            // First try the std directory (next to executable or cwd)
            if let Some(ref std_dir) = self.std_dir {
                let candidate = std_dir.join(&file_name);
                if candidate.is_file() {
                    return Ok(candidate);
                }
            }

            // Fallback: resolve relative to base_dir
            let candidate = base_dir.join("std").join(&file_name);
            if candidate.is_file() {
                return Ok(candidate);
            }

            return Err(RuntimeError {
                message: format!(
                    "module '{}' not found (looked for '{}')",
                    module_path, file_name
                ),
            });
        }

        // Regular module: resolve relative to the importing file
        let file_name = format!("{}.ai", module_path);
        let candidate = base_dir.join(&file_name);
        if candidate.is_file() {
            return Ok(candidate);
        }

        Err(RuntimeError {
            message: format!(
                "module '{}' not found (looked for '{}')",
                module_path,
                candidate.display()
            ),
        })
    }

    /// Load a module: read, parse, and import its public declarations.
    fn load_module(
        &mut self,
        module_path: &str,
        names: &Option<Vec<String>>,
        base_dir: &Path,
        line: usize,
    ) -> Result<(), RuntimeError> {
        let file_path = self.resolve_module_path(module_path, base_dir)?;

        // Canonicalize for circular import detection
        let canonical = file_path.canonicalize().unwrap_or_else(|_| file_path.clone());

        // Circular import check
        if self.loading_modules.contains(&canonical) {
            return Err(RuntimeError {
                message: format!(
                    "circular import detected: '{}' (line {})",
                    module_path, line
                ),
            });
        }

        // Mark as loading
        self.loading_modules.insert(canonical.clone());

        // Read the module source
        let source = std::fs::read_to_string(&file_path).map_err(|e| RuntimeError {
            message: format!("cannot read module '{}': {}", file_path.display(), e),
        })?;

        // Lex
        let mut lexer = crate::lexer::Lexer::new(&source);
        let tokens = lexer.tokenize().map_err(|e| RuntimeError {
            message: format!("in module '{}': {}", module_path, e),
        })?;

        // Parse
        let mut parser = crate::parser::Parser::new(tokens);
        let module_program = parser.parse().map_err(|e| RuntimeError {
            message: format!("in module '{}': {}", module_path, e),
        })?;

        // Recursively process any #use declarations in the module
        let module_base_dir = file_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| base_dir.to_path_buf());

        for use_decl in &module_program.uses {
            self.load_module(
                &use_decl.path,
                &use_decl.names,
                &module_base_dir,
                use_decl.line,
            )?;
        }

        // Import constants from the module
        for c in &module_program.consts {
            // Skip private names (prefixed with _)
            if c.name.starts_with('_') {
                if let Some(selected) = names {
                    if selected.contains(&c.name) {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot import private constant '{}' from module '{}'",
                                c.name, module_path
                            ),
                        });
                    }
                }
                continue;
            }

            // Selective import check
            if let Some(selected) = names {
                if !selected.contains(&c.name) {
                    continue;
                }
            }

            let val = self.eval_expr(&c.value, &Env::new())?;
            self.constants.insert(c.name.clone(), val);
        }

        // Import type definitions from the module
        for t in module_program.types {
            // Skip private types
            if t.name.starts_with('_') {
                if let Some(selected) = names {
                    if selected.contains(&t.name) {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot import private type '{}' from module '{}'",
                                t.name, module_path
                            ),
                        });
                    }
                }
                continue;
            }

            if let Some(selected) = names {
                if !selected.contains(&t.name) {
                    continue;
                }
            }

            self.type_defs.insert(t.name.clone(), t);
        }

        // Import functions from the module
        for f in module_program.functions {
            // Skip private functions (prefixed with _)
            if f.name.starts_with('_') {
                if let Some(selected) = names {
                    if selected.contains(&f.name) {
                        return Err(RuntimeError {
                            message: format!(
                                "cannot import private function '{}' from module '{}'",
                                f.name, module_path
                            ),
                        });
                    }
                }
                continue;
            }

            // Selective import check
            if let Some(selected) = names {
                if !selected.contains(&f.name) {
                    continue;
                }
            }

            self.functions.insert(f.name.clone(), f);
        }

        // Unmark loading (module is fully loaded)
        self.loading_modules.remove(&canonical);

        Ok(())
    }

    fn run_test(&self, test: &TestDecl) -> Result<(), RuntimeError> {
        let mut env = Env::new();
        for (name, val) in &self.constants {
            env.set(name.clone(), val.clone());
        }
        match self.exec_body(&test.body, &mut env) {
            Ok(_) => {
                eprintln!("  PASS: {}", test.name);
                Ok(())
            }
            Err(e) => {
                eprintln!("  FAIL: {} — {}", test.name, e);
                Err(e)
            }
        }
    }

    fn exec_body(&self, body: &[Stmt], env: &mut Env) -> Result<Value, RuntimeError> {
        let mut result = Value::Void;

        for stmt in body {
            match self.exec_stmt(stmt, env)? {
                Signal::Return(val) => return Ok(val),
                Signal::Emit(val) => {
                    println!("{}", val);
                    result = val;
                }
                Signal::Continue => {}
                Signal::TailCall { name, args } => {
                    return self.call_function(&name, &args, env);
                }
            }
        }

        Ok(result)
    }

    fn exec_stmt(&self, stmt: &Stmt, env: &mut Env) -> Result<Signal, RuntimeError> {
        match stmt {
            Stmt::Bind { name, value, .. } => {
                let val = self.eval_expr(value, env)?;
                env.set(name.clone(), val);
                Ok(Signal::Continue)
            }
            Stmt::Return { value } => {
                let val = self.eval_expr(value, env)?;
                Ok(Signal::Return(val))
            }
            Stmt::Emit { value } => {
                let val = self.eval_expr(value, env)?;
                Ok(Signal::Emit(val))
            }
            Stmt::Effect { expr } => {
                self.eval_expr(expr, env)?;
                Ok(Signal::Continue)
            }
        }
    }

    pub fn eval_expr(&self, expr: &Expr, env: &Env) -> Result<Value, RuntimeError> {
        match expr {
            Expr::IntLit(n) => Ok(Value::Int(*n)),
            Expr::FloatLit(f) => Ok(Value::Float(*f)),
            Expr::BoolLit(b) => Ok(Value::Bool(*b)),
            Expr::TextLit(s) => Ok(Value::Text(s.clone())),
            Expr::NullLit => Ok(Value::Null),

            Expr::Var(name) => env
                .get(name)
                .cloned()
                .or_else(|| {
                    // Check if it's a function reference
                    if self.functions.contains_key(name) {
                        Some(Value::FnRef(name.clone()))
                    } else {
                        self.constants.get(name).cloned()
                    }
                })
                .ok_or_else(|| RuntimeError {
                    message: format!("undefined variable '{}'", name),
                }),

            Expr::FieldAccess { object, field } => {
                let obj = self.eval_expr(object, env)?;
                match obj {
                    Value::Map(pairs) => {
                        for (k, v) in &pairs {
                            if let Value::Text(key) = k {
                                if key == field {
                                    return Ok(v.clone());
                                }
                            }
                        }
                        Err(RuntimeError {
                            message: format!("field '{}' not found", field),
                        })
                    }
                    Value::Tuple(items) => {
                        // Allow numeric field access on tuples
                        if let Some(idx) = field.parse::<usize>().ok() {
                            items.get(idx).cloned().ok_or_else(|| RuntimeError {
                                message: format!("tuple index {} out of range", idx),
                            })
                        } else {
                            Err(RuntimeError {
                                message: format!("cannot access field '{}' on tuple", field),
                            })
                        }
                    }
                    _ => Err(RuntimeError {
                        message: format!("cannot access field '{}' on {}", field, obj.type_name()),
                    }),
                }
            }

            Expr::ListLit(elements) => {
                let vals: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.eval_expr(e, env)).collect();
                Ok(Value::List(vals?))
            }

            Expr::MapLit(pairs) => {
                let mut vals = Vec::new();
                for (k, v) in pairs {
                    let key = self.eval_expr(k, env)?;
                    let val = self.eval_expr(v, env)?;
                    vals.push((key, val));
                }
                Ok(Value::Map(vals))
            }

            Expr::TupleLit(elements) => {
                let vals: Result<Vec<_>, _> =
                    elements.iter().map(|e| self.eval_expr(e, env)).collect();
                Ok(Value::Tuple(vals?))
            }

            Expr::BinOp { op, left, right } => {
                let l = self.eval_expr(left, env)?;
                let r = self.eval_expr(right, env)?;
                self.eval_binop(op, &l, &r)
            }

            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand, env)?;
                self.eval_unaryop(op, &val)
            }

            Expr::Call { name, args } => {
                let arg_vals: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_expr(a, env)).collect();
                let arg_vals = arg_vals?;
                self.call_function(name, &arg_vals, env)
            }

            Expr::Select {
                cond,
                then_val,
                else_val,
            } => {
                let c = self.eval_expr(cond, env)?;
                if c.is_truthy() {
                    self.eval_expr(then_val, env)
                } else {
                    self.eval_expr(else_val, env)
                }
            }

            Expr::Cond { branches, default } => {
                for (condition, value) in branches {
                    let c = self.eval_expr(condition, env)?;
                    if c.is_truthy() {
                        return self.eval_expr(value, env);
                    }
                }
                self.eval_expr(default, env)
            }

            Expr::Match { value, arms } => {
                let val = self.eval_expr(value, env)?;
                for arm in arms {
                    if let Some(mut match_env) = self.match_pattern(&arm.pattern, &val, env) {
                        return self.eval_expr(&arm.body, &match_env);
                    }
                }
                Err(RuntimeError {
                    message: "no matching pattern".to_string(),
                })
            }

            Expr::MapIter { func, list } => {
                let fn_val = self.eval_expr(func, env)?;
                let list_val = self.eval_expr(list, env)?;
                if let Value::List(items) = list_val {
                    let mut results = Vec::new();
                    for item in &items {
                        let result = self.apply_fn(&fn_val, &[item.clone()], env)?;
                        results.push(result);
                    }
                    Ok(Value::List(results))
                } else {
                    Err(RuntimeError {
                        message: "map requires a list".to_string(),
                    })
                }
            }

            Expr::FilterIter { func, list } => {
                let fn_val = self.eval_expr(func, env)?;
                let list_val = self.eval_expr(list, env)?;
                if let Value::List(items) = list_val {
                    let mut results = Vec::new();
                    for item in &items {
                        let result = self.apply_fn(&fn_val, &[item.clone()], env)?;
                        if result.is_truthy() {
                            results.push(item.clone());
                        }
                    }
                    Ok(Value::List(results))
                } else {
                    Err(RuntimeError {
                        message: "filter requires a list".to_string(),
                    })
                }
            }

            Expr::FoldIter { list, init, func } => {
                let list_val = self.eval_expr(list, env)?;
                let mut acc = self.eval_expr(init, env)?;
                let fn_val = self.eval_expr(func, env)?;
                if let Value::List(items) = list_val {
                    for item in &items {
                        acc = self.apply_fn(&fn_val, &[acc, item.clone()], env)?;
                    }
                    Ok(acc)
                } else {
                    Err(RuntimeError {
                        message: "fold requires a list".to_string(),
                    })
                }
            }

            Expr::EachIter { list, func } => {
                let list_val = self.eval_expr(list, env)?;
                let fn_val = self.eval_expr(func, env)?;
                if let Value::List(items) = list_val {
                    for item in &items {
                        self.apply_fn(&fn_val, &[item.clone()], env)?;
                    }
                    Ok(Value::Void)
                } else {
                    Err(RuntimeError {
                        message: "each requires a list".to_string(),
                    })
                }
            }

            Expr::ZipIter { list_a, list_b } => {
                let a_val = self.eval_expr(list_a, env)?;
                let b_val = self.eval_expr(list_b, env)?;
                match (a_val, b_val) {
                    (Value::List(a_items), Value::List(b_items)) => {
                        let len = a_items.len().min(b_items.len());
                        let mut results = Vec::with_capacity(len);
                        for i in 0..len {
                            results.push(Value::List(vec![
                                a_items[i].clone(),
                                b_items[i].clone(),
                            ]));
                        }
                        Ok(Value::List(results))
                    }
                    _ => Err(RuntimeError {
                        message: "zip requires two lists".to_string(),
                    }),
                }
            }

            Expr::FlatMapIter { func, list } => {
                let fn_val = self.eval_expr(func, env)?;
                let list_val = self.eval_expr(list, env)?;
                if let Value::List(items) = list_val {
                    let mut results = Vec::new();
                    for item in &items {
                        let result = self.apply_fn(&fn_val, &[item.clone()], env)?;
                        match result {
                            Value::List(sub_items) => results.extend(sub_items),
                            _ => {
                                return Err(RuntimeError {
                                    message: "flatmap function must return a list".to_string(),
                                });
                            }
                        }
                    }
                    Ok(Value::List(results))
                } else {
                    Err(RuntimeError {
                        message: "flatmap requires a list".to_string(),
                    })
                }
            }

            Expr::Cast { target, value } => {
                let val = self.eval_expr(value, env)?;
                self.cast_value(&val, target)
            }

            Expr::TryExpr { value } => match self.eval_expr(value, env) {
                Ok(v) => Ok(Value::Ok(Box::new(v))),
                Err(e) => Ok(Value::Err(e.message)),
            },

            Expr::Unwrap { value, default } => {
                let val = self.eval_expr(value, env)?;
                match val {
                    Value::Ok(v) => Ok(*v),
                    Value::Null => self.eval_expr(default, env),
                    Value::Err(_) => {
                        let def = self.eval_expr(default, env)?;
                        if let Value::FnRef(ref name) = def {
                            if name == "panic" {
                                return Err(RuntimeError {
                                    message: "unwrap of error value".to_string(),
                                });
                            }
                        }
                        Ok(def)
                    }
                    other => Ok(other),
                }
            }

            Expr::OkWrap(value) => {
                let val = self.eval_expr(value, env)?;
                Ok(Value::Ok(Box::new(val)))
            }

            Expr::ToolCall { name, params } => {
                let name_val = self.eval_expr(name, env)?;
                let params_val = self.eval_expr(params, env)?;
                eprintln!("[tool] {} with {}", name_val, params_val);
                Ok(Value::Text(format!(
                    "<tool:{} result>",
                    name_val
                )))
            }

            Expr::Log {
                level,
                message,
                args,
            } => {
                let level_val = self.eval_expr(level, env)?;
                let msg_val = self.eval_expr(message, env)?;
                let mut msg = format!("{}", msg_val);
                for (i, arg) in args.iter().enumerate() {
                    let val = self.eval_expr(arg, env)?;
                    msg = msg.replace(&format!("{{{}}}", i), &format!("{}", val));
                }
                eprintln!("[{}] {}", level_val, msg);
                Ok(Value::Void)
            }

            Expr::Assert { value } => {
                let val = self.eval_expr(value, env)?;
                if val.is_truthy() {
                    Ok(Value::Void)
                } else {
                    Err(RuntimeError {
                        message: format!("assertion failed: {:?}", value),
                    })
                }
            }

            Expr::Construct { type_name, args } => {
                let arg_vals: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_expr(a, env)).collect();
                let arg_vals = arg_vals?;
                // Store as a map with field names from the type def
                if let Some(type_def) = self.type_defs.get(type_name) {
                    let mut pairs = Vec::new();
                    for (i, field) in type_def.fields.iter().enumerate() {
                        if let Some(val) = arg_vals.get(i) {
                            pairs.push((Value::Text(field.name.clone()), val.clone()));
                        }
                    }
                    Ok(Value::Map(pairs))
                } else {
                    // Enum variant — store as tuple with variant name
                    let mut items = vec![Value::Text(type_name.clone())];
                    items.extend(arg_vals);
                    Ok(Value::Tuple(items))
                }
            }

            Expr::Lambda { params, body, .. } => Ok(Value::Lambda {
                params: params.clone(),
                body: body.as_ref().clone(),
                env: env.clone(),
            }),

            Expr::Propagate(inner) => {
                let val = self.eval_expr(inner, env)?;
                match val {
                    Value::Err(msg) => Err(RuntimeError { message: msg }),
                    Value::Ok(v) => Ok(*v),
                    other => Ok(other),
                }
            }
        }
    }

    fn eval_binop(&self, op: &BinOpKind, left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            // Integer arithmetic
            (BinOpKind::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (BinOpKind::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (BinOpKind::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (BinOpKind::Div, Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(RuntimeError {
                        message: "division by zero".to_string(),
                    })
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (BinOpKind::Mod, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),

            // Float arithmetic
            (BinOpKind::Add, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (BinOpKind::Sub, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (BinOpKind::Mul, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (BinOpKind::Div, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),

            // Text concatenation via add
            (BinOpKind::Add, Value::Text(a), Value::Text(b)) => {
                Ok(Value::Text(format!("{}{}", a, b)))
            }

            // Comparison — integers
            (BinOpKind::Eq, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (BinOpKind::Neq, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a != b)),
            (BinOpKind::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (BinOpKind::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (BinOpKind::Lte, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (BinOpKind::Gte, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),

            // Comparison — floats
            (BinOpKind::Eq, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (BinOpKind::Lt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (BinOpKind::Gt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),

            // Comparison — text
            (BinOpKind::Eq, Value::Text(a), Value::Text(b)) => Ok(Value::Bool(a == b)),
            (BinOpKind::Neq, Value::Text(a), Value::Text(b)) => Ok(Value::Bool(a != b)),

            // Comparison — bool
            (BinOpKind::Eq, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),

            // Comparison — null
            (BinOpKind::Eq, Value::Null, Value::Null) => Ok(Value::Bool(true)),
            (BinOpKind::Eq, Value::Null, _) => Ok(Value::Bool(false)),
            (BinOpKind::Eq, _, Value::Null) => Ok(Value::Bool(false)),
            (BinOpKind::Neq, Value::Null, Value::Null) => Ok(Value::Bool(false)),
            (BinOpKind::Neq, Value::Null, _) => Ok(Value::Bool(true)),
            (BinOpKind::Neq, _, Value::Null) => Ok(Value::Bool(true)),

            // Logic
            (BinOpKind::And, a, b) => Ok(Value::Bool(a.is_truthy() && b.is_truthy())),
            (BinOpKind::Or, a, b) => Ok(Value::Bool(a.is_truthy() || b.is_truthy())),

            // Bitwise
            (BinOpKind::Band, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
            (BinOpKind::Bor, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
            (BinOpKind::Bxor, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
            (BinOpKind::Shl, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
            (BinOpKind::Shr, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),

            _ => Err(RuntimeError {
                message: format!(
                    "cannot apply {:?} to {} and {}",
                    op,
                    left.type_name(),
                    right.type_name()
                ),
            }),
        }
    }

    fn eval_unaryop(&self, op: &UnaryOpKind, val: &Value) -> Result<Value, RuntimeError> {
        match (op, val) {
            (UnaryOpKind::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOpKind::Neg, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnaryOpKind::Not, v) => Ok(Value::Bool(!v.is_truthy())),
            (UnaryOpKind::Bnot, Value::Int(n)) => Ok(Value::Int(!n)),
            _ => Err(RuntimeError {
                message: format!("cannot apply {:?} to {}", op, val.type_name()),
            }),
        }
    }

    fn call_function(
        &self,
        name: &str,
        args: &[Value],
        _env: &Env,
    ) -> Result<Value, RuntimeError> {
        // Check built-in functions first
        if let Some(result) = self.try_builtin(name, args)? {
            return Ok(result);
        }

        // Check for error handler
        let handler = self.error_handlers.get(name);

        // If there is no error handler, call directly without interception
        if handler.is_none() {
            return self.call_function_inner(name, args);
        }

        // There IS an error handler — use retry/fallback logic
        let handler = handler.unwrap();
        let max_attempts = handler.retry_count
            .map(|n| (n as usize) + 1)
            .unwrap_or(1);

        let mut last_error_msg = String::new();

        for _attempt in 0..max_attempts {
            match self.call_function_inner(name, args) {
                Ok(Value::Err(msg)) => {
                    // Function returned an error value — eligible for handler
                    last_error_msg = msg;
                    continue; // retry if attempts remain
                }
                Err(e) => {
                    // Function raised a runtime error (e.g., from ? propagation)
                    // Strip the "in 'name': " prefix if present for handler use
                    let prefix = format!("in '{}': ", name);
                    last_error_msg = if e.message.starts_with(&prefix) {
                        e.message[prefix.len()..].to_string()
                    } else {
                        e.message
                    };
                    continue; // retry if attempts remain
                }
                Ok(val) => return Ok(val), // success
            }
        }

        // All attempts exhausted — apply fallback if present
        if let Some(fallback_expr) = &handler.fallback {
            let mut handler_env = Env::new();
            for (cname, cval) in &self.constants {
                handler_env.set(cname.clone(), cval.clone());
            }
            handler_env.set("err".to_string(), Value::Text(last_error_msg));
            return self.eval_expr(fallback_expr, &handler_env);
        }

        // Handler exists but has no fallback — propagate the error
        Err(RuntimeError {
            message: format!("in '{}': {}", name, last_error_msg),
        })
    }

    /// Inner function call logic with TCO trampoline, without error handler wrapping.
    fn call_function_inner(
        &self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Trampoline loop for tail-call optimization
        let mut current_name = name.to_string();
        let mut current_args = args.to_vec();
        let original_name = name.to_string();

        loop {
            // Look up user-defined function
            let func = self.functions.get(&current_name).ok_or_else(|| RuntimeError {
                message: format!("undefined function '{}'", current_name),
            })?;

            // Clone what we need to avoid borrow issues
            let params = func.params.clone();
            let body = func.body.clone();

            if params.len() != current_args.len() {
                return Err(RuntimeError {
                    message: format!(
                        "function '{}' expected {} args, got {}",
                        current_name,
                        params.len(),
                        current_args.len()
                    ),
                });
            }

            let mut fn_env = Env::new();

            // Bind constants
            for (cname, cval) in &self.constants {
                fn_env.set(cname.clone(), cval.clone());
            }

            // Bind parameters
            for (i, param) in params.iter().enumerate() {
                fn_env.set(param.name.clone(), current_args[i].clone());
            }

            match self.exec_body_tco(&body, &mut fn_env) {
                Ok(Signal::TailCall { name: tc_name, args: tc_args }) => {
                    // Check if the tail call target is a builtin
                    if let Some(result) = self.try_builtin(&tc_name, &tc_args)? {
                        return Ok(result);
                    }
                    // Loop with new function name and args (trampoline)
                    current_name = tc_name;
                    current_args = tc_args;
                    continue;
                }
                Ok(Signal::Return(val)) => return Ok(val),
                Ok(Signal::Emit(val)) => return Ok(val),
                Ok(Signal::Continue) => return Ok(Value::Void),
                Err(e) => {
                    return Err(RuntimeError {
                        message: format!("in '{}': {}", original_name, e.message),
                    });
                }
            }
        }
    }

    /// Execute a function body with tail-call optimization awareness.
    /// The last Return statement in the body is evaluated in tail position,
    /// which means a Call in that position returns Signal::TailCall instead
    /// of recursing.
    fn exec_body_tco(&self, body: &[Stmt], env: &mut Env) -> Result<Signal, RuntimeError> {
        if body.is_empty() {
            return Ok(Signal::Return(Value::Void));
        }

        // Find the index of the last statement
        let last_idx = body.len() - 1;

        // Execute all statements except the last one normally
        for stmt in &body[..last_idx] {
            match self.exec_stmt(stmt, env)? {
                Signal::Return(val) => return Ok(Signal::Return(val)),
                Signal::Emit(val) => {
                    println!("{}", val);
                }
                Signal::Continue => {}
                Signal::TailCall { .. } => unreachable!(),
            }
        }

        // Handle the last statement with tail-call awareness
        let last_stmt = &body[last_idx];
        match last_stmt {
            Stmt::Return { value } => {
                // Evaluate in tail position
                self.eval_expr_tail(value, env)
            }
            // Non-return statements are not in tail position
            _ => {
                match self.exec_stmt(last_stmt, env)? {
                    Signal::Return(val) => Ok(Signal::Return(val)),
                    Signal::Emit(val) => {
                        println!("{}", val);
                        Ok(Signal::Return(val))
                    }
                    Signal::Continue => Ok(Signal::Return(Value::Void)),
                    Signal::TailCall { .. } => unreachable!(),
                }
            }
        }
    }

    /// Evaluate an expression in tail position. If it is a Call to a
    /// user-defined function, return Signal::TailCall instead of recursing.
    /// For Select/Cond/Match, propagate tail position into branches.
    fn eval_expr_tail(&self, expr: &Expr, env: &Env) -> Result<Signal, RuntimeError> {
        match expr {
            Expr::Call { name, args } => {
                let arg_vals: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_expr(a, env)).collect();
                let arg_vals = arg_vals?;

                // Check if it is a builtin -- builtins do not need TCO
                if let Some(val) = self.try_builtin(name, &arg_vals)? {
                    return Ok(Signal::Return(val));
                }

                // It is a user-defined function call in tail position
                if self.functions.contains_key(name.as_str()) {
                    Ok(Signal::TailCall {
                        name: name.clone(),
                        args: arg_vals,
                    })
                } else {
                    // Unknown function - let call_function produce the error
                    let val = self.call_function(name, &arg_vals, env)?;
                    Ok(Signal::Return(val))
                }
            }

            Expr::Select { cond, then_val, else_val } => {
                let c = self.eval_expr(cond, env)?;
                if c.is_truthy() {
                    self.eval_expr_tail(then_val, env)
                } else {
                    self.eval_expr_tail(else_val, env)
                }
            }

            Expr::Cond { branches, default } => {
                for (condition, value) in branches {
                    let c = self.eval_expr(condition, env)?;
                    if c.is_truthy() {
                        return self.eval_expr_tail(value, env);
                    }
                }
                self.eval_expr_tail(default, env)
            }

            Expr::Match { value, arms } => {
                let val = self.eval_expr(value, env)?;
                for arm in arms {
                    if let Some(match_env) = self.match_pattern(&arm.pattern, &val, env) {
                        return self.eval_expr_tail(&arm.body, &match_env);
                    }
                }
                Err(RuntimeError {
                    message: "no matching pattern".to_string(),
                })
            }

            // For any other expression, evaluate normally
            _ => {
                let val = self.eval_expr(expr, env)?;
                Ok(Signal::Return(val))
            }
        }
    }

    fn apply_fn(&self, func: &Value, args: &[Value], env: &Env) -> Result<Value, RuntimeError> {
        match func {
            Value::FnRef(name) => self.call_function(name, args, env),
            Value::Lambda {
                params,
                body,
                env: closure_env,
            } => {
                let mut fn_env = closure_env.child();
                for (i, param) in params.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        fn_env.set(param.name.clone(), val.clone());
                    }
                }
                self.eval_expr(body, &fn_env)
            }
            _ => Err(RuntimeError {
                message: format!("cannot call {}", func.type_name()),
            }),
        }
    }

    fn try_builtin(&self, name: &str, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        let result = match name {
            "concat" => {
                let a = self.expect_text(&args[0])?;
                let b = self.expect_text(&args[1])?;
                Some(Value::Text(format!("{}{}", a, b)))
            }
            "len" => match &args[0] {
                Value::Text(s) => Some(Value::Int(s.len() as i64)),
                Value::List(l) => Some(Value::Int(l.len() as i64)),
                Value::Map(m) => Some(Value::Int(m.len() as i64)),
                _ => {
                    return Err(RuntimeError {
                        message: "len requires text, list, or map".to_string(),
                    })
                }
            },
            "split" => {
                let s = self.expect_text(&args[0])?;
                let delim = self.expect_text(&args[1])?;
                let parts: Vec<Value> = s.split(&delim).map(|p| Value::Text(p.to_string())).collect();
                Some(Value::List(parts))
            }
            "join" => {
                if let Value::List(items) = &args[0] {
                    let delim = self.expect_text(&args[1])?;
                    let parts: Vec<String> = items.iter().map(|v| format!("{}", v)).collect();
                    Some(Value::Text(parts.join(&delim)))
                } else {
                    return Err(RuntimeError {
                        message: "join requires a list".to_string(),
                    });
                }
            }
            "upper" => {
                let s = self.expect_text(&args[0])?;
                Some(Value::Text(s.to_uppercase()))
            }
            "lower" => {
                let s = self.expect_text(&args[0])?;
                Some(Value::Text(s.to_lowercase()))
            }
            "trim" => {
                let s = self.expect_text(&args[0])?;
                Some(Value::Text(s.trim().to_string()))
            }
            "sqrt" => match &args[0] {
                Value::Float(f) => Some(Value::Float(f.sqrt())),
                Value::Int(n) => Some(Value::Float((*n as f64).sqrt())),
                _ => {
                    return Err(RuntimeError {
                        message: "sqrt requires a number".to_string(),
                    })
                }
            },
            "get" => {
                if let (Value::List(items), Value::Int(idx)) = (&args[0], &args[1]) {
                    items
                        .get(*idx as usize)
                        .cloned()
                        .map(Some)
                        .unwrap_or(Some(Value::Null))
                } else {
                    return Err(RuntimeError {
                        message: "get requires a list and index".to_string(),
                    });
                }
            }
            "safe_get" => {
                match (&args[0], &args[1]) {
                    (Value::List(items), Value::Int(idx)) => {
                        if *idx < 0 {
                            Some(Value::Null)
                        } else {
                            Some(items.get(*idx as usize).cloned().unwrap_or(Value::Null))
                        }
                    }
                    (Value::Text(s), Value::Int(idx)) => {
                        if *idx < 0 {
                            Some(Value::Null)
                        } else {
                            match s.chars().nth(*idx as usize) {
                                Some(c) => Some(Value::Text(c.to_string())),
                                None => Some(Value::Null),
                            }
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            message: "safe_get requires a list or text and an index".to_string(),
                        });
                    }
                }
            }
            "push" => {
                if let Value::List(items) = &args[0] {
                    let mut new_items = items.clone();
                    new_items.push(args[1].clone());
                    Some(Value::List(new_items))
                } else {
                    return Err(RuntimeError {
                        message: "push requires a list".to_string(),
                    });
                }
            }
            "head" => {
                if let Value::List(items) = &args[0] {
                    Some(items.first().cloned().unwrap_or(Value::Null))
                } else {
                    return Err(RuntimeError {
                        message: "head requires a list".to_string(),
                    });
                }
            }
            "tail" => {
                if let Value::List(items) = &args[0] {
                    if items.len() > 1 {
                        Some(Value::List(items[1..].to_vec()))
                    } else {
                        Some(Value::List(Vec::new()))
                    }
                } else {
                    return Err(RuntimeError {
                        message: "tail requires a list".to_string(),
                    });
                }
            }
            "range" => {
                if let (Value::Int(start), Value::Int(end)) = (&args[0], &args[1]) {
                    let items: Vec<Value> = (*start..*end).map(Value::Int).collect();
                    Some(Value::List(items))
                } else {
                    return Err(RuntimeError {
                        message: "range requires two integers".to_string(),
                    });
                }
            }
            "reverse" => {
                if let Value::List(items) = &args[0] {
                    let mut rev = items.clone();
                    rev.reverse();
                    Some(Value::List(rev))
                } else {
                    return Err(RuntimeError {
                        message: "reverse requires a list".to_string(),
                    });
                }
            }
            "fmt" => {
                let template = self.expect_text(&args[0])?;
                let mut result = template.to_string();
                for (i, arg) in args[1..].iter().enumerate() {
                    result = result.replace(&format!("{{{}}}", i), &format!("{}", arg));
                }
                Some(Value::Text(result))
            }
            "print" => {
                for arg in args {
                    print!("{}", arg);
                }
                println!();
                use std::io::Write;
                std::io::stdout().flush().ok();
                Some(Value::Void)
            }
            "print_no_nl" => {
                for arg in args {
                    print!("{}", arg);
                }
                use std::io::Write;
                std::io::stdout().flush().ok();
                Some(Value::Void)
            }
            "read_line" => {
                use std::io::{self, Write};
                io::stdout().flush().ok();
                let mut input = String::new();
                let bytes_read = io::stdin().read_line(&mut input).map_err(|e| RuntimeError {
                    message: format!("read_line failed: {}", e),
                })?;
                if bytes_read == 0 {
                    return Err(RuntimeError {
                        message: "read_line: end of input".to_string(),
                    });
                }
                Some(Value::Text(input.trim_end_matches('\n').trim_end_matches('\r').to_string()))
            }
            "abs" => match &args[0] {
                Value::Int(n) => Some(Value::Int(n.abs())),
                Value::Float(f) => Some(Value::Float(f.abs())),
                _ => return Err(RuntimeError { message: "abs requires a number".to_string() }),
            },
            "min" => match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => Some(Value::Int(*a.min(b))),
                (Value::Float(a), Value::Float(b)) => Some(Value::Float(a.min(*b))),
                _ => return Err(RuntimeError { message: "min requires two numbers".to_string() }),
            },
            "max" => match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => Some(Value::Int(*a.max(b))),
                (Value::Float(a), Value::Float(b)) => Some(Value::Float(a.max(*b))),
                _ => return Err(RuntimeError { message: "max requires two numbers".to_string() }),
            },
            "sort" => {
                if let Value::List(items) = &args[0] {
                    let mut sorted = items.clone();
                    sorted.sort_by(|a, b| {
                        match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            _ => std::cmp::Ordering::Equal,
                        }
                    });
                    Some(Value::List(sorted))
                } else {
                    return Err(RuntimeError { message: "sort requires a list".to_string() });
                }
            }
            "chars" => {
                let s = self.expect_text(&args[0])?;
                let chars: Vec<Value> = s.chars().map(|c| Value::Text(c.to_string())).collect();
                Some(Value::List(chars))
            }
            "char_at" => {
                let s = self.expect_text(&args[0])?;
                if let Value::Int(idx) = &args[1] {
                    s.chars().nth(*idx as usize)
                        .map(|c| Value::Text(c.to_string()))
                        .or(Some(Value::Null))
                } else {
                    return Err(RuntimeError { message: "char_at requires text and index".to_string() });
                }
            }
            "slice" => {
                match &args[0] {
                    Value::List(items) => {
                        if let (Value::Int(start), Value::Int(end)) = (&args[1], &args[2]) {
                            let s = (*start as usize).min(items.len());
                            let e = (*end as usize).min(items.len());
                            Some(Value::List(items[s..e].to_vec()))
                        } else {
                            return Err(RuntimeError { message: "slice requires start and end indices".to_string() });
                        }
                    }
                    Value::Text(s) => {
                        if let (Value::Int(start), Value::Int(end)) = (&args[1], &args[2]) {
                            let chars: Vec<char> = s.chars().collect();
                            let st = (*start as usize).min(chars.len());
                            let en = (*end as usize).min(chars.len());
                            let sub: String = chars[st..en].iter().collect();
                            Some(Value::Text(sub))
                        } else {
                            return Err(RuntimeError { message: "slice requires start and end indices".to_string() });
                        }
                    }
                    _ => return Err(RuntimeError { message: "slice requires a list or text".to_string() }),
                }
            }
            "append" => {
                if let (Value::List(a), Value::List(b)) = (&args[0], &args[1]) {
                    let mut combined = a.clone();
                    combined.extend(b.clone());
                    Some(Value::List(combined))
                } else {
                    return Err(RuntimeError { message: "append requires two lists".to_string() });
                }
            }
            "to_text" => {
                Some(Value::Text(format!("{}", args[0])))
            }
            "is_empty" => {
                match &args[0] {
                    Value::List(l) => Some(Value::Bool(l.is_empty())),
                    Value::Text(s) => Some(Value::Bool(s.is_empty())),
                    Value::Null => Some(Value::Bool(true)),
                    _ => Some(Value::Bool(false)),
                }
            }
            "mget" => {
                if let (Value::Map(pairs), key) = (&args[0], &args[1]) {
                    let found = pairs.iter().find(|(k, _)| match (k, key) {
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Text(a), Value::Text(b)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        _ => false,
                    });
                    Some(found.map(|(_, v)| v.clone()).unwrap_or(Value::Null))
                } else {
                    return Err(RuntimeError { message: "mget requires a map and key".to_string() });
                }
            }
            "mset" => {
                if let (Value::Map(pairs), key, val) = (&args[0], &args[1], &args[2]) {
                    let mut new_pairs: Vec<(Value, Value)> = pairs.iter()
                        .filter(|(k, _)| match (k, key) {
                            (Value::Int(a), Value::Int(b)) => a != b,
                            (Value::Text(a), Value::Text(b)) => a != b,
                            (Value::Bool(a), Value::Bool(b)) => a != b,
                            _ => true,
                        })
                        .cloned()
                        .collect();
                    new_pairs.push((key.clone(), val.clone()));
                    Some(Value::Map(new_pairs))
                } else {
                    return Err(RuntimeError { message: "mset requires a map, key, and value".to_string() });
                }
            }
            "mdel" => {
                if let (Value::Map(pairs), key) = (&args[0], &args[1]) {
                    let new_pairs: Vec<(Value, Value)> = pairs.iter()
                        .filter(|(k, _)| match (k, key) {
                            (Value::Int(a), Value::Int(b)) => a != b,
                            (Value::Text(a), Value::Text(b)) => a != b,
                            (Value::Bool(a), Value::Bool(b)) => a != b,
                            _ => true,
                        })
                        .cloned()
                        .collect();
                    Some(Value::Map(new_pairs))
                } else {
                    return Err(RuntimeError { message: "mdel requires a map and key".to_string() });
                }
            }
            "mkeys" => {
                if let Value::Map(pairs) = &args[0] {
                    Some(Value::List(pairs.iter().map(|(k, _)| k.clone()).collect()))
                } else {
                    return Err(RuntimeError { message: "mkeys requires a map".to_string() });
                }
            }
            "mvals" => {
                if let Value::Map(pairs) = &args[0] {
                    Some(Value::List(pairs.iter().map(|(_, v)| v.clone()).collect()))
                } else {
                    return Err(RuntimeError { message: "mvals requires a map".to_string() });
                }
            }
            "mhas" => {
                if let (Value::Map(pairs), key) = (&args[0], &args[1]) {
                    let found = pairs.iter().any(|(k, _)| match (k, key) {
                        (Value::Int(a), Value::Int(b)) => a == b,
                        (Value::Text(a), Value::Text(b)) => a == b,
                        (Value::Bool(a), Value::Bool(b)) => a == b,
                        _ => false,
                    });
                    Some(Value::Bool(found))
                } else {
                    return Err(RuntimeError { message: "mhas requires a map and key".to_string() });
                }
            }
            "find" => {
                let haystack = self.expect_text(&args[0])?;
                let needle = self.expect_text(&args[1])?;
                match haystack.find(needle) {
                    Some(pos) => Some(Value::Int(pos as i64)),
                    None => Some(Value::Int(-1)),
                }
            }
            "replace" => {
                let s = self.expect_text(&args[0])?;
                let old = self.expect_text(&args[1])?;
                let new = self.expect_text(&args[2])?;
                Some(Value::Text(s.replacen(old, new, 1)))
            }
            "set" => {
                if let (Value::List(items), Value::Int(idx)) = (&args[0], &args[1]) {
                    let i = *idx as usize;
                    if i < items.len() {
                        let mut new_items = items.clone();
                        new_items[i] = args[2].clone();
                        Some(Value::List(new_items))
                    } else {
                        Some(Value::List(items.clone()))
                    }
                } else {
                    return Err(RuntimeError {
                        message: "set requires a list and index".to_string(),
                    });
                }
            }
            "pop" => {
                if let Value::List(items) = &args[0] {
                    if items.is_empty() {
                        Some(Value::Tuple(vec![Value::List(Vec::new()), Value::Null]))
                    } else {
                        let last = items.last().unwrap().clone();
                        let new_list = Value::List(items[..items.len() - 1].to_vec());
                        Some(Value::Tuple(vec![new_list, last]))
                    }
                } else {
                    return Err(RuntimeError {
                        message: "pop requires a list".to_string(),
                    });
                }
            }
            "typeof" => {
                Some(Value::Text(args[0].type_name().to_string()))
            }
            "is" => {
                let type_name = self.expect_text(&args[0])?;
                let actual = args[1].type_name();
                Some(Value::Bool(type_name == actual))
            }
            // I/O & file builtins
            "read_file" => {
                let path = self.expect_text(&args[0])?;
                match std::fs::read_to_string(path) {
                    Ok(contents) => Some(Value::Text(contents)),
                    Err(e) => return Err(RuntimeError {
                        message: format!("read_file '{}': {}", path, e),
                    }),
                }
            }
            "write_file" => {
                let path = self.expect_text(&args[0])?;
                let content = self.expect_text(&args[1])?;
                match std::fs::write(path, content) {
                    Ok(()) => Some(Value::Null),
                    Err(e) => return Err(RuntimeError {
                        message: format!("write_file '{}': {}", path, e),
                    }),
                }
            }
            "env_get" => {
                let name = self.expect_text(&args[0])?;
                match std::env::var(name) {
                    Ok(val) => Some(Value::Text(val)),
                    Err(std::env::VarError::NotPresent) => Some(Value::Null),
                    Err(e) => return Err(RuntimeError {
                        message: format!("env_get '{}': {}", name, e),
                    }),
                }
            }
            "error" => {
                let msg = self.expect_text(&args[0])?;
                Some(Value::Err(msg.to_string()))
            }
            "zip" => {
                match (&args[0], &args[1]) {
                    (Value::List(a_items), Value::List(b_items)) => {
                        let len = a_items.len().min(b_items.len());
                        let mut results = Vec::with_capacity(len);
                        for i in 0..len {
                            results.push(Value::List(vec![
                                a_items[i].clone(),
                                b_items[i].clone(),
                            ]));
                        }
                        Some(Value::List(results))
                    }
                    _ => return Err(RuntimeError {
                        message: "zip requires two lists".to_string(),
                    }),
                }
            }
            "flatmap" => {
                // flatmap called as builtin: call flatmap fn list
                // args[0] = function, args[1] = list
                if let Value::List(items) = &args[1] {
                    let fn_val = &args[0];
                    let mut results = Vec::new();
                    for item in items {
                        let result = self.apply_fn(fn_val, &[item.clone()], &Env::new())?;
                        match result {
                            Value::List(sub_items) => results.extend(sub_items),
                            _ => {
                                return Err(RuntimeError {
                                    message: "flatmap function must return a list".to_string(),
                                });
                            }
                        }
                    }
                    Some(Value::List(results))
                } else {
                    return Err(RuntimeError {
                        message: "flatmap requires a list".to_string(),
                    });
                }
            }
            // JSON builtins
            "jparse" => {
                let text = self.expect_text(&args[0])?;
                Some(crate::json::json_parse(text)?)
            }
            "jstr" => {
                Some(Value::Text(crate::json::json_stringify(&args[0])?))
            }
            "jget" => {
                // jget data path_component1 path_component2 ...
                if args.is_empty() {
                    return Err(RuntimeError {
                        message: "jget requires at least 1 argument".to_string(),
                    });
                }
                let data = &args[0];
                let path = &args[1..];
                Some(crate::json::json_get(data, path))
            }
            "jset" => {
                // jset data path1 path2 ... new_value
                // Minimum 3 args: data, one path component, value
                if args.len() < 3 {
                    return Err(RuntimeError {
                        message: "jset requires at least 3 arguments (data, path, value)".to_string(),
                    });
                }
                let data = &args[0];
                let path = &args[1..args.len() - 1];
                let new_val = &args[args.len() - 1];
                Some(crate::json::json_set(data, path, new_val)?)
            }
            // HTTP builtins
            "http_get" => {
                let url = self.expect_text(&args[0])?;
                match ureq::get(url).call() {
                    Ok(response) => {
                        match response.into_body().read_to_string() {
                            Ok(body) => Some(Value::Text(body)),
                            Err(e) => Some(Value::Err(format!("http_get read error: {}", e))),
                        }
                    }
                    Err(e) => Some(Value::Err(format!("http_get error: {}", e))),
                }
            }
            "http_post" => {
                let url = self.expect_text(&args[0])?;
                let body = self.expect_text(&args[1])?;
                let content_type = if args.len() >= 3 {
                    self.expect_text(&args[2])?
                } else {
                    "application/json"
                };
                match ureq::post(url)
                    .header("Content-Type", content_type)
                    .send(body.as_bytes())
                {
                    Ok(response) => {
                        match response.into_body().read_to_string() {
                            Ok(resp_body) => Some(Value::Text(resp_body)),
                            Err(e) => Some(Value::Err(format!("http_post read error: {}", e))),
                        }
                    }
                    Err(e) => Some(Value::Err(format!("http_post error: {}", e))),
                }
            }
            _ => None,
        };
        Ok(result)
    }

    fn match_pattern(&self, pattern: &Pattern, value: &Value, env: &Env) -> Option<Env> {
        match pattern {
            Pattern::Wildcard => Some(env.clone()),
            Pattern::Literal(expr) => {
                if let Ok(pat_val) = self.eval_expr(expr, env) {
                    match (&pat_val, value) {
                        (Value::Int(a), Value::Int(b)) if a == b => Some(env.clone()),
                        (Value::Text(a), Value::Text(b)) if a == b => Some(env.clone()),
                        (Value::Bool(a), Value::Bool(b)) if a == b => Some(env.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Pattern::Variant { name, bindings } => {
                if let Value::Tuple(items) = value {
                    if let Some(Value::Text(variant)) = items.first() {
                        if variant == name {
                            let mut new_env = env.clone();
                            for (i, binding) in bindings.iter().enumerate() {
                                if let Some(val) = items.get(i + 1) {
                                    new_env.set(binding.clone(), val.clone());
                                }
                            }
                            return Some(new_env);
                        }
                    }
                }
                None
            }
        }
    }

    fn cast_value(&self, val: &Value, target: &AiType) -> Result<Value, RuntimeError> {
        match (val, target) {
            (Value::Int(n), AiType::F64) => Ok(Value::Float(*n as f64)),
            (Value::Int(n), AiType::F32) => Ok(Value::Float(*n as f64)),
            (Value::Int(n), AiType::Text) => Ok(Value::Text(n.to_string())),
            (Value::Float(f), AiType::I32) => Ok(Value::Int(*f as i64)),
            (Value::Float(f), AiType::I64) => Ok(Value::Int(*f as i64)),
            (Value::Float(f), AiType::Text) => Ok(Value::Text(f.to_string())),
            (Value::Bool(b), AiType::Text) => Ok(Value::Text(b.to_string())),
            (Value::Bool(b), AiType::I32) => Ok(Value::Int(if *b { 1 } else { 0 })),
            (Value::Text(s), AiType::I32) | (Value::Text(s), AiType::I64) => {
                Ok(s.parse::<i64>().map(Value::Int).unwrap_or(Value::Int(0)))
            }
            (Value::Text(s), AiType::F64) => {
                s.parse::<f64>()
                    .map(Value::Float)
                    .map_err(|_| RuntimeError {
                        message: format!("cannot cast '{}' to float", s),
                    })
            }
            _ => Err(RuntimeError {
                message: format!("cannot cast {} to {:?}", val.type_name(), target),
            }),
        }
    }

    fn expect_text<'a>(&self, val: &'a Value) -> Result<&'a str, RuntimeError> {
        match val {
            Value::Text(s) => Ok(s),
            _ => Err(RuntimeError {
                message: format!("expected text, got {}", val.type_name()),
            }),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "runtime error: {}", self.message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    /// Helper: parse + run an AILang program and return the result value
    fn run_program(source: &str) -> Value {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let program = parser.parse().expect("parser failed");
        let mut interp = Interpreter::new();
        interp.run(program, false).expect("runtime error")
    }

    /// Helper: parse + run, expecting a runtime error
    fn run_program_err(source: &str) -> RuntimeError {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let program = parser.parse().expect("parser failed");
        let mut interp = Interpreter::new();
        interp.run(program, false).expect_err("expected runtime error")
    }

    /// Helper: run tests only (test_only=true), returning Ok if all pass
    fn run_tests_only(source: &str) -> Result<Value, RuntimeError> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let program = parser.parse().expect("parser failed");
        let mut interp = Interpreter::new();
        interp.run(program, true)
    }

    // -------------------------------------------------------
    // Arithmetic: + - * / %
    // -------------------------------------------------------
    #[test]
    fn test_arithmetic_add() {
        let v = run_program("#entry\n  = + 3 4");
        assert!(matches!(v, Value::Int(7)));
    }

    #[test]
    fn test_arithmetic_sub() {
        let v = run_program("#entry\n  = - 10 3");
        assert!(matches!(v, Value::Int(7)));
    }

    #[test]
    fn test_arithmetic_mul() {
        let v = run_program("#entry\n  = * 6 7");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_arithmetic_div() {
        let v = run_program("#entry\n  = / 15 3");
        assert!(matches!(v, Value::Int(5)));
    }

    #[test]
    fn test_arithmetic_mod() {
        let v = run_program("#entry\n  = % 17 5");
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_arithmetic_negative() {
        let v = run_program("#entry\n  = + -3 -7");
        assert!(matches!(v, Value::Int(-10)));
    }

    #[test]
    fn test_division_by_zero() {
        let e = run_program_err("#entry\n  = / 1 0");
        assert!(e.message.contains("division by zero"));
    }

    // -------------------------------------------------------
    // Comparison: == != < >
    // -------------------------------------------------------
    #[test]
    fn test_comparison_eq_true() {
        let v = run_program("#entry\n  = == 5 5");
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_comparison_eq_false() {
        let v = run_program("#entry\n  = == 5 3");
        assert!(matches!(v, Value::Bool(false)));
    }

    #[test]
    fn test_comparison_neq() {
        let v = run_program("#entry\n  = != 5 3");
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_comparison_lt() {
        let v = run_program("#entry\n  = < 3 5");
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_comparison_gt() {
        let v = run_program("#fn f :bool\n  = (> 5 3)\n\n#entry\n  = call f");
        assert!(matches!(v, Value::Bool(true)));
    }

    // -------------------------------------------------------
    // Select laziness (only evaluates taken branch)
    // -------------------------------------------------------
    #[test]
    fn test_select_true_branch() {
        let v = run_program("#entry\n  = select true 42 99");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_select_false_branch() {
        let v = run_program("#entry\n  = select false 42 99");
        assert!(matches!(v, Value::Int(99)));
    }

    #[test]
    fn test_select_laziness_true() {
        // If select evaluated both branches eagerly, the false branch
        // would cause a division by zero. Since it's lazy, only the
        // true branch is evaluated.
        let v = run_program("#entry\n  = select true 1 (/ 1 0)");
        assert!(matches!(v, Value::Int(1)));
    }

    #[test]
    fn test_select_laziness_false() {
        // Conversely, the true branch would blow up but isn't evaluated.
        let v = run_program("#entry\n  = select false (/ 1 0) 2");
        assert!(matches!(v, Value::Int(2)));
    }

    // -------------------------------------------------------
    // Function call
    // -------------------------------------------------------
    #[test]
    fn test_function_call() {
        let v = run_program(
            "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#entry\n  = call add 10 20",
        );
        assert!(matches!(v, Value::Int(30)));
    }

    #[test]
    fn test_function_call_recursive() {
        let v = run_program(
            "#fn fact :i32 n:i32\n  = select (<= n 1) 1 (* n (call fact (- n 1)))\n\n#entry\n  = call fact 5",
        );
        assert!(matches!(v, Value::Int(120)));
    }

    // -------------------------------------------------------
    // Builtins: len, concat, push, get, head, tail, range, reverse, abs, min, max
    // -------------------------------------------------------
    #[test]
    fn test_builtin_len_list() {
        let v = run_program("#entry\n  = call len [1 2 3]");
        assert!(matches!(v, Value::Int(3)));
    }

    #[test]
    fn test_builtin_len_text() {
        let v = run_program("#entry\n  = call len \"hello\"");
        assert!(matches!(v, Value::Int(5)));
    }

    #[test]
    fn test_builtin_concat() {
        let v = run_program("#entry\n  = call concat \"hello\" \" world\"");
        assert!(matches!(v, Value::Text(ref s) if s == "hello world"));
    }

    #[test]
    fn test_builtin_push() {
        let v = run_program("#entry\n  v0 :[i32] = call push [1 2] 3\n  = call len v0");
        assert!(matches!(v, Value::Int(3)));
    }

    #[test]
    fn test_builtin_get() {
        let v = run_program("#entry\n  = call get [10 20 30] 1");
        assert!(matches!(v, Value::Int(20)));
    }

    // -------------------------------------------------------
    // safe_get: in-bounds, out-of-bounds, negative index
    // -------------------------------------------------------
    #[test]
    fn test_safe_get_list_in_bounds() {
        let v = run_program("#entry\n  = call safe_get [10 20 30] 1");
        assert!(matches!(v, Value::Int(20)));
    }

    #[test]
    fn test_safe_get_list_out_of_bounds() {
        let v = run_program("#entry\n  = call safe_get [10 20 30] 999");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_safe_get_list_negative_index() {
        let v = run_program("#entry\n  = call safe_get [10 20 30] -1");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_safe_get_text_in_bounds() {
        let v = run_program("#entry\n  = call safe_get \"hello\" 1");
        assert!(matches!(v, Value::Text(ref s) if s == "e"));
    }

    #[test]
    fn test_safe_get_text_out_of_bounds() {
        let v = run_program("#entry\n  = call safe_get \"hi\" 999");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_safe_get_text_negative_index() {
        let v = run_program("#entry\n  = call safe_get \"hi\" -1");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_builtin_head() {
        let v = run_program("#entry\n  = call head [10 20 30]");
        assert!(matches!(v, Value::Int(10)));
    }

    #[test]
    fn test_builtin_head_empty() {
        let v = run_program("#entry\n  = call head []");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_builtin_tail() {
        let v = run_program("#entry\n  v0 :[i32] = call tail [10 20 30]\n  = call len v0");
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_builtin_range() {
        let v = run_program("#entry\n  v0 :[i32] = call range 0 5\n  = call len v0");
        assert!(matches!(v, Value::Int(5)));
    }

    #[test]
    fn test_builtin_reverse() {
        let v = run_program("#entry\n  v0 :[i32] = call reverse [1 2 3]\n  = call head v0");
        assert!(matches!(v, Value::Int(3)));
    }

    #[test]
    fn test_builtin_abs() {
        let v = run_program("#entry\n  = call abs -42");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_builtin_abs_positive() {
        let v = run_program("#entry\n  = call abs 7");
        assert!(matches!(v, Value::Int(7)));
    }

    #[test]
    fn test_builtin_min() {
        let v = run_program("#entry\n  = call min 3 7");
        assert!(matches!(v, Value::Int(3)));
    }

    #[test]
    fn test_builtin_max() {
        let v = run_program("#entry\n  = call max 3 7");
        assert!(matches!(v, Value::Int(7)));
    }

    // -------------------------------------------------------
    // Map operations: mget, mset, mdel, mkeys, mhas
    // -------------------------------------------------------
    #[test]
    fn test_mget() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1 \"b\" 2}\n  = call mget v0 \"b\"");
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_mget_missing() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1}\n  = call mget v0 \"z\"");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_mset() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1}\n  v1 :any = call mset v0 \"b\" 2\n  = call mget v1 \"b\"");
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_mset_overwrite() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1}\n  v1 :any = call mset v0 \"a\" 99\n  = call mget v1 \"a\"");
        assert!(matches!(v, Value::Int(99)));
    }

    #[test]
    fn test_mdel() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1 \"b\" 2}\n  v1 :any = call mdel v0 \"a\"\n  = call mhas v1 \"a\"");
        assert!(matches!(v, Value::Bool(false)));
    }

    #[test]
    fn test_mkeys() {
        let v = run_program("#entry\n  v0 :any = {\"x\" 1 \"y\" 2}\n  v1 :[text] = call mkeys v0\n  = call len v1");
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_mhas_true() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1}\n  = call mhas v0 \"a\"");
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_mhas_false() {
        let v = run_program("#entry\n  v0 :any = {\"a\" 1}\n  = call mhas v0 \"z\"");
        assert!(matches!(v, Value::Bool(false)));
    }

    // -------------------------------------------------------
    // Fold, map, filter
    // -------------------------------------------------------
    #[test]
    fn test_fold_sum() {
        let v = run_program(
            "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#entry\n  = fold [1 2 3 4 5] 0 add",
        );
        assert!(matches!(v, Value::Int(15)));
    }

    #[test]
    fn test_fold_with_lambda() {
        let v = run_program(
            "#entry\n  = fold [1 2 3] 0 (fn a:i32 b:i32 => + a b)",
        );
        assert!(matches!(v, Value::Int(6)));
    }

    #[test]
    fn test_map_with_fn() {
        let v = run_program(
            "#fn double :i32 x:i32\n  = * x 2\n\n#entry\n  v0 :[i32] = map double [1 2 3]\n  = call get v0 2",
        );
        assert!(matches!(v, Value::Int(6)));
    }

    #[test]
    fn test_map_with_lambda() {
        let v = run_program(
            "#entry\n  v0 :[i32] = map (fn x:i32 => + x 10) [1 2 3]\n  = call get v0 0",
        );
        assert!(matches!(v, Value::Int(11)));
    }

    #[test]
    fn test_filter_with_lambda() {
        let v = run_program(
            "#entry\n  v0 :[i32] = filter (fn x:i32 => (> x 2)) [1 2 3 4 5]\n  = call len v0",
        );
        assert!(matches!(v, Value::Int(3)));
    }

    // -------------------------------------------------------
    // Cast operations
    // -------------------------------------------------------
    #[test]
    fn test_cast_int_to_text() {
        let v = run_program("#entry\n  = cast text 42");
        assert!(matches!(v, Value::Text(ref s) if s == "42"));
    }

    #[test]
    fn test_cast_text_to_int() {
        let v = run_program("#entry\n  = cast i32 \"123\"");
        assert!(matches!(v, Value::Int(123)));
    }

    #[test]
    fn test_cast_int_to_float() {
        let v = run_program("#entry\n  = cast f64 5");
        if let Value::Float(f) = v {
            assert!((f - 5.0).abs() < f64::EPSILON);
        } else {
            panic!("expected float");
        }
    }

    #[test]
    fn test_cast_float_to_int() {
        let v = run_program("#entry\n  = cast i32 3.7");
        assert!(matches!(v, Value::Int(3)));
    }

    #[test]
    fn test_cast_bool_to_int() {
        let v = run_program("#entry\n  = cast i32 true");
        assert!(matches!(v, Value::Int(1)));
    }

    #[test]
    fn test_cast_bool_to_text() {
        let v = run_program("#entry\n  = cast text false");
        assert!(matches!(v, Value::Text(ref s) if s == "false"));
    }

    // -------------------------------------------------------
    // read_line is recognized as a builtin (no "unknown builtin" error)
    // -------------------------------------------------------
    #[test]
    fn test_read_line_is_recognized() {
        // We cannot actually call read_line in a test (it reads stdin),
        // but we verify it doesn't produce "unknown built-in" by checking
        // that it exists in the try_builtin match arms.
        let interp = Interpreter::new();
        // In test context, stdin is closed so read_line hits EOF.
        // The important thing is that the error says "end of input",
        // NOT "unknown built-in" (which would mean it's not recognized).
        let result = interp.try_builtin("read_line", &[]);
        match result {
            Ok(Some(_)) => {} // read_line succeeded (unlikely in test)
            Err(e) => assert!(e.message.contains("end of input"), "expected EOF error, got: {}", e.message),
            Ok(None) => panic!("read_line should be a recognized builtin, got None"),
        }
    }

    // -------------------------------------------------------
    // Test block execution
    // -------------------------------------------------------
    #[test]
    fn test_ailang_test_passes() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#test add_works\n  v0 :i32 = call add 2 3\n  assert == v0 5";
        let result = run_tests_only(src);
        assert!(result.is_ok());
    }

    #[test]
    fn test_ailang_test_fails() {
        let src = "#fn add :i32 a:i32 b:i32\n  = + a b\n\n#test add_wrong\n  v0 :i32 = call add 2 3\n  assert == v0 99";
        let result = run_tests_only(src);
        assert!(result.is_err());
    }

    // -------------------------------------------------------
    // Text operations
    // -------------------------------------------------------
    #[test]
    fn test_text_concatenation_via_add() {
        let v = run_program("#entry\n  = + \"hello\" \" world\"");
        assert!(matches!(v, Value::Text(ref s) if s == "hello world"));
    }

    // -------------------------------------------------------
    // Null handling
    // -------------------------------------------------------
    #[test]
    fn test_null_equality() {
        let v = run_program("#entry\n  = == null null");
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_null_neq() {
        let v = run_program("#entry\n  = != null 1");
        assert!(matches!(v, Value::Bool(true)));
    }

    // -------------------------------------------------------
    // Unary ops
    // -------------------------------------------------------
    #[test]
    fn test_neg() {
        let v = run_program("#entry\n  = neg 42");
        assert!(matches!(v, Value::Int(-42)));
    }

    #[test]
    fn test_not() {
        let v = run_program("#entry\n  = not true");
        assert!(matches!(v, Value::Bool(false)));
    }

    // -------------------------------------------------------
    // Logic
    // -------------------------------------------------------
    #[test]
    fn test_and() {
        let v = run_program("#entry\n  = and true false");
        assert!(matches!(v, Value::Bool(false)));
    }

    #[test]
    fn test_or() {
        let v = run_program("#entry\n  = or false true");
        assert!(matches!(v, Value::Bool(true)));
    }

    // -------------------------------------------------------
    // Cond expression
    // -------------------------------------------------------
    #[test]
    fn test_cond_two_branches_first_matches() {
        let v = run_program(
            "#fn classify :text x:i32\n  = cond (== x 0) \"zero\" (> x 0) \"pos\" \"neg\"\n\n#entry\n  = call classify 0",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "zero"));
    }

    #[test]
    fn test_cond_two_branches_second_matches() {
        let v = run_program(
            "#fn classify :text x:i32\n  = cond (== x 0) \"zero\" (> x 0) \"pos\" \"neg\"\n\n#entry\n  = call classify 5",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "pos"));
    }

    #[test]
    fn test_cond_two_branches_default() {
        let v = run_program(
            "#fn classify :text x:i32\n  = cond (== x 0) \"zero\" (> x 0) \"pos\" \"neg\"\n\n#entry\n  = call classify -3",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "neg"));
    }

    #[test]
    fn test_cond_single_branch_match() {
        // Equivalent to select
        let v = run_program("#entry\n  = cond true 42 99");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_cond_single_branch_default() {
        let v = run_program("#entry\n  = cond false 42 99");
        assert!(matches!(v, Value::Int(99)));
    }

    #[test]
    fn test_cond_laziness_first_branch() {
        // If cond evaluated later branches eagerly, division by zero would fire.
        // Since it's lazy, only the matched branch is evaluated.
        let v = run_program("#entry\n  = cond true 1 false (/ 1 0) 99");
        assert!(matches!(v, Value::Int(1)));
    }

    #[test]
    fn test_cond_laziness_default() {
        // Neither branch condition is true, so we get default.
        // The values of branches that don't match are never evaluated.
        let v = run_program("#entry\n  = cond false (/ 1 0) false (/ 1 0) 42");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_cond_nested() {
        // Nested cond inside a cond value
        let v = run_program(
            "#entry\n  v0 :i32 = 5\n  = cond (> v0 10) \"big\" (> v0 0) (cond (> v0 3) \"medium\" \"small\") \"negative\"",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "medium"));
    }

    // -------------------------------------------------------
    // File I/O & env builtins
    // -------------------------------------------------------
    #[test]
    fn test_read_file_not_found() {
        let e = run_program_err(
            "#entry\n  v0 :text = call read_file \"__nonexistent_ailang_test_file_12345.txt\"\n  = v0",
        );
        assert!(
            e.message.contains("read_file"),
            "error should mention read_file, got: {}",
            e.message
        );
    }

    #[test]
    fn test_write_and_read_file() {
        let mut tmp = std::env::temp_dir();
        tmp.push("ailang_test_write_read.txt");
        let path = tmp.to_str().unwrap().replace('\\', "\\\\");
        let src = format!(
            "#entry\n  call write_file \"{}\" \"hello from test\"\n  v0 :text = call read_file \"{}\"\n  = v0",
            path, path
        );
        let v = run_program(&src);
        // Clean up
        let _ = std::fs::remove_file(&tmp);
        assert!(
            matches!(v, Value::Text(ref s) if s == "hello from test"),
            "expected 'hello from test', got: {:?}",
            v
        );
    }

    #[test]
    fn test_env_get_exists() {
        // PATH is set on virtually every OS
        let v = run_program("#entry\n  = call env_get \"PATH\"");
        assert!(
            matches!(v, Value::Text(_)),
            "expected PATH to be text, got: {:?}",
            v
        );
    }

    #[test]
    fn test_env_get_missing() {
        let v = run_program(
            "#entry\n  = call env_get \"__AILANG_NONEXISTENT_VAR_XYZ_999__\"",
        );
        assert!(
            matches!(v, Value::Null),
            "expected null for missing env var, got: {:?}",
            v
        );
    }

    // -------------------------------------------------------
    // Pipeline operator |>
    // -------------------------------------------------------
    #[test]
    fn test_pipeline_simple() {
        // 5 |> double => call double 5 => 10
        let v = run_program(
            "#fn double :i32 x:i32\n  = * x 2\n\n#entry\n  = 5 |> double",
        );
        assert!(matches!(v, Value::Int(10)));
    }

    #[test]
    fn test_pipeline_chain() {
        // 5 |> double |> double => call double (call double 5) => 20
        let v = run_program(
            "#fn double :i32 x:i32\n  = * x 2\n\n#entry\n  = 5 |> double |> double",
        );
        assert!(matches!(v, Value::Int(20)));
    }

    #[test]
    fn test_pipeline_with_builtins() {
        // "  hello  " |> trim |> upper => "HELLO"
        let v = run_program(
            "#entry\n  = \"  hello  \" |> trim |> upper",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "HELLO"));
    }

    #[test]
    fn test_pipeline_with_extra_args() {
        // 5 |> double |> add 3 => call add (call double 5) 3 => add(10, 3) => 13
        let v = run_program(
            "#fn double :i32 x:i32\n  = * x 2\n\n#fn add :i32 a:i32 b:i32\n  = + a b\n\n#entry\n  = 5 |> double |> add 3",
        );
        assert!(matches!(v, Value::Int(13)));
    }

    #[test]
    fn test_pipeline_in_bind() {
        // Pipeline in a bind statement
        let v = run_program(
            "#fn double :i32 x:i32\n  = * x 2\n\n#entry\n  v0 :i32 = 5 |> double\n  = v0",
        );
        assert!(matches!(v, Value::Int(10)));
    }

    #[test]
    fn test_pipeline_with_concat() {
        // "hello" |> concat " world" |> upper => "HELLO WORLD"
        let v = run_program(
            "#entry\n  = \"hello\" |> concat \" world\" |> upper",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "HELLO WORLD"));
    }

    // -------------------------------------------------------
    // Tail-call optimization (TCO)
    // -------------------------------------------------------

    #[test]
    fn test_tco_deep_recursion_countdown() {
        // Without TCO this would stack overflow. countdown(10000) -> 0
        let v = run_program(
            "#fn countdown :i32 n:i32\n  = select (<= n 0) 0 (call countdown (- n 1))\n\n#entry\n  v0 :i32 = call countdown 10000\n  = v0",
        );
        assert!(matches!(v, Value::Int(0)));
    }

    #[test]
    fn test_tco_tail_call_through_select_true_branch() {
        // Tail call in the true branch of select
        let v = run_program(
            "#fn count_up :i32 n:i32 target:i32\n  = select (>= n target) n (call count_up (+ n 1) target)\n\n#entry\n  = call count_up 0 5000",
        );
        assert!(matches!(v, Value::Int(5000)));
    }

    #[test]
    fn test_tco_tail_call_through_select_false_branch() {
        // Tail call in the false branch of select
        let v = run_program(
            "#fn countdown :i32 n:i32\n  = select (<= n 0) 0 (call countdown (- n 1))\n\n#entry\n  = call countdown 5000",
        );
        assert!(matches!(v, Value::Int(0)));
    }

    #[test]
    fn test_tco_tail_call_through_cond() {
        // All cond branches are in tail position
        let v = run_program(
            "#fn classify_loop :i32 n:i32\n  = cond (<= n 0) 0 (> n 5000) (call classify_loop (- n 2)) (call classify_loop (- n 1))\n\n#entry\n  = call classify_loop 10000",
        );
        assert!(matches!(v, Value::Int(0)));
    }

    #[test]
    fn test_tco_cond_default_branch() {
        // Tail call in the default branch of cond
        let v = run_program(
            "#fn loop_down :i32 n:i32\n  = cond (== n 0) 42 (call loop_down (- n 1))\n\n#entry\n  = call loop_down 8000",
        );
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_tco_mutual_recursion() {
        // Function A calls B, B calls A -- both in tail position
        let v = run_program(
            "#fn is_even :bool n:i32\n  = select (<= n 0) true (call is_odd (- n 1))\n\n#fn is_odd :bool n:i32\n  = select (<= n 0) false (call is_even (- n 1))\n\n#entry\n  = call is_even 10000",
        );
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_tco_mutual_recursion_odd() {
        // Verify mutual recursion with odd number
        let v = run_program(
            "#fn is_even :bool n:i32\n  = select (<= n 0) true (call is_odd (- n 1))\n\n#fn is_odd :bool n:i32\n  = select (<= n 0) false (call is_even (- n 1))\n\n#entry\n  = call is_odd 9999",
        );
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_tco_non_tail_call_still_works() {
        // This is NOT a tail call because there's work after the recursive call:
        // (* n (call fact (- n 1))) -- the multiplication happens after the call returns.
        // This should still produce the correct result (no TCO applied).
        let v = run_program(
            "#fn fact :i32 n:i32\n  = select (<= n 1) 1 (* n (call fact (- n 1)))\n\n#entry\n  = call fact 10",
        );
        assert!(matches!(v, Value::Int(3628800)));
    }

    #[test]
    fn test_tco_non_tail_call_addition_after_recursion() {
        // (+ n (call sum_to (- n 1))) is NOT a tail call because + wraps it.
        // Using small depth (10) to avoid stack overflow since this cannot use TCO.
        // Debug builds use large stack frames per interpreter recursion level.
        let v = run_program(
            "#fn sum_to :i32 n:i32\n  = select (<= n 0) 0 (+ n (call sum_to (- n 1)))\n\n#entry\n  = call sum_to 10",
        );
        // sum 1..10 = 55
        assert!(matches!(v, Value::Int(55)));
    }

    #[test]
    fn test_tco_tail_call_to_builtin() {
        // A tail call to a builtin function should still work correctly
        let v = run_program(
            "#fn my_len :i32 lst:[i32]\n  = call len lst\n\n#entry\n  = call my_len [1 2 3 4 5]",
        );
        assert!(matches!(v, Value::Int(5)));
    }

    #[test]
    fn test_tco_accumulator_pattern() {
        // Classic TCO-friendly accumulator pattern for sum
        let v = run_program(
            "#fn sum_acc :i32 n:i32 acc:i32\n  = select (<= n 0) acc (call sum_acc (- n 1) (+ acc n))\n\n#entry\n  = call sum_acc 10000 0",
        );
        // sum 1..10000 = 50005000
        assert!(matches!(v, Value::Int(50005000)));
    }

    // -------------------------------------------------------
    // Error handling: error builtin, ? propagation, #err blocks
    // -------------------------------------------------------

    #[test]
    fn test_error_value_creation() {
        // `error "oops"` creates a Value::Err
        let v = run_program("#entry\n  = error \"oops\"");
        assert!(matches!(v, Value::Err(ref s) if s == "oops"));
    }

    #[test]
    fn test_error_value_creation_via_call() {
        // `call error "boom"` also creates a Value::Err
        let v = run_program("#entry\n  = call error \"boom\"");
        assert!(matches!(v, Value::Err(ref s) if s == "boom"));
    }

    #[test]
    fn test_propagate_error() {
        // ? on an error value propagates it as a RuntimeError
        let e = run_program_err(
            "#fn failing :i32\n  = error \"bad\"\n\n#fn caller :i32\n  v0 :i32 = (call failing)?\n  = v0\n\n#entry\n  = call caller",
        );
        assert!(e.message.contains("bad"), "expected 'bad' in error: {}", e.message);
    }

    #[test]
    fn test_propagate_value() {
        // ? on a normal (non-error) value passes it through unchanged
        let v = run_program("#entry\n  v0 :i32 = 42?\n  = v0");
        assert!(matches!(v, Value::Int(42)));
    }

    #[test]
    fn test_propagate_unwraps_ok() {
        // ? on Value::Ok(v) unwraps to v
        let v = run_program("#entry\n  v0 :i32 = (ok 99)?\n  = v0");
        assert!(matches!(v, Value::Int(99)));
    }

    #[test]
    fn test_err_handler_catches() {
        // #err block catches an error from a function and provides a fallback
        let v = run_program(
            "#fn might_fail :text\n  = error \"oops\"\n\n#err might_fail\n  fallback \"recovered\"\n\n#entry\n  = call might_fail",
        );
        assert!(
            matches!(v, Value::Text(ref s) if s == "recovered"),
            "expected 'recovered', got: {:?}",
            v
        );
    }

    #[test]
    fn test_err_handler_not_triggered() {
        // #err block does NOT run when the function succeeds
        let v = run_program(
            "#fn works :text\n  = \"ok\"\n\n#err works\n  fallback \"fallback\"\n\n#entry\n  = call works",
        );
        assert!(
            matches!(v, Value::Text(ref s) if s == "ok"),
            "expected 'ok', got: {:?}",
            v
        );
    }

    #[test]
    fn test_err_handler_with_err_variable() {
        // The fallback expression can reference `err` (the error message)
        let v = run_program(
            "#fn boom :text\n  = error \"kaboom\"\n\n#err boom\n  fallback err\n\n#entry\n  = call boom",
        );
        assert!(
            matches!(v, Value::Text(ref s) if s == "kaboom"),
            "expected 'kaboom', got: {:?}",
            v
        );
    }

    #[test]
    fn test_error_is_check() {
        // `is "err" val` returns true when val is an error
        let v = run_program(
            "#entry\n  v0 :any = error \"oops\"\n  = call is \"err\" v0",
        );
        assert!(matches!(v, Value::Bool(true)));
    }

    #[test]
    fn test_error_is_check_not_error() {
        // `is "err" val` returns false for non-error values
        let v = run_program("#entry\n  = call is \"err\" 42");
        assert!(matches!(v, Value::Bool(false)));
    }

    #[test]
    fn test_error_propagation_chain() {
        // Error propagates through multiple function calls
        let v = run_program(
            "#fn inner :i32\n  = error \"deep\"\n\n#fn middle :i32\n  v0 :i32 = (call inner)?\n  = + v0 1\n\n#fn outer :text\n  v0 :i32 = (call middle)?\n  = cast text v0\n\n#err outer\n  fallback err\n\n#entry\n  = call outer",
        );
        assert!(
            matches!(v, Value::Text(ref s) if s.contains("deep")),
            "expected error containing 'deep', got: {:?}",
            v
        );
    }

    #[test]
    fn test_error_select_conditional() {
        // error inside select — only created when the branch is taken
        let v = run_program(
            "#fn safe_div :f64 a:f64 b:f64\n  = select (== b 0.0) (error \"division by zero\") (/ a b)\n\n#entry\n  = call safe_div 10.0 2.0",
        );
        if let Value::Float(f) = v {
            assert!((f - 5.0).abs() < f64::EPSILON);
        } else {
            panic!("expected float, got: {:?}", v);
        }
    }

    #[test]
    fn test_error_select_error_path() {
        // error path of select returns Value::Err
        let v = run_program(
            "#fn safe_div :f64 a:f64 b:f64\n  = select (== b 0.0) (error \"division by zero\") (/ a b)\n\n#entry\n  = call safe_div 10.0 0.0",
        );
        assert!(
            matches!(v, Value::Err(ref s) if s == "division by zero"),
            "expected error, got: {:?}",
            v
        );
    }

    #[test]
    fn test_err_handler_catches_propagated_error() {
        // #err handler catches errors that bubble up via ? inside the function
        let v = run_program(
            "#fn inner :i32\n  = error \"inner_fail\"\n\n#fn outer :i32\n  v0 :i32 = (call inner)?\n  = + v0 1\n\n#err outer\n  fallback 0\n\n#entry\n  = call outer",
        );
        assert!(matches!(v, Value::Int(0)), "expected 0, got: {:?}", v);
    }

    // -------------------------------------------------------
    // Zip: combine two lists into list of tuples
    // -------------------------------------------------------
    #[test]
    fn test_zip_basic() {
        // zip [1 2 3] ["a" "b" "c"] => [[1 "a"] [2 "b"] [3 "c"]]
        let v = run_program(
            "#entry\n  v0 :[[any]] = zip [1 2 3] [\"a\" \"b\" \"c\"]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 3);
            // Check first pair
            if let Value::List(pair) = &items[0] {
                assert!(matches!(pair[0], Value::Int(1)));
                assert!(matches!(&pair[1], Value::Text(s) if s == "a"));
            } else {
                panic!("expected list pair, got: {:?}", items[0]);
            }
            // Check third pair
            if let Value::List(pair) = &items[2] {
                assert!(matches!(pair[0], Value::Int(3)));
                assert!(matches!(&pair[1], Value::Text(s) if s == "c"));
            } else {
                panic!("expected list pair, got: {:?}", items[2]);
            }
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    #[test]
    fn test_zip_unequal_lengths() {
        // zip truncates to shorter list length
        let v = run_program(
            "#entry\n  v0 :[[any]] = zip [1 2 3] [\"a\" \"b\"]\n  = call len v0",
        );
        assert!(matches!(v, Value::Int(2)));
    }

    #[test]
    fn test_zip_empty() {
        // zip with an empty list returns empty
        let v = run_program(
            "#entry\n  v0 :[[any]] = zip [] [1 2 3]\n  = call len v0",
        );
        assert!(matches!(v, Value::Int(0)));
    }

    #[test]
    fn test_zip_via_call() {
        // zip also works via call syntax
        let v = run_program(
            "#entry\n  v0 :[[any]] = call zip [10 20] [30 40]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 2);
            if let Value::List(pair) = &items[0] {
                assert!(matches!(pair[0], Value::Int(10)));
                assert!(matches!(pair[1], Value::Int(30)));
            } else {
                panic!("expected list pair, got: {:?}", items[0]);
            }
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    #[test]
    fn test_zip_single_element() {
        // zip with single-element lists
        let v = run_program(
            "#entry\n  v0 :[[any]] = zip [42] [\"x\"]\n  = call len v0",
        );
        assert!(matches!(v, Value::Int(1)));
    }

    // -------------------------------------------------------
    // FlatMap: map then flatten one level
    // -------------------------------------------------------
    #[test]
    fn test_flatmap_basic() {
        // flatmap (fn x:i32 => [x (* x x)]) [1 2 3] => [1 1 2 4 3 9]
        let v = run_program(
            "#entry\n  v0 :[i32] = flatmap (fn x:i32 => [x (* x x)]) [1 2 3]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 6);
            let expected = vec![1i64, 1, 2, 4, 3, 9];
            for (i, exp) in expected.iter().enumerate() {
                assert!(
                    matches!(&items[i], Value::Int(n) if n == exp),
                    "at index {}: expected {}, got {:?}",
                    i, exp, items[i]
                );
            }
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    #[test]
    fn test_flatmap_empty_sublists() {
        // flatmap that returns empty lists for some elements
        let v = run_program(
            "#entry\n  v0 :[i32] = flatmap (fn x:i32 => select (> x 2) [x] []) [1 2 3 4]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], Value::Int(3)));
            assert!(matches!(items[1], Value::Int(4)));
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    #[test]
    fn test_flatmap_empty_input() {
        // flatmap on empty list returns empty
        let v = run_program(
            "#entry\n  v0 :[i32] = flatmap (fn x:i32 => [x x]) []\n  = call len v0",
        );
        assert!(matches!(v, Value::Int(0)));
    }

    #[test]
    fn test_flatmap_with_named_fn() {
        // flatmap with a named function reference
        let v = run_program(
            "#fn dup :[i32] x:i32\n  = [x x]\n\n#entry\n  v0 :[i32] = flatmap dup [5 10]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 4);
            assert!(matches!(items[0], Value::Int(5)));
            assert!(matches!(items[1], Value::Int(5)));
            assert!(matches!(items[2], Value::Int(10)));
            assert!(matches!(items[3], Value::Int(10)));
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    #[test]
    fn test_flatmap_via_call() {
        // flatmap also works via call syntax
        let v = run_program(
            "#fn dup :[i32] x:i32\n  = [x x]\n\n#entry\n  v0 :[i32] = call flatmap dup [3 7]\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 4);
            assert!(matches!(items[0], Value::Int(3)));
            assert!(matches!(items[1], Value::Int(3)));
            assert!(matches!(items[2], Value::Int(7)));
            assert!(matches!(items[3], Value::Int(7)));
        } else {
            panic!("expected list, got: {:?}", v);
        }
    }

    // -------------------------------------------------------
    // JSON builtins: jparse, jstr, jget, jset
    // -------------------------------------------------------

    #[test]
    fn test_jparse_object() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"name\\\": \\\"AI\\\", \\\"age\\\": 3}\"\n  = v0",
        );
        if let Value::Map(pairs) = &v {
            assert_eq!(pairs.len(), 2);
            // Check name key
            let name_val = pairs.iter().find(|(k, _)| matches!(k, Value::Text(s) if s == "name"));
            assert!(name_val.is_some(), "expected 'name' key in map");
            assert!(matches!(name_val.unwrap().1, Value::Text(ref s) if s == "AI"));
            // Check age key
            let age_val = pairs.iter().find(|(k, _)| matches!(k, Value::Text(s) if s == "age"));
            assert!(age_val.is_some(), "expected 'age' key in map");
            assert!(matches!(age_val.unwrap().1, Value::Int(3)));
        } else {
            panic!("expected Map, got: {:?}", v);
        }
    }

    #[test]
    fn test_jparse_array() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"[1, 2, 3]\"\n  = v0",
        );
        if let Value::List(items) = &v {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0], Value::Int(1)));
            assert!(matches!(items[1], Value::Int(2)));
            assert!(matches!(items[2], Value::Int(3)));
        } else {
            panic!("expected List, got: {:?}", v);
        }
    }

    #[test]
    fn test_jparse_nested() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"users\\\": [{\\\"name\\\": \\\"Alice\\\"}, {\\\"name\\\": \\\"Bob\\\"}]}\"\n  = v0",
        );
        if let Value::Map(pairs) = &v {
            assert_eq!(pairs.len(), 1);
            if let Value::List(users) = &pairs[0].1 {
                assert_eq!(users.len(), 2);
                if let Value::Map(user0) = &users[0] {
                    assert!(matches!(&user0[0].1, Value::Text(s) if s == "Alice"));
                } else {
                    panic!("expected inner map");
                }
            } else {
                panic!("expected inner list");
            }
        } else {
            panic!("expected Map, got: {:?}", v);
        }
    }

    #[test]
    fn test_jparse_primitives() {
        // string
        let v = run_program("#entry\n  = call jparse \"\\\"hello\\\"\"");
        assert!(matches!(v, Value::Text(ref s) if s == "hello"));

        // number
        let v = run_program("#entry\n  = call jparse \"42\"");
        assert!(matches!(v, Value::Int(42)));

        // boolean true
        let v = run_program("#entry\n  = call jparse \"true\"");
        assert!(matches!(v, Value::Bool(true)));

        // boolean false
        let v = run_program("#entry\n  = call jparse \"false\"");
        assert!(matches!(v, Value::Bool(false)));

        // null
        let v = run_program("#entry\n  = call jparse \"null\"");
        assert!(matches!(v, Value::Null));
    }

    #[test]
    fn test_jparse_invalid() {
        let e = run_program_err("#entry\n  = call jparse \"{invalid}\"");
        assert!(e.message.contains("JSON parse error"), "expected JSON parse error, got: {}", e.message);
    }

    #[test]
    fn test_jstr_object() {
        let v = run_program(
            "#entry\n  v0 :any = {\"name\" \"AI\"}\n  = call jstr v0",
        );
        if let Value::Text(s) = &v {
            assert!(s.contains("\"name\""), "expected key 'name' in: {}", s);
            assert!(s.contains("\"AI\""), "expected value 'AI' in: {}", s);
        } else {
            panic!("expected Text, got: {:?}", v);
        }
    }

    #[test]
    fn test_jstr_array() {
        let v = run_program(
            "#entry\n  v0 :any = [1 2 3]\n  = call jstr v0",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "[1,2,3]"));
    }

    #[test]
    fn test_jstr_primitives() {
        // number
        let v = run_program("#entry\n  = call jstr 42");
        assert!(matches!(v, Value::Text(ref s) if s == "42"));

        // string
        let v = run_program("#entry\n  = call jstr \"hello\"");
        assert!(matches!(v, Value::Text(ref s) if s == "\"hello\""));

        // boolean
        let v = run_program("#entry\n  = call jstr true");
        assert!(matches!(v, Value::Text(ref s) if s == "true"));

        // null
        let v = run_program("#entry\n  = call jstr null");
        assert!(matches!(v, Value::Text(ref s) if s == "null"));
    }

    #[test]
    fn test_jget_simple() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"name\\\": \\\"AI\\\", \\\"age\\\": 3}\"\n  = call jget v0 \"name\"",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "AI"), "expected 'AI', got: {:?}", v);
    }

    #[test]
    fn test_jget_nested() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"users\\\": [{\\\"name\\\": \\\"Alice\\\"}, {\\\"name\\\": \\\"Bob\\\"}]}\"\n  = call jget v0 \"users\" 1 \"name\"",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "Bob"), "expected 'Bob', got: {:?}", v);
    }

    #[test]
    fn test_jget_missing() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"name\\\": \\\"AI\\\"}\"\n  = call jget v0 \"missing\"",
        );
        assert!(matches!(v, Value::Null), "expected Null, got: {:?}", v);
    }

    #[test]
    fn test_jset_simple() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"name\\\": \\\"AI\\\"}\"\n  v1 :any = call jset v0 \"name\" \"Bot\"\n  = call jget v1 \"name\"",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "Bot"), "expected 'Bot', got: {:?}", v);
    }

    #[test]
    fn test_jset_nested() {
        let v = run_program(
            "#entry\n  v0 :any = call jparse \"{\\\"users\\\": [{\\\"name\\\": \\\"Alice\\\"}, {\\\"name\\\": \\\"Bob\\\"}]}\"\n  v1 :any = call jset v0 \"users\" 0 \"name\" \"Charlie\"\n  = call jget v1 \"users\" 0 \"name\"",
        );
        assert!(matches!(v, Value::Text(ref s) if s == "Charlie"), "expected 'Charlie', got: {:?}", v);
    }

    // -------------------------------------------------------
    // HTTP builtins: http_get, http_post
    // -------------------------------------------------------

    #[test]
    fn test_http_get_invalid_url() {
        // http_get with an invalid URL should return Value::Err, not a RuntimeError
        let v = run_program(
            "#entry\n  = call http_get \"http://invalid.localhost.test\"",
        );
        assert!(
            matches!(v, Value::Err(_)),
            "expected Value::Err for invalid URL, got: {:?}",
            v
        );
    }

    #[test]
    fn test_http_post_invalid_url() {
        // http_post with an invalid URL should return Value::Err, not a RuntimeError
        let v = run_program(
            "#entry\n  = call http_post \"http://invalid.localhost.test\" \"{}\"",
        );
        assert!(
            matches!(v, Value::Err(_)),
            "expected Value::Err for invalid URL, got: {:?}",
            v
        );
    }

    #[test]
    fn test_http_get_is_recognized() {
        // Verify http_get is a recognized builtin (returns Some, not None)
        let interp = Interpreter::new();
        let args = vec![Value::Text("http://invalid.localhost.test".to_string())];
        let result = interp.try_builtin("http_get", &args);
        match result {
            Ok(Some(_)) => {} // recognized and returned a value (likely an Err value)
            Ok(None) => panic!("http_get should be a recognized builtin, got None"),
            Err(_) => {} // recognized but threw an error (still recognized)
        }
    }
}
