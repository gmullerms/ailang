/// Interpreter for AILang
/// Tree-walking interpreter that executes the AST directly.
/// Immutable environments with scope chaining.

use crate::ast::*;
use std::collections::HashMap;

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
}

pub struct Interpreter {
    functions: HashMap<String, FnDecl>,
    type_defs: HashMap<String, TypeDecl>,
    constants: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            functions: HashMap::new(),
            type_defs: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    pub fn run(&mut self, program: Program) -> Result<Value, RuntimeError> {
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

        // Run tests if any
        for test in &program.tests {
            self.run_test(test)?;
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

            Expr::Builtin { name, args } => {
                let arg_vals: Result<Vec<_>, _> =
                    args.iter().map(|a| self.eval_expr(a, env)).collect();
                self.call_builtin(name, &arg_vals?)
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

        // Look up user-defined function
        let func = self.functions.get(name).ok_or_else(|| RuntimeError {
            message: format!("undefined function '{}'", name),
        })?;

        // Clone what we need to avoid borrow issues
        let params = func.params.clone();
        let body = func.body.clone();

        let mut fn_env = Env::new();

        // Bind constants
        for (cname, cval) in &self.constants {
            fn_env.set(cname.clone(), cval.clone());
        }

        // Bind parameters
        for (i, param) in params.iter().enumerate() {
            if let Some(val) = args.get(i) {
                fn_env.set(param.name.clone(), (*val).clone());
            } else {
                return Err(RuntimeError {
                    message: format!(
                        "function '{}' expected {} args, got {}",
                        name,
                        params.len(),
                        args.len()
                    ),
                });
            }
        }

        self.exec_body(&body, &mut fn_env)
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
                Some(Value::Void)
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
            _ => None,
        };
        Ok(result)
    }

    fn call_builtin(&self, name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        self.try_builtin(name, args)?.ok_or_else(|| RuntimeError {
            message: format!("unknown built-in '{}'", name),
        })
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
                s.parse::<i64>()
                    .map(Value::Int)
                    .map_err(|_| RuntimeError {
                        message: format!("cannot cast '{}' to integer", s),
                    })
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
