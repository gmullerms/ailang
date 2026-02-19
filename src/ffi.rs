/// FFI module for AILang
/// Handles loading shared libraries and calling native C functions.

use crate::ast::AiType;
use crate::interpreter::Value;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Typed function pointer aliases
// ---------------------------------------------------------------------------

// All-integer/pointer args, integer/pointer return
type FfiFn0I = unsafe extern "C" fn() -> i64;
type FfiFn1I = unsafe extern "C" fn(i64) -> i64;
type FfiFn2I = unsafe extern "C" fn(i64, i64) -> i64;
type FfiFn3I = unsafe extern "C" fn(i64, i64, i64) -> i64;
type FfiFn4I = unsafe extern "C" fn(i64, i64, i64, i64) -> i64;
type FfiFn5I = unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64;
type FfiFn6I = unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64;

// All-integer/pointer args, f64 return
type FfiFn0F = unsafe extern "C" fn() -> f64;
type FfiFn1F = unsafe extern "C" fn(i64) -> f64;
type FfiFn2F = unsafe extern "C" fn(i64, i64) -> f64;

// All-f64 args, f64 return (for math-like functions)
type FfiFn1FF = unsafe extern "C" fn(f64) -> f64;
type FfiFn2FF = unsafe extern "C" fn(f64, f64) -> f64;

// Void return variants
type FfiFn0V = unsafe extern "C" fn();
type FfiFn1V = unsafe extern "C" fn(i64);
type FfiFn2V = unsafe extern "C" fn(i64, i64);
type FfiFn3V = unsafe extern "C" fn(i64, i64, i64);

// ---------------------------------------------------------------------------
// FfiManager
// ---------------------------------------------------------------------------

pub struct FfiManager {
    libraries: HashMap<String, libloading::Library>,
}

impl FfiManager {
    pub fn new() -> Self {
        FfiManager {
            libraries: HashMap::new(),
        }
    }

    pub fn load_library(&mut self, name: &str, search_dir: Option<&Path>) -> Result<(), String> {
        if self.libraries.contains_key(name) {
            return Ok(()); // Already loaded
        }

        let resolved = resolve_library_path(name, search_dir)?;

        let lib = unsafe {
            libloading::Library::new(&resolved)
                .map_err(|e| format!("cannot load library '{}' (resolved to '{}'): {}", name, resolved.display(), e))?
        };

        self.libraries.insert(name.to_string(), lib);
        Ok(())
    }

    pub fn call_function(
        &self,
        lib_name: &str,
        fn_name: &str,
        args: &[Value],
        param_types: &[AiType],
        return_type: &AiType,
    ) -> Result<Value, String> {
        let lib = self.libraries.get(lib_name)
            .ok_or_else(|| format!("FFI library '{}' not loaded", lib_name))?;

        if args.len() != param_types.len() {
            return Err(format!(
                "FFI function '{}' expected {} args, got {}",
                fn_name, param_types.len(), args.len()
            ));
        }

        if args.len() > 6 {
            return Err(format!(
                "FFI function '{}': too many arguments (max 6, got {})",
                fn_name, args.len()
            ));
        }

        // Classify args as int-like or float
        let all_int = param_types.iter().all(|t| is_int_like(t));
        let all_float = param_types.iter().all(|t| matches!(t, AiType::F64 | AiType::F32));

        if !all_int && !all_float && !param_types.is_empty() {
            return Err(format!(
                "FFI function '{}': mixed int/float args not supported yet",
                fn_name
            ));
        }

        // Create CStrings for text args and keep them alive during the call
        let mut cstrings: Vec<CString> = Vec::new();
        let mut int_args: Vec<i64> = Vec::new();
        let mut float_args: Vec<f64> = Vec::new();

        if all_float && !param_types.is_empty() {
            for (i, arg) in args.iter().enumerate() {
                float_args.push(value_to_f64(arg, i, fn_name)?);
            }
        } else {
            // All int-like (or zero args)
            for (i, (arg, ty)) in args.iter().zip(param_types.iter()).enumerate() {
                match ty {
                    AiType::Text => {
                        let s = match arg {
                            Value::Text(s) => s.clone(),
                            Value::Null => String::new(),
                            _ => format!("{}", arg),
                        };
                        let cs = CString::new(s)
                            .map_err(|_| format!("FFI function '{}': arg {} contains null byte", fn_name, i))?;
                        int_args.push(cs.as_ptr() as i64);
                        cstrings.push(cs);
                    }
                    _ => {
                        int_args.push(value_to_i64(arg, i, fn_name)?);
                    }
                }
            }
        }

        // Get the symbol
        let sym_name = fn_name.as_bytes();

        // Determine return type category
        let ret_is_float = matches!(return_type, AiType::F64 | AiType::F32);
        let ret_is_void = matches!(return_type, AiType::Void);

        // Dispatch based on arity and type classification
        if all_float && !param_types.is_empty() {
            // All-float args, float return
            let raw_result = match float_args.len() {
                1 => {
                    let f: libloading::Symbol<FfiFn1FF> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(float_args[0]) }
                }
                2 => {
                    let f: libloading::Symbol<FfiFn2FF> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(float_args[0], float_args[1]) }
                }
                _ => {
                    return Err(format!(
                        "FFI function '{}': all-float dispatch only supports 1-2 args, got {}",
                        fn_name, float_args.len()
                    ));
                }
            };
            return convert_f64_return(raw_result, return_type);
        }

        // All-int-like args (or zero args)
        if ret_is_void {
            // Void return
            match int_args.len() {
                0 => {
                    let f: libloading::Symbol<FfiFn0V> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f() };
                }
                1 => {
                    let f: libloading::Symbol<FfiFn1V> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(int_args[0]) };
                }
                2 => {
                    let f: libloading::Symbol<FfiFn2V> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(int_args[0], int_args[1]) };
                }
                3 => {
                    let f: libloading::Symbol<FfiFn3V> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(int_args[0], int_args[1], int_args[2]) };
                }
                n => {
                    return Err(format!(
                        "FFI function '{}': void return only supports 0-3 args, got {}",
                        fn_name, n
                    ));
                }
            }
            // CStrings are still alive here
            drop(cstrings);
            return Ok(Value::Void);
        }

        if ret_is_float {
            // Int-like args, float return
            let raw_result: f64 = match int_args.len() {
                0 => {
                    let f: libloading::Symbol<FfiFn0F> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f() }
                }
                1 => {
                    let f: libloading::Symbol<FfiFn1F> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(int_args[0]) }
                }
                2 => {
                    let f: libloading::Symbol<FfiFn2F> = unsafe {
                        lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                    };
                    unsafe { f(int_args[0], int_args[1]) }
                }
                n => {
                    return Err(format!(
                        "FFI function '{}': float return with int args only supports 0-2 args, got {}",
                        fn_name, n
                    ));
                }
            };
            drop(cstrings);
            return convert_f64_return(raw_result, return_type);
        }

        // Int-like args, int-like return
        let raw_result: i64 = match int_args.len() {
            0 => {
                let f: libloading::Symbol<FfiFn0I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f() }
            }
            1 => {
                let f: libloading::Symbol<FfiFn1I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0]) }
            }
            2 => {
                let f: libloading::Symbol<FfiFn2I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0], int_args[1]) }
            }
            3 => {
                let f: libloading::Symbol<FfiFn3I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0], int_args[1], int_args[2]) }
            }
            4 => {
                let f: libloading::Symbol<FfiFn4I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0], int_args[1], int_args[2], int_args[3]) }
            }
            5 => {
                let f: libloading::Symbol<FfiFn5I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0], int_args[1], int_args[2], int_args[3], int_args[4]) }
            }
            6 => {
                let f: libloading::Symbol<FfiFn6I> = unsafe {
                    lib.get(sym_name).map_err(|e| format!("FFI symbol '{}' not found: {}", fn_name, e))?
                };
                unsafe { f(int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5]) }
            }
            _ => unreachable!(), // Already checked above
        };

        // CStrings are still alive here -- drop them after getting the result
        drop(cstrings);

        convert_i64_return(raw_result, return_type)
    }
}

// ---------------------------------------------------------------------------
// Value conversion helpers
// ---------------------------------------------------------------------------

fn is_int_like(ty: &AiType) -> bool {
    matches!(ty, AiType::I32 | AiType::I64 | AiType::Bool | AiType::Text | AiType::Any | AiType::Byte)
}

fn value_to_i64(val: &Value, arg_idx: usize, fn_name: &str) -> Result<i64, String> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
        Value::Null => Ok(0),
        Value::Float(f) => Ok(*f as i64),
        _ => Err(format!(
            "FFI function '{}': cannot convert arg {} ({}) to integer",
            fn_name, arg_idx, val.type_name()
        )),
    }
}

fn value_to_f64(val: &Value, arg_idx: usize, fn_name: &str) -> Result<f64, String> {
    match val {
        Value::Float(f) => Ok(*f),
        Value::Int(n) => Ok(*n as f64),
        _ => Err(format!(
            "FFI function '{}': cannot convert arg {} ({}) to float",
            fn_name, arg_idx, val.type_name()
        )),
    }
}

fn convert_i64_return(raw: i64, return_type: &AiType) -> Result<Value, String> {
    match return_type {
        AiType::I32 => Ok(Value::Int(raw as i32 as i64)), // sign-extend from 32-bit
        AiType::I64 => Ok(Value::Int(raw)),
        AiType::Bool => Ok(Value::Bool(raw != 0)),
        AiType::Text => {
            if raw == 0 {
                Ok(Value::Text(String::new()))
            } else {
                let cstr = unsafe { CStr::from_ptr(raw as *const std::ffi::c_char) };
                let s = cstr.to_string_lossy().into_owned();
                Ok(Value::Text(s))
            }
        }
        AiType::Any => Ok(Value::Int(raw)),
        AiType::Byte => Ok(Value::Int(raw & 0xFF)),
        _ => Err(format!("FFI: unsupported return type {:?} for integer return", return_type)),
    }
}

fn convert_f64_return(raw: f64, return_type: &AiType) -> Result<Value, String> {
    match return_type {
        AiType::F64 | AiType::F32 => Ok(Value::Float(raw)),
        AiType::I32 => Ok(Value::Int(raw as i32 as i64)),
        AiType::I64 => Ok(Value::Int(raw as i64)),
        _ => Err(format!("FFI: unsupported return type {:?} for float return", return_type)),
    }
}

// ---------------------------------------------------------------------------
// Library path resolution
// ---------------------------------------------------------------------------

fn resolve_library_path(name: &str, search_dir: Option<&Path>) -> Result<PathBuf, String> {
    let path = Path::new(name);

    // If name has an extension or is absolute, try it directly
    if path.is_absolute() || path.extension().is_some() {
        if path.exists() {
            return Ok(path.to_path_buf());
        }
        if let Some(dir) = search_dir {
            let full = dir.join(path);
            if full.exists() {
                return Ok(full);
            }
        }
    }

    // Platform-specific extensions
    let candidates = if cfg!(target_os = "windows") {
        vec![format!("{}.dll", name), format!("lib{}.dll", name)]
    } else if cfg!(target_os = "macos") {
        vec![format!("lib{}.dylib", name), format!("{}.dylib", name)]
    } else {
        vec![format!("lib{}.so", name), format!("{}.so", name)]
    };

    // Search in current directory
    for candidate in &candidates {
        let p = Path::new(candidate);
        if p.exists() {
            return Ok(p.to_path_buf());
        }
    }

    // Search relative to the source file's directory
    if let Some(dir) = search_dir {
        for candidate in &candidates {
            let full = dir.join(candidate);
            if full.exists() {
                return Ok(full);
            }
        }
    }

    // Fall back to system library search (let libloading/OS handle it)
    // Return the platform-specific name and let the OS search PATH/LD_LIBRARY_PATH
    Ok(PathBuf::from(&candidates[0]))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_int_like() {
        assert!(is_int_like(&AiType::I32));
        assert!(is_int_like(&AiType::I64));
        assert!(is_int_like(&AiType::Bool));
        assert!(is_int_like(&AiType::Text));
        assert!(is_int_like(&AiType::Any));
        assert!(!is_int_like(&AiType::F64));
        assert!(!is_int_like(&AiType::F32));
    }

    #[test]
    fn test_value_to_i64() {
        assert_eq!(value_to_i64(&Value::Int(42), 0, "test").unwrap(), 42);
        assert_eq!(value_to_i64(&Value::Bool(true), 0, "test").unwrap(), 1);
        assert_eq!(value_to_i64(&Value::Bool(false), 0, "test").unwrap(), 0);
        assert_eq!(value_to_i64(&Value::Null, 0, "test").unwrap(), 0);
        assert_eq!(value_to_i64(&Value::Float(3.14), 0, "test").unwrap(), 3);
    }

    #[test]
    fn test_value_to_f64() {
        assert!((value_to_f64(&Value::Float(3.14), 0, "test").unwrap() - 3.14).abs() < f64::EPSILON);
        assert!((value_to_f64(&Value::Int(42), 0, "test").unwrap() - 42.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_convert_i64_return_i32() {
        // Sign extension from 32-bit: -1i32 as i64 should work
        let val = convert_i64_return(-1i64, &AiType::I32).unwrap();
        assert!(matches!(val, Value::Int(-1)));

        // Large positive i64 truncated to i32 range
        let val = convert_i64_return(0xFFFFFFFF, &AiType::I32).unwrap();
        assert!(matches!(val, Value::Int(-1))); // 0xFFFFFFFF as i32 = -1
    }

    #[test]
    fn test_convert_i64_return_i64() {
        let val = convert_i64_return(12345678901234, &AiType::I64).unwrap();
        assert!(matches!(val, Value::Int(12345678901234)));
    }

    #[test]
    fn test_convert_i64_return_bool() {
        let val = convert_i64_return(0, &AiType::Bool).unwrap();
        assert!(matches!(val, Value::Bool(false)));
        let val = convert_i64_return(1, &AiType::Bool).unwrap();
        assert!(matches!(val, Value::Bool(true)));
    }

    #[test]
    fn test_convert_f64_return() {
        let val = convert_f64_return(3.14, &AiType::F64).unwrap();
        if let Value::Float(f) = val {
            assert!((f - 3.14).abs() < f64::EPSILON);
        } else {
            panic!("expected Float");
        }
    }

    #[test]
    fn test_ffi_manager_new() {
        let mgr = FfiManager::new();
        assert!(mgr.libraries.is_empty());
    }

    #[test]
    fn test_resolve_library_path_with_extension() {
        // A path with extension should be tried as-is (even if it doesn't exist)
        let result = resolve_library_path("mylib.dll", None);
        assert!(result.is_ok()); // Falls through to system search
    }
}
