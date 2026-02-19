use std::ffi::CStr;
use std::os::raw::c_char;

#[unsafe(no_mangle)]
pub extern "C" fn test_add(a: i32, b: i32) -> i32 {
    a + b
}

#[unsafe(no_mangle)]
pub extern "C" fn test_negate(x: i32) -> i32 {
    -x
}

#[unsafe(no_mangle)]
pub extern "C" fn test_strlen(s: *const c_char) -> i32 {
    if s.is_null() {
        return 0;
    }
    unsafe { CStr::from_ptr(s) }.to_bytes().len() as i32
}

#[unsafe(no_mangle)]
pub extern "C" fn test_double_float(x: f64) -> f64 {
    x * 2.0
}

#[unsafe(no_mangle)]
pub extern "C" fn test_no_return(x: i32) {
    let _ = x; // side effect only
}

#[unsafe(no_mangle)]
pub extern "C" fn test_identity_i64(x: i64) -> i64 {
    x
}

#[unsafe(no_mangle)]
pub extern "C" fn test_add_floats(a: f64, b: f64) -> f64 {
    a + b
}
