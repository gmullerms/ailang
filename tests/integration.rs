/// Integration tests for AILang
/// Runs each .ai example file via `cargo run -- test <file>` and asserts exit code 0.

use std::fs;
use std::process::Command;

/// Helper: run `cargo run -- test <file>` and assert success
fn run_ai_test(file: &str) {
    let output = Command::new("cargo")
        .args(["run", "--", "test", file])
        .output()
        .unwrap_or_else(|e| panic!("failed to execute cargo run for {}: {}", file, e));

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "{} tests failed (exit code {:?}):\n{}",
        file,
        output.status.code(),
        stderr
    );
}

/// Helper: run `cargo run -- test <file>` and assert failure (non-zero exit)
fn run_ai_test_expect_fail(file: &str) {
    let output = Command::new("cargo")
        .args(["run", "--", "test", file])
        .output()
        .unwrap_or_else(|e| panic!("failed to execute cargo run for {}: {}", file, e));

    assert!(
        !output.status.success(),
        "{} was expected to fail but succeeded",
        file,
    );
}

#[test]
fn test_hello_ai() {
    run_ai_test("examples/hello.ai");
}

#[test]
fn test_01_two_sum() {
    run_ai_test("examples/01_two_sum.ai");
}

#[test]
fn test_02_fizzbuzz() {
    run_ai_test("examples/02_fizzbuzz.ai");
}

#[test]
fn test_03_palindrome_number() {
    run_ai_test("examples/03_palindrome_number.ai");
}

#[test]
fn test_04_reverse_integer() {
    run_ai_test("examples/04_reverse_integer.ai");
}

#[test]
fn test_05_fibonacci() {
    run_ai_test("examples/05_fibonacci.ai");
}

#[test]
fn test_06_max_subarray() {
    run_ai_test("examples/06_max_subarray.ai");
}

#[test]
fn test_07_valid_parentheses() {
    run_ai_test("examples/07_valid_parentheses.ai");
}

#[test]
fn test_08_merge_sorted_lists() {
    run_ai_test("examples/08_merge_sorted_lists.ai");
}

#[test]
fn test_09_climbing_stairs() {
    run_ai_test("examples/09_climbing_stairs.ai");
}

#[test]
fn test_10_contains_duplicate() {
    run_ai_test("examples/10_contains_duplicate.ai");
}

#[test]
fn test_11_invert_binary_tree() {
    run_ai_test("examples/11_invert_binary_tree.ai");
}

#[test]
fn test_connect4() {
    run_ai_test("examples/connect4.ai");
}

// --- JSON tests ---

#[test]
fn test_18_json_demo() {
    run_ai_test("examples/18_json_demo.ai");
}

// --- HTTP tests ---

#[test]
fn test_19_http_demo() {
    run_ai_test("examples/19_http_demo.ai");
}

// --- Module system tests ---

#[test]
fn test_17_modules_demo() {
    run_ai_test("examples/17_modules_demo.ai");
}

#[test]
fn test_std_math() {
    run_ai_test("std/math.ai");
}

#[test]
fn test_std_list() {
    run_ai_test("std/list.ai");
}

#[test]
fn test_math_helpers() {
    run_ai_test("examples/math_helpers.ai");
}

#[test]
fn test_module_not_found() {
    // A file that imports a non-existent module should fail
    run_ai_test_expect_fail("examples/test_module_not_found.ai");
}

#[test]
fn test_private_import_rejected() {
    // A file that tries to import a private function should fail
    run_ai_test_expect_fail("examples/test_private_import.ai");
}

// --- Formatter tests ---

/// Helper: run `cargo run -- fmt <file>` and assert success
fn run_ai_fmt(file: &str) {
    let output = Command::new("cargo")
        .args(["run", "--", "fmt", file])
        .output()
        .unwrap_or_else(|e| panic!("failed to execute cargo run for {}: {}", file, e));

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "fmt {} failed (exit code {:?}):\n{}",
        file,
        output.status.code(),
        stderr
    );
}

#[test]
fn test_fmt_idempotent() {
    // Copy a test file to a temp location, format it, check it's idempotent
    let src = "examples/05_fibonacci.ai";
    let tmp = "target/test_fmt_idempotent.ai";
    fs::copy(src, tmp).expect("failed to copy test file");

    // First format
    run_ai_fmt(tmp);
    let first = fs::read_to_string(tmp).expect("failed to read formatted file");

    // Second format (should produce identical output)
    run_ai_fmt(tmp);
    let second = fs::read_to_string(tmp).expect("failed to read re-formatted file");

    assert_eq!(first, second, "Formatter should be idempotent");

    // Clean up
    let _ = fs::remove_file(tmp);
}

#[test]
fn test_fmt_block_ordering() {
    // Write a file with blocks in wrong order, verify formatter reorders them
    let src = "#entry\n  = 0\n\n#fn add :i32 a:i32 b:i32\n  = + a b\n\n#const N :i32 = 10\n";
    let tmp = "target/test_fmt_ordering.ai";
    fs::write(tmp, src).expect("failed to write test file");

    run_ai_fmt(tmp);
    let result = fs::read_to_string(tmp).expect("failed to read formatted file");

    // #const should come before #fn, #fn before #entry
    let const_pos = result.find("#const").expect("missing #const");
    let fn_pos = result.find("#fn").expect("missing #fn");
    let entry_pos = result.find("#entry").expect("missing #entry");
    assert!(
        const_pos < fn_pos,
        "Expected #const before #fn"
    );
    assert!(
        fn_pos < entry_pos,
        "Expected #fn before #entry"
    );

    // Clean up
    let _ = fs::remove_file(tmp);
}

#[test]
fn test_fmt_ssa_renaming() {
    // The formatter should produce consistent v0, v1, ... names
    let src = "#fn f :i32 x:i32\n  v0 :i32 = + x 1\n  v1 :i32 = * v0 2\n  = v1\n";
    let tmp = "target/test_fmt_ssa.ai";
    fs::write(tmp, src).expect("failed to write test file");

    run_ai_fmt(tmp);
    let result = fs::read_to_string(tmp).expect("failed to read formatted file");

    assert!(result.contains("v0 :i32 = + x 1"), "Should contain v0 bind");
    assert!(result.contains("v1 :i32 = * v0 2"), "Should contain v1 bind");
    assert!(result.contains("= v1"), "Should return v1");

    // Clean up
    let _ = fs::remove_file(tmp);
}

// --- :any type warning tests ---

#[test]
fn test_any_warn_demo() {
    // The demo uses :any types but tests should still pass (warnings are informational)
    run_ai_test("examples/any_warn_demo.ai");
}

#[test]
fn test_any_warn_output() {
    // Verify that :any type warnings appear on stderr
    let output = Command::new("cargo")
        .args(["run", "--", "test", "examples/any_warn_demo.ai"])
        .output()
        .unwrap_or_else(|e| panic!("failed to execute cargo run: {}", e));

    assert!(output.status.success(), "any_warn_demo should still pass");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Check that warnings are emitted for the various :any usages
    assert!(
        stderr.contains("warning: use of :any type in function 'process_data' return type"),
        "should warn about process_data return type"
    );
    assert!(
        stderr.contains("warning: use of :any type in parameter 'x' of function 'process_data'"),
        "should warn about parameter x"
    );
    assert!(
        stderr.contains("warning: use of :any type in bind 'v0' in function 'process_data'"),
        "should warn about bind v0"
    );
    assert!(
        stderr.contains("warning: use of :any type in const 'ANYTHING'"),
        "should warn about const ANYTHING"
    );
    // list_any uses :[any] — nested any in a list type
    assert!(
        stderr.contains("warning: use of :any type in function 'list_any' return type"),
        "should warn about list_any return type with nested any"
    );
    assert!(
        stderr.contains("warning: use of :any type in parameter 'items' of function 'list_any'"),
        "should warn about list_any parameter with nested any"
    );
}

// --- FFI tests ---

#[test]
fn test_20_ffi_demo() {
    // Build the FFI test library first
    let build_output = Command::new("cargo")
        .args(["build", "--manifest-path", "tests/ffi_test_lib/Cargo.toml"])
        .output()
        .expect("failed to build ffi_test_lib");
    assert!(
        build_output.status.success(),
        "ffi_test_lib build failed:\n{}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    // Now run the FFI demo tests
    run_ai_test("examples/20_ffi_demo.ai");
}

#[test]
fn test_ffi_sandbox_blocked() {
    // FFI should be blocked in sandbox mode
    let output = Command::new("cargo")
        .args(["run", "--", "--sandbox", "examples/20_ffi_demo.ai"])
        .output()
        .expect("failed to execute cargo run");

    assert!(
        !output.status.success(),
        "FFI should fail in sandbox mode"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("sandbox: FFI library loading not permitted"),
        "should mention sandbox FFI restriction, got:\n{}",
        stderr
    );
}
