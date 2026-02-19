/// Integration tests for AILang
/// Runs each .ai example file via `cargo run -- test <file>` and asserts exit code 0.

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
