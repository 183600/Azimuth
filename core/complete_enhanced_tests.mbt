// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Custom assertion functions for testing
pub fn assert_eq_int(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Int assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    let _ = expected + actual
    @builtin.abort("String assertion failed")
  }
}

pub fn assert_eq_bool(expected : Bool, actual : Bool) -> Unit {
  if (expected != actual) {
    let _ = if (expected) { 1 } else { 0 }
    @builtin.abort("Bool assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false")
  }
}

///|
test "int basic arithmetic operations" {
  // Test basic arithmetic operations
  assert_eq_int(2 + 3, 5)
  assert_eq_int(10 - 4, 6)
  assert_eq_int(3 * 4, 12)
  assert_eq_int(15 / 3, 5)
  assert_eq_int(17 % 5, 2)
}

///|
test "int bit operations" {
  // Test bit manipulation
  assert_eq_int(8 & 7, 0)
  assert_eq_int(8 | 7, 15)
  assert_eq_int(8 ^ 7, 15)
  assert_eq_int(-9, -9)
  assert_eq_int(1 << 3, 8)
  assert_eq_int(8 >> 3, 1)
}

///|
test "int comparison operations" {
  // Test comparison operations
  assert_eq_bool(5 > 3, true)
  assert_eq_bool(5 < 3, false)
  assert_eq_bool(5 >= 5, true)
  assert_eq_bool(5 <= 3, false)
  assert_eq_bool(5 == 5, true)
  assert_eq_bool(5 != 3, true)
}

///|
test "string basic operations" {
  // Test string concatenation
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  assert_eq_string(combined, "Hello World")
}

///|
test "array basic operations" {
  // Test array creation and access
  let numbers = [1, 2, 3, 4, 5]
  assert_eq_int(numbers.length(), 5)
  assert_eq_int(numbers[0], 1)
  assert_eq_int(numbers[4], 5)
}

///|
test "boolean logic operations" {
  // Test boolean operations
  let t = true
  let f = false
  assert_eq_bool(t && t, true)
  assert_eq_bool(t && f, false)
  assert_eq_bool(t || f, true)
  assert_eq_bool(f || f, false)
  assert_eq_bool(!t, false)
  assert_eq_bool(!f, true)
}

///|
test "tuple operations" {
  // Test tuple creation and access
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  assert_eq_int(pair.0, 42)
  assert_eq_string(pair.1, "answer")
  assert_eq_int(triple.0, 1)
  assert_eq_int(triple.1, 2)
  assert_eq_int(triple.2, 3)
}

///|
test "int boundary values" {
  // Test integer boundaries
  assert_eq_int(2147483647, 2147483647)
  assert_eq_int(-2147483648, -2147483648)
  assert_eq_int(2147483647 + 1, -2147483648)
  assert_eq_int(-2147483648 - 1, 2147483647)
}

///|
test "hexadecimal literals" {
  // Test hex number representation
  assert_eq_int(0xFF, 255)
  assert_eq_int(0x00, 0)
  assert_eq_int(0x10, 16)
  assert_eq_int(0xFFFF, 65535)
}

///|
test "complex expressions" {
  // Test complex arithmetic expressions
  assert_eq_int(2 + 3 * 4, 14)
  assert_eq_int((2 + 3) * 4, 20)
  assert_eq_int(10 - 2 * 3, 4)
  assert_eq_int(20 / (4 + 1), 4)
}