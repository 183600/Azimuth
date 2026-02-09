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

// 定义断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Assertion failed")
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
    let _ = expected.to_string() + actual.to_string()
    @builtin.abort("Bool assertion failed")
  }
}

pub fn assert_eq_float(expected : Double, actual : Double) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Float assertion failed")
  }
}

// 测试 Int 类型的基本操作
///|
test "Int arithmetic operations" {
  let a = 10
  let b = 3
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
}

// 测试 String 类型的基本操作
///|
test "String operations" {
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  assert_eq_string(combined, "Hello World")
  assert_eq_string(str1, "Hello")
  assert_eq_string(str2, "World")
}

// 测试 Bool 类型的基本操作
///|
test "Bool logical operations" {
  let t = true
  let f = false
  
  assert_eq_bool(t && f, false)
  assert_eq_bool(t || f, true)
  assert_eq_bool(!t, false)
  assert_eq_bool(!f, true)
  
  assert_eq_string(t.to_string(), "true")
  assert_eq_string(f.to_string(), "false")
}

// 测试比较操作
///|
test "Comparison operations" {
  assert_eq_bool(1 < 2, true)
  assert_eq_bool(2 > 1, true)
  assert_eq_bool(1 <= 1, true)
  assert_eq_bool(1 >= 1, true)
  assert_eq_bool(1 == 1, true)
  assert_eq_bool(1 != 2, true)
}

// 测试类型转换
///|
test "Type conversions" {
  let int_val = 42
  let string_val = int_val.to_string()
  assert_eq_string(string_val, "42")
  
  let float_val = 3.14
  let float_string = float_val.to_string()
  assert_eq_string(float_string, "3.14")
}