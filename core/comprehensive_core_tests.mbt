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

// 综合核心测试用例 - 使用项目中的自定义函数

test "add_function_various_cases" {
  // 测试加法函数的各种情况
  assert_eq(7, add(3, 4))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(1000, add(999, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(8, subtract(0, -8))
  assert_eq(0, subtract(-3, -3))
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算
  let a = 12
  let b = 8
  assert_eq(20, add(a, b))
  assert_eq(96, multiply(a, b))
  assert_eq(4, subtract(a, b))
  assert_eq(2, divide_with_ceil(a, b))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(2, subtract(-3, -5))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "zero_operations" {
  // 测试零的运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(0, subtract(1000, 1000))
  assert_eq(1, divide_with_ceil(1000, 999))
}

test "mixed_operations" {
  // 测试混合运算
  assert_eq(15, add(multiply(3, 4), 3))
  assert_eq(5, subtract(multiply(2, 3), 1))
  assert_eq(2, divide_with_ceil(add(3, 2), 3))
  assert_eq(12, multiply(subtract(10, 2), add(1, 1)))
}