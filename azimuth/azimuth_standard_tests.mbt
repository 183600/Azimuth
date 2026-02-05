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

// 标准 MoonBit 测试用例
// 包含 10 个核心功能测试，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_basic_cases" {
  // 测试问候函数基本用例
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base_value = 10
  let multiplied = multiply(base_value, 5)
  let added = add(multiplied, 15)
  let divided = divide_with_ceil(added, 3)
  let final_result = subtract(divided, 5)
  
  assert_eq(50, multiplied)
  assert_eq(65, added)
  assert_eq(22, divided)
  assert_eq(17, final_result)
}

test "edge_cases_handling" {
  // 测试边界情况处理
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 999))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, subtract(-2, -3))
}

test "large_numbers_handling" {
  // 测试大数处理
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000, multiply(large_num1, 2))
  assert_eq(-1000000, subtract(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
}

test "boolean_logic_operations" {
  // 测试布尔逻辑操作
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_true(5 >= 5)
  assert_true(5 <= 5)
  assert_false(5 == 3)
  assert_true(5 != 3)
}

test "string_operations_and_comparisons" {
  // 测试字符串操作和比较
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  
  assert_eq_string("Hello World", combined)
  assert_true(str1 == "Hello")
  assert_false(str1 == str2)
  assert_true(str1 < str2)
  assert_false(str1 > str2)
}

test "calculation_with_negative_results" {
  // 测试产生负数的计算
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(-8, subtract(2, 10))
  assert_eq(-2, divide_with_ceil(-5, 3))
}