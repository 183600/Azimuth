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

// MoonBit 标准库核心测试用例
// 包含 8 个核心功能测试，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 4
  
  // 基本运算
  assert_eq(19, a + b)
  assert_eq(11, a - b)
  assert_eq(60, a * b)
  assert_eq(3, a / b)
  assert_eq(3, a % b)
}

test "string_concatenation" {
  // 测试字符串连接
  let text = "Hello"
  let world = "World"
  
  // 测试字符串连接
  let combined = text + " " + world
  assert_eq_string("Hello World", combined)
  
  // 测试空字符串
  let empty = ""
  assert_eq_string("", empty)
}

test "library_function_tests" {
  // 测试库函数
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(-1, subtract(2, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculations" {
  // 测试复杂计算
  let x = 10
  let y = 20
  let z = 30
  
  // 复合运算
  let result1 = add(multiply(x, y), z)
  assert_eq(230, result1)  // 10 * 20 + 30 = 230
  
  let result2 = multiply(add(x, y), z)
  assert_eq(900, result2)  // (10 + 20) * 30 = 900
}

test "edge_case_values" {
  // 测试边界值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, subtract(-2, -3))
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "unicode_string_operations" {
  // 测试 Unicode 字符串操作
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "business_logic_calculations" {
  // 测试业务逻辑计算
  let unit_price = 25
  let quantity = 8
  let discount = 20
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(add(subtotal, shipping), -discount)
  
  assert_eq(200, subtotal)  // 25 * 8 = 200
  assert_eq(195, total)     // 200 + 15 - 20 = 195
}