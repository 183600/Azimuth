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

// 新的 MoonBit 标准测试用例
// 包含 8 个核心功能测试，使用标准 MoonBit 测试语法

test "int_basic_operations" {
  // 测试整数基本操作
  let a = 10
  let b = 20
  
  azimuth::assert_eq(30, a + b)
  azimuth::assert_eq(200, a * b)
  azimuth::assert_eq(10, b - a)
  azimuth::assert_eq(2, b / a)
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  
  azimuth::assert_eq(5, hello.length())
  azimuth::assert_eq_string("Hello World", hello + " " + world)
  azimuth::assert_eq(true, hello == "Hello")
  azimuth::assert_eq(false, hello == "hello")
}

test "boolean_operations" {
  // 测试布尔操作
  let t = true
  let f = false
  
  azimuth::assert_eq(true, t && t)
  azimuth::assert_eq(false, t && f)
  azimuth::assert_eq(true, t || f)
  azimuth::assert_eq(false, f || f)
  azimuth::assert_eq(false, !t)
  azimuth::assert_eq(true, !f)
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  
  azimuth::assert_eq(5, numbers.length())
  azimuth::assert_eq(1, numbers[0])
  azimuth::assert_eq(5, numbers[4])
  
  let doubled = numbers.map(x => x * 2)
  azimuth::assert_eq(5, doubled.length())
  azimuth::assert_eq(2, doubled[0])
  azimuth::assert_eq(10, doubled[4])
}

test "option_basic_operations" {
  // 测试 Option 基本操作
  let some_val : Int? = Some(42)
  let none_val : Int? = None
  
  azimuth::assert_eq(true, some_val.is_some())
  azimuth::assert_eq(false, some_val.is_none())
  azimuth::assert_eq(false, none_val.is_some())
  azimuth::assert_eq(true, none_val.is_none())
  
  let doubled = some_val.map(x => x * 2)
  azimuth::assert_eq_string("Some(84)", doubled.to_string())
  
  let unwrapped = some_val.unwrap_or(0)
  let default_val = none_val.unwrap_or(0)
  
  azimuth::assert_eq(42, unwrapped)
  azimuth::assert_eq(0, default_val)
}

test "div_with_ceil_operations" {
  // 测试向上取整除法
  let result1 = azimuth::divide_with_ceil(10, 3)
  azimuth::assert_eq(4, result1)
  
  let result2 = azimuth::divide_with_ceil(9, 3)
  azimuth::assert_eq(3, result2)
  
  let result3 = azimuth::divide_with_ceil(-10, 3)
  azimuth::assert_eq(-3, result3)
  
  let result4 = azimuth::divide_with_ceil(0, 5)
  azimuth::assert_eq(0, result4)
}

test "greet_function_tests" {
  // 测试 greet 函数
  let result1 = azimuth::greet("MoonBit")
  azimuth::assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = azimuth::greet("")
  azimuth::assert_eq_string("Hello, !", result2)
  
  let result3 = azimuth::greet("世界")
  azimuth::assert_eq_string("Hello, 世界!", result3)
}

test "combined_operations" {
  // 测试组合操作
  let a = 5
  let b = 3
  
  // 先加后乘
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, 2)
  azimuth::assert_eq(16, product)
  
  // 字符串拼接和长度
  let greeting = azimuth::greet("Test")
  let length = greeting.length()
  azimuth::assert_eq(11, length)  // "Hello, Test!" 长度为 12
}