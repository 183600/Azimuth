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

// 标准 MoonBit 测试套件 - 核心功能测试

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  assert_eq(10, 2 + 8)
  assert_eq(6, 15 - 9)
  assert_eq(24, 6 * 4)
  assert_eq(5, 20 / 4)
  assert_eq(1, 7 % 3)
  assert_eq(-8, -4 * 2)
  assert_eq(0, 5 - 5)
}

test "string_manipulation_functions" {
  // 测试字符串操作函数
  let hello = "Hello"
  let world = "World"
  let greeting = hello + ", " + world + "!"
  
  assert_eq_string("Hello, World!", greeting)
  assert_eq_string(12, greeting.length())
  assert_eq_string(true, greeting.contains("World"))
  assert_eq_string(false, greeting.contains("Bye"))
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  assert_eq(true, true && true)
  assert_eq(false, true && false)
  assert_eq(true, true || false)
  assert_eq(false, false || false)
  assert_eq(false, !true)
  assert_eq(true, !false)
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(5, numbers.length())
  assert_eq(3, numbers[2])
  
  let empty_array : Array[Int] = []
  assert_eq(0, empty_array.length())
  
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(10, doubled[4])
}

test "option_type_handling" {
  // 测试选项类型处理
  let some_value = Some(42)
  let no_value = None[Int]
  
  assert_eq(true, some_value.is_some())
  assert_eq(false, some_value.is_none())
  assert_eq(false, no_value.is_some())
  assert_eq(true, no_value.is_none())
  
  let result = some_value.unwrap_or(0)
  assert_eq(42, result)
  
  let default_result = no_value.unwrap_or(100)
  assert_eq(100, default_result)
}

test "result_type_error_handling" {
  // 测试结果类型错误处理
  let success = Ok(200)
  let error = Err("Not found")
  
  assert_eq(true, success.is_ok())
  assert_eq(false, success.is_err())
  assert_eq(false, error.is_ok())
  assert_eq(true, error.is_err())
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_Z = 'Z'
  
  assert_eq(true, char_a.is_lowercase())
  assert_eq(false, char_a.is_uppercase())
  assert_eq(false, char_Z.is_lowercase())
  assert_eq(true, char_Z.is_uppercase())
  
  assert_eq(97, char_a.to_int())
  assert_eq(90, char_Z.to_int())
}

test "list_functional_operations" {
  // 测试列表函数式操作
  let numbers = [1, 2, 3, 4, 5]
  let list = List::from_array(numbers)
  
  let doubled = list.map(fn(x) { x * 2 })
  let evens = list.filter(fn(x) { x % 2 == 0 })
  let sum = list.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(10, doubled.length())
  assert_eq(2, evens.length())
  assert_eq(15, sum)
}

test "mathematical_functions" {
  // 测试数学函数
  assert_eq(true, 2.0.sqrt() > 1.4 && 2.0.sqrt() < 1.5)
  assert_eq(true, 3.14159.sin() > 0.0 && 3.14159.sin() < 0.1)
  assert_eq(true, (-1.0).abs() == 1.0)
  assert_eq(true, 3.5.floor() == 3.0)
  assert_eq(true, 3.5.ceil() == 4.0)
}

test "combined_operations_test" {
  // 综合操作测试
  let data = [10, 20, 30, 40, 50]
  let processed = data
    .map(fn(x) { x / 10 })
    .filter(fn(x) { x > 1 })
    .reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(9, processed)  // 2 + 3 + 4 + 5 = 14
  
  let status = if (processed > 10) {
    "High"
  } else if (processed > 5) {
    "Medium"
  } else {
    "Low"
  }
  
  assert_eq_string("Medium", status)
}