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

// 标准 MoonBit 测试用例集
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "integer_arithmetic_operations" {
  // 测试基本整数运算
  assert_eq(10 + 5, 15)
  assert_eq(20 - 8, 12)
  assert_eq(6 * 7, 42)
  assert_eq(25 / 5, 5)
  assert_eq(27 % 4, 3)
  
  // 测试负数运算
  assert_eq(-10 + 15, 5)
  assert_eq(-5 * -3, 15)
}

test "string_manipulation" {
  // 测试字符串基本操作
  let greeting = "Hello"
  let target = "World"
  let message = greeting + ", " + target + "!"
  
  assert_eq(message, "Hello, World!")
  assert_eq(message.length(), 13)
  assert_eq(greeting.length(), 5)
  
  // 测试字符串比较
  assert_eq(greeting == "Hello", true)
  assert_eq(greeting != target, true)
}

test "array_functionality" {
  // 测试数组创建和访问
  let numbers = [10, 20, 30, 40, 50]
  let mixed = ["apple", 42, true]
  
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 10)
  assert_eq(numbers[4], 50)
  assert_eq(mixed.length(), 3)
  assert_eq(mixed[0], "apple")
  
  // 测试数组求和
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  assert_eq(sum, 150)
}

test "boolean_logic_expressions" {
  // 测试布尔逻辑运算
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
  
  // 测试布尔比较
  assert_eq(true == true, true)
  assert_eq(true != false, true)
}

test "option_type_patterns" {
  // 测试 Option 类型处理
  let some_number : Int? = Some(100)
  let no_number : Int? = None
  let some_text : String? = Some("test")
  
  // 使用模式匹配处理 Option
  let extracted = match some_number {
    Some(value) => value
    None => 0
  }
  assert_eq(extracted, 100)
  
  let default_value = match no_number {
    Some(value) => value
    None => -1
  }
  assert_eq(default_value, -1)
  
  // 测试 Option 比较
  assert_eq(some_number == Some(100), true)
  assert_eq(no_number == None, true)
}

test "result_type_handling" {
  // 测试 Result 类型处理
  let success_result : Result[String, Int] = Ok("success")
  let error_result : Result[String, Int] = Err(404)
  
  // 使用模式匹配处理 Result
  let success_message = match success_result {
    Ok(message) => message
    Err(code) => "error: " + code.to_string()
  }
  assert_eq(success_message, "success")
  
  let error_message = match error_result {
    Ok(message) => message
    Err(code) => "error: " + code.to_string()
  }
  assert_eq(error_message, "error: 404")
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_Z = 'Z'
  let digit_5 = '5'
  
  // 测试字符转换
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_Z.to_string(), "Z")
  assert_eq(digit_5.to_string(), "5")
  
  // 测试字符比较
  assert_eq('a' > 'Z', true)
  assert_eq('0' < '9', true)
  assert_eq('A' < 'a', true)
}

test "mathematical_functions" {
  // 测试数学运算
  let x = 3.0
  let y = 4.0
  
  // 基本浮点运算
  assert_eq(x + y, 7.0)
  assert_eq(y - x, 1.0)
  assert_eq(x * y, 12.0)
  assert_eq(y / x, 1.3333333333333333)
  
  // 测试特殊值
  let positive = 5.5
  let negative = -5.5
  let zero = 0.0
  
  assert_eq(positive > zero, true)
  assert_eq(negative < zero, true)
  assert_eq(positive + negative, 0.0)
}