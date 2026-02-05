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

// 高质量 MoonBit 测试用例集
// 包含 8 个精心设计的测试用例，使用标准 MoonBit 测试语法

using @builtin { inspect }

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  // 测试负数运算
  inspect(-10 + 15, content="5")
  inspect(-5 * -3, content="15")
}

test "string_operations" {
  // 测试字符串基本操作
  let greeting = "Hello"
  let target = "World"
  let message = greeting + ", " + target + "!"
  
  inspect(message, content="\"Hello, World!\"")
  inspect(message.length(), content="13")
  inspect(greeting.length(), content="5")
  
  // 测试字符串比较
  inspect(greeting == "Hello", content="true")
  inspect(greeting != target, content="true")
}

test "array_functionality" {
  // 测试数组创建和访问
  let numbers = [10, 20, 30, 40, 50]
  let mixed = ["apple", 42, true]
  
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  inspect(mixed.length(), content="3")
  inspect(mixed[0], content="\"apple\"")
  
  // 测试数组求和
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="150")
}

test "boolean_logic_expressions" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
  
  // 测试布尔比较
  inspect(true == true, content="true")
  inspect(true != false, content="true")
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
  inspect(extracted, content="100")
  
  let default_value = match no_number {
    Some(value) => value
    None => -1
  }
  inspect(default_value, content="-1")
  
  // 测试 Option 比较
  inspect(some_number == Some(100), content="true")
  inspect(no_number == None, content="true")
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
  inspect(success_message, content="\"success\"")
  
  let error_message = match error_result {
    Ok(message) => message
    Err(code) => "error: " + code.to_string()
  }
  inspect(error_message, content="\"error: 404\"")
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_Z = 'Z'
  let digit_5 = '5'
  
  // 测试字符转换
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_Z.to_string(), content="\"Z\"")
  inspect(digit_5.to_string(), content="\"5\"")
  
  // 测试字符比较
  inspect('a' > 'Z', content="true")
  inspect('0' < '9', content="true")
  inspect('A' < 'a', content="true")
}

test "mathematical_functions" {
  // 测试数学运算
  let x = 3.0
  let y = 4.0
  
  // 基本浮点运算
  inspect(x + y, content="7")
  inspect(y - x, content="1")
  inspect(x * y, content="12")
  inspect(y / x, content="1.3333333333333333")
  
  // 测试特殊值
  let positive = 5.5
  let negative = -5.5
  let zero = 0.0
  
  inspect(positive > zero, content="true")
  inspect(negative < zero, content="true")
  inspect(positive + negative, content="0")
}