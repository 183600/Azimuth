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

// 标准 MoonBit 核心库测试用例
// 包含 10 个高质量测试用例，覆盖核心功能模块

using @builtin { inspect }

test "integer_arithmetic_operations" {
  // 测试基本整数运算
  inspect(15 + 25, content="40")
  inspect(100 - 37, content="63")
  inspect(8 * 9, content="72")
  inspect(81 / 9, content="9")
  inspect(82 % 9, content="1")
  
  // 测试比较操作
  inspect(42 > 21, content="true")
  inspect(10 <= 10, content="true")
  inspect(-5 < 0, content="true")
  
  // 测试负数运算
  inspect(-10 + 15, content="5")
  inspect(-5 * -3, content="15")
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  let message = hello + ", " + world + "!"
  
  inspect(message, content="\"Hello, World!\"")
  inspect(message.length(), content="13")
  inspect(hello.length(), content="5")
  
  // 测试字符串比较
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
  inspect(hello < world, content="true")
  
  // 测试空字符串
  let empty = ""
  inspect(empty.length(), content="0")
  inspect(empty == "", content="true")
}

test "array_creation_and_access" {
  // 测试数组创建和访问
  let numbers = [3, 6, 9, 12, 15]
  let strings = ["apple", "banana", "cherry"]
  
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="3")
  inspect(numbers[4], content="15")
  inspect(strings.length(), content="3")
  inspect(strings[1], content="\"banana\"")
  
  // 测试数组运算
  let sum = numbers[0] + numbers[2] + numbers[4]
  inspect(sum, content="27")
  
  // 测试空数组
  let empty = [] : Array[Int]
  inspect(empty.length(), content="0")
}

test "boolean_logic_expressions" {
  // 测试基本布尔运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  
  // 测试布尔取反
  inspect(!true, content="false")
  inspect(!false, content="true")
  
  // 测试布尔比较
  inspect(true == true, content="true")
  inspect(true != false, content="true")
  inspect(false == false, content="true")
}

test "option_type_patterns" {
  // 测试 Option 类型的基本操作
  let some_number : Int? = Some(42)
  let no_number : Int? = None
  let some_text : String? = Some("MoonBit")
  
  // 使用模式匹配提取值
  let extracted_number = match some_number {
    Some(value) => value * 2
    None => 0
  }
  inspect(extracted_number, content="84")
  
  let default_number = match no_number {
    Some(value) => value
    None => -1
  }
  inspect(default_number, content="-1")
  
  // 测试 Option 比较
  inspect(some_number == Some(42), content="true")
  inspect(no_number == None, content="true")
  inspect(some_number != None, content="true")
  
  // 测试字符串 Option
  let text_length = match some_text {
    Some(text) => text.length()
    None => 0
  }
  inspect(text_length, content="7")
}

test "result_type_handling" {
  // 测试 Result 类型的成功和失败情况
  let success : Result[Int, String] = Ok(200)
  let failure : Result[Int, String] = Err("Not Found")
  let string_success : Result[String, Int] = Ok("Success")
  
  // 处理成功情况
  let success_value = match success {
    Ok(code) => code
    Err(msg) => 0
  }
  inspect(success_value, content="200")
  
  // 处理失败情况
  let error_message = match failure {
    Ok(code) => "Success: " + code.to_string()
    Err(msg) => "Error: " + msg
  }
  inspect(error_message, content="\"Error: Not Found\"")
  
  // 处理字符串成功情况
  let message = match string_success {
    Ok(text) => "Message: " + text
    Err(code) => "Error code: " + code.to_string()
  }
  inspect(message, content="\"Message: Success\"")
}

test "character_operations" {
  // 测试字符基本操作
  let char_a = 'a'
  let char_Z = 'Z'
  let digit_9 = '9'
  let space = ' '
  
  // 测试字符转换
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_Z.to_string(), content="\"Z\"")
  inspect(digit_9.to_string(), content="\"9\"")
  inspect(space.to_string(), content="\" \"")
  
  // 测试字符比较
  inspect('a' > 'Z', content="true")
  inspect('0' < '9', content="true")
  inspect('A' < 'a', content="true")
  inspect('Z' > 'A', content="true")
  
  // 测试特殊字符
  let newline = '\n'
  let tab = '\t'
  inspect(newline.to_string().length(), content="1")
  inspect(tab.to_string().length(), content="1")
}

test "mathematical_calculations" {
  // 测试浮点数运算
  let x = 2.5
  let y = 7.5
  
  inspect(x + y, content="10")
  inspect(y - x, content="5")
  inspect(x * y, content="18.75")
  inspect(y / x, content="3")
  
  // 测试正负数
  let positive = 8.0
  let negative = -3.0
  let zero = 0.0
  
  inspect(positive > zero, content="true")
  inspect(negative < zero, content="true")
  inspect(positive + negative, content="5")
  inspect(negative * -1.0, content="3")
  
  // 测试整数运算
  let int_val = 5
  inspect(int_val * 2, content="10")
  inspect(int_val + int_val, content="10")
}

test "type_conversions" {
  // 测试基本类型转换
  let int_val = 123
  let bool_val = true
  
  // 整数转字符串
  inspect(int_val.to_string(), content="\"123\"")
  
  // 布尔值转字符串
  inspect(bool_val.to_string(), content="\"true\"")
  inspect((!bool_val).to_string(), content="\"false\"")
  
  // 字符串操作
  let str_num = "42"
  inspect(str_num.length(), content="2")
  
  // 测试数值运算后的转换
  let doubled = int_val * 2
  inspect(doubled.to_string(), content="\"246\"")
  
  let halved = int_val / 2
  inspect(halved.to_string(), content="\"61\"")
  
  // 测试字符转字符串
  let char_val = 'X'
  inspect(char_val.to_string(), content="\"X\"")
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  
  // 零值测试
  let zero_int = 0
  let zero_float = 0.0
  let empty_string = ""
  let empty_array = [] : Array[Int]
  
  inspect(zero_int == 0, content="true")
  inspect(zero_float == 0.0, content="true")
  inspect(empty_string.length(), content="0")
  inspect(empty_array.length(), content="0")
  
  // 单元素集合
  let single_element = [42]
  let single_char_string = "A"
  
  inspect(single_element.length(), content="1")
  inspect(single_element[0], content="42")
  inspect(single_char_string.length(), content="1")
  
  // 布尔值的特殊情况
  inspect(true && true, content="true")
  inspect(false || false, content="false")
  
  // Option 和 Result 的边界情况
  let zero_option : Int? = Some(0)
  let empty_string_option : String? = Some("")
  
  inspect(zero_option == Some(0), content="true")
  inspect(empty_string_option == Some(""), content="true")
  
  // 测试极值比较
  let large_num = 1000000
  let small_num = -1000000
  inspect(large_num > 0, content="true")
  inspect(small_num < 0, content="true")
  inspect(large_num + small_num, content="0")
}