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

// Azimuth 标准测试套件
// 包含 8 个高质量 MoonBit 测试用例，使用标准 MoonBit 测试语法

using @builtin { inspect }

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  inspect(-8 + 3, content="-5")
  inspect(-10 * 2, content="-20")
}

test "string_manipulation" {
  // 测试字符串操作
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="\"Hello World\"")
  inspect(combined.length(), content="11")
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
  inspect("".length(), content="0")
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
}

test "boolean_logic" {
  // 测试布尔逻辑
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(true != false, content="true")
}

test "option_type_handling" {
  // 测试 Option 类型处理
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let result = match some_value {
    Some(x) => x
    None => 0
  }
  inspect(result, content="42")
  
  let default_result = match none_value {
    Some(x) => x
    None => 0
  }
  inspect(default_result, content="0")
}

test "result_type_handling" {
  // 测试 Result 类型处理
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("error")
  
  let success_value = match success {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(success_value, content="42")
  
  let failure_value = match failure {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(failure_value, content="0")
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'A'
  let char_str = char_a.to_string()
  inspect(char_str, content="\"A\"")
  inspect('a' > 'A', content="true")
  inspect('A' < 'B', content="true")
  inspect(char_str.length(), content="1")
}

test "mathematical_operations" {
  // 测试数学运算
  let x = 2
  let y = 8
  inspect(x + y, content="10")
  inspect(y - x, content="6")
  inspect(x * y, content="16")
  inspect(y / x, content="4")
  let pos = 5
  let neg = -5
  inspect(pos, content="5")
  inspect(neg * -1, content="5")
}