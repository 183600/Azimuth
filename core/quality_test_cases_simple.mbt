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

test "basic_integer_operations" {
  // 基本整数运算测试
  inspect(15 + 27, content="42")
  inspect(100 - 37, content="63")
  inspect(12 * 8, content="96")
  inspect(45 / 9, content="5")
  inspect(45 % 8, content="5")
  inspect((-42).abs(), content="42")
  inspect(42.abs(), content="42")
}

test "string_operations" {
  // 字符串操作测试
  let text = "MoonBit Programming"
  inspect(text.length(), content="18")
  inspect(text[0], content="77")  // 'M' ASCII码
  inspect(text[7], content="66")  // 'B' ASCII码
  inspect(text + " Language", content=""MoonBit Programming Language"")
  inspect("Hello" + " " + "World", content=""Hello World"")
}

test "array_operations" {
  // 数组操作测试
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(numbers[2] + numbers[3], content="7")
  
  let strings = ["apple", "banana", "cherry"]
  inspect(strings.length(), content="3")
  inspect(strings[0], content=""apple"")
  inspect(strings[1] + " " + strings[2], content=""banana cherry"")
}

test "boolean_logic" {
  // 布尔逻辑测试
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(true != false, content="true")
}

test "option_type_operations" {
  // Option 类型操作测试
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let result1 = match some_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(result1, content="84")
  
  let result2 = match none_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(result2, content="0")
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

test "result_type_operations" {
  // Result 类型操作测试
  let success : Result[Int, String] = Ok(100)
  let failure : Result[Int, String] = Err("Invalid input")
  
  let result1 = match success {
    Ok(x) => x * 2
    Err(_) => 0
  }
  inspect(result1, content="200")
  
  let result2 = match failure {
    Ok(x) => x * 2
    Err(_) => 0
  }
  inspect(result2, content="0")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
}

test "character_operations" {
  // 字符操作测试
  let char_a = 'a'
  let char_z = 'z'
  let char_A = 'A'
  let char_Z = 'Z'
  
  inspect(char_a.to_string(), content=""a"")
  inspect(char_z.to_string(), content=""z"")
  inspect(char_A.to_string(), content=""A"")
  inspect(char_Z.to_string(), content=""Z"")
  
  inspect(char_a > char_A, content="true")
  inspect(char_z < char_Z, content="false")
  inspect(char_A < char_Z, content="true")
  inspect('1' < '2', content="true")
}

test "comparison_and_bitwise_operations" {
  // 比较和位运算测试
  inspect(15 > 10, content="true")
  inspect(15 >= 15, content="true")
  inspect(10 < 15, content="true")
  inspect(15 <= 15, content="true")
  inspect(15 == 15, content="true")
  inspect(15 != 10, content="true")
  
  inspect(1 << 4, content="16")
  inspect(16 >> 2, content="4")
  inspect(12 & 10, content="8")
  inspect(12 | 10, content="14")
  inspect(12 ^ 10, content="6")
}