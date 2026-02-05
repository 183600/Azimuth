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

// 导入必要的函数
using @builtin { inspect }

// 核心 MoonBit 标准库测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "integer_basic_operations" {
  // 测试整数基本算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="\"Hello World\"")
  inspect(combined.length(), content="11")
  inspect(hello == "Hello", content="true")
  inspect(hello != world, content="true")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(true != false, content="true")
}

test "option_type_basic" {
  // 测试 Option 类型基本操作
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

test "result_type_basic" {
  // 测试 Result 类型基本操作
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
}

test "type_conversions" {
  // 测试类型转换
  let int_val = 123
  inspect(int_val.to_string(), content="\"123\"")
  let doubled = int_val * 2
  inspect(doubled, content="246")
  let halved = int_val / 2
  inspect(halved, content="61")
  inspect(true.to_string(), content="\"true\"")
  inspect(false.to_string(), content="\"false\"")
}

test "pattern_matching" {
  // 测试模式匹配
  let some_int : Int? = Some(123)
  let some_str : String? = Some("hello")
  
  let result = match (some_int, some_str) {
    (Some(x), Some(y)) => x + y.length()
    (Some(x), None) => x
    (None, Some(y)) => y.length()
    (None, None) => 0
  }
  inspect(result, content="128") // 123 + 5
}

test "edge_cases" {
  // 测试边界情况
  let empty_string = ""
  let empty_array = []
  inspect(empty_string.length(), content="0")
  inspect(empty_array.length(), content="0")
  
  let max_int = 2147483647
  let min_int = -2147483648
  inspect(max_int > 0, content="true")
  inspect(min_int < 0, content="true")
  inspect(max_int + min_int, content="-1")
}