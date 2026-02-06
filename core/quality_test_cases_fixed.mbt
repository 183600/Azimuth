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

test "advanced_integer_operations" {
  // 高级整数运算测试
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect((-42).abs(), content="42")
  inspect(42.abs(), content="42")
  inspect(0.abs(), content="0")
}

test "string_manipulation" {
  // 字符串操作测试
  let text = "MoonBit Programming"
  inspect(text.length(), content="18")
  inspect(text.contains("Bit"), content="true")
  inspect(text.contains("Rust"), content="false")
  inspect(text[0], content="77")  // 'M' ASCII码
  inspect(text[7], content="66")  // 'B' ASCII码
}

test "array_basic_operations" {
  // 数组基本操作测试
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  inspect(numbers[2] + numbers[3], content="7")
}

test "option_type_advanced" {
  // Option 类型高级操作测试
  let maybe_number : Int? = Some(42)
  let empty : Int? = None
  
  let result1 = match maybe_number {
    Some(x) => Some(x * 2)
    None => None
  }
  inspect(result1, content="Some(84)")
  
  let result3 = match maybe_number {
    Some(x) => x
    None => 0
  }
  inspect(result3, content="42")
  
  let result4 = match empty {
    Some(x) => x
    None => 0
  }
  inspect(result4, content="0")
}

test "result_type_error_handling" {
  // Result 类型错误处理测试
  let success : Result[Int, String] = Ok(100)
  let failure : Result[Int, String] = Err("Invalid input")
  
  let result1 = match success {
    Ok(x) => Ok(x * 2)
    Err(e) => Err(e)
  }
  inspect(result1, content="Ok(200)")
  
  let result3 = match success {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(result3, content="100")
  
  let result4 = match failure {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(result4, content="0")
}

test "character_encoding" {
  // 字符编码测试
  let char_a = 'a'
  let char_z = 'z'
  let char_A = 'A'
  let char_Z = 'Z'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_z.to_string(), content="\"z\"")
  inspect(char_A.to_string(), content="\"A\"")
  inspect(char_Z.to_string(), content="\"Z\"")
  
  inspect(char_a > char_A, content="true")
  inspect(char_z < char_Z, content="false")
}

test "mathematical_functions" {
  // 数学函数测试
  let x = 16
  let y = 4
  
  inspect(x + y, content="20")
  inspect(x - y, content="12")
  inspect(x * y, content="64")
  inspect(x / y, content="4")
  inspect(x % y, content="0")
  
  inspect(x > y, content="true")
  inspect(y < x, content="true")
}

test "type_conversion_operations" {
  // 类型转换测试
  let number = 456
  let string_num = "789"
  inspect(number.to_string(), content="\"456\"")
  inspect(string_num.to_int(), content="Some(789)")
  inspect("invalid".to_int(), content="None")
  inspect(true.to_string(), content="\"true\"")
  inspect(false.to_string(), content="\"false\"")
}