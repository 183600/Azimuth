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

// 使用标准的 MoonBit 测试语法编写的10个测试用例

using @builtin { inspect }

///|
test "string_length_and_concatenation" {
  let str1 = "Hello"
  let str2 = " World"
  let combined = str1 + str2
  inspect(str1.length(), content="5")
  inspect(str2.length(), content="6")
  inspect(combined.length(), content="11")
  inspect(combined, content="\"Hello World\"")
}

///|
test "integer_arithmetic_operations" {
  let a = 15
  let b = 4
  inspect(a + b, content="19")
  inspect(a - b, content="11")
  inspect(a * b, content="60")
  inspect(a / b, content="3")
  inspect(a % b, content="3")
}

///|
test "boolean_logic_expressions" {
  let x = true
  let y = false
  inspect(x && y, content="false")
  inspect(x || y, content="true")
  inspect(!x, content="false")
  inspect(!y, content="true")
  inspect(x == y, content="false")
  inspect(x != y, content="true")
}

///|
test "array_basic_operations" {
  let numbers = [10, 20, 30, 40, 50]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
  inspect(numbers[2] + numbers[3], content="70")
}

///|
test "option_type_handling" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let result1 = match some_value {
    Some(x) => x * 2
    None => 0
  }
  
  let result2 = match none_value {
    Some(x) => x * 2
    None => -1
  }
  
  inspect(result1, content="84")
  inspect(result2, content="-1")
}

///|
test "pattern_matching_with_integers" {
  let value = 7
  let description = match value {
    0 => "zero"
    1 | 2 | 3 => "small"
    4 | 5 | 6 => "medium"
    _ => "large"
  }
  inspect(description, content="\"large\"")
}

///|
test "tuple_operations" {
  let person = ("Alice", 30, true)
  let name = person.0
  let age = person.1
  let is_active = person.2
  
  inspect(name, content="\"Alice\"")
  inspect(age, content="30")
  inspect(is_active, content="true")
  
  let updated_person = (name, age + 1, is_active)
  inspect(updated_person.1, content="31")
}

///|
test "character_operations" {
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_A.to_string(), content="\"A\"")
  inspect(char_digit.to_string(), content="\"5\"")
  
  inspect(char_a.to_int(), content="97")
  inspect(char_A.to_int(), content="65")
  inspect(char_digit.to_int(), content="53")
}

///|
test "result_type_error_handling" {
  let success_result : Result[String, Int] = Ok("Operation successful")
  let error_result : Result[String, Int] = Err(404)
  
  let success_message = match success_result {
    Ok(msg) => msg
    Err(code) => "Error: " + code.to_string()
  }
  
  let error_message = match error_result {
    Ok(msg) => msg
    Err(code) => "Error code: " + code.to_string()
  }
  
  inspect(success_message, content="\"Operation successful\"")
  inspect(error_message, content="\"Error code: 404\"")
}

///|
test "integer_boundary_values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试边界值附近的运算
  inspect(@int.max_value - 1, content="2147483646")
  inspect(@int.min_value + 1, content="-2147483647")
  
  // 测试溢出行为
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
}