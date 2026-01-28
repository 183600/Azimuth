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

// Import necessary functions
using @builtin { inspect }

// 标准 MoonBit 测试用例 - 10个高质量测试

///|
test "string_length_and_operations" {
  let s = "MoonBit"
  inspect(s.length(), content="7")
  inspect(s[0], content="'M'")
  inspect(s[6], content="'t'")
}

///|
test "basic_arithmetic_operations" {
  let a = 15
  let b = 4
  inspect(a + b, content="19")
  inspect(a - b, content="11")
  inspect(a * b, content="60")
  inspect(a / b, content="3")
  inspect(a % b, content="3")
}

///|
test "boolean_logic_evaluation" {
  let x = true
  let y = false
  inspect(x && y, content="false")
  inspect(x || y, content="true")
  inspect(!x, content="false")
  inspect(!y, content="true")
  inspect(x != y, content="true")
}

///|
test "array_manipulation" {
  let numbers = [10, 20, 30, 40, 50]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="10")
  inspect(numbers[4], content="50")
}

///|
test "option_type_pattern_matching" {
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
test "result_type_error_handling" {
  let success : Result[String, Int] = Ok("Operation completed")
  let failure : Result[String, Int] = Err(404)
  
  let message1 = match success {
    Ok(msg) => msg
    Err(code) => "Error: " + code.to_string()
  }
  
  let message2 = match failure {
    Ok(msg) => msg
    Err(code) => "Error: " + code.to_string()
  }
  
  inspect(message1, content="\"Operation completed\"")
  inspect(message2, content="\"Error: 404\"")
}

///|
test "character_operations" {
  let ch = 'Z'
  inspect(ch.to_string(), content="\"Z\"")
  inspect(ch.to_int(), content="90")
}

///|
test "tuple_destructuring" {
  let point = (3, 7)
  let x = point.0
  let y = point.1
  let sum = x + y
  inspect(sum, content="10")
}

///|
test "complex_pattern_matching" {
  let value = 15
  let category = match value {
    0 => "zero"
    1 | 2 | 3 => "small"
    4 | 5 | 6 | 7 | 8 | 9 => "medium"
    _ => "large"
  }
  inspect(category, content="\"large\"")
}

///|
test "string_concatenation_and_formatting" {
  let first = "Hello"
  let second = "MoonBit"
  let year = "2024"
  let message = first + ", " + second + " (" + year + ")"
  inspect(message, content="\"Hello, MoonBit (2024)\"")
}