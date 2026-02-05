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

///|
test "int basic operations" {
  // Test basic arithmetic
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  // Test negative numbers
  inspect(-10, content="-10")
  inspect(-(-10), content="10")
  
  // Test comparisons
  assert_eq(10 > 5, true)
  assert_eq(10 < 20, true)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
}

///|
test "string operations" {
  // Test string creation and concatenation
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="\"Hello World\"")
  
  // Test string length
  inspect(combined.length(), content="11")
  
  // Test string comparison
  assert_eq(hello == "Hello", true)
  assert_eq(hello != world, true)
}

///|
test "array operations" {
  // Test array creation and access
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // Test array operations
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  inspect(sum, content="15")
  
  // Test array with different types
  let strings = ["apple", "banana", "cherry"]
  inspect(strings.length(), content="3")
  inspect(strings[1], content="\"banana\"")
}

///|
test "option type handling" {
  // Test Some value
  let some_value : Int? = Some(42)
  
  let result = match some_value {
    Some(x) => x
    None => 0
  }
  inspect(result, content="42")
  
  // Test None value
  let none_value : Int? = None
  
  let default_result = match none_value {
    Some(x) => x
    None => 0
  }
  inspect(default_result, content="0")
  
  // Test option comparison
  assert_eq(some_value == Some(42), true)
  assert_eq(none_value == None, true)
}

///|
test "result type handling" {
  // Test Ok value
  let success : Result[Int, String] = Ok(42)
  
  let success_value = match success {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(success_value, content="42")
  
  // Test Err value
  let failure : Result[Int, String] = Err("error")
  
  let failure_value = match failure {
    Ok(x) => x
    Err(_) => 0
  }
  inspect(failure_value, content="0")
  
  // Test result comparison
  assert_eq(success == Ok(42), true)
  assert_eq(failure == Err("error"), true)
}

///|
test "character operations" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  // Test character comparisons
  assert_eq(char_a == 'a', true)
  assert_eq(char_a != char_A, true)
  assert_eq(char_a > char_A, true)  // lowercase has higher ASCII value
  
  // Test character to string conversion
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_digit.to_string(), content="\"5\"")
  
  // Test character arithmetic
  let next_char = 'a' + 1
  inspect(next_char, content="\"b\"")
}

///|
test "boolean logic" {
  // Test basic boolean operations
  assert_eq(true && false, content="false")
  assert_eq(true || false, content="true")
  assert_eq(!true, content="false")
  assert_eq(!false, content="true")
  
  // Test boolean equality
  assert_eq(true == true, true)
  assert_eq(true != false, true)
  
  // Test boolean in conditional
  let result = if (true && !false) { 1 } else { 0 }
  inspect(result, content="1")
}

///|
test "type conversions" {
  // Test integer to string
  let int_val = 123
  let string_from_int = int_val.to_string()
  inspect(string_from_int, content="\"123\"")
  
  // Test character to string
  let char_val = 'X'
  let string_from_char = char_val.to_string()
  inspect(string_from_char, content="\"X\"")
  
  // Test boolean to string
  let bool_val = true
  let string_from_bool = bool_val.to_string()
  inspect(string_from_bool, content="\"true\"")
}

///|
test "array advanced operations" {
  // Test array creation with different methods
  let numbers1 = [1, 2, 3]
  let numbers2 = Array::make(3, 0)
  numbers2[0] = 10
  numbers2[1] = 20
  numbers2[2] = 30
  
  inspect(numbers1.length(), content="3")
  inspect(numbers2.length(), content="3")
  inspect(numbers2[1], content="20")
  
  // Test array bounds
  let safe_array = [100, 200, 300]
  inspect(safe_array[0], content="100")
  inspect(safe_array[2], content="300")
}

///|
test "mathematical operations" {
  // Test basic arithmetic
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
  
  // Test edge cases
  inspect(0 + 5, content="5")
  inspect(5 - 0, content="5")
  inspect(0 * 5, content="0")
  
  // Test negative arithmetic
  inspect(-10 + 5, content="-5")
  inspect(-10 - 5, content="-15")
}