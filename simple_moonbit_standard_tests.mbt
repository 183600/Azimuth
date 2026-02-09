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
test "string concatenation and length" {
  let str1 = "Hello"
  let str2 = " MoonBit"
  let combined = str1 + str2
  
  inspect(combined, content="Hello MoonBit")
  inspect(combined.length(), content="11")
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  
  let doubled = some_value.map(x => x * 2)
  inspect(doubled, content="Some(84)")
  
  let doubled_none = none_value.map(x => x * 2)
  inspect(doubled_none, content="None")
}

///|
test "int arithmetic operations" {
  let a = 15
  let b = 4
  
  inspect(a + b, content="19")
  inspect(a - b, content="11")
  inspect(a * b, content="60")
  inspect(a / b, content="3")
  inspect(a % b, content="3")
  
  inspect(a > b, content="true")
  inspect(a < b, content="false")
  inspect(a == b, content="false")
  inspect(a != b, content="true")
}

///|
test "bool logical operations" {
  let t = true
  let f = false
  
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "string manipulation methods" {
  let text = "MoonBit Programming"
  
  // Use string slicing
  let substring = text[0:7]
  inspect(substring, content="MoonBit")
  
  // Check if string contains substring
  inspect(text.contains("Moon"), content="true")
  inspect(text.contains("Python"), content="false")
  
  // Convert string to array of characters
  let chars = text.to_array()
  inspect(chars.length(), content="18")
  inspect(chars[0], content="'M'")
  inspect(chars[17], content="'g'")
}

///|
test "array filtering operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let even_numbers = numbers.filter(x => x % 2 == 0)
  inspect(even_numbers, content="[2, 4, 6, 8, 10]")
  
  let greater_than_five = numbers.filter(x => x > 5)
  inspect(greater_than_five, content="[6, 7, 8, 9, 10]")
}

///|
test "option chaining operations" {
  let process_value = (x : Int) => if x > 0 { Some(x * 2) } else { None }
  
  let input1 : Int? = Some(5)
  let result1 = input1.bind(process_value)
  inspect(result1, content="Some(10)")
  
  let input2 : Int? = Some(-3)
  let result2 = input2.bind(process_value)
  inspect(result2, content="None")
  
  let input3 : Int? = None
  let result3 = input3.bind(process_value)
  inspect(result3, content="None")
  
  // Test unwrap_or_else
  let fallback1 = input1.unwrap_or_else(() => 99)
  inspect(fallback1, content="5")
  
  let fallback2 = input3.unwrap_or_else(() => 99)
  inspect(fallback2, content="99")
}

///|
test "complex data structure operations" {
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  // Simple array operations
  let first_row = matrix[0]
  inspect(first_row, content="[1, 2, 3]")
  
  let last_row = matrix[2]
  inspect(last_row, content="[7, 8, 9]")
  
  let center_element = matrix[1][1]
  inspect(center_element, content="5")
}

///|
test "error handling with result type" {
  let safe_divide = (x : Int, y : Int) -> Result[Int, String] {
    if y == 0 {
      Error("Division by zero")
    } else {
      Ok(x / y)
    }
  }
  
  let success = safe_divide(10, 2)
  inspect(success, content="Ok(5)")
  
  let failure = safe_divide(10, 0)
  inspect(failure, content="Error(\"Division by zero\")")
  
  // Test result mapping
  let mapped_success = success.map(x => x * 2)
  inspect(mapped_success, content="Ok(10)")
  
  let mapped_failure = failure.map(x => x * 2)
  inspect(mapped_failure, content="Error(\"Division by zero\")")
}