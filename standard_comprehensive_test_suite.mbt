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
  inspect(str1.length(), content="5")
  inspect(str2.length(), content="7")
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
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
  
  // Test short-circuit evaluation
  let mut counter = 0
  let func = () => { counter = counter + 1; true }
  
  counter = 0
  let result = f && func()
  inspect(result, content="false")
  inspect(counter, content="0")  // func should not be called
  
  counter = 0
  let result2 = t || func()
  inspect(result2, content="true")
  inspect(counter, content="0")  // func should not be called
}

///|
test "string manipulation methods" {
  let text = "MoonBit Programming"
  
  // Use string slicing instead of substring
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
test "array filtering and finding" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let even_numbers = numbers.filter(x => x % 2 == 0)
  inspect(even_numbers, content="[2, 4, 6, 8, 10]")
  
  let greater_than_five = numbers.filter(x => x > 5)
  inspect(greater_than_five, content="[6, 7, 8, 9, 10]")
  
  // Use find_with_index instead of find
  let found_option = numbers.find_with_index(x => x == 7)
  inspect(found_option, content="Some((6, 7))")  // index 6, value 7
  
  let not_found = numbers.find_with_index(x => x == 99)
  inspect(not_found, content="None")
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
  
  let flattened = matrix.fold([], (acc, row) => acc + row)
  inspect(flattened, content="[1, 2, 3, 4, 5, 6, 7, 8, 9]")
  
  // Create diagonal using indices
  let diagonal = matrix.with_index().map((i, row) => row[i])
  inspect(diagonal, content="[1, 5, 9]")
  
  let transposed = [
    [1, 4, 7],
    [2, 5, 8],
    [3, 6, 9]
  ]
  inspect(matrix[0][0], content="1")
  inspect(transposed[0][0], content="1")
  inspect(matrix[0][1], content="2")
  inspect(transposed[1][0], content="2")
  inspect(matrix[1][2], content="6")
  inspect(transposed[2][1], content="6")
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