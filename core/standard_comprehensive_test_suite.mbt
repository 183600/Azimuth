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
  
  assert_eq(combined, "Hello MoonBit")
  assert_eq(combined.length(), 11)
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 7)
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
  
  let doubled = some_value.map(x => x * 2)
  assert_eq(doubled, Some(84))
  
  let doubled_none = none_value.map(x => x * 2)
  assert_eq(doubled_none, None)
}

///|
test "int arithmetic operations" {
  let a = 15
  let b = 4
  
  assert_eq(a + b, 19)
  assert_eq(a - b, 11)
  assert_eq(a * b, 60)
  assert_eq(a / b, 3)
  assert_eq(a % b, 3)
  
  assert_eq(a > b, true)
  assert_eq(a < b, false)
  assert_eq(a == b, false)
  assert_eq(a != b, true)
}

///|
test "bool logical operations" {
  let t = true
  let f = false
  
  assert_eq(t && f, false)
  assert_eq(t || f, true)
  assert_eq(!t, false)
  assert_eq(!f, true)
  
  // Test short-circuit evaluation
  let mut counter = 0
  let func = () => { counter = counter + 1; true }
  
  counter = 0
  let result = f && func()
  assert_eq(result, false)
  assert_eq(counter, 0)  // func should not be called
  
  counter = 0
  let result2 = t || func()
  assert_eq(result2, true)
  assert_eq(counter, 0)  // func should not be called
}

///|
test "string manipulation methods" {
  let text = "MoonBit Programming"
  
  assert_eq(text.to_upper_case(), "MOONBIT PROGRAMMING")
  assert_eq(text.to_lower_case(), "moonbit programming")
  
  let substring = text.substring(start=0, end=7)
  assert_eq(substring, "MoonBit")
  
  assert_eq(text.contains("Moon"), true)
  assert_eq(text.contains("Python"), false)
  
  let chars = text.to_char_array()
  assert_eq(chars.length(), 18)
  assert_eq(chars[0], 'M')
  assert_eq(chars[17], 'g')
}

///|
test "array filtering and finding" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let even_numbers = numbers.filter(x => x % 2 == 0)
  assert_eq(even_numbers, [2, 4, 6, 8, 10])
  
  let greater_than_five = numbers.filter(x => x > 5)
  assert_eq(greater_than_five, [6, 7, 8, 9, 10])
  
  let found = numbers.find(x => x == 7)
  assert_eq(found, Some(7))
  
  let not_found = numbers.find(x => x == 99)
  assert_eq(not_found, None)
}

///|
test "option chaining operations" {
  let process_value = (x : Int) => if x > 0 { Some(x * 2) } else { None }
  
  let input1 : Int? = Some(5)
  let result1 = input1.and_then(process_value)
  assert_eq(result1, Some(10))
  
  let input2 : Int? = Some(-3)
  let result2 = input2.and_then(process_value)
  assert_eq(result2, None)
  
  let input3 : Int? = None
  let result3 = input3.and_then(process_value)
  assert_eq(result3, None)
  
  // Test or_else
  let fallback1 = input1.or_else(() => Some(99))
  assert_eq(fallback1, Some(5))
  
  let fallback2 = input3.or_else(() => Some(99))
  assert_eq(fallback2, Some(99))
}

///|
test "complex data structure operations" {
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  let flattened = matrix.fold([], (acc, row) => acc + row)
  assert_eq(flattened, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  let diagonal = matrix.with_index().map((i, row) => row[i])
  assert_eq(diagonal, [1, 5, 9])
  
  let transposed = [
    [1, 4, 7],
    [2, 5, 8],
    [3, 6, 9]
  ]
  assert_eq(matrix[0][0], transposed[0][0])
  assert_eq(matrix[0][1], transposed[1][0])
  assert_eq(matrix[1][2], transposed[2][1])
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
  assert_eq(success, Ok(5))
  
  let failure = safe_divide(10, 0)
  assert_eq(failure, Error("Division by zero"))
  
  // Test result mapping
  let mapped_success = success.map(x => x * 2)
  assert_eq(mapped_success, Ok(10))
  
  let mapped_failure = failure.map(x => x * 2)
  assert_eq(mapped_failure, Error("Division by zero"))
}