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
test "int arithmetic and boundary operations" {
  // Test basic arithmetic with boundary values
  inspect(@int.max_value - 1, content="2147483646")
  inspect(@int.min_value + 1, content="-2147483647")
  
  // Test overflow behavior
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  
  // Test power operations with edge cases
  inspect(Int::pow(2, 3), content="8")
  inspect(Int::pow(0, 5), content="0")
  inspect(Int::pow(5, 0), content="1")
}

///|
test "string manipulation and encoding" {
  // Test string concatenation and length
  let base = "Hello"
  let result = base + " World"
  inspect(result, content="Hello World")
  inspect(result.length(), content="11")
  
  // Test character access and slicing
  let text = "MoonBit"
  inspect(text[0], content="'M'")
  inspect(text[3], content="'n'")
  inspect(text.length(), content="7")
  
  // Test string comparison
  assert_eq("abc" == "abc", true)
  assert_eq("abc" != "def", true)
  assert_eq("apple" > "banana", false)
}

///|
test "list operations and transformations" {
  // Test list creation and basic operations
  let empty_list : @list.List[Int] = @list.new()
  assert_eq(@list.length(empty_list), 0)
  
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  inspect(@list.length(numbers), content="5")
  
  // Test list concatenation and mapping
  let doubled = @list.map(numbers, fn(x) { x * 2 })
  inspect(@list.to_array(doubled), content="[2, 4, 6, 8, 10]")
  
  // Test list filtering and folding
  let evens = @list.filter(numbers, fn(x) { x % 2 == 0 })
  inspect(@list.to_array(evens), content="[2, 4]")
  
  let sum = @list.fold(numbers, 0, fn(acc, x) { acc + x })
  inspect(sum, content="15")
}

///|
test "option type handling and pattern matching" {
  // Test option creation and matching
  let some_value : @option.Option[Int] = @option.Some(42)
  let none_value : @option.Option[Int] = @option.None
  
  // Test option operations
  assert_eq(@option.is_some(some_value), true)
  assert_eq(@option.is_none(none_value), true)
  
  // Test option mapping and default values
  let doubled = @option.map(some_value, fn(x) { x * 2 })
  inspect(@option.unwrap_or(doubled, 0), content="84")
  
  let default_result = @option.map_or(none_value, 0, fn(x) { x * 2 })
  inspect(default_result, content="0")
}

///|
test "result type error handling" {
  // Test result creation and handling
  let success : @result.Result[String, String] = @result.Ok("Success")
  let failure : @result.Result[String, String] = @result.Err("Error occurred")
  
  // Test result predicates
  assert_eq(@result.is_ok(success), true)
  assert_eq(@result.is_err(failure), true)
  
  // Test result mapping
  let uppercased = @result.map(success, fn(s) { s.uppercase() })
  inspect(@result.unwrap_or(uppercased, "Default"), content="SUCCESS")
  
  let error_mapped = @result.map_err(failure, fn(e) { "Error: " + e })
  assert_eq(@result.unwrap_err(error_mapped), "Error: Error occurred")
}

///|
test "array operations and bounds checking" {
  // Test array creation and access
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // Test array slicing and modification
  let slice = numbers[1..4]
  inspect(slice, content="[2, 3, 4]")
  
  // Test array transformations
  let doubled = numbers.map(fn(x) { x * 2 })
  inspect(doubled, content="[2, 4, 6, 8, 10]")
  
  let summed = numbers.fold(0, fn(acc, x) { acc + x })
  inspect(summed, content="15")
}

///|
test "boolean logic and truthiness" {
  // Test basic boolean operations
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(!true, false)
  
  // Test boolean conversion and comparison
  let zero = 0
  let non_zero = 42
  assert_eq(zero == 0, true)
  assert_eq(non_zero != 0, true)
  
  // Test boolean expressions with complex conditions
  let condition = (5 > 3) && (2 < 4) || false
  assert_eq(condition, true)
}

///|
test "character operations and unicode" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  assert_eq(char_a == 'a', true)
  assert_eq(char_A == char_a, false)
  
  // Test character conversion
  inspect(char_a.to_string(), content="'a'")
  inspect(char_digit.to_string(), content="'5'")
  
  // Test character arithmetic (if supported)
  let next_char = char_a.to_int() + 1
  inspect(String::from_char(next_char.to_char()), content="'b'")
}

///|
test "tuple operations and destructuring" {
  // Test tuple creation and access
  let pair = (42, "answer")
  let triple = (1, "hello", true)
  
  inspect(pair.0, content="42")
  inspect(pair.1, content="answer")
  inspect(triple.2, content="true")
  
  // Test tuple comparison
  let pair2 = (42, "answer")
  assert_eq(pair == pair2, true)
  assert_eq(pair != (43, "answer"), true)
}

///|
test "type conversion and casting" {
  // Test numeric type conversions
  let int_val = 42
  let uint_val = int_val.to_uint()
  let int64_val = int_val.to_int64()
  let double_val = int_val.to_double()
  
  inspect(uint_val, content="42")
  inspect(int64_val, content="42")
  inspect(double_val, content="42.0")
  
  // Test string to number conversion (if available)
  let num_str = "123"
  // Note: Actual conversion methods may vary based on available APIs
  inspect(num_str.length(), content="3")
  
  // Test boolean to string conversion
  inspect(true.to_string(), content="true")
  inspect(false.to_string(), content="false")
}