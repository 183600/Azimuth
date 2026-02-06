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
test "int boundary and arithmetic operations" {
  // Test boundary values
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  
  // Test overflow behavior
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  
  // Test basic arithmetic
  assert_eq(15 + 7, 22)
  assert_eq(15 - 7, 8)
  assert_eq(6 * 8, 48)
  assert_eq(20 / 4, 5)
  assert_eq(17 % 5, 2)
  
  // Test bit operations
  assert_eq(12 & 10, 8)
  assert_eq(12 | 10, 14)
  assert_eq(12 ^ 10, 6)
  assert_eq(~5, -6)
  assert_eq(3 << 2, 12)
  assert_eq(16 >> 2, 4)
}

///|
test "string manipulation and comparison" {
  // Test string concatenation
  let first = "Hello"
  let second = "MoonBit"
  let combined = first + " " + second
  assert_eq(combined, "Hello MoonBit")
  
  // Test string properties
  assert_eq(combined.length(), 12)
  assert_eq(first.length(), 5)
  assert_eq(second.length(), 7)
  
  // Test string comparison
  assert_eq("apple" == "apple", true)
  assert_eq("apple" != "banana", true)
  assert_eq("zebra" > "apple", true)
  assert_eq("apple" < "banana", true)
  
  // Test empty string
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty + "test", "test")
}

///|
test "list comprehensive operations" {
  // Test list creation
  let empty_list : List[Int] = []
  assert_eq(empty_list.length(), 0)
  
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  
  // Test list transformations
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let squared = numbers.map(x => x * x)
  assert_eq(squared, [1, 4, 9, 16, 25])
  
  // Test list filtering
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
  
  let odds = numbers.filter(x => x % 2 != 0)
  assert_eq(odds, [1, 3, 5])
  
  // Test list folding
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  let product = numbers.fold(1, (acc, x) => acc * x)
  assert_eq(product, 120)
}

///|
test "option type pattern matching" {
  // Test option creation
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  
  // Test option predicates
  assert_true(some_int is Some(_))
  assert_true(none_int is None)
  
  // Test option operations
  assert_eq(some_int.unwrap_or(0), 42)
  assert_eq(none_int.unwrap_or(99), 99)
  
  // Test option mapping
  let doubled = some_int.map(x => x * 2)
  assert_eq(doubled.unwrap_or(0), 84)
  
  let mapped_none = none_int.map(x => x * 2)
  assert_eq(mapped_none.unwrap_or(0), 0)
  
  // Test option chaining
  let chained = some_int.map(x => x + 10).map(x => x * 2)
  assert_eq(chained.unwrap_or(0), 104)
}

///|
test "result type error handling" {
  // Test result creation
  let success : Result[String, String] = Ok("Success")
  let failure : Result[String, String] = Err("Error occurred")
  
  // Test result predicates
  assert_true(success is Ok(_))
  assert_true(failure is Err(_))
  
  // Test result operations
  assert_eq(success.unwrap_or("Default"), "Success")
  assert_eq(failure.unwrap_or("Default"), "Default")
  
  // Test result mapping
  let upper_success = success.map(s => s + "!")
  assert_eq(upper_success.unwrap_or("Default"), "Success!")
  
  let mapped_failure = failure.map(s => s + "!")
  assert_eq(mapped_failure.unwrap_or("Default"), "Default")
  
  // Test error mapping
  let error_msg = failure.map_err(e => "Error: " + e)
  assert_eq(error_msg.unwrap_err(), "Error: Error occurred")
}

///|
test "array advanced operations" {
  // Test array creation and access
  let numbers = [10, 20, 30, 40, 50]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 10)
  assert_eq(numbers[4], 50)
  
  // Test array transformations
  let halved = numbers.map(x => x / 2)
  assert_eq(halved, [5, 10, 15, 20, 25])
  
  // Test array filtering
  let greater_than_25 = numbers.filter(x => x > 25)
  assert_eq(greater_than_25, [30, 40, 50])
  
  // Test array folding
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 150)
  
  // Test array with mixed operations
  let result = numbers
    .filter(x => x > 15)
    .map(x => x - 5)
    .fold(0, (acc, x) => acc + x)
  assert_eq(result, 90)
}

///|
test "boolean logic and precedence" {
  // Test basic boolean operations
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(!true, false)
  assert_eq(!false, true)
  
  // Test boolean comparisons
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
  
  // Test operator precedence
  assert_eq(true || false && false, true)
  assert_eq((true || false) && false, false)
  assert_eq(true && false || true, true)
  assert_eq(true && (false || true), true)
  
  // Test complex boolean expressions
  let x = 10
  let y = 20
  let z = 30
  assert_eq(x < y && y < z, true)
  assert_eq(x > y || y < z, true)
}

///|
test "character operations and encoding" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  let char_symbol = '@'
  
  assert_eq(char_a == 'a', true)
  assert_eq(char_A == char_a, false)
  assert_eq(char_digit == '5', true)
  assert_eq(char_symbol == '@', true)
  
  // Test character ordering
  assert_true(char_a > char_A)
  assert_true(char_digit < char_a)
  
  // Test character conversion
  assert_eq(char_a.to_string(), "'a'")
  assert_eq(char_digit.to_string(), "'5'")
  
  // Test character arithmetic (if supported)
  let char_code_a = char_a.to_int()
  let char_code_b = char_code_a + 1
  assert_eq(char_code_b > char_code_a, true)
}

///|
test "tuple operations and destructuring" {
  // Test tuple creation
  let pair = (42, "answer")
  let triple = (1, "hello", true)
  let quadruple = (3.14, "pi", false, 2024)
  
  // Test tuple access
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "hello")
  assert_eq(triple.2, true)
  assert_eq(quadruple.2, false)
  
  // Test tuple comparison
  let pair2 = (42, "answer")
  assert_eq(pair == pair2, true)
  assert_eq(pair != (43, "answer"), true)
  
  // Test tuple with different types
  let mixed = (true, 3.14, "test", 42)
  assert_eq(mixed.0, true)
  assert_eq(mixed.1, 3.14)
  assert_eq(mixed.2, "test")
  assert_eq(mixed.3, 42)
}

///|
test "type conversion and casting" {
  // Test numeric type conversions
  let int_val = 100
  let uint_val = int_val.to_uint()
  let int64_val = int_val.to_int64()
  let double_val = int_val.to_double()
  
  assert_eq(uint_val, 100)
  assert_eq(int64_val, 100)
  assert_eq(double_val, 100.0)
  
  // Test negative number conversion
  let negative_int = -50
  let negative_uint = negative_int.to_uint()
  assert_eq(negative_uint > 4000000000, true)
  
  // Test boolean to string conversion
  assert_eq(true.to_string(), "true")
  assert_eq(false.to_string(), "false")
  
  // Test int to string conversion
  assert_eq(42.to_string(), "42")
  assert_eq((-17).to_string(), "-17")
}