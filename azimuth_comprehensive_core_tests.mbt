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
test "int arithmetic operations" {
  let a = 10
  let b = 3
  
  // Test basic arithmetic
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
  
  // Test absolute values
  assert_eq(Int::abs(5), 5)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
  
  // Test min/max values
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  // Test comparison operations
  assert_true(a > b)
  assert_true(b < a)
  assert_true(a >= b)
  assert_true(b <= a)
  assert_false(a == b)
  assert_true(a != b)
}

///|
test "string manipulation operations" {
  let base = "Hello"
  let world = "World"
  
  // Test string concatenation
  let greeting = base + ", " + world + "!"
  assert_eq(greeting, "Hello, World!")
  
  // Test string length
  assert_eq(base.length(), 5)
  assert_eq(greeting.length(), 13)
  
  // Test string comparison
  assert_true(base == "Hello")
  assert_false(base == world)
  assert_true(base < world)
  
  // Test empty string
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty, "")
  
  // Test string with special characters
  let special = "Hello\n\t\"World\"!"
  assert_eq(special.length(), 15)
}

///|
test "list operations and manipulations" {
  // Test list creation and basic operations
  let empty_list : List[Int] = []
  let single_list = [1]
  let multi_list = [1, 2, 3, 4, 5]
  
  assert_eq(empty_list.length(), 0)
  assert_eq(single_list.length(), 1)
  assert_eq(multi_list.length(), 5)
  
  // Test list head and tail
  assert_eq(multi_list.head(), 1)
  assert_eq(multi_list.tail(), [2, 3, 4, 5])
  
  // Test list cons operation
  let new_list = 0 :: multi_list
  assert_eq(new_list, [0, 1, 2, 3, 4, 5])
  
  // Test list append
  let appended = multi_list + [6, 7]
  assert_eq(appended, [1, 2, 3, 4, 5, 6, 7])
  
  // Test list mapping
  let doubled = multi_list.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // Test list filtering
  let evens = multi_list.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
  
  // Test list folding
  let sum = multi_list.fold_left(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "array operations and manipulations" {
  // Test array creation and basic operations
  let empty_array : Array[Int] = []
  let single_array = [1]
  let multi_array = [1, 2, 3, 4, 5]
  
  assert_eq(empty_array.length(), 0)
  assert_eq(single_array.length(), 1)
  assert_eq(multi_array.length(), 5)
  
  // Test array indexing
  assert_eq(multi_array[0], 1)
  assert_eq(multi_array[4], 5)
  
  // Test array modification
  let mutable_array = [1, 2, 3]
  mutable_array[1] = 10
  assert_eq(mutable_array[1], 10)
  assert_eq(mutable_array, [1, 10, 3])
  
  // Test array push and pop
  let push_array = [1, 2, 3]
  push_array.push(4)
  assert_eq(push_array, [1, 2, 3, 4])
  
  let popped = push_array.pop()
  assert_eq(popped, Some(4))
  assert_eq(push_array, [1, 2, 3])
  
  // Test array slicing
  let slice_array = [1, 2, 3, 4, 5]
  let slice = slice_array[1..4]
  assert_eq(slice, [2, 3, 4])
}

///|
test "character operations and properties" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  let char_digit = '5'
  
  // Test character equality
  assert_true(char_a == 'a')
  assert_false(char_a == char_A)
  
  // Test character ordering
  assert_true(char_a < char_z)
  assert_true(char_A < char_a)
  assert_true(char_digit < char_a)
  
  // Test character properties
  assert_true(char_a.is_alpha())
  assert_true(char_A.is_alpha())
  assert_false(char_digit.is_alpha())
  
  assert_true(char_digit.is_digit())
  assert_false(char_a.is_digit())
  
  assert_true(char_a.is_lower())
  assert_false(char_A.is_lower())
  assert_true(char_A.is_upper())
  assert_false(char_a.is_upper())
  
  // Test character conversion
  assert_eq(char_a.to_upper(), 'A')
  assert_eq(char_A.to_lower(), 'a')
  
  // Test character to string conversion
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_digit.to_string(), "5")
}

///|
test "boolean operations and logic" {
  // Test boolean values
  let true_val = true
  let false_val = false
  
  // Test boolean equality
  assert_true(true_val == true)
  assert_false(true_val == false)
  assert_true(false_val == false)
  
  // Test boolean operations
  assert_true(true_val && true_val)
  assert_false(true_val && false_val)
  assert_false(false_val && true_val)
  assert_false(false_val && false_val)
  
  assert_true(true_val || true_val)
  assert_true(true_val || false_val)
  assert_true(false_val || true_val)
  assert_false(false_val || false_val)
  
  assert_false(!true_val)
  assert_true(!false_val)
  
  // Test boolean to string conversion
  assert_eq(true_val.to_string(), "true")
  assert_eq(false_val.to_string(), "false")
}

///|
test "result and option combination operations" {
  // Test option to result conversion
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let result_from_some = some_value.to_result(() => "No value")
  assert_eq(result_from_some, Ok(42))
  
  let result_from_none = none_value.to_result(() => "No value")
  assert_eq(result_from_none, Err("No value"))
  
  // Test result to option conversion
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error")
  
  let option_from_ok = ok_result.to_option()
  assert_eq(option_from_ok, Some(42))
  
  let option_from_err = err_result.to_option()
  assert_eq(option_from_err, None)
  
  // Test chained operations
  let chained_result = some_value
    .to_result(() => "No value")
    .map(x => x * 2)
    .to_option()
  assert_eq(chained_result, Some(84))
  
  // Test complex operations with both types
  let process_option = (opt : Int?) => 
    opt.to_result(() => "Empty")
      .map(x => x + 10)
      .bind(x => if x > 50 { Ok(x * 2) } else { Err("Too small") })
  
  assert_eq(process_option(Some(30)), Ok(80))
  assert_eq(process_option(Some(20)), Err("Too small"))
  assert_eq(process_option(None), Err("Empty"))
}

///|
test "math module functions" {
  // Test basic math functions
  assert_eq(Math::abs(-5), 5)
  assert_eq(Math::abs(5), 5)
  assert_eq(Math::abs(0), 0)
  
  // Test min and max
  assert_eq(Math::min(3, 7), 3)
  assert_eq(Math::max(3, 7), 7)
  assert_eq(Math::min(-5, -2), -5)
  assert_eq(Math::max(-5, -2), -2)
  
  // Test power functions (if available)
  // Note: These might not be available in all implementations
  // assert_eq(Math::pow(2, 3), 8)
  // assert_eq(Math::sqrt(9), 3)
  
  // Test rounding functions (if available)
  // Note: These might not be available in all implementations
  // assert_eq(Math::floor(3.7), 3)
  // assert_eq(Math::ceil(3.2), 4)
  
  // Test trigonometric functions (if available)
  // Note: These might not be available in all implementations
  // Add tests based on available functions in the actual math module
}

///|
test "cross-module integration tests" {
  // Test combining string and list operations
  let words = ["Hello", "World", "MoonBit"]
  let joined = words.fold_left("", (acc, word) => 
    if acc == "" { word } else { acc + ", " + word }
  )
  assert_eq(joined, "Hello, World, MoonBit")
  
  // Test combining option and array operations
  let numbers = [1, 2, 3, 4, 5]
  let safe_divide = (x : Int) => if x == 0 { None } else { Some(100 / x) }
  
  let results = numbers.map(safe_divide)
  let successful_results = results.filter(opt => opt.is_some())
  assert_eq(successful_results.length(), 5)
  
  // Test combining result and string operations
  let validate_positive = (x : Int) -> Result[String, String] =
    if x > 0 { Ok("Positive: " + x.to_string()) } 
    else { Err("Not positive: " + x.to_string()) }
  
  let validation_results = [5, -3, 0, 10].map(validate_positive)
  let successes = validation_results.filter(r => r.is_ok())
  let failures = validation_results.filter(r => r.is_err())
  
  assert_eq(successes.length(), 2)
  assert_eq(failures.length(), 2)
  
  // Test complex data transformation pipeline
  let process_data = (data : List[Int]) -> String => {
    data
    .filter(x => x > 0)
    .map(x => x * 2)
    .fold_left(0, (acc, x) => acc + x)
    .to_string()
  }
  
  let result = process_data([1, -2, 3, -4, 5])
  assert_eq(result, "18") // (1 + 3 + 5) * 2 = 18
}