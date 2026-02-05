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
test "option basic test" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_true(some_value is Some(_))
  assert_false(some_value is None)
  assert_false(none_value is Some(_))
  assert_true(none_value is None)
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "int arithmetic test" {
  let a = 10
  let b = 3
  
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
  
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(5), 5)
}

///|
test "string operations test" {
  let hello = "Hello"
  let world = "World"
  let empty = ""
  
  assert_eq(hello.length(), 5)
  assert_eq(empty.length(), 0)
  
  let combined = hello + " " + world
  assert_eq(combined, "Hello World")
}

///|
test "array operations test" {
  let numbers = [1, 2, 3, 4, 5]
  let empty_array : Array[Int] = []
  
  assert_eq(numbers.length(), 5)
  assert_eq(empty_array.length(), 0)
  
  let first = numbers[0]
  assert_eq(first, 1)
  
  let last = numbers[4]
  assert_eq(last, 5)
}

///|
test "boolean operations test" {
  let true_val = true
  let false_val = false
  
  assert_eq(true_val && false_val, false)
  assert_eq(true_val || false_val, true)
  
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 >= 5, true)
  assert_eq(3 <= 5, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

///|
test "result type test" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("Error message")
  
  assert_true(ok_value is Ok(_))
  assert_false(ok_value is Err(_))
  assert_false(err_value is Ok(_))
  assert_true(err_value is Err(_))
  
  assert_eq(ok_value.unwrap_or(0), 42)
  assert_eq(err_value.unwrap_or(0), 0)
}

///|
test "array functional operations test" {
  let numbers = [1, 2, 3, 4, 5]
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
}

///|
test "type conversion test" {
  let int_val = 42
  
  let int_to_string = int_val.to_string()
  assert_eq(int_to_string, "42")
  
  let char_code = 'A'.to_int()
  assert_eq(char_code, 65)
}