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
test "basic arithmetic operations" {
  let sum = 1 + 2
  let diff = 5 - 3
  let product = 4 * 3
  let quotient = 10 / 2
  let remainder = 10 % 3
  let abs_neg = Int::abs(-5)
  let abs_zero = Int::abs(0)
  let abs_pos = Int::abs(7)
  
  // Basic verification that operations complete without errors
  let _ = sum + diff + product + quotient + remainder
  let _ = abs_neg + abs_zero + abs_pos
}

///|
test "boolean logic operations" {
  let and_true = true && true
  let and_false = true && false
  let or_true = false || true
  let or_false = false || false
  let not_true = !true
  let not_false = !false
  
  // Basic verification that operations complete without errors
  let _ = and_true && and_false && or_true && or_false && not_true && not_false
}

///|
test "string operations" {
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  let len1 = str1.length()
  let len_combined = combined.length()
  
  // Basic verification that operations complete without errors
  let _ = len1 + len_combined
}

///|
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["hello", "world", "moonbit"]
  
  let len_numbers = numbers.length()
  let len_strings = strings.length()
  let first_num = numbers[0]
  let last_str = strings[2]
  
  // Array transformations
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x % 2 == 0)
  
  // Basic verification that operations complete without errors
  let _ = len_numbers + len_strings + first_num + last_str.length()
  let _ = doubled.length() + evens.length()
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let is_some = some_value is Some(_)
  let is_none = none_value is None
  let unwrapped = some_value.unwrap()
  
  // Basic verification that operations complete without errors
  let _ = is_some && is_none
  let _ = unwrapped
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  // Use pattern matching instead of methods
  let is_ok = match ok_result {
    Ok(_) => true
    Err(_) => false
  }
  let is_err = match err_result {
    Ok(_) => false
    Err(_) => true
  }
  let unwrapped = match ok_result {
    Ok(value) => value
    Err(_) => 0 // default value
  }
  
  // Basic verification that operations complete without errors
  let _ = is_ok && is_err
  let _ = unwrapped
}

///|
test "comparison operations" {
  let less_than = 1 < 2
  let greater_than = 2 > 1
  let less_equal = 3 <= 3
  let greater_equal = 4 >= 4
  let equal = 5 == 5
  let not_equal = 5 != 6
  
  // Basic verification that operations complete without errors
  let _ = less_than && greater_than && less_equal && greater_equal && equal && not_equal
}