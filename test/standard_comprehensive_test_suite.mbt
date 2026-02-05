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
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test unwrap_or
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
  
  // Test map
  inspect(some_value.map(x => x * 2), content="Some(84)")
  inspect(none_value.map(x => x * 2), content="None")
  
  // Test is_some and is_none
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "list fundamental operations" {
  let list1 = @list.from_array([1, 2, 3, 4, 5])
  let list2 = @list.from_array([6, 7, 8])
  
  // Test length
  inspect(list1.length(), content="5")
  inspect(list2.length(), content="3")
  
  // Test head and tail
  inspect(list1.head(), content="Some(1)")
  inspect(list1.last(), content="Some(5)")
  
  // Test concat
  let combined = list1.concat(list2)
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6, 7, 8])")
  
  // Test reverse
  let reversed = list1.rev()
  inspect(reversed, content="@list.from_array([5, 4, 3, 2, 1])")
}

///|
test "int boundary values" {
  // Test max and min values
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  
  // Test absolute value
  inspect(Int::abs(42), content="42")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
  
  // Test arithmetic operations
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 5, content="2")
  inspect(10 % 3, content="1")
}

///|
test "string basic operations" {
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + " " + str2
  
  // Test concatenation
  inspect(combined, content="Hello World")
  
  // Test length
  inspect(str1.length(), content="5")
  inspect(combined.length(), content="11")
  
  // Test empty string
  let empty_str = ""
  inspect(empty_str.length(), content="0")
  inspect(empty_str.is_empty(), content="true")
  
  // Test string comparison
  inspect(str1 == "Hello", content="true")
  inspect(str1 == str2, content="false")
}

///|
test "array operations" {
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [6, 7, 8]
  
  // Test array length
  inspect(arr1.length(), content="5")
  inspect(arr2.length(), content="3")
  
  // Test array access
  inspect(arr1[0], content="1")
  inspect(arr1[4], content="5")
  
  // Test array push
  let mutable_arr = [1, 2, 3]
  mutable_arr.push(4)
  inspect(mutable_arr.length(), content="4")
  inspect(mutable_arr[3], content="4")
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error message")
  
  // Test is_ok and is_err
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  // Test unwrap_or
  inspect(ok_result.unwrap_or(0), content="42")
  inspect(err_result.unwrap_or(0), content="0")
  
  // Test map
  inspect(ok_result.map(x => x * 2), content="Ok(84)")
  inspect(err_result.map(x => x * 2), content="Err(\"Error message\")")
}

///|
test "char type operations" {
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  
  // Test character comparison
  inspect(char_a < char_z, content="true")
  inspect(char_a < char_A, content="false")
  
  // Test character equality
  inspect(char_a == 'a', content="true")
  inspect(char_a == char_A, content="false")
  
  // Test character to string conversion
  let str_from_char = char_a.to_string()
  inspect(str_from_char, content="a")
}

///|
test "basic arithmetic operations" {
  // Test addition
  inspect(1 + 2, content="3")
  inspect(-1 + 1, content="0")
  inspect(0 + 0, content="0")
  
  // Test subtraction
  inspect(5 - 3, content="2")
  inspect(1 - 5, content="-4")
  inspect(0 - 0, content="0")
  
  // Test multiplication
  inspect(3 * 4, content="12")
  inspect(-2 * 3, content="-6")
  inspect(0 * 5, content="0")
  
  // Test division
  inspect(10 / 2, content="5")
  inspect(7 / 3, content="2")
  inspect(1 / 1, content="1")
}

///|
test "comparison operations" {
  // Test integer comparisons
  inspect(5 < 10, content="true")
  inspect(10 < 5, content="false")
  inspect(5 <= 5, content="true")
  inspect(10 > 5, content="true")
  inspect(5 > 10, content="false")
  inspect(5 >= 5, content="true")
  
  // Test equality and inequality
  inspect(5 == 5, content="true")
  inspect(5 == 10, content="false")
  inspect(5 != 10, content="true")
  inspect(5 != 5, content="false")
  
  // Test string comparisons
  inspect("apple" < "banana", content="true")
  inspect("zebra" < "apple", content="false")
}

///|
test "boolean operations" {
  // Test boolean values
  inspect(true, content="true")
  inspect(false, content="false")
  
  // Test boolean logic
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  
  // Test boolean negation
  inspect(!false, content="true")
  inspect(!true, content="false")
}