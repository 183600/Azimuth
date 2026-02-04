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
test "boolean logic operations" {
  // Test basic boolean operations
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
  
  // Test complex boolean expressions
  let a = true
  let b = false
  let c = true
  inspect(a && b || c, content="true")
  inspect(a || b && c, content="true")
}

///|
test "integer arithmetic operations" {
  // Test basic arithmetic
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
  
  // Test boundary values
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

///|
test "string basic operations" {
  // Test string concatenation
  let hello = "Hello"
  let world = "World"
  let result = hello + ", " + world + "!"
  inspect(result, content="Hello, World!")
  
  // Test string length and access
  let text = "MoonBit"
  inspect(text.length(), content="7")
  inspect(text[0], content="'M'")
  inspect(text[6], content="'t'")
}

///|
test "list fundamental operations" {
  // Test list creation and basic operations
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  inspect(numbers.length(), content="5")
  inspect(numbers.head(), content="Some(1)")
  inspect(numbers.last(), content="Some(5)")
  
  // Test list transformations
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // Test list filtering
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
}

///|
test "option type operations" {
  // Test option creation and pattern matching
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // Test option operations
  inspect(some_value.unwrap(), content="42")
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

///|
test "result type operations" {
  // Test result creation and operations
  let success = Ok("success")
  let error = Err("error")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(error.is_ok(), content="false")
  inspect(error.is_err(), content="true")
  
  // Test result unwrapping
  inspect(success.unwrap(), content="success")
  inspect(success.unwrap_or("default"), content="success")
  inspect(error.unwrap_or("default"), content="default")
}

///|
test "array operations" {
  // Test array creation and access
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
  
  // Test array slicing
  let slice = arr.slice(1, 4)
  inspect(slice, content="[2, 3, 4]")
  
  // Test array transformations
  let doubled = arr.map(x => x * 2)
  inspect(doubled, content="[2, 4, 6, 8, 10]")
}

///|
test "character operations" {
  // Test character creation and comparison
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_A.to_string(), content="\"A\"")
  
  // Test character comparisons
  inspect(char_a < char_z, content="true")
  inspect(char_A < char_a, content="true")
  inspect(char_a == char_a, content="true")
  inspect(char_a != char_A, content="true")
}

///|
test "comparison and equality operations" {
  // Test numeric comparisons
  inspect(10 > 5, content="true")
  inspect(10 < 20, content="true")
  inspect(10 >= 10, content="true")
  inspect(10 <= 10, content="true")
  inspect(10 == 10, content="true")
  inspect(10 != 5, content="true")
  
  // Test string comparisons
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  
  inspect(str1 == str3, content="true")
  inspect(str1 != str2, content="true")
  inspect(str1 < str2, content="true")
  inspect(str2 > str1, content="true")
}

///|
test "type conversion operations" {
  // Test numeric type conversions
  let int_val = 42
  inspect(int_val.to_int64(), content="42")
  inspect(int_val.to_uint(), content="42")
  inspect(int_val.to_uint64(), content="42")
  inspect(int_val.to_uint16(), content="42")
  inspect(int_val.to_int16(), content="42")
  
  // Test boolean to int conversion
  inspect(true.to_int(), content="1")
  inspect(false.to_int(), content="0")
  
  // Test int to boolean conversion (non-zero is true)
  inspect((1 != 0), content="true")
  inspect((0 != 0), content="false")
}