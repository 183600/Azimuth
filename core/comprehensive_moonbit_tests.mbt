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
test "string basic operations" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  
  inspect(s3, content="Hello World")
  inspect(s1, content="Hello")
  inspect(s2, content="World")
}

///|
test "integer arithmetic operations" {
  let a = 100
  let b = 25
  
  inspect(a + b, content="125")
  inspect(a - b, content="75")
  inspect(a * b, content="2500")
  inspect(a / b, content="4")
  inspect(a % b, content="0")
  
  // Test negative numbers
  let neg_a = -50
  let neg_b = -10
  inspect(neg_a + neg_b, content="-60")
  inspect(neg_a * neg_b, content="500")
  inspect(neg_a / neg_b, content="5")
}

///|
test "boolean logic operations" {
  let t = true
  let f = false
  
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(t && t, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
  
  // Complex boolean expressions
  inspect((t && f) || (t || f), content="true")
  inspect((t && f) && (t || f), content="false")
}

///|
test "array basic operations" {
  let arr = [1, 2, 3, 4, 5]
  
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
  
  let empty_arr : Array[Int] = []
  inspect(empty_arr.length(), content="0")
}

///|
test "option type basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  inspect(some_value.unwrap(), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

///|
test "character basic operations" {
  let char_a = 'a'
  let char_A = 'A'
  let char_zero = '0'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  inspect(char_zero.to_string(), content="0")
  
  inspect(char_a.to_byte(), content="97")
  inspect(char_A.to_byte(), content="65")
  inspect(char_zero.to_byte(), content="48")
}

///|
test "tuple operations" {
  let point = (3, 4)
  let person = ("Alice", 25)
  
  // Access tuple elements
  inspect(point.0, content="3")
  inspect(point.1, content="4")
  inspect(person.0, content="Alice")
  inspect(person.1, content="25")
  
  // Tuple comparison
  let same_point = (3, 4)
  let different_point = (4, 3)
  inspect(point == same_point, content="true")
  inspect(point == different_point, content="false")
}

///|
test "result type basic operations" {
  let success : Result[String, String] = Ok("Success")
  let failure : Result[String, String] = Err("Error")
  
  // Check result type
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(failure.is_ok(), content="false")
  inspect(failure.is_err(), content="true")
  
  // Unwrap operations
  inspect(success.unwrap(), content="Success")
  inspect(failure.unwrap_err(), content="Error")
  inspect(success.unwrap_or("Default"), content="Success")
  inspect(failure.unwrap_or("Default"), content="Default")
}

///|
test "integer boundary values" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  
  // Test overflow behavior
  let overflow_val = max_int + 1
  inspect(overflow_val, content="-2147483648")
}

///|
test "string length and comparison" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = "Hello"
  
  inspect(s1.length(), content="5")
  inspect(s2.length(), content="5")
  inspect("".length(), content="0")
  inspect("A".length(), content="1")
  
  inspect(s1 == s3, content="true")
  inspect(s1 == s2, content="false")
  inspect(s1 != s2, content="true")
}