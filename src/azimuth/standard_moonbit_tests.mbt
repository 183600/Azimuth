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
test "array basic operations" {
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
}

///|
test "string concatenation" {
  let s1 = "Hello"
  let s2 = "World"
  inspect(s1 + " " + s2, content="Hello World")
  inspect("abc".length(), content="3")
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
}

///|
test "result basic operations" {
  let success : Result[Int, String] = Ok(42)
  let error : Result[Int, String] = Err("Error")
  
  inspect(success.is_ok(), content="true")
  inspect(success.is_err(), content="false")
  inspect(error.is_ok(), content="false")
  inspect(error.is_err(), content="true")
}

///|
test "int arithmetic operations" {
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

///|
test "int comparison operations" {
  inspect(10 > 5, content="true")
  inspect(10 < 5, content="false")
  inspect(10 >= 10, content="true")
  inspect(10 <= 5, content="false")
  inspect(10 == 10, content="true")
  inspect(10 != 5, content="true")
}

///|
test "int bit operations" {
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(~10, content="-11")
  inspect(10 << 2, content="40")
  inspect(10 >> 2, content="2")
}

///|
test "boolean operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "char operations" {
  let char_a = 'a'
  let char_A = 'A'
  
  inspect(char_a.to_string(), content="\"a\"")
  inspect(char_A.to_string(), content="\"A\"")
}

///|
test "int overflow behavior" {
  inspect(2147483647 + 1, content="-2147483648")
  inspect(-2147483648 - 1, content="2147483647")
}