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
test "int basic arithmetic operations" {
  inspect(1 + 2, content="3")
  inspect(10 - 5, content="5")
  inspect(3 * 4, content="12")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  inspect(-5 + 3, content="-2")
  inspect(-10 - (-3), content="-7")
}

///|
test "string concatenation and length" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
  inspect(hello.length(), content="5")
  inspect(combined.length(), content="11")
  inspect("".length(), content="0")
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  inspect(some_value.is_some(), content="true")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_some(), content="false")
  inspect(none_value.is_none(), content="true")
  
  inspect(some_value.unwrap_or(0), content="42")
  inspect(none_value.unwrap_or(0), content="0")
}

///|
test "array basic operations" {
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
  
  let empty_arr : Array[Int] = []
  inspect(empty_arr.length(), content="0")
  
  let new_arr = arr.push(6)
  inspect(new_arr.length(), content="6")
  inspect(new_arr[5], content="6")
}

///|
test "boolean logical operations" {
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  
  inspect(!true, content="false")
  inspect(!false, content="true")
}

///|
test "char operations and conversions" {
  let char_a = 'a'
  let char_A = 'A'
  let char_1 = '1'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  inspect(char_1.to_string(), content="1")
  
  inspect(char_a.to_int(), content="97")
  inspect(char_A.to_int(), content="65")
  inspect(char_1.to_int(), content="49")
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
  
  inspect(ok_result.unwrap_or(0), content="42")
  inspect(err_result.unwrap_or(0), content="0")
}

///|
test "tuple operations" {
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  
  inspect(pair.0, content="10")
  inspect(pair.1, content="hello")
  inspect(triple.0, content="1")
  inspect(triple.1, content="2")
  inspect(triple.2, content="3")
}

///|
test "type conversion operations" {
  let int_val = 42
  let uint_val : UInt = 42
  let string_val = "123"
  
  inspect(int_val.to_string(), content="42")
  inspect(uint_val.to_string(), content="42")
  inspect(int_val.to_uint(), content="42")
  inspect(uint_val.to_int(), content="42")
}