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

// Standard MoonBit test cases for core library functionality

///|
test "Int basic arithmetic operations" {
  let a = 10
  let b = 3
  let sum = a + b
  let diff = a - b
  let prod = a * b
  let quot = a / b
  let rem = a % b
}

///|
test "String concatenation" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
}

///|
test "Option creation" {
  let some_val : Int? = Some(42)
  let none_val : Int? = None
}

///|
test "Result creation" {
  let ok_val : Result[Int, String] = Ok(42)
  let err_val : Result[Int, String] = Err("error")
}

///|
test "Option pattern matching" {
  let some_val : Int? = Some(42)
  let result = match some_val {
    Some(x) => x * 2
    None => 0
  }
}

///|
test "Result pattern matching" {
  let ok_val : Result[Int, String] = Ok(42)
  let result = match ok_val {
    Ok(x) => x * 2
    Err(_) => 0
  }
}

///|
test "Boolean operations" {
  let t = true
  let f = false
  let and_result = t && f
  let or_result = t || f
  let not_result = !f
}

///|
test "Tuple operations" {
  let tuple = (1, "hello", true)
  let first = tuple.0
  let second = tuple.1
  let third = tuple.2
}

///|
test "Unit type" {
  let unit_value : Unit = ()
}

///|
test "Character operations" {
  let char_a = 'a'
  let char_b = 'b'
  let string_from_char = char_a.to_string()
}