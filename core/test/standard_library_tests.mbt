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

// Import necessary functions
using @builtin { inspect, assert_eq }

///|
test "basic_arithmetic_operations" {
  let a = 10
  let b = 3
  inspect(a + b, content="13")
  inspect(a - b, content="7")
  inspect(a * b, content="30")
  inspect(a / b, content="3")
  inspect(a % b, content="1")
}

///|
test "string_operations" {
  let s = "Hello"
  inspect(s.length(), content="5")
  inspect(s[0], content="'H'")
  
  let s2 = " World"
  let combined = s + s2
  inspect(combined, content="\"Hello World\"")
}

///|
test "boolean_logic" {
  let t = true
  let f = false
  inspect(t && t, content="true")
  inspect(t || f, content="true")
  inspect(!f, content="true")
  inspect(t != f, content="true")
  inspect(t == f, content="false")
}

///|
test "array_operations" {
  let arr = [1, 2, 3]
  inspect(arr.length(), content="3")
  inspect(arr[0], content="1")
  inspect(arr[2], content="3")
  
  let arr2 = [4, 5, 6]
  let combined = arr + arr2
  inspect(combined, content="[1, 2, 3, 4, 5, 6]")
}

///|
test "comparison_operations" {
  let a = 5
  let b = 10
  inspect(a < b, content="true")
  inspect(a > b, content="false")
  inspect(a <= b, content="true")
  inspect(a >= b, content="false")
  inspect(a == a, content="true")
  inspect(a != b, content="true")
}

///|
test "negative_numbers" {
  let neg = -5
  let pos = 5
  inspect(neg + pos, content="0")
  inspect(neg * pos, content="-25")
  inspect(-neg, content="5")
}

///|
test "character_operations" {
  let c = 'A'
  inspect(c, content="'A'")
  
  let c2 = 'B'
  inspect(c2, content="'B'")
  
  // String from character
  let s = c.to_string()
  inspect(s, content="\"A\"")
}

///|
test "empty_collections" {
  let empty_arr : Array[Int] = []
  inspect(empty_arr.length(), content="0")
  
  let empty_str = ""
  inspect(empty_str.length(), content="0")
}

///|
test "type_coercion" {
  let i = 42
  let s = i.to_string()
  inspect(s, content="\"42\"")
}

///|
test "nested_expressions" {
  let result = (1 + 2) * (3 - 1) / 2
  inspect(result, content="3")
  
  let bool_result = (true && false) || (true || false)
  inspect(bool_result, content="true")
}