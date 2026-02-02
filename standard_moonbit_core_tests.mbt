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
test "int basic arithmetic" {
  assert_eq(1 + 2, 3)
  assert_eq(10 - 5, 5)
  assert_eq(3 * 4, 12)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
}

///|
test "int comparisons" {
  assert_eq(10 > 5, true)
  assert_eq(3 < 8, true)
  assert_eq(7 >= 7, true)
  assert_eq(9 <= 4, false)
  assert_eq(6 == 6, true)
  assert_eq(3 != 3, false)
}

///|
test "string operations" {
  let s1 = "hello"
  let s2 = "world"
  let combined = s1 + " " + s2
  assert_eq(combined, "hello world")
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 5)
}

///|
test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let empty = []
  assert_eq(empty.length(), 0)
}

///|
test "boolean operations" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false && true, false)
  assert_eq(false && false, false)
  
  assert_eq(true || true, true)
  assert_eq(true || false, true)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "option type" {
  let some_val = Some(42)
  let none_val = None
  
  assert_eq(some_val.is_some(), true)
  assert_eq(some_val.is_none(), false)
  assert_eq(none_val.is_some(), false)
  assert_eq(none_val.is_none(), true)
  
  assert_eq(some_val.unwrap_or(0), 42)
  assert_eq(none_val.unwrap_or(0), 0)
}

///|
test "result type" {
  let ok_val = Ok(42)
  let err_val = Err("error")
  
  assert_eq(ok_val.is_ok(), true)
  assert_eq(ok_val.is_err(), false)
  assert_eq(err_val.is_ok(), false)
  assert_eq(err_val.is_err(), true)
  
  assert_eq(ok_val.unwrap_or(0), 42)
  assert_eq(err_val.unwrap_or(0), 0)
}

///|
test "int edge cases" {
  assert_eq(0 + 5, 5)
  assert_eq(0 - 5, -5)
  assert_eq(0 * 5, 0)
  assert_eq(-5 + 3, -2)
  assert_eq(-5 * -3, 15)
}

///|
test "string edge cases" {
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty.is_empty(), true)
  
  let single = "a"
  assert_eq(single.length(), 1)
  assert_eq(single.is_empty(), false)
}

///|
test "array edge cases" {
  let single = [42]
  assert_eq(single.length(), 1)
  assert_eq(single[0], 42)
}