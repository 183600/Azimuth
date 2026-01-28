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
test "int arithmetic operations" {
  assert_eq(10 + 5, 15)
  assert_eq(10 - 5, 5)
  assert_eq(10 * 5, 50)
  assert_eq(10 / 3, 3)
  assert_eq(10 % 3, 1)
  assert_eq(-10 + 5, -5)
  assert_eq(-10 * -2, 20)
}

///|
test "int comparison operations" {
  assert_true(10 > 5)
  assert_false(10 < 5)
  assert_true(10 >= 10)
  assert_false(10 <= 5)
  assert_true(10 == 10)
  assert_true(10 != 5)
}

///|
test "int boundary values" {
  assert_eq(@int.max_value, 2147483647)
  assert_eq(@int.min_value, -2147483648)
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
}

///|
test "string operations" {
  let s1 = "Hello"
  let s2 = "MoonBit"
  let result = s1 + " " + s2
  assert_eq(result, "Hello MoonBit")
  
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 7)
  assert_true(s1 == "Hello")
  assert_false(s1 == s2)
}

///|
test "boolean operations" {
  let t = true
  let f = false
  
  assert_true(t && t)
  assert_false(t && f)
  assert_false(f && t)
  assert_false(f && f)
  
  assert_true(t || t)
  assert_true(t || f)
  assert_true(f || t)
  assert_false(f || f)
  
  assert_true(!f)
  assert_false(!t)
}

///|
test "option basic operations" {
  let some_value : Int = Some(42)
  let none_value : Int = None
  
  assert_true(some_value.is_some())
  assert_false(some_value.is_none())
  assert_false(none_value.is_some())
  assert_true(none_value.is_none())
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "option transformations" {
  let some_value : Int = Some(42)
  let none_value : Int = None
  
  let mapped_some = some_value.map(fn(x) { x * 2 })
  let mapped_none = none_value.map(fn(x) { x * 2 })
  
  assert_eq(mapped_some, Some(84))
  assert_eq(mapped_none, None)
}

///|
test "result basic operations" {
  let ok_value : String = Ok("success")
  let err_value : String = Err("error")
  
  assert_true(ok_value.is_ok())
  assert_false(ok_value.is_err())
  assert_false(err_value.is_ok())
  assert_true(err_value.is_err())
  
  assert_eq(ok_value.unwrap_or("default"), "success")
  assert_eq(err_value.unwrap_or("default"), "default")
}

///|
test "result transformations" {
  let ok_result : Int = Ok(42)
  let err_result : Int = Err("error")
  
  let mapped_ok = ok_result.map(fn(x) { x * 2 })
  let mapped_err = err_result.map(fn(x) { x * 2 })
  
  assert_eq(mapped_ok, Ok(84))
  assert_eq(mapped_err, Err("error"))
}

///|
test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let arr2 = [6, 7, 8]
  let combined = arr.concat(arr2)
  assert_eq(combined.length(), 8)
  assert_eq(combined[5], 6)
}