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
test "integer arithmetic operations" {
  let a = 10
  let b = 3
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
  assert_eq(-a, -10)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(5), 5)
}

///|
test "integer comparison operations" {
  assert_true(5 > 3)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
  assert_false(5 > 10)
  assert_false(5 < 3)
}

///|
test "integer bit operations" {
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  assert_eq(a & b, 8)   // 1000
  assert_eq(a | b, 14)  // 1110
  assert_eq(a ^ b, 6)   // 0110
  assert_eq(a << 1, 24)
  assert_eq(a >> 1, 6)
}

///|
test "string basic operations" {
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  assert_eq(combined, "Hello World")
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 5)
  assert_eq(combined.length(), 11)
  assert_true(s1.contains("ell"))
  assert_false(s1.contains("xyz"))
}

test "string indexing and slicing" {
  let s = "Hello World"
  assert_eq(s.length(), 11)
  assert_eq(s[0], 'H')
  assert_eq(s[10], 'd')
  assert_true(s.starts_with("Hello"))
  assert_true(s.ends_with("World"))
  assert_false(s.starts_with("World"))
  assert_false(s.ends_with("Hello"))
}

///|
test "option type operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  assert_eq(some_value.unwrap(), 42)
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
  
  let doubled = some_value.map(x => x * 2)
  assert_eq(doubled, Some(84))
  
  let filtered = some_value.filter(x => x > 40)
  assert_eq(filtered, Some(42))
  
  let filtered_out = some_value.filter(x => x > 50)
  assert_eq(filtered_out, None)
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  assert_eq(ok_result.is_ok(), true)
  assert_eq(ok_result.is_err(), false)
  assert_eq(err_result.is_ok(), false)
  assert_eq(err_result.is_err(), true)
  
  assert_eq(ok_result.unwrap(), 42)
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
  
  let mapped = ok_result.map(x => x * 2)
  assert_eq(mapped, Ok(84))
  
  let bound = ok_result.bind(x => Ok(x.to_string()))
  assert_eq(bound, Ok("42"))
}

///|
test "boolean operations" {
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let doubled = @array.map(arr, x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = @array.filter(arr, x => x % 2 == 0)
  assert_eq(evens, [2, 4])
  
  let sum = @array.reduce(arr, 0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "type conversion operations" {
  let int_val = 42
  let string_val = "42"
  
  assert_eq(int_val.to_string(), "42")
  assert_eq(string_val.to_int(), Some(42))
  assert_eq("invalid".to_int(), None)
  
  let float_val = 3.14
  assert_eq(float_val.to_int(), 3)
  assert_eq(int_val.to_float(), 42.0)
}