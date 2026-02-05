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
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
}

///|
test "option map transformation" {
  let some_string : String? = Some("hello")
  let none_string : String? = None
  
  let upper_some = some_string.map(s => s.to_upper())
  let upper_none = none_string.map(s => s.to_upper())
  
  assert_eq(upper_some, Some("HELLO"))
  assert_eq(upper_none, None)
}

///|
test "option filter predicate" {
  let numbers = [Some(1), Some(2), None, Some(4), Some(5)]
  let filtered = numbers.map(opt => opt.filter(n => n % 2 == 0))
  
  assert_eq(filtered[0], None)
  assert_eq(filtered[1], Some(2))
  assert_eq(filtered[2], None)
  assert_eq(filtered[3], Some(4))
  assert_eq(filtered[4], None)
}

///|
test "result basic operations" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  assert_eq(ok_value.is_ok(), true)
  assert_eq(ok_value.is_err(), false)
  assert_eq(err_value.is_ok(), false)
  assert_eq(err_value.is_err(), true)
}

///|
test "result map transformation" {
  let ok_string : Result[String, String] = Ok("hello")
  let err_string : Result[String, String] = Err("error")
  
  let length_ok = ok_string.map(s => s.length())
  let length_err = err_string.map(s => s.length())
  
  assert_eq(length_ok, Ok(5))
  assert_eq(length_err, Err("error"))
}

///|
test "result bind chaining" {
  let safe_divide = (x : Int) => if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  
  let result1 = Ok(4).bind(safe_divide)
  let result2 = Ok(0).bind(safe_divide)
  let result3 : Result[Int, String] = Err("initial error").bind(safe_divide)
  
  assert_eq(result1, Ok(25))
  assert_eq(result2, Err("division by zero"))
  assert_eq(result3, Err("initial error"))
}

///|
test "result unwrap operations" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  assert_eq(ok_value.unwrap_or(0), 42)
  assert_eq(err_value.unwrap_or(0), 0)
  assert_eq(ok_value.unwrap_or_else(() => 100), 42)
  assert_eq(err_value.unwrap_or_else(() => 100), 100)
}

///|
test "result to_option conversion" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  assert_eq(ok_value.to_option(), Some(42))
  assert_eq(err_value.to_option(), None)
}

///|
test "combined option and result operations" {
  let parse_int = (s : String) => {
    match s.to_int() {
      Some(n) => Ok(n)
      None => Err("invalid integer")
    }
  }
  
  let result1 = parse_int("42")
  let result2 = parse_int("invalid")
  
  assert_eq(result1.map(n => n * 2), Ok(84))
  assert_eq(result2.map(n => n * 2), Err("invalid integer"))
}

///|
test "nested option operations" {
  let nested_options : Array[Int?> = [Some(1), None, Some(3)]
  let flattened = nested_options.filter_map(opt => opt)
  
  assert_eq(flattened.length(), 2)
  assert_eq(flattened[0], 1)
  assert_eq(flattened[1], 3)
}