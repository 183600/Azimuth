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
test "result basic creation" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  assert_eq(ok_result.is_ok(), true)
  assert_eq(ok_result.is_err(), false)
  assert_eq(err_result.is_ok(), false)
  assert_eq(err_result.is_err(), true)
}

///|
test "result unwrap" {
  let ok_result : Result[Int, String] = Ok(42)
  assert_eq(ok_result.unwrap(), 42)
}

///|
test "result unwrap_or" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
}

///|
test "result map" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  let mapped_err = err_result.map(x => x * 2)
  
  assert_eq(mapped_ok, Ok(84))
  assert_eq(mapped_err, Err("error"))
}

///|
test "result map_err" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok_err = ok_result.map_err(s => s + "!")
  let mapped_err_err = err_result.map_err(s => s + "!")
  
  assert_eq(mapped_ok_err, Ok(42))
  assert_eq(mapped_err_err, Err("error!"))
}

///|
test "result and_then" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  let and_then_ok = ok_result.and_then(x => if x > 40 { Ok(x * 2) } else { Err("too small") })
  let and_then_err = err_result.and_then(x => Ok(x * 2))
  
  assert_eq(and_then_ok, Ok(84))
  assert_eq(and_then_err, Err("error"))
}

///|
test "result or_else" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  let alternative : Result[Int, String] = Ok(99)
  
  assert_eq(ok_result.or_else(|| alternative), Ok(42))
  assert_eq(err_result.or_else(|| alternative), Ok(99))
}

///|
test "result default" {
  let default_result : Result[Int, String] = Result::default()
  assert_eq(default_result, Err("default error"))
}