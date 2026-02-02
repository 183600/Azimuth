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
test "result is_ok and is_err functionality" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  let is_ok = fn(x : Result[Int, String]) -> Bool {
    match x {
      Ok(_) => true
      Err(_) => false
    }
  }
  
  let is_err = fn(x : Result[Int, String]) -> Bool {
    match x {
      Ok(_) => false
      Err(_) => true
    }
  }
  
  assert_true(is_ok(ok_result))
  assert_false(is_err(ok_result))
  assert_false(is_ok(err_result))
  assert_true(is_err(err_result))
}

///|
test "result contains functionality" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  let contains = fn(x : Result[Int, String], value : Int) -> Bool {
    match x {
      Ok(v) => v == value
      Err(_) => false
    }
  }
  
  let contains_err = fn(x : Result[Int, String], error : String) -> Bool {
    match x {
      Ok(_) => false
      Err(e) => e == error
    }
  }
  
  assert_true(contains(ok_result, 42))
  assert_false(contains(ok_result, 100))
  assert_false(contains(err_result, 42))
  
  assert_true(contains_err(err_result, "error message"))
  assert_false(contains_err(err_result, "different error"))
  assert_false(contains_err(ok_result, "error message"))
}

///|
test "result map_err functionality" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  let map_err = fn(x : Result[Int, String], f : String -> String) -> Result[Int, String] {
    match x {
      Ok(v) => Ok(v)
      Err(e) => Err(f(e))
    }
  }
  
  let mapped_ok = map_err(ok_result, s => "prefix: " + s)
  let mapped_err = map_err(err_result, s => "prefix: " + s)
  
  assert_eq(mapped_ok, Ok(42))
  assert_eq(mapped_err, Err("prefix: error message"))
}