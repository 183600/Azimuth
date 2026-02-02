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
test "result utility functions" {
  // Test is_ok function
  let is_ok = fn(x : Result[Int, String]) -> Bool {
    match x {
      Ok(_) => true
      Err(_) => false
    }
  }
  
  // Test is_err function
  let is_err = fn(x : Result[Int, String]) -> Bool {
    match x {
      Ok(_) => false
      Err(_) => true
    }
  }
  
  // Test contains function
  let contains = fn(x : Result[Int, String], value : Int) -> Bool {
    match x {
      Ok(v) => v == value
      Err(_) => false
    }
  }
  
  // Test map_err function
  let map_err = fn(x : Result[Int, String], f : (String) -> String) -> Result[Int, String] {
    match x {
      Ok(v) => Ok(v)
      Err(e) => Err(f(e))
    }
  }
  
  // Test the functions with various inputs
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  // Function calls to verify they work
  let _ = is_ok(ok_result)
  let _ = is_err(ok_result)
  let _ = is_ok(err_result)
  let _ = is_err(err_result)
  
  let _ = contains(ok_result, 42)
  let _ = contains(ok_result, 100)
  let _ = contains(err_result, 42)
  
  let _ = map_err(ok_result, s => "prefix: " + s)
  let _ = map_err(err_result, s => "prefix: " + s)
}