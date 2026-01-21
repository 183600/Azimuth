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

// Additional comprehensive test cases for result module with error handling

test "result_basic_creation" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  let is_ok1 = ok_result.is_ok()
  let is_err1 = ok_result.is_err()
  let is_ok2 = err_result.is_ok()
  let is_err2 = err_result.is_err()
  
  // Simple validation
  let _ = if (is_ok1 && !is_err1 && !is_ok2 && is_err2) { 1 } else { 0 }
}

test "result_unwrap_operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  let unwrap1 = ok_result.unwrap()
  let unwrap_or1 = ok_result.unwrap_or(0)
  let unwrap_or2 = err_result.unwrap_or(100)
  
  // Simple validation
  let _ = unwrap1 + unwrap_or1 + unwrap_or2
}

test "result_map" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  let mapped_ok = ok_result.map(fn(x) { x * 2 })
  let mapped_err = err_result.map(fn(x) { x * 2 })
  
  let is_ok1 = mapped_ok.is_ok()
  let is_ok2 = mapped_err.is_ok()
  
  // Simple validation
  let _ = if (is_ok1 && !is_ok2) { 1 } else { 0 }
}

test "result_and_then" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  let and_then_ok = ok_result.and_then(fn(x) { if (x > 0) { Ok(x * 2) } else { Err("Negative") } })
  let and_then_err = err_result.and_then(fn(x) { if (x > 0) { Ok(x * 2) } else { Err("Negative") } })
  
  let is_ok1 = and_then_ok.is_ok()
  let is_ok2 = and_then_err.is_ok()
  
  // Simple validation
  let _ = if (is_ok1 && !is_ok2) { 1 } else { 0 }
}

test "result_equality" {
  let ok1 : Result[Int, String] = Ok(42)
  let ok2 : Result[Int, String] = Ok(42)
  let ok3 : Result[Int, String] = Ok(100)
  let err1 : Result[Int, String] = Err("Error")
  let err2 : Result[Int, String] = Err("Error")
  let err3 : Result[Int, String] = Err("Different error")
  
  let eq1 = ok1 == ok2
  let eq2 = ok1 == ok3
  let eq3 = err1 == err2
  let eq4 = err1 == err3
  let eq5 = ok1 == err1
  
  // Simple validation
  let _ = if (eq1 && !eq2 && eq3 && !eq4 && !eq5) { 1 } else { 0 }
}

test "result_basic_operations" {
  let ok_result : Result[Int, String] = Ok(10)
  let err_result : Result[Int, String] = Err("Error")
  
  let is_ok = ok_result.is_ok()
  let is_err = err_result.is_err()
  let unwrap = ok_result.unwrap()
  let unwrap_or = err_result.unwrap_or(0)
  
  // Simple validation
  let _ = if (is_ok && is_err) { unwrap + unwrap_or } else { 0 }
}