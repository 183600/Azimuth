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
test "result basic operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error occurred")
  
  inspect(ok_result is Ok(_), content="true")
  inspect(ok_result is Err(_), content="false")
  inspect(err_result is Ok(_), content="false")
  inspect(err_result is Err(_), content="true")
}

///|
test "result unwrap operations" {
  let ok_result : Result[Int, String] = Ok(100)
  let err_result : Result[Int, String] = Err("failure")
  
  inspect(ok_result.unwrap(), content="100")
  inspect(ok_result.unwrap_or(0), content="100")
  inspect(ok_result.unwrap_or_else(() => 999), content="100")
  
  inspect(err_result.unwrap_or(0), content="0")
  inspect(err_result.unwrap_or_else(() => 999), content="999")
}

///|
test "result map operations" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(10)")
  
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(\"error\")")
  
  let mapped_or_ok = ok_result.map_or(0, x => x * 3)
  inspect(mapped_or_ok, content="15")
  
  let mapped_or_err = err_result.map_or(0, x => x * 3)
  inspect(mapped_or_err, content="0")
}

///|
test "result bind/and_then operations" {
  let ok_result : Result[Int, String] = Ok(10)
  let err_result : Result[Int, String] = Err("initial error")
  
  let safe_divide = (x : Int) => if x = 0 { Err("division by zero") } else { Ok(100 / x) }
  
  let bind_ok = ok_result.and_then(safe_divide)
  inspect(bind_ok, content="Ok(10)")
  
  let bind_err = err_result.and_then(safe_divide)
  inspect(bind_err, content="Err(\"initial error\")")
  
  let bind_zero = Ok(0).and_then(safe_divide)
  inspect(bind_zero, content="Err(\"division by zero\")")
}

///|
test "result equality and comparison" {
  let ok1 : Result[Int, String] = Ok(42)
  let ok2 : Result[Int, String] = Ok(42)
  let ok3 : Result[Int, String] = Ok(100)
  let err1 : Result[Int, String] = Err("error")
  let err2 : Result[Int, String] = Err("error")
  let err3 : Result[Int, String] = Err("different")
  
  inspect(ok1 = ok2, content="true")
  inspect(ok1 = ok3, content="false")
  inspect(ok1 = err1, content="false")
  
  inspect(err1 = err2, content="true")
  inspect(err1 = err3, content="false")
  inspect(err1 = ok1, content="false")
}

///|
test "result pattern matching" {
  let ok_result : Result[Int, String] = Ok(200)
  let err_result : Result[Int, String] = Err("not found")
  
  let ok_message = match ok_result {
    Ok(value) => "Success: " + value.to_string()
    Err(error) => "Error: " + error
  }
  inspect(ok_message, content="Success: 200")
  
  let err_message = match err_result {
    Ok(value) => "Success: " + value.to_string()
    Err(error) => "Error: " + error
  }
  inspect(err_message, content="Error: not found")
}