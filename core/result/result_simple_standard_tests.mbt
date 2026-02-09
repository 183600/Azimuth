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
test "result basic creation and inspection" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error message")
  
  inspect(ok_value.is_ok(), content="true")
  inspect(ok_value.is_err(), content="false")
  inspect(err_value.is_ok(), content="false")
  inspect(err_value.is_err(), content="true")
}

///|
test "result unwrap operations" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  inspect(ok_value.unwrap(), content="42")
  inspect(ok_value.unwrap_or(0), content="42")
  inspect(err_value.unwrap_or(0), content="0")
  
  inspect(err_value.unwrap_err(), content="error")
}

///|
test "result map operation" {
  let ok_value : Result[Int, String] = Ok(21)
  let err_value : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_value.map(x => x * 2)
  inspect(mapped_ok, content="Ok(42)")
  
  let mapped_err = err_value.map(x => x * 2)
  inspect(mapped_err, content="Err(\"error\")")
}

///|
test "result map_err operation" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  let mapped_ok_err = ok_value.map_err(e => e + " (mapped)")
  inspect(mapped_ok_err, content="Ok(42)")
  
  let mapped_err_err = err_value.map_err(e => e + " (mapped)")
  inspect(mapped_err_err, content="Err(\"error (mapped)\")")
}

///|
test "result and_then operation" {
  let ok_value : Result[Int, String] = Ok(10)
  let err_value : Result[Int, String] = Err("initial error")
  
  let safe_divide = (x : Int) => if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  
  let result_ok = ok_value.and_then(safe_divide)
  inspect(result_ok, content="Ok(10)")
  
  let result_err = err_value.and_then(safe_divide)
  inspect(result_err, content="Err(\"initial error\")")
  
  let zero_value : Result[Int, String] = Ok(0)
  let result_zero = zero_value.and_then(safe_divide)
  inspect(result_zero, content="Err(\"division by zero\")")
}

///|
test "result or_else operation" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("first error")
  
  let fallback = () => Ok(99)
  
  let result_ok = ok_value.or_else(fallback)
  inspect(result_ok, content="Ok(42)")
  
  let result_err = err_value.or_else(fallback)
  inspect(result_err, content="Ok(99)")
}

///|
test "result equality" {
  let ok1 : Result[Int, String] = Ok(5)
  let ok2 : Result[Int, String] = Ok(5)
  let ok3 : Result[Int, String] = Ok(10)
  let err1 : Result[Int, String] = Err("error")
  let err2 : Result[Int, String] = Err("error")
  let err3 : Result[Int, String] = Err("different error")
  
  inspect(ok1, content="Ok(5)")
  inspect(ok2, content="Ok(5)")
  inspect(ok3, content="Ok(10)")
  inspect(err1, content="Err(\"error\")")
  inspect(err2, content="Err(\"error\")")
  inspect(err3, content="Err(\"different error\")")
}

///|
test "result to_option conversion" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error")
  
  let option_from_ok = ok_value.to_option()
  inspect(option_from_ok, content="Some(42)")
  
  let option_from_err = err_value.to_option()
  inspect(option_from_err, content="None")
}

///|
test "result from_option conversion" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let result_from_some = some_value.to_result(() => "no value")
  inspect(result_from_some, content="Ok(42)")
  
  let result_from_none = none_value.to_result(() => "no value")
  inspect(result_from_none, content="Err(\"no value\")")
}