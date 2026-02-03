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
test "result basic creation and pattern matching" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result, content="Ok(42)")
  inspect(err_result, content="Err(\"error\")")
  
  let ok_value = match ok_result {
    Ok(value) => value * 2
    Err(_) => 0
  }
  inspect(ok_value, content="84")
  
  let err_value = match err_result {
    Ok(value) => value * 2
    Err(_) => 0
  }
  inspect(err_value, content="0")
}

///|
test "result unwrap operations" {
  let ok_result : Result[Int, String] = Ok(123)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.unwrap(), content="123")
  inspect(ok_result.unwrap_or(0), content="123")
  inspect(err_result.unwrap_or(0), content="0")
  inspect(ok_result.unwrap_or(-1), content="123")
  inspect(err_result.unwrap_or(-1), content="-1")
}

///|
test "result is_ok and is_err" {
  let ok_result : Result[String, Int] = Ok("success")
  let err_result : Result[String, Int] = Err(404)
  
  inspect(ok_result.is_ok(), content="true")
  inspect(ok_result.is_err(), content="false")
  inspect(err_result.is_ok(), content="false")
  inspect(err_result.is_err(), content="true")
}

///|
test "result map operation" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  let mapped_err = err_result.map(x => x * 2)
  
  inspect(mapped_ok, content="Ok(10)")
  inspect(mapped_err, content="Err(\"error\")")
  
  let string_ok = ok_result.map(x => x.to_string())
  inspect(string_ok, content="Ok(\"5\")")
}

///|
test "result map_err operation" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("network error")
  
  let mapped_err_ok = ok_result.map_err(e => "Error: " + e)
  let mapped_err_err = err_result.map_err(e => "Error: " + e)
  
  inspect(mapped_err_ok, content="Ok(42)")
  inspect(mapped_err_err, content="Err(\"Error: network error\")")
}

///|
test "result and_then operation" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  let and_then_ok = ok_result.and_then(x => if x > 3 { Ok(x * 2) } else { Err("too small") })
  let and_then_small = ok_result.and_then(x => if x > 10 { Ok(x * 2) } else { Err("too small") })
  let and_then_err = err_result.and_then(x => Ok(x * 2))
  
  inspect(and_then_ok, content="Ok(10)")
  inspect(and_then_small, content="Err(\"too small\")")
  inspect(and_then_err, content="Err(\"error\")")
}

///|
test "result or_else operation" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("first error")
  let alternative : Result[Int, String] = Ok(99)
  let err_alternative : Result[Int, String] = Err("second error")
  
  inspect(ok_result.or_else(alternative), content="Ok(42)")
  inspect(err_result.or_else(alternative), content="Ok(99)")
  inspect(err_result.or_else(err_alternative), content="Err(\"second error\")")
}

///|
test "result default values" {
  let ok_int : Result[Int, String] = Ok(42)
  let err_int : Result[Int, String] = Err("error")
  
  inspect(ok_int.unwrap_or(0), content="42")
  inspect(err_int.unwrap_or(0), content="0")
  
  let ok_string : Result[String, Int] = Ok("hello")
  let err_string : Result[String, Int] = Err(404)
  
  inspect(ok_string.unwrap_or("default"), content="hello")
  inspect(err_string.unwrap_or("default"), content="default")
}