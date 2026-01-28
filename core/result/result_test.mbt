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
test "Result unwrap should return Ok value" {
  let ok_result : Result[Int, Error] = Ok(42)
  assert_eq(ok_result.unwrap(), 42)
  assert_eq(ok_result.unwrap_or_error(), 42)
}

///|
test "bind with Err" {
  let x : Result[Int, String] = Err("error")
  let y = x.bind(v => Ok(v * 7))
  assert_eq(y, Err("error"))
}

///|
test "wrap0 with error" {
  let f : () -> Int raise Failure = () => raise Failure::Failure("error")
  let result = try? f()
  inspect(
    result,
    content=(
      #|Err(Failure("error"))
    ),
  )
}

///|
test "wrap2 with error result" {
  let f : (Int, Int) -> Unit raise Failure = (_, _) => raise Failure::Failure(
    "error",
  )
  let r = try? f(1, 2)
  inspect(
    r,
    content=(
      #|Err(Failure("error"))
    ),
  )
}

///|
test "result basic operations" {
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error message")
  
  inspect(ok_value.is_ok(), content="true")
  inspect(ok_value.is_err(), content="false")
  inspect(err_value.is_ok(), content="false")
  inspect(err_value.is_err(), content="true")
}

///|
test "result mapping operations" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  let mapped_ok = ok_result.map(x => x * 2)
  inspect(mapped_ok, content="Ok(10)")
  
  let mapped_err = err_result.map(x => x * 2)
  inspect(mapped_err, content="Err(\"error\")")
  
  let mapped_ok_err = ok_result.map_err(e => e + " (mapped)")
  inspect(mapped_ok_err, content="Ok(5)")
  
  let mapped_err_err = err_result.map_err(e => e + " (mapped)")
  inspect(mapped_err_err, content="Err(\"error (mapped)\")")
}

///|
test "result unwrap operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  inspect(ok_result.unwrap(), content="42")
  inspect(ok_result.unwrap_or(0), content="42")
  inspect(err_result.unwrap_or(0), content="0")
  
  inspect(ok_result.unwrap_err(), content="\"error\"")
  inspect(err_result.unwrap_err(), content="\"error\"")
}

///|
test "result and_then operations" {
  let ok_result : Result[Int, String] = Ok(10)
  let err_result : Result[Int, String] = Err("initial error")
  
  let safe_divide = (x : Int) => if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  
  let result_ok = ok_result.and_then(safe_divide)
  inspect(result_ok, content="Ok(10)")
  
  let result_err = err_result.and_then(safe_divide)
  inspect(result_err, content="Err(\"initial error\")")
}

///|
test "result or_else operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("first error")
  
  let fallback = () => Ok(99)
  
  let result_ok = ok_result.or_else(fallback)
  inspect(result_ok, content="Ok(42)")
  
  let result_err = err_result.or_else(fallback)
  inspect(result_err, content="Ok(99)")
}

///|
test "result equality operations" {
  let ok1 : Result[Int, String] = Ok(5)
  let ok2 : Result[Int, String] = Ok(5)
  let ok3 : Result[Int, String] = Ok(10)
  let err1 : Result[Int, String] = Err("error")
  let err2 : Result[Int, String] = Err("error")
  let err3 : Result[Int, String] = Err("different error")
  
  assert_eq(ok1, ok2)
  assert_ne(ok1, ok3)
  assert_eq(err1, err2)
  assert_ne(err1, err3)
  assert_ne(ok1, err1)
}

///|
test "result to_option conversion" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  let option_from_ok = ok_result.to_option()
  inspect(option_from_ok, content="Some(42)")
  
  let option_from_err = err_result.to_option()
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
