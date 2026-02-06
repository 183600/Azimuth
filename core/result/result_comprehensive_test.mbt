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
  let ok_value : Result[Int, String] = Ok(42)
  let err_value : Result[Int, String] = Err("error message")
  
  // Test pattern matching
  let result1 = match ok_value {
    Ok(x) => x * 2
    Err(_) => 0
  }
  let result2 = match err_value {
    Ok(x) => x * 2
    Err(e) => -1
  }
  
  assert_eq(result1, 84)
  assert_eq(result2, -1)
}

///|
test "result is_ok and is_err methods" {
  let ok_int : Result[Int, String] = Ok(42)
  let err_int : Result[Int, String] = Err("error")
  let ok_str : Result[String, Int] = Ok("hello")
  let err_str : Result[String, Int] = Err(404)
  
  // Test is_ok
  assert_true(ok_int.is_ok())
  assert_false(err_int.is_ok())
  assert_true(ok_str.is_ok())
  assert_false(err_str.is_ok())
  
  // Test is_err
  assert_false(ok_int.is_err())
  assert_true(err_int.is_err())
  assert_false(ok_str.is_err())
  assert_true(err_str.is_err())
}

///|
test "result unwrap operations" {
  let ok_int : Result[Int, String] = Ok(42)
  let err_int : Result[Int, String] = Err("error")
  let ok_str : Result[String, Int] = Ok("hello")
  let err_str : Result[String, Int] = Err(404)
  
  // Test unwrap
  assert_eq(ok_int.unwrap(), 42)
  assert_eq(ok_str.unwrap(), "hello")
  
  // Test unwrap_err
  assert_eq(err_int.unwrap_err(), "error")
  assert_eq(err_str.unwrap_err(), 404)
  
  // Test unwrap_or
  assert_eq(ok_int.unwrap_or(0), 42)
  assert_eq(err_int.unwrap_or(0), 0)
  assert_eq(ok_str.unwrap_or("default"), "hello")
  assert_eq(err_str.unwrap_or("default"), "default")
  
  // Test unwrap_or_else
  assert_eq(ok_int.unwrap_or_else(|| 100), 42)
  assert_eq(err_int.unwrap_or_else(|| 100), 100)
}

///|
test "result map operations" {
  let ok_result : Result[Int, String] = Ok(5)
  let err_result : Result[Int, String] = Err("error")
  
  // Test map
  assert_eq(ok_result.map(x => x * 2), Ok(10))
  assert_eq(err_result.map(x => x * 2), Err("error"))
  
  // Test map_err
  assert_eq(ok_result.map_err(e => e + " (mapped)"), Ok(5))
  assert_eq(err_result.map_err(e => e + " (mapped)"), Err("error (mapped)"))
  
  // Test map with different types
  let ok_str : Result[String, Int] = Ok("hello")
  let err_str : Result[String, Int] = Err(404)
  
  assert_eq(ok_str.map(s => s.length()), Ok(5))
  assert_eq(err_str.map(s => s.length()), Err(404))
  assert_eq(ok_str.map_err(code => code + 100), Ok("hello"))
  assert_eq(err_str.map_err(code => code + 100), Err(504))
}

///|
test "result and_then operations" {
  let ok_result : Result[Int, String] = Ok(10)
  let err_result : Result[Int, String] = Err("initial error")
  
  // Safe division function
  let safe_divide = (x : Int) -> Result[Int, String] {
    if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  }
  
  // Test and_then
  assert_eq(ok_result.and_then(safe_divide), Ok(10))
  assert_eq(err_result.and_then(safe_divide), Err("initial error"))
  
  // Test with Ok(0)
  let zero_result : Result[Int, String] = Ok(0)
  assert_eq(zero_result.and_then(safe_divide), Err("division by zero"))
  
  // Test chaining
  let chain_result = ok_result
    .and_then(x => if x > 5 { Ok(x * 2) } else { Err("too small") })
    .and_then(x => if x % 2 == 0 { Ok(x / 2) } else { Err("not even") })
  assert_eq(chain_result, Ok(10))
}

///|
test "result or_else operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("first error")
  
  // Test or_else
  assert_eq(ok_result.or_else(|| Ok(99)), Ok(42))
  assert_eq(err_result.or_else(|| Ok(99)), Ok(99))
  assert_eq(err_result.or_else(|| Err("second error")), Err("second error"))
  
  // Test chaining or_else
  let chain_result = err_result
    .or_else(|| Err("second error"))
    .or_else(|| Ok(200))
    .or_else(|| Ok(300))
  assert_eq(chain_result, Ok(200))
}

///|
test "result equality operations" {
  let ok1 : Result[Int, String] = Ok(5)
  let ok2 : Result[Int, String] = Ok(5)
  let ok3 : Result[Int, String] = Ok(10)
  let err1 : Result[Int, String] = Err("error")
  let err2 : Result[Int, String] = Err("error")
  let err3 : Result[Int, String] = Err("different error")
  
  // Test equality
  assert_eq(ok1, ok2)
  assert_ne(ok1, ok3)
  assert_eq(err1, err2)
  assert_ne(err1, err3)
  assert_ne(ok1, err1)
  
  // Test with different types
  let ok_str : Result[String, Int] = Ok("hello")
  let err_str : Result[String, Int] = Err(404)
  assert_eq(ok_str, Ok("hello"))
  assert_ne(ok_str, Ok("world"))
  assert_eq(err_str, Err(404))
  assert_ne(err_str, Err(500))
}

///|
test "result to_option conversion" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  let ok_str : Result[String, Int] = Ok("hello")
  let err_str : Result[String, Int] = Err(404)
  
  // Test to_option
  assert_eq(ok_result.to_option(), Some(42))
  assert_eq(err_result.to_option(), None)
  assert_eq(ok_str.to_option(), Some("hello"))
  assert_eq(err_str.to_option(), None)
}

///|
test "result from_option conversion" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let some_str : String? = Some("hello")
  let none_str : String? = None
  
  // Test to_result
  assert_eq(some_value.to_result(() => "no value"), Ok(42))
  assert_eq(none_value.to_result(() => "no value"), Err("no value"))
  assert_eq(some_str.to_result(() => 404), Ok("hello"))
  assert_eq(none_str.to_result(() => 404), Err(404))
}

///|
test "result comparison operations" {
  let ok1 : Result[Int, String] = Ok(1)
  let ok2 : Result[Int, String] = Ok(2)
  let err : Result[Int, String] = Err("error")
  
  // Test comparison (Err is considered less than Ok)
  assert_eq(err.compare(err), 0)
  assert_eq(err.compare(ok1), -1)
  assert_eq(ok1.compare(err), 1)
  assert_eq(ok1.compare(ok1), 0)
  assert_eq(ok1.compare(ok2), -1)
  assert_eq(ok2.compare(ok1), 1)
}

///|
test "result complex operations" {
  // Test complex chaining with different error types
  let parse_int = (s : String) -> Result[Int, String] {
    match s {
      "0" => Ok(0)
      "1" => Ok(1)
      "2" => Ok(2)
      _ => Err("invalid number")
    }
  }
  
  let safe_divide = (x : Int) -> Result[Int, String] {
    if x == 0 { Err("division by zero") } else { Ok(100 / x) }
  }
  
  let double_if_positive = (x : Int) -> Result[Int, String] {
    if x > 0 { Ok(x * 2) } else { Err("not positive") }
  }
  
  // Test successful chain
  let result1 = parse_int("2")
    .and_then(safe_divide)
    .and_then(double_if_positive)
  assert_eq(result1, Ok(100)) // 100/2 = 50, 50*2 = 100
  
  // Test failure at different stages
  let result2 = parse_int("invalid")
    .and_then(safe_divide)
    .and_then(double_if_positive)
  assert_eq(result2, Err("invalid number"))
  
  let result3 = parse_int("0")
    .and_then(safe_divide)
    .and_then(double_if_positive)
  assert_eq(result3, Err("division by zero"))
  
  let result4 = parse_int("-1")
    .and_then(safe_divide)
    .and_then(double_if_positive)
  assert_eq(result4, Err("invalid number")) // parse_int fails first
}

///|
test "result error handling patterns" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("network error")
  
  // Test error mapping and recovery
  let recover_with_default = (r : Result[Int, String]) -> Int {
    r.map_err(e => "Processed: " + e).unwrap_or_else(|_| 0)
  }
  
  assert_eq(recover_with_default(ok_result), 42)
  assert_eq(recover_with_default(err_result), 0)
  
  // Test conditional operations
  let process_if_positive = (r : Result[Int, String]) -> Result[Int, String> {
    r.and_then(x => if x > 0 { Ok(x * 2) } else { Err("not positive") })
  }
  
  assert_eq(process_if_positive(Ok(5)), Ok(10))
  assert_eq(process_if_positive(Ok(-5)), Err("not positive"))
  assert_eq(process_if_positive(Err("input error")), Err("input error"))
}