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
  let ok_value = Ok(42)
  let err_value = Err("error")
  
  assert_eq(ok_value.is_ok(), true)
  assert_eq(ok_value.is_err(), false)
  assert_eq(err_value.is_ok(), false)
  assert_eq(err_value.is_err(), true)
}

///|
test "result unwrap operations" {
  let ok_value = Ok(42)
  let err_value = Err("error")
  
  assert_eq(ok_value.unwrap(), 42)
  assert_eq(ok_value.unwrap_or(0), 42)
  assert_eq(err_value.unwrap_or(0), 0)
  assert_eq(ok_value.unwrap_or_else(|| 0), 42)
  assert_eq(err_value.unwrap_or_else(|| 0), 0)
  
  // Test expect
  assert_eq(ok_value.expect("Should not panic"), 42)
}

///|
test "result map operations" {
  let ok_value = Ok(5)
  let err_value = Err("error")
  
  let mapped_ok = ok_value.map(fn(x) { x * 2 })
  let mapped_err = err_value.map(fn(x) { x * 2 })
  
  assert_eq(mapped_ok, Ok(10))
  assert_eq(mapped_err, Err("error"))
  
  // Test map_or
  assert_eq(ok_value.map_or(0, fn(x) { x * 2 }), 10)
  assert_eq(err_value.map_or(0, fn(x) { x * 2 }), 0)
  
  // Test map_or_else
  assert_eq(ok_value.map_or_else(|| 0, fn(x) { x * 2 }), 10)
  assert_eq(err_value.map_or_else(|| 0, fn(x) { x * 2 }), 0)
}

///|
test "result map_err operations" {
  let ok_value = Ok(5)
  let err_value = Err("error")
  
  let mapped_err_ok = ok_value.map_err(fn(e) { e + " mapped" })
  let mapped_err_err = err_value.map_err(fn(e) { e + " mapped" })
  
  assert_eq(mapped_err_ok, Ok(5))
  assert_eq(mapped_err_err, Err("error mapped"))
}

///|
test "result and_then operations" {
  let ok_value = Ok(5)
  let err_value = Err("error")
  
  let and_then_ok = ok_value.and_then(fn(x) { if x > 3 { Ok(x * 2) } else { Err("too small") } })
  let and_then_err = err_value.and_then(fn(x) { Ok(x * 2) })
  let and_then_fail = ok_value.and_then(fn(x) { if x < 3 { Ok(x * 2) } else { Err("too large") } })
  
  assert_eq(and_then_ok, Ok(10))
  assert_eq(and_then_err, Err("error"))
  assert_eq(and_then_fail, Err("too large"))
}

///|
test "result or_else operations" {
  let ok_value = Ok(5)
  let err_value = Err("error")
  
  let or_else_ok = ok_value.or_else(fn(e) { if e == "error" { Ok(100) } else { Err("other") } })
  let or_else_err = err_value.or_else(fn(e) { if e == "error" { Ok(100) } else { Err("other") } })
  let or_else_fail = err_value.or_else(fn(e) { if e == "other" { Ok(100) } else { Err("fallback") } })
  
  assert_eq(or_else_ok, Ok(5))
  assert_eq(or_else_err, Ok(100))
  assert_eq(or_else_fail, Err("fallback"))
}

///|
test "result unwrap_or_default" {
  let ok_value = Ok(42)
  let err_value = Err("error")
  
  assert_eq(ok_value.unwrap_or_default(), 42)
  assert_eq(err_value.unwrap_or_default(), 0)  // Default for Int is 0
}

///|
test "result equality" {
  let ok1 = Ok(42)
  let ok2 = Ok(42)
  let ok3 = Ok(43)
  let err1 = Err("error")
  let err2 = Err("error")
  let err3 = Err("different")
  
  assert_eq(ok1 == ok2, true)
  assert_eq(ok1 == ok3, false)
  assert_eq(ok1 == err1, false)
  assert_eq(err1 == err2, true)
  assert_eq(err1 == err3, false)
  assert_eq(err1 == ok1, false)
  
  assert_eq(ok1 != ok2, false)
  assert_eq(ok1 != ok3, true)
  assert_eq(ok1 != err1, true)
  assert_eq(err1 != err2, false)
  assert_eq(err1 != err3, true)
  assert_eq(err1 != ok1, true)
}

///|
test "result contains" {
  let ok_value = Ok(42)
  let err_value = Err("error")
  
  assert_eq(ok_value.contains(42), true)
  assert_eq(ok_value.contains(43), false)
  assert_eq(err_value.contains(42), false)
  
  // Test contains_err
  assert_eq(ok_value.contains_err("error"), false)
  assert_eq(err_value.contains_err("error"), true)
  assert_eq(err_value.contains_err("different"), false)
}

///|
test "result transpose" {
  let ok_some = Ok(Some(42))
  let ok_none = Ok(None[Int])
  let err_some = Err("error")
  
  let transposed_some = ok_some.transpose()
  let transposed_none = ok_none.transpose()
  let transposed_err = err_some.transpose()
  
  assert_eq(transposed_some, Some(Ok(42)))
  assert_eq(transposed_none, None[Result[Int, String]])
  assert_eq(transposed_err, Some(Err("error")))
}

///|
test "result flatten" {
  let nested_ok = Ok(Ok(42))
  let nested_err_inner = Ok(Err("inner error"))
  let nested_err_outer = Err("outer error")
  
  let flattened_ok = nested_ok.flatten()
  let flattened_err_inner = nested_err_inner.flatten()
  let flattened_err_outer = nested_err_outer.flatten()
  
  assert_eq(flattened_ok, Ok(42))
  assert_eq(flattened_err_inner, Err("inner error"))
  assert_eq(flattened_err_outer, Err("outer error"))
}