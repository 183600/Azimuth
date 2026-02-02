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
test "option contains functionality" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let contains = fn(x : Int?, value : Int) -> Bool {
    match x {
      None => false
      Some(v) => v == value
    }
  }
  
  assert_eq(contains(some_value, 42), true)
  assert_eq(contains(some_value, 100), false)
  assert_eq(contains(none_value, 42), false)
}

///|
test "option zip operation" {
  let some1 : Int? = Some(42)
  let some2 : String? = Some("hello")
  let none1 : Int? = None
  let none2 : String? = None
  
  let zip = fn(x : Int?, y : String?) -> (Int, String)? {
    match (x, y) {
      (Some(a), Some(b)) => Some((a, b))
      _ => None
    }
  }
  
  assert_eq(zip(some1, some2), Some((42, "hello")))
  assert_eq(zip(some1, none2), None)
  assert_eq(zip(none1, some2), None)
  assert_eq(zip(none1, none2), None)
}

///|
test "option flatten nested options" {
  let nested_some : Int?? = Some(Some(42))
  let nested_none_outer : Int?? = None
  let nested_none_inner : Int?? = Some(None)
  
  let flatten = fn(x : Int??) -> Int? {
    match x {
      None => None
      Some(inner) => inner
    }
  }
  
  assert_eq(flatten(nested_some), Some(42))
  assert_eq(flatten(nested_none_outer), None)
  assert_eq(flatten(nested_none_inner), None)
}