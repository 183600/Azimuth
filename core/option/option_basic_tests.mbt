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
test "option utility functions" {
  // Test contains function
  let contains = fn(x : Int?, value : Int) -> Bool {
    match x {
      None => false
      Some(v) => v == value
    }
  }
  
  // Test zip function
  let zip = fn(x : Int?, y : String?) -> (Int, String)? {
    match (x, y) {
      (Some(a), Some(b)) => Some((a, b))
      _ => None
    }
  }
  
  // Test flatten function
  let flatten = fn(x : Int??) -> Int? {
    match x {
      None => None
      Some(inner) => inner
    }
  }
  
  // Test the functions with various inputs
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let some_string : String? = Some("hello")
  let none_string : String? = None
  let nested_some : Int?? = Some(Some(42))
  let nested_none : Int?? = Some(None)
  
  // Function calls to verify they work
  let _ = contains(some_value, 42)
  let _ = contains(some_value, 100)
  let _ = contains(none_value, 42)
  
  let _ = zip(some_value, some_string)
  let _ = zip(some_value, none_string)
  let _ = zip(none_value, some_string)
  let _ = zip(none_value, none_string)
  
  let _ = flatten(nested_some)
  let _ = flatten(nested_none)
  let _ = flatten((None : Int??))
}