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
test "option basic functionality" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test is_some and is_none
  let is_some = fn(x : Int?) -> Bool {
    match x {
      None => false
      Some(_) => true
    }
  }
  
  let is_none = fn(x : Int?) -> Bool {
    match x {
      None => true
      Some(_) => false
    }
  }
  
  @test.assert_eq(is_some(some_value), true)
  @test.assert_eq(is_none(some_value), false)
  @test.assert_eq(is_some(none_value), false)
  @test.assert_eq(is_none(none_value), true)
}

///|
test "option map functionality" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  let map = fn(x : Int?, f : Int -> Int) -> Int? {
    match x {
      None => None
      Some(v) => Some(f(v))
    }
  }
  
  let doubled_some = map(some_value, x => x * 2)
  let doubled_none = map(none_value, x => x * 2)
  
  @test.assert_eq(doubled_some, Some(10))
  @test.assert_eq(doubled_none, None)
}

///|
test "option filter functionality" {
  let some_positive : Int? = Some(5)
  let some_negative : Int? = Some(-3)
  let none_value : Int? = None
  
  let filter = fn(x : Int?, predicate : Int -> Bool) -> Int? {
    match x {
      None => None
      Some(v) => if predicate(v) { Some(v) } else { None }
    }
  }
  
  let filtered_positive = filter(some_positive, x => x > 0)
  let filtered_negative = filter(some_negative, x => x > 0)
  let filtered_none = filter(none_value, x => x > 0)
  
  @test.assert_eq(filtered_positive, Some(5))
  @test.assert_eq(filtered_negative, None)
  @test.assert_eq(filtered_none, None)
}