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
test "option basic creation" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
}

///|
test "option unwrap" {
  let some_value : Int? = Some(42)
  assert_eq(some_value.unwrap(), 42)
}

///|
test "option unwrap_or" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "option map" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let mapped_some = some_value.map(x => x * 2)
  let mapped_none = none_value.map(x => x * 2)
  
  assert_eq(mapped_some, Some(84))
  assert_eq(mapped_none, None)
}

///|
test "option filter" {
  let some_value : Int? = Some(42)
  let some_odd : Int? = Some(41)
  let none_value : Int? = None
  
  let filtered_even = some_value.filter(x => x % 2 == 0)
  let filtered_odd = some_odd.filter(x => x % 2 == 0)
  let filtered_none = none_value.filter(x => x % 2 == 0)
  
  assert_eq(filtered_even, Some(42))
  assert_eq(filtered_odd, None)
  assert_eq(filtered_none, None)
}

///|
test "option and_then" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  let and_then_some = some_value.and_then(x => if x > 40 { Some(x * 2) } else { None })
  let and_then_none = none_value.and_then(x => Some(x * 2))
  
  assert_eq(and_then_some, Some(84))
  assert_eq(and_then_none, None)
}

///|
test "option or" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  let alternative : Int? = Some(99)
  
  assert_eq(some_value.or(alternative), Some(42))
  assert_eq(none_value.or(alternative), Some(99))
}

///|
test "option default" {
  let default_option : Int? = Option::default()
  assert_eq(default_option, None)
}