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
test "option basic operations" {
  let some_value = Some(42)
  let none_value = None[Int]
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
}

///|
test "option unwrap operations" {
  let some_value = Some(42)
  let none_value = None[Int]
  
  assert_eq(some_value.unwrap(), 42)
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
  assert_eq(some_value.unwrap_or_else(|| 0), 42)
  assert_eq(none_value.unwrap_or_else(|| 0), 0)
}

///|
test "option map operations" {
  let some_value = Some(5)
  let none_value = None[Int]
  
  let mapped_some = some_value.map(fn(x) { x * 2 })
  let mapped_none = none_value.map(fn(x) { x * 2 })
  
  assert_eq(mapped_some, Some(10))
  assert_eq(mapped_none, None[Int])
  
  // Test map_or
  assert_eq(some_value.map_or(0, fn(x) { x * 2 }), 10)
  assert_eq(none_value.map_or(0, fn(x) { x * 2 }), 0)
  
  // Test map_or_else
  assert_eq(some_value.map_or_else(|| 0, fn(x) { x * 2 }), 10)
  assert_eq(none_value.map_or_else(|| 0, fn(x) { x * 2 }), 0)
}

///|
test "option filter operations" {
  let some_value = Some(5)
  let some_even = Some(6)
  let none_value = None[Int]
  
  let filtered_some = some_value.filter(fn(x) { x % 2 == 0 })
  let filtered_even = some_even.filter(fn(x) { x % 2 == 0 })
  let filtered_none = none_value.filter(fn(x) { x % 2 == 0 })
  
  assert_eq(filtered_some, None[Int])
  assert_eq(filtered_even, Some(6))
  assert_eq(filtered_none, None[Int])
}

///|
test "option and_then operations" {
  let some_value = Some(5)
  let none_value = None[Int]
  
  let and_then_some = some_value.and_then(fn(x) { if x > 3 { Some(x * 2) } else { None[Int] } })
  let and_then_none = none_value.and_then(fn(x) { Some(x * 2) })
  let and_then_fail = some_value.and_then(fn(x) { if x < 3 { Some(x * 2) } else { None[Int] } })
  
  assert_eq(and_then_some, Some(10))
  assert_eq(and_then_none, None[Int])
  assert_eq(and_then_fail, None[Int])
}

///|
test "option or operations" {
  let some_value = Some(5)
  let none_value = None[Int]
  let other_some = Some(10)
  let other_none = None[Int]
  
  assert_eq(some_value.or(other_some), Some(5))
  assert_eq(some_value.or(other_none), Some(5))
  assert_eq(none_value.or(other_some), Some(10))
  assert_eq(none_value.or(other_none), None[Int])
  
  // Test or_else
  assert_eq(some_value.or_else(|| other_some), Some(5))
  assert_eq(some_value.or_else(|| other_none), Some(5))
  assert_eq(none_value.or_else(|| other_some), Some(10))
  assert_eq(none_value.or_else(|| other_none), None[Int])
}

///|
test "option xor operations" {
  let some_value = Some(5)
  let none_value = None[Int]
  let other_some = Some(10)
  let other_none = None[Int]
  
  assert_eq(some_value.xor(other_some), None[Int])
  assert_eq(some_value.xor(other_none), Some(5))
  assert_eq(none_value.xor(other_some), Some(10))
  assert_eq(none_value.xor(other_none), None[Int])
}

///|
test "option get_or_insert" {
  let mut opt = None[Int]
  
  assert_eq(opt.get_or_insert(5), 5)
  assert_eq(opt, Some(5))
  
  assert_eq(opt.get_or_insert(10), 5)
  assert_eq(opt, Some(5))
  
  // Test get_or_insert_with
  let mut opt2 = None[Int]
  assert_eq(opt2.get_or_insert_with(|| 42), 42)
  assert_eq(opt2, Some(42))
  
  assert_eq(opt2.get_or_insert_with(|| 100), 42)
  assert_eq(opt2, Some(42))
}

///|
test "option take" {
  let mut opt = Some(42)
  let taken = opt.take()
  
  assert_eq(taken, Some(42))
  assert_eq(opt, None[Int])
  
  let mut empty_opt = None[Int]
  let empty_taken = empty_opt.take()
  
  assert_eq(empty_taken, None[Int])
  assert_eq(empty_opt, None[Int])
}

///|
test "option replace" {
  let mut opt = Some(42)
  let old_value = opt.replace(100)
  
  assert_eq(old_value, Some(42))
  assert_eq(opt, Some(100))
  
  let mut empty_opt = None[Int]
  let empty_old = empty_opt.replace(200)
  
  assert_eq(empty_old, None[Int])
  assert_eq(empty_opt, Some(200))
}

///|
test "option equality" {
  let some1 = Some(42)
  let some2 = Some(42)
  let some3 = Some(43)
  let none1 = None[Int]
  let none2 = None[Int]
  
  assert_eq(some1 == some2, true)
  assert_eq(some1 == some3, false)
  assert_eq(some1 == none1, false)
  assert_eq(none1 == none2, true)
  assert_eq(none1 == some1, false)
  
  assert_eq(some1 != some2, false)
  assert_eq(some1 != some3, true)
  assert_eq(some1 != none1, true)
  assert_eq(none1 != none2, false)
  assert_eq(none1 != some1, true)
}