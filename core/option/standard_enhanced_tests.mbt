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
test "option_basic_creation" {
  // Test Some creation
  let some_int : Int? = Some(42)
  assert_eq(some_int.is_some(), true)
  assert_eq(some_int.is_none(), false)
  
  let some_string : String? = Some("hello")
  assert_eq(some_string.is_some(), true)
  assert_eq(some_string.is_none(), false)
  
  // Test None
  let none_int : Int? = None
  assert_eq(none_int.is_some(), false)
  assert_eq(none_int.is_none(), true)
  
  let none_string : String? = None
  assert_eq(none_string.is_some(), false)
  assert_eq(none_string.is_none(), true)
}

///|
test "option_unwrap_operations" {
  // Test unwrap on Some
  let some_value : Int? = Some(123)
  assert_eq(some_value.unwrap(), 123)
  
  let some_string : String? = Some("world")
  assert_eq(some_string.unwrap(), "world")
  
  // Test unwrap_or on Some
  let some_int : Int? = Some(456)
  assert_eq(some_int.unwrap_or(999), 456)
  
  // Test unwrap_or on None
  let none_int : Int? = None
  assert_eq(none_int.unwrap_or(999), 999)
  
  let none_string : String? = None
  assert_eq(none_string.unwrap_or("default"), "default")
}

///|
test "option_map_operations" {
  // Test map on Some
  let some_int : Int? = Some(5)
  let mapped_int = some_int.map(x => x * 2)
  assert_eq(mapped_int, Some(10))
  
  let some_string : String? = Some("hello")
  let mapped_string = some_string.map(s => s.length())
  assert_eq(mapped_string, Some(5))
  
  // Test map on None
  let none_int : Int? = None
  let mapped_none = none_int.map(x => x * 2)
  assert_eq(mapped_none, None)
  
  let none_string : String? = None
  let mapped_none_string = none_string.map(s => s.length())
  assert_eq(mapped_none_string, None)
}

///|
test "option_filter_operations" {
  // Test filter on Some with true predicate
  let some_even : Int? = Some(4)
  let filtered_even = some_even.filter(x => x % 2 == 0)
  assert_eq(filtered_even, Some(4))
  
  // Test filter on Some with false predicate
  let some_odd : Int? = Some(3)
  let filtered_odd = some_odd.filter(x => x % 2 == 0)
  assert_eq(filtered_odd, None)
  
  // Test filter on None
  let none_int : Int? = None
  let filtered_none = none_int.filter(x => x % 2 == 0)
  assert_eq(filtered_none, None)
  
  // Test filter with string conditions
  let some_string : String? = Some("hello")
  let filtered_string = some_string.filter(s => s.length() > 3)
  assert_eq(filtered_string, Some("hello"))
  
  let filtered_string_short = some_string.filter(s => s.length() > 10)
  assert_eq(filtered_string_short, None)
}

///|
test "option_bind_operations" {
  // Test bind on Some
  let some_value : Int? = Some(10)
  let bind_result = some_value.bind(x => Some(x * 2))
  assert_eq(bind_result, Some(20))
  
  // Test bind returning None
  let bind_none = some_value.bind(x => None)
  assert_eq(bind_none, None)
  
  // Test bind on None
  let none_value : Int? = None
  let bind_none_result = none_value.bind(x => Some(x * 2))
  assert_eq(bind_none_result, None)
  
  // Test bind with different types
  let some_int : Int? = Some(5)
  let bind_string = some_int.bind(x => Some(x.to_string()))
  assert_eq(bind_string, Some("5"))
}

///|
test "option_equality_operations" {
  // Test Some equality
  let some1 : Int? = Some(42)
  let some2 : Int? = Some(42)
  let some3 : Int? = Some(43)
  
  assert_eq(some1 == some2, true)
  assert_eq(some1 == some3, false)
  assert_eq(some1 != some3, true)
  
  // Test None equality
  let none1 : Int? = None
  let none2 : Int? = None
  assert_eq(none1 == none2, true)
  assert_eq(none1 != none2, false)
  
  // Test Some vs None
  assert_eq(some1 == none1, false)
  assert_eq(some1 != none1, true)
  
  // Test with strings
  let some_str1 : String? = Some("hello")
  let some_str2 : String? = Some("hello")
  let some_str3 : String? = Some("world")
  
  assert_eq(some_str1 == some_str2, true)
  assert_eq(some_str1 == some_str3, false)
}

///|
test "option_with_different_types" {
  // Test with Float
  let some_float : Float? = Some(3.14)
  assert_eq(some_float.is_some(), true)
  assert_eq(some_float.unwrap(), 3.14)
  
  let none_float : Float? = None
  assert_eq(none_float.is_none(), true)
  assert_eq(none_float.unwrap_or(0.0), 0.0)
  
  // Test with Bool
  let some_bool : Bool? = Some(true)
  assert_eq(some_bool.is_some(), true)
  assert_eq(some_bool.unwrap(), true)
  
  let none_bool : Bool? = None
  assert_eq(none_bool.is_none(), true)
  assert_eq(none_bool.unwrap_or(false), false)
  
  // Test with Array
  let some_array : Array[Int]? = Some([1, 2, 3])
  assert_eq(some_array.is_some(), true)
  assert_eq(some_array.unwrap().length(), 3)
  
  let none_array : Array[Int]? = None
  assert_eq(none_array.is_none(), true)
}

///|
test "option_chaining_operations" {
  // Test multiple map operations
  let some_value : Int? = Some(5)
  let chained = some_value
    .map(x => x * 2)
    .map(x => x + 1)
    .map(x => x.to_string())
  assert_eq(chained, Some("11"))
  
  // Test map then filter
  let map_filter = some_value
    .map(x => x * 3)
    .filter(x => x > 10)
  assert_eq(map_filter, Some(15))
  
  // Test filter then map
  let filter_map = some_value
    .filter(x => x > 3)
    .map(x => x * 2)
  assert_eq(filter_map, Some(10))
  
  // Test chain with None
  let none_value : Int? = None
  let none_chained = none_value
    .map(x => x * 2)
    .filter(x => x > 5)
  assert_eq(none_chained, None)
}

///|
test "option_edge_cases" {
  // Test with zero values
  let some_zero : Int? = Some(0)
  assert_eq(some_zero.is_some(), true)
  assert_eq(some_zero.unwrap(), 0)
  
  // Test with empty string
  let some_empty : String? = Some("")
  assert_eq(some_empty.is_some(), true)
  assert_eq(some_empty.unwrap(), "")
  
  // Test with empty array
  let some_empty_array : Array[Int]? = Some([])
  assert_eq(some_empty_array.is_some(), true)
  assert_eq(some_empty_array.unwrap().length(), 0)
  
  // Test unwrap_or with zero/empty defaults
  let none_int : Int? = None
  assert_eq(none_int.unwrap_or(0), 0)
  
  let none_string : String? = None
  assert_eq(none_string.unwrap_or(""), "")
}