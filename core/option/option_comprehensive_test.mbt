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
test "option basic creation and pattern matching" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test pattern matching
  let result1 = match some_value {
    Some(x) => x * 2
    None => 0
  }
  let result2 = match none_value {
    Some(x) => x * 2
    None => 0
  }
  
  assert_eq(result1, 84)
  assert_eq(result2, 0)
}

///|
test "option is_some and is_none methods" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_str : String? = Some("hello")
  let none_str : String? = None
  
  // Test is_some
  assert_true(some_int.is_some())
  assert_false(none_int.is_some())
  assert_true(some_str.is_some())
  assert_false(none_str.is_some())
  
  // Test is_none
  assert_false(some_int.is_none())
  assert_true(none_int.is_none())
  assert_false(some_str.is_none())
  assert_true(none_str.is_none())
}

///|
test "option unwrap operations" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_str : String? = Some("hello")
  let none_str : String? = None
  
  // Test unwrap
  assert_eq(some_int.unwrap(), 42)
  assert_eq(some_str.unwrap(), "hello")
  
  // Test unwrap_or
  assert_eq(some_int.unwrap_or(0), 42)
  assert_eq(none_int.unwrap_or(0), 0)
  assert_eq(some_str.unwrap_or("default"), "hello")
  assert_eq(none_str.unwrap_or("default"), "default")
  
  // Test unwrap_or_else
  assert_eq(some_int.unwrap_or_else(|| 100), 42)
  assert_eq(none_int.unwrap_or_else(|| 100), 100)
  assert_eq(some_str.unwrap_or_else(|| "fallback"), "hello")
  assert_eq(none_str.unwrap_or_else(|| "fallback"), "fallback")
}

///|
test "option map operations" {
  let some_int : Int? = Some(5)
  let none_int : Int? = None
  let some_str : String? = Some("hello")
  let none_str : String? = None
  
  // Test map
  assert_eq(some_int.map(x => x * 2), Some(10))
  assert_eq(none_int.map(x => x * 2), None)
  assert_eq(some_str.map(s => s.length()), Some(5))
  assert_eq(none_str.map(s => s.length()), None)
  
  // Test map_or
  assert_eq(some_int.map_or(0, x => x * 2), 10)
  assert_eq(none_int.map_or(0, x => x * 2), 0)
  assert_eq(some_str.map_or(0, s => s.length()), 5)
  assert_eq(none_str.map_or(0, s => s.length()), 0)
  
  // Test map_or_else
  assert_eq(some_int.map_or_else(|| 100, x => x * 2), 10)
  assert_eq(none_int.map_or_else(|| 100, x => x * 2), 100)
}

///|
test "option filter operations" {
  let some_positive : Int? = Some(5)
  let some_negative : Int? = Some(-3)
  let some_zero : Int? = Some(0)
  let none_value : Int? = None
  
  // Test filter
  assert_eq(some_positive.filter(x => x > 0), Some(5))
  assert_eq(some_negative.filter(x => x > 0), None)
  assert_eq(some_zero.filter(x => x > 0), None)
  assert_eq(none_value.filter(x => x > 0), None)
  
  // Test with different predicate
  assert_eq(some_negative.filter(x => x < 0), Some(-3))
  assert_eq(some_positive.filter(x => x < 0), None)
}

///|
test "option and_then (bind) operations" {
  let some_int : Int? = Some(10)
  let none_int : Int? = None
  
  // Safe division function
  let safe_divide = (x : Int) => if x == 0 { None } else { Some(100 / x) }
  
  // Test and_then
  assert_eq(some_int.and_then(safe_divide), Some(10))
  assert_eq(none_int.and_then(safe_divide), None)
  
  // Test with Some(0)
  let zero_value : Int? = Some(0)
  assert_eq(zero_value.and_then(safe_divide), None)
  
  // Test chaining
  let chain_result = some_int
    .and_then(x => if x > 5 { Some(x * 2) } else { None })
    .and_then(x => if x % 2 == 0 { Some(x / 2) } else { None })
  assert_eq(chain_result, Some(10))
}

///|
test "option or_else operations" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  
  // Test or_else
  assert_eq(some_int.or_else(|| Some(100)), Some(42))
  assert_eq(none_int.or_else(|| Some(100)), Some(100))
  assert_eq(none_int.or_else(|| None), None)
  
  // Test chaining or_else
  let chain_result = none_int
    .or_else(|| None)
    .or_else(|| Some(200))
    .or_else(|| Some(300))
  assert_eq(chain_result, Some(200))
}

///|
test "option equality operations" {
  let some1 : Int? = Some(5)
  let some2 : Int? = Some(5)
  let some3 : Int? = Some(10)
  let none1 : Int? = None
  let none2 : Int? = None
  
  // Test equality
  assert_eq(some1, some2)
  assert_ne(some1, some3)
  assert_eq(none1, none2)
  assert_ne(some1, none1)
  
  // Test with different types
  let some_str : String? = Some("hello")
  let none_str : String? = None
  assert_eq(some_str, Some("hello"))
  assert_ne(some_str, Some("world"))
  assert_eq(none_str, None)
}

///|
test "option to_array conversion" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_str : String? = Some("hello")
  let none_str : String? = None
  
  // Test to_array
  assert_eq(some_int.to_array(), [42])
  assert_eq(none_int.to_array(), [])
  assert_eq(some_str.to_array(), ["hello"])
  assert_eq(none_str.to_array(), [])
}

///|
test "option nested operations" {
  let nested_some : Int?? = Some(Some(42))
  let nested_none_outer : Int?? = None
  let nested_none_inner : Int?? = Some(None)
  
  // Test flatten
  assert_eq(nested_some.flatten(), Some(42))
  assert_eq(nested_none_outer.flatten(), None)
  assert_eq(nested_none_inner.flatten(), None)
  
  // Test nested operations
  let result = nested_some.flatten().map(x => x * 2)
  assert_eq(result, Some(84))
  
  let result2 = nested_none_inner.flatten().map(x => x * 2)
  assert_eq(result2, None)
}

///|
test "option comparison operations" {
  let some1 : Int? = Some(1)
  let some2 : Int? = Some(2)
  let none : Int? = None
  
  // Test comparison (None is considered less than Some)
  assert_eq(none.compare(none), 0)
  assert_eq(none.compare(some1), -1)
  assert_eq(some1.compare(none), 1)
  assert_eq(some1.compare(some1), 0)
  assert_eq(some1.compare(some2), -1)
  assert_eq(some2.compare(some1), 1)
}

///|
test "option iterator operations" {
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  
  // Test iter
  let mut sum1 = 0
  some_int.iter().each(x => { sum1 = sum1 + x })
  assert_eq(sum1, 42)
  
  let mut sum2 = 0
  none_int.iter().each(x => { sum2 = sum2 + x })
  assert_eq(sum2, 0)
  
  // Test collect to array
  let array_from_some = some_int.iter().to_array()
  let array_from_none = none_int.iter().to_array()
  assert_eq(array_from_some, [42])
  assert_eq(array_from_none, [])
}