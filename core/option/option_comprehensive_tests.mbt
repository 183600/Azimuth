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
  let some_int : Int? = Some(42)
  let none_int : Int? = None
  let some_string : String? = Some("hello")
  let none_string : String? = None
  
  // Test is_some and is_none
  inspect(some_int.is_some(), content="true")
  inspect(some_int.is_none(), content="false")
  inspect(none_int.is_some(), content="false")
  inspect(none_int.is_none(), content="true")
  
  // Test with strings
  inspect(some_string.is_some(), content="true")
  inspect(none_string.is_none(), content="true")
}

///|
test "option unwrap operations" {
  let some_value : Int? = Some(100)
  let none_value : Int? = None
  
  // Test unwrap
  inspect(some_value.unwrap(), content="100")
  
  // Test unwrap_or
  inspect(some_value.unwrap_or(0), content="100")
  inspect(none_value.unwrap_or(0), content="0")
  
  // Test unwrap_or_else
  inspect(some_value.unwrap_or_else(|| 42), content="100")
  inspect(none_value.unwrap_or_else(|| 42), content="42")
}

///|
test "option map operations" {
  let some_int : Int? = Some(5)
  let none_int : Int? = None
  let some_string : String? = Some("hello")
  
  // Test map with different operations
  inspect(some_int.map(x => x * 2), content="Some(10)")
  inspect(none_int.map(x => x * 2), content="None")
  
  inspect(some_int.map(x => x.to_string()), content="Some('5')")
  inspect(some_string.map(s => s.to_upper()), content="Some('HELLO')")
  inspect(none_string.map(s => s.to_upper()), content="None")
  
  // Test chained map operations
  let result = some_int
    .map(x => x * 2)
    .map(x => x + 1)
    .map(x => x.to_string())
  
  inspect(result, content="Some('11')")
}

///|
test "option filter operations" {
  let some_even : Int? = Some(4)
  let some_odd : Int? = Some(3)
  let none_value : Int? = None
  
  // Test filter with even predicate
  inspect(some_even.filter(x => x % 2 == 0), content="Some(4)")
  inspect(some_odd.filter(x => x % 2 == 0), content="None")
  inspect(none_value.filter(x => x % 2 == 0), content="None")
  
  // Test filter with string predicate
  let some_long : String? = Some("hello world")
  let some_short : String? = Some("hi")
  
  inspect(some_long.filter(s => s.length() > 5), content="Some('hello world')")
  inspect(some_short.filter(s => s.length() > 5), content="None")
}

///|
test "option bind (flat_map) operations" {
  let safe_divide = (x : Int) => if x == 0 { None } else { Some(100 / x) }
  let safe_sqrt = (x : Int) => if x < 0 { None } else { Some(@int.to_float(x).sqrt()) }
  
  let some_positive : Int? = Some(4)
  let some_zero : Int? = Some(0)
  let some_negative : Int? = Some(-4)
  let none_value : Int? = None
  
  // Test bind with safe division
  inspect(some_positive.bind(x => safe_divide(x)), content="Some(25)")
  inspect(some_zero.bind(x => safe_divide(x)), content="None")
  inspect(none_value.bind(x => safe_divide(x)), content="None")
  
  // Test bind with safe square root
  inspect(some_positive.bind(x => safe_sqrt(x)), content="Some(2.0)")
  inspect(some_negative.bind(x => safe_sqrt(x)), content="None")
  
  // Test chained bind operations
  let chained_result = some_positive
    .bind(x => safe_divide(x))
    .bind(x => safe_sqrt(x))
  
  inspect(chained_result, content="Some(5.0)")
}

///|
test "option equality and comparison" {
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
  let some_string : String? = Some("hello")
  let some_int : Int? = Some(5)
  // These should not be comparable due to type differences
}

///|
test "option with complex types" {
  // Test with arrays
  let some_array : Array[Int]? = Some([1, 2, 3])
  let none_array : Array[Int]? = None
  
  inspect(some_array.map(arr => arr.length()), content="Some(3)")
  inspect(some_array.map(arr => arr[0]), content="Some(1)")
  inspect(none_array.map(arr => arr.length()), content="None")
  
  // Test with tuples
  let some_tuple : (Int, String)? = Some((42, "answer"))
  inspect(some_tuple.map(t => t.0), content="Some(42)")
  inspect(some_tuple.map(t => t.1), content="Some('answer')")
  
  // Test with nested options
  let nested : Option[Int]? = Some(Some(5))
  let flattened = nested.bind(x => x)
  inspect(flattened, content="Some(5)")
  
  let nested_none : Option[Int]? = Some(None)
  let flattened_none = nested_none.bind(x => x)
  inspect(flattened_none, content="None")
}

///|
test "option utility functions" {
  let some_value : Int? = Some(10)
  let none_value : Int? = None
  
  // Test or_else
  inspect(some_value.or_else(|| Some(20)), content="Some(10)")
  inspect(none_value.or_else(|| Some(20)), content="Some(20)")
  inspect(none_value.or_else(|| None), content="None")
  
  // Test and_then (alias for bind)
  inspect(some_value.and_then(x => Some(x * 2)), content="Some(20)")
  inspect(none_value.and_then(x => Some(x * 2)), content="None")
  
  // Test and
  inspect(some_value.and(Some(20)), content="Some(20)")
  inspect(some_value.and(None), content="None")
  inspect(none_value.and(Some(20)), content="None")
}

///|
test "option error handling patterns" {
  let parse_int = (s : String) => {
    try {
      Some(@int.parse(s))
    } catch {
      _ => None
    }
  }
  
  let safe_divide = (a : Int, b : Int) => if b == 0 { None } else { Some(a / b) }
  
  // Test parsing
  inspect(parse_int("42"), content="Some(42)")
  inspect(parse_int("not a number"), content="None")
  
  // Test chained operations
  let result = parse_int("20")
    .bind(x => safe_divide(100, x))
  
  inspect(result, content="Some(5)")
  
  let error_result = parse_int("0")
    .bind(x => safe_divide(100, x))
  
  inspect(error_result, content="None")
}