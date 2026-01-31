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
test "int basic arithmetic operations" {
  assert_eq(1 + 2, 3)
  assert_eq(10 - 5, 5)
  assert_eq(3 * 4, 12)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
  assert_eq(-5 + 10, 5)
  assert_eq(-8 * 3, -24)
}

///|
test "int comparison and logical operations" {
  assert_eq(10 > 5, true)
  assert_eq(3 < 8, true)
  assert_eq(7 >= 7, true)
  assert_eq(4 <= 9, true)
  assert_eq(6 == 6, true)
  assert_eq(5 != 3, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
}

///|
test "string basic operations" {
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  assert_eq(empty.length(), 0)
  assert_eq(hello.length(), 5)
  assert_eq(hello + " " + world, "Hello World")
  assert_eq(hello.char_at(0), 'H')
  assert_eq(hello.char_at(4), 'o')
}

///|
test "string utility functions" {
  let text = "MoonBit Standard Library"
  
  assert_eq(text.contains("MoonBit"), true)
  assert_eq(text.contains("Rust"), false)
  assert_eq(text.starts_with("MoonBit"), true)
  assert_eq(text.ends_with("Library"), true)
  assert_eq(text.to_upper(), "MOONBIT STANDARD LIBRARY")
  assert_eq(text.to_lower(), "moonbit standard library")
}

///|
test "list basic operations" {
  let empty : @list.List[Int] = @list.empty()
  let list1 = @list.cons(1, @list.cons(2, @list.cons(3, empty)))
  let list2 = @list.from_array([4, 5, 6])
  
  assert_eq(@list.length(list1), 3)
  assert_eq(@list.length(list2), 3)
  assert_eq(@list.nth(list1, 0), Some(1))
  assert_eq(@list.nth(list1, 2), Some(3))
  assert_eq(@list.nth(list1, 3), None)
}

///|
test "list transformation operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = @list.map(numbers, fn(x) { x * 2 })
  assert_eq(@list.nth(doubled, 0), Some(2))
  assert_eq(@list.nth(doubled, 4), Some(10))
  
  let evens = @list.filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(@list.length(evens), 2)
  
  let sum = @list.fold(numbers, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  assert_eq(Option::is_some(some_value), true)
  assert_eq(Option::is_none(some_value), false)
  assert_eq(Option::is_some(none_value), false)
  assert_eq(Option::is_none(none_value), true)
  
  assert_eq(Option::unwrap_or(some_value, 0), 42)
  assert_eq(Option::unwrap_or(none_value, 0), 0)
  
  let mapped = Option::map(some_value, fn(x) { x * 2 })
  assert_eq(mapped, Some(84))
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("Error occurred")
  
  assert_eq(Result::is_ok(ok_result), true)
  assert_eq(Result::is_err(ok_result), false)
  assert_eq(Result::is_ok(err_result), false)
  assert_eq(Result::is_err(err_result), true)
  
  assert_eq(Result::unwrap_or(ok_result, 0), 42)
  assert_eq(Result::unwrap_or(err_result, 0), 0)
  
  let mapped_ok = Result::map(ok_result, fn(x) { x * 2 })
  assert_eq(mapped_ok, Ok(84))
}

///|
test "boolean logic and operations" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false && true, false)
  assert_eq(false && false, false)
  
  assert_eq(true || true, true)
  assert_eq(true || false, true)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  
  assert_eq(!true, false)
  assert_eq(!false, true)
  
  assert_eq(true ^ false, true)
  assert_eq(true ^ true, false)
  assert_eq(false ^ false, false)
}

///|
test "array basic operations" {
  let empty_array : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(empty_array.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let copied = numbers.copy()
  assert_eq(copied.length(), numbers.length())
  assert_eq(copied[2], numbers[2])
  
  let slice = numbers.slice(1, 4)
  assert_eq(slice.length(), 3)
  assert_eq(slice[0], 2)
  assert_eq(slice[2], 4)
}