// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "integer basic operations" {
  assert_eq(1 + 2, 3)
  assert_eq(5 - 3, 2)
  assert_eq(4 * 3, 12)
  assert_eq(10 / 2, 5)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(7), 7)
}

///|
test "integer comparisons" {
  assert_eq(1 < 2, true)
  assert_eq(2 > 1, true)
  assert_eq(3 <= 3, true)
  assert_eq(4 >= 4, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 6, true)
}

///|
test "boolean logic" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  inspect(greeting, content="\"Hello World\"")
  assert_eq(hello.length(), 5)
  assert_eq(greeting.length(), 11)
}

///|
test "array basic operations" {
  let empty : Array[Int] = []
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(empty.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
}

///|
test "list basic operations" {
  let empty_list = @list.empty()
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  assert_eq(empty_list.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(numbers.head(), Some(1))
  assert_eq(empty_list.head(), None)
  assert_eq(empty_list.is_empty(), true)
  assert_eq(numbers.is_empty(), false)
}

///|
test "list map operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
}

///|
test "option type operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  assert_eq(some_value.unwrap(), 42)
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "option type map" {
  let some_value : Int? = Some(5)
  let none_value : Int? = None
  
  let mapped_some = some_value.map(x => x * 2)
  assert_eq(mapped_some, Some(10))
  
  let mapped_none = none_value.map(x => x * 2)
  assert_eq(mapped_none, None)
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error message")
  
  assert_eq(ok_result.is_ok(), true)
  assert_eq(ok_result.is_err(), false)
  assert_eq(err_result.is_ok(), false)
  assert_eq(err_result.is_err(), true)
  
  assert_eq(ok_result.unwrap(), 42)
  assert_eq(ok_result.unwrap_or(0), 42)
  assert_eq(err_result.unwrap_or(0), 0)
  assert_eq(err_result.unwrap_err(), "error message")
}