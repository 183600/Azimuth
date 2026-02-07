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
  inspect(10 + 5, content="15")
  inspect(10 - 3, content="7")
  inspect(6 * 7, content="42")
  inspect(15 / 3, content="5")
  inspect(17 % 4, content="1")
  assert_eq(2 + 3 * 4, 14)
  assert_eq((2 + 3) * 4, 20)
}

///|
test "int boundary values and overflow" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  assert_eq(Int::abs(-42), 42)
  assert_eq(Int::abs(0), 0)
}

///|
test "string basic operations" {
  let empty = ""
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
  assert_eq(String::length(combined), 11)
  assert_eq(String::length(empty), 0)
  inspect(String::char_at(combined, 0), content="'H'")
  inspect(String::char_at(combined, 5), content="' '")
}

///|
test "string comparison and utilities" {
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  assert_eq(str1 == str3, true)
  assert_eq(str1 == str2, false)
  assert_eq(str1 < str2, true)
  assert_eq(str2 > str1, true)
  inspect(String::default(), content="")
}

///|
test "list basic operations" {
  let empty_list : @list.List[Int] = @list.empty()
  let list1 = @list.cons(1, @list.cons(2, @list.cons(3, empty_list)))
  let list2 = @list.from_array([1, 2, 3])
  assert_eq(@list.length(list1), 3)
  assert_eq(@list.length(empty_list), 0)
  assert_eq(@list.nth(list1, 0)?, 1)
  assert_eq(@list.nth(list1, 2)?, 3)
}

///|
test "list transformations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let doubled = @list.map(numbers, fn(x) { x * 2 })
  let evens = @list.filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(@list.length(doubled), 5)
  assert_eq(@list.length(evens), 2)
  assert_eq(@list.nth(doubled, 0)?, 2)
  assert_eq(@list.nth(evens, 0)?, 2)
  assert_eq(@list.nth(evens, 1)?, 4)
}

///|
test "boolean operations" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
  assert_eq(true == true, true)
  assert_eq(true != false, true)
}

///|
test "option type operations" {
  let some_value : @option.Option[Int] = @option.Some(42)
  let none_value : @option.Option[Int] = @option.None
  assert_eq(@option.is_some(some_value), true)
  assert_eq(@option.is_none(some_value), false)
  assert_eq(@option.is_some(none_value), false)
  assert_eq(@option.is_none(none_value), true)
  assert_eq(@option.unwrap(some_value), 42)
}

///|
test "result type operations" {
  let ok_result : @result.Result[Int, String] = @result.Ok(100)
  let err_result : @result.Result[Int, String] = @result.Err("error")
  assert_eq(@result.is_ok(ok_result), true)
  assert_eq(@result.is_err(ok_result), false)
  assert_eq(@result.is_ok(err_result), false)
  assert_eq(@result.is_err(err_result), true)
  assert_eq(@result.unwrap(ok_result), 100)
}

///|
test "type conversions and casting" {
  let int_val = 42
  let uint_val = int_val.to_uint()
  let int64_val = int_val.to_int64()
  let double_val = int_val.to_double()
  inspect(uint_val, content="42")
  inspect(int64_val, content="42")
  inspect(double_val, content="42.0")
  assert_eq(uint_val.to_int(), 42)
}