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
test "integer arithmetic operations" {
  assert_eq(1 + 2, 3)
  assert_eq(5 - 3, 2)
  assert_eq(4 * 3, 12)
  assert_eq(10 / 2, 5)
  assert_eq(10 % 3, 1)
  assert_eq(Int::abs(-5), 5)
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(7), 7)
}

///|
test "integer comparison operations" {
  assert_eq(1 < 2, true)
  assert_eq(2 > 1, true)
  assert_eq(3 <= 3, true)
  assert_eq(4 >= 4, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 6, true)
}

///|
test "boolean logic operations" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(false || true, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "array basic operations" {
  let empty : Array[Int] = []
  let numbers = [1, 2, 3]
  let strings = ["hello", "world", "moonbit"]
  
  assert_eq(empty.length(), 0)
  assert_eq(numbers.length(), 3)
  assert_eq(strings.length(), 3)
  assert_eq(numbers[0], 1)
  assert_eq(strings[2], "moonbit")
}

///|
test "string concatenation and operations" {
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  assert_eq(combined, "hello world")
  assert_eq(str1.length(), 5)
  assert_eq(combined.length(), 11)
}

///|
test "array map transformation" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

///|
test "array filter operation" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4, 6, 8, 10])
}

///|
test "option type operations" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  assert_eq(some_value is Some(_), true)
  assert_eq(none_value is None, true)
  assert_eq(some_value.unwrap(), 42)
}

///|
test "result type operations" {
  let ok_result : Result[Int, String] = Ok(42)
  let err_result : Result[Int, String] = Err("error")
  
  assert_eq(ok_result.is_ok(), true)
  assert_eq(err_result.is_err(), true)
  assert_eq(ok_result.unwrap(), 42)
}