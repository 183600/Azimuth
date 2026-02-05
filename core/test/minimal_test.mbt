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
test "simple arithmetic test" {
  assert_eq(2 + 2, 4)
  assert_eq(5 - 3, 2)
  assert_eq(3 * 4, 12)
  assert_eq(10 / 2, 5)
}

///|
test "option basic test" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_true(some_value is Some(_))
  assert_true(none_value is None)
  
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

///|
test "string operations test" {
  let hello = "Hello"
  let world = "World"
  
  assert_eq(hello.length(), 5)
  assert_eq(world.length(), 5)
  
  let combined = hello + " " + world
  assert_eq(combined, "Hello World")
}

///|
test "boolean operations test" {
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

///|
test "array operations test" {
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}