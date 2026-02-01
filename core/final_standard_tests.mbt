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

// 标准 MoonBit 测试用例 - 8个核心功能测试

///|
test "integer arithmetic operations" {
  assert_eq(42 + 17, 59)
  assert_eq(42 - 17, 25)
  assert_eq(6 * 7, 42)
  assert_eq(20 / 4, 5)
  assert_eq(21 % 4, 1)
}

///|
test "string operations" {
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  assert_eq(combined, "Hello, World!")
  assert_eq(hello.length(), 5)
  assert_eq(world.length(), 5)
}

///|
test "boolean logic operations" {
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

///|
test "comparison operations" {
  let a = 10
  let b = 20
  let c = 10
  assert_eq(a < b, true)
  assert_eq(b > a, true)
  assert_eq(a <= c, true)
  assert_eq(a >= c, true)
  assert_eq(a == c, true)
  assert_eq(a != b, true)
}

///|
test "option type operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
  assert_eq(some_value.map(x => x * 2), Some(84))
  assert_eq(none_value.map(x => x * 2), None)
}

///|
test "array basic operations" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

///|
test "result type operations" {
  let success : Result[Int, String] = Ok(42)
  let failure : Result[Int, String] = Err("error")
  assert_eq(success.unwrap_or(0), 42)
  assert_eq(failure.unwrap_or(0), 0)
  assert_eq(success.map(x => x * 2), Ok(84))
  assert_eq(failure.map(x => x * 2), Err("error"))
}

///|
test "tuple operations" {
  let pair = (42, "hello")
  let (num, str) = pair
  assert_eq(num, 42)
  assert_eq(str, "hello")
  let triple = (1, 2, 3)
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
}