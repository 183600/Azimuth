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
test "boolean logic operations" {
  // Test basic boolean operations
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
}

///|
test "integer arithmetic operations" {
  // Test basic arithmetic
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

///|
test "string operations" {
  // Test string concatenation and comparison
  let str1 = "Hello"
  let str2 = "World"
  let result = str1 + ", " + str2
  inspect(result, content="Hello, World")
  assert_eq(str1 == "Hello", true)
  assert_eq(str1 != str2, true)
}

///|
test "array operations" {
  // Test array creation and access
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
}

///|
test "option type operations" {
  // Test option type handling
  let some_value : Int = 42
  let some_option = Some(some_value)
  let none_option : Option[Int] = None
  
  inspect(some_option, content="Some(42)")
  inspect(none_option, content="None")
}

///|
test "list operations" {
  // Test list creation and basic operations
  let list = @list.from_array([1, 2, 3, 4, 5])
  inspect(list.length(), content="5")
  inspect(list.head(), content="Some(1)")
  inspect(list.last(), content="Some(5)")
}

///|
test "comparison operations" {
  // Test various comparison operations
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
}

///|
test "type conversions" {
  // Test type conversions between different numeric types
  inspect(10.to_int64(), content="10")
  inspect(10.to_uint(), content="10")
  inspect(10.to_double(), content="10.0")
}

///|
test "bitwise operations" {
  // Test bitwise operations
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(~10, content="-11")
}

///|
test "conditional expressions" {
  // Test conditional expressions
  let x = 10
  let y = 20
  let result = if x < y { "x is less than y" } else { "x is greater than or equal to y" }
  inspect(result, content="x is less than y")
}

// 简单的测试示例

test "simple_add" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, Test!", greet("Test"))
}