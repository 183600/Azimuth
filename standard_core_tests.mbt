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

// 标准核心模块测试用例
// 包含对 int、string、array 和 option 模块的基础功能测试

///|
test "int max_value and min_value" {
  assert_eq(2147483647, Int.max_value)
  assert_eq(-2147483648, Int.min_value)
}

///|
test "int abs function" {
  assert_eq(5, Int.abs(5))
  assert_eq(5, Int.abs(-5))
  assert_eq(0, Int.abs(0))
  assert_eq(2147483647, Int.abs(Int.max_value))
  assert_eq(-2147483648, Int.abs(Int.min_value)) // overflow case
}

///|
test "int to_be_bytes" {
  let bytes = 0x12345678.to_be_bytes()
  assert_eq(4, bytes.length())
  assert_eq(18, bytes[0])
  assert_eq(52, bytes[1])
  assert_eq(86, bytes[2])
  assert_eq(120, bytes[3])
}

///|
test "int to_le_bytes" {
  let bytes = 0x12345678.to_le_bytes()
  assert_eq(4, bytes.length())
  assert_eq(120, bytes[0])
  assert_eq(86, bytes[1])
  assert_eq(52, bytes[2])
  assert_eq(18, bytes[3])
}

///|
test "string default function" {
  let empty_str = String.default()
  assert_eq_string("", empty_str)
  assert_eq(0, empty_str.length())
}

///|
test "string basic operations" {
  let str1 = "Hello"
  let str2 = " World"
  let combined = str1 + str2
  assert_eq_string("Hello World", combined)
  assert_eq(11, combined.length())
  assert_eq(5, str1.length())
  assert_eq(6, str2.length())
}

///|
test "string character access" {
  let str = "MoonBit"
  assert_eq(77, str[0]) // 'M'
  assert_eq(111, str[1]) // 'o'
  assert_eq(111, str[2]) // 'o'
  assert_eq(110, str[3]) // 'n'
  assert_eq(66, str[4]) // 'B'
  assert_eq(105, str[5]) // 'i'
  assert_eq(116, str[6]) // 't'
}

///|
test "array zip_with function" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let add = (a, b) => a + b
  let result = Array.zip_with(arr1, arr2, add)
  assert_eq(3, result.length())
  assert_eq(5, result[0])
  assert_eq(7, result[1])
  assert_eq(9, result[2])
}

///|
test "array zip_with different lengths" {
  let arr1 = [1, 2, 3, 4]
  let arr2 = [5, 6]
  let add = (a, b) => a + b
  let result = Array.zip_with(arr1, arr2, add)
  assert_eq(2, result.length())
  assert_eq(6, result[0])
  assert_eq(8, result[1])
}

///|
test "array basic operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(5, arr.length())
  assert_eq(1, arr[0])
  assert_eq(5, arr[4])
  
  let new_arr = arr.push(6)
  assert_eq(6, new_arr.length())
  assert_eq(6, new_arr[5])
}

///|
test "option some operations" {
  let some_value = Option.Some(42)
  assert_true(Option.is_some(some_value))
  assert_false(Option.is_none(some_value))
  assert_eq(42, Option.unwrap(some_value))
}

///|
test "option none operations" {
  let none_value = Option.None
  assert_false(Option.is_some(none_value))
  assert_true(Option.is_none(none_value))
}

///|
test "option map function" {
  let some_value = Option.Some(5)
  let none_value = Option.None
  let double = (x) => x * 2
  
  let mapped_some = Option.map(some_value, double)
  let mapped_none = Option.map(none_value, double)
  
  assert_eq(10, Option.unwrap(mapped_some))
  assert_true(Option.is_none(mapped_none))
}

///|
test "option with_default function" {
  let some_value = Option.Some(42)
  let none_value = Option.None
  
  assert_eq(42, Option.with_default(some_value, 0))
  assert_eq(0, Option.with_default(none_value, 0))
}