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

// 简单的核心模块测试用例
// 包含基础功能测试

///|
test "integer arithmetic" {
  assert_eq(5, 2 + 3)
  assert_eq(10, 4 * 2 + 2)
  assert_eq(0, 5 - 5)
  assert_eq(6, 12 / 2)
}

///|
test "boolean operations" {
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

///|
test "string concatenation" {
  assert_eq_string("Hello World", "Hello" + " " + "World")
  assert_eq_string("", "" + "")
  assert_eq_string("abc", "a" + "b" + "c")
}

///|
test "array operations" {
  let arr = [1, 2, 3]
  assert_eq(3, arr.length())
  assert_eq(1, arr[0])
  assert_eq(2, arr[1])
  assert_eq(3, arr[2])
  
  let new_arr = arr.push(4)
  assert_eq(4, new_arr.length())
  assert_eq(4, new_arr[3])
}

///|
test "comparison operations" {
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_false(5 < 3)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_false(5 == 3)
  assert_false(5 != 5)
  assert_true(5 != 3)
}

///|
test "mathematical functions" {
  assert_eq(5, abs(5))
  assert_eq(5, abs(-5))
  assert_eq(0, abs(0))
  
  assert_eq(4, min(4, 7))
  assert_eq(7, max(4, 7))
}

///|
test "string length" {
  assert_eq(0, "".length())
  assert_eq(5, "Hello".length())
  assert_eq(11, "Hello World".length())
}

///|
test "array indexing" {
  let arr = [10, 20, 30, 40, 50]
  assert_eq(10, arr[0])
  assert_eq(20, arr[1])
  assert_eq(30, arr[2])
  assert_eq(40, arr[3])
  assert_eq(50, arr[4])
}

///|
test "nested operations" {
  let a = 10
  let b = 20
  let c = 30
  
  // 复合运算
  let result = (a + b) * c / 10
  assert_eq(90, result)
  
  // 字符串和数字结合
  let str = "Result: " + result.to_string()
  assert_eq_string("Result: 90", str)
}

///|
test "edge cases" {
  // 边界值测试
  assert_eq(0, 0)
  assert_eq(-1, -1)
  assert_eq(1, 1)
  
  // 空字符串
  assert_eq_string("", "")
  
  // 单元素数组
  let single = [42]
  assert_eq(1, single.length())
  assert_eq(42, single[0])
}