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

// 基础核心模块测试用例
// 包含最基本的运算和操作测试

///|
test "basic arithmetic operations" {
  assert_eq(5, 2 + 3)
  assert_eq(1, 5 - 4)
  assert_eq(6, 2 * 3)
  assert_eq(2, 6 / 3)
  assert_eq(1, 7 % 3)
}

///|
test "comparison operations" {
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(5 >= 5)
  assert_false(3 >= 5)
  assert_true(3 < 5)
  assert_false(5 < 3)
  assert_true(5 <= 5)
  assert_false(5 <= 3)
  assert_true(5 == 5)
  assert_false(5 == 3)
  assert_false(5 != 5)
  assert_true(5 != 3)
}

///|
test "boolean logic" {
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  assert_false(!true)
  assert_true(!false)
}

///|
test "string operations" {
  assert_eq_string("Hello World", "Hello" + " " + "World")
  assert_eq_string("", "" + "")
  assert_eq_string("abc", "a" + "b" + "c")
  assert_eq_string("123", "1" + "2" + "3")
}

///|
test "array operations" {
  let arr = [1, 2, 3]
  assert_eq(1, arr[0])
  assert_eq(2, arr[1])
  assert_eq(3, arr[2])
  
  let arr2 = [4, 5, 6, 7, 8]
  assert_eq(4, arr2[0])
  assert_eq(8, arr2[4])
}

///|
test "negative numbers" {
  assert_eq(-5, -5)
  assert_eq(5, -(-5))
  assert_eq(0, -0)
  assert_eq(-10, -5 - 5)
  assert_eq(5, -10 + 15)
}

///|
test "zero operations" {
  assert_eq(0, 0)
  assert_eq(5, 5 + 0)
  assert_eq(5, 5 - 0)
  assert_eq(0, 5 * 0)
  assert_eq(0, 0 * 5)
  assert_eq(0, 0 / 5)
}

///|
test "operator precedence" {
  assert_eq(14, 2 + 3 * 4)
  assert_eq(20, (2 + 3) * 4)
  assert_eq(2, 10 - 4 * 2)
  assert_eq(12, (10 - 4) * 2)
  assert_eq(6, 20 / 2 + 1)
  assert_eq(4, 20 / (2 + 3))
}

///|
test "complex expressions" {
  let a = 10
  let b = 20
  let c = 30
  
  assert_eq(60, a + b + c)
  assert_eq(-40, a - b - c)
  assert_eq(600, a * b * c / 10)
  assert_eq(0, a * b * c % 10)
}

///|
test "string and number conversion" {
  let num = 42
  let str = "42"
  
  // 简单的字符串连接测试
  assert_eq_string("The answer is 42", "The answer is " + num.to_string())
  assert_eq_string("42 is the answer", "42" + " is the answer")
}