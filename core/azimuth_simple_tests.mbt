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

// Azimuth 简化标准 MoonBit 测试用例
// 包含10个基本测试用例

// 定义断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected != actual) {
    let _ = expected - actual
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected != actual) {
    let _ = expected + actual
    @builtin.abort("String assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (!condition) {
    @builtin.abort("Expected true")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Expected false")
  }
}

///|
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(3, 1 + 2)
  assert_eq(7, 10 - 3)
  assert_eq(12, 3 * 4)
  assert_eq(5, 20 / 4)
  assert_eq(3, 15 % 4)
}

///|
test "integer_comparisons" {
  // 测试整数比较
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_true(5 >= 5)
  assert_false(5 <= 3)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  assert_eq(5, hello.length())
  assert_eq_string("Hello World", hello + " " + world)
  assert_true(hello == "Hello")
  assert_true(hello != "world")
}

///|
test "boolean_logic" {
  // 测试布尔逻辑
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  assert_false(!true)
  assert_true(!false)
}

///|
test "negative_numbers" {
  // 测试负数运算
  assert_eq(5, -5 + 10)
  assert_eq(6, -3 * -2)
  assert_eq(-5, -10 + 5)
  assert_eq(-4, -8 / 2)
}

///|
test "string_unicode" {
  // 测试Unicode字符串
  let chinese = "中文"
  let emoji = "🚀"
  let mixed = "Hello 世界"
  
  assert_eq(2, chinese.length())
  assert_eq(1, emoji.length())
  assert_eq(8, mixed.length())
  assert_eq_string("Hello 世界", mixed)
}

///|
test "complex_expressions" {
  // 测试复杂表达式
  assert_eq(21, (1 + 2) * (3 + 4))
  assert_eq(4, 10 - 3 * 2)
  assert_eq(4, 20 / (4 + 1))
  assert_eq(10, 5 + 10 / 2)
}

///|
test "string_concatenation" {
  // 测试字符串连接
  let part1 = "Moon"
  let part2 = "Bit"
  let part3 = "2024"
  
  assert_eq_string("MoonBit", part1 + part2)
  assert_eq_string("Moon Bit", part1 + " " + part2)
  assert_eq_string("MoonBit2024", part1 + part2 + part3)
  assert_eq_string("Moon Bit 2024", part1 + " " + part2 + " " + part3)
}

///|
test "operator_precedence" {
  // 测试运算符优先级
  assert_eq(14, 2 + 3 * 4)
  assert_eq(10, 2 * 3 + 4)
  assert_eq(4, 10 - 2 * 3)
  assert_eq(8, 10 / 2 + 3)
}

///|
test "boundary_values" {
  // 测试边界值
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 5)
  assert_eq(0, 5 * 0)
  assert_eq(0, 0 / 1)
  assert_eq(0, 1 - 1)
}