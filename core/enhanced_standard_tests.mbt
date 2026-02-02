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

// 增强标准测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(5, 2 + 3)
  assert_eq(6, 10 - 4)
  assert_eq(12, 3 * 4)
  assert_eq(3, 10 / 3)
  assert_eq(1, 10 % 3)
}

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  assert_false(!true)
  assert_true(!false)
}

test "string_comparison" {
  // 字符串比较测试
  assert_eq("", "")
  assert_eq("Hello", "Hello")
  assert_true("Hello" != "World")
  assert_true("a" < "b")
  assert_false("z" < "a")
}

test "comparison_operations" {
  // 比较操作测试
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_false(5 < 3)
  assert_true(3 <= 3)
  assert_true(5 == 5)
  assert_false(5 == 3)
  assert_true(5 != 3)
  assert_false(5 != 5)
}

test "integer_boundary_values" {
  // 整数边界值测试
  assert_eq(0, 0)
  assert_eq(-1, -1)
  assert_eq(1, 1)
  assert_eq(2147483647, 2147483647)  // Int 最大值
  assert_eq(-2147483648, -2147483648)  // Int 最小值
}

test "char_comparison" {
  // 字符比较测试
  assert_eq('A', 'A')
  assert_true('a' < 'b')
  assert_true('A' < 'a')
  assert_true('0' < '9')
  assert_false('z' < 'a')
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, -2 - 3)
  assert_eq(5, -2 * -3)
  assert_eq(-2, 6 / -3)
  assert_eq(0, 5 % 5)
  assert_eq(-1, -5 % 2)
}

test "complex_expressions" {
  // 复杂表达式测试
  assert_eq(10, 2 + 3 * 4 - 6)
  assert_eq(14, (2 + 3) * 4 - 6)
  assert_eq(20, 2 + 3 * (4 + 2))
  assert_eq(1, 10 / 3 + 10 % 3)
  assert_eq(0, (5 + 5) % 10)
}