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

// 高质量 MoonBit 标准库测试用例集合
// 包含 8 个核心功能测试，使用标准 MoonBit 测试语法

test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(hello + " " + world, content="Hello World")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试数组长度和访问
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

test "int_boundary_values" {
  // 测试整数边界值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

test "int_arithmetic_operations" {
  // 测试整数算术运算
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

test "int_bit_operations" {
  // 测试整数位运算
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(10 << 2, content="40")
  inspect(10 >> 2, content="2")
}

test "int_comparison_operations" {
  // 测试整数比较运算
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
}

test "int_hexadecimal_operations" {
  // 测试十六进制整数操作
  inspect(0x12345678, content="305419896")
  inspect(0x7fffffff, content="2147483647")
  inspect(0x80000000, content="-2147483648")
  inspect(-0x80000000, content="-2147483648")
}

test "int_overflow_behavior" {
  // 测试整数溢出行为
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
}