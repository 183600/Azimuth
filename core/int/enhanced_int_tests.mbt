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

test "int_basic_arithmetic" {
  // 测试整数基本算术运算
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

test "int_negative_arithmetic" {
  // 测试负数算术运算
  inspect(-10 + 5, content="-5")
  inspect(-10 - 5, content="-15")
  inspect(-10 * 5, content="-50")
  inspect(-10 / 3, content="-3")
  inspect(-10 % 3, content="-1")
}

test "int_bit_operations" {
  // 测试整数位运算
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(10 << 2, content="40")
  inspect(10 >> 2, content="2")
}

test "int_hexadecimal_values" {
  // 测试十六进制值
  inspect(0xFF, content="255")
  inspect(0x100, content="256")
  inspect(0xFFFF, content="65535")
  inspect(0x123456, content="1193046")
}

test "int_boundary_values" {
  // 测试整数边界值
  inspect(2147483647, content="2147483647")
  inspect(-2147483648, content="-2147483648")
  inspect(2147483647 + 1, content="-2147483648")
  inspect(-2147483648 - 1, content="2147483647")
}

test "int_zero_operations" {
  // 测试零的操作
  inspect(0 + 10, content="10")
  inspect(0 - 10, content="-10")
  inspect(0 * 10, content="0")
  inspect(0 / 10, content="0")
  inspect(0 % 10, content="0")
}

test "int_comparison_operations" {
  // 测试整数比较运算
  inspect(10 > 5, content="true")
  inspect(10 < 5, content="false")
  inspect(10 >= 10, content="true")
  inspect(10 <= 5, content="false")
  inspect(10 == 10, content="true")
  inspect(10 != 5, content="true")
}

test "int_complex_expressions" {
  // 测试复杂表达式
  inspect((10 + 5) * (8 - 3), content="75")
  inspect(10 + 5 * 8 - 3, content="47")
  inspect(10 * (5 + 8) - 3, content="127")
  inspect((10 + 5) * 8 - 3, content="117")
}