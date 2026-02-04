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
test "int_basic_arithmetic_operations" {
  // 测试整数基本算术运算
  inspect(5 + 3, content="8")
  inspect(10 - 4, content="6")
  inspect(7 * 6, content="42")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  inspect(-5 + 8, content="3")
  inspect(-10 * -3, content="30")
}

///|
test "int_boundary_values" {
  // 测试整数边界值
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect(Int::max_value + 1, content="-2147483648")  // 溢出测试
  inspect(Int::min_value - 1, content="2147483647")   // 溢出测试
  inspect(0, content="0")
  inspect(-1, content="-1")
  inspect(1, content="1")
}

///|
test "int_comparison_operations" {
  // 测试整数比较运算
  inspect(5 > 3, content="true")
  inspect(3 > 5, content="false")
  inspect(5 >= 5, content="true")
  inspect(3 < 5, content="true")
  inspect(5 <= 5, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="false")
  inspect(-5 < -3, content="true")
}

///|
test "int_bitwise_operations" {
  // 测试整数位运算
  inspect(5 & 3, content="1")    // 101 & 011 = 001
  inspect(5 | 3, content="7")    // 101 | 011 = 111
  inspect(5 ^ 3, content="6")    // 101 ^ 011 = 110
  inspect(5 << 1, content="10")  // 1010
  inspect(5 >> 1, content="2")   // 0010
  inspect(~5, content="-6")      // 按位取反
}

///|
test "int_to_string_conversion" {
  // 测试整数转字符串
  inspect(0.to_string(), content="0")
  inspect(42.to_string(), content="42")
  inspect(-42.to_string(), content="-42")
  inspect(Int::max_value.to_string(), content="2147483647")
  inspect(Int::min_value.to_string(), content="-2147483648")
}

///|
test "int_shift_operations" {
  // 测试位移运算的边界情况
  inspect(1 << 30, content="1073741824")  // 最大安全左移
  inspect(1 << 31, content="-2147483648") // 溢出情况
  inspect(-2147483648 >> 31, content="-1") // 最小值右移
  inspect(1024 >> 10, content="1")         // 精确右移到1
}

///|
test "int_modulo_operations" {
  // 测试取模运算的各种情况
  inspect(10 % 3, content="1")
  inspect(10 % -3, content="1")
  inspect(-10 % 3, content="-1")
  inspect(-10 % -3, content="-1")
  inspect(0 % 5, content="0")
  inspect(5 % 1, content="0")
}

///|
test "int_multiplication_overflow" {
  // 测试乘法溢出情况
  inspect(46340 * 46340, content="2147395600")  // 接近最大值
  inspect(46341 * 46341, content="-2147479015") // 溢出
  inspect(-46340 * 46340, content="-2147395600")
  inspect(-46341 * 46341, content="-2147479015")
}

///|
test "int_division_edge_cases" {
  // 测试除法边界情况
  inspect(7 / 2, content="3")    // 整数除法截断
  inspect(-7 / 2, content="-3")  // 负数除法截断
  inspect(7 / -2, content="-3")
  inspect(-7 / -2, content="3")
  inspect(1 / 2, content="0")    // 小数部分被截断
}

///|
test "int_hexadecimal_literals" {
  // 测试十六进制字面量
  inspect(0xFF, content="255")
  inspect(0x00, content="0")
  inspect(0x80, content="128")
  inspect(0x7F, content="127")
  inspect(0xFFFFFFFF, content="-1") // 32位溢出
}