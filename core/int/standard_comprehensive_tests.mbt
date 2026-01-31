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

using @builtin { inspect }

///|
test "int_basic_arithmetic" {
  inspect(5 + 3, content="8")
  inspect(10 - 4, content="6")
  inspect(7 * 6, content="42")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "int_negative_operations" {
  inspect(-5 + 3, content="-2")
  inspect(-10 - 4, content="-14")
  inspect(-7 * 6, content="-42")
  inspect(-15 / 3, content="-5")
  inspect(-17 % 5, content="-2")
}

///|
test "int_zero_operations" {
  inspect(0 + 5, content="5")
  inspect(0 - 5, content="-5")
  inspect(0 * 5, content="0")
  inspect(0 / 5, content="0")
  inspect(0 % 5, content="0")
}

///|
test "int_comparison_logic" {
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 >= 5, content="true")
  inspect(5 <= 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

///|
test "int_bitwise_operations" {
  inspect(12 & 10, content="8")  // 1100 & 1010 = 1000
  inspect(12 | 10, content="14") // 1100 | 1010 = 1110
  inspect(12 ^ 10, content="6")  // 1100 ^ 1010 = 0110
  inspect(-6, content="-6")      // Bitwise NOT equivalent
}

///|
test "int_shift_operations" {
  inspect(3 << 2, content="12")   // 3 * 2^2 = 12
  inspect(16 >> 2, content="4")   // 16 / 2^2 = 4
  inspect(1 << 0, content="1")    // No shift
  inspect(42 >> 0, content="42")  // No shift
}

///|
test "int_boundary_values" {
  inspect(2147483647, content="2147483647")
  inspect(-2147483648, content="-2147483648")
  inspect(2147483647 + 1, content="-2147483648")
  inspect(-2147483648 - 1, content="2147483647")
}

///|
test "int_absolute_values" {
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(2147483647), content="2147483647")
}

///|
test "int_multiplication_patterns" {
  inspect(2 * 2 * 2, content="8")
  inspect(5 * 5, content="25")
  inspect(3 * 1, content="3")
  inspect(10 * 1, content="10")
}

///|
test "int_mixed_operations" {
  inspect(2 + 3 * 4, content="14")        // Operator precedence
  inspect((2 + 3) * 4, content="20")      // Parentheses
  inspect(20 - 4 * 3, content="8")        // Mixed operations
  inspect(15 + 25 / 5, content="20")      // Division in expression
}