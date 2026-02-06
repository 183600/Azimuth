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
test "int absolute value edge cases" {
  // Test absolute value with minimum value (edge case)
  inspect(abs(int.min_value), content="-2147483648")
  inspect(abs(-1), content="1")
  inspect(abs(1), content="1")
  inspect(abs(0), content="0")
}

///|
test "int bitwise operations comprehensive" {
  // Test complex bitwise operations
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(~10, content="-11")
  
  // Test with negative numbers
  inspect(-1 & 1, content="1")
  inspect(-1 | 0, content="-1")
  inspect(-2 ^ 1, content="-1")
}

///|
test "int multiplication overflow behavior" {
  // Test multiplication that causes overflow
  inspect(int.max_value * 2, content="-2")
  inspect(int.min_value * 2, content="0")
  inspect(int.max_value * -1, content="-2147483647") // min + 1
  inspect((int.min_value / 2) * 2, content="-2147483648")
}

///|
test "int division operations" {
  // Test various division cases
  inspect(10 / 1, content="10")
  inspect(10 / -1, content="-10")
  inspect(-10 / 1, content="-10")
  inspect(-10 / -1, content="10")
  inspect(0 / 1, content="0")
  inspect(0 / -1, content="0")
  inspect(10 / 3, content="3")
  inspect(-10 / 3, content="-3")
}

///|
test "int modulo operations edge cases" {
  // Test modulo with negative numbers
  inspect(7 % 3, content="1")
  inspect(-7 % 3, content="-1")
  inspect(7 % -3, content="1")
  inspect(-7 % -3, content="-1")
  
  // Test modulo boundaries
  inspect(int.max_value % 2, content="1")
  inspect(int.min_value % 2, content="0")
  inspect(0 % 1, content="0")
}

///|
test "int shift operations comprehensive" {
  // Test various shift amounts
  inspect(1 << 1, content="2")
  inspect(1 << 2, content="4")
  inspect(1 << 3, content="8")
  inspect(8 >> 1, content="4")
  inspect(8 >> 2, content="2")
  inspect(8 >> 3, content="1")
  
  // Test shifts with negative numbers
  inspect(-1 << 1, content="-2")
  inspect(-2 >> 1, content="-1")
  
  // Test large shifts (beyond word size)
  inspect(1 << 31, content="-2147483648")
  inspect(-2147483648 >> 31, content="-1")
}

///|
test "int comparison with edge values" {
  // Test comparisons with max and min values
  inspect(int.max_value > int.min_value, content="true")
  inspect(int.min_value < int.max_value, content="true")
  inspect(int.max_value >= int.max_value, content="true")
  inspect(int.min_value <= int.min_value, content="true")
  inspect(int.max_value == int.max_value, content="true")
  inspect(int.min_value != int.max_value, content="true")
}

///|
test "int multiplication operations edge cases" {
  // Test multiplication with zero
  inspect(0 * 0, content="0")
  inspect(0 * 1, content="0")
  inspect(0 * 5, content="0")
  
  // Test multiplication with one
  inspect(1 * 0, content="0")
  inspect(1 * 10, content="10")
  
  // Test multiplication with negative numbers
  inspect(-2 * 0, content="0")
  inspect(-2 * 1, content="-2")
  inspect(-2 * 2, content="-4")
  inspect(-2 * 3, content="-6")
}