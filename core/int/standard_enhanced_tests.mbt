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
test "int_division_edge_cases" {
  // Test division with positive numbers
  inspect(10 / 3, content="3")
  inspect(10 / 2, content="5")
  inspect(10 / 1, content="10")
  
  // Test division with negative numbers
  inspect(-10 / 3, content="-3")
  inspect(10 / -3, content="-3")
  inspect(-10 / -3, content="3")
  
  // Test division with zero
  inspect(0 / 5, content="0")
  inspect(0 / -5, content="0")
}

///|
test "int_modulo_operations" {
  // Test modulo with positive numbers
  inspect(10 % 3, content="1")
  inspect(10 % 2, content="0")
  inspect(10 % 1, content="0")
  
  // Test modulo with negative numbers
  inspect(-10 % 3, content="-1")
  inspect(10 % -3, content="1")
  inspect(-10 % -3, content="-1")
  
  // Test modulo with boundary values
  inspect(@int.max_value % 2, content="1")
  inspect(@int.min_value % 2, content="0")
}

///|
test "int_bitwise_shift_operations" {
  // Test left shift
  inspect(1 << 1, content="2")
  inspect(1 << 2, content="4")
  inspect(1 << 3, content="8")
  inspect(1 << 30, content="1073741824")
  
  // Test right shift
  inspect(8 >> 1, content="4")
  inspect(8 >> 2, content="2")
  inspect(8 >> 3, content="1")
  
  // Test shift with negative numbers
  inspect(-8 >> 1, content="-4")
  inspect(-8 >> 2, content="-2")
  
  // Test shift overflow behavior
  inspect(1 << 31, content="-2147483648")
}

///|
test "int_bitwise_logical_operations" {
  // Test AND operation
  inspect(0b1010 & 0b1100, content="8")
  inspect(0b1111 & 0b0000, content="0")
  inspect(0b1111 & 0b1111, content="15")
  
  // Test OR operation
  inspect(0b1010 | 0b1100, content="14")
  inspect(0b1111 | 0b0000, content="15")
  inspect(0b0000 | 0b0000, content="0")
  
  // Test XOR operation
  inspect(0b1010 ^ 0b1100, content="6")
  inspect(0b1111 ^ 0b1111, content="0")
  inspect(0b1111 ^ 0b0000, content="15")
  
  // Test NOT operation
  inspect(~0, content="-1")
  inspect(~1, content="-2")
  inspect(~(-1), content="0")
}

///|
test "int_comparison_chaining" {
  let x = 5
  let y = 10
  let z = 5
  
  // Test various comparisons
  assert_eq(x < y, true)
  assert_eq(x > y, false)
  assert_eq(x <= y, true)
  assert_eq(x >= y, false)
  assert_eq(x <= z, true)
  assert_eq(x >= z, true)
  assert_eq(x == z, true)
  assert_eq(x != y, true)
  assert_eq(x != z, false)
}

///|
test "int_arithmetic_edge_cases" {
  // Test addition overflow
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value + (-1), @int.max_value)
  
  // Test multiplication edge cases
  assert_eq(@int.max_value * 1, @int.max_value)
  assert_eq(@int.min_value * 1, @int.min_value)
  assert_eq(@int.max_value * 0, 0)
  assert_eq(@int.min_value * 0, 0)
  
  // Test subtraction edge cases
  assert_eq(@int.min_value - 1, @int.max_value)
  assert_eq(@int.max_value - (-1), @int.min_value)
}

///|
test "int_sign_operations" {
  // Test positive numbers
  assert_eq(5.signum(), 1)
  assert_eq(1.signum(), 1)
  assert_eq(@int.max_value.signum(), 1)
  
  // Test negative numbers
  assert_eq((-5).signum(), -1)
  assert_eq((-1).signum(), -1)
  assert_eq(@int.min_value.signum(), -1)
  
  // Test zero
  assert_eq(0.signum(), 0)
}

///|
test "int_clamp_operations" {
  let min_val = 5
  let max_val = 15
  
  // Test values within range
  assert_eq(10.clamp(min_val, max_val), 10)
  assert_eq(5.clamp(min_val, max_val), 5)
  assert_eq(15.clamp(min_val, max_val), 15)
  
  // Test values below range
  assert_eq(0.clamp(min_val, max_val), 5)
  assert_eq((-10).clamp(min_val, max_val), 5)
  
  // Test values above range
  assert_eq(20.clamp(min_val, max_val), 15)
  assert_eq(100.clamp(min_val, max_val), 15)
}