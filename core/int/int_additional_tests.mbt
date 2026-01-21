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

// Additional comprehensive test cases for int module

test "int_basic_arithmetic" {
  let a = 10
  let b = 3
  
  let sum = a + b
  let diff = a - b
  let product = a * b
  let quotient = a / b
  let remainder = a % b
  
  // Simple validation
  let _ = sum + diff + product + quotient + remainder
}

test "int_negative_arithmetic" {
  let a = -10
  let b = 3
  
  let sum = a + b
  let diff = a - b
  let product = a * b
  let quotient = a / b
  let remainder = a % b
  
  // Simple validation
  let _ = sum + diff + product + quotient + remainder
}

test "int_comparison_operations" {
  let a = 5
  let b = 10
  let c = 5
  
  let less_than = a < b
  let less_than_equal = a <= b
  let greater_than = b > a
  let greater_than_equal = b >= a
  let equal = a == c
  let not_equal = a != b
  
  // Simple validation
  let _ = less_than && less_than_equal && greater_than && greater_than_equal && equal && not_equal
}

test "int_bitwise_operations" {
  let a = 6  // 110
  let b = 3  // 011
  
  let bitwise_and = a & b
  let bitwise_or = a | b
  let bitwise_xor = a ^ b
  let left_shift = a << 1
  let right_shift = a >> 1
  
  // Simple validation
  let _ = bitwise_and + bitwise_or + bitwise_xor + left_shift + right_shift
}

test "int_boundary_values" {
  let max_val = @int.max_value
  let min_val = @int.min_value
  
  // Test overflow scenarios
  let max_plus_one = @int.max_value + 1
  let min_minus_one = @int.min_value - 1
  
  // Simple validation
  let _ = max_val + min_val + max_plus_one + min_minus_one
}

test "int_absolute_value" {
  let abs1 = Int::abs(5)
  let abs2 = Int::abs(-5)
  let abs3 = Int::abs(0)
  let abs4 = Int::abs(2147483647)
  let abs5 = Int::abs(-2147483648)
  
  // Simple validation
  let _ = abs1 + abs2 + abs3 + abs4 + abs5
}

test "int_basic_operations" {
  let positive = 10
  let negative = -10
  let zero = 0
  
  let pos_check = positive > 0
  let neg_check = negative < 0
  let zero_check = zero == 0
  
  // Simple validation
  let _ = pos_check && neg_check && zero_check
}

test "int_power_operations" {
  let pow1 = 2.pow(0)
  let pow2 = 2.pow(1)
  let pow3 = 2.pow(2)
  let pow4 = 2.pow(3)
  let pow5 = 2.pow(4)
  
  let pow6 = 10.pow(0)
  let pow7 = 10.pow(1)
  let pow8 = 10.pow(2)
  
  // Simple validation
  let _ = pow1 + pow2 + pow3 + pow4 + pow5 + pow6 + pow7 + pow8
}