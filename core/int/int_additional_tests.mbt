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
test "int max_value and min_value" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // Test that max_value + 1 overflows to min_value
  let overflow = @int.max_value + 1
  inspect(overflow, content="-2147483648")
  
  // Test that min_value - 1 overflows to max_value  
  let underflow = @int.min_value - 1
  inspect(underflow, content="2147483647")
}

///|
test "int abs function" {
  // Test positive numbers
  inspect(@int.abs(5), content="5")
  inspect(@int.abs(100), content="100")
  
  // Test negative numbers
  inspect(@int.abs(-5), content="5")
  inspect(@int.abs(-100), content="100")
  
  // Test zero
  inspect(@int.abs(0), content="0")
  
  // Test edge case: min_value (special case due to two's complement)
  inspect(@int.abs(@int.min_value), content="-2147483648")
}

///|
test "int to_be_bytes conversion" {
  // Test big-endian byte conversion
  let bytes1 = @int.to_be_bytes(0x12345678)
  inspect(bytes1, content="[18, 52, 86, 120]")
  
  let bytes2 = @int.to_be_bytes(-1)
  inspect(bytes2, content="[255, 255, 255, 255]")
  
  let bytes3 = @int.to_be_bytes(0)
  inspect(bytes3, content="[0, 0, 0, 0]")
}

///|
test "int to_le_bytes conversion" {
  // Test little-endian byte conversion
  let bytes1 = @int.to_le_bytes(0x12345678)
  inspect(bytes1, content="[120, 86, 52, 18]")
  
  let bytes2 = @int.to_le_bytes(-1)
  inspect(bytes2, content="[255, 255, 255, 255]")
  
  let bytes3 = @int.to_le_bytes(0)
  inspect(bytes3, content="[0, 0, 0, 0]")
}

///|
test "int arithmetic operations" {
  // Test basic arithmetic
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
  
  // Test with negative numbers
  inspect(-10 + 5, content="-5")
  inspect(-10 - 5, content="-15")
  inspect(-10 * 5, content="-50")
  inspect(-10 / 3, content="-3")
  inspect(-10 % 3, content="-1")
}

///|
test "int comparison operations" {
  // Test equality
  assert_eq(5, 5)
  assert_not_eq(5, 10)
  
  // Test ordering
  assert_true(5 < 10)
  assert_true(5 <= 10)
  assert_true(5 <= 5)
  assert_true(10 > 5)
  assert_true(10 >= 5)
  assert_true(10 >= 10)
  
  // Test with negative numbers
  assert_true(-10 < -5)
  assert_true(-10 <= -5)
  assert_true(-5 > -10)
  assert_true(-5 >= -10)
}