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
test "overflow" {
  // signed integer expected to be negative
  inspect(0xC2B2AE3D, content="-1028477379")
  inspect(0x85EBCA77, content="-2048144777")
  inspect(0x80000000, content="-2147483648")
  // here it works as expected due to overflow
  inspect(-0x80000000, content="-2147483648")
  inspect(0x7fff_ffff, content="2147483647")
  inspect(-0x7fff_ffff, content="-2147483647")
}

///|
test "hash" {
  inspect(Hasher::new(seed=0)..combine(0).finalize(), content="148298089")
  inspect(Hasher::new(seed=0)..combine(1).finalize(), content="-205818221")
  inspect(Hasher::new(seed=0)..combine(2).finalize(), content="527729046")
  inspect(Hasher::new(seed=0)..combine(3).finalize(), content="-2136036233")
  inspect(Hasher::new(seed=0)..combine(4).finalize(), content="-43556126")
  inspect(Hasher::new(seed=0)..combine(5).finalize(), content="-2146748363")
  inspect(Hasher::new(seed=0)..combine(6).finalize(), content="-1455877728")
  inspect(Hasher::new(seed=0)..combine(7).finalize(), content="-427046664")
  inspect(Hasher::new(seed=0)..combine(8).finalize(), content="-1629857596")
  inspect(Hasher::new(seed=0)..combine(9).finalize(), content="970764038")
  inspect(
    Hasher::new(seed=0)..combine(0x7fffffff).finalize(),
    content="1225377669",
  )
  inspect(
    Hasher::new(seed=0)..combine(0x7fffffff + 1).finalize(),
    content="1509677505",
  )
}

///|
test "abs function coverage" {
  // Test cases for abs function
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
  assert_eq(Int::abs(@int.min_value), @int.min_value.abs())
  assert_eq(Int::abs(@int.max_value), @int.max_value)
}

///|
test "to_le_bytes" {
  let buffer = @buffer.new()
  buffer.write_int_le(0x12345678)
  let bytes = buffer.contents()
  inspect(
    bytes,
    content=(
      #|b"xV4\x12"
    ),
  )
}

///|
test "to_be_bytes" {
  let buffer = @buffer.new()
  buffer.write_int_be(0x12345678)
  let bytes = buffer.contents()
  inspect(
    bytes,
    content=(
      #|b"\x124Vx"
    ),
  )
}

///|
test "int boundary values" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
}

///|
test "int arithmetic operations" {
  inspect(10 + 5, content="15")
  inspect(10 - 5, content="5")
  inspect(10 * 5, content="50")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
}

///|
test "int bit operations" {
  inspect(10 & 12, content="8")
  inspect(10 | 12, content="14")
  inspect(10 ^ 12, content="6")
  inspect(~10, content="-11")
  inspect(10 << 2, content="40")
  inspect(10 >> 2, content="2")
}

///|
test "int comparison operations" {
  assert_eq(10 > 5, true)
  assert_eq(10 < 5, false)
  assert_eq(10 >= 10, true)
  assert_eq(10 <= 5, false)
  assert_eq(10 == 10, true)
  assert_eq(10 != 5, true)
}

///|
test "int type conversions" {
  inspect(10.to_int64(), content="10")
  inspect(10.to_uint(), content="10")
  inspect((-10).to_uint(), content="4294967286")
  inspect(10.to_double(), content="10.0")
}

///|
test "int edge cases" {
  // Test minimum value operations
  assert_eq(@int.min_value + 1, -2147483647)
  assert_eq(@int.min_value - 1, @int.max_value)
  
  // Test maximum value operations
  assert_eq(@int.max_value - 1, 2147483646)
  assert_eq(@int.max_value + 1, @int.min_value)
  
  // Test zero operations
  assert_eq(0 + @int.max_value, @int.max_value)
  assert_eq(0 - @int.min_value, @int.min_value)
  assert_eq(0 * @int.max_value, 0)
}

///|
test "int power and exponentiation" {
  inspect(Int::pow(2, 0), content="1")
  inspect(Int::pow(2, 1), content="2")
  inspect(Int::pow(2, 10), content="1024")
  inspect(Int::pow(3, 3), content="27")
  inspect(Int::pow(5, 2), content="25")
}

///|
test "int division edge cases" {
  // Test division by positive numbers
  assert_eq(10 / 3, 3)
  assert_eq(-10 / 3, -3)
  assert_eq(10 / -3, -3)
  assert_eq(-10 / -3, 3)
  
  // Test modulo operations
  assert_eq(10 % 3, 1)
  assert_eq(-10 % 3, -1)
  assert_eq(10 % -3, 1)
  assert_eq(-10 % -3, -1)
  
  // Test division with same numbers
  assert_eq(5 / 5, 1)
  assert_eq(-5 / -5, 1)
  assert_eq(0 / 5, 0)
}

///|
test "int shift operations edge cases" {
  // Test left shift boundaries
  assert_eq(1 << 30, 1073741824)
  assert_eq(1 << 31, -2147483648)  // Overflow to min value
  
  // Test right shift with positive and negative
  assert_eq(100 >> 3, 12)
  assert_eq(-100 >> 3, -13)
  
  // Test shift by zero
  assert_eq(42 << 0, 42)
  assert_eq(42 >> 0, 42)
}

///|
test "int mixed operations precedence" {
  // Test operator precedence
  assert_eq(2 + 3 * 4, 14)
  assert_eq((2 + 3) * 4, 20)
  assert_eq(10 - 2 * 3, 4)
  assert_eq(10 + 20 / 5, 14)
  
  // Test bit operations with arithmetic
  assert_eq(5 | 3 + 2, 5 | 5)
  assert_eq(5 & 3 << 1, 5 & 6)
}

///|
test "int overflow behavior comprehensive" {
  // Test addition overflow
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  
  // Test multiplication overflow
  assert_eq(@int.max_value * 2, -2)
  assert_eq((@int.max_value / 2) * 2, @int.max_value - 1)
  
  // Test subtraction overflow
  assert_eq(@int.min_value - @int.max_value, 1)
  assert_eq(0 - @int.min_value, @int.min_value)
}

///|
test "int bit manipulation advanced" {
  // Test bit counting and manipulation
  assert_eq(0.bit_count(), 0)
  assert_eq(1.bit_count(), 1)
  assert_eq(15.bit_count(), 4)  // 0b1111
  assert_eq(16.bit_count(), 1)  // 0b10000
  
  // Test leading zeros
  assert_eq(0.leading_zeros(), 32)
  assert_eq(1.leading_zeros(), 31)
  assert_eq(@int.max_value.leading_zeros(), 0)
  
  // Test trailing zeros
  assert_eq(0.trailing_zeros(), 32)
  assert_eq(1.trailing_zeros(), 0)
  assert_eq(8.trailing_zeros(), 3)
}

///|
test "int sign extension and bitwise operations" {
  // Test sign extension in right shifts
  assert_eq(-1 >> 1, -1)
  assert_eq(-2 >> 1, -1)
  assert_eq(-4 >> 2, -1)
  assert_eq(-8 >> 3, -1)
  
  // Test bitwise NOT boundaries
  assert_eq(~@int.max_value, @int.min_value)
  assert_eq(~@int.min_value, @int.max_value)
  assert_eq(~0, -1)
  assert_eq(~-1, 0)
}
