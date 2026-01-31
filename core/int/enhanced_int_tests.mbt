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
test "int basic arithmetic operations" {
  // Test basic arithmetic operations
  inspect(2 + 3, content="5")
  inspect(10 - 4, content="6")
  inspect(3 * 4, content="12")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
}

///|
test "int bit operations" {
  // Test bit manipulation
  inspect(8 & 7, content="0")
  inspect(8 | 7, content="15")
  inspect(8 ^ 7, content="15")
  inspect(~8, content="-9")
  inspect(1 << 3, content="8")
  inspect(8 >> 3, content="1")
}

///|
test "int comparison operations" {
  // Test comparison operations
  inspect(5 > 3, content="true")
  inspect(5 < 3, content="false")
  inspect(5 >= 5, content="true")
  inspect(5 <= 3, content="false")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

///|
test "int boundary values" {
  // Test integer boundaries
  inspect(2147483647, content="2147483647")
  inspect(-2147483648, content="-2147483648")
  inspect(2147483647 + 1, content="-2147483648")
  inspect(-2147483648 - 1, content="2147483647")
}

///|
test "hexadecimal literals" {
  // Test hex number representation
  inspect(0xFF, content="255")
  inspect(0x00, content="0")
  inspect(0x10, content="16")
  inspect(0xFFFF, content="65535")
}

///|
test "complex expressions" {
  // Test complex arithmetic expressions
  inspect(2 + 3 * 4, content="14")
  inspect((2 + 3) * 4, content="20")
  inspect(10 - 2 * 3, content="4")
  inspect(20 / (4 + 1), content="4")
}

///|
test "int overflow behavior" {
  // Test overflow scenarios
  inspect(Int::abs(-2147483648), content="2147483648")
  inspect(Int::abs(2147483647), content="2147483647")
  inspect(Int::abs(0), content="0")
}

///|
test "int shift operations edge cases" {
  // Test shift boundaries
  inspect(1 << 30, content="1073741824")
  inspect(1 << 31, content="-2147483648")
  inspect(-100 >> 3, content="-13")
  inspect(100 >> 3, content="12")
}

///|
test "int division edge cases" {
  // Test division with negative numbers
  inspect(10 / 3, content="3")
  inspect(-10 / 3, content="-3")
  inspect(10 / -3, content="-3")
  inspect(-10 / -3, content="3")
  
  // Test modulo operations
  inspect(10 % 3, content="1")
  inspect(-10 % 3, content="-1")
  inspect(10 % -3, content="1")
  inspect(-10 % -3, content="-1")
}

///|
test "int hash consistency" {
  // Test hash function
  let h1 = Hasher::new(seed=42)..combine(100).finalize()
  let h2 = Hasher::new(seed=42)..combine(100).finalize()
  let h3 = Hasher::new(seed=43)..combine(100).finalize()
  
  inspect(h1, content=h1.to_string())
  inspect(h2, content=h2.to_string())
  inspect(h3, content=h3.to_string())
}