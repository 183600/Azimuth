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
test "string basic operations" {
  // Test string concatenation
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  inspect(combined, content="Hello World")
}

///|
test "array basic operations" {
  // Test array creation and access
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

///|
test "boolean logic operations" {
  // Test boolean operations
  let t = true
  let f = false
  inspect(t && t, content="true")
  inspect(t && f, content="false")
  inspect(t || f, content="true")
  inspect(f || f, content="false")
  inspect(!t, content="false")
  inspect(!f, content="true")
}

///|
test "tuple operations" {
  // Test tuple creation and access
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  inspect(pair.0, content="42")
  inspect(pair.1, content="answer")
  inspect(triple.0, content="1")
  inspect(triple.1, content="2")
  inspect(triple.2, content="3")
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