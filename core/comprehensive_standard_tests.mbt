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
test "int arithmetic edge cases" {
  // Test overflow behavior in addition
  assert_eq(@int.max_value + 1, @int.min_value)
  assert_eq(@int.min_value - 1, @int.max_value)
  
  // Test multiplication overflow
  assert_eq(@int.max_value * 2, -2)
  assert_eq((@int.max_value / 2) * 2 + 1, @int.max_value)
  
  // Test division edge cases
  assert_eq(@int.min_value / -1, @int.min_value)
  assert_eq(0 / @int.max_value, 0)
}

///|
test "boolean logic combinations" {
  let a = true
  let b = false
  let c = true
  
  // Test short-circuit evaluation behavior
  inspect(a && (b || c), content="true")
  inspect(b || (a && c), content="true")
  inspect((a || b) && (!b || c), content="true")
  
  // Test complex boolean expressions
  inspect(!(a && b) || (c && a), content="true")
  inspect((a ^ b) && (b ^ c), content="false")
}

///|
test "string operations comprehensive" {
  let base = "Hello"
  let world = "World"
  
  // Test string concatenation
  inspect(base + " " + world, content="\"Hello World\"")
  
  // Test string comparison
  assert_eq(base == "Hello", true)
  assert_eq(base != world, true)
  
  // Test string with special characters
  let special = "Line1\nLine2\tTabbed"
  inspect(special.length(), content="15")
}

///|
test "type conversion integrity" {
  // Test int to various types
  let original = 42
  assert_eq(original.to_int64().to_int(), original)
  assert_eq(original.to_uint().to_int(), original)
  
  // Test boolean to int conversions
  assert_eq(true.to_int(), 1)
  assert_eq(false.to_int(), 0)
  
  // Test conversion chain
  assert_eq(original.to_double().to_int(), original)
}

///|
test "array and list operations" {
  // Test array operations
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  // Test list operations
  let lst = @list.from_array([10, 20, 30])
  assert_eq(lst.length(), 3)
  assert_eq(lst.head(), Some(10))
}

///|
test "hash consistency across types" {
  let hasher = Hasher::new(seed=42)
  
  // Test int hashing
  let int_hash = hasher.clone().combine(123).finalize()
  
  // Test boolean hashing  
  let bool_hash = hasher.clone().combine(true).finalize()
  
  // Test string hashing
  let string_hash = hasher.clone().combine("test").finalize()
  
  // Verify hashes are different
  assert_ne(int_hash, bool_hash)
  assert_ne(bool_hash, string_hash)
  assert_ne(int_hash, string_hash)
}

///|
test "boundary value testing" {
  // Test maximum and minimum values
  assert_eq(@int.max_value > 0, true)
  assert_eq(@int.min_value < 0, true)
  assert_eq(@int.max_value.abs(), @int.max_value)
  assert_eq(@int.min_value.abs(), @int.min_value)
  
  // Test zero boundary
  assert_eq(0.abs(), 0)
  assert_eq(0.to_string(), "\"0\"")
  assert_eq(0.bit_count(), 0)
}

///|
test "bit manipulation precision" {
  // Test bit operations on specific patterns
  assert_eq(0b10101010 & 0b01010101, 0)
  assert_eq(0b10101010 | 0b01010101, 0b11111111)
  assert_eq(0b10101010 ^ 0b01010101, 0b11111111)
  
  // Test shift operations with edge cases
  assert_eq(1 << 31, @int.min_value)
  assert_eq(@int.min_value >> 31, -1)
  
  // Test bit counting
  assert_eq(0b11111111.bit_count(), 8)
  assert_eq(0b10000000.bit_count(), 1)
}

///|
test "comparison operations consistency" {
  // Test transitive property
  let a = 5
  let b = 10
  let c = 15
  
  assert_eq(a < b && b < c, a < c)
  assert_eq(a <= b && b <= c, a <= c)
  
  // Test equality and inequality
  assert_eq(a == a, true)
  assert_eq(a != b, true)
  assert_eq(!(a == b), a != b)
}

///|
test "mathematical function properties" {
  // Test power function properties
  assert_eq(Int::pow(2, 0), 1)
  assert_eq(Int::pow(5, 1), 5)
  assert_eq(Int::pow(1, 100), 1)
  
  // Test absolute value properties
  assert_eq(Int::abs(-10), Int::abs(10))
  assert_eq(Int::abs(0), 0)
  assert_eq(Int::abs(@int.min_value), @int.min_value)
}