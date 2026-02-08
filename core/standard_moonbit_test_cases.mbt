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
  // Test addition
  let _ = 1 + 2
  let _ = 0 + 5
  let _ = -1 + 1
  
  // Test subtraction
  let _ = 5 - 3
  let _ = 0 - 5
  let _ = -2 - (-3)
  
  // Test multiplication
  let _ = 3 * 4
  let _ = 0 * 5
  let _ = -2 * 3
  
  // Test division
  let _ = 10 / 2
  let _ = 7 / 3
  let _ = -8 / 2
}

///|
test "string basic operations" {
  // Test string concatenation
  let _ = "Hello" + " " + "World"
  let _ = "" + "test"
  let _ = "a" + "b" + "c"
  
  // Test string length
  let _ = "".length()
  let _ = "hello".length()
  let _ = "你好".length()
  
  // Test string comparison
  let _ = "abc" == "abc"
  let _ = "abc" == "def"
  let _ = "" == ""
}

///|
test "array operations" {
  // Test array creation and access
  let int_array = [1, 2, 3, 4, 5]
  let string_array = ["a", "b", "c"]
  
  // Test array length
  let _ = int_array.length()
  let _ = string_array.length()
  
  // Test array access
  let _ = int_array[0]
  let _ = int_array[2]
  let _ = int_array[4]
  let _ = string_array[1]
}

///|
test "boolean operations" {
  // Test basic boolean values
  let _ = true
  let _ = false
  let _ = true != false
  
  // Test boolean operations
  let _ = true && true
  let _ = true && false
  let _ = false && true
  let _ = false && false
  
  let _ = true || true
  let _ = true || false
  let _ = false || true
  let _ = false || false
  
  // Test boolean negation
  let _ = !true
  let _ = !false
}

///|
test "comparison operations" {
  // Test numeric comparisons
  let _ = 5 > 3
  let _ = 5 > 5
  let _ = 3 > 5
  
  let _ = 5 < 3
  let _ = 5 < 5
  let _ = 3 < 5
  
  let _ = 5 >= 5
  let _ = 5 >= 3
  let _ = 3 >= 5
  
  let _ = 5 <= 5
  let _ = 5 <= 3
  let _ = 3 <= 5
  
  // Test string comparisons (lexicographic)
  let _ = "abc" < "abd"
  let _ = "abc" > "abb"
  let _ = "abc" == "abc"
}

///|
test "int bit operations" {
  // Test bitwise AND
  let _ = 10 & 12
  let _ = 5 & 3
  let _ = 0 & 5
  
  // Test bitwise OR
  let _ = 10 | 12
  let _ = 5 | 3
  let _ = 0 | 5
  
  // Test bitwise XOR
  let _ = 10 ^ 12
  let _ = 5 ^ 3
  let _ = 0 ^ 5
  
  // Test bitwise NOT
  let _ = 0 - 10 - 1  // Equivalent to ~10
  let _ = -1  // Equivalent to ~0
  let _ = 0  // Equivalent to ~(-1)
  
  // Test left shift
  let _ = 10 << 2
  let _ = 1 << 3
  let _ = 0 << 5
  
  // Test right shift
  let _ = 10 >> 2
  let _ = 8 >> 3
  let _ = 0 >> 5
}

///|
test "int modulo operations" {
  // Test positive modulo
  let _ = 10 % 3
  let _ = 10 % 5
  let _ = 7 % 4
  
  // Test negative modulo
  let _ = -10 % 3
  let _ = 10 % -3
  let _ = -10 % -3
  
  // Test edge cases
  let _ = 0 % 5
  let _ = 5 % 1
}

///|
test "int boundary values" {
  // Test maximum and minimum values
  let _ = 2147483647  // max_value
  let _ = -2147483648 // min_value
  
  // Test overflow behavior
  let _ = 2147483647 + 1
  let _ = -2147483648 - 1
  
  // Test boundary operations
  let _ = 2147483647 - 1
  let _ = -2147483648 + 1
}

///|
test "int sign and absolute value" {
  // Test absolute value
  let _ = Int::abs(5)
  let _ = Int::abs(-5)
  let _ = Int::abs(0)
  let _ = Int::abs(2147483647)
  let _ = Int::abs(-2147483648)
}