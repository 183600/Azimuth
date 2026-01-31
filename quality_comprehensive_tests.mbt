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
test "int arithmetic operations" {
  // Test basic arithmetic
  assert_eq(10 + 5, 15)
  assert_eq(10 - 5, 5)
  assert_eq(10 * 5, 50)
  assert_eq(10 / 3, 3)
  assert_eq(10 % 3, 1)
}

///|
test "int comparison operations" {
  // Test comparisons - using assert_eq with boolean results converted to int
  assert_eq(if 10 > 5 { 1 } else { 0 }, 1)
  assert_eq(if 10 < 5 { 1 } else { 0 }, 0)
  assert_eq(if 10 >= 10 { 1 } else { 0 }, 1)
  assert_eq(if 10 <= 5 { 1 } else { 0 }, 0)
  assert_eq(if 10 == 10 { 1 } else { 0 }, 1)
  assert_eq(if 10 != 5 { 1 } else { 0 }, 1)
}

///|
test "array basic operations" {
  // Test array creation and access
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  // Test array equality
  let arr2 = [1, 2, 3, 4, 5]
  assert_eq(if arr == arr2 { 1 } else { 0 }, 1)
  
  // Test array concatenation
  let arr3 = [6, 7, 8]
  let combined = arr.concat(arr3)
  assert_eq(combined.length(), 8)
  assert_eq(combined[0], 1)
  assert_eq(combined[7], 8)
}

///|
test "string operations" {
  // Test string concatenation
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  assert_eq_string(combined, "Hello, World!")
  
  // Test string comparison
  assert_eq_string(greeting, "Hello")
  assert_eq(if greeting != target { 1 } else { 0 }, 1)
  
  // Test string with numbers
  let num_str = "42"
  assert_eq_string(num_str, "42")
}

///|
test "boolean logic operations" {
  // Test basic boolean operations - convert to int for comparison
  assert_eq(if true && false { 1 } else { 0 }, 0)
  assert_eq(if true || false { 1 } else { 0 }, 1)
  
  // Test comparisons
  assert_eq(if 5 > 3 { 1 } else { 0 }, 1)
  assert_eq(if 5 < 3 { 1 } else { 0 }, 0)
  assert_eq(if 5 == 5 { 1 } else { 0 }, 1)
  assert_eq(if 5 != 3 { 1 } else { 0 }, 1)
  
  // Test comparison chains
  let x = 5
  assert_eq(if 0 < x && x < 10 { 1 } else { 0 }, 1)
  assert_eq(if x == 5 || x == 10 { 1 } else { 0 }, 1)
}

///|
test "tuple operations" {
  // Test tuple creation and access
  let pair = (1, "hello")
  assert_eq(pair.0, 1)
  
  // Test tuple comparison
  let pair2 = (1, "hello")
  let pair3 = (1, "world")
  assert_eq(if pair == pair2 { 1 } else { 0 }, 1)
  assert_eq(if pair == pair3 { 1 } else { 0 }, 0)
  
  // Test nested tuples
  let triple = (1, (2, 3), "test")
  assert_eq(triple.0, 1)
  assert_eq(triple.1.0, 2)
  assert_eq(triple.1.1, 3)
}

///|
test "option type operations" {
  // Test Some operations
  let some_val = Some(42)
  match some_val {
    Some(v) => assert_eq(v, 42)
    None => assert_eq(0, 1) // This should not happen
  }
  
  // Test None operations
  let none_val : Int? = None
  match none_val {
    Some(v) => assert_eq(0, 1) // This should not happen
    None => assert_eq(1, 1) // This should happen
  }
  
  // Test option with default value
  let result = match some_val {
    Some(v) => v
    None => 0
  }
  assert_eq(result, 42)
  
  let result2 = match none_val {
    Some(v) => v
    None => 0
  }
  assert_eq(result2, 0)
}

///|
test "character operations" {
  // Test character creation and comparisons
  let char_a = 'a'
  let char_A = 'A'
  let char_digit = '5'
  
  // Test character comparisons
  assert_eq(if char_a == char_a { 1 } else { 0 }, 1)
  assert_eq(if char_a != char_A { 1 } else { 0 }, 1)
  
  // Test character to integer conversion
  assert_eq(char_a.to_int(), 97)
  assert_eq(char_A.to_int(), 65)
  assert_eq(char_digit.to_int(), 53)
}

///|
test "int bit operations" {
  // Test bit operations
  assert_eq(10 & 12, 8)
  assert_eq(10 | 12, 14)
  assert_eq(10 ^ 12, 6)
  assert_eq(~10, -11)
  assert_eq(10 << 2, 40)
  assert_eq(10 >> 2, 2)
}

///|
test "array iteration and manipulation" {
  // Test array sum calculation
  let arr = [1, 2, 3, 4, 5]
  let mut sum = 0
  let mut i = 0
  while i < arr.length() {
    sum = sum + arr[i]
    i = i + 1
  }
  assert_eq(sum, 15)
  
  // Test array filtering manually
  let even_numbers = []
  i = 0
  while i < arr.length() {
    if arr[i] % 2 == 0 {
      even_numbers.push(arr[i])
    }
    i = i + 1
  }
  assert_eq(even_numbers.length(), 2)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[1], 4)
}