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
test "integer arithmetic edge cases" {
  // Test basic arithmetic operations
  @azimuth.assert_eq(10 + 5, 15)
  @azimuth.assert_eq(10 - 5, 5)
  @azimuth.assert_eq(10 * 5, 50)
  @azimuth.assert_eq(10 / 3, 3)
  @azimuth.assert_eq(10 % 3, 1)
  
  // Test negative numbers
  @azimuth.assert_eq((-7) / 3, -2)
  @azimuth.assert_eq((-7) % 3, -1)
  
  // Test overflow behavior using maximum values
  let max_int = 2147483647
  let min_int = -2147483648
  @azimuth.assert_eq(max_int + 1, min_int)
  @azimuth.assert_eq(min_int - 1, max_int)
}

///|
test "string operations comprehensive" {
  // Test string concatenation
  let base = "MoonBit"
  let greeting = "Hello, " + base + "!"
  @azimuth.assert_eq_string(greeting, "Hello, MoonBit!")
  
  // Test string with Unicode
  let unicode_text = "🌟 MoonBit 🌟"
  @azimuth.assert_eq_string(unicode_text, "🌟 MoonBit 🌟")
  
  // Test string comparison
  @azimuth.assert_eq("hello" == "hello", true)
  @azimuth.assert_eq("hello" != "world", true)
  @azimuth.assert_eq("apple" < "banana", true)
}

///|
test "boolean logic combinations" {
  let a = true
  let b = false
  let c = true
  
  // Test logical operations
  @azimuth.assert_eq(a && b, false)
  @azimuth.assert_eq(a || b, true)
  @azimuth.assert_eq(!a, false)
  @azimuth.assert_eq(!b, true)
  
  // Test complex boolean expressions
  @azimuth.assert_eq(a && (b || c), true)
  @azimuth.assert_eq((a || b) && c, true)
  @azimuth.assert_eq(!(a && b) || c, true)
  
  // Test boolean equality
  @azimuth.assert_eq(a == true, true)
  @azimuth.assert_eq(b == false, true)
  @azimuth.assert_eq(a != b, true)
}

///|
test "array operations and transformations" {
  // Test array creation and access
  let numbers = [1, 2, 3, 4, 5]
  @azimuth.assert_eq(numbers.length(), 5)
  @azimuth.assert_eq(numbers[0], 1)
  @azimuth.assert_eq(numbers[4], 5)
  
  // Test array concatenation
  let more_numbers = [6, 7, 8]
  let combined = numbers + more_numbers
  @azimuth.assert_eq(combined.length(), 8)
  @azimuth.assert_eq(combined[5], 6)
}

///|
test "azimuth function integration" {
  // Test azimuth library functions
  // Using add function
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(-5, 10), 5)
  
  // Using multiply function
  @azimuth.assert_eq(@azimuth.multiply(3, 4), 12)
  @azimuth.assert_eq(@azimuth.multiply(-2, 5), -10)
  @azimuth.assert_eq(@azimuth.multiply(0, 100), 0)
  
  // Using greet function
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

///|
test "mathematical properties" {
  // Test commutative property of addition
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // Test associative property of addition
  let c = 23
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // Test distributive property
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // Test identity properties
  @azimuth.assert_eq(@azimuth.add(a, 0), a)
  @azimuth.assert_eq(@azimuth.multiply(a, 1), a)
}

///|
test "edge cases and boundary conditions" {
  // Test divide_with_ceil function edge cases
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 1), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
  
  // Test with negative numbers
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, -3), -3)
  
  // Test string edge cases
  @azimuth.assert_eq_string(@azimuth.greet("🌟"), "Hello, 🌟!")
  @azimuth.assert_eq_string(@azimuth.greet("中文"), "Hello, 中文!")
}

///|
test "function composition" {
  // Test combining multiple functions
  let x = 10
  let y = 20
  
  // Complex calculation using multiple azimuth functions
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(x, y)
  let result = @azimuth.add(sum, product)
  @azimuth.assert_eq(result, 230)  // (10 + 20) + (10 * 20) = 30 + 200 = 230
  
  // Test with greet and string operations
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string(greeting, "Hello, MoonBit!")
}

///|
test "performance and efficiency" {
  // Test multiple operations in sequence
  let result1 = @azimuth.add(1, 1)
  let result2 = @azimuth.add(result1, 1)
  let result3 = @azimuth.add(result2, 1)
  @azimuth.assert_eq(result3, 3)
  
  // Test chained multiplications
  let mult_result = @azimuth.multiply(2, 3)
  mult_result = @azimuth.multiply(mult_result, 4)
  @azimuth.assert_eq(mult_result, 24)
  
  // Test efficiency with repeated operations
  let base = 5
  let mut total = 0
  for i = 0; i < 10; i = i + 1 {
    total = @azimuth.add(total, base)
  }
  @azimuth.assert_eq(total, 50)
}

///|
test "error handling and validation" {
  // Test functions with potentially problematic inputs
  @azimuth.assert_eq(@azimuth.add(2147483647, 0), 2147483647)  // Max int + 0
  @azimuth.assert_eq(@azimuth.multiply(-1, 1), -1)  // Negative multiplication
  
  // Test string validation
  @azimuth.assert_eq_string(@azimuth.greet("Test"), "Hello, Test!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  @azimuth.assert_eq_string(@azimuth.greet("!@#"), "Hello, !@#!")
  
  // Test mathematical validation
  @azimuth.assert_eq(@azimuth.divide_with_ceil(100, 1), 100)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 100), 1)
}