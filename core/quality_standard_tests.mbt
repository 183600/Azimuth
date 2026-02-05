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
test "int basic arithmetic" {
  let x = 42
  let y = -17
  let zero = 0
  
  assert_eq(x + y, 25)
  assert_eq(x - y, 59)
  assert_eq(x * y, -714)
  assert_eq(x / y, -2)
  assert_eq(x % y, 8)
  
  assert_eq(Int::abs(y), 17)
  assert_eq(Int::abs(zero), 0)
  assert_eq(Int::abs(x), 42)
}

///|
test "int boundary values" {
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  let max_val = Int::max_value
  let min_val = Int::min_value
  
  assert_eq(max_val + 1, -2147483648)
  assert_eq(min_val - 1, 2147483647)
}

///|
test "list basic operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // Test length
  assert_eq(list1.length(), 3)
  assert_eq(list2.length(), 3)
  
  // Test concatenation
  let concatenated = list1.concat(list2)
  assert_eq(concatenated, @list.from_array([1, 2, 3, 4, 5, 6]))
  
  // Test head and tail
  assert_eq(list1.head(), Some(1))
  assert_eq(list1.last(), Some(3))
}

///|
test "list map and filter" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Test map
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled, @list.from_array([2, 4, 6, 8, 10]))
  
  // Test filter
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, @list.from_array([2, 4]))
  
  // Test fold
  let sum = numbers.fold(0, (acc, x) => acc + x)
  assert_eq(sum, 15)
}

///|
test "option basic operations" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test is_some and is_none
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
  
  // Test unwrap_or
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
  
  // Test map
  let mapped = some_value.map(x => x * 2)
  assert_eq(mapped, Some(84))
}

///|
test "option chaining" {
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  // Test and_then
  let chained = some_value.and_then(x => if x > 40 { Some(x * 2) } else { None })
  assert_eq(chained, Some(84))
  
  let none_chained = none_value.and_then(x => Some(x * 2))
  assert_eq(none_chained, None)
  
  // Test filter
  let filtered = some_value.filter(x => x > 40)
  assert_eq(filtered, Some(42))
  
  let filtered_out = some_value.filter(x => x > 50)
  assert_eq(filtered_out, None)
}

///|
test "string basic operations" {
  let text = "Hello"
  let empty = ""
  
  // Test length
  assert_eq(text.length(), 5)
  assert_eq(empty.length(), 0)
  
  // Test concatenation
  let greeting = text + ", World!"
  assert_eq(greeting, "Hello, World!")
  
  // Test character access
  assert_eq(text[0], 'H')
  assert_eq(text[4], 'o')
}

///|
test "string comparison" {
  let text1 = "Hello"
  let text2 = "hello"
  let text3 = "Hello"
  
  // Test equality
  assert_eq(text1 == text3, true)
  assert_eq(text1 == text2, false)
  assert_eq(text1 != text2, true)
  
  // Test empty string
  assert_eq("".is_empty(), true)
  assert_eq(text1.is_empty(), false)
}

///|
test "math basic functions" {
  // Test absolute value
  assert_eq(@math.abs(-5), 5)
  assert_eq(@math.abs(0), 0)
  assert_eq(@math.abs(7), 7)
  
  // Test min and max
  assert_eq(@math.min(3, 7), 3)
  assert_eq(@math.max(3, 7), 3)
  
  // Test power
  assert_eq(@math.pow(2, 3), 8)
  assert_eq(@math.pow(5, 0), 1)
}

///|
test "math constants and special values" {
  // Test operations with zero
  assert_eq(@math.pow(0, 5), 0)
  assert_eq(@math.pow(5, 0), 1)
  
  // Test logarithmic functions (basic checks)
  assert_eq(@math.log(1), 0)
  assert_eq(@math.log10(10), 1)
  
  // Test mathematical constants
  inspect(@math.pi, content="3.14159")
}