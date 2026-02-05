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

// Test file for core modules

///|
test "int basic arithmetic operations" {
  // Addition
  inspect(1 + 2, content="3")
  inspect(-1 + 1, content="0")
  inspect(100 + 200, content="300")
  
  // Subtraction
  inspect(5 - 3, content="2")
  inspect(0 - 1, content="-1")
  inspect(-5 - (-3), content="-2")
  
  // Multiplication
  inspect(3 * 4, content="12")
  inspect(-2 * 3, content="-6")
  inspect(-2 * -3, content="6")
  
  // Division
  inspect(10 / 2, content="5")
  inspect(7 / 3, content="2")
  inspect(-8 / 2, content="-4")
  
  // Modulo
  inspect(10 % 3, content="1")
  inspect(7 % 7, content="0")
  inspect(-7 % 3, content="-1")
}

///|
test "int comparison operations" {
  // Equality
  inspect(5 == 5, content="true")
  inspect(5 == 3, content="false")
  
  // Inequality
  inspect(5 != 3, content="true")
  inspect(5 != 5, content="false")
  
  // Less than
  inspect(3 < 5, content="true")
  inspect(5 < 3, content="false")
  
  // Less than or equal
  inspect(3 <= 5, content="true")
  inspect(5 <= 5, content="true")
  inspect(5 <= 3, content="false")
  
  // Greater than
  inspect(5 > 3, content="true")
  inspect(3 > 5, content="false")
  
  // Greater than or equal
  inspect(5 >= 3, content="true")
  inspect(5 >= 5, content="true")
  inspect(3 >= 5, content="false")
}

///|
test "int boundary values" {
  // Test max and min values
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // Test overflow behavior
  inspect(@int.max_value + 1, content="-2147483648")
  inspect(@int.min_value - 1, content="2147483647")
  
  // Test absolute value
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

///|
test "string basic operations" {
  // String creation and comparison
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(world.length(), content="5")
  
  // String concatenation
  let greeting = hello + " " + world
  inspect(greeting, content="Hello World")
  
  // String equality
  inspect(hello == "Hello", content="true")
  inspect(hello == "hello", content="false")
  
  // String inequality
  inspect(hello != "Hello", content="false")
  inspect(hello != "hello", content="true")
}

///|
test "string utility functions" {
  let test_string = "Hello, World!"
  
  // Test default string
  inspect(@string.default(), content="")
  
  // String character access (if supported)
  // Note: This depends on the specific string API available
  
  // String conversion
  let num_str = "42"
  // Note: String to int conversion would depend on available API
}

///|
test "list basic operations" {
  // Create empty list
  let empty_list = @list.empty()
  inspect(empty_list.is_empty(), content="true")
  inspect(empty_list.length(), content="0")
  
  // Create list from array
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  inspect(numbers.length(), content="5")
  inspect(numbers.is_empty(), content="false")
  
  // List head and tail
  inspect(numbers.head(), content="Some(1)")
  inspect(numbers.last(), content="Some(5)")
  
  // List prepend (cons)
  let new_list = numbers.prepend(0)
  inspect(new_list.length(), content="6")
  inspect(new_list.head(), content="Some(0)")
}

///|
test "list transformations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Map operation
  let doubled = numbers.map(x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // Filter operation
  let evens = numbers.filter(x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  // Fold operation
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // Reverse operation
  let reversed = numbers.rev()
  inspect(reversed, content="@list.from_array([5, 4, 3, 2, 1])")
}

///|
test "list querying operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  
  // Contains operation
  inspect(numbers.contains(3), content="true")
  inspect(numbers.contains(6), content="false")
  
  // All operation
  inspect(numbers.all(x => x > 0), content="true")
  inspect(numbers.all(x => x % 2 == 0), content="false")
  
  // Any operation
  inspect(numbers.any(x => x % 2 == 0), content="true")
  inspect(numbers.any(x => x > 10), content="false")
  
  // Nth operation
  inspect(numbers.nth(0), content="Some(1)")
  inspect(numbers.nth(2), content="Some(3)")
  inspect(numbers.nth(4), content="Some(5)")
  inspect(numbers.nth(5), content="None")
}

///|
test "list combination operations" {
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // Concat operation
  let combined = list1.concat(list2)
  inspect(combined, content="@list.from_array([1, 2, 3, 4, 5, 6])")
  
  // Zip operation
  let zipped = @list.zip(list1, list2)
  inspect(zipped, content="@list.from_array([(1, 4), (2, 5), (3, 6)])")
  
  // Take operation
  let taken = list1.concat(list2).take(4)
  inspect(taken, content="@list.from_array([1, 2, 3, 4])")
}

///|
test "list min/max operations" {
  let numbers = @list.from_array([3, 1, 4, 1, 5, 9, 2, 6])
  
  // Maximum operation
  inspect(numbers.maximum(), content="Some(9)")
  
  // Minimum operation
  inspect(numbers.minimum(), content="Some(1)")
  
  // Empty list edge cases
  let empty = @list.empty()
  inspect(empty.maximum(), content="None")
  inspect(empty.minimum(), content="None")
}