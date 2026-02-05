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
test "int boundary values" {
  let max_int = @int.max_value
  let min_int = @int.min_value
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  
  // Test absolute value function
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(123), content="123")
  
  // Test overflow scenarios
  let near_max = max_int - 1
  let near_min = min_int + 1
  inspect(near_max, content="2147483646")
  inspect(near_min, content="-2147483647")
}

///|
test "string operations and comparisons" {
  let empty = @string.default()
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  
  inspect(empty, content="")
  inspect(hello, content="Hello")
  inspect(combined, content="Hello World")
  
  // Test string length
  inspect(hello.length(), content="5")
  inspect(combined.length(), content="11")
  
  // Test string equality
  inspect(hello == "Hello", content="true")
  inspect(hello == "hello", content="false")
  
  // Test string with special characters
  let special = "Hello\nWorld\t!"
  inspect(special.length(), content="13")
}

///|
test "list functional operations" {
  let numbers = @list.from_array([1, 2, 3, 4, 5])
  let empty = @list.empty()
  
  // Test map operation
  let doubled = @list.map(numbers, x => x * 2)
  inspect(doubled, content="@list.from_array([2, 4, 6, 8, 10])")
  
  // Test filter operation
  let evens = @list.filter(numbers, x => x % 2 == 0)
  inspect(evens, content="@list.from_array([2, 4])")
  
  // Test fold operation
  let sum = @list.fold(numbers, 0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // Test prepend and append
  let with_prepend = @list.cons(0, numbers)
  let with_append = @list.append(numbers, 6)
  inspect(with_prepend, content="@list.from_array([0, 1, 2, 3, 4, 5])")
  inspect(with_append, content="@list.from_array([1, 2, 3, 4, 5, 6])")
  
  // Test list length
  inspect(numbers.length(), content="5")
  inspect(empty.length(), content="0")
}

///|
test "array boundary and index operations" {
  let empty : Array[Int] = []
  let single = [42]
  let multiple = [10, 20, 30, 40, 50]
  
  // Test array creation and length
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(multiple.length(), content="5")
  
  // Test array indexing
  inspect(single[0], content="42")
  inspect(multiple[0], content="10")
  inspect(multiple[4], content="50")
  
  // Test array slice operations
  let first_three = multiple.slice(0, 3)
  inspect(first_three, content="[10, 20, 30]")
  
  // Test array push and pop
  let mut mutable_array = [1, 2, 3]
  mutable_array.push(4)
  inspect(mutable_array, content="[1, 2, 3, 4]")
  
  let popped = mutable_array.pop()?
  inspect(popped, content="4")
  inspect(mutable_array, content="[1, 2, 3]")
}

///|
test "character and unicode operations" {
  let basic_char = 'A'
  let digit_char = '5'
  let space_char = ' '
  let newline_char = '\n'
  
  // Test character equality
  inspect(basic_char == 'A', content="true")
  inspect(basic_char == 'a', content="false")
  
  // Test character properties
  inspect(Char::is_alphabetic(basic_char), content="true")
  inspect(Char::is_alphabetic(digit_char), content="false")
  inspect(Char::is_digit(digit_char), content="true")
  inspect(Char::is_digit(basic_char), content="false")
  
  // Test character conversion
  let upper_a = 'a'
  let to_upper = Char::to_upper(upper_a)
  inspect(to_upper, content="A")
  
  let lower_b = 'B'
  let to_lower = Char::to_lower(lower_b)
  inspect(to_lower, content="b")
  
  // Test character to integer conversion
  let char_to_int = Char::to_int(digit_char)
  inspect(char_to_int, content="5")
  
  // Test whitespace detection
  inspect(Char::is_whitespace(space_char), content="true")
  inspect(Char::is_whitespace(newline_char), content="true")
  inspect(Char::is_whitespace(basic_char), content="false")
}

///|
test "type conversions and error handling" {
  // Test string to integer conversion
  let valid_number = "123"
  let invalid_number = "abc"
  
  // Test parsing with error handling
  match Int::parse(valid_number) {
    Ok(n) => inspect(n, content="123")
    Err(_) => inspect("unexpected error", content="")
  }
  
  match Int::parse(invalid_number) {
    Ok(_) => inspect("unexpected success", content="")
    Err(e) => inspect("parse error expected", content="parse error expected")
  }
  
  // Test integer to string conversion
  let number = 42
  let string_repr = number.to_string()
  inspect(string_repr, content="42")
  
  // Test boolean operations
  let true_val = true
  let false_val = false
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(not true_val, content="false")
}

///|
test "collection pattern matching" {
  let numbers = @list.from_array([1, 2, 3])
  let empty_list = @list.empty()
  
  // Pattern matching on lists
  let list_result = match numbers {
    Empty => "empty"
    Cons(head, tail) => "non-empty"
  }
  inspect(list_result, content="non-empty")
  
  let empty_result = match empty_list {
    Empty => "empty"
    Cons(_, _) => "non-empty"
  }
  inspect(empty_result, content="empty")
  
  // Test array pattern matching
  let arr = [1, 2, 3]
  inspect(arr.length(), content="3")
  inspect(arr[0], content="1")
}