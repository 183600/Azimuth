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
test "string_concatenation_operations" {
  // Basic concatenation
  let s1 = "Hello"
  let s2 = "World"
  let result = s1 + " " + s2
  inspect(result, content="Hello World")
  
  // Multiple concatenations
  let multi = "a" + "b" + "c" + "d" + "e"
  inspect(multi, content="abcde")
  
  // Empty string concatenation
  let empty = ""
  let result2 = empty + "test"
  inspect(result2, content="test")
  
  let result3 = "test" + empty
  inspect(result3, content="test")
}

///|
test "string_length_operations" {
  // Test length of various strings
  inspect("".length(), content="0")
  inspect("a".length(), content="1")
  inspect("hello".length(), content="5")
  inspect("hello world".length(), content="11")
  
  // Test with spaces and special characters
  inspect(" a b c ".length(), content="7")
  inspect("!@#$%^&*()".length(), content="10")
}

///|
test "string_comparison_operations" {
  // Test equality
  assert_eq("hello" == "hello", true)
  assert_eq("hello" == "world", false)
  assert_eq("" == "", true)
  assert_eq("a" == "A", false)
  
  // Test inequality
  assert_eq("hello" != "world", true)
  assert_eq("hello" != "hello", false)
  assert_eq("" != "a", true)
}

///|
test "string_edge_cases" {
  // Test very long string
  let long_string = "a" * 1000
  assert_eq(long_string.length(), 1000)
  
  // Test string with special characters
  let special = "Hello\nWorld\t!"
  assert_eq(special.length(), 12)
  
  // Test string with Unicode characters
  let unicode = "Hello 世界"
  assert_eq(unicode.length(), 8)
}

///|
test "string_empty_operations" {
  // Test empty string properties
  let empty = ""
  assert_eq(empty.length(), 0)
  assert_eq(empty == "", true)
  assert_eq(empty != "", false)
  
  // Test operations with empty string
  let non_empty = "hello"
  assert_eq(empty + non_empty, "hello")
  assert_eq(non_empty + empty, "hello")
  assert_eq(empty + empty, "")
}

///|
test "string_repetition_operations" {
  // Test string repetition
  let single = "a"
  let repeated = single * 5
  inspect(repeated, content="aaaaa")
  
  // Test empty string repetition
  let empty = ""
  let empty_repeated = empty * 10
  inspect(empty_repeated, content="")
  
  // Test longer string repetition
  let word = "ab"
  let word_repeated = word * 3
  inspect(word_repeated, content="ababab")
}

///|
test "string_character_operations" {
  // Test single character strings
  let char_a = "a"
  let char_b = "b"
  
  assert_eq(char_a.length(), 1)
  assert_eq(char_b.length(), 1)
  assert_eq(char_a != char_b, true)
  
  // Test character concatenation
  let result = char_a + char_b
  inspect(result, content="ab")
}

///|
test "string_whitespace_operations" {
  // Test strings with various whitespace
  let spaces = "   "
  let tabs = "\t\t"
  let newlines = "\n\n"
  let mixed = " \t\n"
  
  assert_eq(spaces.length(), 3)
  assert_eq(tabs.length(), 2)
  assert_eq(newlines.length(), 2)
  assert_eq(mixed.length(), 3)
}

///|
test "string_numeric_content" {
  // Test strings containing numbers
  let numbers = "12345"
  let mixed = "abc123def"
  let only_spaces = "   "
  
  assert_eq(numbers.length(), 5)
  assert_eq(mixed.length(), 9)
  assert_eq(only_spaces.length(), 3)
  
  // Test numeric string operations
  assert_eq(numbers == "12345", true)
  assert_eq(mixed == "abc123def", true)
}