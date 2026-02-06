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
test "string empty and default operations" {
  let empty = ""
  let default = String::default()
  
  assert_eq(empty, default)
  assert_eq(empty.length(), 0)
  assert_eq(default.length(), 0)
  assert_true(empty.is_empty())
  assert_true(default.is_empty())
}

///|
test "string concatenation with various types" {
  let str = "Hello"
  let num = 42
  let bool_val = true
  
  // Test interpolation
  let result1 = str + " World"
  let result2 = "\{str} World \{num}"
  let result3 = "Value: \{num}, Boolean: \{bool_val}"
  
  assert_eq(result1, "Hello World")
  assert_eq(result2, "Hello World 42")
  assert_eq(result3, "Value: 42, Boolean: true")
}

///|
test "string character operations comprehensive" {
  let text = "Hello, 世界!"
  
  // Test character access
  assert_eq(text[0], 'H')
  assert_eq(text[4], 'o')
  assert_eq(text[7], '世')
  assert_eq(text[8], '界')
  
  // Test character iteration
  let chars = text.to_array()
  assert_eq(chars.length(), 9)
  assert_eq(chars[0], 'H')
  assert_eq(chars[7], '世')
}

///|
test "string slicing edge cases" {
  let text = "MoonBit Programming"
  
  // Test various slice ranges
  assert_eq(text.slice(0, 0), "")
  assert_eq(text.slice(0, 7), "MoonBit")
  assert_eq(text.slice(8, 19), "Programming")
  assert_eq(text.slice(0, text.length()), "MoonBit Programming")
  
  // Test slice with same start and end
  assert_eq(text.slice(5, 5), "")
  
  // Test slice beyond bounds (should handle gracefully)
  assert_eq(text.slice(10, 30), "gramming")
}

///|
test "string searching operations" {
  let sentence = "The quick brown fox jumps over the lazy dog"
  
  // Test contains
  assert_true(sentence.contains("quick"))
  assert_true(sentence.contains("fox"))
  assert_false(sentence.contains("cat"))
  
  // Test starts_with and ends_with
  assert_true(sentence.starts_with("The"))
  assert_true(sentence.ends_with("dog"))
  assert_false(sentence.starts_with("quick"))
  assert_false(sentence.ends_with("fox"))
  
  // Test with empty string
  assert_true(sentence.contains(""))
  assert_true(sentence.starts_with(""))
  assert_true(sentence.ends_with(""))
}

///|
test "string comparison operations" {
  let str1 = "apple"
  let str2 = "banana"
  let str3 = "apple"
  let str4 = "Apple"
  
  // Test equality
  assert_eq(str1, str3)
  assert_ne(str1, str2)
  assert_ne(str1, str4)
  
  // Test ordering
  assert_true(str1 < str2)
  assert_true(str2 > str1)
  assert_true(str1 <= str3)
  assert_true(str1 >= str3)
  assert_false(str4 == str1) // Case sensitive
}

///|
test "string case conversion operations" {
  let lower = "hello world"
  let upper = "HELLO WORLD"
  let mixed = "HeLlO WoRlD"
  
  // Test to_upper
  assert_eq(lower.to_upper(), "HELLO WORLD")
  assert_eq(mixed.to_upper(), "HELLO WORLD")
  assert_eq(upper.to_upper(), "HELLO WORLD")
  
  // Test to_lower
  assert_eq(upper.to_lower(), "hello world")
  assert_eq(mixed.to_lower(), "hello world")
  assert_eq(lower.to_lower(), "hello world")
}

///|
test "string trimming operations" {
  let spaced = "  hello world  "
  let newlined = "\nhello world\n"
  let tabbed = "\thello world\t"
  let mixed = "  \t\n hello world \n\t  "
  
  // Test trim
  assert_eq(spaced.trim(), "hello world")
  assert_eq(newlined.trim(), "hello world")
  assert_eq(tabbed.trim(), "hello world")
  assert_eq(mixed.trim(), "hello world")
  
  // Test trim_start and trim_end if available
  assert_eq(spaced.trim_start(), "hello world  ")
  assert_eq(spaced.trim_end(), "  hello world")
}

///|
test "string splitting operations" {
  let csv = "apple,banana,cherry,date"
  let empty_commas = ",,,"
  let single = "apple"
  
  // Test split
  let fruits = csv.split(",")
  assert_eq(fruits.length(), 4)
  assert_eq(fruits[0], "apple")
  assert_eq(fruits[3], "date")
  
  // Test split with empty segments
  let empty_segments = empty_commas.split(",")
  assert_eq(empty_segments.length(), 4)
  assert_eq(empty_segments[0], "")
  assert_eq(empty_segments[1], "")
  assert_eq(empty_segments[2], "")
  assert_eq(empty_segments[3], "")
  
  // Test split single item
  let single_split = single.split(",")
  assert_eq(single_split.length(), 1)
  assert_eq(single_split[0], "apple")
}

///|
test "string joining operations" {
  let fruits = ["apple", "banana", "cherry"]
  let empty_array : Array[String] = []
  let single_item = ["apple"]
  
  // Test join
  assert_eq(fruits.join(", "), "apple, banana, cherry")
  assert_eq(fruits.join("|"), "apple|banana|cherry")
  assert_eq(empty_array.join(","), "")
  assert_eq(single_item.join(","), "apple")
}

///|
test "string repeat operations" {
  let text = "abc"
  
  // Test repeat with various counts
  assert_eq(text.repeat(0), "")
  assert_eq(text.repeat(1), "abc")
  assert_eq(text.repeat(3), "abcabcabc")
  
  // Test repeat with empty string
  assert_eq("".repeat(5), "")
  
  // Test repeat with single character
  assert_eq("x".repeat(4), "xxxx")
}