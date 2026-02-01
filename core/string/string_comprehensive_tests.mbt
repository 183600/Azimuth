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
test "string default function" {
  let default_str = @string.default()
  inspect(default_str, content="")
  assert_eq(default_str, "")
}

///|
test "string empty operations" {
  let empty = ""
  inspect(empty.length(), content="0")
  inspect(empty.is_empty(), content="true")
  inspect(empty + "test", content="test")
}

///|
test "string concatenation complex" {
  let words = ["Hello", "beautiful", "MoonBit", "world"]
  let result = words[0] + " " + words[1] + " " + words[2] + " " + words[3] + "!"
  inspect(result, content="Hello beautiful MoonBit world!")
  
  // Test concatenation with empty strings
  let empty = ""
  let non_empty = "test"
  inspect(empty + non_empty, content="test")
  inspect(non_empty + empty, content="test")
  inspect(empty + empty, content="")
}

///|
test "string comparison edge cases" {
  // Test case sensitivity
  let upper = "HELLO"
  let lower = "hello"
  let mixed = "Hello"
  
  assert_ne(upper, lower)
  assert_ne(upper, mixed)
  assert_ne(lower, mixed)
  
  // Test lexicographical ordering
  assert_true("apple" < "banana")
  assert_true("apple" <= "apple")
  assert_true("banana" > "apple")
  assert_true("banana" >= "banana")
  
  // Test with numbers in strings
  assert_true("file1" < "file2")
  assert_true("file10" > "file2")  // Lexicographical comparison
}

///|
test "string character access" {
  let text = "MoonBit"
  
  // Test valid indices
  inspect(text[0], content="'M'")
  inspect(text[3], content="'B'")
  inspect(text[6], content="'t'")
  
  // Test string length
  inspect(text.length(), content="7")
  
  // Test iteration over characters
  let mut char_count = 0
  for _ in text {
    char_count = char_count + 1
  }
  inspect(char_count, content="7")
}

///|
test "string slicing edge cases" {
  let text = "Hello, MoonBit!"
  
  // Test various slice ranges
  inspect(text.slice(0, 0), content="")
  inspect(text.slice(0, 1), content="H")
  inspect(text.slice(text.length() - 1, text.length()), content="!")
  inspect(text.slice(0, text.length()), content="Hello, MoonBit!")
  
  // Test slicing single character strings
  let single = "A"
  inspect(single.slice(0, 1), content="A")
  inspect(single.slice(0, 0), content="")
}

///|
test "string search operations" {
  let text = "The quick brown fox jumps over the lazy dog"
  
  // Test contains
  inspect(text.contains("quick"), content="true")
  inspect(text.contains("QUICK"), content="false")  // Case sensitive
  inspect(text.contains("dog"), content="true")
  inspect(text.contains("cat"), content="false")
  
  // Test starts_with
  inspect(text.starts_with("The"), content="true")
  inspect(text.starts_with("the"), content="false")  // Case sensitive
  inspect(text.starts_with("quick"), content="false")
  
  // Test ends_with
  inspect(text.ends_with("dog"), content="true")
  inspect(text.ends_with("Dog"), content="false")  // Case sensitive
  inspect(text.ends_with("lazy"), content="false")
}

///|
test "string unicode handling" {
  // Test with unicode characters
  let unicode_str = "Hello 世界 🌍"
  inspect(unicode_str.length(), content="11")  // Length in bytes
  
  // Test basic operations with unicode
  let prefix = unicode_str.slice(0, 5)
  inspect(prefix, content="Hello")
  
  // Test concatenation with unicode
  let result = "MoonBit " + "世界"
  inspect(result, content="MoonBit 世界")
}

///|
test "string special characters" {
  // Test with various special characters
  let special = "Hello\n\tWorld!\"'\\"
  inspect(special.length(), content="16")
  
  // Test escape sequences
  let newline = "Line1\nLine2"
  let tab = "Col1\tCol2"
  let quote = "He said \"Hello\""
  
  inspect(newline.contains("\n"), content="true")
  inspect(tab.contains("\t"), content="true")
  inspect(quote.contains("\""), content="true")
}