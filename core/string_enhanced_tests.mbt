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
test "string basic operations" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  
  assert_eq(s3, "Hello World")
  assert_eq(s1, "Hello")
  assert_eq(s2, "World")
}

///|
test "string equality and comparison" {
  let s1 = "abc"
  let s2 = "abc"
  let s3 = "def"
  let s4 = "ABC"
  
  assert_eq(s1 == s2, true)
  assert_eq(s1 != s3, true)
  assert_eq(s1 == s3, false)
  assert_eq(s1 == s4, false)
  assert_eq(s1 != s4, true)
}

///|
test "string length and indexing" {
  let s = "MoonBit"
  assert_eq(s.length(), 7)
  
  let empty = ""
  assert_eq(empty.length(), 0)
  
  let single = "A"
  assert_eq(single.length(), 1)
}

///|
test "string concatenation multiple" {
  let s1 = "a"
  let s2 = "b"
  let s3 = "c"
  let result = s1 + s2 + s3
  
  assert_eq(result, "abc")
  
  let numbers = "1" + "2" + "3" + "4" + "5"
  assert_eq(numbers, "12345")
}

///|
test "string with spaces and special characters" {
  let with_spaces = "Hello MoonBit World"
  assert_eq(with_spaces, "Hello MoonBit World")
  
  let with_newline = "Line1\nLine2"
  assert_eq(with_newline.length(), 10)
  
  let with_tab = "Col1\tCol2"
  assert_eq(with_tab.length(), 9)
}

///|
test "string default value" {
  let default_str = ""
  assert_eq(default_str, "")
  assert_eq(default_str.length(), 0)
  assert_eq(default_str == "", true)
}

///|
test "string unicode characters" {
  let unicode_str = "测试"
  assert_eq(unicode_str, "测试")
  assert_eq(unicode_str.length(), 2)
  
  let emoji = "🚀"
  assert_eq(emoji, "🚀")
  assert_eq(emoji.length(), 1)
}

///|
test "string numeric conversion" {
  let num_str = "42"
  assert_eq(num_str, "42")
  
  let negative_num = "-17"
  assert_eq(negative_num, "-17")
  
  let float_str = "3.14"
  assert_eq(float_str, "3.14")
}