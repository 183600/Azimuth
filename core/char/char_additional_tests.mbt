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
test "char basic properties" {
  assert_eq('A'.to_int(), 65)
  assert_eq('a'.to_int(), 97)
  assert_eq('0'.to_int(), 48)
  assert_eq('\n'.to_int(), 10)
  assert_eq('\t'.to_int(), 9)
  assert_eq('\0'.to_int(), 0)
}

///|
test "char comparison operations" {
  assert_eq('A' == 'A', true)
  assert_eq('A' != 'a', true)
  assert_eq('A' < 'B', true)
  assert_eq('Z' > 'Y', true)
  assert_eq('0' <= '9', true)
  assert_eq('a' >= 'A', true)
}

///|
test "char classification functions" {
  assert_eq('A'.is_ascii_alphabetic(), true)
  assert_eq('z'.is_ascii_alphabetic(), true)
  assert_eq('5'.is_ascii_alphabetic(), false)
  assert_eq(' '.is_ascii_alphabetic(), false)
  
  assert_eq('0'.is_ascii_digit(), true)
  assert_eq('9'.is_ascii_digit(), true)
  assert_eq('A'.is_ascii_digit(), false)
  assert_eq('a'.is_ascii_digit(), false)
  
  assert_eq('A'.is_ascii_alphanumeric(), true)
  assert_eq('5'.is_ascii_alphanumeric(), true)
  assert_eq('_'.is_ascii_alphanumeric(), false)
  assert_eq(' '.is_ascii_alphanumeric(), false)
}

///|
test "char case conversion" {
  assert_eq('A'.to_ascii_lowercase(), 'a')
  assert_eq('Z'.to_ascii_lowercase(), 'z')
  assert_eq('a'.to_ascii_lowercase(), 'a')
  assert_eq('z'.to_ascii_lowercase(), 'z')
  assert_eq('5'.to_ascii_lowercase(), '5')
  assert_eq('_'.to_ascii_lowercase(), '_')
  
  assert_eq('a'.to_ascii_uppercase(), 'A')
  assert_eq('z'.to_ascii_uppercase(), 'Z')
  assert_eq('A'.to_ascii_uppercase(), 'A')
  assert_eq('Z'.to_ascii_uppercase(), 'Z')
  assert_eq('5'.to_ascii_uppercase(), '5')
  assert_eq('_'.to_ascii_uppercase(), '_')
}

///|
test "char whitespace detection" {
  assert_eq(' '.is_ascii_whitespace(), true)
  assert_eq('\t'.is_ascii_whitespace(), true)
  assert_eq('\n'.is_ascii_whitespace(), true)
  assert_eq('\r'.is_ascii_whitespace(), true)
  assert_eq('A'.is_ascii_whitespace(), false)
  assert_eq('5'.is_ascii_whitespace(), false)
  assert_eq('_'.is_ascii_whitespace(), false)
}

///|
test "char control character detection" {
  assert_eq('\0'.is_ascii_control(), true)
  assert_eq('\n'.is_ascii_control(), true)
  assert_eq('\t'.is_ascii_control(), true)
  assert_eq('\r'.is_ascii_control(), true)
  assert_eq('A'.is_ascii_control(), false)
  assert_eq('5'.is_ascii_control(), false)
  assert_eq(' '.is_ascii_control(), false)
}

///|
test "char punctuation detection" {
  assert_eq('!'.is_ascii_punctuation(), true)
  assert_eq('?'.is_ascii_punctuation(), true)
  assert_eq('.'.is_ascii_punctuation(), true)
  assert_eq(','.is_ascii_punctuation(), true)
  assert_eq('_'.is_ascii_punctuation(), true)
  assert_eq('-'.is_ascii_punctuation(), true)
  assert_eq('A'.is_ascii_punctuation(), false)
  assert_eq('5'.is_ascii_punctuation(), false)
  assert_eq(' '.is_ascii_punctuation(), false)
}

///|
test "char hexadecimal detection" {
  assert_eq('0'.is_ascii_hexdigit(), true)
  assert_eq('9'.is_ascii_hexdigit(), true)
  assert_eq('A'.is_ascii_hexdigit(), true)
  assert_eq('F'.is_ascii_hexdigit(), true)
  assert_eq('a'.is_ascii_hexdigit(), true)
  assert_eq('f'.is_ascii_hexdigit(), true)
  assert_eq('G'.is_ascii_hexdigit(), false)
  assert_eq('g'.is_ascii_hexdigit(), false)
  assert_eq('X'.is_ascii_hexdigit(), false)
}

///|
test "char escape sequences" {
  let newline = '\n'
  let tab = '\t'
  let carriage_return = '\r'
  let backslash = '\\'
  let single_quote = '\''
  let double_quote = '\"'
  let null_char = '\0'
  
  inspect(newline.to_int(), content="10")
  inspect(tab.to_int(), content="9")
  inspect(carriage_return.to_int(), content="13")
  inspect(backslash.to_int(), content="92")
  inspect(single_quote.to_int(), content="39")
  inspect(double_quote.to_int(), content="34")
  inspect(null_char.to_int(), content="0")
}