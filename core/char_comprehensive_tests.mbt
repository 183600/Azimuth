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
test "char basic operations" {
  let lowercase = 'a'
  let uppercase = 'A'
  let digit = '5'
  let special = '@'
  
  inspect(lowercase.to_string(), content="a")
  inspect(uppercase.to_string(), content="A")
  inspect(digit.to_string(), content="5")
  inspect(special.to_string(), content="@")
}

///|
test "char comparison operations" {
  let a = 'a'
  let b = 'b'
  let A = 'A'
  
  inspect(a == a, content="true")
  inspect(a == b, content="false")
  inspect(a != b, content="true")
  inspect(a < b, content="true")
  inspect(a > b, content="false")
  inspect(a <= b, content="true")
  inspect(a >= b, content="false")
  inspect(A < a, content="true")
}

///|
test "char to byte conversion" {
  let char_a = 'a'
  let char_A = 'A'
  let char_zero = '0'
  
  inspect(char_a.to_byte(), content="97")
  inspect(char_A.to_byte(), content="65")
  inspect(char_zero.to_byte(), content="48")
}

///|
test "char escape sequences" {
  let newline = '\n'
  let tab = '\t'
  let carriage_return = '\r'
  let backspace = '\b'
  let backslash = '\\'
  let single_quote = '\''
  let double_quote = '"'
  
  inspect(newline.to_string(), content="\n")
  inspect(tab.to_string(), content="\t")
  inspect(carriage_return.to_string(), content="\r")
  inspect(backspace.to_string(), content="\b")
  inspect(backslash.to_string(), content="\\")
  inspect(single_quote.to_string(), content="'")
  inspect(double_quote.to_string(), content="\"")
}

///|
test "char numeric properties" {
  let digit_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
  let letter_chars = ['a', 'b', 'c', 'A', 'B', 'C']
  let special_chars = ['!', '@', '#', '$', '%']
  
  // Test digit characters
  for ch in digit_chars {
    let byte_val = ch.to_byte()
    inspect(byte_val >= 48 && byte_val <= 57, content="true")
  }
  
  // Test uppercase letters
  for ch in ['A', 'B', 'C'] {
    let byte_val = ch.to_byte()
    inspect(byte_val >= 65 && byte_val <= 90, content="true")
  }
  
  // Test lowercase letters
  for ch in ['a', 'b', 'c'] {
    let byte_val = ch.to_byte()
    inspect(byte_val >= 97 && byte_val <= 122, content="true")
  }
}

///|
test "char case conversion logic" {
  let lowercase_a = 'a'
  let uppercase_A = 'A'
  
  // Manual case conversion (using byte arithmetic)
  let a_to_upper = lowercase_a.to_byte() - 32
  let A_to_lower = uppercase_A.to_byte() + 32
  
  inspect(a_to_upper, content="65")  // 'A'
  inspect(A_to_lower, content="97")  // 'a'
}

///|
test "char boundary values" {
  let null_char = (0).unsafe_to_char()
  let max_ascii = (127).unsafe_to_char()
  
  inspect(null_char.to_byte(), content="0")
  inspect(max_ascii.to_byte(), content="127")
}

///|
test "char in string context" {
  let chars = ['H', 'e', 'l', 'l', 'o']
  let result = chars.fold("", (acc, ch) => acc + ch.to_string())
  
  inspect(result, content="Hello")
  
  let numbers = ['1', '2', '3']
  let num_result = numbers.fold("", (acc, ch) => acc + ch.to_string())
  
  inspect(num_result, content="123")
}