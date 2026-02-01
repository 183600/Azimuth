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
  let empty_string = ""
  let hello_string = "Hello"
  
  inspect(empty_string.length(), content="0")
  inspect(hello_string.length(), content="5")
  inspect(String::default(), content="")
}

///|
test "string concatenation" {
  let str1 = "Hello"
  let str2 = " "
  let str3 = "World"
  
  let result = str1 + str2 + str3
  inspect(result, content="Hello World")
  inspect(result.length(), content="11")
}

///|
test "string character access" {
  let test_string = "MoonBit"
  
  inspect(test_string[0], content="M")
  inspect(test_string[1], content="o")
  inspect(test_string[2], content="o")
  inspect(test_string[3], content="n")
  inspect(test_string[4], content="B")
  inspect(test_string[5], content="i")
  inspect(test_string[6], content="t")
}

///|
test "string make function" {
  let repeated_char = String::make(5, 'X')
  inspect(repeated_char, content="XXXXX")
  inspect(repeated_char.length(), content="5")
  
  let empty_make = String::make(0, 'A')
  inspect(empty_make, content="")
  inspect(empty_make.length(), content="0")
}

///|
test "string comparison" {
  let str1 = "hello"
  let str2 = "hello"
  let str3 = "world"
  
  inspect(str1 = str2, content="true")
  inspect(str1 = str3, content="false")
  inspect(str1 < str3, content="true")
  inspect(str3 < str1, content="false")
}

///|
test "string from character conversion" {
  let char_value = 'Z'
  let string_from_char = char_value.to_string()
  
  inspect(string_from_char, content="Z")
  inspect(string_from_char.length(), content="1")
}