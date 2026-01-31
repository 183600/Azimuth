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

using @builtin { inspect }

///|
test "string_concatenation" {
  inspect("Hello" + " " + "World", content="Hello World")
  inspect("" + "test", content="test")
  inspect("test" + "", content="test")
  inspect("" + "", content="")
}

///|
test "string_equality" {
  inspect("hello" == "hello", content="true")
  inspect("hello" == "world", content="false")
  inspect("" == "", content="true")
  inspect("test" != "different", content="true")
}

///|
test "string_length_operations" {
  inspect("".length(), content="0")
  inspect("a".length(), content="1")
  inspect("hello".length(), content="5")
  inspect("hello world".length(), content="11")
}

///|
test "string_character_access" {
  let s = "hello"
  inspect(s[0], content="'h'")
  inspect(s[4], content="'o'")
  inspect("abc"[1], content="'b'")
}

///|
test "string_equality_and_comparison" {
  let s1 = "hello"
  let s2 = "hello"
  let s3 = "world"
  
  inspect(s1 == s2, content="true")
  inspect(s1 == s3, content="false")
  inspect(s1 != s3, content="true")
}

///|
test "string_concatenation_advanced" {
  let s1 = "hello"
  let s2 = " "
  let s3 = "world"
  let combined = s1 + s2 + s3
  
  inspect(combined, content="hello world")
  inspect("" + "test", content="test")
  inspect("test" + "", content="test")
}

///|
test "string_basic_operations" {
  let empty = ""
  let single = "a"
  let phrase = "hello world"
  
  inspect(empty.length(), content="0")
  inspect(single.length(), content="1")
  inspect(phrase.length(), content="11")
}

///|
test "string_content_verification" {
  let greeting = "hello"
  let target = "world"
  
  inspect(greeting == "hello", content="true")
  inspect(target == "world", content="true")
  inspect(greeting == target, content="false")
}