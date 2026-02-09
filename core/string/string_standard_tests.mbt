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
  let empty_string = @string.default()
  inspect(empty_string, content="")
}

///|
test "string length" {
  inspect("".length(), content="0")
  inspect("hello".length(), content="5")
  inspect("你好".length(), content="2") // Unicode characters count as 1
}

///|
test "string empty check" {
  inspect("".is_empty(), content="true")
  inspect("hello".is_empty(), content="false")
}

///|
test "string char access" {
  let s = "hello"
  inspect(s.char_at(0), content="Some('h')")
  inspect(s.char_at(4), content="Some('o')")
  inspect(s.char_at(5), content="None") // Out of bounds
  inspect(s.char_at(-1), content="None") // Negative index
}

///|
test "string concat" {
  inspect("hello" + " " + "world", content="hello world")
  inspect("" + "test", content="test")
  inspect("test" + "", content="test")
}

///|
test "string substring" {
  let s = "hello world"
  inspect(s.substring(0, 5), content="hello")
  inspect(s.substring(6, 11), content="world")
  inspect(s.substring(0, 11), content="hello world")
  inspect(s.substring(3, 8), content="lo wo")
}

///|
test "string contains" {
  let s = "hello world"
  inspect(s.contains("hello"), content="true")
  inspect(s.contains("world"), content="true")
  inspect(s.contains("test"), content="false")
  inspect(s.contains(""), content="true") // Empty string is always contained
}

///|
test "string starts with" {
  let s = "hello world"
  inspect(s.starts_with("hello"), content="true")
  inspect(s.starts_with("world"), content="false")
  inspect(s.starts_with(""), content="true") // Empty prefix
}

///|
test "string ends with" {
  let s = "hello world"
  inspect(s.ends_with("world"), content="true")
  inspect(s.ends_with("hello"), content="false")
  inspect(s.ends_with(""), content="true") // Empty suffix
}