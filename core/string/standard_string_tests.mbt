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
test "string concatenation" {
  let hello = "Hello"
  let world = "World"
  let result = hello + " " + world
  assert_eq(result, "Hello World")
}

///|
test "string equality" {
  let s1 = "test"
  let s2 = "test"
  let s3 = "different"
  assert_eq(s1 == s2, true)
  assert_eq(s1 == s3, false)
  assert_eq(s1 != s3, true)
}

///|
test "string length" {
  let empty = ""
  let short = "hi"
  let long = "Hello, World!"
  assert_eq(empty.length(), 0)
  assert_eq(short.length(), 2)
  assert_eq(long.length(), 13)
}

///|
test "string default" {
  let default_str = @string.default()
  assert_eq(default_str, "")
}

///|
test "string comparison" {
  let a = "apple"
  let b = "banana"
  let c = "apple"
  assert_eq(a < b, true)
  assert_eq(b > a, true)
  assert_eq(a <= c, true)
  assert_eq(a >= c, true)
}

///|
test "string indexing" {
  let s = "Hello"
  assert_eq(s[0], 'H')
  assert_eq(s[1], 'e')
  assert_eq(s[4], 'o')
}

///|
test "string slicing" {
  let s = "Hello, World"
  assert_eq(s.slice(0, 5), "Hello")
  assert_eq(s.slice(7, 12), "World")
}