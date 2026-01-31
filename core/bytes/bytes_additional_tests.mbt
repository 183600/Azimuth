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
test "bytes basic operations" {
  let empty = b""
  let single = b"A"
  let text = b"Hello"
  let binary = b"\x00\x01\x02\xFF"
  
  assert_eq(empty.length(), 0)
  assert_eq(single.length(), 1)
  assert_eq(text.length(), 5)
  assert_eq(binary.length(), 4)
  
  inspect(empty[0], content="0")
  inspect(single[0], content="65")
  inspect(text[0], content="72")
  inspect(binary[0], content="0")
  inspect(binary[3], content="255")
}

///|
test "bytes comparison" {
  let bytes1 = b"Hello"
  let bytes2 = b"Hello"
  let bytes3 = b"World"
  let bytes4 = b"hello"
  
  assert_eq(bytes1 == bytes2, true)
  assert_eq(bytes1 != bytes3, true)
  assert_eq(bytes1 != bytes4, true)
  assert_eq(bytes3 > bytes1, true)
  assert_eq(bytes1 < bytes4, true)
}

///|
test "bytes slicing operations" {
  let text = b"Hello, World!"
  
  inspect(text[0..5], content=b"Hello")
  inspect(text[7..12], content=b"World")
  inspect(text[0..0], content=b"")
  inspect(text[13..13], content=b"")
  inspect(text[0..13], content=b"Hello, World!")
}

///|
test "bytes concatenation" {
  let part1 = b"Hello"
  let part2 = b", "
  let part3 = b"World"
  let part4 = b"!"
  
  let result = part1 + part2 + part3 + part4
  inspect(result, content=b"Hello, World!")
  
  let empty1 = b""
  let empty2 = b""
  let empty_result = empty1 + empty2
  assert_eq(empty_result.length(), 0)
}

///|
test "bytes search operations" {
  let text = b"Hello, World! Hello again!"
  
  assert_eq(text.find(b"Hello"), Some(0))
  assert_eq(text.find(b"World"), Some(7))
  assert_eq(text.find(b"again"), Some(20))
  assert_eq(text.find(b"xyz"), None)
  assert_eq(text.find(b""), Some(0))
  
  assert_eq(text.rfind(b"Hello"), Some(14))
  assert_eq(text.rfind(b"World"), Some(7))
  assert_eq(text.rfind(b"xyz"), None)
}

///|
test "bytes contains operation" {
  let text = b"The quick brown fox jumps over the lazy dog"
  
  assert_eq(text.contains(b"quick"), true)
  assert_eq(text.contains(b"QUICK"), false)
  assert_eq(text.contains(b"fox"), true)
  assert_eq(text.contains(b"cat"), false)
  assert_eq(text.contains(b""), true)
  assert_eq(text.contains(text), true)
}

///|
test "bytes starts with and ends with" {
  let text = b"Hello, World!"
  
  assert_eq(text.starts_with(b"Hello"), true)
  assert_eq(text.starts_with(b"hello"), false)
  assert_eq(text.starts_with(b""), true)
  assert_eq(text.starts_with(b"Hello, World!"), true)
  assert_eq(text.starts_with(b"World"), false)
  
  assert_eq(text.ends_with(b"World!"), true)
  assert_eq(text.ends_with(b"world!"), false)
  assert_eq(text.ends_with(b""), true)
  assert_eq(text.ends_with(b"Hello, World!"), true)
  assert_eq(text.ends_with(b"Hello"), false)
}

///|
test "bytes to array conversion" {
  let bytes = b"ABC"
  let array = bytes.to_array()
  
  assert_eq(array.length(), 3)
  assert_eq(array[0], 65)  // 'A'
  assert_eq(array[1], 66)  // 'B'
  assert_eq(array[2], 67)  // 'C'
}

///|
test "bytes from array conversion" {
  let array = [65, 66, 67]  // ['A', 'B', 'C']
  let bytes = @bytes.from_array(array)
  
  assert_eq(bytes.length(), 3)
  assert_eq(bytes, b"ABC")
}

///|
test "bytes iteration" {
  let text = b"ABC"
  let mut sum = 0
  for byte in text {
    sum = sum + byte.to_int()
  }
  assert_eq(sum, 198)  // 65 + 66 + 67
}