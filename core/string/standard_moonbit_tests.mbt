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
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  // 测试字符串连接
  let greeting = hello + " " + world
  inspect(greeting, content="Hello World")
  
  // 测试空字符串
  inspect(empty, content="")
  inspect(empty.length(), content="0")
  
  // 测试字符串长度
  inspect(hello.length(), content="5")
  inspect(world.length(), content="5")
  inspect(greeting.length(), content="11")
}

///|
test "string unicode support" {
  // 测试 Unicode 字符串支持
  let chinese = "你好世界"
  let emoji = "🌟🚀"
  let mixed = "Hello 世界 🌟"
  
  inspect(chinese.length(), content="12")  // UTF-8 编码长度
  inspect(emoji.length(), content="8")      // UTF-8 编码长度
  inspect(mixed.length(), content="16")     // UTF-8 编码长度
  
  // 测试 Unicode 字符串比较
  let same_chinese = "你好世界"
  let different_chinese = "你好"
  
  inspect(chinese == same_chinese, content="true")
  inspect(chinese == different_chinese, content="false")
}

///|
test "string edge cases" {
  // 测试字符串边界情况
  let single_char = "a"
  let space = " "
  let special_chars = "!@#$%^&*()"
  let newline = "\n"
  let tab = "\t"
  
  // 测试单字符
  inspect(single_char.length(), content="1")
  
  // 测试空格
  inspect(space.length(), content="1")
  
  // 测试特殊字符
  inspect(special_chars.length(), content="10")
  
  // 测试转义字符
  inspect(newline.length(), content="1")
  inspect(tab.length(), content="1")
  
  // 测试包含转义字符的字符串
  let text_with_escapes = "Line1\nLine2\tEnd"
  inspect(text_with_escapes.length(), content="15")
}