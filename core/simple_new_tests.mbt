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

// 简单的 MoonBit 测试用例集合
// 包含 8 个基本功能测试，使用标准 MoonBit 测试语法

test "int_basic_arithmetic" {
  // 测试整数基本算术运算
  inspect(1 + 2, content="3")
  inspect(5 - 3, content="2")
  inspect(3 * 4, content="12")
  inspect(8 / 2, content="4")
  inspect(10 % 3, content="1")
}

test "int_comparisons" {
  // 测试整数比较运算
  inspect(5 > 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(2 < 4, content="true")
  inspect(4 <= 4, content="true")
  inspect(5 == 5, content="true")
  inspect(5 != 3, content="true")
}

test "int_negative_operations" {
  // 测试负数运算
  inspect(-5, content="-5")
  inspect(-10 + 3, content="-7")
  inspect(-2 * 3, content="-6")
  inspect(-8 / 2, content="-4")
  inspect(-(-5), content="5")
}

test "int_boundary_values" {
  // 测试整数边界值
  inspect(2147483647, content="2147483647")
  inspect(-2147483648, content="-2147483648")
  inspect(0, content="0")
  inspect(2147483647 + 1, content="-2147483648")  // 溢出
  inspect(-2147483648 - 1, content="2147483647")  // 下溢
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  inspect(empty.length(), content="0")
  inspect(hello.length(), content="5")
  inspect(hello + " " + world, content="Hello World")
  inspect("abc" == "abc", content="true")
  inspect("abc" != "def", content="true")
}

test "string_character_operations" {
  // 测试字符串字符操作
  let text = "Hello"
  inspect(text.char_at(0), content="'H'")
  inspect(text.char_at(1), content="'e'")
  inspect(text.char_at(4), content="'o'")
  
  // 测试Unicode字符
  let unicode = "世界"
  inspect(unicode.length(), content="2")
  inspect(unicode.char_at(0), content="'世'")
  inspect(unicode.char_at(1), content="'界'")
}

test "boolean_logic" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let empty = []
  let numbers = [1, 2, 3, 4, 5]
  
  inspect(empty.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // 测试数组比较
  inspect([1, 2] == [1, 2], content="true")
  inspect([1, 2] != [2, 1], content="true")
}