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

// 标准 MoonBit 测试用例集
// 包含 8 个核心功能测试，使用标准 MoonBit 测试语法

test "int_basic_arithmetic" {
  // 测试整数基本算术运算
  inspect(1 + 2, content="3")
  inspect(10 - 3, content="7")
  inspect(4 * 5, content="20")
  inspect(15 / 3, content="5")
  inspect(17 % 5, content="2")
  
  // 测试负数运算
  inspect(-5 + 3, content="-2")
  inspect(-10 * -2, content="20")
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let text = "MoonBit"
  inspect(text.length(), content="7")
  
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  inspect(hello + " " + world, content="Hello World")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  inspect(numbers.length(), content="5")
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  
  inspect(!true, content="false")
  inspect(!false, content="true")
}

test "char_basic_operations" {
  // 测试字符基本操作
  let char_a = 'a'
  let char_A = 'A'
  
  inspect(char_a.to_string(), content="a")
  inspect(char_A.to_string(), content="A")
  
  // 测试字符比较
  inspect(char_a < 'z', content="true")
  inspect(char_A < char_a, content="true")
  inspect(char_a != char_A, content="true")
}

test "int_comparisons" {
  // 测试整数比较
  inspect(1 < 2, content="true")
  inspect(2 > 1, content="true")
  inspect(3 <= 3, content="true")
  inspect(3 >= 3, content="true")
  inspect(4 == 4, content="true")
  inspect(5 != 6, content="true")
}

test "string_comparisons" {
  // 测试字符串比较
  inspect("apple" < "banana", content="true")
  inspect("zebra" > "apple", content="true")
  inspect("test" == "test", content="true")
  inspect("hello" != "world", content="true")
}

test "complex_expressions" {
  // 测试复杂表达式
  inspect((1 + 2) * (3 + 4), content="21")
  inspect(10 - 3 * 2, content="4")
  inspect(20 / (2 + 3), content="4")
  inspect(5 + 6 + 7 + 8, content="26")
}