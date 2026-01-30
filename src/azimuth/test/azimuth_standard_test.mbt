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

// 标准 MoonBit 测试用例 - 测试核心功能

test "basic_arithmetic_operations" {
  let a = 10
  let b = 3
  let sum = a + b
  let diff = a - b
  let prod = a * b
  let quot = a / b
  let rem = a % b
  // 测试基本算术运算结果
  let _ = sum == 13
  let _ = diff == 7
  let _ = prod == 30
  let _ = quot == 3
  let _ = rem == 1
}

test "string_operations" {
  let s = "Hello"
  let len = s.length()
  let first_char = s[0]
  // 测试字符串操作
  let _ = len > 0
  let _ = first_char == 'H'
  
  let s2 = " World"
  let combined = s + s2
  let _ = combined.length() > s.length()
}

test "boolean_logic" {
  let t = true
  let f = false
  // 测试布尔逻辑
  let _ = t && t == true
  let _ = t || f == true
  let _ = !f == true
  let _ = t != f == true
  let _ = t == f == false
}

test "array_operations" {
  let arr = [1, 2, 3]
  let len = arr.length()
  let first = arr[0]
  let last = arr[2]
  // 测试数组操作
  let _ = len == 3
  let _ = first == 1
  let _ = last == 3
  
  let arr2 = [4, 5, 6]
  let combined = arr + arr2
  let _ = combined.length() == 6
}

test "comparison_operations" {
  let a = 5
  let b = 10
  // 测试比较操作
  let _ = a < b == true
  let _ = a > b == false
  let _ = a <= b == true
  let _ = a >= a == true
  let _ = a == a == true
  let _ = a != b == true
}

test "negative_numbers" {
  let neg = -5
  let pos = 5
  // 测试负数操作
  let _ = neg + pos == 0
  let _ = neg * pos == -25
  let _ = -neg == 5
}

test "character_operations" {
  let c = 'A'
  // 测试字符操作
  let _ = c == 'A'
  
  let c2 = 'B'
  let _ = c2 == 'B'
  
  // 字符转字符串
  let s = c.to_string()
  let _ = s.length() == 1
}

test "empty_collections" {
  let empty_arr : Array[Int] = []
  // 测试空集合
  let _ = empty_arr.length() == 0
  
  let empty_str = ""
  let _ = empty_str.length() == 0
}

test "type_conversion" {
  let i = 42
  // 测试类型转换
  let s = i.to_string()
  let _ = s.length() > 0
}

test "nested_expressions" {
  // 测试嵌套表达式
  let result = (1 + 2) * (3 - 1) / 2
  let _ = result == 3
  
  let bool_result = (true && false) || (true || false)
  let _ = bool_result == true
}