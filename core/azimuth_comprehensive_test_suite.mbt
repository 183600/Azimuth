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

// Azimuth 项目综合测试套件
// 包含10个标准 MoonBit 测试用例，涵盖核心库的各种功能

///|
test "integer arithmetic operations" {
  // 整数算术运算测试
  let a = 15
  let b = 7
  
  assert_eq(a + b, 22)
  assert_eq(a - b, 8)
  assert_eq(a * b, 105)
  assert_eq(a / b, 2)
  assert_eq(a % b, 1)
  
  // 负数运算
  let x = -12
  let y = 5
  assert_eq(x + y, -7)
  assert_eq(x * y, -60)
}

///|
test "string concatenation" {
  // 字符串连接测试
  let s1 = "Hello"
  let s2 = "MoonBit"
  let s3 = s1 + " " + s2
  
  assert_eq(s3, "Hello MoonBit")
  assert_eq(s1.length(), 5)
  assert_eq(s2.length(), 7)
  assert_eq(s3.length(), 12)
}

///|
test "array basic operations" {
  // 数组基本操作测试
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[numbers.length() - 1]
  let length = numbers.length()
  
  assert_eq(first, 1)
  assert_eq(last, 5)
  assert_eq(length, 5)
}

///|
test "boolean logical operations" {
  // 布尔逻辑运算测试
  let t = true
  let f = false
  
  assert_eq(t && t, true)
  assert_eq(t && f, false)
  assert_eq(f && t, false)
  assert_eq(f && f, false)
  
  assert_eq(t || t, true)
  assert_eq(t || f, true)
  assert_eq(f || t, true)
  assert_eq(f || f, false)
  
  assert_eq(!t, false)
  assert_eq(!f, true)
}

///|
test "option type basic operations" {
  // Option 类型基本操作测试
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(some_value.is_none(), false)
  assert_eq(none_value.is_some(), false)
  assert_eq(none_value.is_none(), true)
}

///|
test "result type basic operations" {
  // Result 类型基本操作测试
  let success = Ok(42)
  let error : Result[Int, String] = Err("error")
  
  assert_eq(success.is_ok(), true)
  assert_eq(success.is_err(), false)
  assert_eq(error.is_ok(), false)
  assert_eq(error.is_err(), true)
}

///|
test "tuple access operations" {
  // 元组访问操作测试
  let pair = (42, "Answer")
  let triple = (1, 2, 3)
  
  // 元组访问
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "Answer")
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
}

///|
test "character comparison" {
  // 字符比较测试
  let char_a = 'a'
  let char_A = 'A'
  let char_z = 'z'
  
  // 字符比较
  assert_eq(char_A < char_a, true)
  assert_eq(char_a < char_z, true)
  
  // 字符转换
  let a_code = char_a.to_int()
  let A_code = char_A.to_int()
  
  assert_eq(a_code, 97)
  assert_eq(A_code, 65)
}

///|
test "arithmetic edge cases" {
  // 算术边界情况测试
  let zero = 0
  let one = 1
  let negative_one = -1
  
  assert_eq(zero + one, 1)
  assert_eq(zero - one, -1)
  assert_eq(zero * one, 0)
  assert_eq(one * negative_one, -1)
  assert_eq(negative_one * negative_one, 1)
}

///|
test "simple calculation scenario" {
  // 简单计算场景测试
  // 场景：计算数组元素的平均值
  
  let numbers = [10, 20, 30, 40, 50]
  let sum = numbers[0] + numbers[1] + numbers[2] + numbers[3] + numbers[4]
  let average = sum / numbers.length()
  
  assert_eq(sum, 150)
  assert_eq(average, 30)
}