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

// 简化的MoonBit测试用例，使用标准测试语法

test "整数基本算术运算" {
  // 测试基本算术运算
  let sum = 2 + 3
  let diff = 10 - 4
  let product = 3 * 4
  let quotient = 15 / 3
  let remainder = 17 % 5
  
  // 验证结果
  @test.assert_eq(sum, 5)
  @test.assert_eq(diff, 6)
  @test.assert_eq(product, 12)
  @test.assert_eq(quotient, 5)
  @test.assert_eq(remainder, 2)
}

test "整数位运算" {
  // 测试位操作
  let and_result = 8 & 7
  let or_result = 8 | 7
  let xor_result = 8 ^ 7
  let not_result = -9  // ~8
  let left_shift = 1 << 3
  let right_shift = 8 >> 3
  
  // 验证结果
  @test.assert_eq(and_result, 0)
  @test.assert_eq(or_result, 15)
  @test.assert_eq(xor_result, 15)
  @test.assert_eq(not_result, -9)
  @test.assert_eq(left_shift, 8)
  @test.assert_eq(right_shift, 1)
}

test "整数比较运算" {
  // 测试比较操作
  let greater = 5 > 3
  let less = 5 < 3
  let greater_equal = 5 >= 5
  let less_equal = 5 <= 3
  let equal = 5 == 5
  let not_equal = 5 != 3
  
  // 验证结果
  @test.assert_eq(greater, true)
  @test.assert_eq(less, false)
  @test.assert_eq(greater_equal, true)
  @test.assert_eq(less_equal, false)
  @test.assert_eq(equal, true)
  @test.assert_eq(not_equal, true)
}

test "字符串基本操作" {
  // 测试字符串连接
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  
  // 验证结果
  @test.assert_eq(combined, "Hello World")
}

test "数组基本操作" {
  // 测试数组创建和访问
  let numbers = [1, 2, 3, 4, 5]
  let length = numbers.length()
  let first = numbers[0]
  let last = numbers[4]
  
  // 验证结果
  @test.assert_eq(length, 5)
  @test.assert_eq(first, 1)
  @test.assert_eq(last, 5)
}

test "布尔逻辑运算" {
  // 测试布尔操作
  let t = true
  let f = false
  let and_true = t && t
  let and_false = t && f
  let or_true = t || f
  let or_false = f || f
  let not_true = !t
  let not_false = !f
  
  // 验证结果
  @test.assert_eq(and_true, true)
  @test.assert_eq(and_false, false)
  @test.assert_eq(or_true, true)
  @test.assert_eq(or_false, false)
  @test.assert_eq(not_true, false)
  @test.assert_eq(not_false, true)
}

test "元组操作" {
  // 测试元组创建和访问
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  let pair_first = pair.0
  let pair_second = pair.1
  let triple_first = triple.0
  let triple_second = triple.1
  let triple_third = triple.2
  
  // 验证结果
  @test.assert_eq(pair_first, 42)
  @test.assert_eq(pair_second, "answer")
  @test.assert_eq(triple_first, 1)
  @test.assert_eq(triple_second, 2)
  @test.assert_eq(triple_third, 3)
}

test "整数边界值" {
  // 测试整数边界
  let max_int = 2147483647
  let min_int = -2147483648
  let overflow_max = max_int + 1
  let overflow_min = min_int - 1
  
  // 验证结果
  @test.assert_eq(max_int, 2147483647)
  @test.assert_eq(min_int, -2147483648)
  @test.assert_eq(overflow_max, -2147483648)
  @test.assert_eq(overflow_min, 2147483647)
}

test "十六进制字面量" {
  // 测试十六进制数表示
  let ff = 0xFF
  let zero = 0x00
  let sixteen = 0x10
  let ffff = 0xFFFF
  
  // 验证结果
  @test.assert_eq(ff, 255)
  @test.assert_eq(zero, 0)
  @test.assert_eq(sixteen, 16)
  @test.assert_eq(ffff, 65535)
}

test "复杂表达式" {
  // 测试复杂算术表达式
  let expr1 = 2 + 3 * 4
  let expr2 = (2 + 3) * 4
  let expr3 = 10 - 2 * 3
  let expr4 = 20 / (4 + 1)
  
  // 验证结果
  @test.assert_eq(expr1, 14)
  @test.assert_eq(expr2, 20)
  @test.assert_eq(expr3, 4)
  @test.assert_eq(expr4, 4)
}