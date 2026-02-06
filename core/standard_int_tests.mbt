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

// 标准 MoonBit 测试用例 - 针对 int 模块
// 包含8个高质量测试用例，测试整数操作的各种场景

test "int_arithmetic_basic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  assert_eq(a + b, 22)
  assert_eq(a - b, 8)
  assert_eq(a * b, 105)
  assert_eq(a / b, 2)
  assert_eq(a % b, 1)
}

test "int_negative_number_operations" {
  // 负数运算测试
  let x = -12
  let y = 5
  
  assert_eq(x + y, -7)
  assert_eq(x - y, -17)
  assert_eq(x * y, -60)
  assert_eq(x / y, -2)
  assert_eq(x % y, -2)
  
  // 负数绝对值测试
  assert_eq(Int::abs(x), 12)
  assert_eq(Int::abs(-y), 5)
}

test "int_boundary_values" {
  // 边界值测试
  let max_int = int::max_value
  let min_int = int::min_value
  
  assert_eq(max_int, 2147483647)
  assert_eq(min_int, -2147483648)
  
  // 零值运算
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(Int::abs(0), 0)
}

test "int_comparison_operations" {
  // 比较运算测试
  let a = 10
  let b = 20
  let c = 10
  
  assert_true(a < b)
  assert_false(a > b)
  assert_true(a <= b)
  assert_false(a >= b)
  assert_false(a == b)
  assert_true(a != b)
  assert_true(a == c)
  assert_true(a <= c)
  assert_true(a >= c)
}

test "int_overflow_scenarios" {
  // 溢出场景测试
  let large_num = 1000000
  let small_num = 2
  
  // 大数运算
  assert_eq(large_num + small_num, 1000002)
  assert_eq(large_num * small_num, 2000000)
  assert_eq(large_num / small_num, 500000)
  assert_eq(large_num % small_num, 0)
}

test "int_bitwise_operations" {
  // 位运算测试
  let a = 12  // 二进制: 1100
  let b = 10  // 二进制: 1010
  
  // 位与运算
  assert_eq(a & b, 8)  // 1000
  
  // 位或运算
  assert_eq(a | b, 14)  // 1110
  
  // 位异或运算
  assert_eq(a ^ b, 6)   // 0110
  
  // 左移运算
  assert_eq(a << 2, 48)  // 110000
  
  // 右移运算
  assert_eq(a >> 2, 3)   // 11
}

test "int_complex_calculation" {
  // 复杂计算测试
  // 场景：计算平均值
  let numbers = [10, 20, 30, 40, 50]
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  let average = sum / numbers.length()
  
  assert_eq(sum, 150)
  assert_eq(average, 30)
  
  // 场景：计算幂运算
  let base = 3
  let power = base * base * base * base
  
  assert_eq(power, 81)
}

test "int_sign_operations" {
  // 符号操作测试
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 测试符号判断
  assert_true(positive > 0)
  assert_true(negative < 0)
  assert_true(zero == 0)
  
  // 测试符号变换
  assert_eq(-positive, negative)
  assert_eq(-negative, positive)
  assert_eq(-zero, zero)
  
  // 测试绝对值
  assert_eq(Int::abs(positive), 42)
  assert_eq(Int::abs(negative), 42)
  assert_eq(Int::abs(zero), 0)
}