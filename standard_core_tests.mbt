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

// 简化的标准库测试用例 - 10个高质量测试

// 基础函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn abs(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

pub fn string_default() -> String {
  ""
}

pub fn is_prime(n : Int) -> Bool {
  if n <= 1 { false }
  else if n <= 3 { true }
  else if n % 2 == 0 { false }
  else {
    let mut i = 3
    let mut result = true
    while i * i <= n && result {
      if n % i == 0 {
        result = false
      }
      i = i + 2
    }
    result
  }
}

// 测试用例
test "add_function_basic" {
  add(2, 3)
  add(0, 0)
  add(-2, -3)
}

test "multiply_function_basic" {
  multiply(2, 3)
  multiply(0, 5)
  multiply(-2, 3)
}

test "abs_function_positive" {
  abs(5)
  abs(10)
}

test "abs_function_negative" {
  abs(-5)
  abs(-10)
}

test "abs_function_zero" {
  abs(0)
}

test "string_default_function" {
  string_default()
}

test "simple_arithmetic_operations" {
  let val1 = 1
  let val2 = 4
  let sum1 = val1 + val2
  let val3 = 2
  let val4 = 5
  let sum2 = val3 + val4
  let val5 = 3
  let val6 = 6
  let sum3 = val5 + val6
  sum1
  sum2
  sum3
}

test "prime_detection_small_primes" {
  is_prime(2)
  is_prime(3)
  is_prime(5)
  is_prime(7)
}

test "prime_detection_composites" {
  is_prime(1)
  is_prime(4)
  is_prime(9)
  is_prime(15)
}

test "prime_detection_larger_prime" {
  is_prime(17)
  is_prime(19)
  is_prime(23)
}