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

// 高质量核心测试用例集
// 包含 8 个高质量测试用例，测试 Azimuth 项目核心功能

test "comprehensive_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(1000, add(500, 500))
  
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 测试负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  
  // 加法交换律
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 12
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 减法性质
  assert_eq(subtract(a, b), add(a, -b))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况
  
  // 大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(100, subtract(100, 0))
  
  // 特殊除法情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 复合运算
  let result = add(multiply(5, 3), subtract(20, 8))
  assert_eq(27, result)  // 5*3 + (20-8) = 15 + 12 = 27
  
  // 嵌套运算
  let nested = multiply(add(10, 5), subtract(100, divide_with_ceil(20, 3)))
  assert_eq(585, nested)  // (10+5) * (100-7) = 15 * 93 = 1395
  
  // 链式运算
  let chain = subtract(multiply(add(2, 3), 4), divide_with_ceil(25, 5))
  assert_eq(15, chain)  // (2+3)*4 - 5 = 5*4 - 5 = 20 - 5 = 15
}

test "string_processing_validation" {
  // 测试字符串处理
  
  // 基本问候功能
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, Charlie!", greet(name3))
  
  // 特殊字符处理
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "performance_and_stress_testing" {
  // 性能和压力测试
  
  // 重复运算测试
  let sum = 0
  let i = 0
  while (i < 100) {
    sum = add(sum, 1)
    i = i + 1
  }
  assert_eq(100, sum)
  
  // 乘法累加测试
  let product = 1
  let j = 0
  while (j < 5) {
    product = multiply(product, 2)
    j = j + 1
  }
  assert_eq(32, product)  // 2^5 = 32
  
  // 复合运算压力测试
  let k = 0
  let complex_result = 0
  while (k < 10) {
    complex_result = add(complex_result, multiply(k, subtract(10, k)))
    k = k + 1
  }
  assert_eq(165, complex_result)  // Σ(k*(10-k)) for k=0..9
}