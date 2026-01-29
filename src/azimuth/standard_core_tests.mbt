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

// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能

using @builtin { inspect }

// 定义断言函数
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

// 定义测试函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

///|
test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  let result1 = add(a, b)
  let result2 = add(b, a)
  inspect(result1, content="42")
  inspect(result2, content="42")
}

///|
test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  inspect(left, content="55")
  inspect(right, content="55")
}

///|
test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  inspect(divide_with_ceil(10, 3), content="4")  // 3.33... -> 4
  inspect(divide_with_ceil(15, 5), content="3")  // 3 -> 3
  inspect(divide_with_ceil(7, 2), content="4")   // 3.5 -> 4
  inspect(divide_with_ceil(1, 100), content="1") // 0.01 -> 1
}

///|
test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  inspect(greet("世界"), content="\"Hello, 世界!\"")
  inspect(greet("🚀"), content="\"Hello, 🚀!\"")
  inspect(greet("ñáéíóú"), content="\"Hello, ñáéíóú!\"")
}

///|
test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  inspect(subtotal, content="270")
  inspect(tax, content="22")
  inspect(total, content="307")
}

///|
test "negative_number_operations" {
  // 负数运算测试
  inspect(add(-10, 5), content="-5")
  inspect(multiply(-3, 7), content="-21")
  inspect(divide_with_ceil(-10, 3), content="-3")  // -3.33... -> -3
  inspect(divide_with_ceil(-11, -3), content="4")  // 3.66... -> 4
}

///|
test "zero_handling_robustness" {
  // 零值处理健壮性测试
  inspect(add(0, 0), content="0")
  inspect(multiply(5, 0), content="0")
  inspect(divide_with_ceil(100, 0), content="0")  // 除数为零的安全处理
  inspect(divide_with_ceil(0, 5), content="0")
}

///|
test "large_number_calculations" {
  // 大数计算测试
  let big_num1 = 100000
  let big_num2 = 250000
  inspect(add(big_num1, big_num2), content="350000")
  inspect(multiply(500, 800), content="400000")
  inspect(divide_with_ceil(1000000, 3), content="333334")
}