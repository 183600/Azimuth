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

// 标准 MoonBit 测试用例集合 - 使用项目函数

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(-1, subtract(2, 3))
  assert_eq(15, add(7, 8))
}

test "mathematical_properties" {
  // 测试数学性质
  assert_eq(10, add(5, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(0, multiply(5, 0))
}

test "greeting_function_tests" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "division_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, subtract(-2, -3))
  assert_eq(-4, divide_with_ceil(-10, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = subtract(multiply(10, 5), add(3, 2))
  assert_eq(45, result2)  // 10*5 - (3+2) = 50 - 5 = 45
}

test "edge_cases_and_boundaries" {
  // 测试边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(12345, -1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "practical_business_calculations" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let discount = 15
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let discounted_total = subtract(subtotal, discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(185, discounted_total)
  assert_eq(195, final_total)
}

test "resource_allocation_calculations" {
  // 资源分配计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_slots = subtract(total_capacity, total_items)
  
  assert_eq(48, total_capacity)
  assert_eq(1, empty_slots)
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式
  let x = 7
  let y = 13
  
  // (x + y) * (x - y)
  let result1 = multiply(add(x, y), subtract(x, y))
  assert_eq(-120, result1)  // (7+13)*(7-13) = 20*(-6) = -120
  
  // x*x + y*y
  let result2 = add(multiply(x, x), multiply(y, y))
  assert_eq(218, result2)  // 7*7 + 13*13 = 49 + 169 = 218
}