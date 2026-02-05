// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

import azimuth

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(100, add(50, 50))
}

test "multiply_function_zero_cases" {
  // 测试乘法函数的零值情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(-10, 0))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(0, -15))
}

test "divide_with_ceil_simple_cases" {
  // 测试向上取整除法的简单情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(3, divide_with_ceil(7, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob123!", greet("Bob123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation_simple" {
  // 简单业务计算场景
  let price_per_item = 15
  let quantity = 4
  let tax = 10
  
  let subtotal = multiply(price_per_item, quantity)
  let total = add(subtotal, tax)
  
  assert_eq(60, subtotal)
  assert_eq(70, total)
}

test "packaging_efficiency" {
  // 包装效率计算
  let total_products = 23
  let capacity_per_box = 6
  
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  let wasted_space = subtract(total_capacity, total_products)
  
  assert_eq(4, boxes_needed)
  assert_eq(24, total_capacity)
  assert_eq(1, wasted_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_expression_evaluation" {
  // 复杂表达式求值
  let x = 5
  let y = 3
  let z = 2
  
  // 计算: (x + y) * z - x
  let result = subtract(multiply(add(x, y), z), x)
  assert_eq(11, result)  // (5 + 3) * 2 - 5 = 8 * 2 - 5 = 16 - 5 = 11
  
  // 计算: x * y + z * x
  let result2 = add(multiply(x, y), multiply(z, x))
  assert_eq(25, result2)  // 5 * 3 + 2 * 5 = 15 + 10 = 25
}