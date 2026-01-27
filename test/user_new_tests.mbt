// 最终的 Azimuth 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(1, -1))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(0, multiply(0, 123))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_special_chars" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B!", greet("A+B"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：订单处理
  let product_price = 35
  let quantity = 7
  let discount = 15
  let shipping = 12
  
  let subtotal = multiply(product_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(245, subtotal)
  assert_eq(230, discounted_total)
  assert_eq(242, final_total)
}