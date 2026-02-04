// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_identity_elements" {
  // 测试加法的单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_identity_elements" {
  // 测试乘法的单位元性质
  assert_eq(7, multiply(7, 1))
  assert_eq(-10, multiply(-10, 1))
  assert_eq(0, multiply(0, 1))
}

test "subtract_basic_functionality" {
  // 测试基本减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
}

test "greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价
  let unit_price = 35
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(210, subtotal)
  assert_eq(17, tax_amount)
  assert_eq(239, total)
}

test "packaging_optimization_scenario" {
  // 测试包装优化场景
  let total_products = 58
  let capacity_per_box = 15
  
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  let unused_space = subtract(total_capacity, total_products)
  
  assert_eq(4, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, unused_space)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(12, multiply(-4, -3))
  assert_eq(-12, multiply(4, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "edge_case_error_handling" {
  // 测试边界情况错误处理
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零
  assert_eq(0, divide_with_ceil(-50, 0))  // 负数除以零
  assert_eq(0, divide_with_ceil(0, 0))    // 零除以零
  
  // 测试极值
  assert_eq(1, divide_with_ceil(1, 1000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
}