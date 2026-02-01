// 新质量测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(10, subtract(20, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(-5, subtract(10, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(304, total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum)
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  let sold = multiply(daily_sales, days)
  let remaining = subtract(initial_stock, sold)
  let final_stock = add(remaining, new_shipment)
  
  assert_eq(175, sold)
  assert_eq(325, remaining)
  assert_eq(525, final_stock)
}

test "performance_metrics_calculation" {
  // 性能指标计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percent = 12
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percent), 100)
  let actual_performance = subtract(optimized_ops, overhead)
  
  assert_eq(3000, optimized_ops)
  assert_eq(360, overhead)
  assert_eq(2640, actual_performance)
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 极值测试
  assert_eq(0, add(1000000, -1000000))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, -1))
  
  // 字符串边界测试
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long string for testing!", greet("This is a very long string for testing!"))
  
  // 除法安全测试
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}