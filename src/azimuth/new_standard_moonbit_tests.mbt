// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 8
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_complex_scenarios" {
  // 测试复杂除法场景
  assert_eq(4, divide_with_ceil(100, 26))
  assert_eq(7, divide_with_ceil(100, 15))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(50, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "combined_business_calculations" {
  // 组合业务计算场景
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax_amount)
  assert_eq(609, total)
}

test "mathematical_sequence_calculations" {
  // 数学序列计算
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 计算等差数列第n项
  let nth_term = add(first_term, multiply(common_diff, n_terms - 1))
  
  // 计算前n项和
  let sum = divide_with_ceil(multiply(n_terms, add(first_term, nth_term)), 2)
  
  assert_eq(23, nth_term)
  assert_eq(78, sum)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, multiply(0, -999))
  assert_eq(0, add(0, 0))
  assert_eq_string("Hello, !", greet(""))
}

test "performance_simulation_large_numbers" {
  // 性能模拟：大数运算
  let base = 10000
  let multiplier = 100
  let iterations = 50
  
  let result = multiply(multiply(base, multiplier), iterations)
  let optimized = divide_with_ceil(result, 10)
  
  assert_eq(50000000, result)
  assert_eq(5000000, optimized)
}

test "real_world_inventory_tracking" {
  // 真实世界库存跟踪
  let initial_stock = 500
  let daily_sales = 35
  let days_of_week = 7
  let new_shipment = 200
  
  let weekly_sales = multiply(daily_sales, days_of_week)
  let stock_after_sales = add(initial_stock, -weekly_sales)
  let final_stock = add(stock_after_sales, new_shipment)
  
  assert_eq(245, weekly_sales)
  assert_eq(455, final_stock)
}

test "string_processing_with_mathematics" {
  // 字符串处理与数学结合
  let target = "World"
  let repeat_count = 3
  
  let full_greeting = greet(target)
  let base_length = 5  // "Hello" 的长度
  let target_length = 5  // "World" 的长度
  let combined_length = add(base_length, target_length)
  let estimated_display_time = multiply(combined_length, repeat_count)
  
  assert_eq_string("Hello, World!", full_greeting)
  assert_eq(10, combined_length)
  assert_eq(30, estimated_display_time)
}