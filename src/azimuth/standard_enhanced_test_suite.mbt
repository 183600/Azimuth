// 标准 MoonBit 测试套件 - 增强版
// 测试 azimuth 包的核心功能

test "enhanced_add_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -12345
  let d = 67890
  
  let result3 = add(c, d)
  let result4 = add(d, c)
  assert_eq(result3, result4)
}

test "enhanced_multiply_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 20
  let c = 25
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "enhanced_divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-4, 4))
}

test "enhanced_string_processing_multilingual" {
  // 测试多语言字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌎!", greet("🌎"))
}

test "enhanced_mathematical_series" {
  // 测试数学级数计算
  // 计算等比数列：2 + 4 + 8 + 16 = 30
  let term1 = 2
  let term2 = multiply(term1, 2)
  let term3 = multiply(term2, 2)
  let term4 = multiply(term3, 2)
  
  let series_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(30, series_sum)
}

test "enhanced_boundary_stress_test" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续边界运算
  let step1 = add(max_val, -1000000)
  let step2 = add(step1, 500000)
  let step3 = add(step2, -250000)
  let step4 = add(step3, 125000)
  let step5 = add(step4, -62500)
  
  assert_eq(2147483647, step5)
  
  // 乘法链测试
  let mul_step1 = multiply(1000, 1000)
  let mul_step2 = multiply(mul_step1, 1000)
  let mul_step3 = multiply(mul_step2, 2)
  
  assert_eq(2000000000, mul_step3)
}

test "enhanced_financial_compound_interest" {
  // 复利计算测试（更精确的版本）
  let principal = 10000
  let rate_percent = 8
  let years = 4
  
  // 简化的复利计算，避免浮点数
  // 本息和 = 本金 × (1 + 利率%)^年数
  let growth_factor = add(100, rate_percent)
  let growth_squared = multiply(growth_factor, growth_factor)
  let growth_fourth = multiply(growth_squared, growth_squared)
  let total = multiply(principal, growth_fourth) / 1000000
  
  assert_eq(13605, total)
}

test "enhanced_scientific_notation_simulation" {
  // 科学计数法模拟测试
  // 模拟 1.23 × 10^6 = 1230000
  let mantissa = 123
  let exponent = 4
  let result = multiply(mantissa, multiply(10, multiply(10, multiply(10, 10))))
  assert_eq(1230000, result)
  
  // 模拟 4.56 × 10^3 = 4560
  let mantissa2 = 456
  let exponent2 = 1
  let result2 = multiply(mantissa2, 10)
  assert_eq(4560, result2)
}

test "enhanced_error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作
  let overflow1 = add(max_val, 1000)
  let overflow2 = add(overflow1, 1000)
  let overflow3 = add(overflow2, 1000)
  
  assert_eq(max_val, overflow3)
  
  // 测试乘法溢出链
  let mul_overflow1 = multiply(100000, 100000)
  let mul_overflow2 = multiply(mul_overflow1, 1000)
  
  assert_eq(max_val, mul_overflow2)
  
  // 测试断言函数的边界情况
  assert_true(max_val > 0)
  assert_false(min_val > 0)
  assert_eq(max_val, 2147483647)
  assert_eq_string("max", "max")
}

test "enhanced_real_world_inventory_management" {
  // 实际库存管理场景测试
  let initial_stock = 1000
  let sales_day1 = 150
  let sales_day2 = 200
  let sales_day3 = 175
  let restock_day2 = 300
  let restock_day3 = 500
  
  // 计算每日库存
  let stock_day1 = add(initial_stock, -sales_day1)
  let stock_day2_before = add(stock_day1, -sales_day2)
  let stock_day2_after = add(stock_day2_before, restock_day2)
  let stock_day3_before = add(stock_day2_after, -sales_day3)
  let stock_day3_after = add(stock_day3_before, restock_day3)
  
  assert_eq(1275, stock_day3_after)
  
  // 计算总销售额
  let total_sales = add(add(sales_day1, sales_day2), sales_day3)
  assert_eq(525, total_sales)
  
  // 计算总补货量
  let total_restock = add(restock_day2, restock_day3)
  assert_eq(800, total_restock)
}