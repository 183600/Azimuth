// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含数学性质、实际应用场景和边界情况

test "mathematical_identities_verification" {
  // 验证基本数学恒等式
  // 加法恒等式：a + 0 = a
  let test_value = 42
  assert_eq(test_value, add(test_value, 0))
  
  // 乘法恒等式：a * 1 = a
  assert_eq(test_value, multiply(test_value, 1))
  
  // 乘法零性质：a * 0 = 0
  assert_eq(0, multiply(test_value, 0))
  
  // 加法逆元：a + (-a) = 0
  assert_eq(0, add(test_value, -test_value))
}

test "real_world_shopping_cart" {
  // 模拟真实购物车计算
  let apple_price = 8
  let apple_qty = 3
  let banana_price = 5
  let banana_qty = 4
  let orange_price = 12
  let orange_qty = 2
  
  // 计算每种水果的小计
  let apple_subtotal = multiply(apple_price, apple_qty)
  let banana_subtotal = multiply(banana_price, banana_qty)
  let orange_subtotal = multiply(orange_price, orange_qty)
  
  // 计算总价
  let total_cost = add(add(apple_subtotal, banana_subtotal), orange_subtotal)
  
  assert_eq(24, apple_subtotal)
  assert_eq(20, banana_subtotal)
  assert_eq(24, orange_subtotal)
  assert_eq(68, total_cost)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试接近整数边界的除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(2, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试极值加法
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multilingual_greeting_scenarios" {
  // 多语言问候场景测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款利息计算
  let principal = 10000
  let annual_rate = 6
  let _years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, annual_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, annual_rate), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(600, year1_interest)
  assert_eq(10600, year1_total)
  assert_eq(636, year2_interest)
  assert_eq(11236, year2_total)
  assert_eq(675, year3_interest)
  assert_eq(11911, final_amount)
}

test "simple_zero_test" {
  // 测试与零相关的运算
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "simple_large_number_test" {
  // 测试大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
}

test "simple_combined_test" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "simple_packaging_test" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_unicode_test" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}