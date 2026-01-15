// 用户新增的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖不同场景

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, John@Doe!", @azimuth.greet("John@Doe"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 🌟MoonBit🌟!", @azimuth.greet("🌟MoonBit🌟"))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  // 计算 (a + b) * (c - d) + a * b
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)
  let product_ab = @azimuth.multiply(a, b)
  let result = @azimuth.add(product_ab_cd, product_ab)
  
  @azimuth.assert_eq(0, result)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(1094, final_total)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let x = 12
  let y = 8
  let z = 5
  
  // 验证分配律: x * (y + z) = x * y + x * z
  let left_side = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证结合律: (x + y) + z = x + (y + z)
  let sum_left = @azimuth.add(@azimuth.add(x, y), z)
  let sum_right = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(sum_left, sum_right)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let empty_string = ""
  let single_char = "A"
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedLength"
  let whitespace_string = "   "
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_string))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single_char))
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedLength!", @azimuth.greet(long_string))
  @azimuth.assert_eq_string("Hello,    !", @azimuth.greet(whitespace_string))
}

test "multiplication_boundary_conditions" {
  // 测试乘法边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
}

test "real_world_application_scenario" {
  // 测试实际应用场景：计算贷款月供
  let loan_amount = 100000
  let annual_rate = 6  // 6%
  let loan_years = 30
  
  // 简化的月供计算：月供 = (本金 + 总利息) / (年数 * 12)
  let total_interest = @azimuth.multiply(loan_amount, @azimuth.multiply(annual_rate, loan_years)) / 100
  let total_payment = @azimuth.add(loan_amount, total_interest)
  let monthly_payment = total_payment / @azimuth.multiply(loan_years, 12)
  
  @azimuth.assert_eq(277, monthly_payment)
}