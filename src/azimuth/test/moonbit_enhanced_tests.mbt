// 增强的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "enhanced_add_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = -67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试与边界值的交换律
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result3 = @azimuth.add(max_val, min_val)
  let result4 = @azimuth.add(min_val, max_val)
  @azimuth.assert_eq(result3, result4)
}

test "enhanced_multiply_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 25
  let c = 35
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的分配律
  let d = -10
  let e = 20
  let f = -30
  
  let left_side2 = @azimuth.multiply(d, @azimuth.add(e, f))
  let right_side2 = @azimuth.add(@azimuth.multiply(d, e), @azimuth.multiply(d, f))
  @azimuth.assert_eq(left_side2, right_side2)
}

test "enhanced_greet_unicode_handling" {
  // 测试 greet 函数的 Unicode 处理能力
  let chinese_name = "张三李四"
  let result1 = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三李四!", result1)
  
  let emoji_name = "🚀🌟💫"
  let result2 = @azimuth.greet(emoji_name)
  @azimuth.assert_eq_string("Hello, 🚀🌟💫!", result2)
  
  let mixed_unicode = "Hello世界🌍"
  let result3 = @azimuth.greet(mixed_unicode)
  @azimuth.assert_eq_string("Hello, Hello世界🌍!", result3)
}

test "enhanced_financial_compound_interest" {
  // 测试复利计算的实际应用场景
  let principal = 10000  // 本金
  let rate = 5           // 年利率（百分比）
  let years = 3          // 年数
  
  // 简化的复利计算：本息和 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, total_amount)
  
  // 测试不同利率和期限
  let principal2 = 5000
  let rate2 = 8
  let years2 = 2
  
  let growth_factor2 = @azimuth.add(1, @azimuth.multiply(rate2, years2))
  let total_amount2 = @azimuth.multiply(principal2, growth_factor2)
  
  @azimuth.assert_eq(85000, total_amount2)
}

test "enhanced_temperature_conversion" {
  // 测试温度转换场景（摄氏度转华氏度）
  // 简化公式：F = C × 2 + 30
  let celsius_temperatures = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  let f1 = @azimuth.add(@azimuth.multiply(celsius_temperatures[0], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  
  let f2 = @azimuth.add(@azimuth.multiply(celsius_temperatures[1], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[1], f2)
  
  let f3 = @azimuth.add(@azimuth.multiply(celsius_temperatures[2], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f3)
  
  let f4 = @azimuth.add(@azimuth.multiply(celsius_temperatures[3], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[3], f4)
  
  let f5 = @azimuth.add(@azimuth.multiply(celsius_temperatures[4], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], f5)
  
  let f6 = @azimuth.add(@azimuth.multiply(celsius_temperatures[5], 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[5], f6)
}

test "enhanced_shopping_cart_total" {
  // 测试购物车总价计算
  let item_prices = [199, 299, 399, 499]
  let quantities = [2, 1, 3, 1]
  
  // 计算每种商品的小计
  let subtotal1 = @azimuth.multiply(item_prices[0], quantities[0])
  let subtotal2 = @azimuth.multiply(item_prices[1], quantities[1])
  let subtotal3 = @azimuth.multiply(item_prices[2], quantities[2])
  let subtotal4 = @azimuth.multiply(item_prices[3], quantities[3])
  
  // 计算总价
  let cart_total = @azimuth.add(@azimuth.add(@azimuth.add(subtotal1, subtotal2), subtotal3), subtotal4)
  
  // 验证：199×2 + 299×1 + 399×3 + 499×1 = 398 + 299 + 1197 + 499 = 2393
  @azimuth.assert_eq(2393, cart_total)
  
  // 测试折扣计算：满2000减200
  let discount_threshold = 2000
  let discount_amount = 200
  let final_total = if (cart_total > discount_threshold) {
    @azimuth.add(cart_total, -discount_amount)
  } else {
    cart_total
  }
  
  @azimuth.assert_eq(2193, final_total)
}

test "enhanced_geometric_calculations" {
  // 测试几何计算场景
  let radius = 7
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
  
  // 球体体积（简化）：4/3 × π × r³ ≈ 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(1372, volume)
  
  // 测试矩形和三角形面积
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "enhanced_sequence_calculations" {
  // 测试数列计算
  // 计算等差数列前n项和：1 + 2 + 3 + ... + n
  let n = 10
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 计算等比数列前n项和：1 + 2 + 4 + 8 + ... + 2^(n-1)
  // 简化计算：2^n - 1
  let geometric_sum = @azimuth.add(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), -1)
  @azimuth.assert_eq(15, geometric_sum)
  
  // 计算平方和：1² + 2² + 3² + ... + n²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, sum_of_squares)
}

test "enhanced_boundary_stress_test" {
  // 测试边界值的压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续边界运算
  let chain1 = @azimuth.add(@azimuth.add(max_val, min_val), max_val)
  @azimuth.assert_eq(max_val, chain1)
  
  let chain2 = @azimuth.multiply(@azimuth.multiply(min_val, 1), -1)
  @azimuth.assert_eq(min_val, chain2)
  
  // 测试边界值与普通值的组合
  let normal1 = 1000000
  let normal2 = -1000000
  
  let result1 = @azimuth.add(max_val, normal1)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.add(min_val, normal2)
  @azimuth.assert_eq(min_val, result2)
  
  let result3 = @azimuth.multiply(normal1, normal2)
  @azimuth.assert_eq(-1000000, result3)
  
  // 测试边界值的特殊运算
  let special1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special1)
  
  let special2 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, special2)
}

test "enhanced_error_resilience" {
  // 测试错误恢复和容错能力
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护的正确性
  let overflow_add1 = @azimuth.add(max_val, max_val)
  let overflow_add2 = @azimuth.add(max_val, 1000000)
  let overflow_add3 = @azimuth.add(min_val, min_val)
  let overflow_add4 = @azimuth.add(min_val, -1000000)
  
  @azimuth.assert_eq(max_val, overflow_add1)
  @azimuth.assert_eq(max_val, overflow_add2)
  @azimuth.assert_eq(min_val, overflow_add3)
  @azimuth.assert_eq(min_val, overflow_add4)
  
  // 测试乘法溢出保护
  let overflow_mul1 = @azimuth.multiply(max_val, 2)
  let overflow_mul2 = @azimuth.multiply(max_val, max_val)
  let overflow_mul3 = @azimuth.multiply(min_val, 2)
  let overflow_mul4 = @azimuth.multiply(min_val, min_val)
  
  @azimuth.assert_eq(max_val, overflow_mul1)
  @azimuth.assert_eq(max_val, overflow_mul2)
  @azimuth.assert_eq(min_val, overflow_mul3)
  @azimuth.assert_eq(max_val, overflow_mul4)
  
  // 验证正常运算仍然正确
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}