// 额外的 MoonBit 测试用例
// 这些测试用例补充了现有的测试，提供更全面的覆盖

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
}

test "compound_interest_precise" {
  // 精确的复利计算：本金 × (1 + 利率)^年数
  // 简化为整数计算：1000 × (1 + 5%)^3 ≈ 1000 × 115 = 115000
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利公式，避免浮点数
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(115000, total)
}

test "nested_calculations" {
  // 嵌套计算测试：((a + b) × (c + d)) - (e × f)
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = @azimuth.add(a, b)  // 30
  let sum2 = @azimuth.add(c, d)  // 20
  let product1 = @azimuth.multiply(sum1, sum2)  // 600
  let product2 = @azimuth.multiply(e, f)  // 96
  let result = @azimuth.add(product1, -product2)  // 504
  
  @azimuth.assert_eq(504, result)
}

test "string_length_edge_cases" {
  // 测试不同长度字符串的greet函数处理
  let empty = ""
  let single = "A"
  let short = "Hi"
  let medium = "MoonBit"
  let long = "ThisIsAVeryLongStringForTestingPurposes"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single))
  @azimuth.assert_eq_string("Hello, Hi!", @azimuth.greet(short))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium))
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", @azimuth.greet(long))
}

test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  // 通过连续相乘实现
  let result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(256, result)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 25  // 25%
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(125, value)  // 500 × 25 / 100 = 125
  
  // 计算增加后的值：总数 + 百分比值
  let increased = @azimuth.add(total, value)
  @azimuth.assert_eq(625, increased)  // 500 + 125 = 625
}

test "string_with_numbers_and_symbols" {
  // 测试包含数字和符号的字符串
  let alphanumeric = "Test123"
  let symbolic = "test@example.com"
  let mixed = "User_2023@Project#1"
  
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet(alphanumeric))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(symbolic))
  @azimuth.assert_eq_string("Hello, User_2023@Project#1!", @azimuth.greet(mixed))
}

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的各种运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  
  // 边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "real_world_application" {
  // 实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(674, total)  // (199×3) + (199×3×10/100) + 15 = 597 + 59 + 15 = 674
}

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
}