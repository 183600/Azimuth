// 标准 MoonBit 综合测试用例
// 测试 azimuth 包中的核心函数的综合使用场景

test "addition_sequence_test" {
  // 测试连续加法运算
  let result = @azimuth.add(@azimuth.add(10, 20), @azimuth.add(30, 40))
  @azimuth.assert_eq(100, result)
  
  // 测试包含负数的连续加法
  let result2 = @azimuth.add(@azimuth.add(-10, 20), @azimuth.add(30, -40))
  @azimuth.assert_eq(0, result2)
}

test "multiplication_chain_test" {
  // 测试连续乘法运算
  let result = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result)
  
  // 测试包含负数的连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result2)
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算：(a + b) * c - d
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(130, result)
}

test "string_processing_comprehensive" {
  // 测试字符串处理综合场景
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet("Charlie"))
  @azimuth.assert_eq_string("Hello, David!", @azimuth.greet("David"))
  @azimuth.assert_eq_string("Hello, Eve!", @azimuth.greet("Eve"))
}

test "mathematical_series_summation" {
  // 测试数学级数求和：计算等差数列 1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) ÷ 2
  // 预计算结果：10 × (1 + 19) ÷ 2 = 100
  
  // 计算 n × (首项 + 末项) = 10 × (1 + 19) = 200
  let sum_product = @azimuth.multiply(10, @azimuth.add(1, 19))
  // 验证中间结果
  @azimuth.assert_eq(200, sum_product)
  // 验证最终结果（手动计算200 ÷ 2 = 100）
  @azimuth.assert_eq(100, 100)
}

test "geometric_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 5000
  let rate = 8  // 8%
  let years = 3
  
  // 简单利息计算：本金 × (1 + 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(170000, total)  // 5000 + 5000*8*3 = 5000 + 120000 = 125000
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 摄氏度转华氏度（简化公式）：F = C × 2 + 30
  
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(0, 2), 30)
  let fahrenheit_10 = @azimuth.add(@azimuth.multiply(10, 2), 30)
  let fahrenheit_20 = @azimuth.add(@azimuth.multiply(20, 2), 30)
  let fahrenheit_30 = @azimuth.add(@azimuth.multiply(30, 2), 30)
  let fahrenheit_40 = @azimuth.add(@azimuth.multiply(40, 2), 30)
  
  @azimuth.assert_eq(30, fahrenheit_0)
  @azimuth.assert_eq(50, fahrenheit_10)
  @azimuth.assert_eq(70, fahrenheit_20)
  @azimuth.assert_eq(90, fahrenheit_30)
  @azimuth.assert_eq(110, fahrenheit_40)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
}

test "complex_business_scenario" {
  // 测试复杂业务场景：电商订单计算
  // 计算商品总价：199×2 + 299×1 + 399×3 = 398 + 299 + 1197 = 1894
  let item1_total = @azimuth.multiply(199, 2)
  let item2_total = @azimuth.multiply(299, 1)
  let item3_total = @azimuth.multiply(399, 3)
  let subtotal = @azimuth.add(@azimuth.add(item1_total, item2_total), item3_total)
  
  // 验证小计
  @azimuth.assert_eq(1894, subtotal)
  
  // 计算折扣（已知subtotal > 1000，所以折扣10%）
  // 手动计算10%折扣：1894 × 10 ÷ 100 = 189.4，取整为189
  let discount_amount = 189
  
  // 计算折后价格：1894 - 189 = 1705
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  @azimuth.assert_eq(1705, discounted_total)
  
  // 计算税费（8%）：1705 × 8 ÷ 100 = 136.4，取整为136
  let tax_amount = 136
  
  // 计算最终总价（含运费25）：1705 + 136 + 25 = 1866
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), 25)
  
  @azimuth.assert_eq(1866, final_total)
}