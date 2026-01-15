// 新增的 MoonBit 标准测试用例
// 测试 azimuth 库的核心功能和边界情况

// 测试大数运算
test "large_number_operations" {
  // 测试大数相加
  let large_sum = @azimuth.add(1000000, 2000000)
  @azimuth.assert_eq(3000000, large_sum)
  
  // 测试大数相乘
  let large_product = @azimuth.multiply(10000, 10000)
  @azimuth.assert_eq(100000000, large_product)
  
  // 测试大负数运算
  let large_neg_sum = @azimuth.add(-1000000, -2000000)
  @azimuth.assert_eq(-3000000, large_neg_sum)
}

// 测试连续运算
test "sequential_operations" {
  // 测试连续加法：(a + b) + c
  let a = 10
  let b = 20
  let c = 30
  
  let step1 = @azimuth.add(a, b)  // 30
  let result = @azimuth.add(step1, c)  // 60
  @azimuth.assert_eq(60, result)
  
  // 测试连续乘法：(a * b) * c
  let x = 2
  let y = 3
  let z = 4
  
  let step2 = @azimuth.multiply(x, y)  // 6
  let result2 = @azimuth.multiply(step2, z)  // 24
  @azimuth.assert_eq(24, result2)
}

// 测试混合运算
test "mixed_operations" {
  // 测试 (a * b) + (c * d)
  let a = 5
  let b = 6
  let c = 7
  let d = 8
  
  let product1 = @azimuth.multiply(a, b)  // 30
  let product2 = @azimuth.multiply(c, d)  // 56
  let result = @azimuth.add(product1, product2)  // 86
  @azimuth.assert_eq(86, result)
  
  // 测试 (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)  // 11
  let sum2 = @azimuth.add(c, d)  // 15
  let result2 = @azimuth.multiply(sum1, sum2)  // 165
  @azimuth.assert_eq(165, result2)
}

// 测试极值边界情况
test "extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试边界值与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

// 测试字符串特殊情况
test "special_string_cases" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊符号的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试包含Unicode表情的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
}

// 测试数学恒等式
test "mathematical_identities" {
  let x = 15
  let y = 25
  let z = 10
  
  // 测试加法恒等式：a + 0 = a
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 测试乘法恒等式：a * 1 = a
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 测试乘法零律：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, y))
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

// 测试实际应用场景
test "practical_application_scenarios" {
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let total = @azimuth.add(subtotal1, subtotal2)  // 697
  @azimuth.assert_eq(697, total)
  
  // 场景2：计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)  // 96
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))  // 40
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
  
  // 场景3：温度转换（简化版）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

// 测试错误处理和边界保护
test "error_handling_and_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val)的平方
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 最小值乘-1的特殊情况
}

// 测试性能相关的大规模运算
test "performance_large_scale_operations" {
  // 测试大量重复运算的累加
  let base = 1000
  let count = 100
  
  // 使用公式计算：base * count * (count + 1) / 2
  let sum_formula = @azimuth.multiply(base, @azimuth.multiply(count, @azimuth.add(count, 1)) / 2)
  @azimuth.assert_eq(5050000, sum_formula)
  
  // 测试指数增长计算
  let initial = 2
  let power_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(initial, 2), 2), 2)  // 2 * 2^3 = 16
  @azimuth.assert_eq(16, power_result)
  
  // 测试大数阶乘的简化计算
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
}

// 测试复杂业务逻辑
test "complex_business_logic" {
  // 场景1：银行利息计算
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算：本金 + 本金 * 利率 * 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 场景2：折扣计算
  let original_price = 500
  let discount1 = 10  // 第一阶段折扣
  let discount2 = 5   // 第二阶段折扣
  
  let price_after_first = @azimuth.add(original_price, -@azimuth.multiply(original_price, discount1) / 100)  // 450
  let final_price = @azimuth.add(price_after_first, -@azimuth.multiply(price_after_first, discount2) / 100)  // 427.5 -> 427 (整数除法)
  @azimuth.assert_eq(427, final_price)
  
  // 场景3：分数计算（简化为整数）
  let numerator = 22
  let denominator = 7
  let whole_part = numerator / denominator  // 3
  let remainder = numerator % denominator  // 1
  let decimal_approx = @azimuth.multiply(remainder, 10) / denominator  // 1
  
  @azimuth.assert_eq(3, whole_part)
  @azimuth.assert_eq(1, remainder)
  @azimuth.assert_eq(1, decimal_approx)
}