// 增强测试场景 - 覆盖更多实际应用场景

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "complex_business_calculation" {
  // 复杂业务计算：批量订单折扣计算
  let unit_price = 120
  let quantity = 15
  let bulk_discount_threshold = 10
  let discount_rate = 15
  
  // 计算原价
  let original_total = @azimuth.multiply(unit_price, quantity)
  
  // 如果数量超过阈值，应用折扣
  let discount_amount = if quantity > bulk_discount_threshold {
    @azimuth.multiply(original_total, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = @azimuth.add(original_total, -discount_amount)
  
  @azimuth.assert_eq(1530, final_total)
}

test "temperature_range_analysis" {
  // 温度范围分析：计算一周的平均最高温度
  let daily_highs = [25, 28, 24, 30, 27, 26, 29]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(daily_highs[0], daily_highs[1]), daily_highs[2]), daily_highs[3]), daily_highs[4])
  let sum_rest = @azimuth.add(@azimuth.add(daily_highs[5], daily_highs[6]), 0)
  let total = @azimuth.add(sum, sum_rest)
  let average = total / 7
  
  @azimuth.assert_eq(27, average)
}

test "string_processing_with_special_cases" {
  // 字符串处理特殊情况测试
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "mathematical_series_progression" {
  // 数学级数计算：等比数列前5项和
  let first_term = 2
  let ratio = 3
  let n_terms = 5
  
  // 计算：2 + 6 + 18 + 54 + 162 = 242
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  @azimuth.assert_eq(242, sum)
}

test "error_boundary_validation" {
  // 错误边界验证：确保溢出保护正常工作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  
  // 测试乘法溢出保护
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "combinatorial_logic_test" {
  // 组合逻辑测试：验证布尔运算的数学等价性
  let a = 5
  let b = 10
  let c = 3
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left, right)
  
  // 验证结合律：(a + b) + c = a + (b + c)
  let assoc_left = @azimuth.add(@azimuth.add(a, b), c)
  let assoc_right = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(assoc_left, assoc_right)
}

test "practical_measurement_conversion" {
  // 实用测量转换：英寸转厘米（简化版：1英寸 ≈ 2.5厘米）
  let inches = 12
  let conversion_factor = 25  // 使用整数表示2.5厘米
  let centimeters = @azimuth.multiply(inches, conversion_factor) / 10
  
  @azimuth.assert_eq(30, centimeters)
  
  // 测试反向转换：厘米转英寸
  let cm_value = 50
  let back_to_inches = @azimuth.multiply(cm_value, 10) / conversion_factor
  
  @azimuth.assert_eq(20, back_to_inches)
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟：比较线性增长和指数增长的差异
  let base_value = 100
  let iterations = 5
  
  // 线性增长：base + base*2 + ... + base*n
  let linear_sum = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  // 指数增长：base * 2^n
  let exponential_result = @azimuth.multiply(base_value, @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  
  @azimuth.assert_eq(1500, linear_sum)
  @azimuth.assert_eq(800, exponential_result)
}