// 额外的 MoonBit 测试用例
// 覆盖更多场景和边界情况

test "arith_progression_sum" {
  // 计算等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n/2 × (2a₁ + (n-1)d)
  let n = 5
  let first_term = 1
  let common_diff = 3
  
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n - 1, common_diff))) / 2
  @azimuth.assert_eq(35, sum)
}

test "geo_progression_sum" {
  // 计算等比数列求和：2 + 6 + 18 + 54
  // 使用公式：a₁ × (rⁿ - 1) / (r - 1)
  let first_term = 2
  let ratio = 3
  let n = 4
  
  let numerator = @azimuth.multiply(first_term, @azimuth.add(@azimuth.multiply(@azimuth.multiply(ratio, ratio), ratio), -1))
  let sum = numerator / (ratio - 1)
  @azimuth.assert_eq(80, sum)
}

test "temperature_conversion_precise" {
  // 精确的温度转换：摄氏度到华氏度 F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - C/5 + 32
  let celsius = 25
  
  let fahrenheit = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius, 2), @azimuth.multiply(celsius / 5, -1)), 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.add(@azimuth.multiply(0, 2), @azimuth.multiply(0 / 5, -1)), 32)
  @azimuth.assert_eq(32, freezing)
}

test "discount_calculation" {
  // 折扣计算：原价、折扣率、最终价格
  let original_price = 1200
  let discount_percent = 15
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  // 计算最终价格
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(180, discount_amount)
  @azimuth.assert_eq(1020, final_price)
}

test "complex_string_scenarios" {
  // 复杂字符串场景测试
  let name_with_spaces = "  John Doe  "
  let name_with_quotes = "\"John\""
  let name_with_newlines = "John\nDoe"
  
  @azimuth.assert_eq_string("Hello,   John Doe  !", @azimuth.greet(name_with_spaces))
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet(name_with_quotes))
  @azimuth.assert_eq_string("Hello, John\nDoe!", @azimuth.greet(name_with_newlines))
}

test "nested_function_calls" {
  // 嵌套函数调用测试：((a + b) × c) + (d × (e - f))
  let a = 5
  let b = 3
  let c = 4
  let d = 7
  let e = 10
  let f = 2
  
  let left_part = @azimuth.multiply(@azimuth.add(a, b), c)
  let right_part = @azimuth.multiply(d, @azimuth.add(e, -f))
  let result = @azimuth.add(left_part, right_part)
  
  @azimuth.assert_eq(58, result)
}

test "cumulative_calculation" {
  // 累积计算测试：计算阶乘和累积和
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 计算 1 × 2 × 3 × 4 × 5
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4), 5)
  @azimuth.assert_eq(120, product1)
}

test "boundary_combination_extreme" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 (max_val + min_val) × (max_val + min_val)
  let sum = @azimuth.add(max_val, min_val)
  let result = @azimuth.multiply(sum, sum)
  @azimuth.assert_eq(1, result)
  
  // 测试 (max_val - 1) × (min_val + 1)
  let diff1 = @azimuth.add(max_val, -1)
  let diff2 = @azimuth.add(min_val, 1)
  let result2 = @azimuth.multiply(diff1, diff2)
  @azimuth.assert_eq(-2147483649, result2)
}

test "practical_business_calculation" {
  // 实际业务计算：订单总价计算
  let item_price = 199
  let quantity = 3
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(666, total)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：O(n²) 和 O(n log n) 的简化模拟
  let n = 10
  
  // 模拟 O(n²) 复杂度：n × n
  let on2 = @azimuth.multiply(n, n)
  @azimuth.assert_eq(100, on2)
  
  // 模拟 O(n log n) 复杂度：n × log₂(n) ≈ n × (n 的位数)
  let log_n = 4  // log₂(10) ≈ 3.32，取整为4
  let onlogn = @azimuth.multiply(n, log_n)
  @azimuth.assert_eq(40, onlogn)
}