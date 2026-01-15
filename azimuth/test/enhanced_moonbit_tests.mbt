// Enhanced MoonBit 测试用例
// 这些测试用例展示了 MoonBit 标准测试语法的各种用法

// 测试加法函数的数学特性
test "add_mathematical_properties" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = -6789
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let c = 9876
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法函数的数学特性
test "multiply_mathematical_properties" {
  // 测试乘法交换律: a * b = b * a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let c = 78
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
}

// 测试字符串处理功能
test "string_processing_features" {
  // 测试包含各种字符的字符串
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  
  // 测试包含空格和特殊字符的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

// 测试边界值和极值情况
test "extreme_values_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法的边界情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法的边界情况
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试实际应用场景：计算三角形面积
test "triangle_area_calculation" {
  // 使用海伦公式计算三角形面积的简化版本
  // 假设三边长为 a, b, c，半周长 p = (a + b + c) / 2
  // 面积 = p * (p - a) * (p - b) * (p - c) 的平方根
  // 这里我们只计算到半周长
  
  let side_a = 3
  let side_b = 4
  let side_c = 5
  
  // 计算半周长
  let perimeter = @azimuth.add(@azimuth.add(side_a, side_b), side_c)
  let semi_perimeter = perimeter / 2
  
  @azimuth.assert_eq(6, semi_perimeter)
}

// 测试实际应用场景：计算平均分
test "average_score_calculation" {
  // 计算5个学生的平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 88
  
  // 计算总分
  let total_score = @azimuth.add(
    @azimuth.add(@azimuth.add(score1, score2), @azimuth.add(score3, score4)),
    score5
  )
  
  // 计算平均分（整数除法）
  let average_score = total_score / 5
  
  @azimuth.assert_eq(87, average_score)  // (85 + 92 + 78 + 96 + 88) / 5 = 439 / 5 = 87
}

// 测试实际应用场景：计算折扣价格
test "discount_price_calculation" {
  // 计算商品折扣后的价格
  let original_price = 299
  let discount_percentage = 20  // 20% 折扣
  
  // 计算折扣金额（使用整数运算）
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(239, final_price)  // 299 - (299 * 20 / 100) = 299 - 59 = 240
}

// 测试实际应用场景：计算温度转换
test "temperature_conversion_calculation" {
  // 摄氏度转华氏度的简化版本：F = C * 2 + 32
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 32)
  @azimuth.assert_eq(82, fahrenheit_temp)
  
  // 华氏度转摄氏度的简化版本：C = (F - 32) / 2
  let fahrenheit_temp2 = 68
  let celsius_temp2 = (@azimuth.add(fahrenheit_temp2, -32)) / 2
  @azimuth.assert_eq(18, celsius_temp2)
}

// 测试实际应用场景：计算简单利息
test "simple_interest_calculation" {
  // 计算简单利息：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5% 年利率
  let time = 3  // 3年
  
  // 计算总利息（使用整数运算）
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  
  // 计算最终金额
  let final_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(11500, final_amount)  // 10000 + (10000 * 5 * 3 / 100) = 10000 + 1500 = 11500
}

// 测试布尔条件表达式
test "boolean_condition_expressions" {
  // 测试各种布尔条件
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(10 >= 10)
  @azimuth.assert_true(-5 < 0)
  @azimuth.assert_true(-10 <= -10)
  
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_false(10 > 10)
  @azimuth.assert_false(0 < -5)
  @azimuth.assert_false(-10 < -10)
  
  // 测试复合条件
  @azimuth.assert_true(5 > 3 && 3 > 1)
  @azimuth.assert_true(5 > 3 || 3 > 5)
  @azimuth.assert_false(5 > 3 && 3 > 5)
  @azimuth.assert_true(!(5 > 3 && 3 > 5))
}