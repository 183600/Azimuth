// 用户聚焦测试用例
// 专门针对 azimuth 库核心功能的测试，使用标准 MoonBit 测试语法

test "addition_overflow_protection" {
  // 测试正数溢出保护
  let max_val = 2147483647
  let overflow_result = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_result)
  
  // 测试负数溢出保护
  let min_val = -2147483648
  let underflow_result = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, underflow_result)
}

test "multiplication_overflow_edge_cases" {
  // 测试大数相乘的溢出处理
  let large_num1 = 46341
  let large_num2 = 46341
  let overflow_result = @azimuth.multiply(large_num1, large_num2)
  @azimuth.assert_eq(2147483647, overflow_result)
  
  // 测试最小值与负数相乘
  let min_val = -2147483648
  let neg_result = @azimuth.multiply(min_val, -2)
  @azimuth.assert_eq(min_val, neg_result)
}

test "greet_function_unicode_handling" {
  // 测试 Unicode 字符串处理
  let unicode_name = "张三李四"
  let unicode_result = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 张三李四!", unicode_result)
  
  // 测试 emoji 表情符号
  let emoji_name = "🚀🌟"
  let emoji_result = @azimuth.greet(emoji_name)
  @azimuth.assert_eq_string("Hello, 🚀🌟!", emoji_result)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  let fee = 50
  
  // 计算复利：本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let subtotal = @azimuth.add(principal, interest)
  let total = @azimuth.add(subtotal, fee)
  
  @azimuth.assert_eq(160050, total)
}

test "geometric_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 计算圆周长（简化）：2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 计算圆面积（简化）：3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化）：F = C × 2 + 30
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
  
  // 测试室温
  let room_c = 22
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(74, room_f)
}

test "statistical_analysis" {
  // 测试统计分析功能
  // 计算一组数的平均值
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92)
  let count = 4
  let average = numbers_sum / count
  @azimuth.assert_eq(86, average)
  
  // 计算标准差（简化版）
  let variance_numerator = @azimuth.add(
    @azimuth.add(@azimuth.multiply(85-86, 85-86), @azimuth.multiply(90-86, 90-86)),
    @azimuth.add(@azimuth.multiply(78-86, 78-86), @azimuth.multiply(92-86, 92-86))
  )
  let variance = variance_numerator / count
  @azimuth.assert_eq(26, variance)
}

test "time_based_calculations" {
  // 测试基于时间的计算
  // 计算工作时长（分钟）
  let start_hour = 9
  let start_min = 30
  let end_hour = 17
  let end_min = 45
  
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_min)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_min)
  let work_duration = @azimuth.add(end_total, -start_total)
  
  @azimuth.assert_eq(495, work_duration)
  
  // 转换为小时
  let work_hours = work_duration / 60
  let remaining_mins = work_duration % 60
  @azimuth.assert_eq(8, work_hours)
  @azimuth.assert_eq(15, remaining_mins)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试各种特殊字符组合
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_result = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_result)
  
  // 测试数字字符串
  let numeric_string = "1234567890"
  let numeric_result = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 1234567890!", numeric_result)
}

test "comprehensive_business_scenario" {
  // 测试综合业务场景：电商订单计算
  let product_price = 299
  let quantity = 2
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_price, tax_amount), shipping)
  
  @azimuth.assert_eq(610, final_total)
}