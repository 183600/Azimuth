// Azimuth 项目高级测试用例
// 使用标准 MoonBit 测试语法，涵盖边界条件、复杂数学运算和实际应用场景

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
}

test "add_fractional_simulation" {
  // 模拟小数运算（通过整数运算）
  // 1.5 + 2.3 = 3.8 (模拟为 15 + 23 = 38，然后除以10)
  let num1 = 15
  let num2 = 23
  let result = @azimuth.add(num1, num2)
  @azimuth.assert_eq(38, result)
  
  // 2.25 + 1.75 = 4.0 (模拟为 225 + 175 = 400，然后除以100)
  let num3 = 225
  let num4 = 175
  let result2 = @azimuth.add(num3, num4)
  @azimuth.assert_eq(400, result2)
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(10000000, @azimuth.multiply(10000, 1000))
  @azimuth.assert_eq(123450, @azimuth.multiply(12345, 10))
  @azimuth.assert_eq(998001, @azimuth.multiply(999, 999))
}

test "multiply_power_of_two" {
  // 测试2的幂次乘法
  @azimuth.assert_eq(2, @azimuth.multiply(1, 2))
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(128, @azimuth.multiply(8, 16))
  @azimuth.assert_eq(2048, @azimuth.multiply(32, 64))
}

test "greet_international_names" {
  // 测试国际化名称问候
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, François!", @azimuth.greet("François"))
  @azimuth.assert_eq_string("Hello, Ælfred!", @azimuth.greet("Ælfred"))
}

test "greet_edge_cases" {
  // 测试边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  // 等差数列: 3, 7, 11, 15, 19 (公差为4)
  let term1 = 3
  let diff = 4
  let term2 = @azimuth.add(term1, diff)  // 7
  let term3 = @azimuth.add(term2, diff)  // 11
  let term4 = @azimuth.add(term3, diff)  // 15
  let term5 = @azimuth.add(term4, diff)  // 19
  
  // 验证数列和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(55, sum)
}

test "area_calculations" {
  // 测试面积计算
  // 正方形面积: 边长²
  let square_side = 6
  let square_area = @azimuth.multiply(square_side, square_side)
  @azimuth.assert_eq(36, square_area)
  
  // 长方形面积: 长 × 宽
  let rect_length = 8
  let rect_width = 5
  let rect_area = @azimuth.multiply(rect_length, rect_width)
  @azimuth.assert_eq(40, rect_area)
  
  // 梯形面积: (上底 + 下底) × 高 ÷ 2
  let top_base = 4
  let bottom_base = 8
  let height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(30, trapezoid_area)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  // 秒到分钟: 3600秒 = 60分钟
  let seconds = 3600
  let minutes = seconds / 60
  @azimuth.assert_eq(60, minutes)
  
  // 分钟到小时: 120分钟 = 2小时
  let total_minutes = 120
  let hours = total_minutes / 60
  @azimuth.assert_eq(2, hours)
  
  // 天到小时: 3天 = 72小时
  let days = 3
  let total_hours = @azimuth.multiply(days, 24)
  @azimuth.assert_eq(72, total_hours)
  
  // 一年的总分钟数（假设365天）
  let days_in_year = 365
  let minutes_in_year = @azimuth.multiply(@azimuth.multiply(days_in_year, 24), 60)
  @azimuth.assert_eq(525600, minutes_in_year)
}

test "compound_interest_simulation" {
  // 测试复利计算模拟
  // 本金 = 1000，年利率 = 5%，期限 = 3年
  let principal = 1000
  let rate_percent = 5
  let _years = 3  // 使用下划线前缀表示未使用的变量
  
  // 第一年结束: 1000 + (1000 * 5%) = 1050
  let year1_interest = @azimuth.multiply(principal, rate_percent) / 100
  let year1_total = @azimuth.add(principal, year1_interest)
  @azimuth.assert_eq(1050, year1_total)
  
  // 第二年结束: 1050 + (1050 * 5%) = 1102.5 (模拟为1102)
  let year2_interest = @azimuth.multiply(year1_total, rate_percent) / 100
  let year2_total = @azimuth.add(year1_total, year2_interest)
  @azimuth.assert_eq(1102, year2_total)  // 整数模拟
  
  // 第三年结束: 1102 + (1102 * 5%) = 1157.1 (模拟为1157)
  let year3_interest = @azimuth.multiply(year2_total, rate_percent) / 100
  let year3_total = @azimuth.add(year2_total, year3_interest)
  @azimuth.assert_eq(1157, year3_total)  // 整数模拟
}