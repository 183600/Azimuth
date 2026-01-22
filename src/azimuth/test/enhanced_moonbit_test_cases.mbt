// 新增的 MoonBit 测试用例
// 覆盖各种功能场景和边界情况

// use line removed

test "arithmetic_sequence_calculation" {
  // 测试等差数列计算：计算前n项和
  let n = 10
  let first_term = 3
  let common_diff = 2
  
  // 使用公式：S_n = n/2 * (2a_1 + (n-1)d)
  let sum_formula = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n - 1, common_diff))) / 2
  @azimuth.assert_eq(80, sum_formula)
  
  // 验证手动计算：3 + 5 + 7 + 9 + 11 + 13 + 15 + 17 + 19 + 21 = 80
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(3, 5), 7), 9), 11), 13), 15), 17), 19), 21)
  @azimuth.assert_eq(80, manual_sum)
}

test "financial_compound_interest" {
  // 测试复利计算：A = P(1 + r/n)^(nt)
  // 简化为整数计算：A = P + P*r*t（单利近似）
  let principal = 10000
  let rate_percent = 5
  let time_years = 3
  
  // 计算利息：本金 × 利率 × 时间
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate_percent), time_years) / 100
  // 计算总额：本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(11500, total_amount)
}

test "geometry_perimeter_calculation" {
  // 测试几何图形周长计算
  // 矩形周长：2 × (长 + 宽)
  let length = 15
  let width = 8
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, rectangle_perimeter)
  
  // 三角形周长：三边之和
  let side_a = 7
  let side_b = 9
  let side_c = 12
  let triangle_perimeter = @azimuth.add(@azimuth.add(side_a, side_b), side_c)
  @azimuth.assert_eq(28, triangle_perimeter)
}

test "string_processing_with_special_chars" {
  // 测试特殊字符字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "  Multiple  Spaces  "
  @azimuth.assert_eq_string("Hello,   Multiple  Spaces  !", @azimuth.greet(spaced_string))
  
  // 测试Unicode字符
  let unicode_string = "Unicode: αβγδε"
  @azimuth.assert_eq_string("Hello, Unicode: αβγδε!", @azimuth.greet(unicode_string))
}

test "extreme_value_combinations" {
  // 测试极值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值与中等值的运算
  let mid_val = 1000000
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, mid_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -mid_val))
  
  // 极值乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 验证年龄范围
  let age = 25
  let min_age = 18
  let max_age = 65
  
  let is_valid_age = @azimuth.add(age, -min_age) >= 0 && @azimuth.add(max_age, -age) >= 0
  @azimuth.assert_eq(is_valid_age, true)
  
  // 验证分数范围
  let score = 85
  let min_score = 0
  let max_score = 100
  
  let is_valid_score = @azimuth.add(score, -min_score) >= 0 && @azimuth.add(max_score, -score) >= 0
  @azimuth.assert_eq(is_valid_score, true)
}

test "inventory_management_calculation" {
  // 测试库存管理计算
  let initial_stock = 500
  let units_sold = 150
  let units_received = 200
  let defective_units = 25
  
  // 计算当前库存：初始库存 - 售出 + 收到 - 次品
  let current_stock = @azimuth.add(@azimuth.add(@azimuth.add(initial_stock, -units_sold), units_received), -defective_units)
  @azimuth.assert_eq(525, current_stock)
  
  // 计算库存价值（单价10元）
  let unit_price = 10
  let stock_value = @azimuth.multiply(current_stock, unit_price)
  @azimuth.assert_eq(5250, stock_value)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  // 天数转换为小时、分钟、秒
  let days = 3
  let hours_in_day = 24
  let minutes_in_hour = 60
  let seconds_in_minute = 60
  
  let total_hours = @azimuth.multiply(days, hours_in_day)
  let total_minutes = @azimuth.multiply(total_hours, minutes_in_hour)
  let total_seconds = @azimuth.multiply(total_minutes, seconds_in_minute)
  
  @azimuth.assert_eq(72, total_hours)
  @azimuth.assert_eq(4320, total_minutes)
  @azimuth.assert_eq(259200, total_seconds)
}

test "statistical_variance_calculation" {
  // 测试方差计算（简化版）
  // 数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)
  
  // 计算离差平方和（简化）
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), @azimuth.multiply(diff5, diff5))
  @azimuth.assert_eq(1000, variance_sum)
}

test "conditional_discount_calculation" {
  // 测试条件折扣计算
  let order_amount = 1200
  let discount_threshold = 1000
  let discount_rate = 10
  let vip_discount_rate = 15
  let is_vip = true
  
  // 确定折扣率
  let applicable_rate = if (order_amount > discount_threshold) {
    if (is_vip) { vip_discount_rate } else { discount_rate }
  } else {
    0
  }
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(order_amount, applicable_rate) / 100
  
  // 计算最终金额
  let final_amount = @azimuth.add(order_amount, -discount_amount)
  
  @azimuth.assert_eq(1020, final_amount)
}