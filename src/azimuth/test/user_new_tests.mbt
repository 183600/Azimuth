// 用户新增的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 负数乘法溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 特殊情况：最小值乘以-1
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("🌍"), "Hello, 🌍!")
  @azimuth.assert_eq_string(@azimuth.greet("café"), "Hello, café!")
  @azimuth.assert_eq_string(@azimuth.greet("naïve"), "Hello, naïve!")
  @azimuth.assert_eq_string(@azimuth.greet("🚀🌕"), "Hello, 🚀🌕!")
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 正数测试
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 1), 1)
  
  // 负数测试
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-10, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-9, 3), -3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-1, 1), -1)
  
  // 零测试
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  // 贷款计算：本金 × (1 + 月利率) ^ 月数
  let principal = 100000
  let monthly_rate = 1  // 1%
  let months = 12
  
  // 简化的复利计算
  let monthly_factor = @azimuth.add(100, monthly_rate)
  let total_factor = monthly_factor  // 简化为单利
  let total_amount = @azimuth.multiply(principal, total_factor)
  
  @azimuth.assert_eq(10100000, total_amount)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 10
  let bottom_base = 20
  let height = 5
  
  let sum_bases = @azimuth.add(top_base, bottom_base)
  let product = @azimuth.multiply(sum_bases, height)
  let trapezoid_area = product / 2
  
  @azimuth.assert_eq(75, trapezoid_area)
  
  // 计算立方体体积：边长³
  let side_length = 5
  let cube_volume = @azimuth.multiply(@azimuth.multiply(side_length, side_length), side_length)
  @azimuth.assert_eq(125, cube_volume)
}

test "statistical_analysis" {
  // 测试统计分析
  // 计算方差：平方和的平均值减去平均值的平方
  let values = [10, 20, 30, 40, 50]
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let mean = sum / 5
  
  // 计算平方和
  let square1 = @azimuth.multiply(10, 10)
  let square2 = @azimuth.multiply(20, 20)
  let square3 = @azimuth.multiply(30, 30)
  let square4 = @azimuth.multiply(40, 40)
  let square5 = @azimuth.multiply(50, 50)
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  
  // 计算方差（简化版）
  let variance = square_sum / 5
  @azimuth.assert_eq(1100, variance)
}

test "time_based_calculations" {
  // 测试基于时间的计算
  // 计算两个时间点之间的分钟差
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  // 转换为总分钟数
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  
  // 计算时间差
  let time_diff = @azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(315, time_diff)  // 5小时15分钟 = 315分钟
}

test "string_processing_scenarios" {
  // 测试字符串处理场景
  // 测试各种特殊字符串
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet(" "), "Hello,  !")
  @azimuth.assert_eq_string(@azimuth.greet("a"), "Hello, a!")
  @azimuth.assert_eq_string(@azimuth.greet("A B C"), "Hello, A B C!")
  @azimuth.assert_eq_string(@azimuth.greet("1234567890"), "Hello, 1234567890!")
  @azimuth.assert_eq_string(@azimuth.greet("!@#$%^&*()"), "Hello, !@#$%^&*()!")
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(@azimuth.add(1, 1) == 2)
  @azimuth.assert_false(@azimuth.add(1, 1) == 3)
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  
  // 测试边界值运算
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
  @azimuth.assert_eq(@azimuth.add(max_val, 0), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, 0), min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
}