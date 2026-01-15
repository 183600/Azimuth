// 增强的 MoonBit 测试用例
// 测试 Azimuth 项目的各种功能和边界情况

test "advanced_arithmetic_operations" {
  // 测试高级算术运算
  let a = 15
  let b = 7
  let c = 3
  
  // 计算 (a + b) * c - a
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -a)
  @azimuth.assert_eq(51, result)
  
  // 计算 a² + b²
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let sum_squares = @azimuth.add(a_squared, b_squared)
  @azimuth.assert_eq(274, sum_squares)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-={}|[]:\";'<>?,./"
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", @azimuth.greet(special_chars))
  
  // 测试包含数字的字符串
  let numeric_string = "1234567890"
  @azimuth.assert_eq_string("Hello, " + numeric_string + "!", @azimuth.greet(numeric_string))
  
  // 测试混合内容字符串
  let mixed_content = "User123_Test!@#"
  @azimuth.assert_eq_string("Hello, " + mixed_content + "!", @azimuth.greet(mixed_content))
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与不同数值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试最小值与不同数值的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试最大值和最小值的组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 5000
  let rate_percent = 8
  let years = 4
  
  // 计算简单利息
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(165000, total)
  
  // 计算复利（简化版）
  let yearly_growth = @azimuth.add(100, rate_percent)
  let compounded = @azimuth.multiply(principal, yearly_growth)
  @azimuth.assert_eq(540000, compounded)
}

test "geometry_calculations" {
  // 测试几何计算
  let radius = 6
  
  // 计算圆的周长（简化：2 × π × r ≈ 2 × 3 × r）
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(36, circumference)
  
  // 计算圆的面积（简化：π × r² ≈ 3 × r²）
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(108, area)
  
  // 计算球的体积（简化：4/3 × π × r³ ≈ 4 × r³）
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(864, volume)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 3
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(13500, total_seconds)
  
  // 计算时间差
  let start_time = 540  // 9:00 AM in minutes
  let end_time = 1200   // 8:00 PM in minutes
  let duration = @azimuth.add(end_time, -start_time)
  @azimuth.assert_eq(660, duration)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(15, -35)
  let diff2 = @azimuth.add(25, -35)
  let diff3 = @azimuth.add(35, -35)
  let diff4 = @azimuth.add(45, -35)
  let diff5 = @azimuth.add(55, -35)
  
  let sq1 = @azimuth.multiply(diff1, diff1)
  let sq2 = @azimuth.multiply(diff2, diff2)
  let sq3 = @azimuth.multiply(diff3, diff3)
  let sq4 = @azimuth.multiply(diff4, diff4)
  let sq5 = @azimuth.multiply(diff5, diff5)
  
  let sum_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq1, sq2), sq3), sq4), sq5)
  let variance = sum_squares / count
  @azimuth.assert_eq(200, variance)
}

test "game_mechanics" {
  // 测试游戏机制计算
  let base_score = 100
  let level = 8
  let combo = 3
  let bonus = 500
  let penalty = 150
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level)
  
  // 应用连击倍数
  let combo_score = @azimuth.multiply(level_score, combo)
  
  // 应用加成和减分
  let final_score = @azimuth.add(@azimuth.add(combo_score, bonus), -penalty)
  @azimuth.assert_eq(2350, final_score)
  
  // 计算排名分数（每500分一个等级）
  let rank_level = final_score / 500
  @azimuth.assert_eq(4, rank_level)
}

test "unit_conversion" {
  // 测试单位转换
  let meters = 100
  
  // 转换为厘米（1米 = 100厘米）
  let centimeters = @azimuth.multiply(meters, 100)
  @azimuth.assert_eq(10000, centimeters)
  
  // 转换为毫米（1厘米 = 10毫米）
  let millimeters = @azimuth.multiply(centimeters, 10)
  @azimuth.assert_eq(100000, millimeters)
  
  // 转换为千米（1000米 = 1千米）
  let kilometers = meters / 1000
  @azimuth.assert_eq(0, kilometers)
}