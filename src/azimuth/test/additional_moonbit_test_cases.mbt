// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的各种功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(30, @azimuth.multiply(5, 6))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "overflow_protection_addition" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "overflow_protection_multiplication" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 15
  let width = 7
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(105, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(44, perimeter)
  
  // 计算对角线平方
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(274, diagonal_squared)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景：简化贷款计算
  let principal = 50000  // 本金
  let rate = 6           // 年利率百分比
  let years = 10         // 年数
  
  // 简化利息计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(3000000, interest)
  
  // 总还款额：本金 + 利息
  let total_payment = @azimuth.add(principal, interest)
  @azimuth.assert_eq(3050000, total_payment)
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 简化公式：F = C × 2 + 30
  
  // 测试常见温度点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
  @azimuth.assert_eq(90, @azimuth.add(@azimuth.multiply(30, 2), 30))
  @azimuth.assert_eq(110, @azimuth.add(@azimuth.multiply(40, 2), 30))
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
  
  // 计算范围
  let range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, range)
}

test "game_score_calculation" {
  // 测试游戏得分计算
  let base_score = 1000
  let level_multiplier = 4
  let bonus_points = 250
  let penalty = 100
  
  // 计算基础得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  @azimuth.assert_eq(4000, level_score)
  
  // 应用加成和减分
  let final_score = @azimuth.add(@azimuth.add(level_score, bonus_points), -penalty)
  @azimuth.assert_eq(4150, final_score)
  
  // 计算排名（每1000分一个等级）
  let rank = final_score / 1000
  @azimuth.assert_eq(4, rank)
}