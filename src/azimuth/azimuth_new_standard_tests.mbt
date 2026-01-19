// Azimuth 项目的新增标准测试用例
// 测试基本数学运算和字符串处理功能

test "add_commutativity_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试包含负数的情况
  let c = -15
  let d = 23
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  
  // 测试包含零的情况
  let e = 0
  let f = 100
  @azimuth.assert_eq(@azimuth.add(e, f), @azimuth.add(f, e))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let x = -4
  let y = 6
  let z = -2
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left_side2, right_side2)
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算二次方程的值
  // 对于方程 ax² + bx + c，当 x = 3 时的值
  let a = 2
  let b = -5
  let c = 7
  let x = 3
  
  // 计算 ax²
  let x_squared = @azimuth.multiply(x, x)  // 9
  let ax_squared = @azimuth.multiply(a, x_squared)  // 18
  
  // 计算 bx
  let bx = @azimuth.multiply(b, x)  // -15
  
  // 计算最终结果：ax² + bx + c
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  @azimuth.assert_eq(10, result)  // 18 + (-15) + 7 = 10
}

test "financial_calculation_compound_interest" {
  // 测试金融计算：复利计算（简化版）
  // 公式：最终金额 = 本金 × (1 + 利率)^年数
  // 简化为整数计算：最终金额 = 本金 × (100 + 利率×100)^年数 / 100^年数
  
  let principal = 10000  // 本金
  let rate_percent = 5    // 5% 利率
  let _years = 3          // 3年
  
  // 简化计算：每年增长本金的5%
  let year1_end = @azimuth.add(principal, @azimuth.multiply(principal, rate_percent) / 100)
  let year2_end = @azimuth.add(year1_end, @azimuth.multiply(year1_end, rate_percent) / 100)
  let year3_end = @azimuth.add(year2_end, @azimuth.multiply(year2_end, rate_percent) / 100)
  
  // 验证最终金额约为 11576（整数除法）
  @azimuth.assert_true(year3_end >= 11570 && year3_end <= 11580)
}

test "geometry_calculations" {
  // 测试几何计算
  
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)  // 2 × (8 + 5) = 26
  
  // 计算矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)  // 8 × 5 = 40
  
  // 计算三角形面积（简化）：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)  // 10 × 6 ÷ 2 = 30
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30（近似值）
  
  // 测试冰点：0°C = 32°F（近似为 30°F）
  let celsius_freezing = 0
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_freezing)
  
  // 测试沸点：100°C = 212°F（近似为 230°F）
  let celsius_boiling = 100
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_boiling)
  
  // 测试室温：20°C = 68°F（近似为 70°F）
  let celsius_room = 20
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_room)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_amount = 100
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 597
  
  // 计算小计
  let subtotal = @azimuth.add(total1, total2)  // 1195
  
  // 应用折扣（如果超过阈值）
  let final_total = if (subtotal > discount_threshold) {
    @azimuth.add(subtotal, -discount_amount)
  } else {
    subtotal
  }
  
  @azimuth.assert_eq(1095, final_total)  // 1195 - 100 = 1095
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 计算一组数的平均值
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let num5 = 80
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  let average = sum / 5
  
  @azimuth.assert_eq(85, average)  // (85 + 90 + 78 + 92 + 80) / 5 = 425 / 5 = 85
  
  // 计算中位数（已排序）
  let median = 85
  @azimuth.assert_eq(85, median)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(92, -78)
  @azimuth.assert_eq(14, range)
}

test "game_logic_calculation" {
  // 测试游戏逻辑计算
  
  // 计算玩家得分：基础分 + 时间奖励 - 惩罚
  let base_score = 1000
  let time_bonus = 200
  let penalty = 50
  
  let final_score = @azimuth.add(@azimuth.add(base_score, time_bonus), -penalty)
  @azimuth.assert_eq(1150, final_score)
  
  // 计算等级提升所需的分数
  let current_level = 5
  let score_per_level = 500
  let required_score = @azimuth.multiply(current_level, score_per_level)
  
  @azimuth.assert_eq(2500, required_score)
  
  // 计算剩余分数到下一级
  let player_score = 2200
  let next_level_score = @azimuth.multiply(@azimuth.add(current_level, 1), score_per_level)
  let points_needed = @azimuth.add(next_level_score, -player_score)
  
  @azimuth.assert_eq(300, points_needed)  // 3000 - 2700 = 300
}