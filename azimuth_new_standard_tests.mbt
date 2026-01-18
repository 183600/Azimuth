// Azimuth 项目新的标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum = azimuth.add(a, b)
  @assertion.assert_eq(22, sum)?
  
  // 测试乘法
  let product = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product)?
  
  // 测试负数运算
  let negative_a = -5
  let positive_b = 10
  let mixed_sum = azimuth.add(negative_a, positive_b)
  @assertion.assert_eq(5, mixed_sum)?
}

test "zero_value_operations" {
  // 零值运算测试
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 零加任何数
  let zero_add_pos = azimuth.add(zero, positive)
  @assertion.assert_eq(42, zero_add_pos)?
  
  let zero_add_neg = azimuth.add(zero, negative)
  @assertion.assert_eq(-17, zero_add_neg)?
  
  // 零乘任何数
  let zero_mul_pos = azimuth.multiply(zero, positive)
  @assertion.assert_eq(0, zero_mul_pos)?
  
  let zero_mul_neg = azimuth.multiply(zero, negative)
  @assertion.assert_eq(0, zero_mul_neg)?
}

test "string_greeting_functions" {
  // 字符串问候函数测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let japanese_name = "田中"
  let empty_name = ""
  let number_name = "123"
  
  // 测试英文名字
  let english_greeting = azimuth.greet(english_name)
  @assertion.assert_eq_string("Hello, Alice!", english_greeting)?
  
  // 测试中文名字
  let chinese_greeting = azimuth.greet(chinese_name)
  @assertion.assert_eq_string("Hello, 小明!", chinese_greeting)?
  
  // 测试日文名字
  let japanese_greeting = azimuth.greet(japanese_name)
  @assertion.assert_eq_string("Hello, 田中!", japanese_greeting)?
  
  // 测试空字符串
  let empty_greeting = azimuth.greet(empty_name)
  @assertion.assert_eq_string("Hello, !", empty_greeting)?
  
  // 测试数字字符串
  let number_greeting = azimuth.greet(number_name)
  @assertion.assert_eq_string("Hello, 123!", number_greeting)?
}

test "mathematical_commutativity" {
  // 数学交换律测试
  let x = 23
  let y = 17
  
  // 加法交换律: a + b = b + a
  let add_ab = azimuth.add(x, y)
  let add_ba = azimuth.add(y, x)
  @assertion.assert_eq(add_ab, add_ba)?
  
  // 乘法交换律: a * b = b * a
  let mul_ab = azimuth.multiply(x, y)
  let mul_ba = azimuth.multiply(y, x)
  @assertion.assert_eq(mul_ab, mul_ba)?
  
  // 测试负数交换律
  let neg_x = -8
  let neg_y = 12
  
  let neg_add_ab = azimuth.add(neg_x, neg_y)
  let neg_add_ba = azimuth.add(neg_y, neg_x)
  @assertion.assert_eq(neg_add_ab, neg_add_ba)?
  
  let neg_mul_ab = azimuth.multiply(neg_x, neg_y)
  let neg_mul_ba = azimuth.multiply(neg_y, neg_x)
  @assertion.assert_eq(neg_mul_ab, neg_mul_ba)?
}

test "financial_simple_interest" {
  // 金融简单利息计算测试
  let principal = 5000  // 本金
  let rate = 6          // 年利率（百分比）
  let years = 3         // 年数
  
  // 计算简单利息: 本金 × 利率 × 年数 / 100
  let interest = azimuth.multiply(azimuth.multiply(principal, rate), years) / 100
  
  // 计算本息和: 本金 + 利息
  let total = azimuth.add(principal, interest)
  
  // 验证结果: 5000 + 5000×6×3/100 = 5000 + 900 = 5900
  @assertion.assert_eq(5900, total)?
  
  // 测试不同本金
  let principal2 = 10000
  let interest2 = azimuth.multiply(azimuth.multiply(principal2, rate), years) / 100
  let total2 = azimuth.add(principal2, interest2)
  @assertion.assert_eq(11800, total2)?
}

test "geometry_rectangle_properties" {
  // 几何矩形属性测试
  let length = 12
  let width = 8
  
  // 计算面积: 长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 计算周长: 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 计算对角线长度平方（勾股定理）: 长² + 宽²
  let diagonal_squared = azimuth.add(azimuth.multiply(length, length), azimuth.multiply(width, width))
  @assertion.assert_eq(208, diagonal_squared)?
  
  // 测试正方形（长=宽）
  let square_side = 5
  let square_area = azimuth.multiply(square_side, square_side)
  let square_perimeter = azimuth.multiply(4, square_side)
  @assertion.assert_eq(25, square_area)?
  @assertion.assert_eq(20, square_perimeter)?
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let days = 2
  let hours = 5
  let minutes = 30
  
  // 转换为小时: 天 × 24 + 小时
  let total_hours = azimuth.add(azimuth.multiply(days, 24), hours)
  @assertion.assert_eq(53, total_hours)?
  
  // 转换为分钟: 总小时 × 60 + 分钟
  let total_minutes = azimuth.add(azimuth.multiply(total_hours, 60), minutes)
  @assertion.assert_eq(3210, total_minutes)?
  
  // 转换为秒: 总分钟 × 60
  let total_seconds = azimuth.multiply(total_minutes, 60)
  @assertion.assert_eq(192600, total_seconds)?
  
  // 测试一周的时间
  let week_days = 7
  let week_hours = azimuth.multiply(week_days, 24)
  let week_minutes = azimuth.multiply(week_hours, 60)
  @assertion.assert_eq(168, week_hours)?
  @assertion.assert_eq(10080, week_minutes)?
}

test "shopping_cart_calculations" {
  // 购物车计算测试
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let shipping = 15
  
  // 计算每种商品总价
  let item1_total = azimuth.multiply(item1_price, item1_quantity)
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = azimuth.add(item1_total, item2_total)
  
  // 计算最终价格（含折扣和运费）
  let final_price = azimuth.add(azimuth.add(subtotal, -discount), shipping)
  
  // 验证结果: (199×2 + 299×1) - 50 + 15 = 697 - 50 + 15 = 662
  @assertion.assert_eq(662, final_price)?
  
  // 测试免运费情况
  let free_shipping_threshold = 500
  let free_shipping_final = azimuth.add(subtotal, -discount)
  @assertion.assert_eq(647, free_shipping_final)?
}

test "statistical_average_calculation" {
  // 统计平均值计算测试
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 89
  
  // 计算总分
  let total_score = azimuth.add(azimuth.add(azimuth.add(azimuth.add(score1, score2), score3), score4), score5)
  @assertion.assert_eq(440, total_score)?
  
  // 计算平均分
  let average_score = total_score / 5
  @assertion.assert_eq(88, average_score)?
  
  // 计算与平均分的差值
  let diff1 = azimuth.add(score1, -average_score)
  let diff2 = azimuth.add(score2, -average_score)
  let diff3 = azimuth.add(score3, -average_score)
  let diff4 = azimuth.add(score4, -average_score)
  let diff5 = azimuth.add(score5, -average_score)
  
  // 验证差值总和为零
  let diff_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(diff1, diff2), diff3), azimuth.add(diff4, diff5))
  @assertion.assert_eq(0, diff_sum)?
}

test "game_score_system" {
  // 游戏得分系统测试
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 50
  let penalty = 20
  
  // 计算等级得分
  let level_score = azimuth.multiply(base_score, level_multiplier)
  @assertion.assert_eq(300, level_score)?
  
  // 计算连击得分
  let combo_score = azimuth.add(level_score, combo_bonus)
  @assertion.assert_eq(350, combo_score)?
  
  // 计算最终得分（扣罚分）
  let final_score = azimuth.add(combo_score, -penalty)
  @assertion.assert_eq(330, final_score)?
  
  // 测试多轮游戏累积得分
  let round1_score = 250
  let round2_score = 180
  let round3_score = 320
  let total_game_score = azimuth.add(azimuth.add(round1_score, round2_score), round3_score)
  @assertion.assert_eq(750, total_game_score)?
}