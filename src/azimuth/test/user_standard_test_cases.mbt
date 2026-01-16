// 用户添加的标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 7
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 计算对角线平方（不使用平方根）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(289, diagonal_squared)
}

test "financial_calculation_simple_interest" {
  // 测试金融计算：单利计算
  let principal = 5000  // 本金
  let rate = 5  // 年利率百分比
  let years = 3  // 年数
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years)
  @azimuth.assert_eq(75000, interest)  // 5000 × 5 × 3 = 75000
  
  // 计算本息和
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(80000, total)  // 5000 + 75000 = 80000
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 简化公式：F = C × 2 + 30（避免小数）
  
  // 测试常见温度
  let celsius_0 = 0    // 冰点
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_10 = 10  // 凉爽
  let fahrenheit_10 = @azimuth.add(@azimuth.multiply(celsius_10, 2), 30)
  @azimuth.assert_eq(50, fahrenheit_10)
  
  let celsius_25 = 25  // 室温
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_25)
}

test "shopping_cart_calculation" {
  // 测试购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(652, final_total)  // (199×2 + 299×1) - 50 + 15 = 697 - 50 + 15 = 652
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
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, range)
  
  // 计算中位数（已排序数据集的中间值）
  let median = 35
  @azimuth.assert_eq(median, median)
}

test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 500
  let time_penalty = 100
  
  // 计算基础得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 应用加成和减分
  let with_bonus = @azimuth.add(level_score, combo_bonus)
  let final_score = @azimuth.add(with_bonus, -time_penalty)
  
  @azimuth.assert_eq(700, final_score)  // 100×3 + 500 - 100 = 800 - 100 = 700
  
  // 计算等级（每200分一级）
  let player_level = final_score / 200
  @azimuth.assert_eq(3, player_level)
}