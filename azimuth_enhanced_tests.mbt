// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "string_operations_basic" {
  // 基本字符串操作测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, Space !", @azimuth.greet("Space "))
}

test "arithmetic_sequence_tests" {
  // 算术序列测试
  
  // 斐波那契数列前几项: 1, 1, 2, 3, 5, 8, 13
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  @azimuth.assert_eq(13, fib7)
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  
  // 等差数列求和: 1 + 2 + 3 + 4 + 5 = 15
  let sum_5 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum_5)
  
  // 等比数列求和: 2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
  
  // 平方数列: 1² + 2² + 3² = 1 + 4 + 9 = 14
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3))
  @azimuth.assert_eq(14, square_sum)
}

test "temperature_conversion_tests" {
  // 温度转换测试
  
  // 摄氏度到华氏度: F = C * 9/5 + 32
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(68, fahrenheit)
  
  // 摄氏度到开尔文: K = C + 273
  let kelvin = @azimuth.add(celsius, 273)
  @azimuth.assert_eq(293, kelvin)
  
  // 沸点测试
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 9) / 5, 32)
  @azimuth.assert_eq(212, boiling_fahrenheit)
}

test "financial_calculations" {
  // 金融计算测试
  
  // 简单利息: 本金 * 利率 * 时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 2  // 2年
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(1100, total_amount)
  
  // 折扣计算: 原价 * (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
}

test "geometry_calculations" {
  // 几何计算测试
  
  // 正方形面积: 边长 * 边长
  let square_side = 5
  let square_area = @azimuth.multiply(square_side, square_side)
  @azimuth.assert_eq(25, square_area)
  
  // 矩形周长: 2 * (长 + 宽)
  let rect_length = 8
  let rect_width = 4
  let rect_perimeter = @azimuth.multiply(2, @azimuth.add(rect_length, rect_width))
  @azimuth.assert_eq(24, rect_perimeter)
  
  // 三角形面积: 底 * 高 / 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = @azimuth.multiply(@azimuth.multiply(triangle_base, triangle_height), 1) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "time_calculations" {
  // 时间计算测试
  
  // 分钟转秒: 分钟 * 60
  let minutes = 5
  let seconds = @azimuth.multiply(minutes, 60)
  @azimuth.assert_eq(300, seconds)
  
  // 小时转分钟: 小时 * 60
  let hours = 3
  let total_minutes = @azimuth.multiply(hours, 60)
  @azimuth.assert_eq(180, total_minutes)
  
  // 天转小时: 天 * 24
  let days = 2
  let total_hours = @azimuth.multiply(days, 24)
  @azimuth.assert_eq(48, total_hours)
  
  // 一周的总分钟数
  let week_minutes = @azimuth.multiply(@azimuth.multiply(7, 24), 60)
  @azimuth.assert_eq(10080, week_minutes)
}

test "data_structure_operations" {
  // 数据结构操作测试
  
  // 栈操作模拟
  let stack_size = 0
  
  // push 操作
  stack_size = @azimuth.add(stack_size, 1)  // push 第一个元素
  @azimuth.assert_eq(1, stack_size)
  
  stack_size = @azimuth.add(stack_size, 1)  // push 第二个元素
  @azimuth.assert_eq(2, stack_size)
  
  stack_size = @azimuth.add(stack_size, 1)  // push 第三个元素
  @azimuth.assert_eq(3, stack_size)
  
  // pop 操作
  stack_size = @azimuth.add(stack_size, -1)  // pop 一个元素
  @azimuth.assert_eq(2, stack_size)
  
  stack_size = @azimuth.add(stack_size, -1)  // pop 另一个元素
  @azimuth.assert_eq(1, stack_size)
}

test "game_logic_simulations" {
  // 游戏逻辑模拟测试
  
  // 简单的得分系统
  let base_score = 0
  let kill_points = 10
  let assist_points = 5
  
  // 玩家1: 3次击杀, 2次助攻
  let player1_kills = 3
  let player1_assists = 2
  let player1_score = @azimuth.add(@azimuth.multiply(player1_kills, kill_points), @azimuth.multiply(player1_assists, assist_points))
  @azimuth.assert_eq(40, player1_score)
  
  // 玩家2: 1次击杀, 5次助攻
  let player2_kills = 1
  let player2_assists = 5
  let player2_score = @azimuth.add(@azimuth.multiply(player2_kills, kill_points), @azimuth.multiply(player2_assists, assist_points))
  @azimuth.assert_eq(35, player2_score)
  
  // 团队总分
  let team_score = @azimuth.add(player1_score, player2_score)
  @azimuth.assert_eq(75, team_score)
}