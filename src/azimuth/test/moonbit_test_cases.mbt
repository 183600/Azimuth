// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(4, 6))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试包含负数的情况
  let c = -15
  @azimuth.assert_eq(@azimuth.add(a, c), @azimuth.add(c, a))
  @azimuth.assert_eq(@azimuth.multiply(a, c), @azimuth.multiply(c, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
}

test "financial_calculation_simple" {
  // 测试简单金融计算：单利计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(150000, interest)
  
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // 简化公式：F = C × 2 + 30
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_calculation" {
  // 测试购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  
  // 验证总价：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, cart_total)
}

test "statistical_calculation" {
  // 测试统计计算：平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  let num5 = 100
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(450, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(90, average)
}

test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 1000
  let level_multiplier = 3
  let bonus = 500
  let penalty = 200
  
  // 计算基础得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 应用加成和减分
  let final_score = @azimuth.add(@azimuth.add(level_score, bonus), -penalty)
  
  // 验证最终得分：1000*3 + 500 - 200 = 3000 + 500 - 200 = 3300
  @azimuth.assert_eq(3300, final_score)
}