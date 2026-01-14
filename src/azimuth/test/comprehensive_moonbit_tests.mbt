// 综合 MoonBit 测试用例
// 覆盖各种功能和场景的标准 MoonBit 测试

test "arithmetics_addition_complex" {
  // 测试复杂加法场景
  let a = 1000000
  let b = -500000
  let c = 750000
  
  // 测试连续加法
  let result1 = azimuth::@azimuth.add(azimuth::@azimuth.add(a, b), c)
  azimuth::@azimuth.assert_eq(1250000, result1)
  
  // 测试包含零的加法
  let result2 = azimuth::@azimuth.add(0, azimuth::@azimuth.add(a, b))
  azimuth::@azimuth.assert_eq(500000, result2)
}

test "arithmetics_multiplication_nested" {
  // 测试嵌套乘法运算
  let base = 15
  
  // 计算 base * (base + 1) * (base + 2)
  let result = azimuth::@azimuth.multiply(
    azimuth::@azimuth.multiply(base, azimuth::@azimuth.add(base, 1)),
    azimuth::@azimuth.add(base, 2)
  )
  
  // 验证结果：15 * 16 * 17 = 4080
  azimuth::@azimuth.assert_eq(4080, result)
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  azimuth::@azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  azimuth::@azimuth.assert_eq_string("Hello, Москва!", azimuth::@azimuth.greet("Москва"))
  azimuth::@azimuth.assert_eq_string("Hello, 🌟!", azimuth::@azimuth.greet("🌟"))
  azimuth::@azimuth.assert_eq_string("Hello, Café!", azimuth::@azimuth.greet("Café"))
}

test "boundary_value_extreme_cases" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值与普通值的运算
  azimuth::@azimuth.assert_eq(2147483646, azimuth::@azimuth.add(max_val, -1))
  azimuth::@azimuth.assert_eq(-2147483647, azimuth::@azimuth.add(min_val, 1))
  
  // 测试极值乘法
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(min_val, 0))
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 4
  
  // 简化复利计算：本金 × (1 + 利率 × 年数)
  let interest = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate, years))
  let total = azimuth::@azimuth.add(principal, interest)
  
  // 验证结果：10000 + 10000*5*4 = 10000 + 200000 = 210000
  azimuth::@azimuth.assert_eq(210000, total)
}

test "geometry_area_perimeter" {
  // 测试几何面积和周长计算
  let length = 12
  let width = 8
  
  // 计算矩形面积：长 × 宽
  let area = azimuth::@azimuth.multiply(length, width)
  azimuth::@azimuth.assert_eq(96, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = azimuth::@azimuth.multiply(2, azimuth::@azimuth.add(length, width))
  azimuth::@azimuth.assert_eq(40, perimeter)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 18
  
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  azimuth::@azimuth.assert_eq(66, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let back_to_celsius = azimuth::@azimuth.add(fahrenheit, -30) / 2
  azimuth::@azimuth.assert_eq(18, back_to_celsius)
}

test "array_index_calculation" {
  // 测试数组索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(row, width), col)
  azimuth::@azimuth.assert_eq(34, index)
  
  // 测试边界情况
  let first_index = azimuth::@azimuth.add(azimuth::@azimuth.multiply(0, width), 0)
  azimuth::@azimuth.assert_eq(0, first_index)
}

test "game_score_calculations" {
  // 测试游戏得分计算
  let base_score = 1000
  let level_bonus = 500
  let combo_multiplier = 3
  let penalty = 200
  
  // 计算最终得分：(基础分 + 关卡奖励) × 连击倍数 - 惩罚
  let subtotal = azimuth::@azimuth.add(base_score, level_bonus)
  let multiplied = azimuth::@azimuth.multiply(subtotal, combo_multiplier)
  let final_score = azimuth::@azimuth.add(multiplied, -penalty)
  
  // 验证结果：(1000 + 500) × 3 - 200 = 1500 × 3 - 200 = 4500 - 200 = 4300
  azimuth::@azimuth.assert_eq(4300, final_score)
}

test "statistical_calculations" {
  // 测试统计计算
  let values = [10, 20, 30, 40, 50]
  
  // 计算总和
  let sum = azimuth::@azimuth.add(
    azimuth::@azimuth.add(
      azimuth::@azimuth.add(values[0], values[1]),
      azimuth::@azimuth.add(values[2], values[3])
    ),
    values[4]
  )
  azimuth::@azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  azimuth::@azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）：平方和的平均值
  let square_sum = azimuth::@azimuth.add(
    azimuth::@azimuth.add(
      azimuth::@azimuth.multiply(values[0], values[0]),
      azimuth::@azimuth.multiply(values[2], values[2])
    ),
    azimuth::@azimuth.multiply(values[4], values[4])
  )
  let variance = square_sum / 3
  azimuth::@azimuth.assert_eq(1166, variance)  // (100 + 900 + 2500) / 3 = 3500 / 3 = 1166（整数除法）
}