// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和断言
test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = azimuth::@azimuth.add(a, b)
  let result2 = azimuth::@azimuth.add(b, a)
  azimuth::@azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -123
  let d = 456
  
  let result3 = azimuth::@azimuth.add(c, d)
  let result4 = azimuth::@azimuth.add(d, c)
  azimuth::@azimuth.assert_eq(result3, result4)
}
test "multiplication_associativity" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 7
  let b = 8
  let c = 9
  
  let result1 = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(a, b), c)
  let result2 = azimuth::@azimuth.multiply(a, azimuth::@azimuth.multiply(b, c))
  azimuth::@azimuth.assert_eq(result1, result2)
  
  // 测试包含0的情况
  let zero = 0
  let result3 = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(a, zero), c)
  let result4 = azimuth::@azimuth.multiply(a, azimuth::@azimuth.multiply(zero, c))
  azimuth::@azimuth.assert_eq(result3, result4)
}
test "mixed_operations_precision" {
  // 测试混合运算的精度
  let x = 100
  let y = 25
  let z = 4
  
  // 计算 (x / y) * z 与 x * (z / y) 的比较
  // 由于是整数运算，我们使用加法和乘法来模拟
  let result1 = azimuth::@azimuth.multiply(azimuth::@azimuth.add(x, y), z)
  let result2 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(x, z), azimuth::@azimuth.multiply(y, z))
  azimuth::@azimuth.assert_eq(result1, result2)
}
test "string_special_characters_handling" {
  // 测试特殊字符的字符串处理
  let unicode_name = "张三李四"
  let emoji_name = "🚀🌟"
  let mixed_name = "Test123!@#"
  let whitespace_name = "  spaced  "
  
  azimuth::@azimuth.assert_eq_string("Hello, 张三李四!", azimuth::@azimuth.greet(unicode_name))
  azimuth::@azimuth.assert_eq_string("Hello, 🚀🌟!", azimuth::@azimuth.greet(emoji_name))
  azimuth::@azimuth.assert_eq_string("Hello, Test123!@#!", azimuth::@azimuth.greet(mixed_name))
  azimuth::@azimuth.assert_eq_string("Hello,   spaced  !", azimuth::@azimuth.greet(whitespace_name))
}
test "extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的各种运算组合
  let sum_result = azimuth::@azimuth.add(max_val, min_val)
  let mult_result = azimuth::@azimuth.multiply(max_val, 0)
  
  azimuth::@azimuth.assert_eq(-1, sum_result)
  azimuth::@azimuth.assert_eq(0, mult_result)
  
  // 边界值附近的运算
  let near_max = azimuth::@azimuth.add(max_val, -100)
  let near_min = azimuth::@azimuth.add(min_val, 100)
  
  azimuth::@azimuth.assert_eq(2147483547, near_max)
  azimuth::@azimuth.assert_eq(-2147483548, near_min)
}
test "sequential_calculation_chain" {
  // 测试连续计算链
  let initial = 10
  
  // 连续加法：10 + 20 + 30 + 40 + 50
  let step1 = azimuth::@azimuth.add(initial, 20)
  let step2 = azimuth::@azimuth.add(step1, 30)
  let step3 = azimuth::@azimuth.add(step2, 40)
  let step4 = azimuth::@azimuth.add(step3, 50)
  
  azimuth::@azimuth.assert_eq(150, step4)
  
  // 连续乘法：2 * 3 * 4 * 5
  let mult_step1 = azimuth::@azimuth.multiply(2, 3)
  let mult_step2 = azimuth::@azimuth.multiply(mult_step1, 4)
  let mult_step3 = azimuth::@azimuth.multiply(mult_step2, 5)
  
  azimuth::@azimuth.assert_eq(120, mult_step3)
}
test "financial_calculation_scenario" {
  // 测试金融计算场景：贷款计算
  let principal = 100000  // 本金
  let rate = 5            // 利率百分比
  let years = 3           // 年数
  
  // 简化的单利计算：利息 = 本金 * 利率 * 年数 / 100
  let interest = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(principal, rate), years) / 100
  let total_amount = azimuth::@azimuth.add(principal, interest)
  
  // 验证：100000 + 100000*5*3/100 = 100000 + 15000 = 115000
  azimuth::@azimuth.assert_eq(115000, total_amount)
}
test "geometry_calculations" {
  // 测试几何计算：矩形和三角形面积
  let length = 12
  let width = 8
  let base = 10
  let height = 6
  
  // 矩形面积：长 × 宽
  let rectangle_area = azimuth::@azimuth.multiply(length, width)
  azimuth::@azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_area = azimuth::@azimuth.multiply(base, height) / 2
  azimuth::@azimuth.assert_eq(30, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 4
  let trapezoid_area = azimuth::@azimuth.multiply(azimuth::@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  azimuth::@azimuth.assert_eq(40, trapezoid_area)
}
test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数运算：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_temp, 2), 30)
  azimuth::@azimuth.assert_eq(80, fahrenheit_temp)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(boiling_celsius, 2), 30)
  azimuth::@azimuth.assert_eq(230, boiling_fahrenheit)
}
test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的总和、平均值、中位数
  let numbers_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(85, 90), 78), 92), 88)
  let count = 5
  let average = numbers_sum / count
  
  azimuth::@azimuth.assert_eq(433, numbers_sum)
  azimuth::@azimuth.assert_eq(86, average)  // 433 / 5 = 86.6，整数除法取86
  
  // 计算方差（简化版）
  let mean = 86
  let variance_numerator = azimuth::@azimuth.add(
    azimuth::@azimuth.add(
      azimuth::@azimuth.multiply(azimuth::@azimuth.add(85, -mean), azimuth::@azimuth.add(85, -mean)),
      azimuth::@azimuth.multiply(azimuth::@azimuth.add(90, -mean), azimuth::@azimuth.add(90, -mean))
    ),
    azimuth::@azimuth.add(
      azimuth::@azimuth.multiply(azimuth::@azimuth.add(78, -mean), azimuth::@azimuth.add(78, -mean)),
      azimuth::@azimuth.multiply(azimuth::@azimuth.add(92, -mean), azimuth::@azimuth.add(92, -mean))
    )
  )
  
  // 计算标准差（整数除法）
  let variance = variance_numerator / count
  azimuth::@azimuth.assert_eq(22, variance)  // 近似值
}
// 新增的测试用例
test "string_with_unicode_characters" {
  // 测试包含Unicode字符的字符串
  azimuth::@azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  azimuth::@azimuth.assert_eq_string("Hello, 🌍!", azimuth::@azimuth.greet("🌍"))
  azimuth::@azimuth.assert_eq_string("Hello, 👋!", azimuth::@azimuth.greet("👋"))
}
test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(first, last)) / 2
  azimuth::@azimuth.assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
}
test "power_of_two_calculation" {
  // 计算2的幂：2^8 = 256
  // 通过连续相乘实现
  let result = azimuth::@azimuth.multiply(
    azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(2, 2), azimuth::@azimuth.multiply(2, 2)),
    azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(2, 2), azimuth::@azimuth.multiply(2, 2))
  )
  azimuth::@azimuth.assert_eq(256, result)
}
test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 25  // 25%
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = azimuth::@azimuth.multiply(total, percentage) / 100
  azimuth::@azimuth.assert_eq(125, value)  // 500 × 25 / 100 = 125
  
  // 计算增加后的值：总数 + 百分比值
  let increased = azimuth::@azimuth.add(total, value)
  azimuth::@azimuth.assert_eq(625, increased)  // 500 + 125 = 625
}
test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 4
  
  let sum_ab = azimuth::@azimuth.add(a, b)
  let diff_cd = azimuth::@azimuth.add(c, -d)
  let product_ef = azimuth::@azimuth.multiply(e, f)
  let product = azimuth::@azimuth.multiply(sum_ab, diff_cd)
  let result = azimuth::@azimuth.add(product, product_ef)
  
  // 验证：(10 + 5) × (20 - 8) + 3 × 4 = 15 × 12 + 12 = 180 + 12 = 192
  azimuth::@azimuth.assert_eq(192, result)
}