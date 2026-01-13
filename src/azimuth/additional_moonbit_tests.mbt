// 额外的 MoonBit 测试用例
// 覆盖一些未充分测试的场景和边界情况

test "add_with_large_positive_numbers" {
  // 测试大正数相加
  let result1 = @azimuth.add(1000000000, 1000000000)
  @azimuth.assert_eq(result1, 2000000000)
  
  let result2 = @azimuth.add(1500000000, 1500000000)
  @azimuth.assert_eq(result2, 2147483647)  // 应该被限制为最大值
}

test "add_with_large_negative_numbers" {
  // 测试大负数相加
  let result1 = @azimuth.add(-1000000000, -1000000000)
  @azimuth.assert_eq(result1, -2000000000)
  
  let result2 = @azimuth.add(-1500000000, -1500000000)
  @azimuth.assert_eq(result2, -2147483648)  // 应该被限制为最小值
}

test "multiply_with_fractional_approximation" {
  // 测试通过整数运算近似分数运算
  // 计算 1/3 * 9 的近似值：1 * 9 / 3 = 3
  let numerator = 1
  let denominator = 3
  let value = 9
  
  let result = @azimuth.multiply(numerator, value) / denominator
  @azimuth.assert_eq(result, 3)
}

test "compound_interest_calculation" {
  // 测试复利计算（简化版）
  // 公式：最终金额 = 本金 * (1 + 利率)^年数
  // 简化为整数运算：最终金额 = 本金 + 本金 * 利率 * 年数
  let principal = 10000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(total, 160000)  // 10000 + 10000*5*3 = 160000，表示1600.00元
}

test "discount_calculation" {
  // 测试折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  
  // 计算折扣金额：原价 * 折扣率 / 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算最终价格：原价 - 折扣金额
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(discount_amount, 200)
  @azimuth.assert_eq(final_price, 800)
}

test "temperature_conversion_advanced" {
  // 测试更精确的温度转换
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 使用整数近似：F = C * 2 - C/5 + 32
  
  let celsius_temps = [@azimuth.add(-40, 0), @azimuth.add(0, 0), @azimuth.add(100, 0)]
  
  // 测试 -40°C（应该等于 -40°F）
  let c1 = celsius_temps[0]
  let f1 = @azimuth.add(@azimuth.add(@azimuth.multiply(c1, 2), -c1/5), 32)
  @azimuth.assert_eq(f1, -40)
  
  // 测试 0°C（应该等于 32°F）
  let c2 = celsius_temps[1]
  let f2 = @azimuth.add(@azimuth.add(@azimuth.multiply(c2, 2), -c2/5), 32)
  @azimuth.assert_eq(f2, 32)
  
  // 测试 100°C（应该约等于 212°F）
  let c3 = celsius_temps[2]
  let f3 = @azimuth.add(@azimuth.add(@azimuth.multiply(c3, 2), -c3/5), 32)
  @azimuth.assert_eq(f3, 212)
}

test "string_template_processing" {
  // 测试字符串模板处理
  // 通过拼接多个字符串来模拟模板处理
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 构建完整介绍字符串
  let greeting = @azimuth.greet(first_name + " " + last_name)
  let introduction = greeting + " I am " + age + " years old."
  
  @azimuth.assert_eq_string(introduction, "Hello, John Doe! I am 30 years old.")
}

test "array_sum_simulation" {
  // 模拟数组求和（使用多个变量表示数组元素）
  let num1 = 15
  let num2 = 25
  let num3 = 35
  let num4 = 45
  let num5 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  
  // 计算平均值
  let average = sum / 5
  
  @azimuth.assert_eq(sum, 175)
  @azimuth.assert_eq(average, 35)
}

test "geometric calculations" {
  // 测试几何计算
  // 计算圆的周长和面积（使用π≈3.14159的近似值）
  let radius = 10
  let pi_approx = 3  // 使用 π≈3 的近似值
  
  // 周长 = 2 * π * r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  
  // 面积 = π * r²
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  
  @azimuth.assert_eq(circumference, 60)  // 2 * 3 * 10 = 60
  @azimuth.assert_eq(area, 300)         // 3 * 10 * 10 = 300
}

test "time_calculations" {
  // 测试时间计算
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 将小时和分钟转换为秒
  let hours_in_seconds = @azimuth.multiply(hours, 3600)
  let minutes_in_seconds = @azimuth.multiply(minutes, 60)
  
  // 计算总秒数
  let total_seconds = @azimuth.add(@azimuth.add(hours_in_seconds, minutes_in_seconds), seconds)
  
  @azimuth.assert_eq(hours_in_seconds, 7200)   // 2 * 3600 = 7200
  @azimuth.assert_eq(minutes_in_seconds, 1500) // 30 * 60 = 1500
  @azimuth.assert_eq(total_seconds, 9045)      // 7200 + 1500 + 45 = 9045
}