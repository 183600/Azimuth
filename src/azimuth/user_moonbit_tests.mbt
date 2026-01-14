// 额外的 MoonBit 测试用例
// 测试 azimuth 包的更多功能组合和边界情况

test "sequential_operations" {
  // 测试连续运算：(a + b) * (c + d)
  let a = 5
  let b = 3
  let c = 2
  let d = 4
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(72, result)  // (5+3)*(2+4) = 8*6 = 48
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版 F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25*2+30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, 用户#123!", @azimuth.greet("用户#123"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}

test "extreme_value_combinations" {
  // 测试极值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值与-1相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值与1相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "financial_calculation" {
  // 测试金融计算：复利（简化版）
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化复利：本金 * (1 + 利率 * 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, total)  // 10000 * (1 + 5*3) = 10000 * 16 = 160000
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 8
  let width = 6
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(28, perimeter)  // 2*(8+6) = 28
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)  // 8*6 = 48
}

test "nested_calculations" {
  // 测试嵌套计算：(a * b) + (c * d) - (e * f)
  let a = 10
  let b = 3
  let c = 5
  let d = 4
  let e = 2
  let f = 6
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let product3 = @azimuth.multiply(e, f)
  
  let result = @azimuth.add(@azimuth.add(product1, product2), -product3)
  @azimuth.assert_eq(38, result)  // (10*3)+(5*4)-(2*6) = 30+20-12 = 38
}

test "boundary_error_handling" {
  // 测试边界错误处理
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试正常情况
  @azimuth.assert_true(@azimuth.add(100, 200) == 300)
  @azimuth.assert_true(@azimuth.multiply(10, 20) == 200)
}

test "average_calculation" {
  // 测试平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  let num5 = 100
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  // 计算平均值
  let average = sum / 5
  
  @azimuth.assert_eq(90, average)  // (85+90+95+80+100)/5 = 450/5 = 90
}

test "power_calculation" {
  // 测试幂运算（通过重复乘法实现）
  let base = 3
  
  // 计算 3^4 = 81
  let power = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, base), base), base)
  @azimuth.assert_eq(81, power)
  
  // 计算 2^6 = 64
  let power2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(64, power2)
}