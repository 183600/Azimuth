// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，补充现有测试覆盖

test "add_zero_identity" {
  // 测试加法零元素特性
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(0, -17), -17)
  @azimuth.assert_eq(@azimuth.add(-100, 0), -100)
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(@azimuth.add(-5, -3), -8)
  @azimuth.assert_eq(@azimuth.add(-10, 7), -3)
  @azimuth.assert_eq(@azimuth.add(15, -20), -5)
  @azimuth.assert_eq(@azimuth.add(-8, 12), 4)
}

test "multiply_zero_property" {
  // 测试乘法零元素特性
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, -7), 0)
  @azimuth.assert_eq(@azimuth.multiply(-12, 0), 0)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(@azimuth.multiply(-3, -4), 12)
  @azimuth.assert_eq(@azimuth.multiply(-5, 6), -30)
  @azimuth.assert_eq(@azimuth.multiply(7, -8), -56)
  @azimuth.assert_eq(@azimuth.multiply(-2, -9), 18)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  @azimuth.assert_eq_string(@azimuth.greet("!@#$%"), "Hello, !@#$%!")
  @azimuth.assert_eq_string(@azimuth.greet("()[]{}"), "Hello, ()[]{}!")
  @azimuth.assert_eq_string(@azimuth.greet("+-*/="), "Hello, +-*/=!")
}

test "greet_multilingual" {
  // 测试多语言问候
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("العالم"), "Hello, العالم!")
  @azimuth.assert_eq_string(@azimuth.greet("мир"), "Hello, мир!")
  @azimuth.assert_eq_string(@azimuth.greet("monde"), "Hello, monde!")
}

test "calculate_perimeter" {
  // 测试计算矩形周长
  let length = 10
  let width = 6
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(32, perimeter)
  
  let length2 = 15
  let width2 = 8
  let perimeter2 = @azimuth.multiply(2, @azimuth.add(length2, width2))
  @azimuth.assert_eq(46, perimeter2)
}

test "calculate_average" {
  // 测试计算平均值
  let num1 = 12
  let num2 = 18
  let num3 = 24
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let average = sum / 3
  @azimuth.assert_eq(18, average)
}

test "temperature_conversion" {
  // 测试温度转换场景 (摄氏度到华氏度)
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  let celsius2 = 0
  let fahrenheit2 = @azimuth.add(@azimuth.multiply(celsius2, 9) / 5, 32)
  @azimuth.assert_eq(32, fahrenheit2)
}

test "compound_interest_simple" {
  // 测试简单复利计算
  let principal = 1000
  let rate = 5  // 5% 利率
  
  // 简化的复利计算: principal * (1 + rate/100)^2
  let first_year = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  let second_year = @azimuth.add(first_year, @azimuth.multiply(first_year, rate) / 100)
  
  @azimuth.assert_eq(1102, second_year)  // 近似值
}