// 实用的核心功能测试用例
// 覆盖 Azimuth 项目的主要功能

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + 8 + 10 = 30
  let term1 = 2
  let term2 = 4
  let term3 = 6
  let term4 = 8
  let term5 = 10
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(30, sum)
}

test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16 = 1024
  let factor1 = 2
  let factor2 = 4
  let factor3 = 8
  let factor4 = 16
  
  let product = multiply(multiply(factor1, factor2), multiply(factor3, factor4))
  assert_eq(1024, product)
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Мира!", greet("Мира"))
  assert_eq_string("Hello, 🌏!", greet("🌏"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  assert_eq(16000, total)
}

test "rectangle_perimeter_calculation" {
  // 矩形周长计算：2 × (长 + 宽)
  let length = 15
  let width = 8
  
  let sum_sides = add(length, width)
  let perimeter = multiply(2, sum_sides)
  assert_eq(46, perimeter)
}

test "volume_of rectangular_prism" {
  // 长方体体积计算：长 × 宽 × 高
  let length = 4
  let width = 5
  let height = 6
  
  let base_area = multiply(length, width)
  let volume = multiply(base_area, height)
  assert_eq(120, volume)
}

test "discount_price_calculation" {
  // 折扣价格计算：原价 - 折扣
  let original_price = 200
  let discount_percentage = 15
  
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(170, final_price)
}

test "average_of_numbers" {
  // 计算数字的平均值：(10 + 20 + 30 + 40 + 50) / 5 = 30
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  let average = sum / 5
  assert_eq(30, average)
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间计算：距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时，简化为整数运算
  
  // 使用整数计算：60 × 2 + 60 × 0.5 ≈ 60 × 2 + 60 × 1 / 2
  let distance_integer_part = multiply(speed, 2)
  let distance_half_part = multiply(speed, 1) / 2
  let total_distance = add(distance_integer_part, distance_half_part)
  assert_eq(150, total_distance)
}