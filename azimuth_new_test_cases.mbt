// Azimuth 项目新测试用例
// 使用标准 MoonBit 测试语法

test "fibonacci_sequence_calculation" {
  // 使用加法计算斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "factorial_calculation" {
  // 使用乘法计算阶乘
  let fact1 = 1
  let fact2 = azimuth::multiply(2, fact1)    // 2
  let fact3 = azimuth::multiply(3, fact2)    // 6
  let fact4 = azimuth::multiply(4, fact3)    // 24
  let fact5 = azimuth::multiply(5, fact4)    // 120
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
}

test "power_calculation" {
  // 使用乘法计算幂运算
  // 2^3 = 2 * 2 * 2 = 8
  let power_2_3 = azimuth::multiply(azimuth::multiply(2, 2), 2)
  assert_eq(8, power_2_3)
  
  // 3^4 = 3 * 3 * 3 * 3 = 81
  let power_3_4 = azimuth::multiply(azimuth::multiply(azimuth::multiply(3, 3), 3), 3)
  assert_eq(81, power_3_4)
  
  // 5^2 = 5 * 5 = 25
  let power_5_2 = azimuth::multiply(5, 5)
  assert_eq(25, power_5_2)
}

test "greet_name_length_categories" {
  // 测试不同长度名称的问候
  let short_name = "AI"
  let medium_name = "MoonBit"
  let long_name = "Artificial Intelligence"
  
  assert_eq("Hello, AI!", azimuth::greet(short_name))
  assert_eq("Hello, MoonBit!", azimuth::greet(medium_name))
  assert_eq("Hello, Artificial Intelligence!", azimuth::greet(long_name))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 7
  let y = 13
  
  // (x + y)² = x² + 2xy + y²
  let x_plus_y = azimuth::add(x, y)
  let left_side = azimuth::multiply(x_plus_y, x_plus_y)
  
  let x_squared = azimuth::multiply(x, x)
  let y_squared = azimuth::multiply(y, y)
  let two_xy = azimuth::multiply(azimuth::multiply(2, x), y)
  let right_side = azimuth::add(azimuth::add(x_squared, two_xy), y_squared)
  
  assert_eq(left_side, right_side)
}

test "temperature_conversion" {
  // 测试温度转换计算
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let fahrenheit2 = 212  // 沸点
  let celsius2 = azimuth::multiply(azimuth::add(fahrenheit2, -32), 5) / 9
  assert_eq(100, celsius2)
}

test "geometric_calculations" {
  // 测试几何计算
  // 正方形面积 = 边长²
  let square_side = 6
  let square_area = azimuth::multiply(square_side, square_side)
  assert_eq(36, square_area)
  
  // 长方形周长 = 2 * (长 + 宽)
  let rect_length = 8
  let rect_width = 5
  let rect_perimeter = azimuth::multiply(2, azimuth::add(rect_length, rect_width))
  assert_eq(26, rect_perimeter)
  
  // 三角形面积 = 底 * 高 / 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = azimuth::multiply(triangle_base, triangle_height) / 2
  assert_eq(30, triangle_area)
}

test "time_calculations" {
  // 测试时间计算
  // 计算总分钟数
  let hours = 3
  let minutes = 45
  let total_minutes = azimuth::add(azimuth::multiply(hours, 60), minutes)
  assert_eq(225, total_minutes)
  
  // 计算总秒数
  let hours2 = 2
  let minutes2 = 30
  let seconds2 = 15
  let total_seconds = azimuth::add(azimuth::add(azimuth::multiply(hours2, 3600), azimuth::multiply(minutes2, 60)), seconds2)
  assert_eq(9015, total_seconds)
}

test "percentage_calculations" {
  // 测试百分比计算
  // 计算百分比值
  let total = 200
  let percentage = 25
  let value = azimuth::multiply(total, percentage) / 100
  assert_eq(50, value)
  
  // 计算折扣价格
  let original_price = 120
  let discount = 20
  let discount_amount = azimuth::multiply(original_price, discount) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  assert_eq(96, final_price)
}

test "compound_interest_simple" {
  // 测试简单复利计算
  // 本金、利率、时间
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1_interest = azimuth::multiply(principal, rate) / 100
  let year1_total = azimuth::add(principal, year1_interest)
  
  // 第二年
  let year2_interest = azimuth::multiply(year1_total, rate) / 100
  let year2_total = azimuth::add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = azimuth::multiply(year2_total, rate) / 100
  let year3_total = azimuth::add(year2_total, year3_interest)
  
  assert_eq(1050, year1_total)
  assert_eq(1102, year2_total)
  assert_eq(1157, year3_total)
}