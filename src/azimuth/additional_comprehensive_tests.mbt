// 额外的综合测试用例
// 使用标准的 MoonBit 测试语法，覆盖各种边界情况和实际应用场景

test "add_prime_numbers" {
  // 测试质数相加
  assert_eq(10, add(3, 7))
  assert_eq(17, add(2, 15))
  assert_eq(30, add(13, 17))
  assert_eq(100, add(47, 53))
}

test "multiply_perfect_squares" {
  // 测试完全平方数相乘
  assert_eq(36, multiply(4, 9))
  assert_eq(100, multiply(10, 10))
  assert_eq(225, multiply(9, 25))
  assert_eq(64, multiply(8, 8))
}

test "calculate_simple_interest" {
  // 测试简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest / 100)
  
  assert_eq(1150, total)  // 1000 + (1000 * 5 * 3) / 100 = 1150
}

test "greet_with_numbers_and_symbols" {
  // 测试包含数字和符号的字符串
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, #hashtag!", greet("#hashtag"))
  assert_eq_string("Hello, @mention!", greet("@mention"))
}

test "calculate_perimeter_of_shapes" {
  // 测试不同形状的周长计算
  
  // 正方形周长：4 × 边长
  let square_side = 5
  let square_perimeter = multiply(4, square_side)
  assert_eq(20, square_perimeter)
  
  // 三角形周长：边长1 + 边长2 + 边长3
  let triangle_side1 = 3
  let triangle_side2 = 4
  let triangle_side3 = 5
  let triangle_perimeter = add(add(triangle_side1, triangle_side2), triangle_side3)
  assert_eq(12, triangle_perimeter)
  
  // 五边形周长：5 × 边长
  let pentagon_side = 6
  let pentagon_perimeter = multiply(5, pentagon_side)
  assert_eq(30, pentagon_perimeter)
}

test "calculate_volume_of_cuboid" {
  // 测试长方体体积计算：长 × 宽 × 高
  let length = 4
  let width = 5
  let height = 6
  
  let base_area = multiply(length, width)
  let volume = multiply(base_area, height)
  
  assert_eq(120, volume)  // 4 × 5 × 6 = 120
}

test "complex_string_scenarios" {
  // 测试复杂字符串场景
  // 测试包含多种字符类型的字符串
  let mixed_string = "aB1!@#"
  assert_eq_string("Hello, aB1!@#!", greet(mixed_string))
  
  // 测试仅包含空格的字符串
  let spaces = "   "
  assert_eq_string("Hello,    !", greet(spaces))
  
  // 测试包含下划线的字符串
  let underscore = "user_name"
  assert_eq_string("Hello, user_name!", greet(underscore))
}

test "calculate_time_conversions" {
  // 测试时间单位转换
  // 小时转分钟：1小时 = 60分钟
  let hours = 3
  let minutes = multiply(hours, 60)
  assert_eq(180, minutes)
  
  // 天转小时：1天 = 24小时
  let days = 2
  let hours_in_days = multiply(days, 24)
  assert_eq(48, hours_in_days)
  
  // 周转天：1周 = 7天
  let weeks = 4
  let days_in_weeks = multiply(weeks, 7)
  assert_eq(28, days_in_weeks)
}

test "calculate_discounted_price" {
  // 测试折扣价格计算：折后价 = 原价 - 折扣金额
  let original_price = 200
  let discount_percentage = 15  // 15%折扣
  
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  assert_eq(170, discounted_price)  // 200 - (200 * 15 / 100) = 170
}

test "calculate_average_speed" {
  // 测试平均速度计算：速度 = 距离 ÷ 时间
  let distance = 300
  let time = 5
  
  let speed = distance / time
  assert_eq(60, speed)  // 300 ÷ 5 = 60 km/h
  
  // 计算多段路程的总平均速度
  let distance1 = 120
  let time1 = 2
  let distance2 = 180
  let time2 = 3
  
  let total_distance = add(distance1, distance2)
  let total_time = add(time1, time2)
  let average_speed = total_distance / total_time
  
  assert_eq(60, average_speed)  // (120 + 180) ÷ (2 + 3) = 300 ÷ 5 = 60
}