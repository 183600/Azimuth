// Azimuth 项目新的综合测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "subtract_operations" {
  // 测试减法操作（通过加负数实现）
  @azimuth.assert_eq(@azimuth.add(10, -5), 5)
  @azimuth.assert_eq(@azimuth.add(20, -8), 12)
  @azimuth.assert_eq(@azimuth.add(-5, -3), -8)
  @azimuth.assert_eq(@azimuth.add(0, -7), -7)
}

test "division_operations" {
  // 测试除法操作（通过乘法倒数实现）
  // 简单的整数除法测试
  @azimuth.assert_eq(@azimuth.multiply(6, 1) / 2, 3)
  @azimuth.assert_eq(@azimuth.multiply(15, 1) / 3, 5)
  @azimuth.assert_eq(@azimuth.multiply(20, 1) / 4, 5)
  @azimuth.assert_eq(@azimuth.multiply(100, 1) / 10, 10)
}

test "exponentiation_basic" {
  // 测试基本指数运算（通过重复乘法实现）
  // 2^3 = 8
  let power_2_3 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)
  @azimuth.assert_eq(8, power_2_3)
  
  // 3^4 = 81
  let power_3_4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(3, 3), 3), 3)
  @azimuth.assert_eq(81, power_3_4)
  
  // 5^2 = 25
  let power_5_2 = @azimuth.multiply(5, 5)
  @azimuth.assert_eq(25, power_5_2)
}

test "factorial_calculations" {
  // 测试阶乘计算
  // 3! = 3 * 2 * 1 = 6
  let fact_3 = @azimuth.multiply(@azimuth.multiply(3, 2), 1)
  @azimuth.assert_eq(6, fact_3)
  
  // 4! = 4 * 3 * 2 * 1 = 24
  let fact_4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(4, 3), 2), 1)
  @azimuth.assert_eq(24, fact_4)
  
  // 5! = 5 * 4 * 3 * 2 * 1 = 120
  let fact_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  @azimuth.assert_eq(120, fact_5)
}

test "greet_with_numbers_and_symbols" {
  // 测试包含数字和符号的问候
  @azimuth.assert_eq_string(@azimuth.greet("User123"), "Hello, User123!")
  @azimuth.assert_eq_string(@azimuth.greet("test@example.com"), "Hello, test@example.com!")
  @azimuth.assert_eq_string(@azimuth.greet("12345"), "Hello, 12345!")
  @azimuth.assert_eq_string(@azimuth.greet("a+b-c"), "Hello, a+b-c!")
}

test "calculate_distance" {
  // 测试距离计算（简化的欧几里得距离）
  // 点 (x1, y1) 和 (x2, y2) 之间的距离
  let x1 = 0
  let y1 = 0
  let x2 = 3
  let y2 = 4
  
  // 简化版本：|x2 - x1| + |y2 - y1| (曼哈顿距离)
  let dx = @azimuth.add(x2, -x1)
  let dy = @azimuth.add(y2, -y1)
  let distance = @azimuth.add(dx, dy)
  @azimuth.assert_eq(7, distance)
  
  // 另一个例子
  let x1_2 = 1
  let y1_2 = 2
  let x2_2 = 4
  let y2_2 = 6
  
  let dx_2 = @azimuth.add(x2_2, -x1_2)
  let dy_2 = @azimuth.add(y2_2, -y1_2)
  let distance_2 = @azimuth.add(dx_2, dy_2)
  @azimuth.assert_eq(7, distance_2)
}

test "calculate_simple_statistics" {
  // 测试简单统计计算
  let numbers = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let mean = numbers / count
  @azimuth.assert_eq(30, mean)
  
  // 计算方差（简化版本）
  let diff1 = @azimuth.add(10, -mean)
  let diff2 = @azimuth.add(20, -mean)
  let diff3 = @azimuth.add(30, -mean)
  let diff4 = @azimuth.add(40, -mean)
  let diff5 = @azimuth.add(50, -mean)
  
  let squared_diffs = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), 
    @azimuth.multiply(diff2, diff2)), 
    @azimuth.multiply(diff3, diff3)), 
    @azimuth.multiply(diff4, diff4)), 
    @azimuth.multiply(diff5, diff5))
  
  let variance = squared_diffs / count
  @azimuth.assert_eq(200, variance)
}

test "calculate_velocity" {
  // 测试速度计算
  let distance = 100  // 米
  let time = 10       // 秒
  let velocity = distance / time
  @azimuth.assert_eq(10, velocity)
  
  // 测试不同单位
  let distance_km = 5    // 千米
  let time_hr = 0.5      // 小时
  let velocity_kmh = distance_km / time_hr
  @azimuth.assert_eq(10, velocity_kmh)
}

test "calculate_area_and_perimeter" {
  // 测试面积和周长计算
  // 圆的面积（简化版本，使用整数近似）
  let radius = 5
  let pi_approx = 3  // 简化的π值
  let circle_area = @azimuth.multiply(@azimuth.multiply(radius, radius), pi_approx)
  @azimuth.assert_eq(75, circle_area)
  
  // 圆的周长（简化版本）
  let circle_circumference = @azimuth.multiply(@azimuth.multiply(2, radius), pi_approx)
  @azimuth.assert_eq(30, circle_circumference)
  
  // 梯形面积：(上底 + 下底) * 高 / 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(32, trapezoid_area)
}

test "currency_conversion" {
  // 测试货币转换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY（示例汇率）
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 反向转换
  let cny_amount_2 = 1400
  let usd_amount_2 = cny_amount_2 / exchange_rate
  @azimuth.assert_eq(200, usd_amount_2)
  
  // 带手续费的转换
  let fee_rate = 2  // 2% 手续费
  let fee = @azimuth.multiply(usd_amount, fee_rate) / 100
  let amount_after_fee = @azimuth.add(usd_amount, -fee)
  let final_cny = @azimuth.multiply(amount_after_fee, exchange_rate)
  @azimuth.assert_eq(686, final_cny)
}