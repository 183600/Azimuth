// 增强的 MoonBit 测试用例
// 包含各种边界条件和实际应用场景的测试

test "arithmetic_sequence_calculation" {
  // 计算等差数列的第n项：an = a1 + (n-1)d
  let first_term = 3
  let common_diff = 4
  let n = 10
  
  // 第10项 = 3 + (10-1)*4 = 3 + 36 = 39
  let nth_term = add(first_term, multiply(common_diff, n - 1))
  assert_eq(39, nth_term)
}

test "geometric_progression_sum" {
  // 计算等比数列前n项和：S_n = a1*(1-r^n)/(1-r)
  // 简化为整数计算：S_5 = 2*(1-3^5)/(1-3) = 2*(1-243)/(-2) = 242
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 手动计算：2 + 6 + 18 + 54 + 162 = 242
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30（近似值）
  let celsius_freezing = 0      // 冰点
  let celsius_boiling = 100     // 沸点
  let celsius_room = 20         // 室温
  let celsius_body = 37         // 体温
  
  // 转换公式（简化版）
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  let fahrenheit_boiling = add(multiply(celsius_boiling, 2), 30)
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  let fahrenheit_body = add(multiply(celsius_body, 2), 30)
  
  assert_eq(30, fahrenheit_freezing)   // 0°C → 30°F（近似）
  assert_eq(230, fahrenheit_boiling)   // 100°C → 230°F（近似）
  assert_eq(70, fahrenheit_room)       // 20°C → 70°F（近似）
  assert_eq(104, fahrenheit_body)      // 37°C → 104°F（近似）
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 计算利息：10000 × 5 × 3 = 150000
  let interest = multiply(principal, multiply(annual_rate, years))
  
  // 计算本息和：本金 + 利息
  let total_amount = add(principal, interest)
  
  assert_eq(150000, interest)
  assert_eq(160000, total_amount)
}

test "discount_price_calculation" {
  // 折扣价格计算：折后价 = 原价 × (1 - 折扣率)
  let original_price = 200
  let discount_percent = 15  // 15% 折扣
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  
  assert_eq(30, discount_amount)
  assert_eq(170, discounted_price)
}

test "basketball_score_calculation" {
  // 篮球得分计算
  let two_pointers = 12
  let three_pointers = 5
  let free_throws = 8
  
  // 计算各类得分
  let two_point_score = multiply(two_pointers, 2)
  let three_point_score = multiply(three_pointers, 3)
  let free_throw_score = free_throws
  
  // 计算总分
  let total_score = add(add(two_point_score, three_point_score), free_throw_score)
  
  assert_eq(24, two_point_score)
  assert_eq(15, three_point_score)
  assert_eq(8, free_throw_score)
  assert_eq(47, total_score)
}

test "digital_root_calculation" {
  // 数字根计算（简化版）：对各位数字求和直到得到一位数
  // 对于数字 12345：1+2+3+4+5 = 15 → 1+5 = 6
  let number = 12345
  
  // 分解数字并求和
  let digit1 = number / 10000        // 1
  let digit2 = (number / 1000) % 10  // 2
  let digit3 = (number / 100) % 10   // 3
  let digit4 = (number / 10) % 10    // 4
  let digit5 = number % 10           // 5
  
  let first_sum = add(add(add(add(digit1, digit2), digit3), digit4), digit5)
  // 15 → 1 + 5 = 6
  let final_sum = add(first_sum / 10, first_sum % 10)
  
  assert_eq(15, first_sum)
  assert_eq(6, final_sum)
}

test "perimeter_calculation" {
  // 周长计算测试
  // 正方形周长
  let square_side = 8
  let square_perimeter = multiply(4, square_side)
  
  // 长方形周长
  let rectangle_length = 12
  let rectangle_width = 7
  let rectangle_perimeter = add(multiply(2, rectangle_length), multiply(2, rectangle_width))
  
  // 三角形周长
  let triangle_side1 = 5
  let triangle_side2 = 7
  let triangle_side3 = 9
  let triangle_perimeter = add(add(triangle_side1, triangle_side2), triangle_side3)
  
  assert_eq(32, square_perimeter)
  assert_eq(38, rectangle_perimeter)
  assert_eq(21, triangle_perimeter)
}

test "volume_calculation" {
  // 体积计算测试（简化公式）
  // 立方体体积：边长³
  let cube_side = 4
  let cube_volume = multiply(multiply(cube_side, cube_side), cube_side)
  
  // 长方体体积：长×宽×高
  let box_length = 6
  let box_width = 4
  let box_height = 3
  let box_volume = multiply(multiply(box_length, box_width), box_height)
  
  // 圆柱体积（简化）：底面积×高，其中底面积≈3×半径²
  let cylinder_radius = 5
  let cylinder_height = 8
  let cylinder_base_area = multiply(3, multiply(cylinder_radius, cylinder_radius))
  let cylinder_volume = multiply(cylinder_base_area, cylinder_height)
  
  assert_eq(64, cube_volume)
  assert_eq(72, box_volume)
  assert_eq(600, cylinder_volume)
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间关系测试
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时，用整数表示为 25/10
  let distance = multiply(speed, multiply(25, 1)) / 10
  
  // 时间 = 距离 / 速度
  let distance2 = 150  // km
  let time2 = distance2 / speed
  
  // 速度 = 距离 / 时间
  let distance3 = 180  // km
  let time3 = 3        // 小时
  let speed3 = distance3 / time3
  
  assert_eq(150, distance)
  assert_eq(2, time2)
  assert_eq(60, speed3)
}