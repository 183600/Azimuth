// Azimuth 项目额外的 MoonBit 测试用例
// 覆盖更多实际应用场景和边界情况

test "arithmetic_progression_sum" {
  // 计算等差数列的和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(35, sum)
}

test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  let product = @azimuth.multiply(@azimuth.multiply(term1, term2), @azimuth.multiply(term3, term4))
  @azimuth.assert_eq(1024, product)
}

test "temperature_conversion_detailed" {
  // 详细的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30（近似值）
  
  let freezing_celsius = 0
  let boiling_celsius = 100
  let room_celsius = 20
  
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  
  @azimuth.assert_eq(30, freezing_fahrenheit)    // 0°C ≈ 30°F
  @azimuth.assert_eq(230, boiling_fahrenheit)    // 100°C ≈ 230°F
  @azimuth.assert_eq(70, room_fahrenheit)        // 20°C ≈ 70°F
}

test "discount_calculation" {
  // 折扣计算测试
  let original_price = 1000
  let discount_percentage = 15  // 15% 折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(150, discount_amount)
  @azimuth.assert_eq(850, discounted_price)
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  let speed = 60  // km/h
  let time = 2.5  // 小时，用整数表示为 25/10
  
  // 计算距离：距离 = 速度 × 时间
  // 使用整数计算：60 × 25 / 10 = 150
  let distance = @azimuth.multiply(speed, 25) / 10
  
  @azimuth.assert_eq(150, distance)  // 150 km
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  let rectangle_length = 8
  let rectangle_width = 5
  let circle_radius = 10
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  // 矩形周长
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(rectangle_length, rectangle_width))
  
  // 圆面积（简化：π ≈ 3）
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(circle_radius, circle_radius))
  // 圆周长（简化：π ≈ 3）
  let circle_circumference = @azimuth.multiply(2, @azimuth.multiply(3, circle_radius))
  
  @azimuth.assert_eq(40, rectangle_area)
  @azimuth.assert_eq(26, rectangle_perimeter)
  @azimuth.assert_eq(300, circle_area)
  @azimuth.assert_eq(60, circle_circumference)
}

test "string_validation_scenarios" {
  // 字符串验证场景测试
  let email = "user@example.com"
  let phone = "123-456-7890"
  let username = "user123"
  let empty_string = ""
  
  // 测试各种字符串类型的greet函数处理
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet(phone))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet(username))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_string))
}

test "binary_number_simulation" {
  // 二进制数运算模拟（使用十进制表示）
  let binary1 = 5  // 二进制 101
  let binary2 = 3  // 二进制 011
  
  // 模拟二进制与运算：101 & 011 = 001 (1)
  let bitwise_and = 1
  
  // 模拟二进制或运算：101 | 011 = 111 (7)
  let bitwise_or = @azimuth.add(binary1, binary2)
  
  // 模拟二进制异或运算：101 ^ 011 = 110 (6)
  let bitwise_xor = @azimuth.add(binary1, -binary2)
  
  @azimuth.assert_eq(1, bitwise_and)
  @azimuth.assert_eq(8, bitwise_or)
  @azimuth.assert_eq(2, bitwise_xor)
}

test "loan_amortization" {
  // 贷款分期计算测试
  let principal = 12000  // 贷款本金
  let annual_rate = 6    // 年利率百分比
  let months = 12        // 分期数
  
  // 简化的月供计算：月供 = (本金 + 总利息) / 分期数
  // 总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, 1)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(720, total_interest)
  @azimuth.assert_eq(12720, total_payment)
  @azimuth.assert_eq(1060, monthly_payment)
}

test "combinatorics_basic" {
  // 基础组合数学测试
  let n = 5  // 5个元素
  let r = 3  // 选择3个
  
  // 计算排列数：P(n,r) = n × (n-1) × ... × (n-r+1)
  let permutation = @azimuth.multiply(n, @azimuth.multiply(n - 1, n - 2))
  
  // 计算组合数：C(n,r) = P(n,r) / r!
  // r! = 3 × 2 × 1 = 6
  let r_factorial = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  let combination = permutation / r_factorial
  
  @azimuth.assert_eq(60, permutation)  // P(5,3) = 5 × 4 × 3 = 60
  @azimuth.assert_eq(10, combination)  // C(5,3) = 60 / 6 = 10
}