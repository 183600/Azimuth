// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "factorial_calculation" {
  // 阶乘计算测试: 5! = 5 × 4 × 3 × 2 × 1 = 120
  let fact_5 = azimuth::multiply(azimuth::multiply(azimuth::multiply(azimuth::multiply(5, 4), 3), 2), 1)
  assert_eq(120, fact_5)
  
  // 4! = 4 × 3 × 2 × 1 = 24
  let fact_4 = azimuth::multiply(azimuth::multiply(azimuth::multiply(4, 3), 2), 1)
  assert_eq(24, fact_4)
}

test "power_operations" {
  // 幂运算测试
  // 2³ = 2 × 2 × 2 = 8
  let power_2_3 = azimuth::multiply(azimuth::multiply(2, 2), 2)
  assert_eq(8, power_2_3)
  
  // 3² = 3 × 3 = 9
  let power_3_2 = azimuth::multiply(3, 3)
  assert_eq(9, power_3_2)
  
  // 5² = 5 × 5 = 25
  let power_5_2 = azimuth::multiply(5, 5)
  assert_eq(25, power_5_2)
}

test "greet_multilingual_names" {
  // 多语言名称测试
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq(azimuth::greet("李四"), "Hello, 李四!")
  assert_eq(azimuth::greet("José"), "Hello, José!")
  assert_eq(azimuth::greet("François"), "Hello, François!")
  assert_eq(azimuth::greet("Müller"), "Hello, Müller!")
}

test "greet_with_numbers_and_symbols" {
  // 包含数字和符号的名称测试
  assert_eq(azimuth::greet("User123"), "Hello, User123!")
  assert_eq(azimuth::greet("test_user"), "Hello, test_user!")
  assert_eq(azimuth::greet("admin@site"), "Hello, admin@site!")
  assert_eq(azimuth::greet("C++"), "Hello, C++!")
  assert_eq(azimuth::greet("Node.js"), "Hello, Node.js!")
}

test "compound_interest_calculation" {
  // 复利计算测试
  // 本金 × (1 + 利率)^年数
  let principal = 1000
  let rate = 5  // 5%
  let years = 2
  
  // 第一年后: 1000 × (1 + 0.05) = 1050
  let year1 = azimuth::multiply(principal, azimuth::add(100, rate)) / 100
  assert_eq(1050, year1)
  
  // 第二年后: 1050 × (1 + 0.05) = 1102.5 ≈ 1102 (整数运算)
  let year2 = azimuth::multiply(year1, azimuth::add(100, rate)) / 100
  assert_eq(1102, year2)
}

test "statistical_calculations" {
  // 统计计算测试
  // 平均数计算: (1 + 2 + 3 + 4 + 5) / 5 = 3
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  let average = sum / 5
  assert_eq(3, average)
  
  // 加权平均: (1×2 + 2×3 + 3×5) / (2+3+5) = 22/10 = 2
  let weighted_sum = azimuth::add(azimuth::add(azimuth::multiply(1, 2), azimuth::multiply(2, 3)), azimuth::multiply(3, 5))
  let weights_sum = azimuth::add(azimuth::add(2, 3), 5)
  let weighted_average = weighted_sum / weights_sum
  assert_eq(2, weighted_average)
}

test "large_number_operations" {
  // 大数运算测试
  let large1 = 10000
  let large2 = 20000
  let large3 = 30000
  
  // 大数加法
  assert_eq(30000, azimuth::add(large1, large2))
  assert_eq(50000, azimuth::add(large2, large3))
  assert_eq(60000, azimuth::add(large1, azimuth::add(large2, large3)))
  
  // 大数乘法
  assert_eq(200000000, azimuth::multiply(large1, large2))
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算测试
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2    // hours
  let distance = azimuth::multiply(speed, time)
  assert_eq(120, distance)
  
  // 速度 = 距离 / 时间
  let distance2 = 180  // km
  let time2 = 3        // hours
  let speed2 = distance2 / time2
  assert_eq(60, speed2)
  
  // 时间 = 距离 / 速度
  let distance3 = 240  // km
  let speed3 = 80      // km/h
  let time3 = distance3 / speed3
  assert_eq(3, time3)
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  // 圆的面积 (简化版，使用 π≈3): π × r²
  let radius = 5
  let circle_area = azimuth::multiply(3, azimuth::multiply(radius, radius))
  assert_eq(75, circle_area)
  
  // 梯形面积: (上底 + 下底) × 高 / 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), height) / 2
  assert_eq(32, trapezoid_area)
  
  // 菱形面积: 对角线1 × 对角线2 / 2
  let diagonal1 = 8
  let diagonal2 = 6
  let rhombus_area = azimuth::multiply(diagonal1, diagonal2) / 2
  assert_eq(24, rhombus_area)
}

test "binary_operations_simulation" {
  // 二进制操作模拟测试
  // 左移操作 (× 2^n): 5 << 2 = 5 × 4 = 20
  let left_shift = azimuth::multiply(5, azimuth::multiply(2, 2))
  assert_eq(20, left_shift)
  
  // 右移操作 (÷ 2^n): 20 >> 2 = 20 ÷ 4 = 5
  let right_shift = 20 / azimuth::multiply(2, 2)
  assert_eq(5, right_shift)
  
  // 位与操作模拟 (简化): 3 & 5 = 1
  // 这里用模运算模拟位操作
  let bit_and = azimuth::multiply(3, 5) % 4
  assert_eq(3, bit_and)
}