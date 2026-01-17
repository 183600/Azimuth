// Azimuth 项目创意测试用例
// 使用标准 MoonBit 测试语法，提供实用的测试场景

test "number_theory_properties" {
  // 测试数论性质
  
  // 测试偶数性质：偶数加偶数等于偶数
  let even1 = 4
  let even2 = 6
  let sum_even = azimuth::add(even1, even2)
  assert_eq(sum_even % 2, 0)  // 结果应该是偶数
  
  // 测试奇数性质：奇数加奇数等于偶数
  let odd1 = 3
  let odd2 = 5
  let sum_odd = azimuth::add(odd1, odd2)
  assert_eq(sum_odd % 2, 0)  // 结果应该是偶数
  
  // 测试乘法性质：任何数乘以0等于0
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(-17, 0), 0)
}

test "physical_unit_conversions" {
  // 物理单位转换测试
  
  // 公里转米：1公里 = 1000米
  let kilometers = 5
  let meters = azimuth::multiply(kilometers, 1000)
  assert_eq(5000, meters)
  
  // 米转厘米：1米 = 100厘米
  let meters_value = 3
  let centimeters = azimuth::multiply(meters_value, 100)
  assert_eq(300, centimeters)
  
  // 千克转克：1千克 = 1000克
  let kilograms = 2
  let grams = azimuth::multiply(kilograms, 1000)
  assert_eq(2000, grams)
}

test "calendar_calculations" {
  // 日历计算测试
  
  // 计算一年中的总天数（非闰年）
  let days_in_year = azimuth::add(azimuth::add(azimuth::add(31, 28), 31), 30)  // 前4个月
  let remaining_days = azimuth::add(azimuth::add(azimuth::add(31, 30), 31), 31)  // 5-8月
  let total_days = azimuth::add(days_in_year, remaining_days)
  assert_eq(242, total_days)  // 前8个月的总天数
  
  // 计算一周的工作小时数
  let daily_work_hours = 8
  let work_days_per_week = 5
  let weekly_work_hours = azimuth::multiply(daily_work_hours, work_days_per_week)
  assert_eq(40, weekly_work_hours)
}

test "statistical_operations" {
  // 统计操作测试
  
  // 计算平均数：(10 + 20 + 30) / 3 = 20
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let sum = azimuth::add(azimuth::add(num1, num2), num3)
  let average = sum / 3
  assert_eq(20, average)
  
  // 计算加权平均：10*0.3 + 20*0.7 = 3 + 14 = 17
  let value1 = 10
  let weight1 = 3  // 0.3 * 10
  let value2 = 20
  let weight2 = 7  // 0.7 * 10
  let weighted_sum = azimuth::add(azimuth::multiply(value1, weight1), azimuth::multiply(value2, weight2))
  let weighted_avg = weighted_sum / 10
  assert_eq(17, weighted_avg)
}

test "binary_operations_simulation" {
  // 二进制操作模拟测试
  
  // 模拟左移操作（乘以2的幂）
  let original = 5
  let shifted_left_by_1 = azimuth::multiply(original, 2)  // 5 * 2 = 10
  let shifted_left_by_2 = azimuth::multiply(original, 4)  // 5 * 4 = 20
  let shifted_left_by_3 = azimuth::multiply(original, 8)  // 5 * 8 = 40
  
  assert_eq(10, shifted_left_by_1)
  assert_eq(20, shifted_left_by_2)
  assert_eq(40, shifted_left_by_3)
}

test "pattern_based_calculations" {
  // 基于模式的计算测试
  
  // 计算连续整数的平方和：1² + 2² + 3² = 1 + 4 + 9 = 14
  let n1 = 1
  let n2 = 2
  let n3 = 3
  let square_sum = azimuth::add(azimuth::add(azimuth::multiply(n1, n1), azimuth::multiply(n2, n2)), azimuth::multiply(n3, n3))
  assert_eq(14, square_sum)
  
  // 计算等差数列的和：3 + 6 + 9 + 12 = 30
  let term1 = 3
  let term2 = 6
  let term3 = 9
  let term4 = 12
  let arithmetic_sum = azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4)
  assert_eq(30, arithmetic_sum)
}

test "greeting_variations" {
  // 问候语变体测试
  
  // 测试不同长度的名字
  assert_eq(azimuth::greet("A"), "Hello, A!")
  assert_eq(azimuth::greet("AB"), "Hello, AB!")
  assert_eq(azimuth::greet("ABC"), "Hello, ABC!")
  
  // 测试包含特殊字符的名字
  assert_eq(azimuth::greet("John-Doe"), "Hello, John-Doe!")
  assert_eq(azimuth::greet("O'Connor"), "Hello, O'Connor!")
  
  // 测试数字名字
  assert_eq(azimuth::greet("007"), "Hello, 007!")
  assert_eq(azimuth::greet("12345"), "Hello, 12345!")
}

test "edge_case_mathematics" {
  // 边界情况数学测试
  
  // 测试大数运算
  let large_num1 = 10000
  let large_num2 = 20000
  assert_eq(azimuth::add(large_num1, large_num2), 30000)
  assert_eq(azimuth::multiply(large_num1, large_num2), 200000000)
  
  // 测试负数运算
  let neg1 = -10
  let neg2 = -20
  assert_eq(azimuth::add(neg1, neg2), -30)
  assert_eq(azimuth::multiply(neg1, neg2), 200)
  
  // 测试混合符号运算
  assert_eq(azimuth::add(neg1, large_num1), 9990)
  assert_eq(azimuth::multiply(neg1, large_num1), -100000)
}

test "real_world_scenarios" {
  // 现实世界场景测试
  
  // 购物车总价计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  
  let item1_total = azimuth::multiply(item1_price, item1_quantity)
  let item2_total = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(item1_total, item2_total)
  
  assert_eq(50, item1_total)
  assert_eq(45, item2_total)
  assert_eq(95, cart_total)
  
  // 速度、距离、时间计算：距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // hours (模拟为整数计算：60 * 25 / 10)
  let distance = azimuth::multiply(speed, 25) / 10
  assert_eq(150, distance)
}