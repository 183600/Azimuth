// 标准 MoonBit 测试用例集合
// 这些测试用例使用标准的 MoonBit 测试语法，为 azimuth 模块提供额外的测试覆盖

// 标准 MoonBit 测试用例集合
// 这些测试用例使用标准的 MoonBit 测试语法，为 azimuth 模块提供额外的测试覆盖

test "arithmetical_sequence_sum" {
  // 计算等差数列的和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let common_diff = 3
  
  // 验证公差
  assert_eq(common_diff, add(4, -1))
  assert_eq(common_diff, add(7, -4))
  assert_eq(common_diff, add(10, -7))
  
  // 计算总和
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(35, sum)
}

test "geometric_sequence_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = multiply(term1, 2)  // 4
  let term3 = multiply(term2, 2)  // 8
  let term4 = multiply(term3, 2)  // 16
  
  // 计算积
  let product = multiply(multiply(term1, term2), multiply(term3, term4))
  assert_eq(1024, product)
}

test "discount_calculation" {
  // 折扣计算场景
  let original_price = 1000
  let discount_percent = 15
  let tax_percent = 8
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_percent) / 100
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(918, final_price)  // 1000 - 150 + 68 = 918
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  let speed = 60  // km/h
  let time = 2.5  // 小时（使用整数表示：250分钟/100）
  let time_minutes = 250
  
  // 计算距离（简化为整数计算）
  let distance = multiply(speed, time_minutes) / 100
  assert_eq(150, distance)  // 60 × 2.5 = 150 km
  
  // 验证时间计算：距离 / 速度
  let calculated_time = multiply(distance, 100) / speed
  assert_eq(time_minutes, calculated_time)
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  // 测试电子邮件格式（简化验证）
  let email1 = "user@example.com"
  let email2 = "test.email+tag@domain.co.uk"
  let invalid_email = "invalid.email"
  
  // 使用 greet 函数验证字符串处理
  assert_eq_string("Hello, user@example.com!", greet(email1))
  assert_eq_string("Hello, test.email+tag@domain.co.uk!", greet(email2))
  assert_eq_string("Hello, invalid.email!", greet(invalid_email))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
}

test "temperature_conversion_formula" {
  // 温度转换公式测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30（近似值）
  
  let celsius_values = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试冰点
  let freezing_f = add(multiply(0, 2), 30)
  assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_f = add(multiply(100, 2), 30)
  assert_eq(230, boiling_f)
  
  // 测试室温
  let room_c = 22
  let room_f = add(multiply(room_c, 2), 30)
  assert_eq(74, room_f)
}

test "simple_interest_calculation" {
  // 单利计算测试
  let principal = 5000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 计算利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(annual_rate, years))
  
  // 计算本息和
  let total_amount = add(principal, interest)
  
  assert_eq(170000, total_amount)  // 5000 + 5000×8×3 = 5000 + 120000 = 125000（简化表示）
}

test "array_manipulation_simulation" {
  // 数组操作模拟测试（使用整数运算模拟数组操作）
  // 模拟数组查找：在长度为10的数组中查找索引为3的元素
  
  let array_length = 10
  let target_index = 3
  let element_size = 4
  
  // 计算元素在内存中的偏移量
  let offset = multiply(target_index, element_size)
  assert_eq(12, offset)
  
  // 模拟二维数组访问：arr[2][3]
  let row = 2
  let col = 3
  let cols_per_row = 5
  
  let flat_index = add(multiply(row, cols_per_row), col)
  assert_eq(13, flat_index)
}

test "game_logic_simulation" {
  // 游戏逻辑模拟测试
  let player_level = 5
  let base_health = 100
  let health_per_level = 20
  let equipment_bonus = 15
  
  // 计算玩家总生命值
  let level_health = multiply(player_level, health_per_level)
  let total_health = add(add(base_health, level_health), equipment_bonus)
  assert_eq(215, total_health)
  
  // 计算伤害输出
  let base_damage = 10
  let weapon_damage = 25
  let skill_multiplier = 2
  
  let total_damage = multiply(add(base_damage, weapon_damage), skill_multiplier)
  assert_eq(70, total_damage)
  
  // 计算经验值需求
  let base_exp = 100
  let exp_multiplier = 15
  
  let exp_needed = multiply(base_exp, multiply(player_level, exp_multiplier))
  assert_eq(7500, exp_needed)
}

test "unit_conversion_practical" {
  // 实用单位转换测试
  // 长度转换：英寸转厘米（1英寸 = 2.54厘米，简化为2.5）
  let inches = 10
  let centimeters = multiply(inches, 25) / 10
  assert_eq(25, centimeters)
  
  // 重量转换：磅转千克（1磅 = 0.45千克，简化为0.5）
  let pounds = 20
  let kilograms = multiply(pounds, 5) / 10
  assert_eq(10, kilograms)
  
  // 容量转换：加仑转升（1加仑 = 3.78升，简化为4）
  let gallons = 5
  let liters = multiply(gallons, 4)
  assert_eq(20, liters)
}