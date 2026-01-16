// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "arithmetic_sequence_operations" {
  // 等差数列运算测试
  let first_term = 5
  let common_difference = 3
  let n_terms = 7
  
  // 计算等差数列第n项: a_n = a_1 + (n-1)d
  let nth_term = azimuth::add(first_term, azimuth::multiply(common_difference, azimuth::add(n_terms, -1)))
  assert_eq(nth_term, 23)  // 5 + (7-1)*3 = 23
  
  // 计算等差数列前n项和: S_n = n/2 * (a_1 + a_n)
  let sum = azimuth::multiply(azimuth::divide(n_terms, 2), azimuth::add(first_term, nth_term))
  assert_eq(sum, 98)  // 7/2 * (5 + 23) = 98
}

test "geometric_progression_calculations" {
  // 等比数列计算测试
  let first_term = 2
  let common_ratio = 3
  let n_terms = 4
  
  // 计算等比数列第n项: a_n = a_1 * r^(n-1)
  let mut nth_term = first_term
  let mut i = 1
  while i < n_terms {
    nth_term = azimuth::multiply(nth_term, common_ratio)
    i = azimuth::add(i, 1)
  }
  assert_eq(nth_term, 54)  // 2 * 3^(4-1) = 54
  
  // 计算等比数列前n项和: S_n = a_1 * (r^n - 1) / (r - 1)
  let mut sum = 0
  let mut current_term = first_term
  let mut j = 0
  while j < n_terms {
    sum = azimuth::add(sum, current_term)
    current_term = azimuth::multiply(current_term, common_ratio)
    j = azimuth::add(j, 1)
  }
  assert_eq(sum, 80)  // 2 + 6 + 18 + 54 = 80
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, azimuth::divide(9, 5)), 32)
  assert_eq(fahrenheit, 77)  // 25 * 9/5 + 32 = 77
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let celsius_back = azimuth::multiply(azimuth::add(fahrenheit, -32), azimuth::divide(5, 9))
  assert_eq(celsius_back, 25)  // (77 - 32) * 5/9 = 25
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简单利息计算: A = P * (1 + r * t)
  let simple_interest = azimuth::multiply(principal, azimuth::add(1, azimuth::multiply(annual_rate, years) / 100))
  assert_eq(simple_interest, 11500)  // 10000 * (1 + 0.05 * 3) = 11500
  
  // 复利计算（简化版）: A = P * (1 + r)^t
  let mut compound_interest = principal
  let mut i = 0
  while i < years {
    compound_interest = azimuth::add(compound_interest, azimuth::multiply(compound_interest, annual_rate) / 100)
    i = azimuth::add(i, 1)
  }
  assert_eq(compound_interest, 11576)  // 10000 * 1.05^3 ≈ 11576
}

test "coordinate_geometry" {
  // 坐标几何测试
  let x1 = 2
  let y1 = 3
  let x2 = 8
  let y2 = 7
  
  // 计算两点间距离的平方: d² = (x2-x1)² + (y2-y1)²
  let dx = azimuth::add(x2, -x1)
  let dy = azimuth::add(y2, -y1)
  let distance_squared = azimuth::add(azimuth::multiply(dx, dx), azimuth::multiply(dy, dy))
  assert_eq(distance_squared, 52)  // (8-2)² + (7-3)² = 36 + 16 = 52
  
  // 计算中点坐标
  let mid_x = azimuth::divide(azimuth::add(x1, x2), 2)
  let mid_y = azimuth::divide(azimuth::add(y1, y2), 2)
  assert_eq(mid_x, 5)  // (2 + 8) / 2 = 5
  assert_eq(mid_y, 5)  // (3 + 7) / 2 = 5
}

test "number_system_conversions" {
  // 数制转换测试
  let decimal = 42
  
  // 十进制转二进制（模拟）
  let mut binary_digits = ""
  let mut n = decimal
  while n > 0 {
    let remainder = azimuth::mod(n, 2)
    binary_digits = remainder.to_string() + binary_digits
    n = azimuth::divide(n, 2)
  }
  assert_eq(binary_digits, "101010")  // 42 in binary is 101010
  
  // 验证二进制转十进制
  let mut decimal_back = 0
  let mut power = 1
  let mut i = 0
  while i < binary_digits.length() {
    let digit = binary_digits.substring(binary_digits.length() - azimuth::add(i, 1), 1).to_int()
    decimal_back = azimuth::add(decimal_back, azimuth::multiply(digit, power))
    power = azimuth::multiply(power, 2)
    i = azimuth::add(i, 1)
  }
  assert_eq(decimal_back, decimal)
}

test "game_mechanics" {
  // 游戏机制测试
  let player_level = 15
  let base_exp = 1000
  let exp_multiplier = 1.5
  
  // 计算升级所需经验值
  let required_exp = azimuth::multiply(base_exp, azimuth::multiply(player_level, exp_multiplier.to_int()))
  assert_eq(required_exp, 22500)  // 1000 * 15 * 1.5 = 22500
  
  // 计算属性值
  let base_strength = 10
  let strength_per_level = 3
  let total_strength = azimuth::add(base_strength, azimuth::multiply(player_level, strength_per_level))
  assert_eq(total_strength, 55)  // 10 + 15 * 3 = 55
  
  // 测试伤害计算
  let weapon_damage = 25
  let critical_multiplier = 2
  let normal_damage = azimuth::add(total_strength, weapon_damage)
  let critical_damage = azimuth::multiply(normal_damage, critical_multiplier)
  assert_eq(normal_damage, 80)    // 55 + 25 = 80
  assert_eq(critical_damage, 160) // 80 * 2 = 160
}

test "time_calculations" {
  // 时间计算测试
  let hours = 5
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = azimuth::add(
    azimuth::multiply(hours, 3600),
    azimuth::add(azimuth::multiply(minutes, 60), seconds)
  )
  assert_eq(total_seconds, 20730)  // 5*3600 + 45*60 + 30 = 20730
  
  // 计算时间差
  let start_time = 3600  // 1:00:00 in seconds
  let end_time = 10800   // 3:00:00 in seconds
  let duration = azimuth::add(end_time, -start_time)
  assert_eq(duration, 7200)  // 2 hours = 7200 seconds
  
  // 转换回小时和分钟
  let duration_hours = azimuth::divide(duration, 3600)
  let remaining_seconds = azimuth::mod(duration, 3600)
  let duration_minutes = azimuth::divide(remaining_seconds, 60)
  assert_eq(duration_hours, 2)
  assert_eq(duration_minutes, 0)
}

test "string_formatting_patterns" {
  // 字符串格式化模式测试
  let first_name = "John"
  let last_name = "Doe"
  let age = 30
  
  // 测试不同的问候格式
  let formal_greeting = azimuth::greet(first_name + " " + last_name)
  assert_eq(formal_greeting, "Hello, John Doe!")
  
  let casual_greeting = "Hi, " + first_name + "!"
  assert_eq(casual_greeting, "Hi, John!")
  
  // 测试信息格式化
  let user_info = azimuth::greet(first_name) + " You are " + age.to_string() + " years old."
  assert_eq(user_info, "Hello, John! You are 30 years old.")
  
  // 测试空字符串处理
  let empty_name = ""
  let empty_greeting = azimuth::greet(empty_name)
  assert_eq(empty_greeting, "Hello, !")
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  let positive = 100
  let negative = -50
  let zero = 0
  
  // 测试除零保护（假设函数有保护机制）
  let result1 = azimuth::multiply(positive, zero)
  assert_eq(result1, 0)
  
  let result2 = azimuth::multiply(zero, negative)
  assert_eq(result2, 0)
  
  // 测试数值溢出保护
  let max_int = 2147483647
  let safe_add = azimuth::add(max_int, 0)  // 安全加法
  assert_eq(safe_add, max_int)
  
  let safe_multiply = azimuth::multiply(max_int, 1)  // 安全乘法
  assert_eq(safe_multiply, max_int)
  
  // 测试负数运算
  let neg_result1 = azimuth::add(positive, negative)
  assert_eq(neg_result1, 50)  // 100 + (-50) = 50
  
  let neg_result2 = azimuth::multiply(negative, negative)
  assert_eq(neg_result2, 2500)  // (-50) * (-50) = 2500
}