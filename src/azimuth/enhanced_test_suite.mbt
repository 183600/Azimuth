// 增强测试套件 - Azimuth 项目
// 包含9个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖各种功能点和边界情况

test "advanced_arithmetic_sequences" {
  // 高级算术序列测试
  let first_term = 3
  let common_diff = 5
  let n_terms = 6
  
  // 计算等差数列前n项和: S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(multiply(n_terms, add(multiply(2, first_term), multiply(subtract(n_terms, 1), common_diff))), 2)
  
  // 手动计算验证
  let manual_sum = add(add(add(add(add(3, 8), 13), 18), 23), 28)
  
  assert_eq(sum_formula, manual_sum)
  assert_eq(93, manual_sum)
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 1000
  let rate_percent = 5
  let time_years = 3
  
  // 简单利息计算: I = P * R * T
  let simple_interest = divide_with_ceil(multiply(multiply(principal, rate_percent), time_years), 100)
  
  // 复利计算（简化版本）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  let compound_amount = year3
  
  assert_eq(150, simple_interest)
  assert_eq(1158, compound_amount)
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 120
  let daily_sales = 15
  let days = 5
  let restock_batch = 80
  
  // 计算剩余库存
  let total_sold = multiply(daily_sales, days)
  let remaining = subtract(initial_stock, total_sold)
  
  // 计算需要补货的批次
  let restock_needed = divide_with_ceil(multiply(-1, remaining), restock_batch)
  
  assert_eq(75, total_sold)
  assert_eq(45, remaining)
  assert_eq(1, restock_needed)
}

test "temperature_conversions" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度: F = (C * 9/5) + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  assert_eq(77, fahrenheit)
  assert_eq(25, celsius_back)
}

test "combinatorial_mathematics" {
  // 组合数学测试
  let n = 10
  let r = 3
  
  // 计算排列: P(n,r) = n! / (n-r)!
  let factorial_n = multiply(multiply(multiply(multiply(multiply(multiply(multiply(multiply(multiply(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  let factorial_n_minus_r = multiply(multiply(multiply(multiply(multiply(multiply(1, 2), 3), 4), 5), 6), 7)
  let permutation = divide_with_ceil(factorial_n, factorial_n_minus_r)
  
  assert_eq(720, permutation)
}

test "signal_processing" {
  // 信号处理测试
  let signal_values = [10, 15, 12, 18, 20, 14, 16]
  
  // 计算信号平均值
  let sum_values = add(add(add(add(add(add(10, 15), 12), 18), 20), 14), 16)
  let average = divide_with_ceil(sum_values, 7)
  
  // 计算信号范围
  let max_val = 20
  let min_val = 10
  let range = subtract(max_val, min_val)
  
  assert_eq(105, sum_values)
  assert_eq(15, average)
  assert_eq(10, range)
}

test "geometric_progressions" {
  // 几何级数测试
  let first_term = 2
  let common_ratio = 3
  let n_terms = 5
  
  // 计算几何级数前n项和: S = a * (r^n - 1) / (r - 1)
  let r_power_n = multiply(multiply(multiply(multiply(1, common_ratio), common_ratio), common_ratio), common_ratio)
  let sum_geometric = divide_with_ceil(multiply(first_term, subtract(r_power_n, 1)), subtract(common_ratio, 1))
  
  // 手动计算验证
  let manual_sum = add(add(add(add(2, 6), 18), 54), 162)
  
  assert_eq(242, sum_geometric)
  assert_eq(242, manual_sum)
}

test "cryptography_basics" {
  // 密码学基础测试
  let plaintext = 42
  let key_a = 7
  let key_b = 11
  let modulus = 100
  
  // 简单加密: E = (a * P + b) mod m
  let encrypted_value = add(multiply(plaintext, key_a), key_b) % modulus
  
  // 简单解密（简化版本）
  let decrypted_approx = divide_with_ceil(subtract(encrypted_value, key_b), key_a)
  
  assert_eq(305, add(multiply(42, 7), 11))
  assert_eq(5, encrypted_value)
  assert_eq(-1, decrypted_approx)
}

test "physics_kinematics" {
  // 物理运动学测试
  let initial_position = 10
  let initial_velocity = 5
  let acceleration = 3
  let time = 4
  
  // 计算最终位置: s = s0 + v0*t + 0.5*a*t^2
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  let final_position = add(initial_position, displacement)
  
  // 计算平均速度
  let average_velocity = divide_with_ceil(displacement, time)
  
  assert_eq(32, displacement)
  assert_eq(42, final_position)
  assert_eq(8, average_velocity)
}