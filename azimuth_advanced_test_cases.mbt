// 新增 MoonBit 测试用例
// 针对 azimuth 模块的高级测试

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  
  // 正数溢出测试
  assert_eq(max_val, azimuth::add(2000000000, 147483647))
  assert_eq(max_val, azimuth::add(1500000000, 647483647))
  
  // 负数溢出测试
  let min_val = -2147483648
  assert_eq(min_val, azimuth::add(-2000000000, -147483648))
  assert_eq(min_val, azimuth::add(-1500000000, -647483648))
}

test "add_edge_cases" {
  // 测试加法边界情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值加0
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(min_val, azimuth::add(0, min_val))
  
  // 最大值加0
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(max_val, azimuth::add(0, max_val))
  
  // 最小值加1（不会溢出）
  assert_eq(-2147483647, azimuth::add(min_val, 1))
  
  // 最大值加-1（不会溢出）
  assert_eq(2147483646, azimuth::add(max_val, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出测试
  assert_eq(max_val, azimuth::multiply(50000, 50000))
  assert_eq(max_val, azimuth::multiply(100000, 30000))
  
  // 负数相乘溢出测试
  assert_eq(min_val, azimuth::multiply(-50000, 50000))
  assert_eq(min_val, azimuth::multiply(50000, -50000))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let min_val = -2147483648
  
  // 最小值乘以-1的特殊情况
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(min_val, azimuth::multiply(-1, min_val))
  
  // 最小值乘以1
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(1, min_val))
  
  // 最小值乘以0
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(0, azimuth::multiply(0, min_val))
}

test "multiply_zero_and_one" {
  // 测试乘法的0和1特殊情况
  // 0乘以任何数
  assert_eq(0, azimuth::multiply(0, 12345))
  assert_eq(0, azimuth::multiply(-6789, 0))
  assert_eq(0, azimuth::multiply(0, -2147483648))
  assert_eq(0, azimuth::multiply(2147483647, 0))
  
  // 1乘以任何数
  assert_eq(12345, azimuth::multiply(1, 12345))
  assert_eq(-6789, azimuth::multiply(-6789, 1))
  assert_eq(2147483647, azimuth::multiply(1, 2147483647))
  assert_eq(-2147483648, azimuth::multiply(-2147483648, 1))
  
  // -1乘以任何数
  assert_eq(-12345, azimuth::multiply(-1, 12345))
  assert_eq(6789, azimuth::multiply(-1, -6789))
  assert_eq(-2147483647, azimuth::multiply(-1, 2147483647))
  assert_eq(-2147483648, azimuth::multiply(-1, -2147483648))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq(azimuth::greet("Alice-Bob"), "Hello, Alice-Bob!")
  assert_eq(azimuth::greet("Mr. Smith"), "Hello, Mr. Smith!")
  assert_eq(azimuth::greet("O'Connor"), "Hello, O'Connor!")
  assert_eq(azimuth::greet("John_Doe"), "Hello, John_Doe!")
}

test "greet_numbers_and_symbols" {
  // 测试包含数字和符号的问候
  assert_eq(azimuth::greet("User123"), "Hello, User123!")
  assert_eq(azimuth::greet("Test@2024"), "Hello, Test@2024!")
  assert_eq(azimuth::greet("$admin"), "Hello, $admin!")
  assert_eq(azimuth::greet("#hashtag"), "Hello, #hashtag!")
}

test "complex_financial_calculations" {
  // 测试复杂金融计算
  // 计算复利：本金 × (1 + 利率)^年数
  let principal = 10000
  let rate_percent = 8
  let years = 4
  
  // 逐年计算复利
  let year1 = azimuth::add(principal, azimuth::multiply(principal, rate_percent) / 100)
  let year2 = azimuth::add(year1, azimuth::multiply(year1, rate_percent) / 100)
  let year3 = azimuth::add(year2, azimuth::multiply(year2, rate_percent) / 100)
  let year4 = azimuth::add(year3, azimuth::multiply(year3, rate_percent) / 100)
  
  assert_eq(13605, year4)  // 10000 * 1.08^4 ≈ 13605
  
  // 计算贷款月供（简化版）
  let loan_amount = 120000
  let annual_rate = 6
  let loan_years = 30
  
  let monthly_rate = annual_rate / 12 / 100  // 月利率（简化）
  let total_months = azimuth::multiply(loan_years, 12)
  
  // 简化的月供计算（不使用复利公式）
  let total_interest = azimuth::multiply(loan_amount, azimuth::multiply(annual_rate, loan_years)) / 100
  let total_payment = azimuth::add(loan_amount, total_interest)
  let monthly_payment = total_payment / total_months
  
  assert_eq(722, monthly_payment)  // 近似值
}

test "physics_calculations" {
  // 测试物理计算
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10  // kg
  let velocity = 20  // m/s
  
  let velocity_squared = azimuth::multiply(velocity, velocity)
  let kinetic_energy = azimuth::multiply(mass, velocity_squared) / 2
  
  assert_eq(2000, kinetic_energy)  // 1/2 * 10 * 20^2 = 2000 J
  
  // 计算势能：E = m * g * h
  let height = 50  // m
  let gravity = 10  // m/s^2 (简化值)
  
  let potential_energy = azimuth::multiply(mass, azimuth::multiply(gravity, height))
  
  assert_eq(5000, potential_energy)  // 10 * 10 * 50 = 5000 J
  
  // 计算总机械能
  let total_energy = azimuth::add(kinetic_energy, potential_energy)
  assert_eq(7000, total_energy)
}