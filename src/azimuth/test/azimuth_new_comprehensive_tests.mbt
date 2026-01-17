// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "statistical_calculations" {
  // 统计学计算测试
  
  // 计算平均数: (a + b + c + d) / 4
  let values_sum = azimuth::add(azimuth::add(azimuth::add(85, 90), 78), 92)
  let average = values_sum / 4
  assert_eq(86, average)
  
  // 计算加权平均: (a*权重1 + b*权重2 + c*权重3) / (权重1+权重2+权重3)
  let weighted_sum = azimuth::add(azimuth::add(azimuth::multiply(80, 3), azimuth::multiply(90, 2)), azimuth::multiply(85, 5))
  let total_weight = azimuth::add(azimuth::add(3, 2), 5)
  let weighted_average = weighted_sum / total_weight
  assert_eq(84, weighted_average)
}

test "physics_calculations" {
  // 物理公式计算测试
  
  // 动能计算: E = 0.5 * m * v² (使用整数近似)
  let mass = 10
  let velocity = 8
  let kinetic_energy = azimuth::multiply(azimuth::multiply(mass, velocity), velocity) / 2
  assert_eq(320, kinetic_energy)
  
  // 势能计算: E = m * g * h (g ≈ 10 m/s²)
  let height = 15
  let potential_energy = azimuth::multiply(azimuth::multiply(mass, 10), height)
  assert_eq(1500, potential_energy)
  
  // 牛顿第二定律: F = m * a
  let acceleration = 5
  let force = azimuth::multiply(mass, acceleration)
  assert_eq(50, force)
}

test "percentage_calculations" {
  // 百分比计算测试
  
  // 计算百分比: 部分 / 总数 * 100
  let part = 25
  let total = 200
  let percentage = azimuth::multiply(part, 100) / total
  assert_eq(12, percentage)
  
  // 百分比增加: 原值 * (1 + 百分比/100)
  let original = 150
  let increase_percent = 20
  let increased = azimuth::add(azimuth::multiply(original, 100), azimuth::multiply(original, increase_percent)) / 100
  assert_eq(180, increased)
  
  // 百分比减少: 原值 * (1 - 百分比/100)
  let decrease_percent = 30
  let decreased = azimuth::add(azimuth::multiply(original, 100), azimuth::multiply(original, -decrease_percent)) / 100
  assert_eq(105, decreased)
}

test "number_system_conversions" {
  // 数制转换计算测试
  
  // 二进制转十进制: 1101 = 1*2³ + 1*2² + 0*2¹ + 1*2⁰ = 13
  let binary_1101 = azimuth::add(azimuth::add(azimuth::multiply(1, 8), azimuth::multiply(1, 4)), azimuth::multiply(1, 1))
  assert_eq(13, binary_1101)
  
  // 十进制转八进制计算: 65 ÷ 8 = 8 余 1, 8 ÷ 8 = 1 余 0, 1 ÷ 8 = 0 余 1 → 101
  let decimal_65 = 65
  let octal_65 = azimuth::add(azimuth::multiply(1, 64), azimuth::multiply(0, 8), 1)
  assert_eq(65, octal_65)
  
  // 十六进制转十进制: 1A = 1*16 + 10 = 26
  let hex_1A = azimuth::add(azimuth::multiply(1, 16), 10)
  assert_eq(26, hex_1A)
}

test "recursive_calculations" {
  // 递归计算模拟测试
  
  // 阶乘计算: 4! = 4 * 3 * 2 * 1
  let factorial_4 = azimuth::multiply(azimuth::multiply(azimuth::multiply(4, 3), 2), 1)
  assert_eq(24, factorial_4)
  
  // 幂运算: 3³ = 3 * 3 * 3
  let power_3_3 = azimuth::multiply(azimuth::multiply(3, 3), 3)
  assert_eq(27, power_3_3)
  
  // 最大公约数模拟: gcd(48, 18) = 6
  // 使用欧几里得算法的简化版本
  let a = 48
  let b = 18
  let remainder = azimuth::add(a, azimuth::multiply(-2, b))  // 48 - 2*18 = 12
  let gcd = azimuth::add(b, azimuth::multiply(-1, remainder))  // 18 - 1*12 = 6
  assert_eq(6, gcd)
}

test "combinatorial_calculations" {
  // 组合数学计算测试
  
  // 排列数: P(5,3) = 5! / (5-3)! = 5*4*3 = 60
  let permutation = azimuth::multiply(azimuth::multiply(5, 4), 3)
  assert_eq(60, permutation)
  
  // 组合数: C(6,2) = 6! / (2! * 4!) = (6*5)/(2*1) = 15
  let combination = azimuth::multiply(azimuth::multiply(6, 5), 1) / azimuth::multiply(2, 1)
  assert_eq(15, combination)
  
  // 二项式系数展开: (x+y)³ = x³ + 3x²y + 3xy² + y³ 的系数之和
  let binomial_coeffs_sum = azimuth::add(azimuth::add(azimuth::add(1, 3), 3), 1)
  assert_eq(8, binomial_coeffs_sum)
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  
  // 字符串长度计算（模拟）
  let short_name = "Bob"
  assert_eq(azimuth::greet(short_name), "Hello, Bob!")
  
  // 包含空格的字符串
  let full_name = "John Doe"
  assert_eq(azimuth::greet(full_name), "Hello, John Doe!")
  
  // 包含数字的字符串
  let alphanumeric = "User123"
  assert_eq(azimuth::greet(alphanumeric), "Hello, User123!")
  
  // 特殊符号字符串
  let symbols = "!@#$%^&*()"
  assert_eq(azimuth::greet(symbols), "Hello, !@#$%^&*()!")
}

test "real_world_applications" {
  // 实际应用场景测试
  
  // 银行复利计算: A = P(1 + r/n)^(nt) 简化版本
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  let simple_compound = azimuth::add(principal, azimuth::multiply(azimuth::multiply(principal, rate), years) / 100)
  assert_eq(1150, simple_compound)
  
  // 贷款月供计算简化版: 本金 + 利息 / 月数
  let loan_amount = 12000
  let interest_rate = 6  // 6%
  let months = 12
  let total_interest = azimuth::multiply(loan_amount, interest_rate) / 100
  let monthly_payment = azimuth::add(loan_amount, total_interest) / months
  assert_eq(1060, monthly_payment)
  
  // BMI计算简化版: 体重(kg) / (身高(m))²
  let weight = 70  // kg
  let height = 175  // cm
  let height_in_meters = height / 100
  let bmi = azimuth::multiply(weight, 10000) / azimuth::multiply(height, height)
  assert_eq(22, bmi)
}

test "edge_case_error_handling" {
  // 边界情况和错误处理测试
  
  // 极大数相加
  let huge_num1 = 1000000000
  let huge_num2 = 2000000000
  assert_eq(3000000000, azimuth::add(huge_num1, huge_num2))
  
  // 极大数相乘
  let large_multiply = azimuth::multiply(100000, 10000)
  assert_eq(1000000000, large_multiply)
  
  // 负数边界
  let negative_boundary = azimuth::add(-1000000, 999999)
  assert_eq(-1, negative_boundary)
  
  // 零的多次运算
  let zero_operations = azimuth::multiply(azimuth::add(0, 0), azimuth::multiply(0, 0))
  assert_eq(0, zero_operations)
}