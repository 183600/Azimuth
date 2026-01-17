// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，提供更多测试场景

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::add(999999999, 1), 1000000000)
  assert_eq(azimuth::add(500000000, 500000000), 1000000000)
}

test "add_decimal_simulation" {
  // 模拟小数运算（通过整数运算）
  // 计算 1.5 + 2.3 = 3.8，通过乘以10实现
  assert_eq(azimuth::add(15, 23), 38)  // 实际表示 3.8
  
  // 计算 0.75 + 0.25 = 1.0，通过乘以100实现
  assert_eq(azimuth::add(75, 25), 100)  // 实际表示 1.0
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(azimuth::multiply(10000, 10000), 100000000)
  assert_eq(azimuth::multiply(12345, 6789), 83810205)
  assert_eq(azimuth::multiply(5000, 2000), 10000000)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(azimuth::multiply(1, 2), 2)      // 2^1
  assert_eq(azimuth::multiply(2, 2), 4)      // 2^2
  assert_eq(azimuth::multiply(4, 2), 8)      // 2^3
  assert_eq(azimuth::multiply(8, 2), 16)     // 2^4
  assert_eq(azimuth::multiply(16, 2), 32)    // 2^5
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("مرحبا"), "Hello, مرحبا!")
  assert_eq(azimuth::greet("🌍"), "Hello, 🌍!")
  assert_eq(azimuth::greet("Café"), "Hello, Café!")
  assert_eq(azimuth::greet("Niño"), "Hello, Niño!")
}

test "greet_numeric_names" {
  // 测试数字名称
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("007"), "Hello, 007!")
  assert_eq(azimuth::greet("2023"), "Hello, 2023!")
}

test "physics_calculations" {
  // 物理计算测试
  
  // 计算动能: KE = 1/2 * m * v^2
  // 质量 = 10kg, 速度 = 5m/s, KE = 0.5 * 10 * 25 = 125J
  let mass = 10
  let velocity = 5
  let kinetic_energy = azimuth::multiply(azimuth::multiply(mass, velocity), velocity) / 2
  assert_eq(125, kinetic_energy)
  
  // 计算势能: PE = m * g * h
  // 质量 = 5kg, 重力加速度 = 10m/s^2, 高度 = 20m, PE = 5 * 10 * 20 = 1000J
  let pe_mass = 5
  let gravity = 10
  let height = 20
  let potential_energy = azimuth::multiply(azimuth::multiply(pe_mass, gravity), height)
  assert_eq(1000, potential_energy)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算平均值: (1 + 2 + 3 + 4 + 5) / 5 = 3
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  let count = 5
  let average = sum / count
  assert_eq(3, average)
  
  // 计算加权平均: (10*2 + 20*3 + 30*5) / (2+3+5) = 23
  let weighted_sum = azimuth::add(azimuth::add(azimuth::multiply(10, 2), azimuth::multiply(20, 3)), azimuth::multiply(30, 5))
  let total_weight = azimuth::add(azimuth::add(2, 3), 5)
  let weighted_average = weighted_sum / total_weight
  assert_eq(23, weighted_average)
}

test "business_calculations" {
  // 商业计算测试
  
  // 计算利润: 收入 - 成本
  let revenue = 50000
  let cost = 35000
  let profit = azimuth::add(revenue, -cost)
  assert_eq(15000, profit)
  
  // 计算利润率: 利润 / 成本 * 100
  let profit_rate = azimuth::multiply(profit, 100) / cost
  assert_eq(42, profit_rate)  // 42%（截断小数部分）
  
  // 计算税收: 利润 * 税率
  let tax_rate = 20  // 20%
  let tax = azimuth::multiply(profit, tax_rate) / 100
  let net_profit = azimuth::add(profit, -tax)
  assert_eq(3000, tax)
  assert_eq(12000, net_profit)
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  
  // 计算: (a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let part1 = azimuth::multiply(azimuth::add(a, b), azimuth::add(c, -d))
  let part2 = azimuth::multiply(e, f)
  let result = azimuth::add(part1, part2)
  assert_eq(181, result)  // (10+5)*(20-8) + 3*7 = 15*12 + 21 = 180 + 21 = 201
  
  // 计算: a^2 + b^2 - 2*a*b (完全平方公式)
  let a2 = azimuth::multiply(a, a)
  let b2 = azimuth::multiply(b, b)
  let twoab = azimuth::multiply(azimuth::multiply(2, a), b)
  let square_result = azimuth::add(azimuth::add(a2, b2), -twoab)
  assert_eq(25, square_result)  // 10^2 + 5^2 - 2*10*5 = 100 + 25 - 100 = 25
}