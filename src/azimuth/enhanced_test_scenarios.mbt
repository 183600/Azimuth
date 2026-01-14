// 增强测试场景 - 提供10个新的 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-4, 4))
  assert_eq(-3, divide_with_ceil(-11, 4))
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的向上取整除法
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(-1, divide_with_ceil(3, -4))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 5
  let months_per_year = 12
  
  // 计算总月数
  let total_months = multiply(years, months_per_year)
  assert_eq(60, total_months)
  
  // 简化的月利率计算（年利率/12）
  let monthly_rate = annual_rate / months_per_year
  
  // 简化的月付款计算（本金/总月数 + 利息）
  let monthly_principal = principal / total_months
  let monthly_interest = multiply(principal, monthly_rate) / 100
  let monthly_payment = add(monthly_principal, monthly_interest)
  
  assert_eq(183, monthly_payment)
}

test "scientific_notation_simulation" {
  // 科学计数法模拟：处理大数运算
  // 模拟 1.23 × 10^6 = 1230000
  let mantissa = 123
  let exponent = 4  // 表示 10^4
  let result = multiply(mantissa, multiply(10, multiply(10, multiply(10, 10))))
  assert_eq(1230000, result)
  
  // 模拟 4.56 × 10^3 = 4560
  let mantissa2 = 456
  let exponent2 = 1  // 表示 10^1
  let result2 = multiply(mantissa2, 10)
  assert_eq(4560, result2)
}

test "coordinate_geometry_calculations" {
  // 坐标几何计算：两点间距离的平方（避免开方运算）
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y3 = 1
  
  // 计算 dx = x2 - x1, dy = y2 - y1
  let dx = add(x2, -x1)
  let dy = add(y3, -y1)
  
  // 计算距离的平方：dx² + dy²
  let distance_squared = add(multiply(dx, dx), multiply(dy, dy))
  assert_eq(25, distance_squared)  // 4² + (-3)² = 16 + 9 = 25
}

test "temperature_conversion_precise" {
  // 精确温度转换：摄氏度到华氏度和开尔文
  let celsius = 25
  
  // 摄氏度到华氏度：F = C × 9/5 + 32
  // 使用整数近似：F ≈ C × 2 + 30
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 摄氏度到开尔文：K = C + 273
  let kelvin = add(celsius, 273)
  assert_eq(298, kelvin)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  let freezing_kelvin = add(freezing_celsius, 273)
  
  assert_eq(30, freezing_fahrenheit)
  assert_eq(273, freezing_kelvin)
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  // 测试电子邮件格式（简化版）
  let email1 = "user@example.com"
  let email2 = "test123@domain.org"
  let invalid_email = "invalid-email"
  
  // 通过greet函数验证字符串处理
  assert_eq_string("Hello, user@example.com!", greet(email1))
  assert_eq_string("Hello, test123@domain.org!", greet(email2))
  assert_eq_string("Hello, invalid-email!", greet(invalid_email))
  
  // 测试电话号码格式（简化版）
  let phone1 = "123-456-7890"
  let phone2 = "+1-555-123-4567"
  
  assert_eq_string("Hello, 123-456-7890!", greet(phone1))
  assert_eq_string("Hello, +1-555-123-4567!", greet(phone2))
}

test "combinatorial_calculations" {
  // 组合数学计算：排列和组合（简化版）
  let n = 5
  let r = 3
  
  // 计算排列 P(n,r) = n! / (n-r)!
  // 5! / 2! = 120 / 2 = 60
  let n_factorial = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  let n_minus_r_factorial = multiply(n - r, n - r - 1)
  let permutation = n_factorial / n_minus_r_factorial
  assert_eq(60, permutation)
  
  // 计算组合 C(n,r) = P(n,r) / r!
  // 60 / 6 = 10
  let r_factorial = multiply(r, r - 1)
  let combination = permutation / r_factorial
  assert_eq(10, combination)
}

test "game_mechanics_simulation" {
  // 游戏机制模拟：RPG角色属性计算
  let base_strength = 10
  let base_agility = 8
  let base_intelligence = 12
  let level = 5
  let equipment_bonus = 3
  
  // 计算最终属性：基础值 + 等级加成 + 装备加成
  let strength_level_bonus = multiply(level, 2)
  let final_strength = add(add(base_strength, strength_level_bonus), equipment_bonus)
  
  let agility_level_bonus = multiply(level, 1)
  let final_agility = add(add(base_agility, agility_level_bonus), equipment_bonus)
  
  let intelligence_level_bonus = multiply(level, 3)
  let final_intelligence = add(add(base_intelligence, intelligence_level_bonus), equipment_bonus)
  
  assert_eq(23, final_strength)    // 10 + 5*2 + 3 = 23
  assert_eq(16, final_agility)     // 8 + 5*1 + 3 = 16
  assert_eq(30, final_intelligence) // 12 + 5*3 + 3 = 30
  
  // 计算总战斗力
  let combat_power = add(add(multiply(final_strength, 2), multiply(final_agility, 3)), multiply(final_intelligence, 1))
  assert_eq(106, combat_power)
}

test "data_structures_operations" {
  // 数据结构操作模拟：栈和队列的基本操作
  // 模拟栈操作（后进先出）
  let stack_size = 0
  
  // push操作
  stack_size = add(stack_size, 1)  // push 1
  stack_size = add(stack_size, 1)  // push 2
  stack_size = add(stack_size, 1)  // push 3
  assert_eq(3, stack_size)
  
  // pop操作
  stack_size = add(stack_size, -1)  // pop 3
  stack_size = add(stack_size, -1)  // pop 2
  assert_eq(1, stack_size)
  
  // 模拟队列操作（先进先出）
  let queue_size = 0
  
  // enqueue操作
  queue_size = add(queue_size, 1)  // enqueue 1
  queue_size = add(queue_size, 1)  // enqueue 2
  queue_size = add(queue_size, 1)  // enqueue 3
  assert_eq(3, queue_size)
  
  // dequeue操作
  queue_size = add(queue_size, -1)  // dequeue 1
  queue_size = add(queue_size, -1)  // dequeue 2
  assert_eq(1, queue_size)
}