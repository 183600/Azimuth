// MoonBit 标准测试用例
// 测试 Azimuth 项目的核心功能

test "addition_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 123
  let b = 456
  let c = 789
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试负数情况
  let d = -100
  let e = 200
  let f = -150
  
  let left_assoc_neg = @azimuth.add(@azimuth.add(d, e), f)
  let right_assoc_neg = @azimuth.add(d, @azimuth.add(e, f))
  
  @azimuth.assert_eq(left_assoc_neg, right_assoc_neg)
}

test "multiplication_commutativity" {
  // 测试乘法交换律：a × b = b × a
  let a = 42
  let b = 17
  
  let ab = @azimuth.multiply(a, b)
  let ba = @azimuth.multiply(b, a)
  
  @azimuth.assert_eq(ab, ba)
  
  // 测试包含零的情况
  let c = 0
  let d = 999
  
  let cd = @azimuth.multiply(c, d)
  let dc = @azimuth.multiply(d, c)
  
  @azimuth.assert_eq(cd, dc)
  @azimuth.assert_eq(0, cd)
  
  // 测试负数情况
  let e = -25
  let f = 8
  
  let ef = @azimuth.multiply(e, f)
  let fe = @azimuth.multiply(f, e)
  
  @azimuth.assert_eq(ef, fe)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "mathematical_formula_validation" {
  // 测试数学公式验证
  let a = 6
  let b = 4
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 3
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum_ab = @azimuth.add(a, b)
  let diff_ab = @azimuth.add(a, -b)
  let left_side = @azimuth.multiply(sum_ab, diff_ab)
  
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let right_side = @azimuth.add(a_squared, -b_squared)
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证结合律：(a × b) × c = a × (b × c)
  let c2 = 5
  let assoc_left = @azimuth.multiply(@azimuth.multiply(a, b), c2)
  let assoc_right = @azimuth.multiply(a, @azimuth.multiply(b, c2))
  @azimuth.assert_eq(assoc_left, assoc_right)
}

test "real_world_application_scenario" {
  // 测试实际应用场景：银行贷款计算
  let principal = 100000  // 本金（单位：元）
  let annual_rate = 6     // 年利率（百分比）
  let years = 3           // 贷款年限
  
  // 简化的单利计算：总利息 = 本金 × 利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  
  // 总还款额 = 本金 + 总利息
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 月还款额 = 总还款额 ÷ (年数 × 12)
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  
  @azimuth.assert_eq(1800000, total_interest)   // 100000 × 6 × 3 = 1800000
  @azimuth.assert_eq(1900000, total_payment)    // 100000 + 1800000 = 1900000
  @azimuth.assert_eq(52777, monthly_payment)    // 1900000 ÷ 36 ≈ 52777（整数除法）
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护是否正常工作
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
  
  // 测试特殊值的处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "performance_related_calculations" {
  // 测试性能相关计算
  // 测试大量数据的累加（使用公式优化）
  let base_value = 100
  let count = 1000
  
  // 使用等差数列求和公式：sum = n × (first + last) / 2
  let last_value = @azimuth.multiply(base_value, count)
  let sum_formula = @azimuth.multiply(count, @azimuth.add(base_value, last_value)) / 2
  
  @azimuth.assert_eq(5050000, sum_formula)
  
  // 测试指数增长计算（2的10次方）
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), 
                                     @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  @azimuth.assert_eq(1024, power_of_2)
  
  // 测试阶乘计算（5!）
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  // 模拟整数到字符串的转换（通过greet函数）
  let year = 2024
  let year_string = "2024"
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet(year_string))
  
  // 模拟布尔值到字符串的转换
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet("true"))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet("false"))
  
  // 模拟浮点数的简化表示
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet("3.14"))
  
  // 测试日期时间字符串
  @azimuth.assert_eq_string("Hello, 2024-01-15!", @azimuth.greet("2024-01-15"))
  @azimuth.assert_eq_string("Hello, 12:30:45!", @azimuth.greet("12:30:45"))
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, ID_2024_001!", @azimuth.greet("ID_2024_001"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：物理公式模拟
  // 自由落体运动（简化版，不考虑重力常数）
  let initial_velocity = 10  // 初始速度
  let time = 5               // 时间
  let acceleration = 2       // 加速度
  
  // 位移 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  // 简化为整数计算：位移 = 初速度 × 时间 + 加速度 × 时间² / 2
  let time_squared = @azimuth.multiply(time, time)  // 25
  let distance_part1 = @azimuth.multiply(initial_velocity, time)  // 50
  let distance_part2 = @azimuth.multiply(acceleration, time_squared) / 2  // 25
  let total_distance = @azimuth.add(distance_part1, distance_part2)  // 75
  
  @azimuth.assert_eq(75, total_distance)
  
  // 测试动能计算（简化版）：动能 = 质量 × 速度² / 2
  let mass = 10
  let velocity = 8
  let velocity_squared = @azimuth.multiply(velocity, velocity)  // 64
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2  // 320
  
  @azimuth.assert_eq(320, kinetic_energy)
  
  // 测试复合计算：计算两个物体碰撞后的总动能
  let mass2 = 15
  let velocity2 = 6
  let velocity2_squared = @azimuth.multiply(velocity2, velocity2)  // 36
  let kinetic_energy2 = @azimuth.multiply(mass2, velocity2_squared) / 2  // 270
  
  let total_kinetic_energy = @azimuth.add(kinetic_energy, kinetic_energy2)
  @azimuth.assert_eq(590, total_kinetic_energy)
}