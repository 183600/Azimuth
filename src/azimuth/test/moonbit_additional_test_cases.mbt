// MoonBit 额外测试用例
// 这些测试用例涵盖了各种实用场景和边界情况

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
  @azimuth.assert_eq_string("Hello, Special@#$%!", @azimuth.greet("Special@#$%"))
}

test "mathematical_operations_sequence" {
  // 测试数学运算的序列组合
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
  
  // (a + b + c) * 2
  let result3 = @azimuth.multiply(@azimuth.add(@azimuth.add(a, b), c), 2)
  @azimuth.assert_eq(120, result3)
}

test "boundary_value_combinations" {
  // 测试边界值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值与0的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "practical_business_calculations" {
  // 实际业务计算测试
  let unit_price = 199
  let quantity = 5
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(1026, final_price)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  
  // 测试各种温度
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))    // 0°C -> 30°F
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))   // 10°C -> 50°F
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))   // 20°C -> 70°F
  @azimuth.assert_eq(100, @azimuth.add(@azimuth.multiply(35, 2), 30))  // 35°C -> 100°F
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) / 2
  @azimuth.assert_eq(0, @azimuth.add(30, -30) / 2)    // 30°F -> 0°C
  @azimuth.assert_eq(10, @azimuth.add(50, -30) / 2)   // 50°F -> 10°C
  @azimuth.assert_eq(20, @azimuth.add(70, -30) / 2)   // 70°F -> 20°C
}

test "array_index_manipulation" {
  // 数组索引操作测试
  let rows = 5
  let cols = 4
  
  // 计算二维数组的一维索引
  let index_1_1 = @azimuth.add(@azimuth.multiply(1, cols), 1)  // 第2行第2列
  @azimuth.assert_eq(9, index_1_1)
  
  let index_2_3 = @azimuth.add(@azimuth.multiply(2, cols), 3)  // 第3行第4列
  @azimuth.assert_eq(11, index_2_3)
  
  let index_4_0 = @azimuth.add(@azimuth.multiply(4, cols), 0)  // 第5行第1列
  @azimuth.assert_eq(16, index_4_0)
  
  // 计算边界索引
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  @azimuth.assert_eq(0, first_index)
  
  let last_index = @azimuth.add(@azimuth.multiply(rows - 1, cols), cols - 1)
  @azimuth.assert_eq(19, last_index)
}

test "game_logic_calculations" {
  // 游戏逻辑计算测试
  let base_score = 100
  let level = 5
  let multiplier = 2
  let bonus = 50
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, @azimuth.multiply(level, multiplier))
  
  // 计算最终得分
  let final_score = @azimuth.add(level_score, bonus)
  
  @azimuth.assert_eq(1050, final_score)
  
  // 计算排名得分（前3名额外加分）
  let rank = 2
  let rank_bonus = if (rank <= 3) {
    @azimuth.multiply(final_score, 20) / 100
  } else {
    0
  }
  
  let total_score = @azimuth.add(final_score, rank_bonus)
  @azimuth.assert_eq(1260, total_score)
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 × (1 + 利率 × 年数)
  let simple_interest = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, simple_interest)
  
  // 复利计算（简化版）：本金 × (1 + 利率)^年数
  // 简化为：本金 × (1 + 利率 × 年数)
  let compound_interest = @azimuth.multiply(principal, @azimuth.add(100, @azimuth.multiply(rate, years))) / 100
  @azimuth.assert_eq(16000, compound_interest)
  
  // 月供计算（简化版）：贷款总额 / 月数 + 利息
  let loan_amount = 120000
  let months = 36
  let monthly_rate = 1
  let monthly_payment = @azimuth.add(loan_amount / months, @azimuth.multiply(loan_amount, monthly_rate) / 100)
  @azimuth.assert_eq(4533, monthly_payment)
}

test "physics_simulation" {
  // 物理模拟测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²
  // 简化计算：s = ut + at²/2
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(@azimuth.multiply(acceleration, time), time) / 2)
  @azimuth.assert_eq(75, displacement)
  
  // 计算动能：KE = 0.5mv²
  // 简化计算：KE = mv²/2
  let mass = 10
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(final_velocity, final_velocity)) / 2
  @azimuth.assert_eq(2000, kinetic_energy)
}