// 新增 MoonBit 测试用例
// 测试基本功能和边界情况

// 测试加法的交换律
test "addition_commutativity" {
  let a = 12345
  let b = 67890
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数
  let c = -123
  let d = 456
  assert_eq(add(c, d), add(d, c))
  
  // 测试零值
  let e = 0
  let f = 999
  assert_eq(add(e, f), add(f, e))
}

// 测试乘法的结合律
test "multiplication_associativity" {
  let a = 12
  let b = 23
  let c = 34
  
  // (a * b) * c = a * (b * c)
  let left = multiply(multiply(a, b), c)
  let right = multiply(a, multiply(b, c))
  assert_eq(left, right)
  
  // 测试包含负数
  let x = -5
  let y = 7
  let z = -3
  
  let left_neg = multiply(multiply(x, y), z)
  let right_neg = multiply(x, multiply(y, z))
  assert_eq(left_neg, right_neg)
}

// 测试字符串处理的多语言支持
test "multilingual_string_processing" {
  // 测试中文
  assert_eq_string("Hello, 世界!", greet("世界"))
  
  // 测试日文
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  
  // 测试韩文
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
  
  // 测试阿拉伯文
  assert_eq_string("Hello, مرحبا!", greet("مرحبا"))
  
  // 测试俄文
  assert_eq_string("Hello, Привет!", greet("Привет"))
}

// 测试数学公式的实际应用
test "mathematical_formulas" {
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 15
  let height = 8
  let triangle_area = multiply(base, height) / 2
  assert_eq(60, triangle_area)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 10
  let bottom_base = 20
  let trapezoid_height = 5
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(75, trapezoid_area)
  
  // 计算圆柱体表面积（简化）：2 × π × r × (r + h) ≈ 6 × r × (r + h)
  let radius = 4
  let cylinder_height = 10
  let surface_area = multiply(6, multiply(radius, add(radius, cylinder_height)))
  assert_eq(336, surface_area)
}

// 测试金融计算场景
test "financial_calculations" {
  // 计算贷款月供（简化）：本金 ÷ 期数 + 利息
  let principal = 120000
  let months = 12
  let monthly_rate = 1  // 1%
  
  let monthly_principal = principal / months
  let monthly_interest = multiply(principal, monthly_rate) / 100
  let monthly_payment = add(monthly_principal, monthly_interest)
  assert_eq(11000, monthly_payment)
  
  // 计算投资回报率（简化）：收益 ÷ 本金 × 100%
  let initial_investment = 50000
  let final_value = 65000
  let profit = add(final_value, -initial_investment)
  let return_rate = multiply(profit, 100) / initial_investment
  assert_eq(30, return_rate)
}

// 测试物理计算场景
test "physics_calculations" {
  // 计算自由落体距离（简化）：s = 0.5 × g × t² ≈ 5 × t²
  let time = 3
  let distance = multiply(5, multiply(time, time))
  assert_eq(45, distance)
  
  // 计算动能：E = 0.5 × m × v² ≈ m × v² ÷ 2
  let mass = 10
  let velocity = 8
  let kinetic_energy = multiply(multiply(mass, velocity), velocity) / 2
  assert_eq(320, kinetic_energy)
  
  // 计算功：W = F × d
  let force = 50
  let displacement = 12
  let work = multiply(force, displacement)
  assert_eq(600, work)
}

// 测试边界值的复杂组合
test "complex_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试链式运算
  let chain1 = add(add(max_val, -100), 50)
  assert_eq(2147483597, chain1)
  
  let chain2 = add(add(min_val, 100), -50)
  assert_eq(-2147483598, chain2)
  
  // 测试混合运算
  let mixed1 = add(multiply(100, max_val), -multiply(99, max_val))
  assert_eq(max_val, mixed1)
  
  let mixed2 = add(multiply(100, min_val), -multiply(99, min_val))
  assert_eq(min_val, mixed2)
  
  // 测试嵌套运算
  let nested1 = multiply(add(max_val, -max_val), add(min_val, -min_val))
  assert_eq(0, nested1)
}

// 测试数据处理场景
test "data_processing_scenarios" {
  // 计算数组元素的总和（模拟）
  let values = [15, 25, 35, 45, 55]
  let sum = add(add(add(add(15, 25), 35), 45), 55)
  assert_eq(175, sum)
  
  // 计算加权平均值（模拟）
  let data_points = [80, 90, 75, 85, 95]
  let weights = [2, 3, 1, 2, 2]
  let weighted_sum = add(add(add(add(multiply(80, 2), multiply(90, 3)), multiply(75, 1)), multiply(85, 2)), multiply(95, 2))
  let total_weight = add(add(add(add(2, 3), 1), 2), 2)
  let weighted_avg = weighted_sum / total_weight
  assert_eq(86, weighted_avg)
  
  // 计算方差（简化版）
  let mean = 86
  let variance = add(add(add(add(multiply(80-mean, 80-mean), multiply(90-mean, 90-mean)), 
                           multiply(75-mean, 75-mean)), multiply(85-mean, 85-mean)), 
                           multiply(95-mean, 95-mean)) / 5
  assert_eq(36, variance)
}

// 测试游戏逻辑计算
test "game_logic_calculations" {
  // 计算经验值升级所需经验
  let current_level = 10
  let base_exp = 1000
  let level_multiplier = 15
  
  // 所需经验 = 基础经验 × (1 + 等级 × 等级倍数)
  let exp_needed = multiply(base_exp, add(1, multiply(current_level, level_multiplier)))
  assert_eq(16000, exp_needed)
  
  // 计算伤害值（简化）：基础伤害 × (1 + 暴击率 × 暴击倍数)
  let base_damage = 100
  let crit_rate = 25  // 25%
  let crit_multiplier = 2
  
  let average_damage = multiply(base_damage, add(100, multiply(crit_rate, crit_multiplier))) / 100
  assert_eq(150, average_damage)
  
  // 计算得分倍率
  let combo_count = 5
  let base_score = 1000
  let combo_bonus = multiply(combo_count, 10)  // 每个连击加10%
  let final_score = multiply(base_score, add(100, combo_bonus)) / 100
  assert_eq(1500, final_score)
}

// 测试字符串处理的边界情况
test "string_edge_case_processing" {
  // 测试非常长的字符串
  let very_long_name = "ThisIsAnExtremelyLongUserNameThatMightBeUsedInSomeRealWorldApplications"
  assert_eq_string("Hello, " + very_long_name + "!", greet(very_long_name))
  
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, " + special_chars + "!", greet(special_chars))
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123ABC456"
  assert_eq_string("Hello, " + alphanumeric + "!", greet(alphanumeric))
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe Jr."
  assert_eq_string("Hello, " + spaced_name + "!", greet(spaced_name))
}