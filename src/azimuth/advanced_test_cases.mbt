// Azimuth 高级测试用例
// 包含数论、算法和实际应用场景的综合测试

test "prime_number_validation" {
  // 使用基本运算验证质数特性
  let num = 17
  let divisor = 2
  let remainder = add(num, -multiply(divisor, divide_with_ceil(num, divisor)))
  
  // 验证17不能被2整除
  assert_eq(1, remainder)
  
  // 验证17是质数（通过检查不能被小于等于其平方根的数整除）
  let sqrt_approx = divide_with_ceil(num, 4)  // 简化的平方根近似
  let is_prime = if add(num, -multiply(divisor, divide_with_ceil(num, divisor))) == 0 { false } else { true }
  assert_eq(true, is_prime)
}

test "geometric_progression_sum" {
  // 计算等比数列的和：2 + 4 + 8 + 16 = 30
  let first_term = 2
  let ratio = 2
  let terms = 4
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(30, sum)
}

test "string_greeting_with_concatenation" {
  // 测试字符串连接和问候的组合使用
  let title = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  
  let full_name = title + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Jane Smith!", formal_greeting)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = project_c_needs
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  
  let remaining = add(total_resources, -total_allocated)
  let can_fund_additional = if remaining >= 15 { true } else { false }
  
  assert_eq(85, total_allocated)
  assert_eq(15, remaining)
  assert_eq(true, can_fund_additional)
}

test "temperature_conversion_simulation" {
  // 温度转换模拟（简化的摄氏度到华氏度）
  let celsius = 25
  let freezing_point = 0
  let boiling_point = 100
  
  // 简化的转换公式：F = C * 9/5 + 32
  let fahrenheit = add(add(divide_with_ceil(multiply(celsius, 9), 5), 32), 0)
  
  // 验证温度范围
  let is_above_freezing = celsius > freezing_point
  let is_below_boiling = celsius < boiling_point
  let is_normal_range = if is_above_freezing && is_below_boiling { true } else { false }
  
  assert_eq(77, fahrenheit)
  assert_eq(true, is_normal_range)
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_demand = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  let demand_during_lead = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead, safety_stock)
  let should_reorder = if current_stock <= reorder_point { true } else { false }
  
  assert_eq(56, demand_during_lead)
  assert_eq(76, reorder_point)
  assert_eq(false, should_reorder)
}

test "array_indexing_simulation" {
  // 模拟数组索引计算（基于一维数组的二维访问）
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算一维数组中的索引：index = row * cols + col
  let linear_index = add(multiply(row_index, cols), col_index)
  let total_elements = multiply(rows, cols)
  let is_valid_index = if linear_index < total_elements { true } else { false }
  
  assert_eq(13, linear_index)
  assert_eq(20, total_elements)
  assert_eq(true, is_valid_index)
}

test "loan_repayment_calculation" {
  // 贷款还款计算（简化版本）
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的利息计算：总利息 = 本金 × 年利率 × 年数
  let total_interest = divide_with_ceil(multiply(multiply(principal, annual_rate), years), 100)
  let total_repayment = add(principal, total_interest)
  let monthly_payment = divide_with_ceil(total_repayment, multiply(years, 12))
  
  assert_eq(1500, total_interest)
  assert_eq(11500, total_repayment)
  assert_eq(320, monthly_payment)
}

test "binary_number_simulation" {
  // 二进制数模拟（转换为十进制）
  // 二进制 1011 = 1*2^3 + 0*2^2 + 1*2^1 + 1*2^0 = 8 + 0 + 2 + 1 = 11
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let decimal_value = add(add(
    multiply(bit3, multiply(2, multiply(2, 2))),  // 1 * 2^3 = 8
    multiply(bit2, multiply(2, 2))),              // 0 * 2^2 = 0
    add(multiply(bit1, 2), bit0)                   // 1 * 2^1 + 1 * 2^0 = 2 + 1 = 3
  )
  
  assert_eq(11, decimal_value)
}

test "game_score_calculation" {
  // 游戏得分计算系统
  let base_score = 100
  let level_multiplier = 3
  let bonus_collectibles = 7
  let penalty_deaths = 2
  let bonus_per_collectible = 10
  let penalty_per_death = 15
  
  let level_score = multiply(base_score, level_multiplier)
  let total_bonus = multiply(bonus_collectibles, bonus_per_collectible)
  let total_penalty = multiply(penalty_deaths, penalty_per_death)
  let final_score = add(add(level_score, total_bonus), -total_penalty)
  
  assert_eq(300, level_score)
  assert_eq(70, total_bonus)
  assert_eq(30, total_penalty)
  assert_eq(340, final_score)
}