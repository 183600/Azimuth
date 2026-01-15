// 新的 MoonBit 测试用例
// 覆盖多种实际应用场景和边界条件
// 使用标准的 MoonBit 测试语法

test "datetime_string_processing" {
  // 测试日期时间字符串处理
  let date = "2023-12-25"
  let time = "14:30:00"
  let datetime = date + " " + time
  
  // 使用 greet 函数处理日期时间字符串
  let date_greeting = @azimuth.greet(date)
  let time_greeting = @azimuth.greet(time)
  let datetime_greeting = @azimuth.greet(datetime)
  
  @azimuth.assert_eq_string("Hello, 2023-12-25!", date_greeting)
  @azimuth.assert_eq_string("Hello, 14:30:00!", time_greeting)
  @azimuth.assert_eq_string("Hello, 2023-12-25 14:30:00!", datetime_greeting)
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 计算物体自由落体距离：s = 0.5 * g * t²
  // 简化为整数计算：s = 5 * t² (g ≈ 10 m/s²)
  let time = 3
  let gravity = 5  // 简化的重力常数
  let distance = @azimuth.multiply(gravity, @azimuth.multiply(time, time))
  @azimuth.assert_eq(45, distance)
  
  // 计算简单动能：KE = 0.5 * m * v²
  // 简化为整数计算：KE = m * v² / 2
  let mass = 10
  let velocity = 8
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, velocity), velocity) / 2
  @azimuth.assert_eq(320, kinetic_energy)
}

test "financial_calculations" {
  // 测试财务计算场景
  // 计算贷款月供（简化版）：月供 = (本金 + 总利息) / 月数
  let principal = 100000
  let annual_rate = 6  // 6%
  let years = 10
  let months = @azimuth.multiply(years, 12)
  
  // 简化的利息计算：本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(13333, monthly_payment)
  
  // 计算投资回报率（简化版）
  let initial_investment = 50000
  let final_value = 75000
  let profit = @azimuth.add(final_value, -initial_investment)
  let roi_percentage = @azimuth.multiply(profit, 100) / initial_investment
  
  @azimuth.assert_eq(50, roi_percentage)
}

test "string_formatting_advanced" {
  // 测试高级字符串格式化
  // 模拟货币格式化
  let amount = 1234567
  let currency_symbol = "¥"
  let formatted_amount = currency_symbol + amount.to_string()
  
  @azimuth.assert_eq_string("Hello, ¥1234567!", @azimuth.greet(formatted_amount))
  
  // 模拟百分比格式化
  let percentage_value = 85
  let percentage_symbol = "%"
  let formatted_percentage = percentage_value.to_string() + percentage_symbol
  
  @azimuth.assert_eq_string("Hello, 85%!", @azimuth.greet(formatted_percentage))
  
  // 测试多语言字符串
  let chinese_greeting = "你好"
  let english_greeting = "Hello"
  let japanese_greeting = "こんにちは"
  
  @azimuth.assert_eq_string("Hello, 你好!", @azimuth.greet(chinese_greeting))
  @azimuth.assert_eq_string("Hello, Hello!", @azimuth.greet(english_greeting))
  @azimuth.assert_eq_string("Hello, こんにちは!", @azimuth.greet(japanese_greeting))
}

test "business_logic_integration" {
  // 测试业务逻辑集成
  // 计算员工月薪：基本工资 + 绩效奖金 - 扣款
  let base_salary = 8000
  let performance_bonus = 2000
  let deductions = 500
  let monthly_salary = @azimuth.add(@azimuth.add(base_salary, performance_bonus), -deductions)
  
  @azimuth.assert_eq(9500, monthly_salary)
  
  // 计算项目成本：人力成本 + 材料成本 + 其他费用
  let labor_cost = 50000
  let material_cost = 30000
  let other_costs = 15000
  let total_project_cost = @azimuth.add(@azimuth.add(labor_cost, material_cost), other_costs)
  
  @azimuth.assert_eq(95000, total_project_cost)
  
  // 计算利润率：(收入 - 成本) / 成本 * 100%
  let revenue = 150000
  let cost = 95000
  let profit = @azimuth.add(revenue, -cost)
  let profit_margin = @azimuth.multiply(profit, 100) / cost
  
  @azimuth.assert_eq(57, profit_margin)
}

test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值与普通值的组合运算
  let normal_positive = 1000
  let normal_negative = -1000
  
  // 极值与普通值的加减
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, normal_positive))
  @azimuth.assert_eq(2147482647, @azimuth.add(max_val, normal_negative))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, normal_negative))
  @azimuth.assert_eq(-2147482648, @azimuth.add(min_val, normal_positive))
  
  // 极值与普通值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  
  // 测试连续极值运算
  let complex_calculation = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), 1), max_val)
  @azimuth.assert_eq(max_val, complex_calculation)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试各种溢出情况
  let positive_overflow = @azimuth.add(max_val, max_val)
  let negative_overflow = @azimuth.add(min_val, min_val)
  let multiplication_overflow = @azimuth.multiply(max_val, 100)
  
  @azimuth.assert_eq(max_val, positive_overflow)
  @azimuth.assert_eq(min_val, negative_overflow)
  @azimuth.assert_eq(max_val, multiplication_overflow)
  
  // 测试特殊值的处理
  let zero_division_avoidance = if (100 != 0) { 100 / 100 } else { 0 }
  let negative_square_root_handling = @azimuth.multiply(-5, -5)  // 避免负数开方
  
  @azimuth.assert_eq(1, zero_division_avoidance)
  @azimuth.assert_eq(25, negative_square_root_handling)
  
  // 测试断言函数的可靠性
  @azimuth.assert_true(@azimuth.add(100, 200) == 300)
  @azimuth.assert_false(@azimuth.add(100, 200) == 400)
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试边界条件下的断言
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}