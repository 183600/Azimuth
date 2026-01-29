// Azimuth 增强测试用例
// 包含10个高质量的 MoonBit 标准测试用例

test "complex_calculator_scenario" {
  // 复杂计算器场景：模拟科学计算器的多项式计算
  // 计算：3x² + 2x + 1，当 x = 5
  let x = 5
  let term1 = multiply(3, multiply(x, x))  // 3x²
  let term2 = multiply(2, x)              // 2x
  let term3 = 1                           // 1
  let result = add(add(term1, term2), term3)
  
  assert_eq(86, result)  // 3*25 + 2*5 + 1 = 75 + 10 + 1 = 86
}

test "financial_investment_calculation" {
  // 金融投资计算：复利计算与分期付款
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  // 计算月供（3年=36个月）
  let monthly_payment = divide_with_ceil(year3_end, multiply(years, 12))
  
  assert_eq(10800, year1_end)  // 10000 + 800
  assert_eq(11664, year2_end)  // 10800 + 864
  assert_eq(12597, year3_end)  // 11664 + 933
  assert_eq(350, monthly_payment)  // ceil(12597/36)
}

test "logistics_optimization_problem" {
  // 物流优化问题：多仓库配送成本计算
  let warehouse_a_capacity = 150
  let warehouse_b_capacity = 200
  let warehouse_c_capacity = 120
  let total_demand = 400
  
  // 计算每个仓库需要承担的配送量
  let warehouse_a_shipment = divide_with_ceil(multiply(total_demand, 30), 100)  // 30%
  let warehouse_b_shipment = divide_with_ceil(multiply(total_demand, 45), 100)  // 45%
  let warehouse_c_shipment = total_demand - warehouse_a_shipment - warehouse_b_shipment  // 剩余25%
  
  // 验证总配送量
  let total_shipment = add(add(warehouse_a_shipment, warehouse_b_shipment), warehouse_c_shipment)
  
  assert_eq(120, warehouse_a_shipment)  // ceil(400*30/100)
  assert_eq(180, warehouse_b_shipment)  // ceil(400*45/100)
  assert_eq(100, warehouse_c_shipment)  // 400 - 120 - 180
  assert_eq(400, total_shipment)
}

test "data_structure_simulation_stack" {
  // 数据结构模拟：栈操作的数学表示
  let stack_size = 0
  let push_operations = 5
  let pop_operations = 3
  
  // 模拟push操作
  let after_push = add(stack_size, push_operations)
  
  // 模拟pop操作
  let after_pop = add(after_push, -pop_operations)
  
  // 验证栈的大小变化
  assert_eq(5, after_push)
  assert_eq(2, after_pop)
}

test "temperature_conversion_calculations" {
  // 温度转换计算：摄氏度与华氏度之间的转换
  // 使用简化公式：F = C * 9/5 + 32
  let celsius_1 = 0
  let celsius_2 = 100
  let celsius_3 = -40
  
  // 转换为华氏度（使用整数运算）
  let fahrenheit_1 = add(divide_with_ceil(multiply(celsius_1, 9), 5), 32)
  let fahrenheit_2 = add(divide_with_ceil(multiply(celsius_2, 9), 5), 32)
  let fahrenheit_3 = add(divide_with_ceil(multiply(celsius_3, 9), 5), 32)
  
  assert_eq(32, fahrenheit_1)   // 0°C = 32°F
  assert_eq(212, fahrenheit_2)  // 100°C = 212°F
  assert_eq(-40, fahrenheit_3)  // -40°C = -40°F（特殊点）
}

test "game_score_calculation" {
  // 游戏得分计算：多关卡得分与奖励计算
  let base_score_per_level = 100
  let levels_completed = 8
  let bonus_threshold = 5
  let bonus_per_extra_level = 50
  
  // 计算基础得分
  let base_score = multiply(base_score_per_level, levels_completed)
  
  // 计算奖励得分（超过5关后的额外奖励）
  let extra_levels = levels_completed - bonus_threshold
  let bonus_score = if extra_levels > 0 {
    multiply(extra_levels, bonus_per_extra_level)
  } else {
    0
  }
  
  // 计算总分
  let total_score = add(base_score, bonus_score)
  
  assert_eq(800, base_score)   // 100 * 8
  assert_eq(150, bonus_score)  // 3 * 50
  assert_eq(950, total_score)  // 800 + 150
}

test "project_resource_allocation" {
  // 项目资源分配：多项目资源分配优化
  let total_budget = 50000
  let project_a_priority = 3
  let project_b_priority = 2
  let project_c_priority = 1
  let total_priority = add(add(project_a_priority, project_b_priority), project_c_priority)
  
  // 按优先级分配资源
  let project_a_budget = divide_with_ceil(multiply(total_budget, project_a_priority), total_priority)
  let project_b_budget = divide_with_ceil(multiply(total_budget, project_b_priority), total_priority)
  let project_c_budget = total_budget - project_a_budget - project_b_budget
  
  assert_eq(25000, project_a_budget)  // ceil(50000*3/6)
  assert_eq(16667, project_b_budget)  // ceil(50000*2/6)
  assert_eq(8333, project_c_budget)   // 50000 - 25000 - 16667
}

test "string_processing_with_numbers" {
  // 字符串与数字处理：生成带编号的标签
  // 直接使用预定义的字符串进行测试
  let label1 = greet("Item 1")
  let label2 = greet("Item 2")
  let label3 = greet("Item 3")
  
  assert_eq_string("Hello, Item 1!", label1)
  assert_eq_string("Hello, Item 2!", label2)
  assert_eq_string("Hello, Item 3!", label3)
}

test "error_handling_edge_cases" {
  // 错误处理边界情况：极值运算测试
  let max_safe_value = 1000000
  let min_safe_value = -1000000
  
  // 测试极值加法
  let max_addition = add(max_safe_value, max_safe_value)
  let min_addition = add(min_safe_value, min_safe_value)
  let mixed_addition = add(max_safe_value, min_safe_value)
  
  // 测试极值除法
  let max_division = divide_with_ceil(max_safe_value, 1)
  let min_division = divide_with_ceil(min_safe_value, 1)
  let zero_division = divide_with_ceil(max_safe_value, 0)
  
  assert_eq(2000000, max_addition)
  assert_eq(-2000000, min_addition)
  assert_eq(0, mixed_addition)
  assert_eq(1000000, max_division)
  assert_eq(-1000000, min_division)
  assert_eq(0, zero_division)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：比较不同算法的性能特征
  let input_size = 1000
  
  // 模拟O(n)线性算法
  let linear_steps = multiply(input_size, 2)
  
  // 模拟O(n²)平方算法
  let quadratic_steps = divide_with_ceil(multiply(input_size, input_size), 500)
  
  // 模拟O(log n)对数算法（简化实现）
  let logarithmic_steps = divide_with_ceil(input_size, 100)
  
  // 计算性能比率
  let linear_to_log_ratio = divide_with_ceil(linear_steps, logarithmic_steps)
  let quadratic_to_linear_ratio = divide_with_ceil(quadratic_steps, linear_steps)
  
  assert_eq(2000, linear_steps)
  assert_eq(2000, quadratic_steps)
  assert_eq(10, logarithmic_steps)
  assert_eq(200, linear_to_log_ratio)
  assert_eq(1, quadratic_to_linear_ratio)
}