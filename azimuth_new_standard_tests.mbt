// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "compound_calculations_financial_scenario" {
  // 复合计算金融场景测试
  // 场景：计算投资回报率
  let initial_investment = 10000
  let monthly_return = 150
  let months = 12
  
  // 计算总回报
  let total_return = multiply(monthly_return, months)
  
  // 计算最终金额
  let final_amount = add(initial_investment, total_return)
  
  // 计算需要的投资周期数（目标金额20000）
  let target_amount = 20000
  let additional_needed = subtract(target_amount, final_amount)
  let additional_months = divide_with_ceil(additional_needed, monthly_return)
  
  assert_eq(1800, total_return)  // 150 * 12
  assert_eq(11800, final_amount)  // 10000 + 1800
  assert_eq(7, additional_months)  // ceil(8200/150) = 55
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  // 场景：计算最优订货量
  let annual_demand = 1200
  let ordering_cost = 50
  let holding_cost_per_unit = 2
  
  // 计算经济订货量 (EOQ) 简化版本
  let demand_per_month = divide_with_ceil(annual_demand, 12)
  let orders_per_month = divide_with_ceil(demand_per_month, 100)
  
  let monthly_ordering_cost = multiply(orders_per_month, ordering_cost)
  let average_inventory = divide_with_ceil(demand_per_month, 2)
  let monthly_holding_cost = multiply(average_inventory, holding_cost_per_unit)
  
  let total_monthly_cost = add(monthly_ordering_cost, monthly_holding_cost)
  
  assert_eq(100, demand_per_month)  // ceil(1200/12)
  assert_eq(1, orders_per_month)  // ceil(100/100)
  assert_eq(50, monthly_ordering_cost)
  assert_eq(50, average_inventory)  // ceil(100/2)
  assert_eq(100, monthly_holding_cost)
  assert_eq(150, total_monthly_cost)
}

test "string_processing_validation" {
  // 字符串处理验证测试
  // 测试各种特殊情况的问候处理
  
  // 测试包含特殊字符的名称
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Connor!", greet("O'Connor"))
  
  // 测试数字和字母混合
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, 2024Year!", greet("2024Year"))
  
  // 测试包含空格的复合名称
  assert_eq_string("Hello, John Smith!", greet("John Smith"))
  
  // 测试单字符名称
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 5!", greet("5"))
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 场景：计算等差数列和等比数列
  
  // 等差数列：1, 3, 5, 7, 9 的和
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 5
  
  let last_term = add(first_term, multiply(common_difference, subtract(number_of_terms, 1)))
  let arithmetic_sum = divide_with_ceil(multiply(number_of_terms, add(first_term, last_term)), 2)
  
  // 等比数列：2, 4, 8, 16 的积（简化计算）
  let geometric_first = 2
  let ratio = 2
  let geo_terms = 4
  
  let geometric_product = multiply(geometric_first, multiply(ratio, multiply(ratio, ratio)))
  
  assert_eq(9, last_term)  // 1 + 2*(5-1) = 9
  assert_eq(25, arithmetic_sum)  // ceil(5*(1+9)/2) = 25
  assert_eq(16, geometric_product)  // 2 * 2 * 2 * 2 = 16
}

test "resource_allocation_algorithm" {
  // 资源分配算法测试
  // 场景：将任务分配给多个团队
  
  let total_tasks = 87
  let team_a_capacity = 15
  let team_b_capacity = 12
  let team_c_capacity = 18
  
  // 计算每个团队需要的轮数
  let team_a_rounds = divide_with_ceil(total_tasks, team_a_capacity)
  let team_b_rounds = divide_with_ceil(total_tasks, team_b_capacity)
  let team_c_rounds = divide_with_ceil(total_tasks, team_c_capacity)
  
  // 计算总工作量（轮数 * 容量）
  let team_a_total_work = multiply(team_a_rounds, team_a_capacity)
  let team_b_total_work = multiply(team_b_rounds, team_b_capacity)
  let team_c_total_work = multiply(team_c_rounds, team_c_capacity)
  
  // 找出最优团队（最小总工作量）
  assert_eq(6, team_a_rounds)  // ceil(87/15)
  assert_eq(8, team_b_rounds)  // ceil(87/12)
  assert_eq(5, team_c_rounds)  // ceil(87/18)
  
  assert_eq(90, team_a_total_work)
  assert_eq(96, team_b_total_work)
  assert_eq(90, team_c_total_work)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  // 场景：项目时间管理和排期
  
  let project_hours = 480
  let work_hours_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日数
  let work_days_needed = divide_with_ceil(project_hours, work_hours_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算剩余时间
  let total_allocated_hours = multiply(work_days_needed, work_hours_per_day)
  let remaining_hours = subtract(total_allocated_hours, project_hours)
  
  // 计算实际工作周数（考虑剩余时间不足一周）
  let full_weeks = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(60, work_days_needed)  // ceil(480/8)
  assert_eq(12, weeks_needed)  // ceil(60/5)
  assert_eq(480, total_allocated_hours)
  assert_eq(0, remaining_hours)
  assert_eq(12, full_weeks)
}

test "boundary_stress_testing" {
  // 边界压力测试
  // 测试极值和边界情况的处理
  
  // 大数运算压力测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  assert_eq(199999, add(large_num1, large_num2))
  assert_eq(10000000000, multiply(100000, 100000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, divide_with_ceil(large_num1, 2)))
  
  // 零值边界测试
  assert_eq(0, multiply(0, large_num1))
  assert_eq(large_num1, add(large_num1, 0))
  assert_eq(0, divide_with_ceil(0, large_num1))
  assert_eq(0, divide_with_ceil(large_num1, 0))
  
  // 负数边界测试
  let negative_large = -100000
  assert_eq(0, add(large_num1, negative_large))
  assert_eq(-9999900000, multiply(large_num1, negative_large))
  assert_eq(-1, divide_with_ceil(negative_large, large_num1))
}

test "data_validation_scenarios" {
  // 数据验证场景测试
  // 场景：输入数据验证和边界检查
  
  // 年龄验证场景
  let min_age = 18
  let max_age = 65
  let applicant_age = 42
  
  let is_valid_age = applicant_age >= min_age && applicant_age <= max_age
  let years_to_retirement = subtract(max_age, applicant_age)
  let years_since_eligibility = subtract(applicant_age, min_age)
  
  assert_true(is_valid_age)
  assert_eq(23, years_to_retirement)
  assert_eq(24, years_since_eligibility)
  
  // 评分验证场景
  let max_score = 100
  let passing_score = 60
  let student_score = 85
  
  let percentage_score = divide_with_ceil(multiply(student_score, 100), max_score)
  let is_passing = student_score >= passing_score
  let points_above_passing = subtract(student_score, passing_score)
  
  assert_eq(85, percentage_score)
  assert_true(is_passing)
  assert_eq(25, points_above_passing)
}

test "algorithmic_complexity_test" {
  // 算法复杂度测试
  // 场景：模拟不同复杂度的算法运算
  
  // O(1) 常数时间操作
  let constant_result = add(5, 3)
  
  // O(n) 线性操作模拟
  let array_size = 100
  let linear_operations = multiply(array_size, 2)
  
  // O(n²) 二次操作模拟
  let nested_operations = multiply(array_size, array_size)
  
  // O(log n) 对数操作模拟（简化）
  let log_operations = divide_with_ceil(array_size, 2)
  
  // 计算总操作数
  let total_operations = add(linear_operations, add(nested_operations, log_operations))
  
  assert_eq(8, constant_result)
  assert_eq(200, linear_operations)
  assert_eq(10000, nested_operations)
  assert_eq(50, log_operations)
  assert_eq(10250, total_operations)
}

test "real_world_business_logic" {
  // 真实世界业务逻辑测试
  // 场景：电商订单处理和物流计算
  
  let order_items = 47
  let items_per_box = 12
  let cost_per_box = 5
  let shipping_cost_per_box = 8
  let discount_threshold = 50
  let discount_rate = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  // 计算箱子成本
  let box_cost = multiply(boxes_needed, cost_per_box)
  
  // 计算运输成本
  let shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  
  // 计算总成本
  let total_cost = add(box_cost, shipping_cost)
  
  // 判断是否享受折扣
  let qualifies_for_discount = order_items >= discount_threshold
  let discount_amount = 0
  if qualifies_for_discount {
    discount_amount = divide_with_ceil(multiply(total_cost, discount_rate), 100)
  }
  
  let final_cost = subtract(total_cost, discount_amount)
  
  assert_eq(4, boxes_needed)  // ceil(47/12)
  assert_eq(20, box_cost)     // 4 * 5
  assert_eq(32, shipping_cost)  // 4 * 8
  assert_eq(52, total_cost)   // 20 + 32
  assert_false(qualifies_for_discount)  // 47 < 50
  assert_eq(0, discount_amount)
  assert_eq(52, final_cost)
}