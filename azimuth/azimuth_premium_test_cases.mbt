// Azimuth 高级测试用例
// 包含10个标准 MoonBit 测试用例，覆盖各种高级场景

test "premium_arithmetic_precision" {
  // 高级算术精度测试
  assert_eq(1000001, add(1000000, 1))
  assert_eq(999999, subtract(1000000, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, divide_with_ceil(1000001, 10))
}

test "premium_negative_number_operations" {
  // 高级负数运算测试
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(-10, divide_with_ceil(-95, 10))
  assert_eq(10, divide_with_ceil(-95, -10))
}

test "premium_string_unicode_handling" {
  // 高级字符串 Unicode 处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌟✨!", greet("🌟✨"))
}

test "premium_business_inventory_management" {
  // 高级库存管理业务测试
  let total_products = 487
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(21, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(504, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
  
  // 验证空间利用率
  let utilization_percentage = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(97, utilization_percentage)
}

test "premium_complex_calculation_workflow" {
  // 高级复杂计算工作流测试
  // 场景：多阶段项目资源计算
  let phase1_team = 8
  let phase1_duration = 3
  let phase2_team = 12
  let phase2_duration = 4
  let phase3_team = 6
  let phase3_duration = 2
  
  // 计算各阶段总人时
  let phase1_hours = multiply(phase1_team, multiply(phase1_duration, 40))  // 8*3*40 = 960
  let phase2_hours = multiply(phase2_team, multiply(phase2_duration, 40))  // 12*4*40 = 1920
  let phase3_hours = multiply(phase3_team, multiply(phase3_duration, 40))  // 6*2*40 = 480
  
  // 计算项目总人时
  let total_hours = add(add(phase1_hours, phase2_hours), phase3_hours)
  assert_eq(3360, total_hours)
  
  // 计算需要的冲刺数（每个冲刺 400 小时）
  let sprint_capacity = 400
  let sprints_needed = divide_with_ceil(total_hours, sprint_capacity)
  assert_eq(9, sprints_needed)
}

test "premium_mathematical_properties_validation" {
  // 高级数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 验证具体数值
  assert_eq(22, add(15, 7))
  assert_eq(105, multiply(15, 7))
  assert_eq(66, multiply(15, add(7, 3)))
  assert_eq(210, add(multiply(15, 7), multiply(15, 3)))
}

test "premium_edge_cases_and_boundary_values" {
  // 高级边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000000, 0))
  assert_eq(0, divide_with_ceil(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1000000, multiply(1000000, 1))
  assert_eq(1000000, add(1000000, 0))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(2, divide_with_ceil(1001, 1000))
}

test "premium_error_handling_robustness" {
  // 高级错误处理健壮性测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_test_value = 1000000
  let min_test_value = -1000000
  
  assert_eq(0, multiply(max_test_value, 0))
  assert_eq(0, multiply(min_test_value, 0))
  assert_eq(max_test_value, add(max_test_value, 0))
  assert_eq(min_test_value, add(min_test_value, 0))
  assert_eq(0, subtract(max_test_value, max_test_value))
  assert_eq(0, subtract(min_test_value, min_test_value))
}

test "premium_real_world_event_planning" {
  // 高级真实世界事件规划测试
  let attendees = 342
  let tables_available = 40
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 8000
  let staff_per_table = 1
  let staff_cost_per_person = 150
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(43, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(344, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(2, remaining_seats)
  
  // 计算椅子总成本
  let chair_cost = multiply(total_seats, cost_per_chair)
  assert_eq(8600, chair_cost)
  
  // 计算工作人员成本
  let staff_needed = tables_needed
  let staff_cost = multiply(staff_needed, staff_cost_per_person)
  assert_eq(6450, staff_cost)
  
  // 计算总成本
  let total_cost = add(chair_cost, staff_cost)
  assert_eq(15050, total_cost)
  
  // 验证预算充足性（这里应该失败，因为成本超过预算）
  assert_true(total_cost > total_budget)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(7050, budget_shortage)
}

test "premium_financial_calculation_scenario" {
  // 高级金融计算场景测试
  let loan_amount = 250000
  let interest_rate = 5  // 5%
  let loan_term_years = 30
  let monthly_payment_limit = 1500
  
  // 计算总利息
  let total_interest = divide_with_ceil(multiply(loan_amount, interest_rate), 100)
  assert_eq(12500, total_interest)
  
  // 计算总还款金额
  let total_repayment = add(loan_amount, total_interest)
  assert_eq(262500, total_repayment)
  
  // 计算总月数
  let total_months = multiply(loan_term_years, 12)
  assert_eq(360, total_months)
  
  // 计算最低月供
  let min_monthly_payment = divide_with_ceil(total_repayment, total_months)
  assert_eq(730, min_monthly_payment)
  
  // 验证月供在限制范围内
  assert_true(min_monthly_payment <= monthly_payment_limit)
  
  // 计算实际月供（使用限制值）
  let actual_months_needed = divide_with_ceil(total_repayment, monthly_payment_limit)
  assert_eq(175, actual_months_needed)
  
  // 计算实际还款年限
  let actual_years = divide_with_ceil(actual_months_needed, 12)
  assert_eq(15, actual_years)
  
  // 计算节省的年限
  let years_saved = subtract(loan_term_years, actual_years)
  assert_eq(15, years_saved)
}