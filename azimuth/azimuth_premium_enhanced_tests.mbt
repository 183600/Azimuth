// Azimuth 高质量增强测试用例
// 包含10个精心设计的标准 MoonBit 测试用例
// 覆盖边界情况、实际应用场景和复杂计算逻辑

test "enhanced_mathematical_properties" {
  // 增强数学性质测试：验证交换律、结合律和分配律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律验证
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "comprehensive_divide_with_ceil_scenarios" {
  // 综合向上取整除法场景测试
  // 正数场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(10, 10))
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：电商订单处理
  let orders = 127
  let items_per_box = 24
  let boxes_needed = divide_with_ceil(orders, items_per_box)
  
  assert_eq(6, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(144, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, orders)
  assert_eq(17, remaining_space)
  
  // 验证空间利用率
  let utilization = divide_with_ceil(multiply(orders, 100), total_capacity)
  assert_eq(89, utilization)  // floor(12700/144) = 88%
}

test "edge_cases_and_extreme_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 1000))
  assert_eq(0, divide_with_ceil(0, 1000))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 负数极值测试
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(100000000, multiply(-10000, -10000))
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算额外利息
  let extra_interest = subtract(total_payment, loan_amount)
  assert_eq(600, extra_interest)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试：项目团队分配
  let total_tasks = 95
  let team_members = 6
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(16, tasks_per_member)
  
  // 验证分配结果
  let allocated_tasks = multiply(tasks_per_member, team_members)
  assert_eq(96, allocated_tasks)
  
  // 计算未分配任务
  let unallocated_tasks = subtract(allocated_tasks, total_tasks)
  assert_eq(1, unallocated_tasks)
  
  // 验证每个成员的工作量平衡
  let base_tasks = divide_with_ceil(total_tasks, team_members)
  assert_true(tasks_per_member >= base_tasks)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误全面测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 极值运算健壮性
  let large_num = 100000
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(-large_num, multiply(large_num, -1))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试：会议安排
  let participants = 156
  let tables_available = 20
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
}

test "advanced_calculation_pipeline" {
  // 高级计算流水线测试：多步骤复杂计算
  let initial_value = 15
  let factor_a = 3
  let factor_b = 4
  let divisor = 7
  let offset = 8
  
  // 复杂计算流程
  let step1 = multiply(initial_value, factor_a)  // 15 * 3 = 45
  let step2 = add(step1, multiply(factor_b, 2))  // 45 + 4*2 = 53
  let step3 = divide_with_ceil(step2, divisor)   // ceil(53/7) = 8
  let step4 = multiply(step3, 2)                 // 8 * 2 = 16
  let final_result = subtract(step4, offset)     // 16 - 8 = 8
  
  assert_eq(8, final_result)
  
  // 验证中间步骤
  assert_eq(45, step1)
  assert_eq(53, step2)
  assert_eq(8, step3)
  assert_eq(16, step4)
}