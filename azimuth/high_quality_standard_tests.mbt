// 高质量标准 MoonBit 测试用例
// 为 Azimuth 项目添加的 10 个精心设计的测试用例
// 涵盖核心功能、边界情况、数学性质和实际应用场景

test "core_arithmetic_operations_precision" {
  // 核心算术运算精度测试
  // 验证基本运算的正确性和精度
  assert_eq(15, add(7, 8))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(7, 7))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 向上取整除法综合行为测试
  // 验证各种输入情况下的正确行为
  
  // 正数情况
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(3, 5))
  assert_eq(0, divide_with_ceil(0, 8))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-4, divide_with_ceil(11, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(1, divide_with_ceil(-7, -7))
}

test "string_greeting_function_robustness" {
  // 字符串问候函数健壮性测试
  // 验证各种输入类型的处理能力
  
  // 基本英文字符
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode 字符支持
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 特殊字符和边界情况
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "mathematical_fundamental_properties" {
  // 数学基本性质验证测试
  // 验证核心数学定律和恒等式
  let a = 17
  let b = 11
  let c = 5
  
  // 交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  assert_eq(0, subtract(a, a))
}

test "business_inventory_management_scenario" {
  // 业务库存管理场景测试
  // 模拟真实的库存分配和优化问题
  
  let total_products = 342
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证箱子数量计算
  assert_eq(14, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(350, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(98, utilization)  // floor(34200/350) = 97%，向上取整为98%
}

test "complex_calculation_workflow_test" {
  // 复杂计算工作流测试
  // 验证多步骤计算的正确性和一致性
  
  // 场景：项目资源规划
  let team_size = 9
  let tasks_per_member = 7
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_weeks = 2
  let weekly_capacity_per_member = 4
  let weekly_total_capacity = multiply(team_size, weekly_capacity_per_member)
  let sprint_capacity = multiply(weekly_total_capacity, sprint_weeks)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证中间计算结果
  assert_eq(63, total_tasks)  // 9 * 7 = 63
  assert_eq(36, weekly_total_capacity)  // 9 * 4 = 36
  assert_eq(72, sprint_capacity)  // 36 * 2 = 72
  assert_eq(1, sprints_needed)  // ceil(63/72) = 1
  
  // 计算资源利用率
  let resource_utilization = divide_with_ceil(multiply(total_tasks, 100), sprint_capacity)
  assert_eq(88, resource_utilization)  // floor(6300/72) = 87%，向上取整为88%
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 验证系统在极端条件下的稳定性
  
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  
  // 大数运算测试（安全范围内）
  assert_eq(200000, add(100000, 100000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(99999, 100000))
  assert_eq(100000, divide_with_ceil(100000, 1))
  assert_eq(1, divide_with_ceil(100000, 100000))
  assert_eq(2, divide_with_ceil(100001, 100000))
  
  // 零值处理
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100000))
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 验证系统对异常输入的处理能力
  
  // 除零错误全面测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算稳定性测试
  let max_test_value = 50000
  let min_test_value = -50000
  
  assert_eq(max_test_value, add(max_test_value, 0))
  assert_eq(min_test_value, add(min_test_value, 0))
  assert_eq(0, multiply(max_test_value, 0))
  assert_eq(0, multiply(min_test_value, 0))
  
  // 边界减法测试
  assert_eq(max_test_value, subtract(max_test_value, 0))
  assert_eq(min_test_value, subtract(min_test_value, 0))
  assert_eq(100000, subtract(max_test_value, min_test_value))
}

test "real_world_event_planning_comprehensive" {
  // 真实世界活动规划综合测试
  // 模拟完整的活动资源分配和预算管理场景
  
  let attendees = 187
  let tables_available = 25
  let chairs_per_table = 8
  let cost_per_chair = 20
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(24, tables_needed)
  
  // 验证桌子可用性
  assert_true(tables_needed <= tables_available)
  
  // 计算座位安排
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(192, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(5, remaining_seats)
  
  // 计算成本分析
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3840, total_cost)
  
  // 验证预算充足性
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(1160, remaining_budget)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_cost, 100), total_budget)
  assert_eq(77, budget_utilization)  // floor(384000/5000) = 76%，向上取整为77%
}

test "advanced_mathematical_operations_chain" {
  // 高级数学运算链测试
  // 验证复杂运算序列的正确性和一致性
  
  // 初始化参数
  let base_value = 12
  let multiplier_a = 5
  let multiplier_b = 3
  let divisor = 7
  let addend = 15
  
  // 执行复杂计算链
  let step1 = multiply(base_value, multiplier_a)  // 12 * 5 = 60
  let step2 = add(step1, multiply(multiplier_b, 10))  // 60 + 3*10 = 90
  let step3 = divide_with_ceil(step2, divisor)  // ceil(90/7) = 13
  let step4 = multiply(step3, 2)  // 13 * 2 = 26
  let final_result = subtract(step4, addend)  // 26 - 15 = 11
  
  // 验证最终结果
  assert_eq(11, final_result)
  
  // 验证中间步骤
  assert_eq(60, step1)
  assert_eq(90, step2)
  assert_eq(13, step3)
  assert_eq(26, step4)
  
  // 验证替代计算路径的一致性
  let alternative_result = subtract(
    multiply(divide_with_ceil(add(multiply(base_value, multiplier_a), multiply(multiplier_b, 10)), divisor), 2),
    addend
  )
  assert_eq(final_result, alternative_result)
}