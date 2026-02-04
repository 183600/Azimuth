// Azimuth 项目高质量测试用例
// 包含10个标准 MoonBit 测试用例，覆盖核心功能和边界情况

test "arithmetic_comprehensive_operations" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(3, subtract(10, 7))
  assert_eq(4, divide_with_ceil(10, 3))
  
  // 负数运算
  assert_eq(-2, add(-5, 3))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(-13, subtract(-8, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 4
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位元测试
  assert_eq(7, add(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(7, subtract(7, 0))
  assert_eq(7, divide_with_ceil(7, 1))
  
  // 相等数测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, subtract(5, 5))
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_products = 250
  let box_capacity = 30
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(20, remaining_space)
  
  // 计算空间利用率
  let utilization_percentage = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(93, utilization_percentage)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 100000
  let monthly_payment = 3500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(29, months_needed)
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(101500, total_payment)
  
  // 计算额外支付金额
  let extra_payment = subtract(total_payment, loan_amount)
  assert_eq(1500, extra_payment)
  
  // 计算月均实际支付
  let average_payment = divide_with_ceil(total_payment, months_needed)
  assert_eq(3500, average_payment)
}

test "complex_workflow_pipeline" {
  // 复杂工作流流水线测试
  let initial_value = 15
  let multiplier = 4
  let subtractor = 7
  let divisor = 3
  let addition = 12
  
  // 计算流程：((initial * multiplier - subtractor) / divisor) + addition
  let step1 = multiply(initial_value, multiplier)
  let step2 = subtract(step1, subtractor)
  let step3 = divide_with_ceil(step2, divisor)
  let result = add(step3, addition)
  
  assert_eq(25, result)  // ceil((15*4-7)/3) + 12 = ceil(53/3) + 12 = 18 + 12 = 30
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_safe_value = 1000000
  let min_safe_value = -1000000
  
  assert_eq(0, multiply(max_safe_value, 0))
  assert_eq(0, multiply(min_safe_value, 0))
  assert_eq(max_safe_value, add(max_safe_value, 0))
  assert_eq(min_safe_value, add(min_safe_value, 0))
  assert_eq(0, subtract(max_safe_value, max_safe_value))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 185
  let teams = 4
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(47, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(188, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(99, utilization)
  
  // 计算剩余资源
  let remaining_resources = subtract(allocated_total, total_resources)
  assert_eq(3, remaining_resources)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 156
  let tables_available = 20
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 检查桌子是否足够
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
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(0, remaining_budget)
}