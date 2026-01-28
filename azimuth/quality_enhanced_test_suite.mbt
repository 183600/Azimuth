// 质量增强测试套件 - 标准MoonBit测试用例
// 为azimuth库提供额外的测试覆盖

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
}

test "arithmetic_sequence_test" {
  // 测试算术运算序列
  let initial = 20
  let step1 = subtract(initial, 5)      // 15
  let step2 = add(step1, 10)            // 25
  let step3 = multiply(step2, 2)        // 50
  let step4 = divide_with_ceil(step3, 12) // 5
  assert_eq(5, step4)
}

test "inventory_calculation_test" {
  // 业务库存计算测试
  let initial_stock = 150
  let sold_items = 47
  let returned_items = 8
  let new_stock = 35
  
  // 计算最终库存
  let current_stock = subtract(initial_stock, sold_items)
  let after_returns = add(current_stock, returned_items)
  let final_stock = add(after_returns, new_stock)
  
  assert_eq(146, final_stock)
  
  // 计算需要的存储箱数（每箱装25个）
  let boxes_needed = divide_with_ceil(final_stock, 25)
  assert_eq(6, boxes_needed)
}

test "mathematical_properties_test" {
  // 数学运算性质测试
  let x = 12
  let y = 8
  
  // 减法的非交换性验证
  assert_eq(subtract(x, y), 4)
  assert_eq(subtract(y, x), -4)
  
  // 减法与加法的关系
  assert_eq(subtract(x, y), add(x, multiply(y, -1)))
}

test "error_boundary_test" {
  // 错误处理和边界情况测试
  // 测试极值情况
  assert_eq(2147483647, subtract(2147483647, 0))
  assert_eq(-2147483648, subtract(-2147483648, 0))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_test" {
  // 字符串问候测试
  assert_eq_string("Hello, JavaScript!", greet("JavaScript"))
  assert_eq_string("Hello, TypeScript!", greet("TypeScript"))
  assert_eq_string("Hello, Python!", greet("Python"))
  assert_eq_string("Hello, Rust!", greet("Rust"))
  assert_eq_string("Hello, Go!", greet("Go"))
}

test "complex_workflow_test" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 12
  let project_duration = 8  // 周
  let hours_per_week = 40
  let budget_per_hour = 75
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(project_duration, hours_per_week))
  assert_eq(3840, total_hours)  // 12 * 8 * 40 = 3840
  
  // 计算总预算
  let total_budget = multiply(total_hours, budget_per_hour)
  assert_eq(288000, total_budget)  // 3840 * 75 = 288000
  
  // 计算需要的支付次数（每次支付不超过50000）
  let payment_limit = 50000
  let payments_needed = divide_with_ceil(total_budget, payment_limit)
  assert_eq(6, payments_needed)  // ceil(288000/50000) = 6
}

test "sequence_calculation_test" {
  // 数学序列计算测试
  // 计算等差数列和等比数列
  let first_term = 3
  let difference = 4
  let terms_count = 6
  
  // 等差数列第n项：a + (n-1)d
  let nth_term = add(first_term, multiply(difference, subtract(terms_count, 1)))
  assert_eq(23, nth_term)  // 3 + (6-1)*4 = 23
  
  // 等差数列前n项和：n/2 * (2a + (n-1)d)
  let sum_first_n = multiply(terms_count, divide_with_ceil(add(multiply(2, first_term), multiply(difference, subtract(terms_count, 1))), 2))
  assert_eq(78, sum_first_n)  // 6/2 * (2*3 + 5*4) = 3 * 26 = 78
}

test "resource_optimization_test" {
  // 资源优化场景测试
  // 场景：生产计划优化
  let product_a_demand = 120
  let product_b_demand = 85
  let product_c_demand = 200
  
  let machine_a_capacity = 50  // 每台机器A的生产能力
  let machine_b_capacity = 40  // 每台机器B的生产能力
  let machine_c_capacity = 60  // 每台机器C的生产能力
  
  // 计算需要的机器数量
  let machines_a_needed = divide_with_ceil(product_a_demand, machine_a_capacity)
  let machines_b_needed = divide_with_ceil(product_b_demand, machine_b_capacity)
  let machines_c_needed = divide_with_ceil(product_c_demand, machine_c_capacity)
  
  assert_eq(3, machines_a_needed)   // ceil(120/50) = 3
  assert_eq(3, machines_b_needed)   // ceil(85/40) = 3
  assert_eq(4, machines_c_needed)   // ceil(200/60) = 4
  
  // 计算总机器数量
  let total_machines = add(add(machines_a_needed, machines_b_needed), machines_c_needed)
  assert_eq(10, total_machines)
}

test "function_integration_test" {
  // 综合函数集成测试
  // 测试所有函数的协同工作
  let base_value = 15
  let multiplier = 3
  let subtractor = 7
  let divisor = 4
  
  // 复杂的计算链
  let step1 = multiply(base_value, multiplier)        // 15 * 3 = 45
  let step2 = subtract(step1, subtractor)             // 45 - 7 = 38
  let step3 = add(step2, base_value)                  // 38 + 15 = 53
  let step4 = divide_with_ceil(step3, divisor)        // ceil(53/4) = 14
  let step5 = multiply(step4, 2)                      // 14 * 2 = 28
  let final_result = subtract(step5, base_value)      // 28 - 15 = 13
  
  assert_eq(13, final_result)
  
  // 验证中间步骤
  assert_eq(45, step1)
  assert_eq(38, step2)
  assert_eq(53, step3)
  assert_eq(14, step4)
  assert_eq(28, step5)
}