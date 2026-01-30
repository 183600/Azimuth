// Azimuth 综合单元测试
// 包含 8 个高质量的 MoonBit 测试用例，覆盖核心功能的实际应用场景

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_calculator_workflow" {
  // 复杂计算器工作流测试
  // 模拟实际计算场景：计算折扣后的价格
  let original_price = 100
  let discount_percentage = 15
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percentage), 100)
  let final_price = subtract(original_price, discount_amount)
  
  assert_eq(15, discount_amount)
  assert_eq(85, final_price)
}

test "batch_processing_calculation" {
  // 批处理计算测试
  // 场景：计算多批物品的总处理时间
  let batch1_size = 25
  let batch2_size = 30
  let batch3_size = 45
  let processing_time_per_item = 2
  
  let total_items = add(add(batch1_size, batch2_size), batch3_size)
  let total_time = multiply(total_items, processing_time_per_item)
  let time_saved = divide_with_ceil(total_items, 10)  // 每10个物品节省1个时间单位
  
  assert_eq(100, total_items)
  assert_eq(200, total_time)
  assert_eq(10, time_saved)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  // 场景：项目资源分配规划
  let project_duration = 12  // 月
  let team_size = 8
  let budget_per_month = 5000
  let total_budget = multiply(project_duration, budget_per_month)
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  
  assert_eq(60000, total_budget)
  assert_eq(7500, budget_per_person)
  
  // 验证总分配不超过预算
  let total_allocated = multiply(budget_per_person, team_size)
  assert_eq(60000, total_allocated)
}

test "string_validation_and_processing" {
  // 字符串验证和处理测试
  // 测试各种输入类型的问候函数
  assert_eq_string("Hello, Admin123!", greet("Admin123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, special&chars*!", greet("special&chars*"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
  assert_eq_string("Hello, CamelCase!", greet("CamelCase"))
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 场景：计算等差数列的和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 计算最后一项
  let last_term = add(first_term, multiply(common_difference, subtract(number_of_terms, 1)))
  
  // 计算数列的和：(首项 + 末项) * 项数 / 2
  let sequence_sum = divide_with_ceil(multiply(add(first_term, last_term), number_of_terms), 2)
  
  assert_eq(23, last_term)  // 3 + 4 * (6-1) = 23
  assert_eq(78, sequence_sum)  // ceil((3+23)*6/2) = 78
}

test "performance_metrics_calculation" {
  // 性能指标计算测试
  // 场景：计算系统性能指标
  let total_requests = 1000
  let successful_requests = 850
  let failed_requests = subtract(total_requests, successful_requests)
  let success_rate = multiply(divide_with_ceil(multiply(successful_requests, 100), total_requests), 1)
  let error_rate = multiply(divide_with_ceil(multiply(failed_requests, 100), total_requests), 1)
  
  assert_eq(150, failed_requests)
  assert_eq(85, success_rate)
  assert_eq(15, error_rate)
  
  // 验证总百分比为100%
  assert_eq(100, add(success_rate, error_rate))
}

test "inventory_optimization_problem" {
  // 库存优化问题测试
  // 场景：多产品库存优化
  let product_a_demand = 120
  let product_b_demand = 85
  let product_c_demand = 200
  
  let box_capacity_a = 25
  let box_capacity_b = 20
  let box_capacity_c = 45
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_demand, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_demand, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_demand, box_capacity_c)
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  
  assert_eq(5, boxes_a)    // ceil(120/25) = 5
  assert_eq(5, boxes_b)    // ceil(85/20) = 5
  assert_eq(5, boxes_c)    // ceil(200/45) = 5
  assert_eq(15, total_boxes)
  assert_eq(450, total_capacity)  // 5*25 + 5*20 + 5*45 = 125 + 100 + 225 = 450
}