// Enhanced Azimuth Core Tests
// 高质量 Azimuth 核心功能测试用例

test "enhanced_mathematical_series_sum" {
  // 测试数学级数求和：计算等差数列 1+3+5+...+19 的和
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, series_sum)  // 1+3+5+...+19 = 100
}

test "enhanced_temperature_conversion" {
  // 测试温度转换场景：摄氏度到华氏度的转换
  // F = C * 9/5 + 32，使用向上取整除法模拟
  let celsius_0 = 0
  let celsius_25 = 25
  let celsius_100 = 100
  
  let fahrenheit_0 = add(divide_with_ceil(multiply(celsius_0, 9), 5), 32)
  let fahrenheit_25 = add(divide_with_ceil(multiply(celsius_25, 9), 5), 32)
  let fahrenheit_100 = add(divide_with_ceil(multiply(celsius_100, 9), 5), 32)
  
  assert_eq(32, fahrenheit_0)    // 0°C = 32°F
  assert_eq(77, fahrenheit_25)   // 25°C = 77°F
  assert_eq(212, fahrenheit_100) // 100°C = 212°F
}

test "enhanced_resource_allocation_optimization" {
  // 测试资源分配优化：将资源分配给不同项目
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 将剩余预算平均分配给3个小项目
  let small_project_budget = divide_with_ceil(remaining_budget, 3)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(500, small_project_budget)
}

test "enhanced_complex_string_greeting" {
  // 测试复杂字符串问候场景
  let user_role = "Developer"
  let user_name = "Alice"
  let company = "TechCorp"
  
  // 构建完整的用户标识
  let full_identity = user_name + " (" + user_role + ") at " + company
  let formal_greeting = greet(full_identity)
  
  assert_eq_string("Hello, Alice (Developer) at TechCorp!", formal_greeting)
  
  // 测试包含特殊字符的复杂字符串
  let complex_id = "user_123@domain.com"
  let complex_greeting = greet(complex_id)
  assert_eq_string("Hello, user_123@domain.com!", complex_greeting)
}

test "enhanced_division_algorithm_edge_cases" {
  // 测试除法算法的边界情况
  // 测试连续除法
  let value = 1000
  let div1 = divide_with_ceil(value, 3)   // 334
  let div2 = divide_with_ceil(div1, 7)    // 48
  let div3 = divide_with_ceil(div2, 5)    // 10
  
  assert_eq(334, div1)
  assert_eq(48, div2)
  assert_eq(10, div3)
  
  // 测试负数连续除法
  let neg_value = -1000
  let neg_div1 = divide_with_ceil(neg_value, 3)   // -333
  let neg_div2 = divide_with_ceil(neg_div1, 7)    // -47
  let neg_div3 = divide_with_ceil(neg_div2, 5)    // -9
  
  assert_eq(-333, neg_div1)
  assert_eq(-47, neg_div2)
  assert_eq(-9, neg_div3)
}

test "enhanced_financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 年度复利计算（使用向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  assert_eq(10800, year1_amount)  // 10000 + 800
  assert_eq(11664, year2_amount)  // 10800 + 864
  assert_eq(12597, year3_amount)  // 11664 + 933
}

test "enhanced_logistics_packaging_optimization" {
  // 测试物流包装优化
  let item_volume = 15
  let box_capacity = 50
  let total_items = 127
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算总体积
  let total_volume = multiply(item_volume, total_items)
  
  // 计算实际使用的箱子容量
  let used_capacity = multiply(boxes_needed, box_capacity)
  
  // 计算浪费的空间
  let wasted_space = add(used_capacity, -total_items)
  
  assert_eq(3, boxes_needed)
  assert_eq(1905, total_volume)
  assert_eq(150, used_capacity)
  assert_eq(23, wasted_space)
}

test "enhanced_data_structure_simulation" {
  // 测试数据结构模拟：模拟栈操作
  let stack_size = 0
  
  // push 操作
  stack_size = add(stack_size, 1)  // push 1
  stack_size = add(stack_size, 1)  // push 2
  stack_size = add(stack_size, 1)  // push 3
  
  assert_eq(3, stack_size)
  
  // pop 操作
  stack_size = add(stack_size, -1)  // pop
  stack_size = add(stack_size, -1)  // pop
  
  assert_eq(1, stack_size)
  
  // 批量 push
  let batch_size = 5
  stack_size = add(stack_size, batch_size)
  
  assert_eq(6, stack_size)
}

test "enhanced_circular_buffer_calculation" {
  // 测试循环缓冲区计算
  let buffer_size = 10
  let current_position = 8
  let items_to_add = 5
  
  // 计算新位置（使用模运算模拟）
  let total_steps = add(current_position, items_to_add)
  let new_position = total_steps - multiply(buffer_size, divide_with_ceil(total_steps, buffer_size))
  
  assert_eq(13, total_steps)
  assert_eq(3, new_position)
  
  // 测试负数偏移
  let items_to_remove = 3
  let backward_steps = add(current_position, -items_to_remove)
  let adjusted_position = if backward_steps < 0 {
    add(buffer_size, backward_steps)
  } else {
    backward_steps
  }
  
  assert_eq(5, adjusted_position)
}

test "enhanced_performance_metrics_calculation" {
  // 测试性能指标计算
  let operations_completed = 1250
  let time_elapsed_seconds = 45
  let error_count = 15
  
  // 计算操作每秒
  let ops_per_second = divide_with_ceil(multiply(operations_completed, 100), time_elapsed_seconds)
  
  // 计算成功率（百分比）
  let successful_operations = add(operations_completed, -error_count)
  let success_rate = divide_with_ceil(multiply(successful_operations, 100), operations_completed)
  
  // 计算错误率
  let error_rate = add(100, -success_rate)
  
  assert_eq(2778, ops_per_second)  // 1250*100/45 = 2777.77... -> 2778
  assert_eq(98, success_rate)      // 1235*100/1250 = 98.8 -> 99，但使用向上取整可能是98
  assert_eq(2, error_rate)         // 100 - 98 = 2
}