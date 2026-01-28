// 增强综合测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "advanced_mathematical_operations" {
  // 高级数学运算测试
  let x = 15
  let y = 7
  
  // 测试复合运算：(x + y) * (x - y)
  let sum = @azimuth.add(x, y)
  let diff = @azimuth.add(x, -y)
  let product = @azimuth.multiply(sum, diff)
  @azimuth.assert_eq(176, product)  // (15 + 7) * (15 - 7) = 22 * 8 = 176
  
  // 测试平方计算：x^2
  let square = @azimuth.multiply(x, x)
  @azimuth.assert_eq(225, square)
}

test "financial_calculations" {
  // 金融计算场景测试
  let principal = 10000
  let rate_percent = 8
  let _years = 3
  
  // 简单利息计算（每年）
  let yearly_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let total_after_3_years = @azimuth.add(@azimuth.add(@azimuth.add(principal, yearly_interest), yearly_interest), yearly_interest)
  
  @azimuth.assert_eq(800, yearly_interest)
  @azimuth.assert_eq(12400, total_after_3_years)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let days = 14
  let new_shipment = 300
  
  // 计算剩余库存
  let total_sold = @azimuth.multiply(daily_sales, days)
  let stock_before_shipment = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(350, total_sold)
  @azimuth.assert_eq(150, stock_before_shipment)
  @azimuth.assert_eq(450, final_stock)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 1500
  
  // 计算剩余预算和可分配的小项目数量
  let allocated_budget = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 200)
  
  @azimuth.assert_eq(4500, allocated_budget)
  @azimuth.assert_eq(500, remaining_budget)
  @azimuth.assert_eq(3, small_projects)
}

test "data_processing_pipeline" {
  // 数据处理管道测试
  let data_points = 1000
  let processing_batches = 7
  let error_rate_percent = 2
  
  // 计算处理批次和错误数量
  let batch_size = @azimuth.divide_with_ceil(data_points, processing_batches)
  let error_count = @azimuth.divide_with_ceil(@azimuth.multiply(data_points, error_rate_percent), 100)
  let successful_processed = @azimuth.add(data_points, -error_count)
  
  @azimuth.assert_eq(143, batch_size)
  @azimuth.assert_eq(20, error_count)
  @azimuth.assert_eq(980, successful_processed)
}

test "time_management_scenarios" {
  // 时间管理场景测试
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days = 22
  
  // 计算完成时间和工作效率
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let monthly_capacity = @azimuth.multiply(tasks_per_day, working_days)
  let efficiency_percent = @azimuth.divide_with_ceil(@azimuth.multiply(total_tasks, 100), monthly_capacity)
  
  @azimuth.assert_eq(11, days_needed)
  @azimuth.assert_eq(176, monthly_capacity)
  @azimuth.assert_eq(50, efficiency_percent)
}

test "complex_string_greeting_scenarios" {
  // 复杂字符串问候场景测试
  let company = "TechCorp"
  let department = "研发部"
  let employee_id = "EMP-2024-001"
  
  // 构建复杂的问候字符串
  let full_identifier = company + " " + department + " " + employee_id
  let formal_greeting = @azimuth.greet(full_identifier)
  let casual_greeting = @azimuth.greet("团队成员")
  
  @azimuth.assert_eq_string("Hello, TechCorp 研发部 EMP-2024-001!", formal_greeting)
  @azimuth.assert_eq_string("Hello, 团队成员!", casual_greeting)
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  let large_number = 1000000
  let small_number = 1
  let negative_large = -500000
  
  // 测试极端数值运算
  let large_sum = @azimuth.add(large_number, small_number)
  let large_product = @azimuth.multiply(large_number, small_number)
  let mixed_operation = @azimuth.add(@azimuth.multiply(negative_large, 2), large_number)
  
  @azimuth.assert_eq(1000001, large_sum)
  @azimuth.assert_eq(1000000, large_product)
  @azimuth.assert_eq(0, mixed_operation)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let normal_dividend = 100
  let zero_divisor = 0
  let negative_dividend = -50
  let negative_divisor = -5
  
  // 测试各种除法情况
  let zero_division_result = @azimuth.divide_with_ceil(normal_dividend, zero_divisor)
  let negative_division_result = @azimuth.divide_with_ceil(negative_dividend, negative_divisor)
  let mixed_division_result = @azimuth.divide_with_ceil(normal_dividend, negative_divisor)
  
  @azimuth.assert_eq(0, zero_division_result)
  @azimuth.assert_eq(10, negative_division_result)
  @azimuth.assert_eq(-20, mixed_division_result)
}

test "real_world_shipping_calculation" {
  // 真实世界运输计算测试
  let package_weight = 47
  let weight_limit_per_box = 10
  let cost_per_box = 25
  let base_handling_fee = 15
  
  // 计算需要的箱数和总成本
  let boxes_needed = @azimuth.divide_with_ceil(package_weight, weight_limit_per_box)
  let shipping_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  let total_cost = @azimuth.add(shipping_cost, base_handling_fee)
  
  @azimuth.assert_eq(5, boxes_needed)
  @azimuth.assert_eq(125, shipping_cost)
  @azimuth.assert_eq(140, total_cost)
}