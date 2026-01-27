// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_function_basic" {
  // 测试基本的减法功能
  azimuth::assert_eq(5, azimuth::subtract(10, 5))
  azimuth::assert_eq(-5, azimuth::subtract(5, 10))
  azimuth::assert_eq(0, azimuth::subtract(7, 7))
  azimuth::assert_eq(10, azimuth::subtract(0, -10))
}

test "mathematical_identity_laws" {
  // 测试数学恒等律
  let a = 15
  
  // 加法单位元
  azimuth::assert_eq(a, azimuth::add(a, 0))
  azimuth::assert_eq(a, azimuth::add(0, a))
  
  // 乘法单位元
  azimuth::assert_eq(a, azimuth::multiply(a, 1))
  azimuth::assert_eq(a, azimuth::multiply(1, a))
  
  // 减法恒等式
  azimuth::assert_eq(0, azimuth::subtract(a, a))
}

test "complex_business_calculation" {
  // 复杂业务计算：计算订单总价
  let product_price = 89
  let quantity = 4
  let discount = 20
  let shipping = 12
  
  let subtotal = azimuth::multiply(product_price, quantity)
  let discounted_total = azimuth::subtract(subtotal, discount)
  let final_total = azimuth::add(discounted_total, shipping)
  
  azimuth::assert_eq(356, subtotal)
  azimuth::assert_eq(336, discounted_total)
  azimuth::assert_eq(348, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 220
  
  let allocated_budget = azimuth::add(azimuth::add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = azimuth::subtract(total_budget, allocated_budget)
  
  // 计算可以支持的额外小项目数量
  let small_project_cost = 50
  let additional_projects = azimuth::divide_with_ceil(remaining_budget, small_project_cost)
  
  azimuth::assert_eq(850, allocated_budget)
  azimuth::assert_eq(150, remaining_budget)
  azimuth::assert_eq(3, additional_projects)
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_work_hours = 8
  let task_complexity_factor = 3
  let break_time = 2
  
  let total_time_needed = azimuth::multiply(daily_work_hours, task_complexity_factor)
  let actual_work_time = azimuth::subtract(total_time_needed, break_time)
  let days_required = azimuth::divide_with_ceil(actual_work_time, daily_work_hours)
  
  azimuth::assert_eq(24, total_time_needed)
  azimuth::assert_eq(22, actual_work_time)
  azimuth::assert_eq(3, days_required)
}

test "data_processing_pipeline" {
  // 数据处理管道模拟
  let input_data = 1000
  let filter_rate = 75  // 75% 的数据通过过滤
  let transformation_factor = 2
  let validation_overhead = 50
  
  let filtered_data = azimuth::multiply(input_data, filter_rate) / 100
  let transformed_data = azimuth::multiply(filtered_data, transformation_factor)
  let final_result = azimuth::subtract(transformed_data, validation_overhead)
  
  azimuth::assert_eq(750, filtered_data)
  azimuth::assert_eq(1500, transformed_data)
  azimuth::assert_eq(1450, final_result)
}

test "greeting_multilingual_support" {
  // 多语言问候支持测试
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, 用户123!", azimuth::greet("用户123"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "performance_benchmark_calculation" {
  // 性能基准计算
  let baseline_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  let optimized_operations = azimuth::multiply(baseline_operations, optimization_factor)
  let overhead = azimuth::divide_with_ceil(azimuth::multiply(optimized_operations, overhead_percentage), 100)
  let net_performance = azimuth::subtract(optimized_operations, overhead)
  
  azimuth::assert_eq(3000, optimized_operations)
  azimuth::assert_eq(450, overhead)
  azimuth::assert_eq(2550, net_performance)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let monthly_sales = 120
  let months_to_simulate = 3
  let new_stock_received = 200
  
  let total_sales = azimuth::multiply(monthly_sales, months_to_simulate)
  let ending_inventory = azimuth::subtract(azimuth::add(initial_inventory, new_stock_received), total_sales)
  let turnover_rate = azimuth::divide_with_ceil(total_sales, initial_inventory)
  
  azimuth::assert_eq(360, total_sales)
  azimuth::assert_eq(340, ending_inventory)
  azimuth::assert_eq(1, turnover_rate)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 测试除零安全
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-50, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 测试极端值
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1000000))
  azimuth::assert_eq(1000000, azimuth::divide_with_ceil(1000000, 1))
  
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::subtract(0, 10))
  azimuth::assert_eq(10, azimuth::subtract(0, -10))
}