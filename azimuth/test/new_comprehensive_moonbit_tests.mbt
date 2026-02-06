// 新的综合 MoonBit 测试用例 - 8个高质量测试
// 测试 azimuth 库的高级功能和边界情况

test "azimuth_advanced_mathematical_sequences" {
  // 高级数学序列测试
  let fib1 = @azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 0))
  let fib2 = @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(2, 1))
  let fib3 = @azimuth.add(@azimuth.multiply(1, 3), @azimuth.multiply(2, 2))
  
  @azimuth.assert_eq(1, fib1)
  @azimuth.assert_eq(4, fib2)
  @azimuth.assert_eq(7, fib3)
}

test "azimuth_complex_business_calculations" {
  // 复杂业务计算测试
  let product_price = 125
  let quantity = 47
  let discount_rate = 10
  let subtotal = @azimuth.multiply(product_price, quantity)
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, discount_rate), 100)
  let final_total = @azimuth.subtract(subtotal, discount_amount)
  
  @azimuth.assert_eq(5875, subtotal)
  @azimuth.assert_eq(588, discount_amount)
  @azimuth.assert_eq(5287, final_total)
}

test "azimuth_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 157
  let team_size = 8
  let resources_per_team = @azimuth.divide_with_ceil(total_resources, team_size)
  let allocated_resources = @azimuth.multiply(resources_per_team, team_size)
  let unused_resources = @azimuth.subtract(allocated_resources, total_resources)
  
  @azimuth.assert_eq(20, resources_per_team)
  @azimuth.assert_eq(160, allocated_resources)
  @azimuth.assert_eq(3, unused_resources)
}

test "azimuth_time_based_calculations" {
  // 基于时间的计算测试
  let work_days_per_month = 22
  let hours_per_day = 8
  let hourly_rate = 75
  let monthly_hours = @azimuth.multiply(work_days_per_month, hours_per_day)
  let monthly_income = @azimuth.multiply(monthly_hours, hourly_rate)
  
  @azimuth.assert_eq(176, monthly_hours)
  @azimuth.assert_eq(13200, monthly_income)
}

test "azimuth_inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 35
  let days_of_operation = 12
  let total_sold = @azimuth.multiply(daily_sales, days_of_operation)
  let remaining_stock = @azimuth.subtract(initial_stock, total_sold)
  let reorder_point = @azimuth.divide_with_ceil(initial_stock, 4)
  let needs_reorder = remaining_stock < reorder_point
  
  @azimuth.assert_eq(420, total_sold)
  @azimuth.assert_eq(80, remaining_stock)
  @azimuth.assert_eq(125, reorder_point)
  @azimuth.assert_eq(1, if(needs_reorder) { 1 } else { 0 })
}

test "azimuth_project_planning_calculations" {
  // 项目规划计算测试
  let project_tasks = 145
  let team_velocity = 18
  let sprints_needed = @azimuth.divide_with_ceil(project_tasks, team_velocity)
  let buffer_tasks = @azimuth.divide_with_ceil(project_tasks, 10)
  let total_tasks_with_buffer = @azimuth.add(project_tasks, buffer_tasks)
  let adjusted_sprints = @azimuth.divide_with_ceil(total_tasks_with_buffer, team_velocity)
  
  @azimuth.assert_eq(9, sprints_needed)
  @azimuth.assert_eq(15, buffer_tasks)
  @azimuth.assert_eq(160, total_tasks_with_buffer)
  @azimuth.assert_eq(9, adjusted_sprints)
}

test "azimuth_financial_projections" {
  // 财务预测测试
  let initial_investment = 50000
  let monthly_return_rate = 8
  let investment_months = 24
  let monthly_return = @azimuth.divide_with_ceil(@azimuth.multiply(initial_investment, monthly_return_rate), 100)
  let total_return = @azimuth.multiply(monthly_return, investment_months)
  let final_amount = @azimuth.add(initial_investment, total_return)
  
  @azimuth.assert_eq(4000, monthly_return)
  @azimuth.assert_eq(96000, total_return)
  @azimuth.assert_eq(146000, final_amount)
}

test "azimuth_boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值和边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1000000, 0))
  
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 测试负数边界
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1000000))
  @azimuth.assert_eq(-1000000, @azimuth.divide_with_ceil(-1000000, 1))
}