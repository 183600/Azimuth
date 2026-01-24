// 综合单元测试用例
// 针对 azimuth 库的全面测试，包含实际应用场景

test "financial_calculations" {
  // 金融计算：复利计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_optimization" {
  // 库存优化：计算最佳订货量
  let monthly_demand = 500
  let order_cost = 50
  let holding_cost = 2
  
  // 经济订货量 (EOQ) 简化计算
  let annual_demand = multiply(monthly_demand, 12)
  let eoq_numerator = multiply(multiply(2, annual_demand), order_cost)
  let eoq = divide_with_ceil(eoq_numerator, holding_cost)
  
  assert_eq(6000, annual_demand)
  assert_eq(60000, eoq_numerator)
  assert_eq(30000, eoq)
}

test "resource_allocation" {
  // 资源分配：项目资源分配
  let total_budget = 100000
  let project_a_percentage = 40
  let project_b_percentage = 35
  let project_c_percentage = 25
  
  let project_a_budget = divide_with_ceil(multiply(total_budget, project_a_percentage), 100)
  let project_b_budget = divide_with_ceil(multiply(total_budget, project_b_percentage), 100)
  let project_c_budget = divide_with_ceil(multiply(total_budget, project_c_percentage), 100)
  
  let allocated_total = add(add(project_a_budget, project_b_budget), project_c_budget)
  
  assert_eq(40000, project_a_budget)
  assert_eq(35000, project_b_budget)
  assert_eq(25000, project_c_budget)
  assert_eq(100000, allocated_total)
}

test "time_management" {
  // 时间管理：任务时间分配
  let total_hours = 40
  let urgent_tasks = 3
  let important_tasks = 5
  let routine_tasks = 8
  
  let urgent_hours = multiply(urgent_tasks, 4)
  let important_hours = multiply(important_tasks, 3)
  let routine_hours = divide_with_ceil(total_hours - urgent_hours - important_hours, routine_tasks)
  
  let total_allocated = add(add(urgent_hours, important_hours), multiply(routine_hours, routine_tasks))
  
  assert_eq(12, urgent_hours)
  assert_eq(15, important_hours)
  assert_eq(2, routine_hours)
  assert_eq(43, total_allocated)
}

test "data_analysis" {
  // 数据分析：统计计算
  let data_points = [15, 22, 18, 25, 20, 17, 23, 19, 21, 16]
  let sum = add(add(add(add(add(add(add(add(add(15, 22), 18), 25), 20), 17), 23), 19), 21), 16)
  let count = 10
  let mean = divide_with_ceil(sum, count)
  
  assert_eq(196, sum)
  assert_eq(20, mean)
}

test "logistics_optimization" {
  // 物流优化：运输路线规划
  let distance_a = 120
  let distance_b = 85
  let distance_c = 95
  let distance_d = 110
  
  let total_distance = add(add(add(distance_a, distance_b), distance_c), distance_d)
  let fuel_consumption = divide_with_ceil(total_distance, 15)
  let estimated_time = divide_with_ceil(total_distance, 60)
  
  assert_eq(410, total_distance)
  assert_eq(28, fuel_consumption)
  assert_eq(7, estimated_time)
}

test "quality_control" {
  // 质量控制：产品检验
  let total_products = 1000
  let defect_rate = 2
  let sample_size = 50
  
  let expected_defects = divide_with_ceil(multiply(total_products, defect_rate), 100)
  let sample_defects = divide_with_ceil(multiply(sample_size, defect_rate), 100)
  let quality_score = add(100, -multiply(defect_rate, 10))
  
  assert_eq(20, expected_defects)
  assert_eq(1, sample_defects)
  assert_eq(80, quality_score)
}

test "energy_efficiency" {
  // 能源效率：能耗计算
  let power_rating = 1500
  let hours_per_day = 8
  let days_per_month = 30
  let cost_per_kwh = 12
  
  let daily_consumption = multiply(power_rating, hours_per_day)
  let monthly_consumption = multiply(daily_consumption, days_per_month)
  let monthly_cost = divide_with_ceil(multiply(monthly_consumption, cost_per_kwh), 1000)
  
  assert_eq(12000, daily_consumption)
  assert_eq(360000, monthly_consumption)
  assert_eq(4320, monthly_cost)
}

test "project_planning" {
  // 项目规划：里程碑计算
  let project_duration = 180
  let milestones = 5
  let buffer_percentage = 15
  
  let milestone_interval = divide_with_ceil(project_duration, milestones)
  let total_buffer = divide_with_ceil(multiply(project_duration, buffer_percentage), 100)
  let adjusted_duration = add(project_duration, total_buffer)
  
  assert_eq(36, milestone_interval)
  assert_eq(27, total_buffer)
  assert_eq(207, adjusted_duration)
}

test "customer_service" {
  // 客户服务：响应时间分析
  let total_tickets = 500
  let resolved_tickets = 450
  let target_response_time = 24
  let actual_response_time = 18
  
  let resolution_rate = divide_with_ceil(multiply(resolved_tickets, 100), total_tickets)
  let response_efficiency = divide_with_ceil(multiply(target_response_time, 100), actual_response_time)
  let performance_score = divide_with_ceil(add(resolution_rate, response_efficiency), 2)
  
  assert_eq(90, resolution_rate)
  assert_eq(134, response_efficiency)
  assert_eq(112, performance_score)
}