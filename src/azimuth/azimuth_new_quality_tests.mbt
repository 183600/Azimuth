// 新增高质量测试用例 - 针对 azimuth 库的核心功能测试

test "mathematical_series_calculation" {
  // 测试数学级数计算
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 5
  
  // 计算等差数列的和
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(25, series_sum)  // 5/2 * (6 + 8) = 2 * 14 = 28，向上取整
}

test "financial_simple_interest" {
  // 测试简单利息计算
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 年
  
  // 计算简单利息
  let interest = divide_with_ceil(multiply(principal, multiply(rate, time)), 100)
  let total_amount = add(principal, interest)
  
  assert_eq(150, interest)
  assert_eq(1150, total_amount)
}

test "inventory_management" {
  // 测试库存管理
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  
  // 计算一周后的库存
  let weekly_sales = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -weekly_sales)
  
  // 计算需要补货的数量（每批200个）
  let restock_batches = divide_with_ceil(weekly_sales, 200)
  
  assert_eq(175, weekly_sales)
  assert_eq(325, remaining_stock)
  assert_eq(1, restock_batches)
}

test "project_planning" {
  // 测试项目规划
  let total_tasks = 45
  let tasks_per_week = 8
  let team_members = 3
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_tasks, tasks_per_week)
  
  // 计算每人每周的任务数
  let tasks_per_person_per_week = divide_with_ceil(tasks_per_week, team_members)
  
  assert_eq(6, weeks_needed)
  assert_eq(3, tasks_per_person_per_week)
}

test "resource_allocation" {
  // 测试资源分配
  let total_budget = 10000
  let department_a = 3500
  let department_b = 2800
  let department_c = 2200
  
  // 计算剩余预算
  let allocated = add(add(department_a, department_b), department_c)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的额外项目数量（每个800）
  let extra_projects = divide_with_ceil(remaining, 800)
  
  assert_eq(8500, allocated)
  assert_eq(1500, remaining)
  assert_eq(2, extra_projects)
}

test "production_efficiency" {
  // 测试生产效率
  let units_per_hour = 120
  let hours_per_day = 8
  let days_per_week = 5
  let efficiency_factor = 90  // 90%
  
  // 计算理论周产量
  let theoretical_weekly = multiply(units_per_hour, multiply(hours_per_day, days_per_week))
  
  // 计算实际周产量
  let actual_weekly = divide_with_ceil(multiply(theoretical_weekly, efficiency_factor), 100)
  
  assert_eq(4800, theoretical_weekly)
  assert_eq(4320, actual_weekly)
}

test "data_processing" {
  // 测试数据处理
  let records_per_batch = 250
  let total_records = 1750
  let processing_time_per_record = 2  // 秒
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_records, records_per_batch)
  
  // 计算总处理时间（分钟）
  let total_seconds = multiply(total_records, processing_time_per_record)
  let total_minutes = divide_with_ceil(total_seconds, 60)
  
  assert_eq(7, batches_needed)
  assert_eq(3500, total_seconds)
  assert_eq(59, total_minutes)
}

test "logistics_optimization" {
  // 测试物流优化
  let packages_per_truck = 150
  let total_packages = 875
  let delivery_distance = 120  // 公里
  let fuel_consumption = 15  // 升/100公里
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, packages_per_truck)
  
  // 计算总油耗
  let total_fuel = multiply(trucks_needed, divide_with_ceil(multiply(delivery_distance, fuel_consumption), 100))
  
  assert_eq(6, trucks_needed)
  assert_eq(108, total_fuel)
}

test "quality_control" {
  // 测试质量控制
  let total_products = 1200
  let defect_rate = 3  // 3%
  let sample_size = 50
  
  // 计算预期缺陷产品数
  let expected_defects = divide_with_ceil(multiply(total_products, defect_rate), 100)
  
  // 计算需要的抽样批次数
  let sample_batches = divide_with_ceil(total_products, sample_size)
  
  assert_eq(36, expected_defects)
  assert_eq(24, sample_batches)
}

test "cost_analysis" {
  // 测试成本分析
  let fixed_costs = 5000
  let variable_cost_per_unit = 25
  let production_volume = 300
  let selling_price_per_unit = 45
  
  // 计算总成本
  let variable_costs = multiply(variable_cost_per_unit, production_volume)
  let total_costs = add(fixed_costs, variable_costs)
  
  // 计算总收入
  let total_revenue = multiply(selling_price_per_unit, production_volume)
  
  // 计算利润
  let profit = add(total_revenue, -total_costs)
  
  assert_eq(7500, variable_costs)
  assert_eq(12500, total_costs)
  assert_eq(13500, total_revenue)
  assert_eq(1000, profit)
}