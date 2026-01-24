// 增强的 azimuth 测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度
  let fahrenheit_back = 77
  let celsius_back = multiply(fahrenheit_back - 32, 5) / 9
  assert_eq(25, celsius_back)
}

test "loan_calculation" {
  // 贷款计算测试
  let principal = 100000
  let annual_rate = 5
  let years = 3
  
  // 简单利息计算
  let total_interest = multiply(multiply(principal, annual_rate), years) / 100
  let total_payment = add(principal, total_interest)
  
  assert_eq(15000, total_interest)
  assert_eq(115000, total_payment)
}

test "batch_processing" {
  // 批处理计算测试
  let batch_size = 47
  let items_per_batch = 8
  let total_items = 150
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, items_per_batch)
  
  // 计算最后一个批次的项目数
  let last_batch_size = total_items - multiply(batches_needed - 1, items_per_batch)
  
  assert_eq(19, batches_needed)
  assert_eq(6, last_batch_size)
}

test "currency_exchange" {
  // 货币兑换计算测试
  let amount_usd = 100
  let exchange_rate = 6.85  // 简化为整数计算：685/100
  let fees = 5
  
  // 计算兑换金额（使用整数运算）
  let exchanged_amount = multiply(amount_usd, 685) / 100
  let final_amount = add(exchanged_amount, -fees)
  
  assert_eq(685, exchanged_amount)
  assert_eq(680, final_amount)
}

test "resource_allocation" {
  // 资源分配测试
  let total_resources = 1000
  let project_a_needs = 350
  let project_b_needs = 280
  let project_c_needs = 200
  
  // 计算已分配资源
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = add(total_resources, -allocated)
  
  // 计算每个项目的百分比（向上取整）
  let project_a_percentage = divide_with_ceil(multiply(project_a_needs, 100), total_resources)
  let project_b_percentage = divide_with_ceil(multiply(project_b_needs, 100), total_resources)
  let project_c_percentage = divide_with_ceil(multiply(project_c_needs, 100), total_resources)
  
  assert_eq(830, allocated)
  assert_eq(170, remaining)
  assert_eq(35, project_a_percentage)
  assert_eq(28, project_b_percentage)
  assert_eq(20, project_c_percentage)
}

test "inventory_reorder" {
  // 库存重新订购测试
  let current_stock = 45
  let daily_sales = 8
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算在lead time期间的销售量
  let lead_time_sales = multiply(daily_sales, lead_time_days)
  
  // 计算重新订购点
  let reorder_point = add(lead_time_sales, safety_stock)
  
  // 判断是否需要重新订购
  let should_reorder = if current_stock <= reorder_point { 1 } else { 0 }
  
  assert_eq(56, lead_time_sales)
  assert_eq(76, reorder_point)
  assert_eq(1, should_reorder)
}

test "data_transmission" {
  // 数据传输计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  // 计算基础传输时间
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 计算额外开销时间
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_percentage), 100)
  
  // 计算总传输时间
  let total_time = add(base_time, overhead_time)
  
  assert_eq(17, base_time)
  assert_eq(2, overhead_time)
  assert_eq(19, total_time)
}

test "project_management" {
  // 项目管理计算测试
  let total_tasks = 125
  let completed_tasks = 78
  let team_members = 5
  let remaining_days = 8
  
  // 计算完成百分比
  let completion_percentage = divide_with_ceil(multiply(completed_tasks, 100), total_tasks)
  
  // 计算剩余任务
  let remaining_tasks = add(total_tasks, -completed_tasks)
  
  // 计算每人每天需要完成的任务数
  let daily_tasks_per_person = divide_with_ceil(remaining_tasks, multiply(team_members, remaining_days))
  
  assert_eq(63, completion_percentage)
  assert_eq(47, remaining_tasks)
  assert_eq(2, daily_tasks_per_person)
}

test "energy_efficiency" {
  // 能源效率计算测试
  let old_power_consumption = 500
  let new_power_consumption = 350
  let operating_hours_per_day = 24
  let days_per_month = 30
  
  // 计算每小时节省的能源
  let hourly_savings = add(old_power_consumption, -new_power_consumption)
  
  // 计算每月节省的能源
  let monthly_savings = multiply(multiply(hourly_savings, operating_hours_per_day), days_per_month)
  
  // 计算效率提升百分比
  let efficiency_improvement = divide_with_ceil(
    multiply(hourly_savings, 100), 
    old_power_consumption
  )
  
  assert_eq(150, hourly_savings)
  assert_eq(108000, monthly_savings)
  assert_eq(30, efficiency_improvement)
}

test "user_engagement_metrics" {
  // 用户参与度指标测试
  let total_users = 10000
  let active_users_daily = 3500
  let active_users_weekly = 6200
  let active_users_monthly = 8500
  
  // 计算日活跃率
  let daily_active_rate = divide_with_ceil(multiply(active_users_daily, 100), total_users)
  
  // 计算周活跃率
  let weekly_active_rate = divide_with_ceil(multiply(active_users_weekly, 100), total_users)
  
  // 计算月活跃率
  let monthly_active_rate = divide_with_ceil(multiply(active_users_monthly, 100), total_users)
  
  // 计算用户粘性指数（日活/月活）
  let stickiness_index = divide_with_ceil(multiply(active_users_daily, 100), active_users_monthly)
  
  assert_eq(35, daily_active_rate)
  assert_eq(62, weekly_active_rate)
  assert_eq(85, monthly_active_rate)
  assert_eq(42, stickiness_index)
}