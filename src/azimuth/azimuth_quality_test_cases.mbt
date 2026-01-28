// 高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "mathematical_series_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let series_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, series_sum)
}

test "business_profit_calculation" {
  // 业务利润计算：收入 - 成本 = 利润
  let revenue = 15000
  let cost = 8500
  let profit = add(revenue, -cost)
  let profit_margin = divide_with_ceil(multiply(profit, 100), revenue)
  
  assert_eq(6500, profit)
  assert_eq(44, profit_margin)  // 向上取整的利润率百分比
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let projects = 3
  let base_allocation = divide_with_ceil(total_resources, projects)
  let remaining = add(total_resources, -multiply(base_allocation, projects))
  
  assert_eq(34, base_allocation)  // ceil(100/3) = 34
  assert_eq(100, add(multiply(base_allocation, projects), remaining))  // 验证总和不变
}

test "temperature_conversion_simulation" {
  // 模拟温度转换计算（简化版）
  let celsius = 25
  // 使用近似公式 F = C * 2 + 30 (简化的华氏度转换)
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
  
  // 转换回来
  let back_to_celsius = divide_with_ceil(add(fahrenheit, -30), 2)
  assert_eq(25, back_to_celsius)
}

test "loan_interest_calculation" {
  // 贷款利息计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的单利计算
  let total_interest = multiply(divide_with_ceil(multiply(principal, annual_rate), 100), years)
  let total_repayment = add(principal, total_interest)
  
  assert_eq(1500, total_interest)
  assert_eq(11500, total_repayment)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let annual_sales = 120000
  let average_inventory = 30000
  
  // 库存周转率 = 年销售额 / 平均库存
  let turnover_rate = divide_with_ceil(annual_sales, average_inventory)
  
  assert_eq(4, turnover_rate)
}

test "data_storage_planning" {
  // 数据存储规划
  let daily_data_growth = 50  // GB per day
  let days_in_quarter = 90
  let storage_per_device = 1000  // GB
  
  let quarterly_growth = multiply(daily_data_growth, days_in_quarter)
  let devices_needed = divide_with_ceil(quarterly_growth, storage_per_device)
  
  assert_eq(4500, quarterly_growth)
  assert_eq(5, devices_needed)
}

test "team_productivity_metrics" {
  // 团队生产力指标
  let team_members = 8
  let tasks_per_member = 15
  let completed_tasks = 110
  
  let expected_tasks = multiply(team_members, tasks_per_member)
  let completion_rate = divide_with_ceil(multiply(completed_tasks, 100), expected_tasks)
  
  assert_eq(120, expected_tasks)
  assert_eq(92, completion_rate)  // 向上取整的完成率
}

test "network_bandwidth_planning" {
  // 网络带宽规划
  let users = 50
  let bandwidth_per_user = 10  // Mbps
  let total_bandwidth = multiply(users, bandwidth_per_user)
  
  // 考虑20%的冗余
  let redundant_bandwidth = add(total_bandwidth, divide_with_ceil(multiply(total_bandwidth, 20), 100))
  
  assert_eq(500, total_bandwidth)
  assert_eq(600, redundant_bandwidth)
}

test "greeting_multilingual_support" {
  // 多语言问候支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界2024!", greet("世界2024"))
}