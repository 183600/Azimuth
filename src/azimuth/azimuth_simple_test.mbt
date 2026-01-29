// 简单的 Azimuth 测试用例
// 包含5个高质量的 MoonBit 标准测试用例

test "advanced_mathematical_operations" {
  // 高级数学运算测试
  let x = 10
  let y = 15
  let z = 20
  
  // 测试复杂的数学表达式
  let result1 = add(multiply(x, y), z)
  let result2 = multiply(add(x, y), z)
  let result3 = divide_with_ceil(multiply(x, y), z)
  
  assert_eq(160, result1)  // 10*15 + 20 = 150 + 20 = 170
  assert_eq(500, result2)  // (10 + 15) * 20 = 25 * 20 = 500
  assert_eq(8, result3)    // ceil(10*15/20) = ceil(150/20) = 8
}

test "business_financial_calculations" {
  // 业务金融计算测试
  let revenue = 50000
  let expenses = 35000
  let tax_rate = 20
  
  // 计算利润
  let profit = add(revenue, -expenses)
  
  // 计算税费（简化计算）
  let tax_amount = divide_with_ceil(multiply(profit, tax_rate), 100)
  
  // 计算净利润
  let net_profit = add(profit, -tax_amount)
  
  assert_eq(15000, profit)      // 50000 - 35000
  assert_eq(3000, tax_amount)   // ceil(15000*20/100)
  assert_eq(12000, net_profit)  // 15000 - 3000
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 100
  let days_to_simulate = 7
  
  // 计算一周内的销售量
  let weekly_sales = multiply(daily_sales, days_to_simulate)
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -weekly_sales)
  
  // 计算需要补货的次数
  let restock_times = divide_with_ceil(weekly_sales, restock_amount)
  
  assert_eq(175, weekly_sales)     // 25 * 7
  assert_eq(325, remaining_stock)  // 500 - 175
  assert_eq(2, restock_times)      // ceil(175/100)
}

test "project_timeline_estimation" {
  // 项目时间线估算测试
  let total_tasks = 120
  let team_members = 5
  let work_days_per_week = 5
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的总工作日
  let total_work_days = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_work_days, work_days_per_week)
  
  assert_eq(10, daily_capacity)    // 5 * 2
  assert_eq(12, total_work_days)   // ceil(120/10)
  assert_eq(3, weeks_needed)       // ceil(12/5)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let data_points = 15
  let sum_values = 450
  let outlier_threshold = 30
  
  // 计算平均值
  let average = divide_with_ceil(sum_values, data_points)
  
  // 计算标准化值（简化）
  let normalized_values = divide_with_ceil(multiply(average, 100), outlier_threshold)
  
  // 计算数据质量评分
  let quality_score = divide_with_ceil(multiply(data_points, 10), 20)
  
  assert_eq(30, average)           // ceil(450/15)
  assert_eq(100, normalized_values) // ceil(30*100/30)
  assert_eq(8, quality_score)      // ceil(15*10/20)
}