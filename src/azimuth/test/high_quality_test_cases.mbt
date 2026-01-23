// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_series_sum" {
  // 测试数学级数求和
  // 计算等差数列 1, 4, 7, 10, 13 的和
  let term1 = 1
  let term2 = 4
  let term3 = 7
  let term4 = 10
  let term5 = 13
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(35, series_sum)
}

test "business_profit_calculation" {
  // 测试商业利润计算
  let revenue = 50000
  let cost1 = 12000
  let cost2 = 8000
  let cost3 = 15000
  
  let total_cost = @azimuth.add(@azimuth.add(cost1, cost2), cost3)
  let profit = revenue - total_cost
  
  @azimuth.assert_eq(35000, total_cost)
  @azimuth.assert_eq(15000, profit)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let project1_needs = 23
  let project2_needs = 31
  let project3_needs = 19
  
  let allocated = @azimuth.add(@azimuth.add(project1_needs, project2_needs), project3_needs)
  let remaining = total_resources - allocated
  
  @azimuth.assert_eq(73, allocated)
  @azimuth.assert_eq(27, remaining)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let work_hours_per_day = 8
  let meeting_hours = 2
  let focus_work_hours = work_hours_per_day - meeting_hours
  
  let tasks_per_hour = 3
  let daily_tasks = @azimuth.multiply(focus_work_hours, tasks_per_hour)
  
  @azimuth.assert_eq(6, focus_work_hours)
  @azimuth.assert_eq(18, daily_tasks)
}

test "inventory_turnover_calculation" {
  // 测试库存周转率计算
  let annual_sales = 120000
  let average_inventory = 30000
  
  let turnover_ratio = @azimuth.divide_with_ceil(annual_sales, average_inventory)
  
  @azimuth.assert_eq(4, turnover_ratio)
}

test "team_productivity_metrics" {
  // 测试团队生产力指标
  let team_size = 12
  let tasks_completed = 156
  let work_days = 20
  
  let tasks_per_person = @azimuth.divide_with_ceil(tasks_completed, team_size)
  let daily_team_output = @azimuth.divide_with_ceil(tasks_completed, work_days)
  
  @azimuth.assert_eq(13, tasks_per_person)
  @azimuth.assert_eq(8, daily_team_output)
}

test "financial_compound_interest" {
  // 测试复利计算（简化版）
  let principal = 10000
  let interest_rate = 5  // 5%
  let _years = 3  // 未使用的变量
  
  // 简化的年复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, interest_rate), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, interest_rate), 100))
  
  @azimuth.assert_eq(10500, year1_amount)
  @azimuth.assert_eq(11025, year2_amount)
  @azimuth.assert_eq(11576, year3_amount)
}

test "logistics_packaging_optimization" {
  // 测试物流包装优化
  let item_volume = 15
  let box_capacity = 100
  let total_items = 45
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  let total_volume = @azimuth.multiply(total_items, item_volume)
  let used_box_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let wasted_space = used_box_capacity - total_volume
  
  @azimuth.assert_eq(1, boxes_needed)
  @azimuth.assert_eq(675, total_volume)
  @azimuth.assert_eq(100, used_box_capacity)
  @azimuth.assert_eq(0, wasted_space)  // 因为使用了向上取整，没有浪费空间
}

test "energy_efficiency_calculation" {
  // 测试能效计算
  let energy_input = 1000
  let useful_output = 750
  
  let efficiency_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(useful_output, 100), energy_input)
  
  @azimuth.assert_eq(75, efficiency_percentage)
}

test "customer_satisfaction_score" {
  // 测试客户满意度评分
  let response1 = 4
  let response2 = 5
  let response3 = 3
  let response4 = 5
  let response5 = 4
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(response1, response2), response3), response4), response5)
  let average_score = @azimuth.divide_with_ceil(@azimuth.multiply(total_score, 10), 5)  // 扩大10倍以保留一位小数
  
  @azimuth.assert_eq(21, total_score)
  @azimuth.assert_eq(42, average_score)  // 平均4.2分，扩大10倍为42
}