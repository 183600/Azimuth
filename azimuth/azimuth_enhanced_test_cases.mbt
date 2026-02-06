// Azimuth 增强测试用例
// 包含8个高质量的标准MoonBit测试用例

test "azimuth_calculate_discounted_price" {
  // 计算折扣价格的业务场景测试
  let original_price = 1000
  let discount_percentage = 15
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = subtract(original_price, discount_amount)
  
  assert_eq(150, discount_amount)
  assert_eq(850, final_price)
}

test "azimuth_calculate_compound_interest" {
  // 计算复利的财务场景测试
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 简化的年复利计算
  let year1_end = add(principal, multiply(principal, annual_rate) / 100)
  let year2_end = add(year1_end, multiply(year1_end, annual_rate) / 100)
  let year3_end = add(year2_end, multiply(year2_end, annual_rate) / 100)
  
  assert_eq(10500, year1_end)
  assert_eq(11025, year2_end)
  assert_eq(11576, year3_end)  // 向下取整
}

test "azimuth_calculate_shipping_cost" {
  // 计算运费的物流场景测试
  let package_weight = 15
  let base_rate = 10
  let weight_rate = 2
  let weight_surcharge = multiply(subtract(package_weight, 10), weight_rate)
  
  let total_cost = add(base_rate, weight_surcharge)
  
  assert_eq(20, total_cost)
}

test "azimuth_calculate_tax_with_brackets" {
  // 计算分段税率的税务场景测试
  let income = 75000
  let first_bracket_limit = 30000
  let first_bracket_rate = 10
  let second_bracket_rate = 20
  
  let first_bracket_tax = multiply(first_bracket_limit, first_bracket_rate) / 100
  let second_bracket_income = subtract(income, first_bracket_limit)
  let second_bracket_tax = multiply(second_bracket_income, second_bracket_rate) / 100
  let total_tax = add(first_bracket_tax, second_bracket_tax)
  
  assert_eq(3000, first_bracket_tax)
  assert_eq(9000, second_bracket_tax)
  assert_eq(12000, total_tax)
}

test "azimuth_calculate_team_productivity" {
  // 计算团队生产力的管理场景测试
  let team_size = 8
  let hours_per_day = 8
  let tasks_per_hour = 2
  let working_days = 5
  
  let daily_tasks_per_person = multiply(hours_per_day, tasks_per_hour)
  let daily_team_tasks = multiply(team_size, daily_tasks_per_person)
  let weekly_tasks = multiply(daily_team_tasks, working_days)
  
  assert_eq(16, daily_tasks_per_person)
  assert_eq(128, daily_team_tasks)
  assert_eq(640, weekly_tasks)
}

test "azimuth_calculate_inventory_reorder" {
  // 计算库存重新订购点的供应链场景测试
  let daily_demand = 50
  let lead_time_days = 7
  let safety_stock = 100
  
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  assert_eq(350, lead_time_demand)
  assert_eq(450, reorder_point)
}

test "azimuth_calculate_project_timeline" {
  // 计算项目时间线的项目管理场景测试
  let total_tasks = 120
  let team_members = 6
  let tasks_per_person_per_day = 2
  let buffer_percentage = 20
  
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  let estimated_days = divide_with_ceil(total_tasks, daily_capacity)
  let buffer_days = multiply(estimated_days, buffer_percentage) / 100
  let total_timeline = add(estimated_days, buffer_days)
  
  assert_eq(12, daily_capacity)
  assert_eq(10, estimated_days)
  assert_eq(2, buffer_days)
  assert_eq(12, total_timeline)
}

test "azimuth_calculate_roi" {
  // 计算投资回报率(ROI)的投资场景测试
  let initial_investment = 50000
  let final_value = 65000
  
  let profit = subtract(final_value, initial_investment)
  let roi_percentage = multiply(profit, 100) / initial_investment
  
  assert_eq(15000, profit)
  assert_eq(30, roi_percentage)
}