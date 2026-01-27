// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试，覆盖更多实际应用场景

test "financial_calculations" {
  // 金融计算：复利计算
  let principal = 1000
  let rate = 5  // 5%
  let _years = 3
  
  // 简化的年复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "inventory_management" {
  // 库存管理：计算补货需求
  let current_stock = 45
  let daily_sales = 8
  let days_until_reorder = 7
  let safety_stock = 20
  
  let expected_sales = multiply(daily_sales, days_until_reorder)
  let reorder_point = add(expected_sales, safety_stock)
  let need_to_reorder = current_stock < reorder_point
  
  assert_eq(56, expected_sales)
  assert_eq(76, reorder_point)
  // 由于 need_to_reorder 是布尔值，我们通过条件判断来验证
  assert_eq(1, if need_to_reorder { 1 } else { 0 })
}

test "time_management" {
  // 时间管理：项目完成时间估算
  let total_tasks = 67
  let tasks_per_day = 5
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  let total_hours = multiply(days_needed, 8)  // 每天8小时
  
  assert_eq(14, days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(112, total_hours)
}

test "geometry_calculations" {
  // 几何计算：矩形面积和周长
  let length = 12
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(96, area)
  assert_eq(40, perimeter)
}

test "temperature_conversion" {
  // 温度转换（简化版）：摄氏度到华氏度
  let celsius = 25
  let fahrenheit = add(multiply(9, divide_with_ceil(multiply(celsius, 5), 5)), 32)
  
  assert_eq(77, fahrenheit)
}

test "data_analysis" {
  // 数据分析：统计计算
  let values = [15, 22, 18, 31, 24]
  let sum = add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  let average = divide_with_ceil(sum, 5)
  
  assert_eq(110, sum)
  assert_eq(22, average)
}

test "resource_allocation" {
  // 资源分配：项目预算分配
  let total_budget = 50000
  let team_size = 8
  let project_duration_months = 6
  
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  let monthly_budget = divide_with_ceil(total_budget, project_duration_months)
  let weekly_budget = divide_with_ceil(monthly_budget, 4)
  
  assert_eq(6250, budget_per_person)
  assert_eq(8334, monthly_budget)
  assert_eq(2084, weekly_budget)
}

test "shipping_calculations" {
  // 运输计算：包裹运费
  let base_rate = 10
  let weight = 15
  let distance = 120
  let weight_rate = 2
  let distance_rate = 1
  
  let weight_cost = multiply(weight, weight_rate)
  let distance_cost = divide_with_ceil(multiply(distance, distance_rate), 10)
  let total_cost = add(add(base_rate, weight_cost), distance_cost)
  
  assert_eq(30, weight_cost)
  assert_eq(12, distance_cost)
  assert_eq(52, total_cost)
}

test "performance_metrics" {
  // 性能指标：效率计算
  let units_produced = 450
  let hours_worked = 40
  let defect_count = 15
  let target_efficiency = 10
  
  let actual_efficiency = divide_with_ceil(units_produced, hours_worked)
  let defect_rate = divide_with_ceil(multiply(defect_count, 100), units_produced)
  let efficiency_rating = if actual_efficiency >= target_efficiency { 1 } else { 0 }
  
  assert_eq(12, actual_efficiency)
  assert_eq(4, defect_rate)
  assert_eq(1, efficiency_rating)
}

test "string_processing_advanced" {
  // 高级字符串处理：多语言问候
  let names = ["张三", "John", "María", "Yuki"]
  let greeting1 = greet(names[0])
  let greeting2 = greet(names[1])
  let greeting3 = greet(names[2])
  let greeting4 = greet(names[3])
  
  assert_eq_string("Hello, 张三!", greeting1)
  assert_eq_string("Hello, John!", greeting2)
  assert_eq_string("Hello, María!", greeting3)
  assert_eq_string("Hello, Yuki!", greeting4)
}