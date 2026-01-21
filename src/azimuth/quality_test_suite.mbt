// 高质量测试套件 - 针对 azimuth 库的综合测试
// 包含实际应用场景和边界情况测试

test "financial_interest_calculation" {
  // 金融利息计算场景
  // 计算复利：本金 = 1000，年利率 = 5%，期限 = 3年
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(1158, final_amount)  // 1000 -> 1050 -> 1103 -> 1158
}

test "inventory_batch_processing" {
  // 库存批次处理场景
  // 计算需要的批次数量
  let total_products = 157
  let batch_capacity = 25
  let batches_needed = divide_with_ceil(total_products, batch_capacity)
  
  // 计算最后一个批次的产品数量
  let full_batches = total_products / batch_capacity
  let last_batch_size = total_products - multiply(full_batches, batch_capacity)
  
  assert_eq(7, batches_needed)  // ceil(157/25) = 7
  assert_eq(7, last_batch_size)  // 157 - 6*25 = 7
}

test "team_project_allocation" {
  // 团队项目分配场景
  // 将任务分配给团队成员
  let total_tasks = 47
  let team_members = 8
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  // 计算总分配任务数
  let allocated_tasks = multiply(tasks_per_member, team_members)
  let extra_tasks = allocated_tasks - total_tasks
  
  assert_eq(6, tasks_per_member)  // ceil(47/8) = 6
  assert_eq(1, extra_tasks)  // 6*8 - 47 = 1
}

test "temperature_conversion_batch" {
  // 温度转换批量处理
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius_temp = 20
  let expected_fahrenheit = 68
  
  // 测试温度转换
  let fahrenheit = add(divide_with_ceil(multiply(celsius_temp, 9), 5), 32)
  assert_eq(expected_fahrenheit, fahrenheit)
  
  // 测试另一个温度
  let celsius_temp2 = 0
  let fahrenheit2 = add(divide_with_ceil(multiply(celsius_temp2, 9), 5), 32)
  assert_eq(32, fahrenheit2)
}

test "shipping_cost_calculation" {
  // 运费计算场景
  // 根据重量和距离计算运费
  let base_rate = 5
  let weight_kg = 12
  let distance_km = 45
  
  // 重量费用：每kg基础费率，向上取整到最近的5kg
  let weight_units = divide_with_ceil(weight_kg, 5)
  let weight_cost = multiply(weight_units, multiply(base_rate, 2))
  
  // 距离费用：每10km一个计价单位
  let distance_units = divide_with_ceil(distance_km, 10)
  let distance_cost = multiply(distance_units, base_rate)
  
  let total_cost = add(weight_cost, distance_cost)
  
  assert_eq(55, total_cost)  // weight: ceil(12/5)*5*2 = 3*10 = 30, distance: ceil(45/10)*5 = 5*5 = 25, total: 30+25=55
}

test "classroom_seating_arrangement" {
  // 教室座位安排场景
  let total_students = 67
  let seats_per_row = 8
  let rows_needed = divide_with_ceil(total_students, seats_per_row)
  
  // 计算最后一排的座位数
  let full_rows = total_students / seats_per_row
  let last_row_students = total_students - multiply(full_rows, seats_per_row)
  
  // 计算空座位数
  let total_seats = multiply(rows_needed, seats_per_row)
  let empty_seats = total_seats - total_students
  
  assert_eq(9, rows_needed)  // ceil(67/8) = 9
  assert_eq(3, last_row_students)  // 67 - 8*8 = 3
  assert_eq(5, empty_seats)  // 9*8 - 67 = 5
}

test "event_planning_budget" {
  // 活动规划预算场景
  let participants = 85
  let cost_per_person = 25
  let fixed_venue_cost = 500
  let contingency_percent = 10
  
  // 计算基础成本
  let variable_cost = multiply(participants, cost_per_person)
  let base_total = add(variable_cost, fixed_venue_cost)
  
  // 计算应急预算
  let contingency = divide_with_ceil(multiply(base_total, contingency_percent), 100)
  let total_budget = add(base_total, contingency)
  
  assert_eq(2625, variable_cost)  // 85 * 25 = 2125
  assert_eq(3125, base_total)  // 2625 + 500 = 3125
  assert_eq(313, contingency)  // ceil(3125 * 10 / 100) = 313
  assert_eq(3438, total_budget)  // 3125 + 313 = 3438
}

test "manufacturing_production_cycle" {
  // 制造业生产周期场景
  let daily_target = 500
  let machine_capacity_per_hour = 45
  let working_hours = 8
  
  // 计算每日实际产量
  let daily_production = multiply(machine_capacity_per_hour, working_hours)
  
  // 计算需要的机器数量
  let machines_needed = divide_with_ceil(daily_target, machine_capacity_per_hour)
  
  // 计算完成订单需要的天数
  let order_quantity = 5000
  let days_needed = divide_with_ceil(order_quantity, daily_production)
  
  assert_eq(360, daily_production)  // 45 * 8 = 360
  assert_eq(12, machines_needed)  // ceil(500/45) = 12
  assert_eq(14, days_needed)  // ceil(5000/360) = 14
}

test "multilingual_greeting_system" {
  // 多语言问候系统场景
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "María"
  let emoji_name = "🌟"
  
  // 测试不同语言的问候
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 小明!", chinese_greeting)
  assert_eq_string("Hello, María!", spanish_greeting)
  assert_eq_string("Hello, 🌟!", emoji_greeting)
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(358, final_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}