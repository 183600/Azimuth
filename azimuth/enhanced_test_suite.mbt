// 增强的 MoonBit 测试套件
// 为 azimuth 库添加额外的高质量测试用例

// 测试复杂计算场景：项目管理
test project_management_calculation {
  // 场景：计算项目资源和时间安排
  let team_size = 8
  let story_points = 120
  let velocity_per_sprint = 25
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(story_points, velocity_per_sprint)
  assert_eq(5, sprints_needed)  // ceil(120/25) = 5
  
  // 计算总工作量
  let total_effort = multiply(team_size, multiply(sprints_needed, 2))  // 每冲刺2周
  assert_eq(80, total_effort)  // 8 * 5 * 2 = 80人周
  
  // 计算剩余故事点
  let total_capacity = multiply(sprints_needed, velocity_per_sprint)
  let buffer_capacity = subtract(total_capacity, story_points)
  assert_eq(5, buffer_capacity)  // 125 - 120 = 5
}

// 测试财务计算：贷款和利息
test financial_calculation_scenario {
  // 场景：计算贷款分期付款
  let loan_amount = 50000
  let monthly_payment = 2200
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)  // 23 * 2200 = 50600
  
  // 计算利息
  let interest = subtract(total_payment, loan_amount)
  assert_eq(600, interest)  // 50600 - 50000 = 600
}

// 测试库存管理：批量处理
test inventory_management_scenario {
  // 场景：计算仓库存储需求
  
  // 产品1：157个，每箱24个
  let product1_qty = 157
  let product1_box_size = 24
  let boxes1 = divide_with_ceil(product1_qty, product1_box_size)
  
  // 产品2：89个，每箱12个
  let product2_qty = 89
  let product2_box_size = 12
  let boxes2 = divide_with_ceil(product2_qty, product2_box_size)
  
  // 产品3：267个，每箱30个
  let product3_qty = 267
  let product3_box_size = 30
  let boxes3 = divide_with_ceil(product3_qty, product3_box_size)
  
  // 产品4：45个，每箱10个
  let product4_qty = 45
  let product4_box_size = 10
  let boxes4 = divide_with_ceil(product4_qty, product4_box_size)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes1, boxes2), add(boxes3, boxes4))
  assert_eq(29, total_boxes)  // 7 + 8 + 9 + 5 = 29
  
  // 验证各产品的箱子数
  assert_eq(7, boxes1)   // ceil(157/24) = 7
  assert_eq(8, boxes2)   // ceil(89/12) = 8
  assert_eq(9, boxes3)   // ceil(267/30) = 9
  assert_eq(5, boxes4)   // ceil(45/10) = 5
}

// 测试网络带宽计算
test network_bandwidth_calculation {
  // 场景：计算数据传输时间和带宽需求
  let data_size_mb = 2048  // 2GB
  let network_speed_mbps = 50
  
  // 计算传输时间（秒）
  let transfer_time = divide_with_ceil(data_size_mb, network_speed_mbps)
  assert_eq(41, transfer_time)  // ceil(2048/50) = 41
  
  // 计算在多个并行连接下的时间
  let parallel_connections = 4
  let effective_speed = multiply(network_speed_mbps, parallel_connections)
  let parallel_time = divide_with_ceil(data_size_mb, effective_speed)
  assert_eq(11, parallel_time)  // ceil(2048/200) = 11
  
  // 计算时间节省
  let time_saved = subtract(transfer_time, parallel_time)
  assert_eq(30, time_saved)  // 41 - 11 = 30
}

// 测试字符串处理：多语言支持
test multilingual_string_processing {
  // 测试各种语言和特殊字符的问候
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, ελληνικά!", greet("ελληνικά"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

// 测试数学算法：素数相关计算
test mathematical_algorithm_test {
  // 场景：使用基本运算实现素数检查的辅助计算
  let number = 97
  
  // 计算平方根的整数部分（使用简化方法）
  let sqrt_estimate = divide_with_ceil(number, 10)
  let refined_estimate = add(sqrt_estimate, divide_with_ceil(sqrt_estimate, 2))
  assert_eq(15, refined_estimate)  // ceil(97/10) + ceil(10/2) = 10 + 5 = 15
  
  // 计算检查范围
  let check_range = multiply(refined_estimate, 2)
  assert_eq(30, check_range)  // 15 * 2 = 30
  
  // 验证奇偶性
  let is_even = divide_with_ceil(number, 2)
  let even_check = multiply(is_even, 2)
  assert_eq(98, even_check)  // ceil(97/2) * 2 = 49 * 2 = 98
  assert_true(number <> even_check)  // 97 != 98，所以是奇数
}

// 测试几何计算：面积和体积
test geometric_calculation_test {
  // 场景：计算几何形状的面积和体积
  // 矩形房间计算
  let room_length = 12
  let room_width = 8
  let room_height = 3
  
  // 计算地板面积
  let floor_area = multiply(room_length, room_width)
  assert_eq(96, floor_area)  // 12 * 8 = 96
  
  // 计算墙壁总面积
  let wall_area_1 = multiply(room_length, room_height)
  let wall_area_2 = multiply(room_width, room_height)
  let total_wall_area = multiply(add(wall_area_1, wall_area_2), 2)
  assert_eq(120, total_wall_area)  // 2 * (12*3 + 8*3) = 120
  
  // 计算房间体积
  let room_volume = multiply(floor_area, room_height)
  assert_eq(288, room_volume)  // 96 * 3 = 288
  
  // 计算需要的油漆桶数（每桶覆盖25平方米）
  let paint_buckets = divide_with_ceil(total_wall_area, 25)
  assert_eq(5, paint_buckets)  // ceil(120/25) = 5
}

// 测试时间计算：工作日和假期
test time_calculation_scenario {
  // 场景：计算项目工作日和假期安排
  let project_days = 45
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(project_days, work_days_per_week)
  assert_eq(9, weeks_needed)  // ceil(45/5) = 9
  
  // 计算总日历天数（包括周末）
  let calendar_days = multiply(weeks_needed, 7)
  assert_eq(63, calendar_days)  // 9 * 7 = 63
  
  // 计算周末天数
  let weekend_days = subtract(calendar_days, project_days)
  assert_eq(18, weekend_days)  // 63 - 45 = 18
  
  // 计算假期天数（假设每3周休息1周）
  let holiday_weeks = divide_with_ceil(weeks_needed, 3)
  let holiday_days = multiply(holiday_weeks, 7)
  assert_eq(21, holiday_days)  // ceil(9/3) * 7 = 21
}

// 测试能源消耗计算
test energy_consumption_calculation {
  // 场景：计算建筑能源消耗
  let daily_consumption_kwh = 850
  let solar_panel_generation = 200
  let battery_capacity_kwh = 5000
  
  // 计算净日消耗量
  let net_daily_consumption = subtract(daily_consumption_kwh, solar_panel_generation)
  assert_eq(650, net_daily_consumption)  // 850 - 200 = 650
  
  // 计算电池可持续天数
  let battery_days = divide_with_ceil(battery_capacity_kwh, net_daily_consumption)
  assert_eq(8, battery_days)  // ceil(5000/650) = 8
  
  // 计算月消耗量
  let monthly_consumption = multiply(net_daily_consumption, 30)
  assert_eq(19500, monthly_consumption)  // 650 * 30 = 19500
  
  // 计算需要的电池数量以满足30天需求
  let batteries_needed = divide_with_ceil(monthly_consumption, battery_capacity_kwh)
  assert_eq(4, batteries_needed)  // ceil(19500/5000) = 4
}