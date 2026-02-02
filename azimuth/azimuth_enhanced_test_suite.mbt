// Azimuth 增强测试套件
// 包含 8 个高质量的 MoonBit 标准测试用例

test "calendar_calculation_scenario" {
  // 日历计算场景：计算工作日和周末分布
  let total_days = 365
  let weeks = divide_with_ceil(total_days, 7)
  let remaining_days = subtract(total_days, multiply(weeks, 7))
  
  assert_eq(53, weeks)  // ceil(365/7) = 53
  assert_eq(1, remaining_days)  // 365 - 53*7 = 1
  
  // 计算工作日数量（假设每周5个工作日）
  // 简化：由于remaining_days=1且小于5，所以直接加1
  let workdays = add(multiply(weeks, 5), 1)
  assert_eq(266, workdays)  // 53*5 + 1 = 265 + 1 = 266
}

test "temperature_conversion_workflow" {
  // 温度转换工作流测试
  // 验证温度转换公式：F = C * 9/5 + 32
  let celsius = 25  // 测试 25°C
  let celsius_times_nine = multiply(celsius, 9)
  let celsius_times_nine_divided_by_five = divide_with_ceil(celsius_times_nine, 5)
  let fahrenheit = add(celsius_times_nine_divided_by_five, 32)
  
  assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
}

test "inventory_reorder_point_calculation" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算重新订购点
  let lead_time_demand = multiply(daily_demand, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  assert_eq(125, reorder_point)  // 15*7 + 20 = 105 + 20 = 125
  
  // 计算订购批量（基于经济订购量简化版）
  let annual_demand = multiply(daily_demand, 365)
  let orders_per_year = 12
  let order_quantity = divide_with_ceil(annual_demand, orders_per_year)
  
  assert_eq(457, order_quantity)  // ceil(15*365/12) = ceil(5475/12) = 457
}

test "project_milestone_tracking" {
  // 项目里程碑跟踪测试
  let total_tasks = 120
  let team_size = 8
  let tasks_per_member_per_day = 2
  let working_days = 15
  
  // 计算团队日产能
  let daily_capacity = multiply(team_size, tasks_per_member_per_day)
  let total_capacity = multiply(daily_capacity, working_days)
  
  assert_eq(16, daily_capacity)  // 8 * 2 = 16
  assert_eq(240, total_capacity)  // 16 * 15 = 240
  
  // 计算完成项目所需天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(8, days_needed)  // ceil(120/16) = 8
  
  // 验证项目可以在规定时间内完成
  assert_true(days_needed <= working_days)
}

test "budget_allocation_optimization" {
  // 预算分配优化测试
  let total_budget = 50000
  let departments = 4
  let min_budget_per_dept = 10000
  
  // 计算基础分配
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(12500, base_allocation)  // ceil(50000/4) = 12500
  
  // 验证每个部门都能获得最低预算
  assert_true(base_allocation >= min_budget_per_dept)
  
  // 计算剩余预算
  let allocated_total = multiply(base_allocation, departments)
  let remaining_budget = subtract(total_budget, multiply(base_allocation, subtract(departments, 1)))
  
  assert_eq(50000, allocated_total)  // 12500 * 4 = 50000
  assert_eq(12500, remaining_budget)  // 最后一个部门的预算
}

test "shipping_container_optimization" {
  // 货运集装箱优化测试
  let total_packages = 234
  let container_capacity = 50
  let weight_per_package = 25
  let max_weight_per_container = 1200
  
  // 基于数量计算需要的集装箱
  let containers_by_count = divide_with_ceil(total_packages, container_capacity)
  assert_eq(5, containers_by_count)  // ceil(234/50) = 5
  
  // 基于重量计算需要的集装箱
  let total_weight = multiply(total_packages, weight_per_package)
  let containers_by_weight = divide_with_ceil(total_weight, max_weight_per_container)
  assert_eq(5, containers_by_weight)  // ceil(234*25/1200) = ceil(5850/1200) = 5
  
  // 由于两者计算结果相同，直接使用该值
  let containers_needed = containers_by_count
  assert_eq(5, containers_needed)
  
  // 计算空间利用率
  let total_capacity = multiply(containers_needed, container_capacity)
  let utilization = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(94, utilization)  // floor(23400/250) = 93%
}

test "restaurant_seating_arrangement" {
  // 餐厅座位安排测试
  let total_guests = 87
  let tables_4person = 8
  let tables_6person = 5
  let tables_8person = 3
  
  // 计算各类型桌子的总容量
  let capacity_4person = multiply(tables_4person, 4)
  let capacity_6person = multiply(tables_6person, 6)
  let capacity_8person = multiply(tables_8person, 8)
  
  assert_eq(32, capacity_4person)   // 8 * 4 = 32
  assert_eq(30, capacity_6person)   // 5 * 6 = 30
  assert_eq(24, capacity_8person)   // 3 * 8 = 24
  
  // 计算总容量
  let total_capacity = add(add(capacity_4person, capacity_6person), capacity_8person)
  assert_eq(86, total_capacity)  // 32 + 30 + 24 = 86
  
  // 检查是否需要额外座位
  let additional_seats_needed = subtract(total_guests, total_capacity)
  assert_eq(1, additional_seats_needed)  // 需要额外1个座位
  
  // 计算需要添加的桌子数量
  let extra_tables = divide_with_ceil(additional_seats_needed, 4)
  assert_eq(1, extra_tables)  // ceil(1/4) = 1
}

test "digital_storage_planning" {
  // 数字存储规划测试
  let file_count = 1250
  let avg_file_size_mb = 15
  let drive_capacity_gb = 20
  let drive_capacity_mb = multiply(drive_capacity_gb, 1024)
  
  // 计算总存储需求
  let total_storage_mb = multiply(file_count, avg_file_size_mb)
  assert_eq(18750, total_storage_mb)  // 1250 * 15 = 18750
  
  // 计算需要的驱动器数量
  let drives_needed = divide_with_ceil(total_storage_mb, drive_capacity_mb)
  assert_eq(10, drives_needed)  // ceil(18750/20480) = 10
  
  // 计算总可用容量
  let total_available_mb = multiply(drives_needed, drive_capacity_mb)
  assert_eq(204800, total_available_mb)  // 10 * 20480 = 204800
  
  // 计算剩余空间
  let remaining_space_mb = subtract(total_available_mb, total_storage_mb)
  assert_eq(186050, remaining_space_mb)  // 204800 - 18750 = 186050
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_storage_mb, 100), total_available_mb)
  assert_eq(10, utilization_percent)  // floor(1875000/204800) = 9%
}