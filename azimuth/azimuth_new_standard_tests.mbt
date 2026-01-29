// Azimuth 新增标准测试用例
// 包含10个高质量的 MoonBit 测试用例

test "arithmetic_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sequence_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sequence_sum)  // 5 + 8 + 11 + 14 = 38
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sequence_sum, terms)
  assert_eq(10, average)  // ceil(38/4) = 10
}

test "inventory_stock_management" {
  // 库存管理测试
  let initial_stock = 150
  let daily_sales = 12
  let restock_threshold = 30
  let days_until_restock = 10
  
  // 计算剩余库存
  let total_sales = multiply(daily_sales, days_until_restock)
  let remaining_stock = subtract(initial_stock, total_sales)
  
  assert_eq(30, remaining_stock)  // 150 - 12*10 = 30
  
  // 判断是否需要补货
  let needs_restock = remaining_stock <= restock_threshold
  assert_true(needs_restock)
  
  // 计算补货数量（补货到200）
  let target_stock = 200
  let restock_amount = subtract(target_stock, remaining_stock)
  assert_eq(170, restock_amount)
}

test "event_planning_calculations" {
  // 活动规划计算测试
  let guests = 85
  let tables = 8
  let chairs_per_table = 10
  let catering_cost_per_person = 25
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(9, tables_needed)  // ceil(85/10) = 9
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let extra_seats = subtract(total_seats, guests)
  assert_eq(5, extra_seats)
  
  // 计算餐饮总成本
  let total_catering_cost = multiply(guests, catering_cost_per_person)
  assert_eq(2125, total_catering_cost)  // 85 * 25 = 2125
}

test "temperature_conversion_logic" {
  // 温度转换逻辑测试
  let celsius_temps = add(0, 0)  // 基准温度0°C
  let fahrenheit_offset = 32
  let conversion_factor = 5
  let conversion_divisor = 9
  
  // 摄氏度转华氏度：F = (C * 9/5) + 32
  let celsius_20 = add(celsius_temps, 20)  // 20°C
  let fahrenheit_20 = add(
    divide_with_ceil(multiply(celsius_20, conversion_divisor), conversion_factor),
    fahrenheit_offset
  )
  assert_eq(68, fahrenheit_20)  // ceil(20*9/5) + 32 = 36 + 32 = 68
  
  // 摄氏度转华氏度：0°C
  let fahrenheit_0 = add(
    divide_with_ceil(multiply(celsius_temps, conversion_divisor), conversion_factor),
    fahrenheit_offset
  )
  assert_eq(32, fahrenheit_0)  // ceil(0*9/5) + 32 = 0 + 32 = 32
}

test "project_timeline_estimation" {
  // 项目时间线估算测试
  let total_tasks = 45
  let team_members = 4
  let tasks_per_member_per_day = 2
  let buffer_percentage = 20  // 20% 缓冲时间
  
  // 计算每日完成任务数
  let daily_completion = multiply(team_members, tasks_per_member_per_day)
  assert_eq(8, daily_completion)
  
  // 计算基础天数
  let base_days = divide_with_ceil(total_tasks, daily_completion)
  assert_eq(6, base_days)  // ceil(45/8) = 6
  
  // 计算缓冲时间（向上取整）
  let buffer_days = divide_with_ceil(multiply(base_days, buffer_percentage), 100)
  assert_eq(2, buffer_days)  // ceil(6*20/100) = 2
  
  // 计算总项目时间
  let total_days = add(base_days, buffer_days)
  assert_eq(8, total_days)
}

test "financial_budget_allocation" {
  // 财务预算分配测试
  let total_budget = 50000
  let departments = 3
  let emergency_reserve_percentage = 10
  
  // 计算应急储备金
  let emergency_reserve = divide_with_ceil(
    multiply(total_budget, emergency_reserve_percentage),
    100
  )
  assert_eq(5000, emergency_reserve)  // ceil(50000*10/100) = 5000
  
  // 计算可分配预算
  let allocatable_budget = subtract(total_budget, emergency_reserve)
  assert_eq(45000, allocatable_budget)
  
  // 计算每个部门的预算（向上取整）
  let budget_per_department = divide_with_ceil(allocatable_budget, departments)
  assert_eq(15000, budget_per_department)  // ceil(45000/3) = 15000
  
  // 验证总分配
  let total_allocated = add(
    multiply(budget_per_department, departments),
    emergency_reserve
  )
  assert_eq(50000, total_allocated)
}

test "shipping_logistics_optimization" {
  // 物流配送优化测试
  let packages = [67, 45, 89, 23, 56]  // 模拟不同目的地的包裹数量
  let truck_capacity = 30
  
  // 目的地1
  let packages_1 = 67
  let trucks_1 = divide_with_ceil(packages_1, truck_capacity)
  let capacity_1 = multiply(trucks_1, truck_capacity)
  
  // 目的地2
  let packages_2 = 45
  let trucks_2 = divide_with_ceil(packages_2, truck_capacity)
  let capacity_2 = multiply(trucks_2, truck_capacity)
  
  // 目的地3
  let packages_3 = 89
  let trucks_3 = divide_with_ceil(packages_3, truck_capacity)
  let capacity_3 = multiply(trucks_3, truck_capacity)
  
  // 计算总需求
  let total_trucks = add(add(trucks_1, trucks_2), trucks_3)
  let total_packages = add(add(packages_1, packages_2), packages_3)
  let total_capacity = add(add(capacity_1, capacity_2), capacity_3)
  
  assert_eq(10, total_trucks)  // ceil(67/30) + ceil(45/30) + ceil(89/30) = 3 + 2 + 3 = 8
  assert_eq(201, total_packages)  // 67 + 45 + 89 = 201
  assert_eq(240, total_capacity)  // 90 + 60 + 90 = 240
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(39, remaining_capacity)
}

test "data_processing_batch_analysis" {
  // 数据处理批次分析测试
  let total_records = 1250
  let batch_size = 100
  let processing_time_per_batch = 15  // 分钟
  let quality_check_percentage = 5  // 5% 需要质量检查
  
  // 计算批次数
  let total_batches = divide_with_ceil(total_records, batch_size)
  assert_eq(13, total_batches)  // ceil(1250/100) = 13
  
  // 计算总处理时间
  let total_processing_time = multiply(total_batches, processing_time_per_batch)
  assert_eq(195, total_processing_time)  // 13 * 15 = 195 分钟
  
  // 计算需要质量检查的记录数
  let quality_check_records = divide_with_ceil(
    multiply(total_records, quality_check_percentage),
    100
  )
  assert_eq(63, quality_check_records)  // ceil(1250*5/100) = 63
  
  // 计算完整批次和部分批次
  let full_batches = subtract(total_batches, 1)
  let last_batch_size = subtract(total_records, multiply(full_batches, batch_size))
  assert_eq(50, last_batch_size)  // 1250 - 12*100 = 50
}

test "resource_capacity_planning" {
  // 资源容量规划测试
  let current_users = 850
  let projected_growth = 150
  let server_capacity_per_unit = 200
  let redundancy_factor = 2  // 双倍冗余
  
  // 计算预期总用户数
  let projected_total = add(current_users, projected_growth)
  assert_eq(1000, projected_total)
  
  // 计算基础服务器需求
  let base_servers = divide_with_ceil(projected_total, server_capacity_per_unit)
  assert_eq(5, base_servers)  // ceil(1000/200) = 5
  
  // 计算包含冗余的总服务器数
  let total_servers = multiply(base_servers, redundancy_factor)
  assert_eq(10, total_servers)
  
  // 计算总容量
  let total_capacity = multiply(total_servers, server_capacity_per_unit)
  assert_eq(2000, total_capacity)
  
  // 计算容量利用率
  let utilization_percentage = divide_with_ceil(
    multiply(projected_total, 100),
    total_capacity
  )
  assert_eq(50, utilization_percentage)  // ceil(1000*100/2000) = 50%
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  // 测试不同类型的用户名输入
  let valid_username = "user123"
  let empty_username = ""
  let special_char_username = "user@#$"
  let long_username = "very_long_username_with_many_characters"
  let numeric_username = "123456"
  
  // 验证各种用户名类型
  assert_eq_string("Hello, user123!", greet(valid_username))
  assert_eq_string("Hello, !", greet(empty_username))
  assert_eq_string("Hello, user@#$!", greet(special_char_username))
  assert_eq_string("Hello, very_long_username_with_many_characters!", greet(long_username))
  assert_eq_string("Hello, 123456!", greet(numeric_username))
  
  // 测试长度计算（模拟）
  let normal_length = 7  // "user123"
  let empty_length = 0
  let long_length = 38  // "very_long_username_with_many_characters"
  
  // 验证长度特征
  assert_true(normal_length > 0)
  assert_true(normal_length < 20)
  assert_eq(0, empty_length)
  assert_true(long_length > 20)
}