// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "subtract_basic_functionality" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期
  let loan_amount = 12000
  let monthly_payment = 850
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(15, months_needed)  // ceil(12000/850) = 15
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(12750, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(750, final_payment)
}

test "inventory_optimization" {
  // 库存优化：多产品装箱计算
  // 产品A：37个，每箱12个
  let product_a = 37
  let box_capacity_a = 12
  let boxes_a = divide_with_ceil(product_a, box_capacity_a)
  
  // 产品B：89个，每箱15个
  let product_b = 89
  let box_capacity_b = 15
  let boxes_b = divide_with_ceil(product_b, box_capacity_b)
  
  // 计算总箱数
  let total_boxes = add(boxes_a, boxes_b)
  assert_eq(13, total_boxes)  // ceil(37/12) + ceil(89/15) = 4 + 6 = 10
  
  // 计算剩余空间
  let total_capacity = add(multiply(boxes_a, box_capacity_a), multiply(boxes_b, box_capacity_b))
  let total_items = add(product_a, product_b)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(16, remaining_space)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 8
  
  // 计算最后一项
  let last_term = add(first_term, multiply(number_of_terms - 1, common_difference))
  
  // 使用求和公式：S = n * (a1 + an) / 2
  let sum = divide_with_ceil(multiply(number_of_terms, add(first_term, last_term)), 2)
  
  assert_eq(31, last_term)  // 3 + 7*4 = 31
  assert_eq(136, sum)       // 8 * (3 + 31) / 2 = 8 * 34 / 2 = 136
}

test "energy_efficiency_calculation" {
  // 能效计算：设备功耗分析
  let device_a_power = 150  // 瓦特
  let device_b_power = 75   // 瓦特
  let device_c_power = 200  // 瓦特
  
  let hours_per_day = 8
  let days_per_month = 30
  
  // 计算日耗电量
  let daily_consumption = add(add(device_a_power, device_b_power), device_c_power)
  let daily_kwh = divide_with_ceil(daily_consumption, 1000)  // 转换为千瓦时
  
  // 计算月耗电量
  let monthly_kwh = multiply(daily_kwh, days_per_month)
  
  assert_eq(425, daily_consumption)  // 150 + 75 + 200 = 425W
  assert_eq(1, daily_kwh)           // ceil(425/1000) = 1kWh
  assert_eq(30, monthly_kwh)         // 1 * 30 = 30kWh
}

test "project_resource_allocation" {
  // 项目资源分配：团队任务分配
  let total_tasks = 67
  let team_members = 5
  let base_tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(14, base_tasks_per_member)  // ceil(67/5) = 14
  
  // 计算总分配任务数
  let total_allocated = multiply(base_tasks_per_member, team_members)
  assert_eq(70, total_allocated)
  
  // 计算多余任务槽位
  let extra_slots = subtract(total_allocated, total_tasks)
  assert_eq(3, extra_slots)
  
  // 计算实际工作量分布
  let members_with_extra = extra_slots
  let members_with_base = subtract(team_members, members_with_extra)
  
  assert_eq(3, members_with_extra)  // 3个成员多1个任务
  assert_eq(2, members_with_base)   // 2个成员保持基础任务数
}

test "string_processing_multilingual" {
  // 多语言字符串处理
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  assert_eq_string("Hello, Cairo!", greet("Cairo"))
}

test "complex_shipping_calculation" {
  // 复杂运输计算：多地区配送
  let region_a_distance = 150  // 公里
  let region_b_distance = 320  // 公里
  let region_c_distance = 85   // 公里
  
  let base_rate_per_km = 2     // 每公里基础费率
  let fuel_surcharge = 15      // 燃油附加费百分比
  let handling_fee = 25        // 固定处理费
  
  // 计算各地区运费
  let freight_a = multiply(region_a_distance, base_rate_per_km)
  let freight_b = multiply(region_b_distance, base_rate_per_km)
  let freight_c = multiply(region_c_distance, base_rate_per_km)
  
  // 计算燃油附加费
  let total_freight = add(add(freight_a, freight_b), freight_c)
  let fuel_charge = divide_with_ceil(multiply(total_freight, fuel_surcharge), 100)
  
  // 计算总费用
  let total_cost = add(add(total_freight, fuel_charge), handling_fee)
  
  assert_eq(300, freight_a)   // 150 * 2 = 300
  assert_eq(640, freight_b)   // 320 * 2 = 640
  assert_eq(170, freight_c)   // 85 * 2 = 170
  assert_eq(1110, total_freight)  // 300 + 640 + 170 = 1110
  assert_eq(167, fuel_charge)     // ceil(1110 * 15 / 100) = 167
  assert_eq(1302, total_cost)     // 1110 + 167 + 25 = 1302
}

test "temperature_conversion_analysis" {
  // 温度转换分析：批量温度数据处理
  let celsius_readings = 5
  let fahrenheit_readings = 3
  let kelvin_readings = 2
  
  // 模拟温度转换计算（简化）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let avg_celsius = 25
  let converted_to_fahrenheit = add(divide_with_ceil(multiply(avg_celsius, 9), 5), 32)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let avg_fahrenheit = 77
  let converted_to_celsius = divide_with_ceil(multiply(subtract(avg_fahrenheit, 32), 5), 9)
  
  // 计算总读数
  let total_readings = add(add(celsius_readings, fahrenheit_readings), kelvin_readings)
  
  assert_eq(77, converted_to_fahrenheit)  // ceil(25 * 9 / 5) + 32 = 45 + 32 = 77
  assert_eq(25, converted_to_celsius)     // ceil((77 - 32) * 5 / 9) = ceil(45 * 5 / 9) = 25
  assert_eq(10, total_readings)           // 5 + 3 + 2 = 10
}

test "digital_storage_calculation" {
  // 数字存储计算：文件压缩和备份策略
  let original_files_gb = 45
  let compression_ratio = 40  // 压缩到40%
  let backup_copies = 3
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(multiply(original_files_gb, compression_ratio), 100)
  
  // 计算总备份大小
  let total_backup_size = multiply(compressed_size, backup_copies)
  
  // 计算节省的存储空间
  let original_total_size = multiply(original_files_gb, backup_copies)
  let space_saved = subtract(original_total_size, total_backup_size)
  
  // 计算存储设备需求（向上取整到最接近的10GB）
  let storage_needed = multiply(divide_with_ceil(total_backup_size, 10), 10)
  
  assert_eq(18, compressed_size)      // ceil(45 * 40 / 100) = 18
  assert_eq(54, total_backup_size)    // 18 * 3 = 54
  assert_eq(81, space_saved)          // 45 * 3 - 54 = 135 - 54 = 81
  assert_eq(60, storage_needed)       // ceil(54/10) * 10 = 60
}