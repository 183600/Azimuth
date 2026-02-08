// Azimuth 项目专业级 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 测试覆盖：数值运算、字符串处理、数学算法、边界条件、实际应用场景

test "arithmetic_sequence_calculation" {
  // 等差数列计算测试
  // 首项 a1 = 3，公差 d = 4，项数 n = 6
  // 求第 n 项：an = a1 + (n-1)d
  // 求前 n 项和：Sn = n(a1 + an)/2
  
  let a1 = 3
  let d = 4
  let n = 6
  
  // 计算第6项
  let an = add(a1, multiply(n - 1, d))
  
  // 计算前6项和
  let sum = divide_with_ceil(multiply(n, add(a1, an)), 2)
  
  assert_eq(an, 23)
  assert_eq(sum, 78)
}

test "percentage_calculations" {
  // 百分比计算测试 - 商业应用场景
  let original_price = 200
  let discount_rate = 15
  let tax_rate = 8
  
  // 计算折扣金额
  let discount_amount = divide_with_ceil(multiply(original_price, discount_rate), 100)
  let discounted_price = subtract(original_price, discount_amount)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  let final_price = add(discounted_price, tax_amount)
  
  // 计算总折扣百分比
  let total_discount = divide_with_ceil(multiply(subtract(original_price, final_price), 100), original_price)
  
  assert_eq(discount_amount, 30)
  assert_eq(discounted_price, 170)
  assert_eq(tax_amount, 14)
  assert_eq(final_price, 184)
  assert_eq(total_discount, 8)
}

test "time_zone_conversion" {
  // 时区转换测试 - 实际应用场景
  let local_hour = 14  // 下午2点
  let local_minute = 30
  let offset_hours = -8  // 东八区到UTC的时差
  
  // 转换为UTC时间
  let utc_hour = add(local_hour, offset_hours)
  let utc_minute = local_minute
  
  // 处理跨日情况（简化处理）
  let adjusted_utc_hour = if utc_hour < 0 { add(utc_hour, 24) } else { utc_hour }
  
  // 转换为12小时制
  let hour_12 = if adjusted_utc_hour > 12 { subtract(adjusted_utc_hour, 12) } else { adjusted_utc_hour }
  let period = if adjusted_utc_hour >= 12 { "PM" } else { "AM" }
  
  assert_eq(utc_hour, 6)
  assert_eq(adjusted_utc_hour, 6)
  assert_eq(hour_12, 6)
  assert_eq_string(period, "AM")
}

test "inventory_optimization" {
  // 库存优化算法测试
  let demand_forecast = 1000
  let holding_cost_per_unit = 5
  let ordering_cost = 100
  let days_per_period = 30
  
  // 经济订货量 (EOQ) 简化计算
  let sqrt_component = divide_with_ceil(multiply(demand_forecast, multiply(ordering_cost, 2)), holding_cost_per_unit)
  let eoq = divide_with_ceil(sqrt_component, 10)  // 简化的平方根计算
  
  // 年订货次数
  let orders_per_year = divide_with_ceil(demand_forecast, eoq)
  
  // 订货间隔天数
  let days_between_orders = divide_with_ceil(days_per_period, divide_with_ceil(orders_per_year, 12))
  
  assert_eq(eoq, 200)
  assert_eq(orders_per_year, 5)
  assert_eq(days_between_orders, 6)
}

test "data_validation_rules" {
  // 数据验证规则测试
  let min_value = 10
  let max_value = 100
  let step = 5
  
  // 测试有效值
  let valid1 = 25
  let valid2 = 50
  let valid3 = 95
  
  // 测试无效值
  let invalid1 = 5   // 小于最小值
  let invalid2 = 105 // 大于最大值
  let invalid3 = 27  // 不符合步长
  
  // 验证函数（简化）
  let is_valid1 = valid1 >= min_value && valid1 <= max_value && (valid1 - min_value) % step == 0
  let is_valid2 = valid2 >= min_value && valid2 <= max_value && (valid2 - min_value) % step == 0
  let is_valid3 = valid3 >= min_value && valid3 <= max_value && (valid3 - min_value) % step == 0
  
  let is_invalid1 = invalid1 >= min_value
  let is_invalid2 = invalid2 <= max_value
  let is_invalid3 = (invalid3 - min_value) % step == 0
  
  assert_eq(is_valid1, true)
  assert_eq(is_valid2, true)
  assert_eq(is_valid3, true)
  assert_eq(is_invalid1, false)
  assert_eq(is_invalid2, false)
  assert_eq(is_invalid3, false)
}

test "signal_processing_simulation" {
  // 信号处理模拟测试
  let signal_amplitude = 100
  let noise_level = 15
  let samples = 8
  
  // 计算信噪比 (SNR)
  let snr_db = divide_with_ceil(multiply(20, 10), 1)  // 简化的SNR计算
  
  // 移动平均滤波
  let sample1 = 95
  let sample2 = 105
  let sample3 = 90
  let sample4 = 110
  let sample5 = 98
  let sample6 = 102
  let sample7 = 92
  let sample8 = 108
  
  let window_size = 3
  let avg1 = divide_with_ceil(add(add(sample1, sample2), sample3), window_size)
  let avg2 = divide_with_ceil(add(add(sample2, sample3), sample4), window_size)
  let avg3 = divide_with_ceil(add(add(sample3, sample4), sample5), window_size)
  
  // 计算方差（简化）
  let variance = divide_with_ceil(add(add(multiply(subtract(avg1, 100), subtract(avg1, 100)), 
                                       multiply(subtract(avg2, 100), subtract(avg2, 100))),
                                       multiply(subtract(avg3, 100), subtract(avg3, 100))), 3)
  
  assert_eq(snr_db, 200)
  assert_eq(avg1, 97)
  assert_eq(avg2, 102)
  assert_eq(avg3, 99)
  assert_eq(variance, 14)
}

test "resource_scheduling" {
  // 资源调度算法测试
  let tasks = [30, 20, 50, 10, 40]  // 任务持续时间
  let processors = 2
  
  // 最短作业优先 (SJF) 调度
  let task1 = 10  // 最短任务
  let task2 = 20
  let task3 = 30
  let task4 = 40
  let task5 = 50  // 最长任务
  
  // 双处理器并行执行
  let processor1_time = add(add(task1, task3), task5)  // 10 + 30 + 50 = 90
  let processor2_time = add(task2, task4)              // 20 + 40 = 60
  
  // 总执行时间（取最大值）
  let total_time = if processor1_time > processor2_time { processor1_time } else { processor2_time }
  
  // 处理器利用率
  let utilization = divide_with_ceil(multiply(total_time, 100), multiply(processor1_time, 2))
  
  assert_eq(processor1_time, 90)
  assert_eq(processor2_time, 60)
  assert_eq(total_time, 90)
  assert_eq(utilization, 50)
}

test "compression_ratio_analysis" {
  // 压缩比分析测试
  let original_data = [100, 200, 150, 300, 250, 180, 220, 160]
  let compressed_data = [50, 80, 60, 100]  // 压缩后的代表性值
  
  // 计算原始数据大小
  let original_size = add(add(add(add(add(add(add(original_data[0], original_data[1]), original_data[2]), 
                                    original_data[3]), original_data[4]), original_data[5]), 
                                    original_data[6]), original_data[7])
  
  // 计算压缩后大小
  let compressed_size = add(add(add(compressed_data[0], compressed_data[1]), compressed_data[2]), compressed_data[3])
  
  // 计算压缩比
  let compression_ratio = divide_with_ceil(multiply(original_size, 100), compressed_size)
  
  // 计算空间节省
  let space_saved = subtract(original_size, compressed_size)
  let savings_percentage = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  // 计算压缩效率（每单位压缩数据节省的空间）
  let efficiency = divide_with_ceil(space_saved, compressed_size)
  
  assert_eq(original_size, 1560)
  assert_eq(compressed_size, 290)
  assert_eq(compression_ratio, 538)
  assert_eq(space_saved, 1270)
  assert_eq(savings_percentage, 81)
  assert_eq(efficiency, 4)
}