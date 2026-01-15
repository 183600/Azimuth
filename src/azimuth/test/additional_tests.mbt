// Azimuth 项目额外的 MoonBit 测试用例
// 专注于尚未充分覆盖的功能和场景

test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 函数的基本功能
  // 正常情况：向上取整除法
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 需要向上取整的情况
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(11, 2))  // 11/2 = 5.5 → 6
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))   // 7/2 = 3.5 → 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))   // 8/3 = 2.67 → 3
}

test "divide_with_ceil_negative_numbers" {
  // 测试 divide_with_ceil 函数的负数处理
  // 负数除法的向上取整行为
  
  // 负数除以正数
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-10, 2))   // -10/2 = -5
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-7, 2))    // -7/2 = -3.5 → -3（向上取整）
  
  // 正数除以负数
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(10, -2))   // 10/-2 = -5
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(7, -2))    // 7/-2 = -3.5 → -3（向上取整）
  
  // 负数除以负数
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-10, -2))   // -10/-2 = 5
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-7, -2))    // -7/-2 = 3.5 → 4
}

test "divide_with_ceil_edge_cases" {
  // 测试 divide_with_ceil 函数的边界情况
  
  // 除数为1
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  @azimuth.assert_eq(-10, @azimuth.divide_with_ceil(-10, 1))
  
  // 被除数为0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -3))
  
  // 相等的数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(5, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
}

test "complex_calculation_with_divide_ceil" {
  // 测试 divide_with_ceil 在复杂计算中的应用
  
  // 场景：计算需要的页数（每页固定条目数）
  let total_items = 23
  let items_per_page = 5
  let needed_pages = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(5, needed_pages)  // 23/5 = 4.6 → 5页
  
  // 场景：计算需要的箱子数（每个箱子固定容量）
  let total_products = 37
  let box_capacity = 8
  let needed_boxes = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(5, needed_boxes)  // 37/8 = 4.625 → 5个箱子
  
  // 场景：计算需要的车辆数（每辆车固定载重）
  let total_weight = 1500
  let vehicle_capacity = 350
  let needed_vehicles = @azimuth.divide_with_ceil(total_weight, vehicle_capacity)
  @azimuth.assert_eq(5, needed_vehicles)  // 1500/350 = 4.29 → 5辆车
}

test "combination_operations_with_divide_ceil" {
  // 测试 divide_with_ceil 与其他函数的组合操作
  
  let a = 17
  let b = 4
  let c = 3
  
  // 计算 (a + b) / c 的向上取整
  let sum_ab = @azimuth.add(a, b)  // 17 + 4 = 21
  let result1 = @azimuth.divide_with_ceil(sum_ab, c)  // 21/3 = 7
  @azimuth.assert_eq(7, result1)
  
  // 计算 (a * b) / c 的向上取整
  let product_ab = @azimuth.multiply(a, b)  // 17 * 4 = 68
  let result2 = @azimuth.divide_with_ceil(product_ab, c)  // 68/3 = 22.67 → 23
  @azimuth.assert_eq(23, result2)
  
  // 复杂计算：((a + b) * c) / (a - c)
  let numerator = @azimuth.multiply(sum_ab, c)  // 21 * 3 = 63
  let denominator = @azimuth.add(a, -c)  // 17 - 3 = 14
  let result3 = @azimuth.divide_with_ceil(numerator, denominator)  // 63/14 = 4.5 → 5
  @azimuth.assert_eq(5, result3)
}

test "financial_calculations_with_divide_ceil" {
  // 测试 divide_with_ceil 在金融计算中的应用
  
  // 场景：计算需要的付款期数
  let total_amount = 1250
  let monthly_payment = 300
  let payment_periods = @azimuth.divide_with_ceil(total_amount, monthly_payment)
  @azimuth.assert_eq(5, payment_periods)  // 1250/300 = 4.17 → 5期
  
  // 场景：计算需要的批次数
  let total_units = 475
  let batch_size = 100
  let batches_needed = @azimuth.divide_with_ceil(total_units, batch_size)
  @azimuth.assert_eq(5, batches_needed)  // 475/100 = 4.75 → 5批次
  
  // 场景：计算投资回报期（简化）
  let investment = 10000
  let monthly_return = 2200
  let payback_months = @azimuth.divide_with_ceil(investment, monthly_return)
  @azimuth.assert_eq(5, payback_months)  // 10000/2200 = 4.55 → 5个月
}

test "resource_allocation_with_divide_ceil" {
  // 测试 divide_with_ceil 在资源分配中的应用
  
  // 场景：计算需要的会议室数量
  let participants = 45
  let room_capacity = 10
  let rooms_needed = @azimuth.divide_with_ceil(participants, room_capacity)
  @azimuth.assert_eq(5, rooms_needed)  // 45/10 = 4.5 → 5间会议室
  
  // 场景：计算需要的服务器数量
  let total_load = 850
  let server_capacity = 200
  let servers_needed = @azimuth.divide_with_ceil(total_load, server_capacity)
  @azimuth.assert_eq(5, servers_needed)  // 850/200 = 4.25 → 5台服务器
  
  // 场景：计算需要的存储设备数量
  let data_size = 950
  let device_capacity = 256
  let devices_needed = @azimuth.divide_with_ceil(data_size, device_capacity)
  @azimuth.assert_eq(4, devices_needed)  // 950/256 = 3.71 → 4台设备
}

test "time_scheduling_with_divide_ceil" {
  // 测试 divide_with_ceil 在时间调度中的应用
  
  // 场景：计算完成任务需要的完整工作日
  let total_work_hours = 37
  let work_hours_per_day = 8
  let work_days_needed = @azimuth.divide_with_ceil(total_work_hours, work_hours_per_day)
  @azimuth.assert_eq(5, work_days_needed)  // 37/8 = 4.625 → 5个工作日
  
  // 场景：计算需要的完整周数
  let total_days = 32
  let days_per_week = 7
  let weeks_needed = @azimuth.divide_with_ceil(total_days, days_per_week)
  @azimuth.assert_eq(5, weeks_needed)  // 32/7 = 4.57 → 5周
  
  // 场景：计算需要的完整月数（简化：每月30天）
  let total_project_days = 95
  let days_per_month = 30
  let months_needed = @azimuth.divide_with_ceil(total_project_days, days_per_month)
  @azimuth.assert_eq(4, months_needed)  // 95/30 = 3.17 → 4个月
}

test "extreme_boundary_divide_ceil" {
  // 测试 divide_with_ceil 的极端边界情况
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数除法
  @azimuth.assert_eq(214748365, @azimuth.divide_with_ceil(max_val, 10))  // 2147483647/10 = 214748364.7 → 214748365
  @azimuth.assert_eq(214748364, @azimuth.divide_with_ceil(-2147483640, -10))  // -2147483640/-10 = 214748364
  
  // 小数除以大数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, max_val))  // 100/max_val ≈ 0.000000046 → 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_val))  // 0/max_val = 0
  
  // 边界值组合
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))  // max_val/max_val = 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))  // min_val/min_val = 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_val, max_val))  // min_val/max_val ≈ -1 → 0（整数除法）
}