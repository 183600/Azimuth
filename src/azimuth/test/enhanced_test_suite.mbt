// Azimuth 增强测试用例集合
// 10个高质量的 MoonBit 测试用例，涵盖各种实际应用场景

test "statistical_analysis_operations" {
  // 统计分析操作测试
  // 场景：计算数据集的基本统计量
  let data_point_1 = 15
  let data_point_2 = 23
  let data_point_3 = 18
  let data_point_4 = 31
  let data_point_5 = 27
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data_point_1, data_point_2), data_point_3), data_point_4), data_point_5)
  @azimuth.assert_eq(114, sum)
  
  // 计算平均值（使用向上取整除法）
  let count = 5
  let average = @azimuth.divide_with_ceil(sum, count)
  @azimuth.assert_eq(23, average)  // ceil(114/5) = 23
  
  // 计算最大值和最小值的差
  let max_value = 31
  let min_value = 15
  let range = @azimuth.add(max_value, -min_value)
  @azimuth.assert_eq(16, range)
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  // 场景：多产品仓库的最优装箱策略
  let product_a_quantity = 47
  let product_b_quantity = 63
  let product_c_quantity = 29
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = @azimuth.divide_with_ceil(product_a_quantity, box_capacity)
  let boxes_b = @azimuth.divide_with_ceil(product_b_quantity, box_capacity)
  let boxes_c = @azimuth.divide_with_ceil(product_c_quantity, box_capacity)
  
  @azimuth.assert_eq(4, boxes_a)  // ceil(47/12) = 4
  @azimuth.assert_eq(6, boxes_b)  // ceil(63/12) = 6
  @azimuth.assert_eq(3, boxes_c)  // ceil(29/12) = 3
  
  // 计算总箱子数和总容量
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  let total_capacity = @azimuth.multiply(total_boxes, box_capacity)
  
  @azimuth.assert_eq(13, total_boxes)
  @azimuth.assert_eq(156, total_capacity)
  
  // 计算总产品数量
  let total_products = @azimuth.add(@azimuth.add(product_a_quantity, product_b_quantity), product_c_quantity)
  @azimuth.assert_eq(139, total_products)
  
  // 计算空间利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(90, utilization)  // ceil(13900/156) = 90
}

test "time_management_calculations" {
  // 时间管理计算测试
  // 场景：项目时间规划和资源分配
  let total_tasks = 87
  let team_members = 5
  let working_hours_per_day = 8
  let days_per_sprint = 10
  
  // 计算团队每日总工时
  let daily_team_hours = @azimuth.multiply(team_members, working_hours_per_day)
  @azimuth.assert_eq(40, daily_team_hours)
  
  // 计算冲刺总工时
  let sprint_hours = @azimuth.multiply(daily_team_hours, days_per_sprint)
  @azimuth.assert_eq(400, sprint_hours)
  
  // 假设每个任务平均需要4小时，计算需要的冲刺数
  let hours_per_task = 4
  let total_hours_needed = @azimuth.multiply(total_tasks, hours_per_task)
  let sprints_needed = @azimuth.divide_with_ceil(total_hours_needed, sprint_hours)
  
  @azimuth.assert_eq(1, sprints_needed)  // ceil(87*4/400) = 1
  
  // 计算剩余工时
  let remaining_hours = @azimuth.add(sprint_hours, -total_hours_needed)
  @azimuth.assert_eq(52, remaining_hours)  // 400 - 348 = 52
}

test "financial_budget_allocation" {
  // 财务预算分配测试
  // 场景：部门预算的公平分配
  let total_budget = 50000
  let departments = 7
  let minimum_budget_per_dept = 5000
  
  // 计算平均分配
  let equal_share = @azimuth.divide_with_ceil(total_budget, departments)
  @azimuth.assert_eq(7143, equal_share)  // ceil(50000/7) = 7143
  
  // 验证每个部门都能获得最低预算
  @azimuth.assert_eq(1, if (equal_share >= minimum_budget_per_dept) { 1 } else { 0 })
  
  // 计算总分配金额
  let total_allocated = @azimuth.multiply(equal_share, departments)
  @azimuth.assert_eq(50001, total_allocated)
  
  // 计算预算溢出
  let budget_overrun = @azimuth.add(total_allocated, -total_budget)
  @azimuth.assert_eq(1, budget_overrun)
}

test "geometric_calculations" {
  // 几何计算测试
  // 场景：计算矩形的面积和周长相关运算
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算需要多少个面积为15的地砖来覆盖整个矩形
  let tile_area = 15
  let tiles_needed = @azimuth.divide_with_ceil(area, tile_area)
  @azimuth.assert_eq(7, tiles_needed)  // ceil(96/15) = 7
  
  // 计算实际覆盖面积
  let actual_coverage = @azimuth.multiply(tiles_needed, tile_area)
  @azimuth.assert_eq(105, actual_coverage)
  
  // 计算浪费的面积
  let wasted_area = @azimuth.add(actual_coverage, -area)
  @azimuth.assert_eq(9, wasted_area)
}

test "temperature_conversion_pipeline" {
  // 温度转换流水线测试
  // 场景：批量温度数据转换和处理
  let celsius_1 = 25
  let celsius_2 = -10
  let celsius_3 = 100
  let celsius_4 = 0
  let celsius_5 = 37
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit_1 = @azimuth.add(@azimuth.multiply(celsius_1, 9), 160)  // 25*9/5 + 32 = 77
  let fahrenheit_2 = @azimuth.add(@azimuth.multiply(celsius_2, 9), 160)  // -10*9/5 + 32 = 14
  let fahrenheit_3 = @azimuth.add(@azimuth.multiply(celsius_3, 9), 160)  // 100*9/5 + 32 = 212
  let fahrenheit_4 = @azimuth.add(@azimuth.multiply(celsius_4, 9), 160)  // 0*9/5 + 32 = 32
  let fahrenheit_5 = @azimuth.add(@azimuth.multiply(celsius_5, 9), 160)  // 37*9/5 + 32 = 98.6 -> 98
  
  // 使用整数运算（需要除以5，这里使用向上取整）
  let f_1 = @azimuth.divide_with_ceil(fahrenheit_1, 5)
  let f_2 = @azimuth.divide_with_ceil(fahrenheit_2, 5)
  let f_3 = @azimuth.divide_with_ceil(fahrenheit_3, 5)
  let f_4 = @azimuth.divide_with_ceil(fahrenheit_4, 5)
  let f_5 = @azimuth.divide_with_ceil(fahrenheit_5, 5)
  
  @azimuth.assert_eq(77, f_1)   // ceil(385/5) = 77
  @azimuth.assert_eq(14, f_2)   // ceil(70/5) = 14
  @azimuth.assert_eq(212, f_3)  // ceil(1060/5) = 212
  @azimuth.assert_eq(32, f_4)   // ceil(160/5) = 32
  @azimuth.assert_eq(99, f_5)   // ceil(493/5) = 99
  
  // 计算平均温度
  let total_fahrenheit = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f_1, f_2), f_3), f_4), f_5)
  let average_fahrenheit = @azimuth.divide_with_ceil(total_fahrenheit, 5)
  @azimuth.assert_eq(107, average_fahrenheit)  // ceil(534/5) = 107
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  // 场景：计算数据压缩率和存储需求
  let original_data_size = 2500  // KB
  let compression_ratio_1 = 4    // 4:1 compression
  let compression_ratio_2 = 3    // 3:1 compression
  let compression_ratio_3 = 5    // 5:1 compression
  
  // 计算不同压缩比下的压缩后大小
  let compressed_size_1 = @azimuth.divide_with_ceil(original_data_size, compression_ratio_1)
  let compressed_size_2 = @azimuth.divide_with_ceil(original_data_size, compression_ratio_2)
  let compressed_size_3 = @azimuth.divide_with_ceil(original_data_size, compression_ratio_3)
  
  @azimuth.assert_eq(625, compressed_size_1)   // ceil(2500/4) = 625
  @azimuth.assert_eq(834, compressed_size_2)   // ceil(2500/3) = 834
  @azimuth.assert_eq(500, compressed_size_3)   // ceil(2500/5) = 500
  
  // 计算节省的存储空间
  let savings_1 = @azimuth.add(original_data_size, -compressed_size_1)
  let savings_2 = @azimuth.add(original_data_size, -compressed_size_2)
  let savings_3 = @azimuth.add(original_data_size, -compressed_size_3)
  
  @azimuth.assert_eq(1875, savings_1)  // 2500 - 625 = 1875
  @azimuth.assert_eq(1666, savings_2)  // 2500 - 834 = 1666
  @azimuth.assert_eq(2000, savings_3)  // 2500 - 500 = 2000
  
  // 计算压缩率百分比
  let compression_percent_1 = @azimuth.divide_with_ceil(@azimuth.multiply(savings_1, 100), original_data_size)
  let compression_percent_2 = @azimuth.divide_with_ceil(@azimuth.multiply(savings_2, 100), original_data_size)
  let compression_percent_3 = @azimuth.divide_with_ceil(@azimuth.multiply(savings_3, 100), original_data_size)
  
  @azimuth.assert_eq(75, compression_percent_1)   // ceil(187500/2500) = 75
  @azimuth.assert_eq(67, compression_percent_2)   // ceil(166600/2500) = 67
  @azimuth.assert_eq(80, compression_percent_3)   // ceil(200000/2500) = 80
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  // 场景：计算网络传输需求和时间
  let file_size_mb = 850  // MB
  let network_speed_mbps_1 = 10   // 10 Mbps
  let network_speed_mbps_2 = 25   // 25 Mbps
  let network_speed_mbps_3 = 100  // 100 Mbps
  
  // 计算不同网络速度下的传输时间（秒）
  let transfer_time_1 = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps_1)
  let transfer_time_2 = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps_2)
  let transfer_time_3 = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps_3)
  
  @azimuth.assert_eq(85, transfer_time_1)    // ceil(850/10) = 85 seconds
  @azimuth.assert_eq(34, transfer_time_2)    // ceil(850/25) = 34 seconds
  @azimuth.assert_eq(9, transfer_time_3)     // ceil(850/100) = 9 seconds
  
  // 转换为分钟（使用向上取整）
  let minutes_1 = @azimuth.divide_with_ceil(transfer_time_1, 60)
  let minutes_2 = @azimuth.divide_with_ceil(transfer_time_2, 60)
  let minutes_3 = @azimuth.divide_with_ceil(transfer_time_3, 60)
  
  @azimuth.assert_eq(2, minutes_1)  // ceil(85/60) = 2 minutes
  @azimuth.assert_eq(1, minutes_2)  // ceil(34/60) = 1 minute
  @azimuth.assert_eq(1, minutes_3)  // ceil(9/60) = 1 minute
  
  // 计算在1小时内可以传输多少个这样的文件
  let files_per_hour_1 = @azimuth.divide_with_ceil(3600, transfer_time_1)
  let files_per_hour_2 = @azimuth.divide_with_ceil(3600, transfer_time_2)
  let files_per_hour_3 = @azimuth.divide_with_ceil(3600, transfer_time_3)
  
  @azimuth.assert_eq(43, files_per_hour_1)  // ceil(3600/85) = 43
  @azimuth.assert_eq(106, files_per_hour_2) // ceil(3600/34) = 106
  @azimuth.assert_eq(400, files_per_hour_3) // ceil(3600/9) = 400
}

test "manufacturing_production_optimization" {
  // 制造业生产优化测试
  // 场景：计算生产线的最优配置
  let daily_demand = 1250
  let machine_capacity_per_hour = 85
  let working_hours_per_shift = 8
  let shifts_per_day = 2
  
  // 计算每班次的总产能
  let shift_capacity = @azimuth.multiply(machine_capacity_per_hour, working_hours_per_shift)
  @azimuth.assert_eq(680, shift_capacity)  // 85 * 8 = 680
  
  // 计算每日总产能
  let daily_capacity = @azimuth.multiply(shift_capacity, shifts_per_day)
  @azimuth.assert_eq(1360, daily_capacity)  // 680 * 2 = 1360
  
  // 验证产能是否满足需求
  @azimuth.assert_eq(1, if (daily_capacity >= daily_demand) { 1 } else { 0 })
  
  // 计算剩余产能
  let surplus_capacity = @azimuth.add(daily_capacity, -daily_demand)
  @azimuth.assert_eq(110, surplus_capacity)  // 1360 - 1250 = 110
  
  // 计算产能利用率
  let utilization_rate = @azimuth.divide_with_ceil(@azimuth.multiply(daily_demand, 100), daily_capacity)
  @azimuth.assert_eq(92, utilization_rate)  // ceil(125000/1360) = 92
  
  // 如果只有一个班次，需要的额外机器数量
  let single_shift_capacity = shift_capacity
  let additional_capacity_needed = @azimuth.add(daily_demand, -single_shift_capacity)
  let additional_machines = @azimuth.divide_with_ceil(additional_capacity_needed, working_hours_per_shift)
  
  @azimuth.assert_eq(570, additional_capacity_needed)  // 1250 - 680 = 570
  @azimuth.assert_eq(72, additional_machines)          // ceil(570/8) = 72
}

test "educational_classroom_scheduling" {
  // 教育课堂调度测试
  // 场景：学校教室和课程的最优安排
  let total_students = 245
  let classroom_capacity = 30
  let subjects_per_student = 6
  let periods_per_day = 8
  let days_per_week = 5
  
  // 计算需要的教室数量
  let classrooms_needed = @azimuth.divide_with_ceil(total_students, classroom_capacity)
  @azimuth.assert_eq(9, classrooms_needed)  // ceil(245/30) = 9
  
  // 计算实际座位容量
  let total_seats = @azimuth.multiply(classrooms_needed, classroom_capacity)
  @azimuth.assert_eq(270, total_seats)
  
  // 计算剩余座位
  let available_seats = @azimuth.add(total_seats, -total_students)
  @azimuth.assert_eq(25, available_seats)
  
  // 计算每周总课时数
  let weekly_periods_per_student = @azimuth.multiply(subjects_per_student, days_per_week)
  @azimuth.assert_eq(30, weekly_periods_per_student)  // 6 * 5 = 30
  
  // 计算全校每周总课时数
  let total_weekly_periods = @azimuth.multiply(weekly_periods_per_student, total_students)
  @azimuth.assert_eq(7350, total_weekly_periods)  // 30 * 245 = 7350
  
  // 计算每天需要的课时数
  let daily_periods_needed = @azimuth.divide_with_ceil(total_weekly_periods, days_per_week)
  @azimuth.assert_eq(1470, daily_periods_needed)  // ceil(7350/5) = 1470
  
  // 计算每间教室每天需要的课时数
  let periods_per_classroom_per_day = @azimuth.divide_with_ceil(daily_periods_needed, classrooms_needed)
  @azimuth.assert_eq(164, periods_per_classroom_per_day)  // ceil(1470/9) = 164
  
  // 验证每间教室的课时数不超过可用时段
  @azimuth.assert_eq(1, if (periods_per_classroom_per_day > periods_per_day) { 1 } else { 0 })  // 需要更多时间段或教室
}