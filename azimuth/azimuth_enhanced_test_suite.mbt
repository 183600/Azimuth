// Azimuth 增强测试套件
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "number_system_conversions" {
  // 数字系统转换测试
  // 测试不同进制和表示法的运算
  
  // 二进制运算模拟
  let binary_5 = @azimuth.add(2, 3)  // 10 + 11 = 101 (5)
  let binary_10 = @azimuth.multiply(binary_5, 2)  // 101 * 10 = 1010 (10)
  @azimuth.assert_eq(10, binary_10)
  
  // 十六进制运算模拟
  let hex_ff = @azimuth.add(255, 0)  // FF + 00 = FF
  let hex_1fe = @azimuth.multiply(hex_ff, 2)  // FF * 2 = 1FE
  @azimuth.assert_eq(510, hex_1fe)
}

test "time_calculation_scenarios" {
  // 时间计算场景测试
  // 场景：项目时间管理
  
  let work_hours_per_day = 8
  let work_days_per_week = 5
  let total_tasks = 120
  let tasks_per_hour = 2
  
  // 计算每周工作小时数
  let weekly_hours = @azimuth.multiply(work_hours_per_day, work_days_per_week)
  @azimuth.assert_eq(40, weekly_hours)
  
  // 计算每周处理任务数
  let weekly_tasks = @azimuth.multiply(weekly_hours, tasks_per_hour)
  @azimuth.assert_eq(80, weekly_tasks)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(total_tasks, weekly_tasks)
  @azimuth.assert_eq(2, weeks_needed)
  
  // 计算总工作小时数
  let total_hours = @azimuth.multiply(total_tasks, @azimuth.divide_with_ceil(1, tasks_per_hour))
  @azimuth.assert_eq(60, total_hours)
}

test "memory_allocation_simulation" {
  // 内存分配模拟测试
  // 场景：计算机内存块分配
  
  let total_memory = 1024  // KB
  let block_size = 64      // KB
  let allocated_blocks = 7
  let requested_memory = 512
  
  // 计算已分配内存
  let allocated_memory = @azimuth.multiply(allocated_blocks, block_size)
  @azimuth.assert_eq(448, allocated_memory)
  
  // 计算剩余内存
  let remaining_memory = @azimuth.subtract(total_memory, allocated_memory)
  @azimuth.assert_eq(576, remaining_memory)
  
  // 计算请求内存需要的块数
  let blocks_needed = @azimuth.divide_with_ceil(requested_memory, block_size)
  @azimuth.assert_eq(8, blocks_needed)
}

test "coordinate_geometry_calculations" {
  // 坐标几何计算测试
  // 场景：二维坐标系中的距离和面积计算
  
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  // 计算x轴距离
  let x_distance = @azimuth.subtract(x2, x1)
  @azimuth.assert_eq(4, x_distance)
  
  // 计算y轴距离
  let y_distance = @azimuth.subtract(y2, y1)
  @azimuth.assert_eq(-3, y_distance)
  
  // 计算曼哈顿距离（简化版，不使用abs）
  let manhattan_distance = @azimuth.add(x_distance, y_distance)
  @azimuth.assert_eq(1, manhattan_distance)  // 4 + (-3) = 1
  
  // 计算矩形面积（简化版，假设正值）
  let rectangle_area = @azimuth.multiply(x_distance, 3)  // 使用3而不是abs(y_distance)
  @azimuth.assert_eq(12, rectangle_area)
}

test "statistical_analysis_basic" {
  // 基础统计分析测试
  // 场景：数据集的基本统计计算
  
  let data_points = 15
  let sum_values = 240
  let min_value = 8
  let max_value = 32
  
  // 计算平均值
  let mean_value = @azimuth.divide_with_ceil(sum_values, data_points)
  @azimuth.assert_eq(16, mean_value)
  
  // 计算范围
  let range = @azimuth.subtract(max_value, min_value)
  @azimuth.assert_eq(24, range)
  
  // 计算中位数近似值（使用范围的中点）
  let median_approx = @azimuth.add(min_value, @azimuth.divide_with_ceil(range, 2))
  @azimuth.assert_eq(20, median_approx)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：生产资源分配优化
  
  let total_budget = 50000
  let product_a_cost = 120
  let product_b_cost = 180
  let min_products_a = 150
  let min_products_b = 100
  
  // 计算产品A的最低成本
  let cost_a_min = @azimuth.multiply(min_products_a, product_a_cost)
  @azimuth.assert_eq(18000, cost_a_min)
  
  // 计算产品B的最低成本
  let cost_b_min = @azimuth.multiply(min_products_b, product_b_cost)
  @azimuth.assert_eq(18000, cost_b_min)
  
  // 计算总最低成本
  let total_min_cost = @azimuth.add(cost_a_min, cost_b_min)
  @azimuth.assert_eq(36000, total_min_cost)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_min_cost)
  @azimuth.assert_eq(14000, remaining_budget)
  
  // 计算可以额外生产的产品A数量
  let extra_products_a = @azimuth.divide_with_ceil(remaining_budget, product_a_cost)
  @azimuth.assert_eq(117, extra_products_a)
}

test "temperature_conversion_logic" {
  // 温度转换逻辑测试
  // 场景：不同温度单位转换计算
  
  let celsius_freezing = 0
  let celsius_boiling = 100
  let fahrenheit_freezing = 32
  let fahrenheit_boiling = 212
  
  // 摄氏度到华氏度转换：F = C * 9/5 + 32
  let c_to_f_multiplier_numerator = 9
  let c_to_f_multiplier_denominator = 5
  let c_to_f_offset = 32
  
  // 转换冰点
  let freezing_f_calc = @azimuth.add(@azimuth.multiply(celsius_freezing, c_to_f_multiplier_numerator), @azimuth.multiply(c_to_f_offset, c_to_f_multiplier_denominator))
  let freezing_f_actual = @azimuth.divide_with_ceil(freezing_f_calc, c_to_f_multiplier_denominator)
  @azimuth.assert_eq(32, freezing_f_actual)
  
  // 转换沸点
  let boiling_f_calc = @azimuth.add(@azimuth.multiply(celsius_boiling, c_to_f_multiplier_numerator), @azimuth.multiply(c_to_f_offset, c_to_f_multiplier_denominator))
  let boiling_f_actual = @azimuth.divide_with_ceil(boiling_f_calc, c_to_f_multiplier_denominator)
  @azimuth.assert_eq(212, boiling_f_actual)
  
  // 验证转换范围
  let f_range = @azimuth.subtract(fahrenheit_boiling, fahrenheit_freezing)
  let c_range = @azimuth.subtract(celsius_boiling, celsius_freezing)
  let range_ratio = @azimuth.divide_with_ceil(f_range, c_range)
  @azimuth.assert_eq(2, range_ratio)  // 180/100 = 1.8 ≈ 2
}

test "network_data_transmission" {
  // 网络数据传输测试
  // 场景：数据包传输计算
  
  let file_size_mb = 15
  let packet_size_kb = 4
  let mb_to_kb = 1024
  let transmission_rate_kbps = 128
  let overhead_percent = 10
  
  // 转换文件大小为KB
  let file_size_kb = @azimuth.multiply(file_size_mb, mb_to_kb)
  @azimuth.assert_eq(15360, file_size_kb)
  
  // 计算需要的包数
  let packets_needed = @azimuth.divide_with_ceil(file_size_kb, packet_size_kb)
  @azimuth.assert_eq(3840, packets_needed)
  
  // 计算原始传输时间（秒）
  let base_time_seconds = @azimuth.divide_with_ceil(file_size_kb, transmission_rate_kbps)
  @azimuth.assert_eq(120, base_time_seconds)
  
  // 计算开销时间
  let overhead_time = @azimuth.divide_with_ceil(@azimuth.multiply(base_time_seconds, overhead_percent), 100)
  @azimuth.assert_eq(12, overhead_time)
  
  // 计算总传输时间
  let total_time = @azimuth.add(base_time_seconds, overhead_time)
  @azimuth.assert_eq(132, total_time)
  
  // 验证传输效率
  let efficiency = @azimuth.divide_with_ceil(@azimuth.multiply(base_time_seconds, 100), total_time)
  @azimuth.assert_eq(91, efficiency)  // 120/132 ≈ 91%
}