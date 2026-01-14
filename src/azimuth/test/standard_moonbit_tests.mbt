// Azimuth 项目新增 MoonBit 标准测试用例
// 使用标准的 MoonBit 测试语法，覆盖各种实际应用场景

test "binary_number_operations" {
  // 二进制数运算模拟
  // 模拟二进制左移：n << 1 = n * 2
  @azimuth.assert_eq(16, @azimuth.multiply(8, 2))
  @azimuth.assert_eq(20, @azimuth.multiply(10, 2))
  
  // 模拟二进制右移：n >> 1 = n / 2
  @azimuth.assert_eq(4, 8 / 2)
  @azimuth.assert_eq(5, 10 / 2)
  
  // 模拟位与运算（简化版）
  @azimuth.assert_eq(0, @azimuth.multiply(2, 0))
  @azimuth.assert_eq(4, @azimuth.multiply(4, 1))
}

test "loan_calculation_scenario" {
  // 贷款计算场景
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 10          // 贷款年限
  
  // 简化的利息计算：总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  @azimuth.assert_eq(6000000, total_interest)
  
  // 总还款额 = 本金 + 利息
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(6100000, total_payment)
  
  // 月还款额（简化版）
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(50833, monthly_payment)
}

test "data_validation_scenarios" {
  // 数据验证场景
  let min_age = 18
  let max_age = 65
  let user_age = 25
  
  // 年龄验证
  @azimuth.assert_true(@azimuth.add(user_age, -min_age) >= 0)
  @azimuth.assert_true(@azimuth.add(max_age, -user_age) >= 0)
  
  // 边界值测试
  @azimuth.assert_true(@azimuth.add(min_age, -min_age) == 0)
  @azimuth.assert_true(@azimuth.add(max_age, -max_age) == 0)
  
  // 密码长度验证（至少8位）
  let password_length = 12
  let min_length = 8
  @azimuth.assert_true(password_length >= min_length)
}

test "inventory_management" {
  // 库存管理场景
  let initial_stock = 100
  let sales = 30
  let restock = 50
  let safety_stock = 20
  
  // 计算当前库存
  let current_stock = @azimuth.add(initial_stock, -sales)
  @azimuth.assert_eq(70, current_stock)
  
  // 补货后库存
  let updated_stock = @azimuth.add(current_stock, restock)
  @azimuth.assert_eq(120, updated_stock)
  
  // 检查是否需要补货
  let needs_restock = current_stock < safety_stock
  @azimuth.assert_false(needs_restock)
  
  // 模拟销量增加后的情况
  let increased_sales = 90
  let new_stock = @azimuth.add(initial_stock, -increased_sales)
  let urgent_restock = new_stock < safety_stock
  @azimuth.assert_true(urgent_restock)
}

test "coordinate_geometry" {
  // 坐标几何计算
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  // 计算两点间距离的平方（避免平方根）
  let dx = @azimuth.add(x2, -x1)  // 4
  let dy = @azimuth.add(y2, -y1)  // -3
  let distance_squared = @azimuth.add(@azimuth.multiply(dx, dx), @azimuth.multiply(dy, dy))
  @azimuth.assert_eq(25, distance_squared)  // 4² + (-3)² = 16 + 9 = 25
  
  // 计算中点坐标
  let midpoint_x = @azimuth.add(x1, x2) / 2
  let midpoint_y = @azimuth.add(y1, y2) / 2
  @azimuth.assert_eq(5, midpoint_x)
  @azimuth.assert_eq(2, midpoint_y)
}

test "signal_processing" {
  // 信号处理基础计算
  let sample_rate = 44100  // 采样率
  let duration = 3         // 持续时间（秒）
  let frequency = 440      // 频率
  
  // 计算总样本数
  let total_samples = @azimuth.multiply(sample_rate, duration)
  @azimuth.assert_eq(132300, total_samples)
  
  // 计算周期数
  let periods = @azimuth.multiply(frequency, duration)
  @azimuth.assert_eq(1320, periods)
  
  // 计算每周期样本数
  let samples_per_period = total_samples / periods
  @azimuth.assert_eq(100, samples_per_period)
}

test "network_throughput" {
  // 网络吞吐量计算
  let file_size = 1048576  // 1MB in bytes
  let bandwidth = 1048576  // 1MB/s
  let latency = 50         // 50ms延迟
  
  // 计算传输时间（简化，不考虑延迟）
  let transfer_time = file_size / bandwidth
  @azimuth.assert_eq(1, transfer_time)
  
  // 计算考虑延迟的总时间
  let total_time = @azimuth.add(transfer_time, latency / 1000)
  @azimuth.assert_eq(1, total_time)  // 整数除法
  
  // 计算实际吞吐量
  let actual_throughput = file_size / total_time
  @azimuth.assert_eq(1048576, actual_throughput)
}

test "compression_ratio" {
  // 压缩比计算
  let original_size = 1000000
  let compressed_size = 250000
  
  // 计算压缩比
  let compression_ratio = original_size / compressed_size
  @azimuth.assert_eq(4, compression_ratio)
  
  // 计算空间节省百分比
  let space_saved = @azimuth.add(original_size, -compressed_size)
  let saved_percentage = @azimuth.multiply(space_saved, 100) / original_size
  @azimuth.assert_eq(75, saved_percentage)
  
  // 测试不同压缩级别
  let level2_compressed = 400000
  let level2_ratio = original_size / level2_compressed
  @azimuth.assert_eq(2, level2_ratio)
}

test "cpu_utilization" {
  // CPU利用率计算
  let total_time = 1000    // 总时间单位
  let idle_time = 300      // 空闲时间
  let user_time = 500      // 用户时间
  let system_time = 200    // 系统时间
  
  // 计算CPU利用率
  let busy_time = @azimuth.add(user_time, system_time)
  let utilization = @azimuth.multiply(busy_time, 100) / total_time
  @azimuth.assert_eq(70, utilization)
  
  // 计算空闲百分比
  let idle_percentage = @azimuth.multiply(idle_time, 100) / total_time
  @azimuth.assert_eq(30, idle_percentage)
  
  // 验证总和为100%
  @azimuth.assert_eq(100, @azimuth.add(utilization, idle_percentage))
}

test "memory_allocation" {
  // 内存分配计算
  let block_size = 1024    // 块大小
  let num_blocks = 100     // 块数量
  let overhead = 512       // 开销
  
  // 计算总内存需求
  let data_memory = @azimuth.multiply(block_size, num_blocks)
  let total_memory = @azimuth.add(data_memory, overhead)
  @azimuth.assert_eq(102912, total_memory)
  
  // 计算内存效率
  let efficiency = @azimuth.multiply(data_memory, 100) / total_memory
  @azimuth.assert_eq(99, efficiency)  // 102400/102912 ≈ 99.5%
  
  // 计算碎片化率
  let fragmentation = @azimuth.add(100, -efficiency)
  @azimuth.assert_eq(1, fragmentation)
}