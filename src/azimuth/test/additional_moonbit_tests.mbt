// Azimuth 项目额外的 MoonBit 测试用例
// 测试边界条件、性能和实际应用场景

// 测试1: 二进制运算模拟
test "binary_operations_simulation" {
  // 模拟位运算：使用加减乘除实现位运算效果
  let a = 16
  let b = 5
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(32, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  @azimuth.assert_eq(8, right_shift)
  
  // 模拟按位与运算（简化版）
  let bitwise_and_sim = @azimuth.multiply(a, b) / b
  @azimuth.assert_eq(16, bitwise_and_sim)
}

// 测试2: 货币转换计算
test "currency_exchange_calculation" {
  // 实际货币转换场景
  let usd_amount = 250
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  let transaction_fee = 15  // 固定手续费
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(1750, cny_amount)
  
  // 添加手续费
  let total_with_fee = @azimuth.add(cny_amount, transaction_fee)
  @azimuth.assert_eq(1765, total_with_fee)
  
  // 计算折扣（5%）
  let discount = @azimuth.multiply(total_with_fee, 5) / 100
  let final_amount = @azimuth.add(total_with_fee, -discount)
  @azimuth.assert_eq(1676, final_amount)
}

// 测试3: 几何体积计算
test "geometry_volume_calculations" {
  // 计算3D几何体的体积
  let cube_side = 4
  
  // 立方体体积：side³
  let cube_volume = @azimuth.multiply(
    @azimuth.multiply(cube_side, cube_side), 
    cube_side
  )
  @azimuth.assert_eq(64, cube_volume)
  
  // 长方体体积
  let length = 6
  let width = 4
  let height = 3
  let rectangular_volume = @azimuth.multiply(
    @azimuth.multiply(length, width), 
    height
  )
  @azimuth.assert_eq(72, rectangular_volume)
  
  // 圆柱体体积（简化计算）：π × r² × h ≈ 3 × r² × h
  let radius = 5
  let cylinder_height = 10
  let cylinder_volume = @azimuth.multiply(
    @azimuth.multiply(3, @azimuth.multiply(radius, radius)),
    cylinder_height
  )
  @azimuth.assert_eq(750, cylinder_volume)
}

// 测试4: 数列计算
test "mathematical_series_calculations" {
  // 计算等差数列和等比数列
  
  // 等差数列：2, 5, 8, 11, 14（公差为3）
  let ap_first = 2
  let ap_diff = 3
  let ap_n = 5
  
  // 使用公式：n × (2×a + (n-1)×d) / 2
  let ap_sum = @azimuth.multiply(
    ap_n, 
    @azimuth.add(
      @azimuth.multiply(2, ap_first),
      @azimuth.multiply(ap_n - 1, ap_diff)
    )
  ) / 2
  @azimuth.assert_eq(40, ap_sum)
  
  // 等比数列：3, 6, 12, 24, 48（公比为2）
  let gp_first = 3
  let gp_ratio = 2
  let gp_n = 5
  
  // 手动计算前5项
  let gp_term1 = gp_first
  let gp_term2 = @azimuth.multiply(gp_term1, gp_ratio)
  let gp_term3 = @azimuth.multiply(gp_term2, gp_ratio)
  let gp_term4 = @azimuth.multiply(gp_term3, gp_ratio)
  let gp_term5 = @azimuth.multiply(gp_term4, gp_ratio)
  
  let gp_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    gp_term1, gp_term2), gp_term3), gp_term4), gp_term5)
  @azimuth.assert_eq(93, gp_sum)
}

// 测试5: 数据压缩模拟
test "data_compression_simulation" {
  // 模拟简单的数据压缩算法
  
  let original_size = 1000
  let compression_ratio = 70  // 压缩到70%
  
  // 计算压缩后大小
  let compressed_size = @azimuth.multiply(original_size, compression_ratio) / 100
  @azimuth.assert_eq(700, compressed_size)
  
  // 计算节省的空间
  let space_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(300, space_saved)
  
  // 计算压缩效率（节省百分比）
  let compression_efficiency = @azimuth.multiply(space_saved, 100) / original_size
  @azimuth.assert_eq(30, compression_efficiency)
}

// 测试6: 网络带宽计算
test "network_bandwidth_calculations" {
  // 网络带宽和下载时间计算
  
  let file_size_mb = 850
  let bandwidth_mbps = 50  // 50 Mbps
  
  // 计算下载时间（秒）：文件大小 / 带宽
  let download_time_seconds = file_size_mb / bandwidth_mbps
  @azimuth.assert_eq(17, download_time_seconds)
  
  // 转换为分钟
  let download_time_minutes = download_time_seconds / 60
  @azimuth.assert_eq(0, download_time_minutes)  // 整数除法
  
  // 计算剩余秒数
  let remaining_seconds = download_time_seconds % 60
  @azimuth.assert_eq(17, remaining_seconds)
  
  // 考虑网络开销（增加20%时间）
  let overhead_percentage = 20
  let overhead_time = @azimuth.multiply(download_time_seconds, overhead_percentage) / 100
  let total_time = @azimuth.add(download_time_seconds, overhead_time)
  @azimuth.assert_eq(20, total_time)
}

// 测试7: 字符串处理性能
test "string_processing_performance" {
  // 测试字符串处理的性能相关计算
  
  let base_string_length = 10
  let repeat_count = 100
  
  // 计算重复拼接后的总长度
  let total_length = @azimuth.multiply(base_string_length, repeat_count)
  @azimuth.assert_eq(1000, total_length)
  
  // 计算内存使用估算（每个字符2字节）
  let memory_usage_bytes = @azimuth.multiply(total_length, 2)
  @azimuth.assert_eq(2000, memory_usage_bytes)
  
  // 转换为KB
  let memory_usage_kb = memory_usage_bytes / 1024
  @azimuth.assert_eq(1, memory_usage_kb)
  
  // 测试greet函数的处理
  let test_name = "PerformanceTest"
  let greet_result = @azimuth.greet(test_name)
  @azimuth.assert_eq_string("Hello, PerformanceTest!", greet_result)
}

// 测试8: 物理运动计算
test "physics_motion_calculations" {
  // 简单的物理运动计算
  
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²
  let displacement_part1 = @azimuth.multiply(initial_velocity, time)  // ut = 50
  let displacement_part2 = @azimuth.multiply(
    @azimuth.multiply(acceleration, time), 
    time
  ) / 2  // 0.5at² = 25
  let total_displacement = @azimuth.add(displacement_part1, displacement_part2)
  @azimuth.assert_eq(75, total_displacement)
  
  // 计算平均速度
  let average_velocity = total_displacement / time
  @azimuth.assert_eq(15, average_velocity)
}

// 测试9: 电池续航计算
test "battery_life_calculations" {
  // 电池续航时间计算
  
  let battery_capacity_mah = 4000  // mAh
  let device_consumption_ma = 200  // mA
  
  // 计算理论续航时间（小时）
  let battery_life_hours = battery_capacity_mah / device_consumption_ma
  @azimuth.assert_eq(20, battery_life_hours)
  
  // 转换为分钟
  let battery_life_minutes = @azimuth.multiply(battery_life_hours, 60)
  @azimuth.assert_eq(1200, battery_life_minutes)
  
  // 考虑实际使用效率（80%）
  let efficiency_percentage = 80
  let actual_life_hours = @azimuth.multiply(battery_life_hours, efficiency_percentage) / 100
  @azimuth.assert_eq(16, actual_life_hours)
  
  // 计算可使用次数（每次使用2小时）
  let usage_per_session_hours = 2
  let sessions_count = actual_life_hours / usage_per_session_hours
  @azimuth.assert_eq(8, sessions_count)
}

// 测试10: 复杂表达式计算
test "complex_expression_evaluation" {
  // 测试复杂数学表达式的计算
  
  let x = 3
  let y = 4
  let z = 2
  
  // 计算：(x + y) × (x - y) + z²
  let sum_xy = @azimuth.add(x, y)  // 7
  let diff_xy = @azimuth.add(x, -y)  // -1
  let product_xy = @azimuth.multiply(sum_xy, diff_xy)  // -7
  let z_squared = @azimuth.multiply(z, z)  // 4
  let result1 = @azimuth.add(product_xy, z_squared)  // -3
  @azimuth.assert_eq(-3, result1)
  
  // 计算：x² + y² + 2xy (即 (x + y)²)
  let x_squared = @azimuth.multiply(x, x)  // 9
  let y_squared = @azimuth.multiply(y, y)  // 16
  let two_xy = @azimuth.multiply(@azimuth.multiply(2, x), y)  // 24
  let result2 = @azimuth.add(@azimuth.add(x_squared, y_squared), two_xy)  // 49
  @azimuth.assert_eq(49, result2)
  
  // 验证 (x + y)² = x² + y² + 2xy
  let x_plus_y = @azimuth.add(x, y)  // 7
  let square_of_sum = @azimuth.multiply(x_plus_y, x_plus_y)  // 49
  @azimuth.assert_eq(result2, square_of_sum)
}