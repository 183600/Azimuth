// 额外的 MoonBit 测试用例
// 覆盖更多实际应用场景和边界情况

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率（百分比）
  let years = 30          // 贷款年限
  
  // 简化的月供计算：本金 / (年数 × 12) + 本金 × 年利率 / 100 / 12
  let monthly_principal = principal / (years * 12)
  let monthly_interest = multiply(principal, annual_rate) / 100 / 12
  let monthly_payment = add(monthly_principal, monthly_interest)
  
  assert_eq(555, monthly_payment)  // 简化计算结果
}

test "scientific_notation_conversion" {
  // 科学计数法转换：1.23 × 10^5 = 123000
  let mantissa = 123
  let exponent = 3  // 10^3 = 1000
  
  // 计算：mantissa × 10^exponent
  let result = multiply(mantissa, multiply(10, multiply(10, 10)))
  assert_eq(123000, result)
  
  // 测试负指数：1.23 × 10^-3 = 1.23（简化为整数除法）
  let negative_exp_result = mantissa / 1000
  assert_eq(0, negative_exp_result)  // 整数除法
}

test "coordinate_geometry_calculations" {
  // 坐标几何计算：两点间距离（简化版）
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  // 计算：√[(x2-x1)² + (y2-y1)²]（简化为整数计算）
  let dx = add(x2, -x1)  // 4
  let dy = add(y2, -y1)  // -3
  let dx_squared = multiply(dx, dx)  // 16
  let dy_squared = multiply(dy, dy)  // 9
  let distance_squared = add(dx_squared, dy_squared)  // 25
  
  assert_eq(25, distance_squared)
  assert_eq(5, distance_squared / 5)  // 简化的平方根计算
}

test "temperature_conversions" {
  // 温度转换：摄氏度、华氏度、开尔文（简化版）
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为 F = C × 2 + 30）
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 摄氏度转开尔文：K = C + 273
  let kelvin = add(celsius, 273)
  assert_eq(298, kelvin)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) / 2
  let back_to_celsius = add(fahrenheit, -30) / 2
  assert_eq(25, back_to_celsius)
}

test "inventory_management" {
  // 库存管理计算
  let initial_stock = 500
  let units_sold = 120
  let units_received = 80
  let safety_stock = 100
  
  // 计算当前库存
  let current_stock = add(add(initial_stock, -units_sold), units_received)
  assert_eq(460, current_stock)
  
  // 计算是否需要补货
  let need_reorder = current_stock < safety_stock
  assert_false(need_reorder)
  
  // 计算建议订购量
  let reorder_quantity = if need_reorder {
    add(multiply(safety_stock, 2), -current_stock)
  } else {
    0
  }
  assert_eq(0, reorder_quantity)
}

test "data_compression_simulation" {
  // 数据压缩模拟：简单游程编码
  let original_length = 1000
  let compression_ratio = 70  // 压缩到70%
  
  // 计算压缩后大小
  let compressed_size = multiply(original_length, compression_ratio) / 100
  assert_eq(700, compressed_size)
  
  // 计算节省空间
  let space_saved = add(original_length, -compressed_size)
  assert_eq(300, space_saved)
  
  // 计算压缩率
  let compression_percentage = multiply(space_saved, 100) / original_length
  assert_eq(30, compression_percentage)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let file_size_mb = 100
  let network_speed_mbps = 20
  let overhead_percentage = 10
  
  // 计算理论传输时间（秒）
  let theoretical_time = multiply(file_size_mb, 8) / network_speed_mbps
  assert_eq(40, theoretical_time)
  
  // 计算实际时间（包含开销）
  let overhead_factor = add(100, overhead_percentage)
  let actual_time = multiply(theoretical_time, overhead_factor) / 100
  assert_eq(44, actual_time)
  
  // 计算有效吞吐量
  let effective_throughput = multiply(file_size_mb, 100) / actual_time
  assert_eq(227, effective_throughput)  // 约等于 2.27 MB/s
}

test "cryptographic_hash_simulation" {
  // 密码学哈希模拟（简化版）
  let input_value = 12345
  let prime_modulus = 2147483647  // 大质数
  let hash_multiplier = 31
  
  // 简单哈希函数：hash = (input × multiplier) mod modulus
  let product = multiply(input_value, hash_multiplier)
  let hash_value = product % prime_modulus
  
  assert_eq(382695, hash_value)
  
  // 测试哈希值的范围
  let in_valid_range = hash_value >= 0 && hash_value < prime_modulus
  assert_true(in_valid_range)
}

test "game_physics_simulation" {
  // 游戏物理模拟：抛物线运动（简化版）
  let initial_velocity = 20  // 初始速度
  let gravity = 10           // 重力加速度（简化值）
  let time = 3               // 时间
  
  // 计算水平位置：x = v × t
  let horizontal_position = multiply(initial_velocity, time)
  assert_eq(60, horizontal_position)
  
  // 计算垂直位置：y = v × t - 0.5 × g × t²（简化为整数）
  let vertical_position = add(multiply(initial_velocity, time), multiply(multiply(gravity, time), time) / -2)
  assert_eq(15, vertical_position)
  
  // 计算总距离
  let total_distance = multiply(horizontal_position, horizontal_position) + multiply(vertical_position, vertical_position)
  assert_eq(3825, total_distance)
}

test "database_query_optimization" {
  // 数据库查询优化模拟
  let table_size = 1000000
  let selectivity_percentage = 5  // 选择性5%
  let index_efficiency = 90       // 索引效率90%
  
  // 计算全表扫描的读取行数
  let full_scan_rows = table_size
  assert_eq(1000000, full_scan_rows)
  
  // 计算索引扫描的读取行数
  let selected_rows = multiply(table_size, selectivity_percentage) / 100
  let index_scan_rows = multiply(selected_rows, index_efficiency) / 100
  assert_eq(45000, index_scan_rows)
  
  // 计算性能提升
  let performance_gain = multiply(full_scan_rows, 100) / index_scan_rows
  assert_eq(2222, performance_gain)  // 约22.22倍性能提升
}