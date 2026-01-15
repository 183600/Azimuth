// Azimuth 项目新增 MoonBit 测试用例
// 包含各种实际应用场景和边界情况测试

test "physics_calculation_velocity" {
  // 物理计算：速度和距离
  // 距离 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 计算初速度行驶的距离
  let distance_initial = @azimuth.multiply(initial_velocity, time)  // 10 × 5 = 50
  
  // 计算加速度增加的距离（简化：0.5 × a × t²）
  let time_squared = @azimuth.multiply(time, time)  // 25
  let distance_acceleration = @azimuth.multiply(@azimuth.multiply(2, acceleration), time_squared) / 10  // 简化的 0.5 × a × t²
  
  // 总距离
  let total_distance = @azimuth.add(distance_initial, distance_acceleration)
  @azimuth.assert_eq(100, total_distance)
}

test "array_manipulation_index_calculation" {
  // 数组操作：索引计算
  // 二维数组转一维数组的索引计算
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算一维数组中的索引：index = row × cols + col
  let linear_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, linear_index)
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  @azimuth.assert_eq(0, first_index)
  
  let last_index = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), @azimuth.add(cols, -1)), @azimuth.add(cols, -1))
  @azimuth.assert_eq(19, last_index)
}

test "business_profit_calculation" {
  // 商业利润计算
  let revenue = 50000      // 收入
  let cost_per_unit = 30   // 单位成本
  let units_sold = 1000    // 销售数量
  let fixed_costs = 10000  // 固定成本
  
  // 计算总成本
  let variable_costs = @azimuth.multiply(cost_per_unit, units_sold)  // 30000
  let total_costs = @azimuth.add(variable_costs, fixed_costs)       // 40000
  
  // 计算利润
  let profit = @azimuth.add(revenue, -total_costs)
  @azimuth.assert_eq(10000, profit)
  
  // 计算利润率（百分比）
  let profit_margin = @azimuth.multiply(profit, 100) / revenue
  @azimuth.assert_eq(20, profit_margin)
}

test "data_compression_simulation" {
  // 数据压缩模拟
  let original_size = 10000  // 原始大小（字节）
  let compression_ratio = 7  // 压缩比（7:1）
  
  // 计算压缩后大小
  let compressed_size = original_size / compression_ratio
  @azimuth.assert_eq(1428, compressed_size)  // 整数除法
  
  // 计算节省的空间
  let space_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(8572, space_saved)
  
  // 计算压缩率（百分比）
  let compression_percentage = @azimuth.multiply(@azimuth.multiply(compressed_size, 100), 1) / original_size
  @azimuth.assert_eq(14, compression_percentage)
}

test "network_bandwidth_calculation" {
  // 网络带宽计算
  let file_size_mb = 100     // 文件大小（MB）
  let bandwidth_mbps = 10    // 带宽（Mbps）
  
  // 计算下载时间（秒）：文件大小 × 8 / 带宽
  let file_size_megabits = @azimuth.multiply(file_size_mb, 8)  // 800 Mb
  let download_time_seconds = file_size_megabits / bandwidth_mbps
  @azimuth.assert_eq(80, download_time_seconds)
  
  // 转换为分钟
  let download_time_minutes = download_time_seconds / 60
  @azimuth.assert_eq(1, download_time_minutes)
  
  // 计算剩余秒数
  let remaining_seconds = download_time_seconds % 60
  @azimuth.assert_eq(20, remaining_seconds)
}

test "cryptography_simple_hash_simulation" {
  // 密码学：简单哈希模拟
  let input_value = 12345
  
  // 简单的哈希函数模拟：(value × prime1 + prime2) % mod
  let prime1 = 31
  let prime2 = 17
  let mod_value = 1000
  
  let step1 = @azimuth.multiply(input_value, prime1)  // 12345 × 31
  let step2 = @azimuth.add(step1, prime2)             // + 17
  let hash_value = step2 % mod_value                  // % 1000
  
  @azimuth.assert_eq(672, hash_value)
  
  // 测试不同输入产生不同哈希
  let input_value2 = 12346
  let step1_2 = @azimuth.multiply(input_value2, prime1)
  let step2_2 = @azimuth.add(step1_2, prime2)
  let hash_value2 = step2_2 % mod_value
  
  @azimuth.assert_eq(703, hash_value2)
  @azimuth.assert_true(hash_value2 != hash_value)
}

test "game_character_attributes" {
  // 游戏角色属性计算
  let base_strength = 15
  let base_agility = 12
  let base_intelligence = 18
  let level_bonus = 5
  let equipment_bonus = 3
  
  // 计算最终属性
  let final_strength = @azimuth.add(@azimuth.add(base_strength, level_bonus), equipment_bonus)
  let final_agility = @azimuth.add(@azimuth.add(base_agility, level_bonus), equipment_bonus)
  let final_intelligence = @azimuth.add(@azimuth.add(base_intelligence, level_bonus), equipment_bonus)
  
  @azimuth.assert_eq(23, final_strength)
  @azimuth.assert_eq(20, final_agility)
  @azimuth.assert_eq(26, final_intelligence)
  
  // 计算总属性值
  let total_attributes = @azimuth.add(@azimuth.add(final_strength, final_agility), final_intelligence)
  @azimuth.assert_eq(69, total_attributes)
  
  // 计算战斗评分（简化公式）
  let combat_rating = @azimuth.add(@azimuth.multiply(final_strength, 2), @azimuth.multiply(final_agility, 3))
  @azimuth.assert_eq(106, combat_rating)
}

test "database_query_optimization" {
  // 数据库查询优化模拟
  let table_size = 1000000     // 表大小（行数）
  let selectivity = 5          // 选择性（百分比）
  let index_efficiency = 80    // 索引效率（百分比）
  
  // 计算预估扫描行数
  let selected_rows = @azimuth.multiply(table_size, selectivity) / 100  // 50000
  @azimuth.assert_eq(50000, selected_rows)
  
  // 计算索引优化后的扫描行数
  let optimized_rows = @azimuth.multiply(selected_rows, index_efficiency) / 100
  @azimuth.assert_eq(40000, optimized_rows)
  
  // 计算性能提升（百分比）
  let performance_improvement = @azimuth.multiply(@azimuth.add(selected_rows, -optimized_rows), 100) / selected_rows
  @azimuth.assert_eq(20, performance_improvement)
  
  // 计算查询时间（毫秒）
  let base_time_per_row = 1    // 每行基础时间（毫秒）
  let original_time = @azimuth.multiply(selected_rows, base_time_per_row)
  let optimized_time = @azimuth.multiply(optimized_rows, base_time_per_row)
  
  @azimuth.assert_eq(50000, original_time)
  @azimuth.assert_eq(40000, optimized_time)
}

test "energy_efficiency_calculation" {
  // 能效计算
  let power_input_watts = 1000     // 输入功率（瓦特）
  let power_output_watts = 850     // 输出功率（瓦特）
  let operating_hours = 24         // 运行时间（小时）
  
  // 计算效率（百分比）
  let efficiency = @azimuth.multiply(power_output_watts, 100) / power_input_watts
  @azimuth.assert_eq(85, efficiency)
  
  // 计算能耗（千瓦时）
  let energy_consumption_kwh = @azimuth.multiply(@azimuth.multiply(power_input_watts, operating_hours), 1) / 1000
  @azimuth.assert_eq(24, energy_consumption_kwh)
  
  // 计算有用能量输出
  let useful_energy_kwh = @azimuth.multiply(@azimuth.multiply(power_output_watts, operating_hours), 1) / 1000
  @azimuth.assert_eq(20, useful_energy_kwh)
  
  // 计算能量损失
  let energy_loss_kwh = @azimuth.add(energy_consumption_kwh, -useful_energy_kwh)
  @azimuth.assert_eq(4, energy_loss_kwh)
}

test "string_pattern_matching_simulation" {
  // 字符串模式匹配模拟
  let text_length = 100
  let pattern_length = 5
  let alphabet_size = 26
  
  // 简化的模式匹配复杂度计算：O(n × m)
  let basic_complexity = @azimuth.multiply(text_length, pattern_length)
  @azimuth.assert_eq(500, basic_complexity)
  
  // KMP算法的复杂度：O(n + m)
  let kmp_complexity = @azimuth.add(text_length, pattern_length)
  @azimuth.assert_eq(105, kmp_complexity)
  
  // 计算性能提升
  let performance_gain = @azimuth.add(basic_complexity, -kmp_complexity)
  @azimuth.assert_eq(395, performance_gain)
  
  // 测试包含特殊字符的字符串处理
  let special_text = "Hello@World#2023$Test"
  @azimuth.assert_eq_string("Hello, Hello@World#2023$Test!", @azimuth.greet(special_text))
  
  // 测试数字字符串
  let numeric_text = "1234567890"
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet(numeric_text))
}