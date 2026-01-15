// 增强应用测试用例 - 测试实际应用场景和边界情况
test "financial_compound_interest" {
  // 金融复利计算测试
  // 本金 × (1 + 年利率) ^ 年数
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算，避免指数运算
  let year1_total = azimuth::add(principal, azimuth::multiply(principal, rate_percent) / 100)
  let year2_total = azimuth::add(year1_total, azimuth::multiply(year1_total, rate_percent) / 100)
  let year3_total = azimuth::add(year2_total, azimuth::multiply(year2_total, rate_percent) / 100)
  
  // 验证结果 (10000 * 1.05 * 1.05 * 1.05 ≈ 11576)
  assert_true(year3_total >= 11570 && year3_total <= 11580)
}

test "physics_kinematics" {
  // 物理运动学计算测试
  // 距离 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  let initial_velocity = 10  // 米/秒
  let acceleration = 2       // 米/秒²
  let time = 5               // 秒
  
  // 简化计算：距离 = v*t + a*t²/2
  let distance_part1 = azimuth::multiply(initial_velocity, time)
  let distance_part2 = azimuth::multiply(acceleration, azimuth::multiply(time, time)) / 2
  let total_distance = azimuth::add(distance_part1, distance_part2)
  
  // 验证结果：10*5 + 2*25/2 = 50 + 25 = 75
  assert_eq(75, total_distance)
}

test "data_structure_array_operations" {
  // 数据结构数组操作测试
  // 模拟二维数组的一维索引计算
  let rows = 4
  let cols = 5
  let target_row = 2
  let target_col = 3
  
  // 计算一维索引：index = row * cols + col
  let index = azimuth::add(azimuth::multiply(target_row, cols), target_col)
  assert_eq(13, index)
  
  // 计算边界索引
  let first_index = azimuth::add(azimuth::multiply(0, cols), 0)
  let last_index = azimuth::add(azimuth::multiply(rows - 1, cols), cols - 1)
  assert_eq(0, first_index)
  assert_eq(19, last_index)
}

test "algorithm_binary_search_simulation" {
  // 算法二分查找模拟测试
  // 模拟在有序数组中查找元素的位置
  let array_size = 16
  let target = 11
  
  // 简化的二分查找步骤模拟
  let left = 0
  let right = array_size - 1
  let mid = azimuth::add(left, right) / 2
  
  // 模拟查找过程
  let found_position = azimuth::add(mid, 3)  // 模拟找到目标位置
  assert_eq(11, found_position)
  
  // 验证边界情况
  assert_true(found_position >= left && found_position <= right)
}

test "network_data_transfer" {
  // 网络数据传输计算测试
  let file_size_mb = 100
  let network_speed_mbps = 20
  let overhead_percent = 10
  
  // 计算理论传输时间（秒）
  let base_time = azimuth::multiply(file_size_mb, 8) / network_speed
  
  // 计算包含开销的实际时间
  let overhead_time = azimuth::multiply(base_time, overhead_percent) / 100
  let total_time = azimuth::add(base_time, overhead_time)
  
  // 验证结果：100*8/20 + 10% = 40 + 4 = 44秒
  assert_eq(44, total_time)
}

test "image_processing_pixel_manipulation" {
  // 图像处理像素操作测试
  let image_width = 1920
  let image_height = 1080
  let bytes_per_pixel = 4  // RGBA
  
  // 计算图像总字节数
  let total_pixels = azimuth::multiply(image_width, image_height)
  let total_bytes = azimuth::multiply(total_pixels, bytes_per_pixel)
  
  // 验证结果：1920*1080*4 = 8294400
  assert_eq(8294400, total_bytes)
  
  // 计算特定像素的位置（第100行，第200列）
  let pixel_row = 100
  let pixel_col = 200
  let pixel_offset = azimuth::add(azimuth::multiply(pixel_row, image_width), pixel_col)
  let byte_offset = azimuth::multiply(pixel_offset, bytes_per_pixel)
  
  assert_eq(azimuth::multiply(azimuth::add(azimuth::multiply(100, 1920), 200), 4), byte_offset)
}

test "cryptography_simple_hashing" {
  // 密码学简单哈希模拟测试
  let input_value = 12345
  let prime_modulus = 9973
  let multiplier = 31
  
  // 简化的哈希函数模拟
  let hash_value = azimuth::multiply(input_value, multiplier) % prime_modulus
  assert_true(hash_value >= 0 && hash_value < prime_modulus)
  
  // 验证哈希值的一致性
  let hash_value2 = azimuth::multiply(input_value, multiplier) % prime_modulus
  assert_eq(hash_value, hash_value2)
}

test "database_pagination_calculations" {
  // 数据库分页计算测试
  let total_records = 1234
  let page_size = 50
  let target_page = 10
  
  // 计算总页数（向上取整）
  let total_pages = azimuth::divide_with_ceil(total_records, page_size)
  assert_eq(25, total_pages)  // 1234/50 = 24.68 → 25页
  
  // 计算目标页的起始记录索引
  let start_index = azimuth::multiply(target_page - 1, page_size)
  assert_eq(450, start_index)  // (10-1)*50 = 450
  
  // 计算目标页的结束记录索引
  let end_index = azimuth::add(start_index, page_size) - 1
  assert_eq(499, end_index)  // 450 + 50 - 1 = 499
  
  // 验证边界情况
  assert_true(start_index >= 0 && start_index < total_records)
  assert_true(end_index >= start_index && end_index < total_records)
}

test "compression_ratio_calculations" {
  // 压缩率计算测试
  let original_size = 1048576  // 1MB
  let compressed_size = 262144  // 256KB
  
  // 计算压缩率（百分比）
  let compression_ratio = azimuth::multiply(compressed_size, 100) / original_size
  assert_eq(25, compression_ratio)  // 262144*100/1048576 = 25%
  
  // 计算空间节省率
  let space_saved = azimuth::multiply(azimuth::add(original_size, -compressed_size), 100) / original_size
  assert_eq(75, space_saved)  // (1048576-262144)*100/1048576 = 75%
  
  // 验证压缩率和空间节省率的关系
  assert_eq(azimuth::add(compression_ratio, space_saved), 100)
}

test "game_physics_projectile_motion" {
  // 游戏物理抛物线运动测试
  let initial_velocity = 50  // 米/秒
  let launch_angle = 45      // 度
  let gravity = 10           // 米/秒²（简化值）
  
  // 简化的抛物线运动计算
  // 水平速度 = 初速度 × cos(45°) ≈ 初速度 × 0.7（简化）
  let horizontal_velocity = azimuth::multiply(initial_velocity, 7) / 10
  
  // 垂直速度 = 初速度 × sin(45°) ≈ 初速度 × 0.7（简化）
  let vertical_velocity = azimuth::multiply(initial_velocity, 7) / 10
  
  // 飞行时间 = 2 × 垂直速度 / 重力
  let flight_time = azimuth::multiply(2, vertical_velocity) / gravity
  
  // 最大距离 = 水平速度 × 飞行时间
  let max_distance = azimuth::multiply(horizontal_velocity, flight_time)
  
  // 验证结果：50*0.7 * (2*50*0.7/10) = 35 * 7 = 245
  assert_eq(245, max_distance)
}