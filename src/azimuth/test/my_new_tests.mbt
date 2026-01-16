// 用户补充的 MoonBit 测试用例
// 使用 @azimuth 前缀格式，补充现有测试覆盖

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(11, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(12, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 4))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(25, 5))
  
  // 负数测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
}

test "complex_calculator_expression" {
  // 测试复杂计算器表达式：((a + b) * c - d) / e
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  let e = 5
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)  // 40
  let product = @azimuth.multiply(sum_ab, c)  // 160
  let difference = @azimuth.add(product, -d)  // 130
  let result = difference / e  // 26
  
  @azimuth.assert_eq(26, result)
}

test "algorithm_binary_search_simulation" {
  // 模拟二分查找算法的索引计算
  let low = 0
  let high = 15
  let target = 9
  
  // 计算中间索引（避免溢出的方式）：mid = low + (high - low) / 2
  let mid = @azimuth.add(low, (high - low) / 2)
  @azimuth.assert_eq(7, mid)
  
  // 调整搜索范围
  let new_low = @azimuth.add(mid, 1)  // 8
  let new_mid = @azimuth.add(new_low, (high - new_low) / 2)
  @azimuth.assert_eq(11, new_mid)
  
  // 再次调整
  let new_high = @azimuth.add(new_mid, -1)  // 10
  let final_mid = @azimuth.add(new_low, (new_high - new_low) / 2)
  @azimuth.assert_eq(9, final_mid)
}

test "data_structure_array_operations" {
  // 测试数组相关操作的数学计算
  let rows = 4
  let cols = 5
  
  // 计算二维数组元素总数
  let total_elements = @azimuth.multiply(rows, cols)
  @azimuth.assert_eq(20, total_elements)
  
  // 计算二维数组转一维数组的索引
  let row_index = 2
  let col_index = 3
  let linear_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, linear_index)
  
  // 计算对角线元素数量
  let diagonal_count = if (rows < cols) { rows } else { cols }
  @azimuth.assert_eq(4, diagonal_count)
}

test "game_physics_simulation" {
  // 测试游戏物理模拟计算
  let initial_velocity = 10  // 初始速度
  let acceleration = 2       // 加速度
  let time = 5               // 时间
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²（简化为整数计算）
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2)
  @azimuth.assert_eq(75, displacement)
  
  // 计算动能：KE = 0.5mv²（简化为 m*v²/2，假设质量m=2）
  let mass = 2
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(final_velocity, final_velocity)) / 2
  @azimuth.assert_eq(400, kinetic_energy)
}

test "cryptography_simple_hash_simulation" {
  // 测试简单哈希算法模拟
  let input = 12345
  
  // 简单哈希函数：hash = (input * prime) mod table_size
  let prime = 31
  let table_size = 100
  
  // 计算 hash 值
  let hash_value = @azimuth.multiply(input, prime) % table_size
  @azimuth.assert_eq(35, hash_value)
  
  // 测试冲突解决：线性探测
  let collision_index = 35
  let step = 1
  let new_index = @azimuth.add(collision_index, step) % table_size
  @azimuth.assert_eq(36, new_index)
  
  // 双重哈希：hash2 = prime2 - (input mod prime2)
  let prime2 = 7
  let hash2 = @azimuth.add(prime2, -(input % prime2))
  @azimuth.assert_eq(3, hash2)
}

test "network_data_packet_calculations" {
  // 测试网络数据包相关计算
  let header_size = 20
  let payload_size = 1500
  let overhead_percent = 10
  
  // 计算总包大小
  let total_packet_size = @azimuth.add(header_size, payload_size)
  @azimuth.assert_eq(1520, total_packet_size)
  
  // 计算开销
  let overhead_bytes = @azimuth.multiply(total_packet_size, overhead_percent) / 100
  @azimuth.assert_eq(152, overhead_bytes)
  
  // 计算有效数据传输率
  let efficiency = @azimuth.multiply(payload_size, 100) / total_packet_size
  @azimuth.assert_eq(98, efficiency)  // 整数除法，舍去小数
  
  // 计算传输时间（假设带宽为1MB/s）
  let bandwidth_mbps = 1
  let transmission_time_ms = @azimuth.multiply(total_packet_size, 1000) / @azimuth.multiply(bandwidth_mbps, 1024 * 1024)
  @azimuth.assert_eq(1, transmission_time_ms)  // 整数除法
}

test "machine_learning_simple_predictions" {
  // 测试简单的机器学习预测计算
  let weight1 = 3
  let weight2 = 2
  let bias = 1
  let feature1 = 4
  let feature2 = 5
  
  // 线性回归预测：prediction = w1*x1 + w2*x2 + bias
  let weighted_sum1 = @azimuth.multiply(weight1, feature1)  // 12
  let weighted_sum2 = @azimuth.multiply(weight2, feature2)  // 10
  let prediction = @azimuth.add(@azimuth.add(weighted_sum1, weighted_sum2), bias)
  @azimuth.assert_eq(23, prediction)
  
  // 简单的激活函数（阶跃函数）
  let threshold = 20
  let activated = if (prediction > threshold) { 1 } else { 0 }
  @azimuth.assert_eq(1, activated)
  
  // 计算误差：error = actual - predicted
  let actual_value = 25
  let error = @azimuth.add(actual_value, -prediction)
  @azimuth.assert_eq(2, error)
}

test "image_processing_pixel_operations" {
  // 测试图像处理像素操作
  let image_width = 1920
  let image_height = 1080
  let bytes_per_pixel = 3  // RGB
  
  // 计算图像总像素数
  let total_pixels = @azimuth.multiply(image_width, image_height)
  @azimuth.assert_eq(2073600, total_pixels)
  
  // 计算图像总字节数
  let total_bytes = @azimuth.multiply(total_pixels, bytes_per_pixel)
  @azimuth.assert_eq(6220800, total_bytes)
  
  // 计算像素坐标（二维转一维）
  let pixel_x = 100
  let pixel_y = 200
  let pixel_index = @azimuth.add(@azimuth.multiply(pixel_y, image_width), pixel_x)
  @azimuth.assert_eq(385100, pixel_index)
  
  // 计算字节偏移
  let byte_offset = @azimuth.multiply(pixel_index, bytes_per_pixel)
  @azimuth.assert_eq(1155300, byte_offset)
}