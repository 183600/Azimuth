// 额外的 MoonBit 测试场景
// 包含 10 个新的测试用例，涵盖不同的实际应用场景

test "data_structure_simulation" {
  // 模拟数据结构操作：使用整数运算模拟栈操作
  let initial_size = 0
  
  // 模拟入栈操作
  let stack_size_after_pushes = @azimuth.add(@azimuth.add(@azimuth.add(initial_size, 1), 1), 1)
  
  @azimuth.assert_eq(3, stack_size_after_pushes)
  
  // 模拟出栈操作
  let final_stack_size = @azimuth.add(@azimuth.add(@azimuth.add(stack_size_after_pushes, -1), -1), -1)
  
  @azimuth.assert_eq(0, final_stack_size)
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  let n = 100
  
  // O(1) 常数时间操作
  let constant_time = 1
  @azimuth.assert_eq(1, constant_time)
  
  // O(n) 线性时间操作：计算 1 + 2 + ... + n = n * (n + 1) / 2
  let linear_time = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, linear_time)
  
  // O(n²) 平方时间操作：计算前n个数的平方和（简化版）
  // 使用公式：n * (n + 1) * (2n + 1) / 6 ≈ n * n * n / 3（简化）
  let quadratic_time = @azimuth.multiply(n, @azimuth.multiply(n, n)) / 3
  @azimuth.assert_eq(333333, quadratic_time)
}

test "encryption_simulation" {
  // 模拟简单的加密算法：凯撒密码
  let shift = 3
  let original_char_code = 65  // 'A' 的 ASCII 码
  
  // 加密：(原值 + 位移) mod 26
  let encrypted_code = @azimuth.add(original_char_code, shift)
  @azimuth.assert_eq(68, encrypted_code)  // 'D'
  
  // 解密：(加密值 - 位移) mod 26
  let decrypted_code = @azimuth.add(encrypted_code, -shift)
  @azimuth.assert_eq(65, decrypted_code)  // 'A'
  
  // 测试循环位移
  let z_code = 90  // 'Z'
  let z_shifted = @azimuth.add(z_code, shift)
  @azimuth.assert_eq(93, z_shifted)  // 超出字母范围
}

test "physics_simulation" {
  // 物理计算模拟：自由落体运动
  let gravity = 10  // 重力加速度 m/s²（简化值）
  let time = 5      // 时间 s
  let initial_velocity = 0  // 初始速度 m/s
  
  // 计算位移：s = v₀t + ½gt²
  let velocity_term = @azimuth.multiply(initial_velocity, time)
  let gravity_term = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2
  let displacement = @azimuth.add(velocity_term, gravity_term)
  
  @azimuth.assert_eq(125, displacement)  // 0*5 + 10*5*5/2 = 125
  
  // 计算最终速度：v = v₀ + gt
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, time))
  @azimuth.assert_eq(50, final_velocity)  // 0 + 10*5 = 50
}

test "financial_amortization" {
  // 金融计算：贷款摊销（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率 %
  let years = 30          // 贷款年限
  
  // 计算总利息（简化）：本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 计算月供（简化）：总还款额 / (年数 × 12)
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  
  @azimuth.assert_eq(18000000, total_interest)   // 100000 × 6 × 30 = 18000000
  @azimuth.assert_eq(18100000, total_payment)    // 100000 + 18000000 = 18100000
  @azimuth.assert_eq(50277, monthly_payment)     // 18100000 / 360 ≈ 50277
}

test "network_protocol_simulation" {
  // 网络协议模拟：数据包传输
  let packet_size = 1024    // 数据包大小 字节
  let header_size = 20      // 包头大小 字节
  let payload_size = @azimuth.add(packet_size, -header_size)  // 有效载荷
  
  // 计算传输效率
  let efficiency = @azimuth.multiply(payload_size, 100) / packet_size
  
  @azimuth.assert_eq(1004, payload_size)
  @azimuth.assert_eq(98, efficiency)  // 1004 * 100 / 1024 ≈ 98%
  
  // 模拟数据传输：发送10个包
  let num_packets = 10
  let total_data = @azimuth.multiply(num_packets, packet_size)
  let total_payload = @azimuth.multiply(num_packets, payload_size)
  
  @azimuth.assert_eq(10240, total_data)
  @azimuth.assert_eq(10040, total_payload)
}

test "machine_learning_prediction" {
  // 机器学习预测模拟：线性回归
  let x1 = 2  // 特征1
  let x2 = 3  // 特征2
  let x3 = 4  // 特征3
  
  // 模型参数（简化）
  let w1 = 5  // 权重1
  let w2 = -3  // 权重2
  let w3 = 2   // 权重3
  let b = 10   // 偏置
  
  // 预测：y = w1*x1 + w2*x2 + w3*x3 + b
  let term1 = @azimuth.multiply(w1, x1)
  let term2 = @azimuth.multiply(w2, x2)
  let term3 = @azimuth.multiply(w3, x3)
  let prediction = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), b)
  
  @azimuth.assert_eq(10, term1)   // 5 * 2 = 10
  @azimuth.assert_eq(-9, term2)   // -3 * 3 = -9
  @azimuth.assert_eq(8, term3)    // 2 * 4 = 8
  @azimuth.assert_eq(19, prediction)  // 10 + (-9) + 8 + 10 = 19
  
  // 计算误差（简化）
  let actual = 20
  let error = @azimuth.add(actual, -prediction)
  @azimuth.assert_eq(1, error)
}

test "database_index_simulation" {
  // 数据库索引模拟：B树索引查找
  let tree_height = 3
  let fan_out = 4    // 每个节点的最大子节点数
  let records_per_leaf = 10
  
  // 计算最大记录数：(fan_out ^ tree_height) * records_per_leaf
  let power = @azimuth.multiply(@azimuth.multiply(fan_out, fan_out), fan_out)
  let max_records = @azimuth.multiply(power, records_per_leaf)
  
  @azimuth.assert_eq(64, power)        // 4^3 = 64
  @azimuth.assert_eq(640, max_records) // 64 * 10 = 640
  
  // 计算查找复杂度：O(log n)
  // 简化计算：tree_height
  let search_cost = tree_height
  @azimuth.assert_eq(3, search_cost)
  
  // 计算插入成本（可能需要分裂）
  let split_probability = 75  // 75% 概率需要分裂
  let insertion_cost = @azimuth.add(search_cost, @azimuth.multiply(search_cost, split_probability) / 100)
  @azimuth.assert_eq(5, insertion_cost)  // 3 + 3*75/100 = 3 + 2 = 5
}

test "image_processing_simulation" {
  // 图像处理模拟：简单滤镜效果
  let image_width = 100
  let image_height = 100
  let total_pixels = @azimuth.multiply(image_width, image_height)
  
  // RGB 值
  let red = 255
  let green = 128
  let blue = 64
  
  // 亮度调整（增加20%）
  let brightness_factor = 120  // 120%
  let new_red = @azimuth.multiply(red, brightness_factor) / 100
  let new_green = @azimuth.multiply(green, brightness_factor) / 100
  let new_blue = @azimuth.multiply(blue, brightness_factor) / 100
  
  // 确保不超过最大值255
  let max_value = 255
  let final_red = if new_red > max_value { max_value } else { new_red }
  let final_green = if new_green > max_value { max_value } else { new_green }
  let final_blue = if new_blue > max_value { max_value } else { new_blue }
  
  @azimuth.assert_eq(10000, total_pixels)
  @azimuth.assert_eq(255, final_red)    // 255 * 120 / 100 = 306 > 255, 所以为255
  @azimuth.assert_eq(153, final_green)  // 128 * 120 / 100 = 153.6 ≈ 153
  @azimuth.assert_eq(76, final_blue)    // 64 * 120 / 100 = 76.8 ≈ 76
  
  // 计算灰度值（加权平均法）
  let gray_value = @azimuth.add(@azimuth.multiply(final_red, 3), @azimuth.add(@azimuth.multiply(final_green, 6), @azimuth.multiply(final_blue, 1))) / 10
  @azimuth.assert_eq(169, gray_value)   // (255*3 + 153*6 + 76*1) / 10 = 169
}

test "compression_algorithm_simulation" {
  // 压缩算法模拟：简化的行程编码
  let original_size = 1000
  let run_length = 50    // 平均行程长度
  let symbol_size = 8    // 每个符号位数
  let count_size = 16    // 计数器位数
  
  // 计算压缩后大小
  let num_runs = original_size / run_length
  let compressed_size = @azimuth.multiply(num_runs, @azimuth.add(symbol_size, count_size))
  
  // 计算压缩比
  let compression_ratio = @azimuth.multiply(compressed_size, 100) / original_size
  
  @azimuth.assert_eq(20, num_runs)      // 1000 / 50 = 20
  @azimuth.assert_eq(480, compressed_size)  // 20 * (8 + 16) = 480
  @azimuth.assert_eq(48, compression_ratio) // 480 * 100 / 1000 = 48%
  
  // 计算节省空间
  let space_saved = @azimuth.add(original_size, -compressed_size)
  let saving_percentage = @azimuth.multiply(space_saved, 100) / original_size
  
  @azimuth.assert_eq(520, space_saved)      // 1000 - 480 = 520
  @azimuth.assert_eq(52, saving_percentage) // 520 * 100 / 1000 = 52%
}