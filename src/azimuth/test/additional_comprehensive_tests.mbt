// 额外综合测试用例
// 为 azimuth 项目添加更多测试覆盖

test "financial_compound_interest_calculation" {
  // 复利计算测试：本金 × (1 + 利率)^年数
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简化复利计算：10000 × (1 + 8%)^3 ≈ 10000 × 124 = 1240000
  let growth_factor = add(100, multiply(rate_percent, years))
  let total_amount = multiply(principal, growth_factor)
  
  assert_eq(1240000, total_amount)  // 10000 × (100 + 8×3) = 10000 × 124 = 1240000
}

test "data_structure_simulation" {
  // 模拟数据结构操作：计算数组索引和内存偏移
  let row = 5
  let col = 3
  let row_size = 10
  let element_size = 4
  
  // 计算二维数组在一维数组中的索引
  let array_index = add(multiply(row, row_size), col)
  // 计算内存偏移量
  let memory_offset = multiply(array_index, element_size)
  
  assert_eq(53, array_index)  // 5×10 + 3 = 53
  assert_eq(212, memory_offset)  // 53×4 = 212
}

test "cryptography_simulation" {
  // 模拟简单的加密算法：凯撒密码
  let original = 65  // 'A' 的 ASCII 码
  let shift = 3
  
  // 加密：(65 - 65 + 3) % 26 + 65 = 68 ('D')
  let encrypted = add(add(multiply(add(original, -65), 1), shift), 65)
  
  assert_eq(68, encrypted)
}

test "physics_calculation" {
  // 物理计算：自由落体运动
  let initial_velocity = 0
  let gravity = 10  // 简化重力加速度
  let time = 5
  
  // 距离 = v₀t + ½gt²
  let velocity_component = multiply(initial_velocity, time)
  let gravity_component = multiply(multiply(gravity, time), time) / 2
  let distance = add(velocity_component, gravity_component)
  
  assert_eq(125, distance)  // 0×5 + (10×5×5)/2 = 125
}

test "network_protocol_simulation" {
  // 模拟网络协议计算：数据包大小和传输时间
  let header_size = 20
  let payload_size = 1024
  let bandwidth = 1000  // 字节/秒
  
  // 总数据包大小
  let packet_size = add(header_size, payload_size)
  // 传输时间
  let transmission_time = packet_size / bandwidth
  
  assert_eq(1044, packet_size)
  assert_eq(1, transmission_time)  // 1044/1000 = 1.04，取整为1
}

test "game_physics_simulation" {
  // 游戏物理模拟：碰撞检测
  let object1_x = 100
  let object1_y = 200
  let object1_width = 50
  let object1_height = 50
  
  let object2_x = 140
  let object2_y = 240
  let object2_width = 60
  let object2_height = 60
  
  // 计算物体边界
  let obj1_right = add(object1_x, object1_width)
  let obj1_bottom = add(object1_y, object1_height)
  let obj2_right = add(object2_x, object2_width)
  let obj2_bottom = add(object2_y, object2_height)
  
  // 简化的碰撞检测：检查是否有重叠
  let horizontal_overlap = object1_x < obj2_right && obj1_right > object2_x
  let vertical_overlap = object1_y < obj2_bottom && obj1_bottom > object2_y
  
  assert_true(horizontal_overlap)
  assert_true(vertical_overlap)
}

test "image_processing_simulation" {
  // 模拟图像处理：像素颜色转换
  let red = 255
  let green = 128
  let blue = 64
  
  // 转换为灰度值：0.299×R + 0.587×G + 0.114×B
  // 简化为整数计算：(299×R + 587×G + 114×B) / 1000
  let gray_value = add(add(multiply(red, 299), multiply(green, 587)), multiply(blue, 114)) / 1000
  
  assert_eq(151, gray_value)  // (255×299 + 128×587 + 64×114) / 1000 ≈ 151
}

test "database_query_simulation" {
  // 模拟数据库查询计算：分页查询
  let total_records = 1250
  let page_size = 100
  let requested_page = 3
  
  // 计算偏移量：页码 × 页面大小
  let offset = multiply(requested_page, page_size)
  // 计算剩余记录数
  let remaining = add(total_records, multiply(-1, offset))
  
  assert_eq(300, offset)  // 3×100 = 300
  assert_eq(950, remaining)  // 1250 - 300 = 950
}

test "compression_algorithm_simulation" {
  // 模拟压缩算法：计算压缩比
  let original_size = 5000
  let compressed_size = 1500
  
  // 计算压缩比：(原始大小 - 压缩后大小) / 原始大小 × 100
  let size_difference = add(original_size, multiply(-1, compressed_size))
  let compression_ratio = multiply(size_difference, 100) / original_size
  
  assert_eq(70, compression_ratio)  // (5000-1500)/5000×100 = 70%
}

test "machine_learning_calculation" {
  // 机器学习计算：线性回归预测
  let weight = 2
  let bias = 10
  let input = 15
  
  // 预测值：weight × input + bias
  let prediction = add(multiply(weight, input), bias)
  
  assert_eq(40, prediction)  // 2×15 + 10 = 40
  
  // 计算误差：实际值 - 预测值
  let actual_value = 38
  let error = add(actual_value, multiply(-1, prediction))
  
  assert_eq(-2, error)  // 38 - 40 = -2
}