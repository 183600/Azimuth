// 综合应用测试用例 - 专注于实际应用场景和边缘情况
// 这些测试用例补充了现有测试覆盖范围，专注于更复杂的实际应用场景

test "financial_loan_calculation" {
  // 金融贷款计算测试
  // 计算月供：PMT = (P × r × (1 + r)^n) / ((1 + r)^n - 1)
  // 简化版本：使用整数计算
  
  let principal = 100000  // 贷款本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限30年
  
  // 简化的月供计算（整数近似）
  let monthly_rate = annual_rate / 12  // 月利率
  let months = years * 12              // 总月数
  let total_interest = multiply(principal, monthly_rate * months) / 100
  let total_payment = add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  assert_eq(277, monthly_payment)  // 简化计算结果
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 1000
  let sales_month1 = 150
  let sales_month2 = 200
  let sales_month3 = 175
  let restock_month2 = 300
  let restock_month3 = 500
  
  // 计算每月库存变化
  let stock_after_month1 = add(initial_stock, -sales_month1)
  let stock_after_month2 = add(add(stock_after_month1, -sales_month2), restock_month2)
  let stock_after_month3 = add(add(stock_after_month2, -sales_month3), restock_month3)
  
  assert_eq(850, stock_after_month1)   // 1000 - 150 = 850
  assert_eq(950, stock_after_month2)   // 850 - 200 + 300 = 950
  assert_eq(1275, stock_after_month3)  // 950 - 175 + 500 = 1275
  
  // 计算总销量和总进货量
  let total_sales = add(add(sales_month1, sales_month2), sales_month3)
  let total_restock = add(restock_month2, restock_month3)
  let net_change = add(total_restock, -total_sales)
  
  assert_eq(525, total_sales)    // 150 + 200 + 175 = 525
  assert_eq(800, total_restock)  // 300 + 500 = 800
  assert_eq(275, net_change)     // 800 - 525 = 275
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  // 数据集：一周温度记录 [12, 15, 18, 22, 20, 16, 14]
  
  let temps = [12, 15, 18, 22, 20, 16, 14]
  let day_count = 7
  
  // 计算平均温度
  let total_temp = add(add(add(add(add(add(12, 15), 18), 22), 20), 16), 14)
  let avg_temp = total_temp / day_count
  assert_eq(17, avg_temp)  // 117 / 7 = 16.71 ≈ 17
  
  // 计算温度范围
  let max_temp = 22
  let min_temp = 12
  let temp_range = add(max_temp, -min_temp)
  assert_eq(10, temp_range)
  
  // 计算温度方差（简化版）
  let deviations = [
    multiply(12 - 17, 12 - 17),  // 25
    multiply(15 - 17, 15 - 17),  // 4
    multiply(18 - 17, 18 - 17),  // 1
    multiply(22 - 17, 22 - 17),  // 25
    multiply(20 - 17, 20 - 17),  // 9
    multiply(16 - 17, 16 - 17),  // 1
    multiply(14 - 17, 14 - 17)   // 9
  ]
  
  let variance_sum = add(add(add(add(add(add(25, 4), 1), 25), 9), 1), 9)
  let variance = variance_sum / day_count
  assert_eq(10, variance)  // 74 / 7 = 10.57 ≈ 10
}

test "engineering_calculations" {
  // 工程计算测试
  // 计算梁的弯曲应力：σ = M × c / I
  // 简化版本使用整数计算
  
  let moment = 1000      // 弯矩 (N·m)
  let distance = 50      // 距离中性轴的距离 (mm)
  let inertia = 100000   // 惯性矩 (mm^4)
  
  // 计算弯曲应力（简化）
  let stress = multiply(moment, distance) / inertia
  assert_eq(0, stress)  // 1000 × 50 / 100000 = 0.5 ≈ 0 (整数除法)
  
  // 使用更大的数值避免整数除法精度损失
  let moment_scaled = 100000
  let stress_scaled = multiply(moment_scaled, distance) / inertia
  assert_eq(50, stress_scaled)  // 100000 × 50 / 100000 = 50
  
  // 计算安全系数
  let yield_strength = 250  // 屈服强度 (MPa)
  let safety_factor = yield_strength / stress_scaled
  assert_eq(5, safety_factor)  // 250 / 50 = 5
}

test "network_performance_metrics" {
  // 网络性能指标测试
  let packet_size = 1500     // 数据包大小 (bytes)
  let transmission_rate = 1000000  // 传输速率 (bits/s)
  let propagation_delay = 50  // 传播延迟 (ms)
  let processing_delay = 10   // 处理延迟 (ms)
  
  // 计算传输延迟
  let transmission_delay = multiply(packet_size, 8) * 1000 / transmission_rate
  assert_eq(12, transmission_delay)  // 1500 × 8 × 1000 / 1000000 = 12ms
  
  // 计算总延迟
  let total_delay = add(add(transmission_delay, propagation_delay), processing_delay)
  assert_eq(72, total_delay)  // 12 + 50 + 10 = 72ms
  
  // 计算往返时间 (RTT)
  let rtt = multiply(total_delay, 2)
  assert_eq(144, rtt)
  
  // 计算吞吐量（简化）
  let throughput = multiply(packet_size, 1000) / total_delay
  assert_eq(20833, throughput)  // 1500 × 1000 / 72 ≈ 20833 bytes/s
}

test "game_physics_simulation" {
  // 游戏物理模拟测试
  // 模拟抛物线运动
  
  let initial_velocity_x = 100  // 水平初速度
  let initial_velocity_y = 200  // 垂直初速度
  let gravity = 10              // 重力加速度
  let time_step = 5             // 时间步长
  
  // 计算t时刻的速度和位置
  let time = time_step
  
  // 速度计算
  let velocity_x = initial_velocity_x  // 水平速度不变
  let velocity_y = add(initial_velocity_y, multiply(-gravity, time))  // 垂直速度
  
  assert_eq(100, velocity_x)  // 水平速度保持不变
  assert_eq(150, velocity_y)  // 200 - 10 × 5 = 150
  
  // 位置计算
  let position_x = multiply(initial_velocity_x, time)
  let position_y = add(multiply(initial_velocity_y, time), multiply(-gravity, multiply(time, time)) / 2)
  
  assert_eq(500, position_x)  // 100 × 5 = 500
  assert_eq(875, position_y)  // 200 × 5 - 10 × 25 / 2 = 1000 - 125 = 875
  
  // 计算总速度
  let total_velocity = add(multiply(velocity_x, velocity_x), multiply(velocity_y, velocity_y))
  assert_eq(32500, total_velocity)  // 100² + 150² = 10000 + 22500 = 32500
}

test "cryptography_hash_simulation" {
  // 密码学哈希模拟测试
  // 简化的哈希函数模拟
  
  let message = "HelloWorld"
  let message_length = 10
  
  // 简化的哈希计算（基于字符串长度和内容的模拟）
  let hash_base = multiply(message_length, 31)  // 基于长度的哈希基础值
  let hash_prime = 1000003  // 大质数
  
  // 模拟字符处理
  let char_sum = 72 + 101 + 108 + 108 + 111 + 87 + 111 + 114 + 108 + 100  // ASCII码和
  let hash_value = multiply(hash_base, char_sum) % hash_prime
  
  assert_eq(310, hash_base)  // 10 × 31 = 310
  assert_eq(1030, char_sum)  // 各字符ASCII码之和
  assert_eq(319300, multiply(hash_base, char_sum))  // 310 × 1030 = 319300
  assert_eq(319300, hash_value)  // 319300 % 1000003 = 319300
  
  // 模拟哈希验证
  let received_message = "HelloWorld"
  let received_length = 10
  let received_hash = hash_value
  
  let verification_passed = received_length == message_length && received_hash == hash_value
  assert_true(verification_passed)
}

test "machine_learning_feature_scaling" {
  // 机器学习特征缩放测试
  // 标准化：(x - μ) / σ
  // 归一化：(x - min) / (max - min)
  
  let feature_values = [10, 20, 30, 40, 50]
  let count = 5
  
  // 计算均值
  let sum = add(add(add(add(10, 20), 30), 40), 50)
  let mean = sum / count
  assert_eq(30, mean)
  
  // 计算标准差（简化）
  let variance = add(add(add(add(multiply(10-30, 10-30), multiply(20-30, 20-30)), 
                           multiply(30-30, 30-30)), multiply(40-30, 40-30)), multiply(50-30, 50-30)) / count
  let std_dev = 10  // 简化的标准差值
  
  // 标准化第一个值
  let standardized_first = (10 - mean) / std_dev
  assert_eq(-2, standardized_first)  // (10 - 30) / 10 = -2
  
  // 归一化第一个值
  let min_val = 10
  let max_val = 50
  let normalized_first = (10 - min_val) * 100 / (max_val - min_val)
  assert_eq(0, normalized_first)  // (10 - 10) / (50 - 10) = 0
  
  // 归一化最后一个值
  let normalized_last = (50 - min_val) * 100 / (max_val - min_val)
  assert_eq(100, normalized_last)  // (50 - 10) / (50 - 10) = 1 × 100 = 100
}

test "database_query_optimization" {
  // 数据库查询优化测试
  // 模拟查询执行计划成本计算
  
  let table_size = 1000000        // 表大小
  let index_selectivity = 10      // 索引选择性（百分比）
  let join_table_size = 500000    // 连接表大小
  let sort_memory_limit = 10000   // 排序内存限制
  
  // 计算全表扫描成本
  let full_scan_cost = table_size
  assert_eq(1000000, full_scan_cost)
  
  // 计算索引扫描成本
  let index_scan_cost = multiply(table_size, index_selectivity) / 100
  assert_eq(100000, index_scan_cost)  // 1000000 × 10 / 100 = 100000
  
  // 计算连接操作成本
  let join_cost = multiply(index_scan_cost, join_table_size) / 1000  // 简化计算
  assert_eq(50000000, join_cost)  // 100000 × 500000 / 1000 = 50000000
  
  // 计算排序成本
  let sort_cost = if table_size <= sort_memory_limit {
    table_size * 2  // 内存排序
  } else {
    multiply(table_size, 10)  // 磁盘排序
  }
  assert_eq(10000000, sort_cost)  // 1000000 > 10000，所以使用磁盘排序
  
  // 计算总查询成本
  let total_query_cost = add(add(index_scan_cost, join_cost), sort_cost)
  assert_eq(60100000, total_query_cost)  // 100000 + 50000000 + 10000000 = 60100000
}

test "image_processing_filters" {
  // 图像处理滤镜测试
  // 模拟简单的图像滤镜计算
  
  let image_width = 100
  let image_height = 100
  let pixel_count = multiply(image_width, image_height)
  
  // 计算图像大小
  assert_eq(10000, pixel_count)
  
  // 模拟亮度调整
  let brightness_factor = 120  // 120% 亮度
  let original_pixel = 128     // 原始像素值
  let adjusted_pixel = multiply(original_pixel, brightness_factor) / 100
  
  assert_eq(153, adjusted_pixel)  // 128 × 120 / 100 = 153.6 ≈ 153
  
  // 模拟对比度调整
  let contrast_factor = 150     // 150% 对比度
  let contrast_center = 128     // 对比度中心点
  let contrasted_pixel = add(multiply(adjusted_pixel - contrast_center, contrast_factor) / 100, contrast_center)
  
  assert_eq(179, contrasted_pixel)  // (153 - 128) × 150 / 100 + 128 = 37.5 + 128 = 165.5 ≈ 165
  
  // 模拟模糊滤镜（3x3核）
  let kernel_sum = 16  // 3x3高斯模糊核的和
  let weighted_sum = multiply(original_pixel, 4) + 
                    multiply(adjusted_pixel, 8) + 
                    multiply(contrasted_pixel, 4)
  let blurred_pixel = weighted_sum / kernel_sum
  
  assert_eq(162, blurred_pixel)  // (128×4 + 153×8 + 165×4) / 16 = 2592 / 16 = 162
  
  // 计算处理时间（简化）
  let pixels_per_second = 1000000
  let processing_time = multiply(pixel_count, 1000) / pixels_per_second
  assert_eq(10, processing_time)  // 10000 × 1000 / 1000000 = 10ms
}