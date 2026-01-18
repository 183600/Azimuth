// Azimuth 项目增强测试套件
// 包含10个新的 MoonBit 测试用例，使用标准 MoonBit 测试语法

// 导入必要的模块
use azimuth

test "recursive_factorial_simulation" {
  // 递归阶乘模拟测试
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  
  // 模拟递归计算
  let step1 = azimuth.multiply(n, n - 1)        // 5 × 4 = 20
  let step2 = azimuth.multiply(step1, n - 2)    // 20 × 3 = 60
  let step3 = azimuth.multiply(step2, n - 3)    // 60 × 2 = 120
  let step4 = azimuth.multiply(step3, n - 4)    // 120 × 1 = 120
  
  @assertion.assert_eq(120, step4)?
}

test "binary_search_simulation" {
  // 二分查找模拟测试
  // 模拟在有序数组中查找元素的位置
  let array_size = 16
  let target = 7
  
  // 模拟二分查找的步骤
  let low = 0
  let high = array_size - 1
  let mid = azimuth.add(low, high) / 2  // (0 + 15) / 2 = 7
  
  // 检查是否找到目标
  let found = azimuth.eq(mid, target)
  @assertion.assert_true(found)?
  
  // 测试查找其他元素
  let target2 = 3
  let mid2 = azimuth.add(low, high) / 4  // 模拟第二次查找
  @assertion.assert_eq(3, mid2)?
}

test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 1000
  let eur_rate = 92    // 1 USD = 0.92 EUR (简化为整数)
  let gbp_rate = 79    // 1 USD = 0.79 GBP (简化为整数)
  let jpy_rate = 110   // 1 USD = 110 JPY
  
  // 计算兑换金额（使用整数除法模拟小数）
  let eur_amount = azimuth.multiply(usd_amount, eur_rate) / 100
  let gbp_amount = azimuth.multiply(usd_amount, gbp_rate) / 100
  let jpy_amount = azimuth.multiply(usd_amount, jpy_rate)
  
  @assertion.assert_eq(920, eur_amount)?   // 1000 × 92 / 100 = 920
  @assertion.assert_eq(790, gbp_amount)?   // 1000 × 79 / 100 = 790
  @assertion.assert_eq(110000, jpy_amount)? // 1000 × 110 = 110000
}

test "physics_motion_calculation" {
  // 物理运动计算测试
  // 模拟自由落体运动：s = v₀t + ½gt²
  let initial_velocity = 10  // 初始速度 (m/s)
  let time = 5               // 时间 (s)
  let gravity = 10           // 重力加速度 (m/s²，简化)
  
  // 计算位移：s = 10×5 + ½×10×5² = 50 + 5×25 = 50 + 125 = 175
  let velocity_distance = azimuth.multiply(initial_velocity, time)
  let gravity_distance = azimuth.multiply(azimuth.multiply(gravity, time), time) / 2
  let total_distance = azimuth.add(velocity_distance, gravity_distance)
  
  @assertion.assert_eq(175, total_distance)?
  
  // 计算最终速度：v = v₀ + gt = 10 + 10×5 = 60
  let final_velocity = azimuth.add(initial_velocity, azimuth.multiply(gravity, time))
  @assertion.assert_eq(60, final_velocity)?
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作：push和pop
  let stack_size = 0
  
  // push操作
  stack_size := azimuth.add(stack_size, 1)  // push 1
  stack_size := azimuth.add(stack_size, 1)  // push 2
  stack_size := azimuth.add(stack_size, 1)  // push 3
  @assertion.assert_eq(3, stack_size)?
  
  // pop操作
  stack_size := azimuth.add(stack_size, -1)  // pop 1
  @assertion.assert_eq(2, stack_size)?
  
  // 模拟队列操作：enqueue和dequeue
  let queue_head = 0
  let queue_tail = 0
  
  // enqueue操作
  queue_tail := azimuth.add(queue_tail, 1)  // enqueue 1
  queue_tail := azimuth.add(queue_tail, 1)  // enqueue 2
  @assertion.assert_eq(2, azimuth.add(queue_tail, -queue_head))?
  
  // dequeue操作
  queue_head := azimuth.add(queue_head, 1)  // dequeue 1
  @assertion.assert_eq(1, azimuth.add(queue_tail, -queue_head))?
}

test "compression_algorithm_simulation" {
  // 压缩算法模拟测试
  // 模拟简单的行程长度编码压缩
  let original_size = 100
  let repeated_char_count = 80
  let unique_char_count = 20
  
  // 压缩后的大小 = 唯一字符数 + 重复字符对数
  let compressed_size = azimuth.add(unique_char_count, repeated_char_count / 2)
  
  @assertion.assert_eq(60, compressed_size)?
  
  // 计算压缩率
  let compression_ratio = azimuth.multiply(compressed_size, 100) / original_size
  @assertion.assert_eq(60, compression_ratio)?  // 60%压缩率
}

test "network_data_transfer" {
  // 网络数据传输测试
  let file_size = 1048576  // 1MB = 1024×1024 bytes
  let bandwidth = 1048576  // 1MB/s带宽
  let latency = 50         // 50ms延迟
  let packet_size = 1400   // 1400字节包大小
  
  // 计算传输时间（不包括延迟）
  let transfer_time = azimuth.divide(file_size, bandwidth)
  @assertion.assert_eq(1, transfer_time)?  // 1秒
  
  // 计算包数量
  let packet_count = azimuth.divide(file_size, packet_size)
  @assertion.assert_eq(749, packet_count)?  // 1048576 / 1400 ≈ 749
  
  // 计算总延迟时间
  let total_latency = azimuth.multiply(packet_count, latency)
  @assertion.assert_eq(37450, total_latency)?  // 749 × 50 = 37450ms
}

test "database_index_calculation" {
  // 数据库索引计算测试
  let table_size = 10000
  let index_selectivity = 10  // 10%的选择性
  let page_size = 100
  
  // 计算索引扫描的行数
  let indexed_rows = azimuth.multiply(table_size, index_selectivity) / 100
  @assertion.assert_eq(1000, indexed_rows)?
  
  // 计算全表扫描的页数
  let full_scan_pages = azimuth.divide(table_size, page_size)
  @assertion.assert_eq(100, full_scan_pages)?
  
  // 计算索引扫描的页数
  let index_scan_pages = azimuth.divide(indexed_rows, page_size)
  @assertion.assert_eq(10, index_scan_pages)?
  
  // 计算性能提升倍数
  let performance_gain = azimuth.divide(full_scan_pages, index_scan_pages)
  @assertion.assert_eq(10, performance_gain)?
}

test "machine_learning_prediction" {
  // 机器学习预测模拟测试
  let feature1 = 5
  let feature2 = 3
  let feature3 = 2
  let weight1 = 2
  let weight2 = 4
  let weight3 = 6
  let bias = 1
  
  // 计算线性回归预测值：y = w1×x1 + w2×x2 + w3×x3 + b
  let weighted_sum1 = azimuth.multiply(feature1, weight1)
  let weighted_sum2 = azimuth.multiply(feature2, weight2)
  let weighted_sum3 = azimuth.multiply(feature3, weight3)
  
  let prediction = azimuth.add(azimuth.add(azimuth.add(weighted_sum1, weighted_sum2), weighted_sum3), bias)
  @assertion.assert_eq(41, prediction)?  // 5×2 + 3×4 + 2×6 + 1 = 10 + 12 + 12 + 1 = 35
  
  // 计算激活函数（ReLU）
  let activated_prediction = azimuth.max(0, prediction)
  @assertion.assert_eq(41, activated_prediction)?
  
  // 测试负值情况
  let negative_prediction = -10
  let activated_negative = azimuth.max(0, negative_prediction)
  @assertion.assert_eq(0, activated_negative)?
}

test "cryptography_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key = 54321
  let modulo = 100000  // 简化的模数
  
  // 模拟简单的加密：(message + key) % modulo
  let encrypted = azimuth.modulo(azimuth.add(message, key), modulo)
  @assertion.assert_eq(66666, encrypted)?  // (12345 + 54321) % 100000 = 66666
  
  // 模拟解密：(encrypted - key) % modulo
  let decrypted = azimuth.modulo(azimuth.add(encrypted, -key), modulo)
  @assertion.assert_eq(12345, decrypted)?
  
  // 模拟哈希函数（简化版）
  let hash_input = 98765
  let hash_prime = 31
  let hash_result = azimuth.modulo(azimuth.multiply(hash_input, hash_prime), modulo)
  @assertion.assert_eq(57015, hash_result)?  // (98765 × 31) % 100000 = 57015
}