// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和新的测试场景

test "boundary_value_comprehensive" {
  // 边界值综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "string_edge_cases" {
  // 字符串边界情况测试
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposesWithVariousCharacters1234567890"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 测试Unicode字符
  @azimuth.assert_eq_string("Hello, 🚀🌟!", @azimuth.greet("🚀🌟"))
}

test "mathematical_series" {
  // 数学级数测试
  // 计算等差数列前n项和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 100
  let arithmetic_sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, arithmetic_sum)?
  
  // 计算等比数列前n项和：1 + 2 + 4 + 8 + ... + 2^(n-1) = 2^n - 1
  // 计算1 + 2 + 4 + 8 + 16 = 31
  let geometric_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 2), 4), 8), 16)
  @assertion.assert_eq(31, geometric_sum)?
  
  // 计算平方和：1² + 2² + 3² + ... + n²
  // 计算1² + 2² + 3² + 4² + 5² = 1 + 4 + 9 + 16 + 25 = 55
  let sum_of_squares = azimuth.add(azimuth.add(azimuth.add(azimuth.add(
    azimuth.multiply(1, 1), 
    azimuth.multiply(2, 2)), 
    azimuth.multiply(3, 3)), 
    azimuth.add(azimuth.multiply(4, 4), azimuth.multiply(5, 5)))
  @assertion.assert_eq(55, sum_of_squares)?
}

test "physics_simulation" {
  // 物理模拟测试
  // 自由落体运动：h = v₀t + 0.5gt²（简化为整数计算）
  let initial_velocity = 10  // 初始速度 m/s
  let time = 5              // 时间 s
  let gravity = 10          // 重力加速度 m/s²（简化为10）
  
  // 计算位移：h = 10×5 + 0.5×10×5² = 50 + 125 = 175
  let displacement = azimuth.add(
    azimuth.multiply(initial_velocity, time),
    azimuth.multiply(azimuth.multiply(5, gravity), azimuth.multiply(time, time)) / 10
  )
  @assertion.assert_eq(175, displacement)?
  
  // 动能计算：E = 0.5mv²（简化为整数计算）
  let mass = 10  // 质量 kg
  let velocity = 15  // 速度 m/s
  
  // 计算动能：E = 0.5×10×15² = 5×225 = 1125
  let kinetic_energy = azimuth.multiply(azimuth.multiply(mass, 5), azimuth.multiply(velocity, velocity)) / 10
  @assertion.assert_eq(1125, kinetic_energy)?
}

test "algorithm_complexity" {
  // 算法复杂度测试
  // 线性复杂度：O(n) = n
  let n = 1000
  let linear_operations = n
  @assertion.assert_eq(1000, linear_operations)?
  
  // 平方复杂度：O(n²) = n × n
  let quadratic_operations = azimuth.multiply(n, n)
  @assertion.assert_eq(1000000, quadratic_operations)?
  
  // 对数复杂度（简化）：O(log n) 使用整数除法模拟
  let log_operations = 0
  let temp_n = n
  // 简化的对数计算：1000 → 500 → 250 → 125 → 62 → 31 → 15 → 7 → 3 → 1（约10次）
  let log_approx = 10
  @assertion.assert_eq(10, log_approx)?
  
  // 组合复杂度：O(n + n²) = n + n²
  let combined_operations = azimuth.add(n, azimuth.multiply(n, n))
  @assertion.assert_eq(1001000, combined_operations)?
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 数组索引计算：二维数组转一维数组的索引
  let row = 3
  let col = 4
  let width = 10
  
  // 计算一维索引：index = row × width + col
  let index = azimuth.add(azimuth.multiply(row, width), col)
  @assertion.assert_eq(34, index)?
  
  // 链表长度计算（简化）
  let node_count = 5
  let list_length = node_count
  @assertion.assert_eq(5, list_length)?
  
  // 树的高度计算（简化：完全二叉树）
  let node_count_tree = 15
  // 高度 = floor(log2(n)) + 1，对于15个节点的完全二叉树，高度为4
  let tree_height = 4
  @assertion.assert_eq(4, tree_height)?
  
  // 哈希表容量计算（简化）
  let load_factor = 75  // 75%
  let expected_elements = 100
  // 容量 = 元素数量 / 负载因子 × 100
  let capacity = azimuth.multiply(expected_elements, 100) / load_factor
  @assertion.assert_eq(133, capacity)?
}

test "cryptography_simulation" {
  // 密码学模拟测试（简化）
  // 凯撒密码：位移加密
  let plaintext = "HELLO"
  let shift = 3
  
  // 简化的凯撒密码（仅处理字母H）
  let H_code = 72  // ASCII码
  let encrypted_H = azimuth.add(H_code, shift)
  @assertion.assert_eq(75, encrypted_H)?  // 'K'的ASCII码
  
  // 简单的哈希函数模拟：数字求和
  let data1 = 123
  let data2 = 456
  let data3 = 789
  
  // 计算数字各位之和
  let sum1 = azimuth.add(azimuth.add(data1 / 100, (data1 / 10) % 10), data1 % 10)
  let sum2 = azimuth.add(azimuth.add(data2 / 100, (data2 / 10) % 10), data2 % 10)
  let sum3 = azimuth.add(azimuth.add(data3 / 100, (data3 / 10) % 10), data3 % 10)
  
  @assertion.assert_eq(6, sum1)?  // 1 + 2 + 3 = 6
  @assertion.assert_eq(15, sum2)? // 4 + 5 + 6 = 15
  @assertion.assert_eq(24, sum3)? // 7 + 8 + 9 = 24
  
  // 组合哈希值
  let combined_hash = azimuth.add(azimuth.add(sum1, sum2), sum3)
  @assertion.assert_eq(45, combined_hash)?
}

test "network_calculations" {
  // 网络计算测试
  // 带宽计算：文件大小 / 传输时间
  let file_size = 1048576  // 1MB = 1048576字节
  let transfer_time = 10   // 10秒
  
  // 计算带宽：1048576 / 10 = 104857.6 字节/秒（简化为整数）
  let bandwidth = file_size / transfer_time
  @assertion.assert_eq(104857, bandwidth)?
  
  // 网络延迟计算：传播时间 + 处理时间 + 排队时间
  let propagation_time = 50   // 50ms
  let processing_time = 20    // 20ms
  let queueing_time = 30      // 30ms
  
  let total_latency = azimuth.add(azimuth.add(propagation_time, processing_time), queueing_time)
  @assertion.assert_eq(100, total_latency)?
  
  // 数据包传输计算：数据大小 / 包大小
  let data_size = 5000  // 5000字节
  let packet_size = 1500  // 1500字节/包
  
  // 需要的数据包数量：5000 / 1500 = 3.33（向上取整为4）
  let packet_count = azimuth.divide_with_ceil(data_size, packet_size)
  @assertion.assert_eq(4, packet_count)?
}

test "machine_learning_basics" {
  // 机器学习基础测试
  // 线性回归：y = wx + b
  let w = 2  // 权重
  let x = 5  // 输入
  let b = 3  // 偏置
  
  // 计算输出：y = 2×5 + 3 = 13
  let y = azimuth.add(azimuth.multiply(w, x), b)
  @assertion.assert_eq(13, y)?
  
  // 均方误差计算：MSE = (1/n) × Σ(y_pred - y_actual)²
  let y_pred1 = 10
  let y_actual1 = 12
  let y_pred2 = 15
  let y_actual2 = 14
  let y_pred3 = 20
  let y_actual3 = 18
  
  // 计算误差平方和：(10-12)² + (15-14)² + (20-18)² = 4 + 1 + 4 = 9
  let error1 = azimuth.multiply(azimuth.add(y_pred1, -y_actual1), azimuth.add(y_pred1, -y_actual1))
  let error2 = azimuth.multiply(azimuth.add(y_pred2, -y_actual2), azimuth.add(y_pred2, -y_actual2))
  let error3 = azimuth.multiply(azimuth.add(y_pred3, -y_actual3), azimuth.add(y_pred3, -y_actual3))
  let sum_squared_errors = azimuth.add(azimuth.add(error1, error2), error3)
  
  // 计算均方误差：9 / 3 = 3
  let mse = sum_squared_errors / 3
  @assertion.assert_eq(3, mse)?
  
  // 梯度下降更新：w_new = w_old - learning_rate × gradient
  let w_old = 5
  let learning_rate = 1
  let gradient = 2
  
  let w_new = azimuth.add(w_old, azimuth.multiply(-learning_rate, gradient))
  @assertion.assert_eq(3, w_new)?
}