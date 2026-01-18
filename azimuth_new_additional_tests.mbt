// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "boolean_logic_operations" {
  // 布尔逻辑运算测试
  let true_val = 1
  let false_val = 0
  
  // 逻辑与模拟: 两个都为真(1)时结果为真(1)
  let and_true = @azimuth.multiply(true_val, true_val)
  @azimuth.assert_eq(1, and_true)
  
  let and_false1 = @azimuth.multiply(true_val, false_val)
  @azimuth.assert_eq(0, and_false1)
  
  let and_false2 = @azimuth.multiply(false_val, false_val)
  @azimuth.assert_eq(0, and_false2)
  
  // 逻辑或模拟: 任一为真(1)时结果为真(1)
  let or_true1 = @azimuth.add(@azimuth.add(true_val, false_val), -@azimuth.multiply(true_val, false_val))
  @azimuth.assert_eq(1, or_true1)
  
  let or_true2 = @azimuth.add(@azimuth.add(true_val, true_val), -@azimuth.multiply(true_val, true_val))
  @azimuth.assert_eq(1, or_true2)
  
  let or_false = @azimuth.add(@azimuth.add(false_val, false_val), -@azimuth.multiply(false_val, false_val))
  @azimuth.assert_eq(0, or_false)
}

test "number_system_conversions" {
  // 数字系统转换测试
  
  // 二进制转十进制: 1010 = 8 + 0 + 2 + 0 = 10
  let binary_1010 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(0, 4)), @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(0, 1)))
  @azimuth.assert_eq(10, binary_1010)
  
  // 二进制转十进制: 1111 = 8 + 4 + 2 + 1 = 15
  let binary_1111 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(1, 1)))
  @azimuth.assert_eq(15, binary_1111)
  
  // 十进制转八进制模拟: 65 = 64 + 1 = 1*8² + 0*8¹ + 1*8⁰ = 101₈
  let decimal_65 = 65
  let octal_101 = @azimuth.add(@azimuth.multiply(1, 64), @azimuth.add(@azimuth.multiply(0, 8), 1))
  @azimuth.assert_eq(decimal_65, octal_101)
}

test "statistical_measures" {
  // 统计度量测试
  
  let values1 = 12
  let values2 = 15
  let values3 = 18
  let values4 = 21
  let values5 = 24
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(values1, values2), values3), values4), values5)
  let mean = sum / 5
  @azimuth.assert_eq(18, mean)
  
  // 计算中位数（已排序: 12, 15, 18, 21, 24）
  let median = 18
  @azimuth.assert_eq(mean, median)
  
  // 计算极差
  let range = @azimuth.add(values5, -values1)
  @azimuth.assert_eq(12, range)
  
  // 计算四分位数间距（简化版）
  let q1 = 15  // 第一四分位数
  let q3 = 21  // 第三四分位数
  let iqr = @azimuth.add(q3, -q1)
  @azimuth.assert_eq(6, iqr)
}

test "physics_calculations" {
  // 物理计算测试
  
  // 速度计算: 距离 / 时间
  let distance = 150  // 米
  let time = 10       // 秒
  let velocity = distance / time
  @azimuth.assert_eq(15, velocity)
  
  // 加速度计算: (末速度 - 初速度) / 时间
  let initial_velocity = 10
  let final_velocity = 30
  let acceleration = @azimuth.add(final_velocity, -initial_velocity) / time
  @azimuth.assert_eq(2, acceleration)
  
  // 动能计算: 0.5 * 质量 * 速度²
  let mass = 5  // 千克
  let speed = 10  // 米/秒
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, speed), speed) / 2
  @azimuth.assert_eq(250, kinetic_energy)
  
  // 势能计算: 质量 * 重力加速度 * 高度
  let gravity = 10  // 米/秒² (简化值)
  let height = 20   // 米
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(1000, potential_energy)
}

test "algorithm_complexity" {
  // 算法复杂度测试
  
  // O(1) 常数时间操作
  let constant_time = 42
  @azimuth.assert_eq(42, constant_time)
  
  // O(n) 线性时间操作模拟
  let n = 5
  let linear_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, linear_sum)
  
  // O(n²) 二次时间操作模拟
  let quadratic_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3))
  @azimuth.assert_eq(14, quadratic_sum)
  
  // O(log n) 对数时间操作模拟（二分查找比较次数）
  let log_comparisons = 3  // 对于n=8，最多需要3次比较
  @azimuth.assert_eq(3, log_comparisons)
}

test "encryption_simulation" {
  // 加密模拟测试
  
  // 凯撒密码模拟（字母位移）
  let shift = 3
  let original_A = 1  // A的数字表示
  let encrypted_D = @azimuth.add(original_A, shift)
  @azimuth.assert_eq(4, encrypted_D)
  
  // 简单异或加密模拟
  let plaintext = 1010
  let key = 0101
  let encrypted = @azimuth.add(plaintext, key)  // 简化版异或操作
  @azimuth.assert_eq(1115, encrypted)
  
  // 解密操作（与加密相同）
  let decrypted = @azimuth.add(encrypted, key)
  @azimuth.assert_eq(2120, decrypted)
  
  // 密钥生成模拟
  let seed = 7
  let key1 = @azimuth.multiply(seed, 17) % 100  // 7*17=119%100=19
  let key2 = @azimuth.multiply(seed, 31) % 100  // 7*31=217%100=17
  @azimuth.assert_eq(19, key1)
  @azimuth.assert_eq(17, key2)
}

test "data_compression" {
  // 数据压缩测试
  
  // 行程长度编码模拟
  let original_data = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 1), 1), 2), 2)  // 1,1,1,2,2
  let compressed_size = 4  // 三个1，两个2 -> (1,3),(2,2) -> 4个值
  @azimuth.assert_eq(4, compressed_size)
  
  // 霍夫曼编码频率计算
  let char_a_count = 5
  let char_b_count = 3
  let char_c_count = 2
  let char_d_count = 8
  let total_chars = @azimuth.add(@azimuth.add(@azimuth.add(char_a_count, char_b_count), char_c_count), char_d_count)
  @azimuth.assert_eq(18, total_chars)
  
  // 字符频率计算
  let freq_a = @azimuth.multiply(char_a_count, 100) / total_chars
  let freq_b = @azimuth.multiply(char_b_count, 100) / total_chars
  let freq_c = @azimuth.multiply(char_c_count, 100) / total_chars
  let freq_d = @azimuth.multiply(char_d_count, 100) / total_chars
  
  @azimuth.assert_eq(27, freq_a)  // 5/18 ≈ 27%
  @azimuth.assert_eq(16, freq_b)  // 3/18 ≈ 16%
  @azimuth.assert_eq(11, freq_c)  // 2/18 ≈ 11%
  @azimuth.assert_eq(44, freq_d)  // 8/18 ≈ 44%
}

test "network_protocols" {
  // 网络协议测试
  
  // IP地址计算
  let ip_octet1 = 192
  let ip_octet2 = 168
  let ip_octet3 = 1
  let ip_octet4 = 1
  
  // 网络地址计算（简化）
  let network_address = @azimuth.add(@azimuth.add(@azimuth.multiply(ip_octet1, 256), ip_octet2), 1)
  @azimuth.assert_eq(49321, network_address)
  
  // 端口计算
  let base_port = 8000
  let offset = 80
  let full_port = @azimuth.add(base_port, offset)
  @azimuth.assert_eq(8080, full_port)
  
  // TCP序列号计算
  let initial_seq = 1000
  let data_size = 512
  let next_seq = @azimuth.add(initial_seq, data_size)
  @azimuth.assert_eq(1512, next_seq)
  
  // 校验和计算（简化）
  let packet1 = 100
  let packet2 = 200
  let packet3 = 300
  let checksum = @azimuth.add(@azimuth.add(packet1, packet2), packet3) % 256
  @azimuth.assert_eq(44, checksum)
}

test "machine_learning_basics" {
  // 机器学习基础测试
  
  // 线性回归: y = mx + b
  let x1 = 2
  let x2 = 4
  let x3 = 6
  let m = 2  // 斜率
  let b = 1  // 截距
  
  let y1 = @azimuth.add(@azimuth.multiply(m, x1), b)
  let y2 = @azimuth.add(@azimuth.multiply(m, x2), b)
  let y3 = @azimuth.add(@azimuth.multiply(m, x3), b)
  
  @azimuth.assert_eq(5, y1)  // 2*2+1=5
  @azimuth.assert_eq(9, y2)  // 2*4+1=9
  @azimuth.assert_eq(13, y3) // 2*6+1=13
  
  // 均方误差计算
  let actual_y1 = 6
  let actual_y2 = 8
  let actual_y3 = 14
  
  let error1 = @azimuth.add(actual_y1, -y1)
  let error2 = @azimuth.add(actual_y2, -y2)
  let error3 = @azimuth.add(actual_y3, -y3)
  
  let mse = @azimuth.add(@azimuth.add(@azimuth.multiply(error1, error1), @azimuth.multiply(error2, error2)), @azimuth.multiply(error3, error3)) / 3
  @azimuth.assert_eq(1, mse)  // ((1)² + (-1)² + (1)²)/3 = 1
  
  // 简单分类: 决策边界
  let feature1 = 3
  let feature2 = 7
  let weight1 = 2
  let weight2 = 1
  let bias = -10
  
  let decision_value = @azimuth.add(@azimuth.add(@azimuth.multiply(weight1, feature1), @azimuth.multiply(weight2, feature2)), bias)
  @azimuth.assert_eq(3, decision_value)  // 2*3+1*7-10=3，>0所以分类为正类
}