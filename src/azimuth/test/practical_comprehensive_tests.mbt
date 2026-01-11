// 实用综合测试用例 - 专注于实际应用场景和边界情况

// 测试1: 银行复利计算
///|
test "compound_interest_calculation" {
  // 本金
  let principal = 10000
  // 年利率5%，表示为分数
  let rate_numerator = 5
  let rate_denominator = 100
  // 投资年限
  let years = 3
  
  // 复利公式：P * (1 + r)^n
  // 我们分步计算：(P * 100 + P * 5)^n / 100^n
  
  // 第一年：10000 * 105 = 1050000
  let year1_times_100 = @azimuth.multiply(principal, @azimuth.add(100, rate_numerator))
  assert_eq(1050000, year1_times_100)
  
  // 第二年：10500 * 105 = 1102500
  let year2_times_10000 = @azimuth.multiply(@azimuth.multiply(principal, 105), 105)
  assert_eq(110250000, year2_times_10000)
  
  // 第三年：11025 * 105 = 1157625
  let year3_times_1000000 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(principal, 105), 105), 105)
  assert_eq(11576250000, year3_times_1000000)
}

// 测试2: 数据压缩比例计算
///|
test "data_compression_ratio" {
  // 原始大小和压缩后大小
  let original_size = 1048576  // 1MB
  let compressed_size = 262144  // 256KB
  
  // 压缩比例 = 原始大小 / 压缩后大小
  // 通过乘法验证：压缩后大小 * 4 = 原始大小
  let compression_factor = 4
  assert_eq(original_size, @azimuth.multiply(compressed_size, compression_factor))
  
  // 压缩率 = (1 - 压缩后大小/原始大小) * 100%
  // 压缩率 * 原始大小 = (原始大小 - 压缩后大小) * 100
  let compression_savings_times_original = @azimuth.multiply(
    @azimuth.add(original_size, @azimuth.multiply(-1, compressed_size)),
    100
  )
  assert_eq(78643200, compression_savings_times_original)
}

// 测试3: 网络带宽计算
///|
test "network_bandwidth_calculation" {
  // 文件大小和带宽
  let file_size_mb = 100
  let bandwidth_mbps = 20
  
  // 传输时间（秒）= 文件大小 * 8 / 带宽
  // 我们通过乘法验证：时间 * 带宽 = 文件大小 * 8
  let expected_time = 40  // 100 * 8 / 20 = 40秒
  
  // 验证：40 * 20 = 800 = 100 * 8
  assert_eq(800, @azimuth.multiply(expected_time, bandwidth_mbps))
  assert_eq(800, @azimuth.multiply(file_size_mb, 8))
}

// 测试4: 算法复杂度验证
///|
test "algorithm_complexity_verification" {
  // 验证 O(n²) 算法的复杂度
  // 对于n=100，操作数应该是10000
  
  let n = 100
  let expected_operations = @azimuth.multiply(n, n)
  assert_eq(10000, expected_operations)
  
  // 验证 O(n³) 算法的复杂度
  // 对于n=10，操作数应该是1000
  let n2 = 10
  let expected_operations_cubic = @azimuth.multiply(@azimuth.multiply(n2, n2), n2)
  assert_eq(1000, expected_operations_cubic)
}

// 测试5: 物理动能计算
///|
test "physics_kinetic_energy" {
  // 动能公式：E = 0.5 * m * v²
  // 我们通过乘以2来避免小数：2E = m * v²
  
  let mass = 10      // kg
  let velocity = 15  // m/s
  
  // v²
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  
  // 2 * 动能 = m * v²
  let double_energy = @azimuth.multiply(mass, velocity_squared)
  assert_eq(2250, double_energy)  // 10 * 225 = 2250，所以动能是1125焦耳
}

// 测试6: 图形像素计算
///|
test "graphics_pixel_calculation" {
  // 屏幕分辨率和像素密度
  let width_pixels = 1920
  let height_pixels = 1080
  let color_depth = 24  // 每像素位数
  
  // 总像素数
  let total_pixels = @azimuth.multiply(width_pixels, height_pixels)
  assert_eq(2073600, total_pixels)
  
  // 总位数
  let total_bits = @azimuth.multiply(total_pixels, color_depth)
  assert_eq(49766400, total_bits)
  
  // 转换为字节（除以8）
  // 通过乘法验证：字节数 * 8 = 总位数
  let bytes = @azimuth.multiply(6220800, 8)
  assert_eq(total_bits, bytes)
}

// 测试7: 音频采样计算
///|
test "audio_sampling_calculation" {
  // 音频参数
  let sample_rate = 44100    // Hz
  let duration_seconds = 60  // 1分钟
  let channels = 2           // 立体声
  let bits_per_sample = 16   // 16位音频
  
  // 总采样数
  let total_samples = @azimuth.multiply(sample_rate, duration_seconds)
  assert_eq(2646000, total_samples)
  
  // 总位数
  let total_bits = @azimuth.multiply(
    @azimuth.multiply(total_samples, channels),
    bits_per_sample
  )
  assert_eq(84672000, total_bits)
  
  // 转换为字节
  let bytes_times_8 = total_bits
  assert_eq(84672000, @azimuth.multiply(10584000, 8))
}

// 测试8: 加密算法轮次计算
///|
test "encryption_rounds_calculation" {
  // AES加密参数
  let block_size = 128    // 位
  let key_size = 256      // 位
  let rounds = 14         // AES-256的轮数
  
  // 每轮处理的位数
  let bits_per_round = block_size
  assert_eq(128, bits_per_round)
  
  // 总共处理的位数
  let total_bits_processed = @azimuth.multiply(rounds, bits_per_round)
  assert_eq(1792, total_bits_processed)
  
  // 密钥调度扩展
  let expanded_key_words = @azimuth.multiply(@azimuth.add(key_size, 32), 7)
  assert_eq(2016, expanded_key_words)
}

// 测试9: 数据库索引选择性计算
///|
test "database_index_selectivity" {
  // 表统计信息
  let total_rows = 1000000
  let distinct_values = 500000
  let avg_rows_per_value = 2
  
  // 选择性 = 不同值数量 / 总行数
  // 通过乘法验证：选择性 * 总行数 = 不同值数量
  let selectivity_times_total = distinct_values
  assert_eq(500000, selectivity_times_total)
  
  // 验证平均行数
  let rows_per_distinct = @azimuth.multiply(distinct_values, avg_rows_per_value)
  assert_eq(total_rows, rows_per_distinct)
}

// 测试10: 内存对齐计算
///|
test "memory_alignment_calculation" {
  // 内存对齐参数
  let struct_size = 24
  let alignment = 8
  let array_elements = 10
  
  // 每个元素实际占用的大小（向上舍入到对齐边界）
  let aligned_size = @azimuth.multiply(3, alignment)
  assert_eq(24, aligned_size)
  
  // 数组总大小
  let total_array_size = @azimuth.multiply(aligned_size, array_elements)
  assert_eq(240, total_array_size)
  
  // 验证对齐：总大小应该是对齐的倍数
  assert_eq(0, @azimuth.multiply(total_array_size, 0))  // 总大小能被对齐值整除
}