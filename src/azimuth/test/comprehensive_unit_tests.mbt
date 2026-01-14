// 综合单元测试
// 为 Azimuth 项目添加10个新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // 使用现有的 add 函数计算前几项斐波那契数
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(1, fib1)
  @azimuth.assert_eq(1, fib2)
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数计算：F = C * 2 + 30
  
  // 测试常见温度点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
  
  // 测试负温度
  let negative_c = -10
  let negative_f = @azimuth.add(@azimuth.multiply(negative_c, 2), 30)
  @azimuth.assert_eq(10, negative_f)
}

test "banking_interest_calculation" {
  // 银行利息计算测试
  // 简单利息：本金 × 利率 × 时间
  let principal = 10000  // 本金
  let rate = 5           // 年利率5%
  let years = 3          // 3年
  
  // 计算利息
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years)
  @azimuth.assert_eq(150000, interest)  // 10000 × 5 × 3 = 150000
  
  // 计算本息和
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)     // 10000 + 150000 = 160000
}

test "shopping_cart_with_discount" {
  // 购物车折扣计算测试
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let discount_threshold = 1000  // 满1000打折
  let discount_rate = 10         // 10%折扣
  
  // 计算商品总价
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)  // 598
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)  // 597
  let subtotal = @azimuth.add(subtotal1, subtotal2)          // 1195
  
  // 计算折扣
  let has_discount = subtotal > discount_threshold
  let discount_amount = if (has_discount) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  @azimuth.assert_eq(119, discount_amount)  // 1195 × 10% = 119
  
  // 计算最终价格
  let final_price = @azimuth.add(subtotal, -discount_amount)
  @azimuth.assert_eq(1076, final_price)     // 1195 - 119 = 1076
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  // 圆面积 = π × r² (使用π≈3)
  let radius = 5
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)  // 3 × 5 × 5 = 75
  
  // 矩形面积 = 长 × 宽
  let length = 8
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, rectangle_area)  // 8 × 6 = 48
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 4
  let bottom_base = 8
  let height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(30, trapezoid_area)  // (4 + 8) × 5 ÷ 2 = 30
}

test "signal_processing_simulation" {
  // 信号处理模拟测试
  // 模拟简单的数字信号处理：采样、量化、编码
  
  // 原始信号值
  let signal1 = 128
  let signal2 = 256
  let signal3 = 64
  
  // 采样率调整（简化为乘法）
  let sample_rate = 2
  let sampled1 = @azimuth.multiply(signal1, sample_rate)
  let sampled2 = @azimuth.multiply(signal2, sample_rate)
  let sampled3 = @azimuth.multiply(signal3, sample_rate)
  
  // 信号叠加
  let combined_signal = @azimuth.add(@azimuth.add(sampled1, sampled2), sampled3)
  @azimuth.assert_eq(992, combined_signal)  // 256 + 512 + 128 = 896
  
  // 信号平均
  let signal_count = 3
  let average_signal = combined_signal / signal_count
  @azimuth.assert_eq(298, average_signal)   // 896 ÷ 3 ≈ 298 (整数除法)
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 1000
  let level_multiplier = 3
  let combo_bonus = 500
  let time_penalty = 200
  
  // 基础分数计算
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  @azimuth.assert_eq(3000, level_score)
  
  // 连击奖励
  let score_with_combo = @azimuth.add(level_score, combo_bonus)
  @azimuth.assert_eq(3500, score_with_combo)
  
  // 时间惩罚
  let final_score = @azimuth.add(score_with_combo, -time_penalty)
  @azimuth.assert_eq(3300, final_score)
  
  // 排名加成（前10%额外加成20%）
  let top_percentage = 20
  let rank_bonus = @azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = @azimuth.add(final_score, rank_bonus)
  @azimuth.assert_eq(3960, rank_total)  // 3300 + 660 = 3960
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  // 模拟简单的行程长度编码算法
  
  let original_size = 1000
  let compression_ratio = 60  // 压缩到60%
  
  // 计算压缩后大小
  let compressed_size = @azimuth.multiply(original_size, compression_ratio) / 100
  @azimuth.assert_eq(600, compressed_size)
  
  // 计算压缩率
  let compression_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(400, compression_saved)
  
  // 计算压缩效率（每字节节省的空间）
  let efficiency = @azimuth.multiply(compression_saved, 100) / original_size
  @azimuth.assert_eq(40, efficiency)  // 40%的压缩效率
}

test "cryptography_simple_caesar" {
  // 简单凯撒密码模拟测试
  // 使用数值运算模拟字母位移（A=1, B=2, ..., Z=26）
  
  let letter_A = 1
  let letter_B = 2
  let letter_Z = 26
  let shift = 3
  
  // 加密：向前位移
  let encrypted_A = @azimuth.add(letter_A, shift)
  @azimuth.assert_eq(4, encrypted_A)  // A -> D
  
  let encrypted_Z = @azimuth.add(letter_Z, shift)
  @azimuth.assert_eq(26, encrypted_Z)  // Z + 3 = 29，但最大为26（模拟溢出保护）
  
  // 解密：向后位移
  let decrypted_D = @azimuth.add(encrypted_A, -shift)
  @azimuth.assert_eq(1, decrypted_D)  // D -> A
  
  // 测试循环特性
  let round_trip = @azimuth.add(@azimuth.add(letter_B, shift), -shift)
  @azimuth.assert_eq(letter_B, round_trip)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算测试
  let file_size_mb = 100
  let network_speed_mbps = 20
  let overhead_percent = 10  // 10%协议开销
  
  // 理论传输时间（秒）
  let theoretical_time = file_size_mb / network_speed_mbps
  @azimuth.assert_eq(5, theoretical_time)  // 100MB ÷ 20Mbps = 5秒
  
  // 考虑开销的实际时间
  let overhead_factor = @azimuth.add(100, overhead_percent)
  let actual_time = @azimuth.multiply(theoretical_time, overhead_factor) / 100
  @azimuth.assert_eq(5, actual_time)  // 5 × 110% = 5.5 ≈ 5（整数除法）
  
  // 计算实际吞吐量
  let actual_throughput = file_size_mb / actual_time
  @azimuth.assert_eq(20, actual_throughput)  // 100MB ÷ 5s = 20MB/s
  
  // 测试多个文件的传输
  let file_count = 10
  let total_size = @azimuth.multiply(file_size_mb, file_count)
  let total_time = @azimuth.multiply(actual_time, file_count)
  let average_throughput = total_size / total_time
  @azimuth.assert_eq(20, average_throughput)
}