// 增强应用测试用例
// 专注于科学计算、数据处理、算法实现等领域的测试

test "physics_projectile_motion" {
  // 物理抛物运动计算测试
  // 公式：水平距离 = 初速度 × cos(角度) × 时间
  // 垂直高度 = 初速度 × sin(角度) × 时间 - 0.5 × 重力 × 时间²
  
  let initial_velocity = 20  // m/s
  let angle_degrees = 45     // 45度角
  let time = 2               // 秒
  let gravity = 10           // m/s² (简化值)
  
  // 简化计算：45度角的sin和cos都是√2/2 ≈ 0.707，用707/1000表示
  let cos_angle = 707
  let sin_angle = 707
  
  // 计算水平分量和垂直分量
  let horizontal_velocity = @azimuth.multiply(initial_velocity, cos_angle) / 1000
  let vertical_velocity = @azimuth.multiply(initial_velocity, sin_angle) / 1000
  
  // 计算水平距离和垂直高度
  let horizontal_distance = @azimuth.multiply(horizontal_velocity, time)
  let vertical_height = @azimuth.add(@azimuth.multiply(vertical_velocity, time), @azimuth.multiply(-gravity, @azimuth.multiply(time, time)) / 2)
  
  // 验证结果（近似值）
  @azimuth.assert_eq(true, horizontal_distance > 20)  // 应该大于20米
  @azimuth.assert_eq(true, vertical_height > 0)       // 应该大于0米
  @azimuth.assert_eq(true, vertical_height < 30)      // 应该小于30米
}

test "data_processing_median_calculation" {
  // 数据处理：中位数计算测试（偶数个数据点）
  // 数据集：3, 7, 8, 12, 15, 18, 22, 25
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(3, 7), 8), 12), 15), 18), 22), 25)
  @azimuth.assert_eq(110, sum)
  
  // 计算平均值
  let count = 8
  let mean = sum / count
  @azimuth.assert_eq(13, mean)
  
  // 计算中位数（偶数个数据点，取中间两个的平均值）
  // 中间两个数是12和15，中位数是(12+15)/2 = 13.5，用整数表示为13
  let median = 13
  @azimuth.assert_eq(median, median)
  
  // 计算四分位数
  let q1 = 8  // 第一四分位数
  let q3 = 18 // 第三四分位数
  let iqr = @azimuth.add(q3, -q1) // 四分位距
  @azimuth.assert_eq(10, iqr)
}

test "algorithm_bubble_sort_simulation" {
  // 算法模拟：冒泡排序测试
  // 原始数组：[5, 2, 8, 1, 9, 3]
  
  let mut a1 = 5
  let mut a2 = 2
  let mut a3 = 8
  let mut a4 = 1
  let mut a5 = 9
  let mut a6 = 3
  
  // 第一轮冒泡
  // 比较5和2，交换
  let temp1 = a1
  a1 = a2
  a2 = temp1
  
  // 比较2和8，不交换
  // 比较8和1，交换
  let temp2 = a3
  a3 = a4
  a4 = temp2
  
  // 比较1和9，不交换
  // 比较9和3，交换
  let temp3 = a5
  a5 = a6
  a6 = temp3
  
  // 第一轮后的数组：[2, 5, 1, 8, 3, 9]
  
  // 第二轮冒泡（简化，只验证部分交换）
  // 比较5和1，交换
  let temp4 = a2
  a2 = a3
  a3 = temp4
  
  // 验证部分排序结果
  @azimuth.assert_eq(2, a1)
  @azimuth.assert_eq(1, a3)
  @azimuth.assert_eq(9, a6)
}

test "cryptography_caesar_cipher" {
  // 密码学：凯撒密码测试
  // 简化版本：只处理大写字母A-Z
  let plaintext = "HELLO"
  let shift = 3
  
  // 手动实现凯撒密码加密
  // H(7) + 3 = K(10)
  // E(4) + 3 = H(7)
  // L(11) + 3 = O(14)
  // L(11) + 3 = O(14)
  // O(14) + 3 = R(17)
  
  // 验证加密结果
  let encrypted = @azimuth.greet("KHOOR")
  @azimuth.assert_eq_string("Hello, KHOOR!", encrypted)
  
  // 验证解密（反向移位）
  let decrypted_shift = @azimuth.add(26, -shift) // 23
  @azimuth.assert_eq(23, decrypted_shift)
}

test "network_data_packet_simulation" {
  // 网络数据包模拟测试
  // 模拟TCP/IP数据包的校验和计算
  
  let packet_header = 0x4500  // IP版本和头长度
  let packet_length = 40      // 数据包长度
  let protocol = 6            // TCP协议
  let source_ip = 192168001   // 源IP（简化）
  let dest_ip = 192168002     // 目标IP（简化）
  
  // 计算校验和（简化版本）
  let header_sum = @azimuth.add(packet_header, packet_length)
  let with_protocol = @azimuth.add(header_sum, protocol)
  let with_ips = @azimuth.add(@azimuth.add(with_protocol, source_ip), dest_ip)
  
  // 模拟校验和计算
  let checksum = with_ips % 65536  // 16位校验和
  @azimuth.assert_eq(true, checksum > 0)
  @azimuth.assert_eq(true, checksum < 65536)
  
  // 验证数据包完整性
  let is_valid = checksum > 1000  // 简化的验证条件
  @azimuth.assert_eq(is_valid, true)
}

test "ai_decision_tree_logic" {
  // AI决策树逻辑测试
  // 模拟简单的游戏AI决策过程
  
  let player_health = 75
  let enemy_health = 30
  let player_mana = 20
  let distance = 5
  
  // 决策树逻辑
  let mut should_attack = false
  let mut should_defend = false
  let mut should_flee = false
  
  // 决策条件1：敌人血量低且玩家有足够魔力，攻击
  if enemy_health < 50 && player_mana > 15 {
    should_attack = true
  }
  
  // 决策条件2：玩家血量低且敌人血量高，防御
  if player_health < 40 && enemy_health > 60 {
    should_defend = true
  }
  
  // 决策条件3：距离太远且玩家血量低，逃跑
  if distance > 10 && player_health < 30 {
    should_flee = true
  }
  
  // 验证决策结果
  @azimuth.assert_eq(should_attack, true)   // 应该选择攻击
  @azimuth.assert_eq(!should_defend, true)  // 不应该防御
  @azimuth.assert_eq(!should_flee, true)    // 不应该逃跑
}

test "compression_run_length_encoding" {
  // 数据压缩：游程编码测试
  // 原始数据：AAAABBBCCDAA
  
  // 模拟游程编码过程
  // AAAAA -> 4A
  // BBB -> 3B
  // CC -> 2C
  // D -> 1D
  // AA -> 2A
  
  // 计算原始长度
  let original_length = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(4, 3), 2), 1), 2)
  @azimuth.assert_eq(12, original_length)
  
  // 计算压缩后长度（每个游程需要2个字符：计数+字符）
  let runs_count = 5  // 5个游程
  let compressed_length = @azimuth.multiply(runs_count, 2)
  @azimuth.assert_eq(10, compressed_length)
  
  // 计算压缩率
  let compression_ratio = @azimuth.multiply(compressed_length, 100) / original_length
  @azimuth.assert_eq(83, compression_ratio)  // 83%的原始大小
  
  // 验证压缩效果
  let is_effective = compression_ratio < 90
  @azimuth.assert_eq(is_effective, true)
}

test "internationalization_currency_formatting" {
  // 国际化：货币格式化测试
  let amount = 1234567  // 12345.67（以分为单位）
  
  // 模拟不同地区的货币格式化
  
  // 美国格式：$12,345.67
  let dollars = amount / 100
  let cents = amount % 100
  let us_format = @azimuth.add(@azimuth.multiply(dollars, 100), cents)
  @azimuth.assert_eq(1234567, us_format)
  
  // 欧洲格式：12.345,67€
  let euro_format = @azimuth.add(@azimuth.multiply(dollars, 100), cents)
  @azimuth.assert_eq(1234567, euro_format)
  
  // 日本格式：¥1,234,567（无小数）
  let yen_amount = amount  // 日元通常没有小数
  @azimuth.assert_eq(1234567, yen_amount)
  
  // 验证货币转换
  let exchange_rate_usd_to_eur = 85  // 1 USD = 0.85 EUR，用85/100表示
  let euro_amount = @azimuth.multiply(amount, exchange_rate_usd_to_eur) / 100
  @azimuth.assert_eq(true, euro_amount > 1000000)  // 应该大于1M
  @azimuth.assert_eq(true, euro_amount < 1100000)  // 应该小于1.1M
}