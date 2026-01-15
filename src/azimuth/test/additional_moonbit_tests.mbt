// Azimuth 项目附加 MoonBit 测试用例集
// 包含10个新的测试用例，使用标准 MoonBit 测试语法

test "number_sequence_operations" {
  // 测试数列运算：计算等差数列和等比数列
  let first_term = 3
  let common_diff = 5
  let terms = 6
  
  // 计算等差数列第n项：a_n = a_1 + (n-1)×d
  let nth_term = @azimuth.add(first_term, @azimuth.multiply(common_diff, terms - 1))
  @azimuth.assert_eq(28, nth_term)  // 3 + 5×5 = 28
  
  // 计算等差数列前n项和：S_n = n×(a_1 + a_n)/2
  let sum = @azimuth.multiply(terms, @azimuth.add(first_term, nth_term)) / 2
  @azimuth.assert_eq(93, sum)  // 6×(3+28)/2 = 93
}

test "financial_calculation_compound" {
  // 测试金融计算：复利和贷款计算
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 简化复利计算：本息和 = 本金 × (1 + 利率×年数)
  let total_amount = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(annual_rate, years)))
  @azimuth.assert_eq(250000, total_amount)  // 10000 × (1 + 8×3) = 10000 × 25 = 250000
  
  // 计算月还款额（简化版）：总金额 ÷ (年数×12)
  let monthly_payment = total_amount / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(6944, monthly_payment)  // 250000 ÷ 36 ≈ 6944
}

test "physics_kinematics_calculations" {
  // 测试物理运动学计算
  let initial_velocity = 10
  let acceleration = 5
  let time = 4
  
  // 计算末速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(30, final_velocity)  // 10 + 5×4 = 30
  
  // 计算位移：s = ut + 0.5at²（简化为整数：s = ut + at²/2）
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), 
    @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2)
  @azimuth.assert_eq(80, displacement)  // 10×4 + 5×16/2 = 40 + 40 = 80
}

test "data_structure_array_operations" {
  // 测试数据结构：数组操作模拟
  let array_size = 10
  let element_size = 4
  
  // 计算数组总大小
  let total_size = @azimuth.multiply(array_size, element_size)
  @azimuth.assert_eq(40, total_size)
  
  // 计算数组索引：row × width + col
  let row = 3
  let col = 2
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(17, index)  // 3×5 + 2 = 17
  
  // 计算内存偏移量：index × element_size
  let offset = @azimuth.multiply(index, element_size)
  @azimuth.assert_eq(68, offset)  // 17×4 = 68
}

test "game_logic_score_system" {
  // 测试游戏逻辑：得分系统
  let base_score = 1000
  let level_multiplier = 4
  let combo_bonus = 500
  let time_penalty = 200
  let difficulty_bonus = 300
  
  // 计算基础得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  
  // 计算最终得分
  let final_score = @azimuth.add(@azimuth.add(@azimuth.add(level_score, combo_bonus), 
    difficulty_bonus), -time_penalty)
  
  @azimuth.assert_eq(4600, final_score)  // 1000×4 + 500 + 300 - 200 = 4600
  
  // 计算排名得分（前20%额外加分）
  let top_percentage = 20
  let rank_bonus = @azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = @azimuth.add(final_score, rank_bonus)
  
  @azimuth.assert_eq(5520, rank_total)  // 4600 + 4600×20/100 = 4600 + 920 = 5520
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  let username = "Player"
  let level = 25
  let score = 9850
  
  // 构建状态消息
  let level_str = "25"
  let score_str = "9850"
  let status_prefix = @azimuth.greet(username)
  let status_message = status_prefix + " You are at level " + level_str + " with score " + score_str + "!"
  
  @azimuth.assert_eq_string("Hello, Player! You are at level 25 with score 9850!", status_message)
  
  // 测试特殊字符处理
  let special_name = "Test@User#123"
  let special_greeting = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, Test@User#123!", special_greeting)
  
  // 测试Unicode字符
  let unicode_name = "游戏玩家"
  let unicode_greeting = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 游戏玩家!", unicode_greeting)
}

test "algorithm_binary_search_simulation" {
  // 测试算法：二分查找模拟
  let array_size = 100
  let target = 75
  
  // 模拟二分查找的步骤计算
  let low = 0
  let high = array_size - 1
  let mid = @azimuth.add(low, high) / 2
  
  // 第一次查找
  @azimuth.assert_eq(49, mid)  // (0 + 99) / 2 = 49
  
  // 调整查找范围
  let new_low = @azimuth.add(mid, 1)
  let new_mid = @azimuth.add(new_low, high) / 2
  @azimuth.assert_eq(74, new_mid)  // (50 + 99) / 2 = 74
  
  // 再次调整
  let final_low = @azimuth.add(new_mid, 1)
  let final_mid = @azimuth.add(final_low, high) / 2
  @azimuth.assert_eq(87, final_mid)  // (75 + 99) / 2 = 87
  
  // 查找步数统计
  let search_steps = 3
  @azimuth.assert_eq(3, search_steps)
}

test "cryptography_simple_hashing" {
  // 测试密码学：简单哈希函数模拟
  let input = "Hello"
  let prime1 = 31
  let prime2 = 37
  
  // 简单哈希算法：逐字符处理
  // H = (((H * A) + B) * C + D) ...
  let hash = @azimuth.add(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(72, prime1), 101), prime2), 111)
  @azimuth.assert_eq(8350663, hash)
  
  // 测试不同输入的哈希值
  let input2 = "World"
  let hash2 = @azimuth.add(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(87, prime1), 111), prime2), 108)
  @azimuth.assert_eq(12493938, hash2)
  
  // 验证哈希值不同
  @azimuth.assert_true(hash != hash2)
}

test "network_data_packet_simulation" {
  // 测试网络：数据包模拟
  let header_size = 20
  let payload_size = 1024
  let checksum_size = 4
  
  // 计算数据包总大小
  let packet_size = @azimuth.add(@azimuth.add(header_size, payload_size), checksum_size)
  @azimuth.assert_eq(1048, packet_size)
  
  // 计算传输时间（简化）：数据包大小 ÷ 带宽
  let bandwidth = 100  // 100 bytes/ms
  let transmission_time = packet_size / bandwidth
  @azimuth.assert_eq(10, transmission_time)  // 1048 ÷ 100 ≈ 10
  
  // 计算网络延迟下的总时间
  let latency = 50  // 50ms
  let total_time = @azimuth.add(transmission_time, latency)
  @azimuth.assert_eq(60, total_time)
}

test "optimization_greedy_algorithm" {
  // 测试优化：贪心算法模拟（背包问题简化版）
  let item1_value = 60
  let item1_weight = 10
  let item2_value = 100
  let item2_weight = 20
  let item3_value = 120
  let item3_weight = 30
  let capacity = 50
  
  // 计算价值重量比
  let ratio1 = item1_value / item1_weight  // 6
  let ratio2 = item2_value / item2_weight  // 5
  let ratio3 = item3_value / item3_weight  // 4
  
  // 按价值重量比排序：item1 > item2 > item3
  // 贪心选择
  let mut remaining_capacity = capacity
  let mut total_value = 0
  
  // 选择item1
  total_value = @azimuth.add(total_value, item1_value)
  remaining_capacity = @azimuth.add(remaining_capacity, -item1_weight)
  
  // 选择item2
  total_value = @azimuth.add(total_value, item2_value)
  remaining_capacity = @azimuth.add(remaining_capacity, -item2_weight)
  
  // 选择item3（部分）
  let fraction = remaining_capacity / item3_weight  // 20/30 = 0（整数除法）
  total_value = @azimuth.add(total_value, @azimuth.multiply(item3_value, fraction))
  
  @azimuth.assert_eq(160, total_value)  // 60 + 100 + 0 = 160
}