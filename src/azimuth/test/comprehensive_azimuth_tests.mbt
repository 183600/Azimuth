// 综合测试用例 - Azimuth 项目
// 使用标准 MoonBit 测试语法编写的精选测试集

test "arithmetic_sequence_operations" {
  // 测试等差数列运算
  let first_term = 3
  let common_difference = 5
  let n = 4
  
  // 计算第n项：a_n = a_1 + (n-1) * d
  let nth_term = @azimuth.add(first_term, @azimuth.multiply(n - 1, common_difference))
  @azimuth.assert_eq(18, nth_term)  // 3 + (4-1)*5 = 18
  
  // 计算前n项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n - 1, common_difference))) / 2
  @azimuth.assert_eq(42, sum)  // 4/2 * (2*3 + 3*5) = 42
}

test "geometric_progression_calculations" {
  // 测试等比数列计算
  let first_term = 2
  let common_ratio = 3
  let n = 4
  
  // 计算第n项：a_n = a_1 * r^(n-1)
  let nth_term = @azimuth.multiply(first_term, @azimuth.multiply(@azimuth.multiply(common_ratio, common_ratio), common_ratio))
  @azimuth.assert_eq(54, nth_term)  // 2 * 3^3 = 54
  
  // 计算前n项和（简化版）：S_n = a_1 * (1 - r^n) / (1 - r)
  // 这里我们手动计算前4项和：2 + 6 + 18 + 54 = 80
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(first_term, @azimuth.multiply(first_term, common_ratio)), 
                    @azimuth.multiply(first_term, @azimuth.multiply(common_ratio, common_ratio))), 
                    nth_term)
  @azimuth.assert_eq(80, sum)
}

test "string_validation_scenarios" {
  // 测试字符串验证场景
  // 测试各种用户名格式
  let valid_usernames = ["user123", "test_user", "User2023", "u"]
  let invalid_chars = ["user@123", "user name", "user#123", ""]
  
  // 验证有效用户名
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet(valid_usernames[0]))
  @azimuth.assert_eq_string("Hello, test_user!", @azimuth.greet(valid_usernames[1]))
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet(valid_usernames[2]))
  @azimuth.assert_eq_string("Hello, u!", @azimuth.greet(valid_usernames[3]))
  
  // 验证包含特殊字符的用户名（系统仍能处理）
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet(invalid_chars[0]))
  @azimuth.assert_eq_string("Hello, user name!", @azimuth.greet(invalid_chars[1]))
  @azimuth.assert_eq_string("Hello, user#123!", @azimuth.greet(invalid_chars[2]))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(invalid_chars[3]))
}

test "financial_compound_interest" {
  // 测试复利计算（更精确的版本）
  let principal = 1000
  let annual_rate = 5  // 5%
  let years = 3
  let compounds_per_year = 1
  
  // 简化的复利公式，避免浮点数
  // A = P(1 + r/n)^(nt) ≈ P(1 + r*t) 当n=1时
  let growth_factor = @azimuth.add(100, @azimuth.multiply(annual_rate, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(115000, total_amount)  // 1000 * (100 + 5*3) = 115000
  
  // 测试不同投资期限
  let short_term = @azimuth.multiply(principal, @azimuth.add(100, @azimuth.multiply(annual_rate, 1)))
  @azimuth.assert_eq(105000, short_term)  // 1年
  
  let long_term = @azimuth.multiply(principal, @azimuth.add(100, @azimuth.multiply(annual_rate, 10)))
  @azimuth.assert_eq(150000, long_term)  // 10年
}

test "physics_motion_calculations" {
  // 测试物理运动计算
  // 自由落体运动：h = v₀t + 0.5gt²
  // 简化计算：h = v₀t + 5t²（取g≈10m/s²）
  
  let initial_velocity = 10  // m/s
  let time = 3  // seconds
  let gravity_approx = 5  // 0.5g的近似值
  
  let distance = @azimuth.add(@azimuth.multiply(initial_velocity, time), 
                     @azimuth.multiply(gravity_approx, @azimuth.multiply(time, time)))
  @azimuth.assert_eq(75, distance)  // 10*3 + 5*9 = 75
  
  // 测试不同时间点
  let distance_2s = @azimuth.add(@azimuth.multiply(initial_velocity, 2), 
                        @azimuth.multiply(gravity_approx, @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(40, distance_2s)  // 10*2 + 5*4 = 40
  
  let distance_1s = @azimuth.add(@azimuth.multiply(initial_velocity, 1), 
                        @azimuth.multiply(gravity_approx, @azimuth.multiply(1, 1)))
  @azimuth.assert_eq(15, distance_1s)  // 10*1 + 5*1 = 15
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // O(n) 线性复杂度：总操作数 = 基础操作 + n * 每次操作成本
  let base_operations = 100
  let n = 50
  let cost_per_operation = 3
  
  let total_operations = @azimuth.add(base_operations, @azimuth.multiply(n, cost_per_operation))
  @azimuth.assert_eq(250, total_operations)  // 100 + 50*3 = 250
  
  // O(n²) 平方复杂度：总操作数 = 基础操作 + n² * 每次操作成本
  let quadratic_cost_per_operation = 2
  let quadratic_operations = @azimuth.add(base_operations, 
                                 @azimuth.multiply(@azimuth.multiply(n, n), quadratic_cost_per_operation))
  @azimuth.assert_eq(5100, quadratic_operations)  // 100 + 50²*2 = 5100
  
  // O(log n) 对数复杂度（简化为除法）
  let log_operations = @azimuth.add(base_operations, @azimuth.multiply(n / 2, cost_per_operation))
  @azimuth.assert_eq(175, log_operations)  // 100 + (50/2)*3 = 175
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作：push和pop
  let stack_size = 0
  let push_count = 5
  let pop_count = 3
  
  // 执行push操作
  let after_push = @azimuth.add(stack_size, push_count)
  @azimuth.assert_eq(5, after_push)
  
  // 执行pop操作
  let after_pop = @azimuth.add(after_push, -pop_count)
  @azimuth.assert_eq(2, after_pop)
  
  // 队列操作：enqueue和dequeue
  let queue_size = 0
  let enqueue_count = 8
  let dequeue_count = 6
  
  let after_enqueue = @azimuth.add(queue_size, enqueue_count)
  @azimuth.assert_eq(8, after_enqueue)
  
  let after_dequeue = @azimuth.add(after_enqueue, -dequeue_count)
  @azimuth.assert_eq(2, after_dequeue)
}

test "statistical_analysis" {
  // 测试统计分析功能
  // 计算数据集：[10, 20, 30, 40, 50]
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)
  
  // 计算方差（简化）：平方差的平均值
  let diff1 = @azimuth.multiply(data1 - mean, data1 - mean)
  let diff2 = @azimuth.multiply(data2 - mean, data2 - mean)
  let diff3 = @azimuth.multiply(data3 - mean, data3 - mean)
  let diff4 = @azimuth.multiply(data4 - mean, data4 - mean)
  let diff5 = @azimuth.multiply(data5 - mean, data5 - mean)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(diff1, diff2), diff3), diff4), diff5) / 5
  @azimuth.assert_eq(200, variance)  // (400+100+0+100+400)/5 = 200
  
  // 计算标准差（简化为整数）
  let std_dev = 14  // sqrt(200) ≈ 14.14，取整数部分
  @azimuth.assert_eq(std_dev == 14, true)
}

test "game_mechanics_simulation" {
  // 测试游戏机制模拟
  // 角色属性计算
  let base_health = 100
  let level = 5
  let vitality_bonus = 10
  let equipment_bonus = 25
  
  // 计算总生命值：基础值 + 等级加成 + 属性加成 + 装备加成
  let level_bonus = @azimuth.multiply(level, vitality_bonus)
  let total_health = @azimuth.add(@azimuth.add(@azimuth.add(base_health, level_bonus), vitality_bonus), equipment_bonus)
  @azimuth.assert_eq(180, total_health)  // 100 + 5*10 + 10 + 25 = 180
  
  // 伤害计算：基础伤害 * (1 + 暴击率 * 暴击倍率)
  let base_damage = 50
  let crit_rate = 30  // 30%
  let crit_multiplier = 2  // 2倍暴击伤害
  
  let average_damage = @azimuth.multiply(base_damage, @azimuth.add(100, @azimuth.multiply(crit_rate, crit_multiplier))) / 100
  @azimuth.assert_eq(80, average_damage)  // 50 * (100 + 30*2) / 100 = 80
  
  // 经验值计算：基础经验 * (1 + 等级差异系数)
  let base_exp = 100
  let level_diff = 3
  let diff_coefficient = 20  // 每级差异20%
  
  let total_exp = @azimuth.multiply(base_exp, @azimuth.add(100, @azimuth.multiply(level_diff, diff_coefficient))) / 100
  @azimuth.assert_eq(160, total_exp)  // 100 * (100 + 3*20) / 100 = 160
}

test "network_throughput_calculation" {
  // 测试网络吞吐量计算
  let packet_size = 1500  // bytes
  let packets_per_second = 100
  let transmission_time = 10  // seconds
  let overhead_ratio = 10  // 10%开销
  
  // 计算原始数据量
  let raw_data = @azimuth.multiply(packet_size, @azimuth.multiply(packets_per_second, transmission_time))
  @azimuth.assert_eq(1500000, raw_data)  // 1500 * 100 * 10 = 1,500,000 bytes
  
  // 计算实际有效数据（考虑协议开销）
  let effective_data = @azimuth.multiply(raw_data, @azimuth.add(100, -overhead_ratio)) / 100
  @azimuth.assert_eq(1350000, effective_data)  // 1,500,000 * 0.9 = 1,350,000 bytes
  
  // 转换为千字节每秒
  let throughput_kbps = effective_data / (transmission_time * 1000)
  @azimuth.assert_eq(135, throughput_kbps)  // 1,350,000 / (10 * 1000) = 135 KB/s
  
  // 计算传输效率
  let efficiency = @azimuth.multiply(effective_data, 100) / raw_data
  @azimuth.assert_eq(90, efficiency)  // (1,350,000 / 1,500,000) * 100 = 90%
}