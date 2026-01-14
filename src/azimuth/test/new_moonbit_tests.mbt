// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的各种功能

test "binary_operations_simulation" {
  // 模拟二进制运算：使用加减乘除实现位运算效果
  let a = 16
  let b = 5
  
  // 左移运算模拟：a << 1 = a * 2
  let left_shift = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(32, left_shift)
  
  // 右移运算模拟：a >> 1 = a / 2
  let right_shift = a / 2
  @azimuth.assert_eq(8, right_shift)
  
  // 模按位与运算（简化）：a & b = min(a, b) 当某些条件满足时
  let bitwise_and_sim = @azimuth.multiply(a / b, b)
  @azimuth.assert_eq(0, bitwise_and_sim)
}

test "currency_conversion_practical" {
  // 实际货币转换场景
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加手续费 5%
  let fee = @azimuth.multiply(cny_amount, 5) / 100
  let total_with_fee = @azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(735, total_with_fee)
}

test "energy_consumption_calculation" {
  // 能耗计算测试
  let power_rating = 100  // 瓦特
  let hours_used = 24
  let days_used = 30
  let cost_per_kwh = 10  // 每千瓦时费用（简化）
  
  // 计算总能耗（瓦时）
  let daily_consumption = @azimuth.multiply(power_rating, hours_used)
  let monthly_consumption = @azimuth.multiply(daily_consumption, days_used)
  
  // 转换为千瓦时
  let kwh_consumption = monthly_consumption / 1000
  @azimuth.assert_eq(72, kwh_consumption)
  
  // 计算费用
  let total_cost = @azimuth.multiply(kwh_consumption, cost_per_kwh)
  @azimuth.assert_eq(720, total_cost)
}

test "game_damage_calculation" {
  // 游戏伤害计算测试
  let base_damage = 100
  let weapon_multiplier = 2
  let critical_strike = 3  // 暴击倍数
  let enemy_defense = 25
  let skill_bonus = 50
  
  // 计算基础伤害
  let weapon_damage = @azimuth.multiply(base_damage, weapon_multiplier)
  
  // 计算暴击伤害
  let critical_damage = @azimuth.multiply(weapon_damage, critical_strike)
  
  // 减去敌人防御
  let damage_after_defense = @azimuth.add(critical_damage, -enemy_defense)
  
  // 添加技能加成
  let final_damage = @azimuth.add(damage_after_defense, skill_bonus)
  
  @azimuth.assert_eq(625, final_damage)
  
  // 计算 DPS（每秒伤害），假设攻击间隔为2秒
  let attack_interval = 2
  let dps = final_damage / attack_interval
  @azimuth.assert_eq(312, dps)  // 整数除法
}

test "array_index_calculation" {
  // 数组索引计算测试：二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, width), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, width), last_col)
  @azimuth.assert_eq(49, last_index)
}

test "data_compression_ratio" {
  // 压缩比计算：模拟数据压缩效果
  let original_size = 10000
  let compressed_size = 2500
  
  // 计算压缩比
  let compression_ratio = @azimuth.multiply(compressed_size, 100) / original_size
  @azimuth.assert_eq(25, compression_ratio)  // 25% of original size
  
  // 计算空间节省
  let space_saved = @azimuth.add(original_size, -compressed_size)
  @azimuth.assert_eq(7500, space_saved)
  
  // 计算节省百分比
  let savings_percent = @azimuth.multiply(space_saved, 100) / original_size
  @azimuth.assert_eq(75, savings_percent)
}

test "physics_projectile_motion" {
  // 物理抛物运动模拟
  let initial_velocity = 20  // m/s
  let angle = 45  // 度（简化，假设sin(45)=cos(45)=0.7≈1/√2）
  let gravity = 10  // m/s²
  
  // 计算水平和垂直分量（简化）
  let v_horizontal = initial_velocity / 1  // 简化计算
  let v_vertical = initial_velocity / 1    // 简化计算
  
  // 计算飞行时间：t = 2v/g
  let flight_time = @azimuth.multiply(2, v_vertical) / gravity
  @azimuth.assert_eq(4, flight_time)
  
  // 计算水平距离：d = v * t
  let horizontal_distance = @azimuth.multiply(v_horizontal, flight_time)
  @azimuth.assert_eq(80, horizontal_distance)
  
  // 计算最大高度：h = v²/(2g)
  let max_height = @azimuth.multiply(v_vertical, v_vertical) / @azimuth.multiply(2, gravity)
  @azimuth.assert_eq(20, max_height)
}

test "statistical_variance_calculation" {
  // 统计方差计算
  // 数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)
  
  // 计算方差：Σ(x - mean)² / n
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let sq1 = @azimuth.multiply(diff1, diff1)
  let sq2 = @azimuth.multiply(diff2, diff2)
  let sq3 = @azimuth.multiply(diff3, diff3)
  let sq4 = @azimuth.multiply(diff4, diff4)
  let sq5 = @azimuth.multiply(diff5, diff5)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq1, sq2), sq3), sq4), sq5) / 5
  @azimuth.assert_eq(200, variance)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let packet_size = 1500  // 字节
  let packets_per_second = 100
  let transmission_time = 10  // 秒
  let overhead_ratio = 10  // 10% 开销
  
  // 计算原始数据量
  let raw_data = @azimuth.multiply(packet_size, packets_per_second)
  
  // 计算总传输量
  let total_transferred = @azimuth.multiply(raw_data, transmission_time)
  @azimuth.assert_eq(1500000, total_transferred)
  
  // 计算有效数据（减去开销）
  let overhead = @azimuth.multiply(total_transferred, overhead_ratio) / 100
  let effective_data = @azimuth.add(total_transferred, -overhead)
  @azimuth.assert_eq(1350000, effective_data)
  
  // 计算吞吐量（字节/秒）
  let throughput = effective_data / transmission_time
  @azimuth.assert_eq(135000, throughput)
}

test "advanced_string_patterns" {
  // 高级字符串模式测试
  // 测试包含各种格式的字符串
  let email = "user@example.com"
  let url = "https://www.example.com/path"
  let phone = "+1-555-123-4567"
  let date = "2023-12-25"
  
  // 验证字符串处理
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, https://www.example.com/path!", @azimuth.greet(url))
  @azimuth.assert_eq_string("Hello, +1-555-123-4567!", @azimuth.greet(phone))
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet(date))
  
  // 测试包含特殊字符的字符串
  let json = "{\"key\":\"value\"}"
  let xml = "<element>content</element>"
  let sql = "SELECT * FROM users"
  
  @azimuth.assert_eq_string("Hello, {\"key\":\"value\"}!", @azimuth.greet(json))
  @azimuth.assert_eq_string("Hello, <element>content</element>!", @azimuth.greet(xml))
  @azimuth.assert_eq_string("Hello, SELECT * FROM users!", @azimuth.greet(sql))
}