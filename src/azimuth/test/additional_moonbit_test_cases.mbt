// 新增的 MoonBit 测试用例
// 覆盖各种边界条件、实际应用场景和复杂计算

test "extreme_boundary_addition" {
  // 测试极值加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最大值溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "complex_financial_calculation" {
  // 测试复杂财务计算：贷款分期付款
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  let months = years * 12 // 总月数
  
  // 简化的月供计算：本金 + 总利息
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(555, monthly_payment)  // 简化计算结果
}

test "geometry_volume_calculations" {
  // 测试几何体积计算
  let radius = 5
  
  // 球体体积（简化公式）：4/3 * π * r³ ≈ 4 * r³
  let sphere_volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(500, sphere_volume)
  
  // 圆柱体体积（简化公式）：π * r² * h ≈ 3 * r² * h
  let height = 10
  let cylinder_volume = @azimuth.multiply(3, @azimuth.multiply(@azimuth.multiply(radius, radius), height))
  @azimuth.assert_eq(750, cylinder_volume)
  
  // 圆锥体体积（简化公式）：1/3 * π * r² * h ≈ r² * h
  let cone_volume = @azimuth.multiply(@azimuth.multiply(radius, radius), height)
  @azimuth.assert_eq(250, cone_volume)
}

test "advanced_string_processing" {
  // 测试高级字符串处理
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet(special_chars))
  
  // 测试包含Unicode字符的字符串
  let unicode_text = "Hello世界🌍"
  @azimuth.assert_eq_string("Hello, Hello世界🌍!", @azimuth.greet(unicode_text))
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User2023ProjectAlpha"
  @azimuth.assert_eq_string("Hello, User2023ProjectAlpha!", @azimuth.greet(alphanumeric))
}

test "physics_motion_calculations" {
  // 测试物理运动计算
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 2       // 加速度 m/s²
  let time = 5               // 时间 s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移（简化公式）：s = ut + 0.5at² ≈ ut + at²/2
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), 
                                 @azimuth.multiply(@azimuth.multiply(acceleration, time), time) / 2)
  @azimuth.assert_eq(75, displacement)
  
  // 计算动能（简化公式）：KE = 0.5 * m * v² ≈ m * v²
  let mass = 10
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(final_velocity, final_velocity))
  @azimuth.assert_eq(4000, kinetic_energy)
}

test "statistical_analysis" {
  // 测试统计分析计算
  // 数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  let sum1 = @azimuth.add(data1, data2)
  let sum2 = @azimuth.add(sum1, data3)
  let sum3 = @azimuth.add(sum2, data4)
  let data_sum = @azimuth.add(sum3, data5)
  
  @azimuth.assert_eq(150, data_sum)
  
  // 计算平均值
  let data_count = 5
  let mean = data_sum / data_count
  @azimuth.assert_eq(30, mean)
  
  // 计算方差（简化）：平方和的平均值
  let diff1 = data1 - mean
  let diff2 = data2 - mean
  let diff3 = data3 - mean
  let diff4 = data4 - mean
  let diff5 = data5 - mean
  
  let sq1 = @azimuth.multiply(diff1, diff1)
  let sq2 = @azimuth.multiply(diff2, diff2)
  let sq3 = @azimuth.multiply(diff3, diff3)
  let sq4 = @azimuth.multiply(diff4, diff4)
  let sq5 = @azimuth.multiply(diff5, diff5)
  
  let sq_sum1 = @azimuth.add(sq1, sq2)
  let sq_sum2 = @azimuth.add(sq_sum1, sq3)
  let sq_sum3 = @azimuth.add(sq_sum2, sq4)
  let variance_sum = @azimuth.add(sq_sum3, sq5)
  
  let variance = variance_sum / data_count
  @azimuth.assert_eq(200, variance)
}

test "cryptography_simulation" {
  // 测试密码学模拟（简化版）
  let plaintext = 1234
  let key = 5678
  
  // 简单的加密操作：异或的简化版本（使用加减法模拟）
  let encrypted = @azimuth.add(plaintext, key)
  @azimuth.assert_eq(6912, encrypted)
  
  // 解密操作
  let decrypted = @azimuth.add(encrypted, -key)
  @azimuth.assert_eq(plaintext, decrypted)
  
  // 测试密钥强度（简化：密钥的平方）
  let key_strength = @azimuth.multiply(key, key)
  @azimuth.assert_eq(32239684, key_strength)
}

test "game_mechanics_calculation" {
  // 测试游戏机制计算
  let base_damage = 100
  let weapon_multiplier = 2
  let critical_multiplier = 2
  
  // 计算基础伤害
  let normal_damage = @azimuth.multiply(base_damage, weapon_multiplier)
  @azimuth.assert_eq(200, normal_damage)
  
  // 计算暴击伤害
  let critical_damage = @azimuth.multiply(normal_damage, critical_multiplier)
  @azimuth.assert_eq(400, critical_damage)
  
  // 计算期望伤害（简化）：普通伤害 * 80% + 暴击伤害 * 20%
  let normal_part = @azimuth.multiply(normal_damage, 80)
  let critical_part = @azimuth.multiply(critical_damage, 20)
  let total_parts = @azimuth.add(normal_part, critical_part)
  let expected_damage = total_parts / 100
  @azimuth.assert_eq(240, expected_damage)
}

test "data_structure_operations" {
  // 测试数据结构操作（模拟）
  let array_size = 10
  let element_size = 4
  
  // 计算数组总大小
  let total_size = @azimuth.multiply(array_size, element_size)
  @azimuth.assert_eq(40, total_size)
  
  // 计算二维数组索引（行主序）
  let row = 3
  let col = 2
  let width = 5
  let row_offset = @azimuth.multiply(row, width)
  let index = @azimuth.add(row_offset, col)
  @azimuth.assert_eq(17, index)
  
  // 计算哈希表容量（简化：下一个2的幂）
  let hash_capacity = 128  // 简化为固定的下一个2的幂
  @azimuth.assert_eq(128, hash_capacity)
}

test "network_protocol_simulation" {
  // 测试网络协议模拟
  let packet_size = 1024
  let header_size = 20
  let payload_size = @azimuth.add(packet_size, -header_size)
  @azimuth.assert_eq(1004, payload_size)
  
  // 计算带宽利用率
  let transmission_time = @azimuth.multiply(packet_size, 8)  // 转换为位
  @azimuth.assert_eq(8192, transmission_time)
  
  // 计算延迟（简化）
  let propagation_delay = 50  // 50ms
  let transmission_delay = 10  // 10ms
  let total_delay = @azimuth.add(propagation_delay, transmission_delay)
  @azimuth.assert_eq(60, total_delay)
}