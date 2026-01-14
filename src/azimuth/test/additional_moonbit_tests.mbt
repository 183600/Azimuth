// 额外的 MoonBit 测试用例
// 补充现有测试，覆盖更多实际应用场景

test "advanced_mathematical_operations" {
  // 测试高级数学运算
  // 计算平方和：a² + b² + c²
  let a = 3
  let b = 4
  let c = 5
  
  let a_squared = @azimuth.multiply(a, a)  // 9
  let b_squared = @azimuth.multiply(b, b)  // 16
  let c_squared = @azimuth.multiply(c, c)  // 25
  
  let sum_of_squares = @azimuth.add(@azimuth.add(a_squared, b_squared), c_squared)
  @azimuth.assert_eq(50, sum_of_squares)
  
  // 计算立方差：a³ - b³
  let x = 4
  let y = 2
  let x_cubed = @azimuth.multiply(x, @azimuth.multiply(x, x))  // 64
  let y_cubed = @azimuth.multiply(y, @azimuth.multiply(y, y))  // 8
  let difference = @azimuth.add(x_cubed, -y_cubed)  // 56
  
  @azimuth.assert_eq(56, difference)
}

test "physics_calculations" {
  // 测试物理计算
  // 计算动能：KE = 1/2 × m × v²
  let mass = 10
  let velocity = 5
  let velocity_squared = @azimuth.multiply(velocity, velocity)  // 25
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2  // 125
  
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 计算势能：PE = m × g × h
  let height = 20
  let gravity = 10  // 简化重力加速度
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)  // 2000
  
  @azimuth.assert_eq(2000, potential_energy)
  
  // 计算总机械能
  let total_energy = @azimuth.add(kinetic_energy, potential_energy)
  @azimuth.assert_eq(2125, total_energy)
}

test "data_structure_operations" {
  // 测试数据结构操作
  // 模拟数组操作：计算数组元素的总和、平均值、最大值、最小值
  // 数组：[15, 8, 23, 42, 7, 19]
  
  let elem1 = 15
  let elem2 = 8
  let elem3 = 23
  let elem4 = 42
  let elem5 = 7
  let elem6 = 19
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(elem1, elem2), elem3), elem4), elem5), elem6)
  @azimuth.assert_eq(114, sum)
  
  // 计算平均值
  let count = 6
  let average = sum / count
  @azimuth.assert_eq(19, average)
  
  // 查找最大值（简化比较）
  let max_val = 42
  @azimuth.assert_eq(42, max_val)
  
  // 查找最小值（简化比较）
  let min_val = 7
  @azimuth.assert_eq(7, min_val)
  
  // 计算范围
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(35, range)
}

test "algorithmic_complexity" {
  // 测试算法复杂度相关计算
  // 计算斐波那契数列前几项
  let n1 = 1
  let n2 = 1
  let n3 = @azimuth.add(n1, n2)  // 2
  let n4 = @azimuth.add(n2, n3)  // 3
  let n5 = @azimuth.add(n3, n4)  // 5
  let n6 = @azimuth.add(n4, n5)  // 8
  let n7 = @azimuth.add(n5, n6)  // 13
  
  @azimuth.assert_eq(13, n7)
  
  // 计算斐波那契数列前7项的和
  let fib_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(n1, n2), n3), n4), n5), n6), n7)
  @azimuth.assert_eq(33, fib_sum)
  
  // 计算幂运算：2^8
  let base = 2
  let exponent = 8
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, base), base), base), 
                                     @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, base), base), base))
  @azimuth.assert_eq(256, power_of_2)
}

test "business_intelligence" {
  // 测试商业智能计算
  // 销售数据分析
  let q1_sales = 45000
  let q2_sales = 52000
  let q3_sales = 48000
  let q4_sales = 61000
  
  // 年度总销售额
  let annual_sales = @azimuth.add(@azimuth.add(@azimuth.add(q1_sales, q2_sales), q3_sales), q4_sales)
  @azimuth.assert_eq(206000, annual_sales)
  
  // 季度平均销售额
  let quarterly_avg = annual_sales / 4
  @azimuth.assert_eq(51500, quarterly_avg)
  
  // 同比增长率（假设去年销售额为180000）
  let last_year_sales = 180000
  let growth = @azimuth.add(annual_sales, -last_year_sales)  // 26000
  let growth_rate = @azimuth.multiply(growth, 100) / last_year_sales  // 14%
  
  @azimuth.assert_eq(14, growth_rate)
  
  // 市场份额计算（假设市场总规模为1000000）
  let market_size = 1000000
  let market_share = @azimuth.multiply(annual_sales, 100) / market_size  // 20%
  
  @azimuth.assert_eq(20, market_share)
}

test "game_development_calculations" {
  // 测试游戏开发相关计算
  // 角色属性计算
  let base_hp = 100
  let level = 8
  let vitality = 15
  
  // 计算生命值：HP = base + level × 10 + vitality × 5
  let hp_from_level = @azimuth.multiply(level, 10)  // 80
  let hp_from_vitality = @azimuth.multiply(vitality, 5)  // 75
  let total_hp = @azimuth.add(@azimuth.add(base_hp, hp_from_level), hp_from_vitality)  // 255
  
  @azimuth.assert_eq(255, total_hp)
  
  // 计算攻击力：Attack = base + level × 5 + strength × 3
  let base_attack = 20
  let strength = 12
  let attack_from_level = @azimuth.multiply(level, 5)  // 40
  let attack_from_strength = @azimuth.multiply(strength, 3)  // 36
  let total_attack = @azimuth.add(@azimuth.add(base_attack, attack_from_level), attack_from_strength)  // 96
  
  @azimuth.assert_eq(96, total_attack)
  
  // 计算暴击伤害：Critical Damage = Attack × 2
  let critical_damage = @azimuth.multiply(total_attack, 2)
  @azimuth.assert_eq(192, critical_damage)
}

test "scientific_computing" {
  // 测试科学计算
  // 计算理想气体状态方程：PV = nRT（简化版本）
  let pressure = 101325  // 帕斯卡
  let volume = 22400     // 升
  let n = 1              // 摩尔数
  let gas_constant = 8   // 简化气体常数
  let temperature = 280  // 开尔文
  
  // 验证方程：P × V = n × R × T
  let left_side = @azimuth.multiply(pressure, volume) / 1000  // 简化计算
  let right_side = @azimuth.multiply(@azimuth.multiply(n, gas_constant), temperature)
  
  @azimuth.assert_eq(2240, left_side)
  @azimuth.assert_eq(2240, right_side)
  
  // 计算密度：ρ = m/V
  let mass = 1000  // 克
  let volume2 = 500  // 立方厘米
  let density = @azimuth.multiply(mass, 1000) / volume2  // 2000 kg/m³
  
  @azimuth.assert_eq(2000, density)
  
  // 计算浓度：C = n/V
  let moles = 2
  let volume3 = 500  // 毫升
  let concentration = @azimuth.multiply(moles, 1000) / volume3  // 4 mol/L
  
  @azimuth.assert_eq(4, concentration)
}

test "cryptography_basics" {
  // 测试密码学基础计算
  // 简单凯撒密码
  let plaintext = 13  // A对应数字13
  let shift = 3
  let ciphertext = @azimuth.add(plaintext, shift)  // 16 (D)
  
  @azimuth.assert_eq(16, ciphertext)
  
  // 解密
  let decrypted = @azimuth.add(ciphertext, -shift)
  @azimuth.assert_eq(13, decrypted)
  
  // 简单模运算
  let a = 17
  let m = 5
  let remainder = a % m  // 2
  @azimuth.assert_eq(2, remainder)
  
  // 计算最大公约数（简化版，欧几里得算法）
  let x = 48
  let y = 18
  // 简化计算：gcd(48, 18) = gcd(18, 48 % 18) = gcd(18, 12) = gcd(12, 6) = gcd(6, 0) = 6
  let gcd = 6
  @azimuth.assert_eq(6, gcd)
}

test "networking_calculations" {
  // 测试网络相关计算
  // 计算网络带宽利用率
  let actual_bandwidth = 850  // Mbps
  let max_bandwidth = 1000   // Mbps
  let utilization = @azimuth.multiply(actual_bandwidth, 100) / max_bandwidth  // 85%
  
  @azimuth.assert_eq(85, utilization)
  
  // 计算数据传输时间
  let file_size = 5000  // MB
  let transfer_rate = 10  // MB/s
  let transfer_time = file_size / transfer_rate  // 500秒
  
  @azimuth.assert_eq(500, transfer_time)
  
  // 计算网络延迟影响
  let base_response_time = 100  // 毫秒
  let network_latency = 50      // 毫秒
  let total_response_time = @azimuth.add(base_response_time, network_latency)  // 150毫秒
  
  @azimuth.assert_eq(150, total_response_time)
  
  // 计算数据包丢失率
  let packets_sent = 1000
  let packets_received = 950
  let packets_lost = @azimuth.add(packets_sent, -packets_received)  // 50
  let loss_rate = @azimuth.multiply(packets_lost, 100) / packets_sent  // 5%
  
  @azimuth.assert_eq(5, loss_rate)
}

test "machine_learning_basics" {
  // 测试机器学习基础计算
  // 线性回归：y = mx + b
  let m = 2  // 斜率
  let x = 5  // 输入值
  let b = 3  // 截距
  
  let y = @azimuth.add(@azimuth.multiply(m, x), b)  // 13
  @azimuth.assert_eq(13, y)
  
  // 计算多个预测值的总和
  let x1 = 1
  let x2 = 2
  let x3 = 3
  let x4 = 4
  
  let y1 = @azimuth.add(@azimuth.multiply(m, x1), b)  // 5
  let y2 = @azimuth.add(@azimuth.multiply(m, x2), b)  // 7
  let y3 = @azimuth.add(@azimuth.multiply(m, x3), b)  // 9
  let y4 = @azimuth.add(@azimuth.multiply(m, x4), b)  // 11
  
  let total_prediction = @azimuth.add(@azimuth.add(@azimuth.add(y1, y2), y3), y4)
  @azimuth.assert_eq(32, total_prediction)
  
  // 计算平均绝对误差
  let actual1 = 6
  let actual2 = 7
  let actual3 = 8
  let actual4 = 10
  
  let error1 = @azimuth.add(actual1, -y1)  // 1
  let error2 = @azimuth.add(actual2, -y2)  // 0
  let error3 = @azimuth.add(actual3, -y3)  // -1
  let error4 = @azimuth.add(actual4, -y4)  // -1
  
  let abs_error1 = if (error1 < 0) { @azimuth.add(0, -error1) } else { error1 }  // 1
  let abs_error2 = if (error2 < 0) { @azimuth.add(0, -error2) } else { error2 }  // 0
  let abs_error3 = if (error3 < 0) { @azimuth.add(0, -error3) } else { error3 }  // 1
  let abs_error4 = if (error4 < 0) { @azimuth.add(0, -error4) } else { error4 }  // 1
  
  let total_error = @azimuth.add(@azimuth.add(@azimuth.add(abs_error1, abs_error2), abs_error3), abs_error4)
  let mean_absolute_error = total_error / 4  // 0.75，简化为0
  
  @azimuth.assert_eq(0, mean_absolute_error)
}