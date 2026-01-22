// 新增的 MoonBit 测试用例
// 针对 azimuth 库的补充测试，包含实际应用场景

test "binary_number_operations" {
  // 二进制数运算模拟
  // 模拟二进制左移：n << k = n * 2^k
  let n = 5
  let left_shift_result = @azimuth.multiply(n, @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  @azimuth.assert_eq(40, left_shift_result)  // 5 * 2^3 = 5 * 8 = 40
  
  // 模拟二进制右移：n >> k = n / 2^k
  let right_shift_result = n / @azimuth.multiply(@azimuth.multiply(2, 2), 2)
  @azimuth.assert_eq(0, right_shift_result)  // 5 / 2^3 = 5 / 8 = 0 (整数除法)
}

test "physics_calculation_scenario" {
  // 物理计算场景：计算自由落体距离（简化版）
  // s = 0.5 * g * t^2，简化为 s = g * t^2 / 2
  let gravity = 10  // 重力加速度 m/s² (简化值)
  let time = 5      // 时间秒
  
  let distance = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2
  @azimuth.assert_eq(125, distance)  // 10 * 25 / 2 = 125
  
  // 计算速度：v = g * t
  let velocity = @azimuth.multiply(gravity, time)
  @azimuth.assert_eq(50, velocity)  // 10 * 5 = 50
}

test "chemical_reaction_calculation" {
  // 化学反应计算：计算反应物质量
  // 2H₂ + O₂ → 2H₂O
  let h2_molar_mass = 2
  let o2_molar_mass = 32
  let h2o_molar_mass = 18
  
  let h2_moles = 2
  let o2_moles = 1
  
  // 计算反应物总质量
  let h2_mass = @azimuth.multiply(h2_molar_mass, h2_moles)
  let o2_mass = @azimuth.multiply(o2_molar_mass, o2_moles)
  let total_reactant_mass = @azimuth.add(h2_mass, o2_mass)
  
  // 计算生成物总质量
  let h2o_mass = @azimuth.multiply(h2o_molar_mass, h2_moles)
  
  // 验证质量守恒
  @azimuth.assert_eq(total_reactant_mass, h2o_mass)
  @azimuth.assert_eq(36, total_reactant_mass)
}

test "financial_investment_calculation" {
  // 金融投资计算：复利与分期付款
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 5
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率)^年数
  // 使用 (1 + 利率 × 年数) 作为近似
  let growth_factor = @azimuth.add(1, @azimuth.multiply(annual_rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  // 计算总利息
  let total_interest = @azimuth.add(final_amount, -principal)
  
  @azimuth.assert_eq(50000, final_amount)
  @azimuth.assert_eq(40000, total_interest)
}

test "string_encoding_scenarios" {
  // 字符串编码场景测试
  // 测试Base64编码模拟（简化版）
  let original_length = 5  // "Hello" 的长度
  let encoded_length = @azimuth.multiply(original_length, 4) / 3 + 3  // 简化的Base64长度计算
  @azimuth.assert_eq(9, encoded_length)
  
  // 测试Unicode字符串
  let unicode_text = "测试"
  let unicode_result = @azimuth.greet(unicode_text)
  @azimuth.assert_eq_string("Hello, 测试!", unicode_result)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  // 模拟O(n²)算法的计算量
  let n = 100
  let o_n_squared = @azimuth.multiply(n, n)
  
  // 模拟O(n³)算法的计算量
  let o_n_cubed = @azimuth.multiply(o_n_squared, n)
  
  // 模拟O(log n)算法的计算量（简化为n的位数）
  let o_log_n = 7  // log₂100 ≈ 6.64，向上取整为7
  
  // 模拟O(n log n)算法的计算量
  let o_n_log_n = @azimuth.multiply(n, o_log_n)
  
  @azimuth.assert_eq(10000, o_n_squared)
  @azimuth.assert_eq(1000000, o_n_cubed)
  @azimuth.assert_eq(700, o_n_log_n)
}

test "database_index_calculation" {
  // 数据库索引计算
  let page_size = 4096  // 页面大小（字节）
  let key_size = 64     // 键大小（字节）
  let pointer_size = 8  // 指针大小（字节）
  
  // 计算每个页面能容纳的键数量
  let keys_per_page = page_size / @azimuth.add(key_size, pointer_size)
  
  // 计算3层B+树能容纳的键数量
  let keys_per_leaf = keys_per_page
  let keys_per_internal = keys_per_page
  let total_keys = @azimuth.multiply(@azimuth.multiply(keys_per_leaf, keys_per_internal), keys_per_internal)
  
  @azimuth.assert_eq(55, keys_per_page)  // 4096 / (64 + 8) = 4096 / 72 = 56
  @azimuth.assert_eq(166375, total_keys)  // 55³ = 166375
}

test "network_protocol_calculation" {
  // 网络协议计算
  let mtu = 1500  // 最大传输单元
  let header_size = 40  // IP+TCP头部
  let payload_size = @azimuth.add(mtu, -header_size)
  
  // 计算传输大数据需要的包数
  let data_size = 1000000
  let packets_needed = @azimuth.divide_with_ceil(data_size, payload_size)
  
  // 计算总开销
  let total_overhead = @azimuth.multiply(packets_needed, header_size)
  
  @azimuth.assert_eq(1460, payload_size)
  @azimuth.assert_eq(685, packets_needed)
  @azimuth.assert_eq(27400, total_overhead)
}

test "machine_learning_calculation" {
  // 机器学习计算模拟
  // 简单的线性回归：y = wx + b
  let weight = 2
  let bias = 10
  let input = 15
  
  let prediction = @azimuth.add(@azimuth.multiply(weight, input), bias)
  
  // 计算均方误差（简化版）
  let actual = 45
  let error = @azimuth.add(prediction, -actual)
  let squared_error = @azimuth.multiply(error, error)
  
  // 计算梯度下降的权重更新
  let learning_rate = 1
  let weight_update = @azimuth.multiply(learning_rate, @azimuth.multiply(error, input)) / 100
  let new_weight = @azimuth.add(weight, -weight_update)
  
  @azimuth.assert_eq(40, prediction)
  @azimuth.assert_eq(25, squared_error)
  @azimuth.assert_eq(17, new_weight)
}

test "cryptography_simulation" {
  // 密码学算法模拟
  // 简化的Caesar密码
  let plaintext = "HELLO"
  let shift = 3
  
  // 模拟加密（简化为数值计算）
  let A_value = 65  // ASCII值
  let Z_value = 90
  
  // 计算字母范围
  let alphabet_size = @azimuth.add(@azimuth.add(Z_value, -A_value), 1)
  @azimuth.assert_eq(26, alphabet_size)
  
  // 模拟哈希计算（简化）
  let hash_base = 31
  let hash_result = @azimuth.multiply(A_value, hash_base) + @azimuth.multiply(shift, hash_base)
  
  // 模拟简单的校验和
  let checksum = @azimuth.add(@azimuth.add(A_value, A_value), @azimuth.multiply(shift, 5)) % alphabet_size
  
  @azimuth.assert_eq(215, hash_result)
  @azimuth.assert_eq(15, checksum)
}