// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "error_handling_scenarios" {
  // 错误处理场景测试
  let valid_input = 10
  let zero_input = 0
  let negative_input = -5
  
  // 测试正常输入
  let normal_result = azimuth.add(valid_input, valid_input)
  @assertion.assert_eq(20, normal_result)?
  
  // 测试零值输入
  let zero_result = azimuth.multiply(zero_input, valid_input)
  @assertion.assert_eq(0, zero_result)?
  
  // 测试负值输入
  let negative_result = azimuth.add(negative_input, valid_input)
  @assertion.assert_eq(5, negative_result)?
  
  // 测试混合运算
  let mixed_result = azimuth.multiply(negative_input, negative_input)
  @assertion.assert_eq(25, mixed_result)?
}

test "data_type_compatibility" {
  // 数据类型兼容性测试
  let small_int = 127
  let medium_int = 32767
  let large_int = 2147483647
  
  // 测试不同大小整数的运算
  let result1 = azimuth.add(small_int, medium_int)
  @assertion.assert_eq(32894, result1)?
  
  let result2 = azimuth.multiply(small_int, 2)
  @assertion.assert_eq(254, result2)?
  
  // 测试边界值
  let result3 = azimuth.add(large_int, 0)
  @assertion.assert_eq(2147483647, result3)?
  
  let result4 = azimuth.multiply(large_int, 1)
  @assertion.assert_eq(2147483647, result4)?
}

test "string_edge_cases" {
  // 字符串边界情况测试
  let empty_string = ""
  let single_char = "A"
  let long_string = "This is a very long string with many characters"
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  
  // 测试空字符串
  let empty_result = azimuth.greet(empty_string)
  @assertion.assert_eq_string("Hello, !", empty_result)?
  
  // 测试单字符
  let single_result = azimuth.greet(single_char)
  @assertion.assert_eq_string("Hello, A!", single_result)?
  
  // 测试长字符串
  let long_result = azimuth.greet(long_string)
  @assertion.assert_eq_string("Hello, This is a very long string with many characters!", long_result)?
  
  // 测试特殊字符
  let special_result = azimuth.greet(special_chars)
  @assertion.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_result)?
}

test "algorithmic_patterns" {
  // 算法模式测试
  let base = 2
  let exponent = 10
  
  // 测试指数运算（通过重复乘法）
  let power_result = azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(base, base), base), base), base), base), base), base), base)
  @assertion.assert_eq(1024, power_result)?
  
  // 测试斐波那契数列（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth.add(fib1, fib2)
  let fib4 = azimuth.add(fib2, fib3)
  let fib5 = azimuth.add(fib3, fib4)
  @assertion.assert_eq(5, fib5)?
  
  // 测试阶乘（简化版）
  let factorial_input = 5
  let factorial_result = azimuth.multiply(factorial_input, azimuth.multiply(azimuth.add(factorial_input, -1), azimuth.multiply(azimuth.add(factorial_input, -2), azimuth.multiply(azimuth.add(factorial_input, -3), azimuth.add(factorial_input, -4)))))
  @assertion.assert_eq(120, factorial_result)?
}

test "business_logic_validation" {
  // 业务逻辑验证测试
  let product_price = 199
  let discount_rate = 15
  let customer_points = 1200
  let points_rate = 10
  
  // 计算折扣金额
  let discount_amount = azimuth.multiply(product_price, discount_rate) / 100
  let discounted_price = azimuth.add(product_price, -discount_amount)
  
  // 计算积分抵扣
  let points_discount = azimuth.multiply(customer_points, points_rate) / 100
  let final_price = azimuth.add(discounted_price, -points_discount)
  
  // 验证最终价格：199 - 199×15/100 - 1200×10/100 = 199 - 29 - 120 = 50
  @assertion.assert_eq(50, final_price)?
  
  // 测试会员等级加成
  let member_level = 3
  let member_bonus = azimuth.multiply(final_price, member_level) / 100
  let member_price = azimuth.add(final_price, -member_bonus)
  
  // 验证会员价格：50 - 50×3/100 = 50 - 1 = 49
  @assertion.assert_eq(49, member_price)?
}

test "scientific_computing" {
  // 科学计算测试
  let mass = 1000  // kg
  let acceleration = 9.8  // m/s² (简化为整数)
  let time = 10  // seconds
  
  // 计算力：F = m × a
  let force = azimuth.multiply(mass, acceleration)
  @assertion.assert_eq(9800, force)?
  
  // 计算功：W = F × d (假设距离 = a × t² / 2，简化为整数)
  let distance = azimuth.multiply(acceleration, azimuth.multiply(time, time)) / 2
  let work = azimuth.multiply(force, distance)
  @assertion.assert_eq(490000, work)?
  
  // 计算功率：P = W / t
  let power = work / time
  @assertion.assert_eq(49000, power)?
  
  // 计算动能：KE = 0.5 × m × v² (假设 v = a × t)
  let velocity = azimuth.multiply(acceleration, time)
  let kinetic_energy = azimuth.multiply(mass, azimuth.multiply(velocity, velocity)) / 2
  @assertion.assert_eq(4802000, kinetic_energy)?
}

test "date_time_calculations" {
  // 日期时间计算测试
  let days_in_month = 30
  let months_in_year = 12
  let hours_in_day = 24
  let minutes_in_hour = 60
  
  // 计算一年中的天数
  let days_in_year = azimuth.multiply(days_in_month, months_in_year)
  @assertion.assert_eq(360, days_in_year)?
  
  // 计算一年中的小时数
  let hours_in_year = azimuth.multiply(days_in_year, hours_in_day)
  @assertion.assert_eq(8640, hours_in_year)?
  
  // 计算一年中的分钟数
  let minutes_in_year = azimuth.multiply(hours_in_year, minutes_in_hour)
  @assertion.assert_eq(518400, minutes_in_year)?
  
  // 计算工作日（假设一周5天，一年52周）
  let work_days_per_week = 5
  let weeks_in_year = 52
  let work_days_in_year = azimuth.multiply(work_days_per_week, weeks_in_year)
  @assertion.assert_eq(260, work_days_in_year)?
}

test "network_protocol_simulation" {
  // 网络协议模拟测试
  let packet_size = 1500  // bytes
  let overhead = 20  // bytes header
  let number_of_packets = 100
  let error_rate = 2  // 2%
  
  // 计算有效载荷
  let payload_size = azimuth.add(packet_size, -overhead)
  @assertion.assert_eq(1480, payload_size)?
  
  // 计算总传输量
  let total_transfer = azimuth.multiply(packet_size, number_of_packets)
  @assertion.assert_eq(150000, total_transfer)?
  
  // 计算有效数据量
  let effective_data = azimuth.multiply(payload_size, number_of_packets)
  @assertion.assert_eq(148000, effective_data)?
  
  // 计算错误数据包数
  let error_packets = azimuth.multiply(number_of_packets, error_rate) / 100
  @assertion.assert_eq(2, error_packets)?
  
  // 计算重传数据量
  let retransmission = azimuth.multiply(error_packets, packet_size)
  @assertion.assert_eq(3000, retransmission)?
  
  // 计算实际传输效率
  let efficiency = azimuth.multiply(effective_data, 100) / azimuth.add(total_transfer, retransmission)
  @assertion.assert_eq(97, efficiency)?
}

test "cryptography_simulation" {
  // 密码学模拟测试
  let plaintext = 12345
  let key1 = 7
  let key2 = 13
  let key3 = 17
  
  // 简单的多层加密（异或的加法模拟）
  let encrypted1 = azimuth.add(plaintext, key1)
  let encrypted2 = azimuth.add(encrypted1, key2)
  let encrypted3 = azimuth.add(encrypted2, key3)
  
  // 验证加密结果
  @assertion.assert_eq(12382, encrypted3)?
  
  // 解密过程
  let decrypted1 = azimuth.add(encrypted3, -key3)
  let decrypted2 = azimuth.add(decrypted1, -key2)
  let decrypted3 = azimuth.add(decrypted2, -key1)
  
  // 验证解密结果
  @assertion.assert_eq(plaintext, decrypted3)?
  
  // 测试密钥交换（简化版）
  let alice_secret = 42
  let bob_secret = 58
  let public_base = 5
  let public_modulus = 97
  
  // Alice的计算
  let alice_public = azimuth.multiply(public_base, alice_secret) % public_modulus
  // Bob的计算
  let bob_public = azimuth.multiply(public_base, bob_secret) % public_modulus
  
  // 共享密钥计算
  let alice_shared = azimuth.multiply(bob_public, alice_secret) % public_modulus
  let bob_shared = azimuth.multiply(alice_public, bob_secret) % public_modulus
  
  // 验证共享密钥相同
  @assertion.assert_eq(alice_shared, bob_shared)?
}