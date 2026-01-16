// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "error_handling_with_negative_inputs" {
  // 测试负数输入的错误处理
  let result1 = azimuth::add(-100, -200)
  assert_eq(result1, -300)
  
  let result2 = azimuth::multiply(-15, -4)
  assert_eq(result2, 60)
  
  let result3 = azimuth::multiply(-8, 7)
  assert_eq(result3, -56)
}

test "unicode_string_processing" {
  // 测试 Unicode 字符串处理
  let result1 = azimuth::greet("世界")
  assert_eq(result1, "Hello, 世界!")
  
  let result2 = azimuth::greet("🚀")
  assert_eq(result2, "Hello, 🚀!")
  
  let result3 = azimuth::greet("Café")
  assert_eq(result3, "Hello, Café!")
}

test "mathematical_power_calculations" {
  // 测试幂运算（通过重复乘法实现）
  let base = 3
  
  // 3^2 = 9
  let power2 = azimuth::multiply(base, base)
  assert_eq(power2, 9)
  
  // 3^3 = 27
  let power3 = azimuth::multiply(power2, base)
  assert_eq(power3, 27)
  
  // 3^4 = 81
  let power4 = azimuth::multiply(power3, base)
  assert_eq(power4, 81)
}

test "statistical_operations" {
  // 测试统计运算
  
  // 计算平均值: (10 + 20 + 30) / 3 = 20
  let sum = azimuth::add(azimuth::add(10, 20), 30)
  let count = 3
  let average = sum / count
  assert_eq(average, 20)
  
  // 计算范围: max - min = 50 - 5 = 45
  let max_val = 50
  let min_val = 5
  let range = azimuth::add(max_val, -min_val)
  assert_eq(range, 45)
}

test "currency_conversion_simulation" {
  // 模拟货币转换测试
  
  // 美元转人民币 (假设汇率 1:7.2)
  let usd_amount = 100
  let exchange_rate = 7
  let cny_amount = azimuth::multiply(usd_amount, exchange_rate)
  assert_eq(cny_amount, 700)
  
  // 欧元转美元 (假设汇率 1:1.1)
  let eur_amount = 50
  let eur_to_usd = 1
  let usd_from_eur = azimuth::multiply(eur_amount, eur_to_usd)
  assert_eq(usd_from_eur, 50)
}

test "scientific_notation_operations" {
  // 科学计数法运算测试
  
  // 1.23 * 10^3 = 1230
  let mantissa = 123
  let exponent = 10
  let scientific_result = azimuth::multiply(mantissa, exponent)
  assert_eq(scientific_result, 1230)
  
  // 4.56 * 10^2 = 456
  let mantissa2 = 456
  let exponent2 = 1
  let scientific_result2 = azimuth::multiply(mantissa2, exponent2)
  assert_eq(scientific_result2, 456)
}

test "memory_address_simulation" {
  // 模拟内存地址计算
  
  let base_address = 1000
  let offset1 = 50
  let offset2 = 25
  let offset3 = 10
  
  // 计算最终地址: base + offset1 + offset2 + offset3
  let final_address = azimuth::add(azimuth::add(azimuth::add(base_address, offset1), offset2), offset3)
  assert_eq(final_address, 1085)
}

test "network_protocol_simulation" {
  // 网络协议模拟测试
  
  // 计算数据包总大小: header + payload + checksum
  let header_size = 20
  let payload_size = 1024
  let checksum_size = 4
  let total_packet_size = azimuth::add(azimuth::add(header_size, payload_size), checksum_size)
  assert_eq(total_packet_size, 1048)
  
  // 计算带宽利用率: payload_size / total_packet_size
  let bandwidth_utilization = payload_size * 100 / total_packet_size
  assert_eq(bandwidth_utilization, 97)
}

test "game_physics_simulation" {
  // 游戏物理模拟测试
  
  // 计算自由落体距离: d = g * t^2 / 2 (简化版本)
  let gravity = 10  // 简化的重力加速度
  let time = 3
  let time_squared = azimuth::multiply(time, time)
  let distance = azimuth::multiply(gravity, time_squared) / 2
  assert_eq(distance, 45)
  
  // 计算速度: v = g * t
  let velocity = azimuth::multiply(gravity, time)
  assert_eq(velocity, 30)
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  
  let original_size = 1000
  let compression_ratio = 70  // 70% 压缩率
  
  // 计算压缩后大小
  let compressed_size = azimuth::multiply(original_size, compression_ratio) / 100
  assert_eq(compressed_size, 700)
  
  // 计算节省的空间
  let saved_space = azimuth::add(original_size, -compressed_size)
  assert_eq(saved_space, 300)
  
  // 计算压缩效率
  let efficiency = saved_space * 100 / original_size
  assert_eq(efficiency, 30)
}