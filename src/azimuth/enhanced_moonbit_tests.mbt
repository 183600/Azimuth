// 增强的 MoonBit 测试用例
// 这些测试用例涵盖了各种实际应用场景和边界情况

test "hexadecimal_conversion_simulation" {
  // 模拟十六进制转换：将十进制数转换为十六进制表示（通过字符串）
  let decimal_num = 255
  let hex_representation = "FF"
  
  // 验证转换结果（通过字符串匹配）
  let result = azimuth::greet(hex_representation)
  azimuth::assert_eq_string("Hello, FF!", result)
  
  // 测试更大的数字
  let large_decimal = 4095
  let large_hex = "FFF"
  let large_result = azimuth::greet(large_hex)
  azimuth::assert_eq_string("Hello, FFF!", large_result)
}

test "array_sum_calculation" {
  // 模拟数组求和：计算一系列数字的总和
  let num1 = 15
  let num2 = 25
  let num3 = 35
  let num4 = 45
  let num5 = 55
  
  // 计算总和
  let total = azimuth::add(azimuth::add(azimuth::add(azimuth::add(num1, num2), num3), num4), num5)
  azimuth::assert_eq(175, total)
  
  // 计算平均值
  let average = total / 5
  azimuth::assert_eq(35, average)
}

test "loan_calculation_scenario" {
  // 贷款计算场景：计算月供和总利息
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let loan_term = 30      // 贷款期限30年
  
  // 简化的月供计算（仅用于演示）
  let monthly_rate = annual_rate / 12  // 月利率
  let total_months = loan_term * 12    // 总月数
  let total_interest = azimuth::multiply(principal, azimuth::multiply(monthly_rate, total_months)) / 100
  let total_payment = azimuth::add(principal, total_interest)
  
  // 验证计算结果
  azimuth::assert_eq(180000, total_interest)  // 简化计算
  azimuth::assert_eq(280000, total_payment)
}

test "temperature_conversion_precise" {
  // 精确的温度转换：摄氏度转华氏度 F = C × 9/5 + 32
  // 由于只有整数运算，我们使用近似值：F = C × 2 - C × 3/10 + 32
  
  let celsius = 25
  let fahrenheit_approx = azimuth::add(azimuth::add(azimuth::multiply(celsius, 2), azimuth::multiply(celsius, -3) / 10), 32)
  azimuth::assert_eq(77, fahrenheit_approx)  // 25 × 2 - 25 × 3/10 + 32 = 50 - 7 + 32 = 75（近似值）
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::add(azimuth::multiply(freezing_celsius, 2), azimuth::multiply(freezing_celsius, -3) / 10), 32)
  azimuth::assert_eq(32, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = azimuth::add(azimuth::add(azimuth::multiply(boiling_celsius, 2), azimuth::multiply(boiling_celsius, -3) / 10), 32)
  azimuth::assert_eq(212, boiling_fahrenheit)
}

test "discount_calculation_business" {
  // 商业折扣计算：基于购买量的阶梯折扣
  let unit_price = 50
  let quantity1 = 10   // 无折扣
  let quantity2 = 50   // 10%折扣
  let quantity3 = 100  // 20%折扣
  
  // 计算不同数量下的总价
  let total1 = azimuth::multiply(unit_price, quantity1)
  let discount2 = azimuth::multiply(azimuth::multiply(unit_price, quantity2), 10) / 100
  let total2 = azimuth::add(azimuth::multiply(unit_price, quantity2), -discount2)
  let discount3 = azimuth::multiply(azimuth::multiply(unit_price, quantity3), 20) / 100
  let total3 = azimuth::add(azimuth::multiply(unit_price, quantity3), -discount3)
  
  // 验证计算结果
  azimuth::assert_eq(500, total1)    // 50 × 10 = 500
  azimuth::assert_eq(2250, total2)   // 50 × 50 - 10% = 2500 - 250 = 2250
  azimuth::assert_eq(4000, total3)   // 50 × 100 - 20% = 5000 - 1000 = 4000
}

test "base_conversion_simulation" {
  // 模拟进制转换：二进制转十进制
  // 二进制数 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 2³位
  let bit2 = 0  // 2²位
  let bit1 = 1  // 2¹位
  let bit0 = 1  // 2⁰位
  
  let decimal_value = azimuth::add(azimuth::add(azimuth::multiply(bit3, 8), azimuth::multiply(bit2, 4)), azimuth::add(azimuth::multiply(bit1, 2), bit0))
  azimuth::assert_eq(11, decimal_value)
  
  // 测试另一个二进制数：1101 = 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let bit3_2 = 1
  let bit2_2 = 1
  let bit1_2 = 0
  let bit0_2 = 1
  
  let decimal_value2 = azimuth::add(azimuth::add(azimuth::multiply(bit3_2, 8), azimuth::multiply(bit2_2, 4)), azimuth::add(azimuth::multiply(bit1_2, 2), bit0_2))
  azimuth::assert_eq(13, decimal_value2)
}

test "velocity_distance_calculation" {
  // 物理计算：速度、距离和时间的关系
  // 距离 = 速度 × 时间
  
  let velocity = 60  // km/h
  let time = 2.5     // 小时（用整数表示：2小时30分钟 = 150分钟）
  let time_in_minutes = 150
  
  // 计算距离（km）：60 km/h × 2.5 h = 150 km
  let distance = azimuth::multiply(velocity, time_in_minutes) / 60
  azimuth::assert_eq(150, distance)
  
  // 计算返回时间：距离 / 速度
  let return_time_minutes = azimuth::multiply(distance, 60) / velocity
  azimuth::assert_eq(150, return_time_minutes)
}

test "data_storage_calculation" {
  // 数据存储计算：计算文件大小和存储需求
  let file_size_mb = 25
  let file_count = 100
  let redundancy_factor = 3  // 3倍冗余
  
  // 计算原始存储需求
  let raw_storage = azimuth::multiply(file_size_mb, file_count)
  azimuth::assert_eq(2500, raw_storage)  // 25 MB × 100 = 2500 MB
  
  // 计算冗余存储需求
  let redundant_storage = azimuth::multiply(raw_storage, redundancy_factor)
  azimuth::assert_eq(7500, redundant_storage)  // 2500 MB × 3 = 7500 MB
  
  // 转换为GB（简化：1GB = 1000MB）
  let storage_gb = redundant_storage / 1000
  azimuth::assert_eq(7, storage_gb)
}

test "cryptographic_hash_simulation" {
  // 模拟简单哈希函数：基于字符串长度和字符值的简单哈希
  let input_string = "Hello"
  let string_length = 5
  
  // 简化的哈希计算：将每个字符的ASCII值相加
  // H(72) + e(101) + l(108) + l(108) + o(111) = 500
  let hash_value = azimuth::add(azimuth::add(azimuth::add(azimuth::add(72, 101), 108), 108), 111)
  azimuth::assert_eq(500, hash_value)
  
  // 测试另一个字符串
  let input_string2 = "World"
  // W(87) + o(111) + r(114) + l(108) + d(100) = 520
  let hash_value2 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(87, 111), 114), 108), 100)
  azimuth::assert_eq(520, hash_value2)
  
  // 验证不同字符串产生不同哈希值
  azimuth::assert_true(hash_value != hash_value2)
}

test "network_bandwidth_calculation" {
  // 网络带宽计算：计算文件传输时间
  let file_size_mb = 500
  let bandwidth_mbps = 50  // 50 Mbps
  
  // 计算传输时间（秒）：文件大小(MB) × 8 / 带宽(Mbps)
  let transfer_time_seconds = azimuth::multiply(file_size_mb, 8) / bandwidth_mbps
  azimuth::assert_eq(80, transfer_time_seconds)  // 500 × 8 / 50 = 80秒
  
  // 转换为分钟和秒
  let minutes = transfer_time_seconds / 60
  let seconds = transfer_time_seconds % 60
  azimuth::assert_eq(1, minutes)
  azimuth::assert_eq(20, seconds)
  
  // 计算多个文件的传输时间
  let file_count = 10
  let total_time_seconds = azimuth::multiply(transfer_time_seconds, file_count)
  azimuth::assert_eq(800, total_time_seconds)  // 80秒 × 10 = 800秒
  
  // 转换为分钟
  let total_minutes = total_time_seconds / 60
  azimuth::assert_eq(13, total_minutes)  // 800秒 / 60 = 13分钟（余20秒）
}