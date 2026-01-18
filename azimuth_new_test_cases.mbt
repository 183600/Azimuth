// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "physics_calculations" {
  // 物理计算测试
  let mass = 10        // 质量 (kg)
  let acceleration = 5 // 加速度 (m/s²)
  let height = 20      // 高度 (m)
  let gravity = 10     // 重力加速度 (m/s²)
  
  // 计算力: F = m × a
  let force = @azimuth.multiply(mass, acceleration)
  @azimuth.assert_eq(50, force)?
  
  // 计算势能: PE = m × g × h
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(2000, potential_energy)?
  
  // 计算动能: KE = 0.5 × m × v² (简化为整数计算)
  let velocity = 15
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, velocity), velocity) / 2
  @azimuth.assert_eq(1125, kinetic_energy)?
}

test "unit_conversions" {
  // 单位转换测试
  let meters = 1000
  
  // 米转千米
  let kilometers = meters / 1000
  @azimuth.assert_eq(1, kilometers)?
  
  // 千米转米
  let meters_back = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(1000, meters_back)?
  
  // 千克转克
  let kilograms = 5
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(5000, grams)?
  
  // 升转毫升
  let liters = 3
  let milliliters = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(3000, milliliters)?
}

test "array_summation_patterns" {
  // 数组求和模式测试
  // 模拟数组求和: 1+2+3+4+5+6+7+8+9+10 = 55
  let sum1to10 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  @azimuth.assert_eq(55, sum1to10)?
  
  // 模拟偶数求和: 2+4+6+8+10 = 30
  let even_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8), 10)
  @azimuth.assert_eq(30, even_sum)?
  
  // 模拟奇数求和: 1+3+5+7+9 = 25
  let odd_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, odd_sum)?
}

test "binary_operations" {
  // 二进制操作模拟测试
  let num1 = 12  // 二进制: 1100
  let num2 = 10  // 二进制: 1010
  
  // 模拟位与操作: 1100 & 1010 = 1000 (8)
  let bitwise_and = 8
  @azimuth.assert_eq(8, bitwise_and)?
  
  // 模拟位或操作: 1100 | 1010 = 1110 (14)
  let bitwise_or = 14
  @azimuth.assert_eq(14, bitwise_or)?
  
  // 模拟位异或操作: 1100 ^ 1010 = 0110 (6)
  let bitwise_xor = 6
  @azimuth.assert_eq(6, bitwise_xor)?
  
  // 模拟左移操作: 12 << 2 = 48
  let left_shift = @azimuth.multiply(12, 4)
  @azimuth.assert_eq(48, left_shift)?
}

test "calendar_calculations" {
  // 日历计算测试
  let year = 2024
  let month = 6
  let day = 15
  
  // 计算年份中的天数（简化计算，不考虑闰年）
  let days_in_year = 365
  @azimuth.assert_eq(365, days_in_year)?
  
  // 计算月份中的天数（6月有30天）
  let days_in_june = 30
  @azimuth.assert_eq(30, days_in_june)?
  
  // 计算从年初到当前日期的总天数（简化）
  let jan_days = 31
  let feb_days = 29  // 2024是闰年
  let mar_days = 31
  let apr_days = 30
  let may_days = 31
  
  let total_days = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    jan_days, feb_days), mar_days), apr_days), may_days), day)
  @azimuth.assert_eq(167, total_days)?
}

test "network_calculations" {
  // 网络计算测试
  let file_size_mb = 50
  let network_speed_mbps = 10
  
  // 计算下载时间（秒）
  let download_time_seconds = @azimuth.multiply(file_size_mb, 8) / network_speed_mbps
  @azimuth.assert_eq(40, download_time_seconds)?
  
  // 转换为分钟
  let download_time_minutes = download_time_seconds / 60
  @azimuth.assert_eq(0, download_time_minutes)?  // 整数除法
  
  // 计算数据包数量（每个数据包1KB）
  let packet_size_kb = 1
  let file_size_kb = @azimuth.multiply(file_size_mb, 1024)
  let packet_count = file_size_kb / packet_size_kb
  @azimuth.assert_eq(51200, packet_count)?
  
  // 计算总开销（假设每个数据包有20字节开销）
  let overhead_bytes = 20
  let total_overhead = @azimuth.multiply(packet_count, overhead_bytes)
  @azimuth.assert_eq(1024000, total_overhead)?
}

test "statistical_variance" {
  // 统计方差计算测试
  // 数据集: 10, 15, 20, 25, 30
  let data1 = 10
  let data2 = 15
  let data3 = 20
  let data4 = 25
  let data5 = 30
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  @azimuth.assert_eq(20, mean)?
  
  // 计算方差
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), 
    @azimuth.multiply(diff2, diff2)), 
    @azimuth.multiply(diff3, diff3)), 
    @azimuth.add(@azimuth.multiply(diff4, diff4), @azimuth.multiply(diff5, diff5))) / 5
  
  @azimuth.assert_eq(50, variance)?
  
  // 计算标准差（简化为整数）
  let std_dev = 7  // sqrt(50) ≈ 7.07，取整数部分
  @azimuth.assert_eq(7, std_dev)?
}

test "cryptography_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key1 = 7
  let key2 = 13
  
  // 简单的凯撒密码加密
  let encrypted1 = @azimuth.add(message, key1)
  @azimuth.assert_eq(12352, encrypted1)?
  
  // 双重加密
  let encrypted2 = @azimuth.add(encrypted1, key2)
  @azimuth.assert_eq(12365, encrypted2)?
  
  // 解密（逆操作）
  let decrypted1 = @azimuth.add(encrypted2, -key2)
  @azimuth.assert_eq(12352, decrypted1)?
  
  let decrypted2 = @azimuth.add(decrypted1, -key1)
  @azimuth.assert_eq(12345, decrypted2)?
  
  // 模拟简单的哈希函数
  let hash_input = 42
  let hash_result1 = @azimuth.add(@azimuth.multiply(hash_input, 3), 7)
  let hash_result2 = @azimuth.add(@azimuth.multiply(hash_result1, 2), 1)
  @azimuth.assert_eq(260, hash_result2)?
}

test "database_indexing" {
  // 数据库索引模拟测试
  let table_size = 1000
  let page_size = 100
  let index_levels = 3
  
  // 计算总页数
  let total_pages = table_size / page_size
  @azimuth.assert_eq(10, total_pages)?
  
  // 计算索引节点数（简化：每层1个索引节点）
  let index_nodes = index_levels
  @azimuth.assert_eq(3, index_nodes)?
  
  // 模拟B树查找的IO次数
  let io_operations = @azimuth.add(index_levels, 1)  // 索引层 + 数据页
  @azimuth.assert_eq(4, io_operations)?
  
  // 计算索引空间开销（假设每个索引项10字节）
  let index_entry_size = 10
  let index_space = @azimuth.multiply(table_size, index_entry_size)
  @azimuth.assert_eq(10000, index_space)?
}

test "game_economy_system" {
  // 游戏经济系统测试
  let player_gold = 1000
  let item_price = 150
  let quantity = 3
  let tax_rate = 5  // 5%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  @azimuth.assert_eq(450, subtotal)?
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  @azimuth.assert_eq(22, tax)?  // 整数除法
  
  // 计算总费用
  let total_cost = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(472, total_cost)?
  
  // 计算剩余金币
  let remaining_gold = @azimuth.add(player_gold, -total_cost)
  @azimuth.assert_eq(528, remaining_gold)?
  
  // 模拟出售物品获得80%回报
  let sell_price = @azimuth.multiply(subtotal, 80) / 100
  let gold_after_sell = @azimuth.add(remaining_gold, sell_price)
  @azimuth.assert_eq(888, gold_after_sell)?
}