// 用户增强测试用例 - Azimuth 项目
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "binary_number_operations" {
  // 二进制数运算测试
  let binary1 = 8  // 1000 in binary
  let binary2 = 3  // 0011 in binary
  
  // 模拟位运算：左移相当于乘以2
  let left_shift = azimuth.multiply(binary1, 2)
  @assertion.assert_eq(16, left_shift)?
  
  // 模拟位运算：右移相当于除以2
  let right_shift = binary1 / 2
  @assertion.assert_eq(4, right_shift)?
  
  // 测试二进制加法
  let binary_sum = azimuth.add(binary1, binary2)
  @assertion.assert_eq(11, binary_sum)?
}

test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 100
  let exchange_rate_eur = 0.85  // 简化为整数：85
  let exchange_rate_gbp = 0.75  // 简化为整数：75
  let fee_percentage = 2        // 2%手续费
  
  // 转换为欧元（使用整数计算）
  let eur_amount = azimuth.multiply(usd_amount, 85) / 100
  @assertion.assert_eq(85, eur_amount)?
  
  // 转换为英镑（使用整数计算）
  let gbp_amount = azimuth.multiply(usd_amount, 75) / 100
  @assertion.assert_eq(75, gbp_amount)?
  
  // 计算手续费
  let fee = azimuth.multiply(eur_amount, fee_percentage) / 100
  let final_eur = azimuth.add(eur_amount, -fee)
  @assertion.assert_eq(83, final_eur)?
}

test "physics_motion_calculation" {
  // 物理运动计算测试
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // seconds
  
  // 计算最终速度：v = u + at
  let final_velocity = azimuth.add(initial_velocity, azimuth.multiply(acceleration, time))
  @assertion.assert_eq(20, final_velocity)?
  
  // 计算位移：s = ut + 0.5at²（简化为整数计算）
  let displacement = azimuth.add(
    azimuth.multiply(initial_velocity, time),
    azimuth.multiply(azimuth.multiply(acceleration, time), time) / 2
  )
  @assertion.assert_eq(75, displacement)?
}

test "data_structure_indexing" {
  // 数据结构索引计算测试
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算二维数组在一维数组中的索引
  let linear_index = azimuth.add(azimuth.multiply(row_index, cols), col_index)
  @assertion.assert_eq(13, linear_index)?
  
  // 测试边界索引
  let first_index = azimuth.add(azimuth.multiply(0, cols), 0)
  @assertion.assert_eq(0, first_index)?
  
  let last_index = azimuth.add(azimuth.multiply(azimuth.add(rows, -1), cols), azimuth.add(cols, -1))
  @assertion.assert_eq(19, last_index)?
}

test "statistical_median_calculation" {
  // 统计中位数计算测试
  // 奇数个数据点的中位数
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 20
  let data5 = 25
  
  // 排序后的中位数是第三个数据点
  let median_odd = data3
  @assertion.assert_eq(18, median_odd)?
  
  // 偶数个数据点的中位数（简化为取中间两个的平均值）
  let data6 = 28
  // 中位数 = (第3个 + 第4个) / 2 = (18 + 20) / 2 = 19
  let median_even = azimuth.add(data3, data4) / 2
  @assertion.assert_eq(19, median_even)?
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 10
  
  // O(1) 常数时间复杂度
  let constant_time = 42
  @assertion.assert_eq(42, constant_time)?
  
  // O(n) 线性时间复杂度：1 + 2 + ... + n = n*(n+1)/2
  let linear_sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(55, linear_sum)?
  
  // O(n²) 平方时间复杂度：1² + 2² + ... + n²
  let square1 = azimuth.multiply(1, 1)
  let square2 = azimuth.multiply(2, 2)
  let square3 = azimuth.multiply(3, 3)
  let square_sum = azimuth.add(azimuth.add(azimuth.add(square1, square2), square3), 
    azimuth.add(azimuth.multiply(4, 4), azimuth.multiply(5, 5)))
  @assertion.assert_eq(55, square_sum)?
}

test "date_time_arithmetic" {
  // 日期时间算术测试
  let base_day = 1
  let base_month = 1
  let base_year = 2023
  
  // 添加天数
  let days_to_add = 45
  let new_day = azimuth.add(base_day, days_to_add)
  
  // 简化的日期计算（不考虑月份长度）
  let expected_day = 46
  @assertion.assert_eq(expected_day, new_day)?
  
  // 计算时间差（小时）
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total = azimuth.add(azimuth.multiply(start_hour, 60), start_minute)
  let end_total = azimuth.add(azimuth.multiply(end_hour, 60), end_minute)
  let hour_diff = azimuth.add(end_total, -start_total) / 60
  
  @assertion.assert_eq(5, hour_diff)?
}

test "string_pattern_matching" {
  // 字符串模式匹配测试
  let pattern = "test"
  let input1 = "this is a test string"
  let input2 = "this is a TEST string"
  let input3 = "no match here"
  
  // 使用greet函数测试不同的字符串输入
  let result1 = azimuth.greet(input1)
  let result2 = azimuth.greet(input2)
  let result3 = azimuth.greet(input3)
  
  @assertion.assert_eq_string("Hello, this is a test string!", result1)?
  @assertion.assert_eq_string("Hello, this is a TEST string!", result2)?
  @assertion.assert_eq_string("Hello, no match here!", result3)?
  
  // 测试空字符串和特殊字符
  let empty_result = azimuth.greet("")
  let special_result = azimuth.greet("@#$%^&*()")
  
  @assertion.assert_eq_string("Hello, !", empty_result)?
  @assertion.assert_eq_string("Hello, @#$%^&*()!", special_result)?
}

test "network_protocol_simulation" {
  // 网络协议模拟测试
  let packet_size = 1024
  let header_size = 20
  let payload_size = azimuth.add(packet_size, -header_size)
  
  @assertion.assert_eq(1004, payload_size)?
  
  // 计算校验和（简化）
  let data1 = 100
  let data2 = 200
  let data3 = 300
  let checksum = azimuth.add(azimuth.add(data1, data2), data3)
  
  @assertion.assert_eq(600, checksum)?
  
  // 计算传输时间（简化）
  let bandwidth = 1000  // bytes per second
  let transmission_time = packet_size / bandwidth
  
  @assertion.assert_eq(1, transmission_time)?
}

test "game_inventory_management" {
  // 游戏物品管理测试
  let max_slots = 20
  let used_slots = 15
  let item_stack_size = 64
  let item_count = 150
  
  // 计算剩余槽位
  let available_slots = azimuth.add(max_slots, -used_slots)
  @assertion.assert_eq(5, available_slots)?
  
  // 计算需要的槽位数
  let required_slots = azimuth.add(item_count, azimuth.add(item_stack_size, -1)) / item_stack_size
  @assertion.assert_eq(3, required_slots)?
  
  // 检查是否有足够空间
  let has_space = required_slots <= available_slots
  @assertion.assert_true(has_space)?
  
  // 计算物品价值
  let item_value = 50
  let total_value = azimuth.multiply(item_count, item_value)
  @assertion.assert_eq(7500, total_value)?
}