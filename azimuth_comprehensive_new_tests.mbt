// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "string_length_validation" {
  // 字符串长度验证测试
  let short_name = "Bob"
  let medium_name = "Elizabeth"
  let long_name = "Alexander"
  let empty_name = ""
  
  // 测试不同长度字符串的问候
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(short_name))
  @azimuth.assert_eq_string("Hello, Elizabeth!", @azimuth.greet(medium_name))
  @azimuth.assert_eq_string("Hello, Alexander!", @azimuth.greet(long_name))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_name))
}

test "number_sequence_patterns" {
  // 数字序列模式测试
  
  // 奇数序列: 1 + 3 + 5 + 7 + 9 = 25
  let odd_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, odd_sum)
  
  // 偶数序列: 2 + 4 + 6 + 8 = 20
  let even_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8)
  @azimuth.assert_eq(20, even_sum)
  
  // 质数序列: 2 + 3 + 5 + 7 = 17
  let prime_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 3), 5), 7)
  @azimuth.assert_eq(17, prime_sum)
}

test "percentage_calculations" {
  // 百分比计算测试
  
  let base_value = 200
  
  // 计算百分比: 25% of 200 = 50
  let quarter_percent = @azimuth.multiply(base_value, 25) / 100
  @azimuth.assert_eq(50, quarter_percent)
  
  // 计算百分比: 50% of 200 = 100
  let half_percent = @azimuth.multiply(base_value, 50) / 100
  @azimuth.assert_eq(100, half_percent)
  
  // 计算百分比: 75% of 200 = 150
  let three_quarter_percent = @azimuth.multiply(base_value, 75) / 100
  @azimuth.assert_eq(150, three_quarter_percent)
  
  // 计算百分比: 100% of 200 = 200
  let full_percent = @azimuth.multiply(base_value, 100) / 100
  @azimuth.assert_eq(200, full_percent)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2    // hours
  let distance = @azimuth.multiply(speed, time)
  @azimuth.assert_eq(120, distance)
  
  // 时间 = 距离 / 速度
  let distance2 = 180  // km
  let speed2 = 60      // km/h
  let time2 = distance2 / speed2
  @azimuth.assert_eq(3, time2)
  
  // 速度 = 距离 / 时间
  let distance3 = 240  // km
  let time3 = 4        // hours
  let speed3 = distance3 / time3
  @azimuth.assert_eq(60, speed3)
}

test "array_index_calculations" {
  // 数组索引计算测试（模拟）
  
  let array_size = 10
  
  // 第一个元素索引
  let first_index = 0
  @azimuth.assert_eq(0, first_index)
  
  // 最后一个元素索引
  let last_index = @azimuth.add(array_size, -1)
  @azimuth.assert_eq(9, last_index)
  
  // 中间元素索引
  let middle_index = array_size / 2
  @azimuth.assert_eq(5, middle_index)
  
  // 四分之一位置索引
  let quarter_index = array_size / 4
  @azimuth.assert_eq(2, quarter_index)
}

test "binary_operations" {
  // 二进制操作测试（模拟）
  
  // 左移操作（模拟为乘以2的幂）
  let value = 5
  let shift_left_1 = @azimuth.multiply(value, 2)    // 5 << 1 = 10
  let shift_left_2 = @azimuth.multiply(value, 4)    // 5 << 2 = 20
  let shift_left_3 = @azimuth.multiply(value, 8)    // 5 << 3 = 40
  
  @azimuth.assert_eq(10, shift_left_1)
  @azimuth.assert_eq(20, shift_left_2)
  @azimuth.assert_eq(40, shift_left_3)
  
  // 右移操作（模拟为除以2的幂）
  let value2 = 40
  let shift_right_1 = value2 / 2    // 40 >> 1 = 20
  let shift_right_2 = value2 / 4    // 40 >> 2 = 10
  let shift_right_3 = value2 / 8    // 40 >> 3 = 5
  
  @azimuth.assert_eq(20, shift_right_1)
  @azimuth.assert_eq(10, shift_right_2)
  @azimuth.assert_eq(5, shift_right_3)
}

test "currency_conversion" {
  // 货币转换测试
  
  // 美元到人民币转换（假设汇率: 1 USD = 7 CNY）
  let usd_amount = 100
  let exchange_rate = 7
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 人民币到美元转换
  let cny_amount2 = 1400
  let usd_amount2 = cny_amount2 / exchange_rate
  @azimuth.assert_eq(200, usd_amount2)
  
  // 欧元到美元转换（假设汇率: 1 EUR = 1.1 USD，简化为整数）
  let eur_amount = 100
  let eur_to_usd_rate = 11  // 1.1 * 10
  let usd_from_eur = @azimuth.multiply(eur_amount, eur_to_usd_rate) / 10
  @azimuth.assert_eq(110, usd_from_eur)
}

test "calendar_calculations" {
  // 日历计算测试
  
  // 一年的天数（不考虑闰年）
  let days_in_year = 365
  @azimuth.assert_eq(365, days_in_year)
  
  // 一年的周数
  let weeks_in_year = days_in_year / 7
  @azimuth.assert_eq(52, weeks_in_year)  // 365/7 = 52.14，取整数部分
  
  // 一个月的天数（假设30天）
  let days_in_month = 30
  @azimuth.assert_eq(30, days_in_month)
  
  // 一季度的天数
  let days_in_quarter = @azimuth.multiply(days_in_month, 3)
  @azimuth.assert_eq(90, days_in_quarter)
  
  // 半年的天数
  let days_in_half_year = @azimuth.multiply(days_in_month, 6)
  @azimuth.assert_eq(180, days_in_half_year)
}

test "unit_conversions" {
  // 单位转换测试
  
  // 长度转换: 米到厘米
  let meters = 5
  let centimeters = @azimuth.multiply(meters, 100)
  @azimuth.assert_eq(500, centimeters)
  
  // 重量转换: 千克到克
  let kilograms = 3
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(3000, grams)
  
  // 容量转换: 升到毫升
  let liters = 2
  let milliliters = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(2000, milliliters)
  
  // 面积转换: 平方米到平方厘米
  let square_meters = 4
  let square_centimeters = @azimuth.multiply(square_meters, 10000)
  @azimuth.assert_eq(40000, square_centimeters)
}

test "logical_operations" {
  // 逻辑操作测试（模拟）
  
  // AND 操作（模拟为乘法）
  let true_val = 1
  let false_val = 0
  
  let true_and_true = @azimuth.multiply(true_val, true_val)
  let true_and_false = @azimuth.multiply(true_val, false_val)
  let false_and_true = @azimuth.multiply(false_val, true_val)
  let false_and_false = @azimuth.multiply(false_val, false_val)
  
  @azimuth.assert_eq(1, true_and_true)    // 1 AND 1 = 1
  @azimuth.assert_eq(0, true_and_false)   // 1 AND 0 = 0
  @azimuth.assert_eq(0, false_and_true)   // 0 AND 1 = 0
  @azimuth.assert_eq(0, false_and_false)  // 0 AND 0 = 0
  
  // OR 操作（模拟为加法，但限制最大值为1）
  let or_1_1 = @azimuth.add(true_val, true_val) > 0 ? 1 : 0
  let or_1_0 = @azimuth.add(true_val, false_val) > 0 ? 1 : 0
  let or_0_1 = @azimuth.add(false_val, true_val) > 0 ? 1 : 0
  let or_0_0 = @azimuth.add(false_val, false_val) > 0 ? 1 : 0
  
  @azimuth.assert_eq(1, or_1_1)  // 1 OR 1 = 1
  @azimuth.assert_eq(1, or_1_0)  // 1 OR 0 = 1
  @azimuth.assert_eq(1, or_0_1)  // 0 OR 1 = 1
  @azimuth.assert_eq(0, or_0_0)  // 0 OR 0 = 0
}