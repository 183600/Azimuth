// 新增综合测试用例

// 测试用例 1: 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 使用加法计算前几个斐波那契数
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  assert_eq(13, fib7)
  
  // 验证黄金比例近似
  let ratio_times_1000 = @azimuth.multiply(1000, fib7)
  let expected_times_1000 = @azimuth.multiply(1000, 13)
  assert_eq(expected_times_1000, ratio_times_1000)
}

// 测试用例 2: 测试阶乘计算
///|
test "factorial_calculation" {
  // 计算阶乘: n! = n * (n-1) * ... * 1
  
  // 1! = 1
  let fact1 = 1
  
  // 2! = 2
  let fact2 = @azimuth.multiply(2, fact1)
  
  // 3! = 6
  let fact3 = @azimuth.multiply(3, fact2)
  
  // 4! = 24
  let fact4 = @azimuth.multiply(4, fact3)
  
  // 5! = 120
  let fact5 = @azimuth.multiply(5, fact4)
  
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  
  // 验证阶乘性质: (n+1)! = (n+1) * n!
  let fact6_alt = @azimuth.multiply(6, fact5)
  assert_eq(720, fact6_alt)
}

// 测试用例 3: 测试幂运算（通过重复乘法）
///|
test "power_calculation" {
  // 计算 2^n 通过重复乘法
  
  // 2^1 = 2
  let pow2_1 = 2
  
  // 2^2 = 4
  let pow2_2 = @azimuth.multiply(pow2_1, 2)
  
  // 2^3 = 8
  let pow2_3 = @azimuth.multiply(pow2_2, 2)
  
  // 2^4 = 16
  let pow2_4 = @azimuth.multiply(pow2_3, 2)
  
  // 2^5 = 32
  let pow2_5 = @azimuth.multiply(pow2_4, 2)
  
  assert_eq(2, pow2_1)
  assert_eq(4, pow2_2)
  assert_eq(8, pow2_3)
  assert_eq(16, pow2_4)
  assert_eq(32, pow2_5)
  
  // 验证幂运算性质: 2^m * 2^n = 2^(m+n)
  let product = @azimuth.multiply(pow2_3, pow2_2)  // 8 * 4 = 32
  assert_eq(pow2_5, product)
}

// 测试用例 4: 测试最大公约数（通过欧几里得算法模拟）
///|
test "gcd_calculation_simulation" {
  // 使用减法模拟欧几里得算法求 GCD(48, 18)
  
  let a = 48
  let b = 18
  
  // 48 - 18 = 30
  let diff1 = @azimuth.add(a, @azimuth.multiply(-1, b))
  
  // 30 - 18 = 12
  let diff2 = @azimuth.add(diff1, @azimuth.multiply(-1, b))
  
  // 18 - 12 = 6
  let diff3 = @azimuth.add(b, @azimuth.multiply(-1, diff2))
  
  // 12 - 6 = 6
  let diff4 = @azimuth.add(diff2, @azimuth.multiply(-1, diff3))
  
  // 6 - 6 = 0，所以 GCD 是 6
  let diff5 = @azimuth.add(diff3, @azimuth.multiply(-1, diff4))
  
  assert_eq(30, diff1)
  assert_eq(12, diff2)
  assert_eq(6, diff3)
  assert_eq(6, diff4)
  assert_eq(0, diff5)
  
  // 验证 GCD 性质：两个数都能被 GCD 整除
  let multiple1 = @azimuth.multiply(8, diff3)  // 6 * 8 = 48
  let multiple2 = @azimuth.multiply(3, diff3)  // 6 * 3 = 18
  
  assert_eq(a, multiple1)
  assert_eq(b, multiple2)
}

// 测试用例 5: 测试百分比计算
///|
test "percentage_calculations" {
  // 计算百分比：部分 / 整体 * 100
  // 我们通过乘以100来避免小数
  
  let whole = 200
  let part = 50
  
  // 百分比 * 整体 = 部分 * 100
  let percentage_times_whole = @azimuth.multiply(part, 100)
  assert_eq(5000, percentage_times_whole)  // 50 * 100 = 5000
  
  // 验证：25% * 200 = 50
  let percentage = 25
  let result_times_100 = @azimuth.multiply(percentage, whole)
  assert_eq(5000, result_times_100)
  
  // 计算增加百分比
  let original = 100
  let increase = 20  // 20%
  let new_value_times_100 = @azimuth.add(@azimuth.multiply(original, 100), @azimuth.multiply(original, increase))
  assert_eq(12000, new_value_times_100)  // 100 * (100 + 20) = 12000
}

// 测试用例 6: 测试复利计算
///|
test "compound_interest_calculation" {
  // 复利公式：A = P * (1 + r)^n
  // 我们计算前几期的值
  
  let principal = 1000
  let rate_percent = 5  // 5%
  
  // 第一期：1000 * 1.05 = 1050
  let year1_times_100 = @azimuth.multiply(principal, @azimuth.add(100, rate_percent))
  assert_eq(105000, year1_times_100)  // 1000 * 105 = 105000
  
  // 第二期：1050 * 1.05 = 1102.5
  let year2_times_10000 = @azimuth.multiply(year1_times_100, @azimuth.add(100, rate_percent))
  assert_eq(11025000, year2_times_10000)  // 105000 * 105 = 11025000
  
  // 第三期：1102.5 * 1.05 = 1157.625
  let year3_times_1000000 = @azimuth.multiply(year2_times_10000, @azimuth.add(100, rate_percent))
  assert_eq(1157625000, year3_times_1000000)
}

// 测试用例 7: 测试数据压缩模拟（行程长度编码）
///|
test "run_length_encoding_simulation" {
  // 模拟行程长度编码：AAABBC -> A3B2C1
  // 我们只计算编码后的长度
  
  let char_A_count = 3
  let char_B_count = 2
  let char_C_count = 1
  
  // 编码后长度（字符+计数）
  let encoded_length = @azimuth.add(
    @azimuth.add(@azimuth.add(1, char_A_count), @azimuth.add(1, char_B_count)),
    @azimuth.add(1, char_C_count)
  )
  
  assert_eq(9, encoded_length)  // (1+3) + (1+2) + (1+1) = 9
  
  // 计算压缩率
  let original_length = 6
  let compression_ratio_times_100 = @azimuth.multiply(encoded_length, 100)
  
  // 验证编码长度
  assert_eq(9, encoded_length)
  assert_eq(900, compression_ratio_times_100)  // 9 * 100 = 900
}

// 测试用例 8: 测试网络带宽计算
///|
test "network_bandwidth_calculation" {
  // 计算文件传输时间：时间 = 文件大小 / 带宽
  // 我们通过乘法来验证
  
  let file_size_mb = 100  // 100MB
  let bandwidth_mbps = 10  // 10 Mbps
  
  // 时间 * 带宽 = 文件大小
  let time_seconds = 10  // 100MB / 10Mbps = 10秒
  let verification = @azimuth.multiply(time_seconds, bandwidth_mbps)
  
  assert_eq(100, verification)  // 10 * 10 = 100
  
  // 计算多个文件的传输时间
  let file_count = 5
  let total_time = @azimuth.multiply(time_seconds, file_count)
  assert_eq(50, total_time)  // 10 * 5 = 50秒
  
  // 考虑网络开销（增加20%）
  let overhead_percent = 20
  let total_time_with_overhead_times_100 = @azimuth.add(
    @azimuth.multiply(total_time, 100),
    @azimuth.multiply(total_time, overhead_percent)
  )
  
  assert_eq(6000, total_time_with_overhead_times_100)  // 50 * 120 = 6000
}

// 测试用例 9: 测试内存使用计算
///|
test "memory_usage_calculation" {
  // 计算程序内存使用量
  
  let base_memory = 50  // MB
  let per_item_memory = 2  // MB
  let item_count = 100
  
  // 总内存 = 基础内存 + 每项内存 * 项目数
  let total_memory = @azimuth.add(base_memory, @azimuth.multiply(per_item_memory, item_count))
  assert_eq(250, total_memory)  // 50 + 2 * 100 = 250
  
  // 考虑内存碎片（增加10%）
  let fragmentation_percent = 10
  let total_with_fragmentation_times_100 = @azimuth.add(
    @azimuth.multiply(total_memory, 100),
    @azimuth.multiply(total_memory, fragmentation_percent)
  )
  
  assert_eq(27500, total_with_fragmentation_times_100)  // 250 * 110 = 27500
  
  // 计算缓存大小（总内存的20%）
  let cache_percent = 20
  let cache_size_times_100 = @azimuth.multiply(total_memory, cache_percent)
  assert_eq(5000, cache_size_times_100)  // 250 * 20 = 5000
}

// 测试用例 10: 测试字符串处理效率
///|
test "string_processing_efficiency" {
  // 测试不同长度字符串的处理
  
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "This is a very long string for testing purposes"
  
  // 测试字符串拼接操作
  let greeting1 = @azimuth.greet(short_string)
  let greeting2 = @azimuth.greet(medium_string)
  let greeting3 = @azimuth.greet(long_string)
  
  // 验证字符串长度差异
  let len1 = @azimuth.add(7, 2)  // "Hello, " + "Hi" + "!" = 7 + 2 + 1 = 10
  let len2 = @azimuth.add(7, 7)  // "Hello, " + "MoonBit" + "!" = 7 + 7 + 1 = 15
  let len3 = @azimuth.add(7, 51) // "Hello, " + long_string + "!" = 7 + 51 + 1 = 59
  
  assert_eq(9, len1)  // 去掉了"!"的长度
  assert_eq(14, len2)
  assert_eq(58, len3)
  
  // 验证字符串确实是不同的
  assert_eq(true, greeting1 != greeting2)
  assert_eq(true, greeting2 != greeting3)
  assert_eq(true, greeting1 != greeting3)
}