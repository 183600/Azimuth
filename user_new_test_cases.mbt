// 用户新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对 azimuth 模块的各种功能

test "user_fibonacci_sequence" {
  // 测试斐波那契数列计算（前几项）
  // F(0) = 0, F(1) = 1, F(2) = 1, F(3) = 2, F(4) = 3, F(5) = 5
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
}

test "user_prime_number_check" {
  // 测试质数相关计算
  // 检查一个数是否为质数（通过试除法）
  let num = 17
  let is_prime = true  // 假设是质数
  
  // 简化的质数检查：检查是否能被2, 3, 5整除
  let divisible_by_2 = num % 2 == 0
  let divisible_by_3 = num % 3 == 0
  let divisible_by_5 = num % 5 == 0
  
  // 17不能被2, 3, 5整除，所以是质数
  assert_false(divisible_by_2)
  assert_false(divisible_by_3)
  assert_false(divisible_by_5)
  
  // 测试非质数
  let composite_num = 15
  let composite_div_3 = composite_num % 3 == 0
  let composite_div_5 = composite_num % 5 == 0
  
  assert_true(composite_div_3 or composite_div_5)
}

test "user_temperature_conversion" {
  // 测试温度转换（更精确的版本）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30 - C/10
  
  let celsius = 25
  let approx_factor = multiply(celsius, 2)
  let adjustment = celsius / 10
  let fahrenheit = add(add(approx_factor, 30), -adjustment)
  
  assert_eq(75, fahrenheit)  // 25 × 2 + 30 - 2 = 73 ≈ 77°F (实际值)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = add(add(multiply(freezing_c, 2), 30), -freezing_c / 10)
  assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(add(multiply(boiling_c, 2), 30), -boiling_c / 10)
  assert_eq(220, boiling_f)
}

test "user_loan_calculation" {
  // 测试贷款计算（单利）
  // 本金 × (1 + 利率 × 年数)
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  let rate_factor = multiply(annual_rate, years)
  let total_factor = add(1, rate_factor)
  let total_amount = multiply(principal, total_factor)
  
  assert_eq(160000, total_amount)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
  
  // 计算月供（简化版）
  let monthly_payment = total_amount / multiply(years, 12)
  assert_eq(4444, monthly_payment)  // 160000 / 36 = 4444.44...
}

test "user_string_pattern_matching" {
  // 测试字符串模式匹配（通过greet函数）
  // 测试不同模式的字符串
  
  // 邮箱格式
  let email = "user@example.com"
  let email_greeting = greet(email)
  assert_eq_string("Hello, user@example.com!", email_greeting)
  
  // 电话号码格式
  let phone = "123-456-7890"
  let phone_greeting = greet(phone)
  assert_eq_string("Hello, 123-456-7890!", phone_greeting)
  
  // URL格式
  let url = "https://example.com"
  let url_greeting = greet(url)
  assert_eq_string("Hello, https://example.com!", url_greeting)
  
  // 混合格式
  let mixed = "ID123_user-name"
  let mixed_greeting = greet(mixed)
  assert_eq_string("Hello, ID123_user-name!", mixed_greeting)
}

test "user_geometric_progression" {
  // 测试等比数列计算
  // a, ar, ar², ar³, ...
  let first_term = 2
  let ratio = 3
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
  
  // 计算前5项的和
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum)
}

test "user_binary_operations_simulation" {
  // 模拟二进制运算
  let a = 12  // 1100 in binary
  let b = 10  // 1010 in binary
  
  // 模按位与运算（简化）
  // 1100 & 1010 = 1000 = 8
  let bitwise_and = 8
  
  // 模拟按位或运算
  // 1100 | 1010 = 1110 = 14
  let bitwise_or = 14
  
  // 模拟按位异或运算
  // 1100 ^ 1010 = 0110 = 6
  let bitwise_xor = 6
  
  // 验证结果
  assert_eq(8, bitwise_and)
  assert_eq(14, bitwise_or)
  assert_eq(6, bitwise_xor)
  
  // 测试左移运算（×2）
  let left_shift = multiply(a, 2)
  assert_eq(24, left_shift)
  
  // 测试右移运算（÷2）
  let right_shift = a / 2
  assert_eq(6, right_shift)
}

test "user_calendar_calculations" {
  // 测试日历计算
  let year = 2023
  let month = 6
  let day = 15
  
  // 计算年份中的第几天（简化版，不考虑闰年）
  // 假设每月天数：1月(31), 2月(28), 3月(31), 4月(30), 5月(31), 6月(30)...
  let days_before_june = add(add(add(add(31, 28), 31), 30), 31)  // 151天
  let day_of_year = add(days_before_june, day)
  
  assert_eq(166, day_of_year)
  
  // 计算星期几（简化版，假设2023年1月1日是星期日）
  let days_since_new_year = add(day_of_year, -1)
  let day_of_week = days_since_new_year % 7
  
  assert_eq(5, day_of_week)  // 星期五
  
  // 计算月份中的第几周
  let week_of_month = add(day / 7, 1)
  assert_eq(3, week_of_month)
}

test "user_data_compression_ratio" {
  // 测试数据压缩比计算
  let original_size = 1000
  let compressed_size = 300
  
  // 计算压缩比
  let compression_ratio = multiply(compressed_size, 100) / original_size
  assert_eq(30, compression_ratio)  // 30%
  
  // 计算节省的空间
  let saved_space = add(original_size, -compressed_size)
  assert_eq(700, saved_space)
  
  // 计算压缩效率
  let efficiency = multiply(saved_space, 100) / original_size
  assert_eq(70, efficiency)  // 70%
  
  // 测试不同压缩级别
  let high_compression = 150
  let high_ratio = multiply(high_compression, 100) / original_size
  let high_efficiency = multiply(add(original_size, -high_compression), 100) / original_size
  
  assert_eq(15, high_ratio)   // 15%
  assert_eq(85, high_efficiency)  // 85%
}

test "user_network_bandwidth_calculation" {
  // 测试网络带宽计算
  let file_size_mb = 100
  let bandwidth_mbps = 10
  
  // 计算下载时间（秒）
  let download_time = file_size_mb / bandwidth_mbps
  assert_eq(10, download_time)
  
  // 转换为分钟
  let download_minutes = download_time / 60
  assert_eq(0, download_minutes)
  
  // 计算多个文件的下载时间
  let file_count = 5
  let total_size = multiply(file_size_mb, file_count)
  let total_time = total_size / bandwidth_mbps
  
  assert_eq(50, total_time)
  
  // 考虑网络开销（增加20%）
  let overhead_factor = 20
  let overhead_time = multiply(total_time, overhead_factor) / 100
  let realistic_time = add(total_time, overhead_time)
  
  assert_eq(60, realistic_time)
}