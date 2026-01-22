// 新增的 MoonBit 测试用例
// 包含各种实用场景的测试，使用标准 MoonBit 测试语法

test "binary_number_conversion" {
  // 二进制数转换测试：将二进制转换为十进制
  // 二进制 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  // 使用位运算模拟二进制转换
  let binary_1011 = @azimuth.add(
    @azimuth.multiply(1, @azimuth.multiply(2, @azimuth.multiply(2, 2))),  // 1×2³
    @azimuth.add(
      @azimuth.multiply(0, @azimuth.multiply(2, 2)),                     // 0×2²
      @azimuth.add(
        @azimuth.multiply(1, 2),                                          // 1×2¹
        1                                                                 // 1×2⁰
      )
    )
  )
  
  @azimuth.assert_eq(11, binary_1011)
  
  // 二进制 1100 = 1×2³ + 1×2² + 0×2¹ + 0×2⁰ = 8 + 4 + 0 + 0 = 12
  let binary_1100 = @azimuth.add(
    @azimuth.multiply(1, @azimuth.multiply(2, @azimuth.multiply(2, 2))),  // 1×2³
    @azimuth.add(
      @azimuth.multiply(1, @azimuth.multiply(2, 2)),                      // 1×2²
      @azimuth.add(
        @azimuth.multiply(0, 2),                                          // 0×2¹
        0                                                                 // 0×2⁰
      )
    )
  )
  
  @azimuth.assert_eq(12, binary_1100)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试（使用欧几里得算法的简化版本）
  let a = 48
  let b = 18
  
  // 简化的GCD计算：通过连续减法
  let temp_a = a
  let temp_b = b
  
  // 模拟辗转相除法的过程
  // 48 ÷ 18 = 2 余 12
  // 18 ÷ 12 = 1 余 6
  // 12 ÷ 6 = 2 余 0
  // 所以GCD是6
  
  let remainder1 = @azimuth.add(temp_a, @azimuth.multiply(-2, temp_b))  // 48 - 2×18 = 12
  let remainder2 = @azimuth.add(temp_b, @azimuth.multiply(-1, remainder1))  // 18 - 1×12 = 6
  
  @azimuth.assert_eq(6, remainder2)
  
  // 测试另一对数
  let c = 56
  let d = 32
  
  // 56 ÷ 32 = 1 余 24
  // 32 ÷ 24 = 1 余 8
  // 24 ÷ 8 = 3 余 0
  // 所以GCD是8
  
  let remainder3 = @azimuth.add(c, @azimuth.multiply(-1, d))  // 56 - 1×32 = 24
  let remainder4 = @azimuth.add(d, @azimuth.multiply(-1, remainder3))  // 32 - 1×24 = 8
  
  @azimuth.assert_eq(8, remainder4)
}

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1 + 0 = 1
  let f3 = @azimuth.add(f2, f1)  // 1 + 1 = 2
  let f4 = @azimuth.add(f3, f2)  // 2 + 1 = 3
  let f5 = @azimuth.add(f4, f3)  // 3 + 2 = 5
  let f6 = @azimuth.add(f5, f4)  // 5 + 3 = 8
  let f7 = @azimuth.add(f6, f5)  // 8 + 5 = 13
  let f8 = @azimuth.add(f7, f6)  // 13 + 8 = 21
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  
  // 计算斐波那契数列的和
  let sum_first_9 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), f4), f5), f6), f7), f8)
  @azimuth.assert_eq(54, sum_first_9)  // 0+1+1+2+3+5+8+13+21 = 54
}

test "prime_number_check" {
  // 质数检查测试（简化版本）
  // 检查7是否为质数：不能被2、3整除
  let number = 7
  
  // 检查是否能被2整除
  let divisible_by_2 = @azimuth.multiply(number / 2, 2) == number
  @azimuth.assert_eq(0, 1)(divisible_by_2)
  
  // 检查是否能被3整除
  let divisible_by_3 = @azimuth.multiply(number / 3, 3) == number
  @azimuth.assert_eq(0, 1)(divisible_by_3)
  
  // 检查9是否为质数：能被3整除
  let composite_number = 9
  let divisible_by_3_composite = @azimuth.multiply(composite_number / 3, 3) == composite_number
  @azimuth.assert_eq(divisible_by_3_composite, true)
  
  // 检查11是否为质数：不能被2、3整除
  let prime_number = 11
  let divisible_by_2_prime = @azimuth.multiply(prime_number / 2, 2) == prime_number
  let divisible_by_3_prime = @azimuth.multiply(prime_number / 3, 3) == prime_number
  @azimuth.assert_eq(0, 1)(divisible_by_2_prime)
  @azimuth.assert_eq(0, 1)(divisible_by_3_prime)
}

test "calendar_date_calculation" {
  // 日历日期计算测试
  // 计算两个日期之间的天数（简化版本）
  let year1 = 2023
  let month1 = 1
  let day1 = 15
  
  let year2 = 2023
  let month2 = 2
  let day2 = 10
  
  // 简化计算：假设每月30天
  let days_in_month = 30
  
  // 转换为天数
  let total_days1 = @azimuth.add(
    @azimuth.multiply(year1, @azimuth.multiply(12, days_in_month)),
    @azimuth.add(@azimuth.multiply(month1, days_in_month), day1)
  )
  
  let total_days2 = @azimuth.add(
    @azimuth.multiply(year2, @azimuth.multiply(12, days_in_month)),
    @azimuth.add(@azimuth.multiply(month2, days_in_month), day2)
  )
  
  // 计算天数差
  let days_difference = @azimuth.add(total_days2, -total_days1)
  @azimuth.assert_eq(25, days_difference)  // 约25天
  
  // 计算星期几（简化版本）
  let start_day = 0  // 假设1月1日是星期日
  let target_day = @azimuth.add(start_day, @azimuth.multiply(14, 1))  // 1月15日
  let day_of_week = target_day % 7  // 14 % 7 = 0，星期日
  @azimuth.assert_eq(0, day_of_week)
}

test "bank_loan_calculation" {
  // 银行贷款计算测试
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 5           // 贷款年限
  
  // 简化的利息计算：总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(
    principal,
    @azimuth.multiply(annual_rate, years)
  )
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 计算月供（简化版本）
  let total_months = @azimuth.multiply(years, 12)
  let monthly_payment = total_payment / total_months
  
  @azimuth.assert_eq(3000000, total_interest)  // 100000 × 6 × 5 = 3000000
  @azimuth.assert_eq(3100000, total_payment)   // 100000 + 3000000 = 3100000
  @azimuth.assert_eq(51666, monthly_payment)   // 3100000 ÷ 60 ≈ 51666（整数除法）
  
  // 测试提前还款的情况
  let early_payment_months = 36  // 3年后提前还款
  let paid_amount = @azimuth.multiply(monthly_payment, early_payment_months)
  let remaining_balance = @azimuth.add(total_payment, -paid_amount)
  
  @azimuth.assert_eq(1859976, paid_amount)      // 51666 × 36 = 1859976
  @azimuth.assert_eq(1240024, remaining_balance)  // 3100000 - 1859976 = 1240024
}

test "data_compression_simulation" {
  // 数据压缩模拟测试
  let original_size = 10000  // 原始数据大小（字节）
  
  // 模拟压缩率：重复字符压缩
  let repeated_chars = 2000  // 重复字符数量
  let compression_ratio = 8  // 压缩比例
  
  // 计算压缩后的大小
  let compressed_repeated = repeated_chars / compression_ratio  // 2000 ÷ 8 = 250
  let non_repeated_chars = @azimuth.add(original_size, -repeated_chars)  // 10000 - 2000 = 8000
  let compressed_size = @azimuth.add(compressed_repeated, non_repeated_chars)  // 250 + 8000 = 8250
  
  // 计算压缩率
  let actual_compression_ratio = @azimuth.multiply(100, compressed_size) / original_size
  
  @azimuth.assert_eq(8250, compressed_size)
  @azimuth.assert_eq(82, actual_compression_ratio)  // 82.5%，整数除法为82
  
  // 模拟解压缩
  let decompressed_size = @azimuth.add(
    @azimuth.multiply(compressed_repeated, compression_ratio),
    non_repeated_chars
  )
  @azimuth.assert_eq(original_size, decompressed_size)
}

test "network_bandwidth_calculation" {
  // 网络带宽计算测试
  let file_size_mb = 100     // 文件大小（MB）
  let bandwidth_mbps = 10    // 带宽（Mbps）
  
  // 计算下载时间（秒）
  // 注意：1 MB = 8 Mb（兆字节与兆比特的转换）
  let file_size_mbits = @azimuth.multiply(file_size_mb, 8)  // 100 × 8 = 800 Mb
  let download_time_seconds = file_size_mbits / bandwidth_mbps  // 800 ÷ 10 = 80秒
  
  // 转换为分钟和秒
  let download_minutes = download_time_seconds / 60  // 80 ÷ 60 = 1
  let remaining_seconds = download_time_seconds % 60  // 80 % 60 = 20
  
  @azimuth.assert_eq(800, file_size_mbits)
  @azimuth.assert_eq(80, download_time_seconds)
  @azimuth.assert_eq(1, download_minutes)
  @azimuth.assert_eq(20, remaining_seconds)
  
  // 计算多个文件的下载时间
  let file_count = 5
  let total_download_time = @azimuth.multiply(download_time_seconds, file_count)
  @azimuth.assert_eq(400, total_download_time)  // 80 × 5 = 400秒
  
  // 计算带宽利用率（假设实际速度是理论速度的80%）
  let actual_speed = @azimuth.multiply(bandwidth_mbps, 80) / 100  // 10 × 80 ÷ 100 = 8
  let actual_download_time = file_size_mbits / actual_speed  // 800 ÷ 8 = 100秒
  @azimuth.assert_eq(8, actual_speed)
  @azimuth.assert_eq(100, actual_download_time)
}

test "physics_velocity_calculation" {
  // 物理速度计算测试
  let distance_meters = 1000  // 距离（米）
  let time_seconds = 60       // 时间（秒）
  
  // 计算速度（米/秒）
  let velocity_mps = distance_meters / time_seconds
  @azimuth.assert_eq(16, velocity_mps)  // 1000 ÷ 60 = 16（整数除法）
  
  // 转换为公里/小时
  let velocity_kmph = @azimuth.multiply(velocity_mps, 36) / 10  // m/s × 3.6 = km/h
  @azimuth.assert_eq(57, velocity_kmph)  // 16 × 36 ÷ 10 = 57（整数除法）
  
  // 计算加速度（假设从0加速到最终速度）
  let acceleration = velocity_mps / time_seconds  // v/t
  @azimuth.assert_eq(0, acceleration)  // 16 ÷ 60 = 0（整数除法）
  
  // 计算动能（简化版本：E = ½mv²，假设m=1000kg）
  let mass = 1000  // 质量（千克）
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity_mps, velocity_mps)) / 2
  @azimuth.assert_eq(128000, kinetic_energy)  // 1000 × 16² ÷ 2 = 128000
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟测试（简化版本）
  let input_string = "Hello"
  
  // 模拟简单的哈希函数：字符ASCII值之和
  // H = 72 + 101 + 108 + 108 + 111 = 500
  let hash_value = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(72, 101), 108), 108), 111)
  @azimuth.assert_eq(500, hash_value)
  
  // 模拟哈希的雪崩效应：微小变化导致大差异
  let slightly_different_string = "Hellp"  // 最后一个字符不同
  // H = 72 + 101 + 108 + 108 + 112 = 501
  let hash_value_different = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(72, 101), 108), 108), 112)
  @azimuth.assert_eq(501, hash_value_different)
  
  // 计算哈希差异
  let hash_difference = @azimuth.add(hash_value_different, -hash_value)
  @azimuth.assert_eq(1, hash_difference)
  
  // 模拟哈希的模运算（用于固定长度输出）
  let hash_mod_256 = hash_value % 256
  let hash_different_mod_256 = hash_value_different % 256
  
  @azimuth.assert_eq(244, hash_mod_256)        // 500 % 256 = 244
  @azimuth.assert_eq(245, hash_different_mod_256)  // 501 % 256 = 245
  
  // 验证模运算后的差异仍然保持
  let mod_difference = @azimuth.add(hash_different_mod_256, -hash_mod_256)
  @azimuth.assert_eq(1, mod_difference)
}

// 新增的9个测试用例
test "enhanced_add_commutativity" {
  // 测试加法交换律的多种情况
  let a = 12345
  let b = -6789
  
  // 验证交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试包含0的情况
  @azimuth.assert_eq(@azimuth.add(a, 0), @azimuth.add(0, a))
  
  // 测试两个负数
  let x = -1000
  let y = -2000
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "enhanced_multiply_properties" {
  // 测试乘法的数学性质
  let a = 15
  let b = 7
  let c = 3
  
  // 测试结合律 (a * b) * c = a * (b * c)
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试分配律 a * (b + c) = a * b + a * c
  let result3 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result4 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result3, result4)
}

test "enhanced_string_scenarios" {
  // 测试字符串处理的各种场景
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@domain.com!", @azimuth.greet("test@domain.com"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Smith!", @azimuth.greet("John Smith"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "enhanced_financial_calculations" {
  // 测试金融计算场景
  // 计算贷款月供（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  
  // 简化的月供计算：本金 + 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(1900000, total_payment)
  
  // 计算投资回报
  let investment = 50000
  let return_rate = 8     // 8%年回报率
  let investment_years = 5
  
  let returns = @azimuth.multiply(investment, @azimuth.multiply(return_rate, investment_years))
  let final_value = @azimuth.add(investment, returns)
  
  @azimuth.assert_eq(250000, final_value)
}

test "enhanced_temperature_conversions" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
  @azimuth.assert_eq(90, @azimuth.add(@azimuth.multiply(30, 2), 30))
  @azimuth.assert_eq(110, @azimuth.add(@azimuth.multiply(40, 2), 30))
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))
  
  // 华氏度转摄氏度（简化版：C = (F - 30) ÷ 2）
  @azimuth.assert_eq(0, @azimuth.add(@azimuth.multiply(30, -1), 30) / 2)
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.multiply(70, -1), 30) / 2)
}

test "enhanced_geometry_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  
  @azimuth.assert_eq(30, triangle_area)
  
  // 计算梯形面积（（上底+下底）×高÷2）
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 4
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  
  @azimuth.assert_eq(32, trapezoid_area)
}

test "enhanced_boundary_value_analysis" {
  // 测试边界值分析
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "enhanced_statistical_calculations" {
  // 测试统计计算
  // 计算一组数据的总和、平均值、最大值、最小值
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -average)
  let diff2 = @azimuth.add(data2, -average)
  let diff3 = @azimuth.add(data3, -average)
  let diff4 = @azimuth.add(data4, -average)
  let diff5 = @azimuth.add(data5, -average)
  
  let variance = (@azimuth.multiply(diff1, diff1) + @azimuth.multiply(diff2, diff2) + 
                  @azimuth.multiply(diff3, diff3) + @azimuth.multiply(diff4, diff4) + 
                  @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(200, variance)
}

test "enhanced_complex_expressions" {
  // 测试复杂表达式计算
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product_ef = @azimuth.multiply(e, f)
  
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  // (10 + 5) * (20 - 8) + 3 * 7 = 15 * 12 + 21 = 180 + 21 = 201
  @azimuth.assert_eq(201, result)
  
  // 计算：a² + b² + 2*a*b (完全平方公式)
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let two_ab = @azimuth.multiply(2, @azimuth.multiply(a, b))
  
  let perfect_square = @azimuth.add(@azimuth.add(a_squared, b_squared), two_ab)
  
  // 10² + 5² + 2*10*5 = 100 + 25 + 100 = 225
  @azimuth.assert_eq(225, perfect_square)
}