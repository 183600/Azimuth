// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了更多的边界情况和实际应用场景

test "add_large_positive_numbers" {
  // 测试大正数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(1500000000, @azimuth.add(750000000, 750000000))
  @azimuth.assert_eq(2147483647, @azimuth.add(1073741823, 1073741824))
}

test "add_large_negative_numbers" {
  // 测试大负数相加
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(-1500000000, @azimuth.add(-750000000, -750000000))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
}

test "multiply_fractional_approximation" {
  // 测试分数近似值的乘法
  // 1/3 ≈ 0.333..., 0.333 * 3 ≈ 1 (整数近似)
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  
  // 使用整数模拟分数运算
  // 1/2 * 4 = 2 (使用 1 * 4 / 2 = 2)
  let half_times_four = @azimuth.multiply(1, 4) / 2
  @azimuth.assert_eq(2, half_times_four)
  
  // 2/3 * 6 = 4 (使用 2 * 6 / 3 = 4)
  let two_thirds_times_six = @azimuth.multiply(2, 6) / 3
  @azimuth.assert_eq(4, two_thirds_times_six)
}

test "calculate_rectangle_properties" {
  // 计算矩形的属性
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(46, perimeter)
  
  // 计算对角线长度的平方 (a² + b²)
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(289, diagonal_squared)  // 15² + 8² = 225 + 64 = 289
}

test "compound_interest_calculation" {
  // 复利计算（简化版，整数运算）
  let principal = 1000  // 本金
  let rate = 5          // 利率（百分比）
  let years = 3         // 年数
  
  // 计算每年的利息并累加
  let year1_interest = @azimuth.multiply(principal, rate) / 100
  let year1_total = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.multiply(year1_total, rate) / 100
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  let year3_interest = @azimuth.multiply(year2_total, rate) / 100
  let final_total = @azimuth.add(year2_total, year3_interest)
  
  // 验证最终金额约等于 1157 (1000 * 1.05³)
  @azimuth.assert_eq(1157, final_total)
}

test "string_validation_and_processing" {
  // 测试字符串验证和处理
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Smith!", @azimuth.greet("John Smith"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌟MoonBit🌟!", @azimuth.greet("🌟MoonBit🌟"))
}

test "statistical_calculations" {
  // 统计计算
  let numbers = [12, 15, 18, 21, 24]  // 模拟数组
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  @azimuth.assert_eq(90, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(18, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.multiply(numbers[0] - average, numbers[0] - average)
  let diff2 = @azimuth.multiply(numbers[1] - average, numbers[1] - average)
  let diff3 = @azimuth.multiply(numbers[2] - average, numbers[2] - average)
  let diff4 = @azimuth.multiply(numbers[3] - average, numbers[3] - average)
  let diff5 = @azimuth.multiply(numbers[4] - average, numbers[4] - average)
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(diff1, diff2), diff3), diff4), diff5)
  let variance = variance_sum / 5
  @azimuth.assert_eq(18, variance)  // 简化的方差计算
}

test "time_calculations" {
  // 时间计算（秒转换为时分秒）
  let total_seconds = 3665  // 1小时1分5秒
  
  // 计算小时
  let hours = total_seconds / 3600
  @azimuth.assert_eq(1, hours)
  
  // 计算剩余秒数
  let remaining_seconds = total_seconds - @azimuth.multiply(hours, 3600)
  @azimuth.assert_eq(65, remaining_seconds)
  
  // 计算分钟
  let minutes = remaining_seconds / 60
  @azimuth.assert_eq(1, minutes)
  
  // 计算秒数
  let seconds = remaining_seconds - @azimuth.multiply(minutes, 60)
  @azimuth.assert_eq(5, seconds)
}

test "number_system_conversions" {
  // 数字系统转换（简化版）
  // 十进制转二进制（模拟）
  let decimal_num = 13
  
  // 使用除2取余法模拟二进制转换
  let rem1 = decimal_num % 2
  let div1 = decimal_num / 2
  let rem2 = div1 % 2
  let div2 = div1 / 2
  let rem3 = div2 % 2
  let div3 = div2 / 2
  let rem4 = div3 % 2
  
  // 将余数组合成二进制数（1101）
  let binary_num = @azimuth.add(@azimuth.add(@azimuth.multiply(rem4, 1000), @azimuth.multiply(rem3, 100)), @azimuth.add(@azimuth.multiply(rem2, 10), rem1))
  @azimuth.assert_eq(1101, binary_num)
  
  // 二进制转十进制验证
  let decimal_back = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(0, 2), @azimuth.multiply(1, 1)))
  @azimuth.assert_eq(13, decimal_back)
}

test "geometric_progression" {
  // 等比数列计算
  let first_term = 2
  let ratio = 3
  let n = 5  // 项数
  
  // 计算前5项：2, 6, 18, 54, 162
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  let term5 = @azimuth.multiply(term4, ratio)
  
  // 验证各项
  @azimuth.assert_eq(2, term1)
  @azimuth.assert_eq(6, term2)
  @azimuth.assert_eq(18, term3)
  @azimuth.assert_eq(54, term4)
  @azimuth.assert_eq(162, term5)
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(242, sum)
}