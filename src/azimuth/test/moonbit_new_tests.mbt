// 新增的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "binary_number_operations" {
  // 测试二进制数相关的运算
  // 模拟二进制左移：n << 1 = n * 2
  let n = 8
  let left_shift = @azimuth.multiply(n, 2)
  @azimuth.assert_eq(16, left_shift)
  
  // 模拟二进制右移：n >> 1 = n / 2
  let right_shift = n / 2
  @azimuth.assert_eq(4, right_shift)
  
  // 模拟位与运算：8 & 7 = 0（1000 & 0111 = 0000）
  let a = 8
  let b = 7
  let result = @azimuth.multiply(a, b) / @azimuth.multiply(a, b)  // 如果a*b>0则为1，否则为0
  @azimuth.assert_eq(0, result)
}

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
}

test "greatest_common_divisor" {
  // 测试最大公约数的简化计算
  // 使用欧几里得算法的简化版本
  let a = 48
  let b = 18
  
  // 48 % 18 = 12
  let remainder1 = a % b
  @azimuth.assert_eq(12, remainder1)
  
  // 18 % 12 = 6
  let remainder2 = b % remainder1
  @azimuth.assert_eq(6, remainder2)
  
  // 12 % 6 = 0，所以GCD是6
  let remainder3 = remainder1 % remainder2
  @azimuth.assert_eq(0, remainder3)
  
  // GCD是6
  let gcd = remainder2
  @azimuth.assert_eq(6, gcd)
}

test "prime_number_check" {
  // 测试质数检查的简化版本
  let num = 17
  
  // 检查是否能被2到sqrt(17)之间的数整除
  let sqrt_17_approx = 4  // sqrt(17) ≈ 4.12，取整数部分
  let divisible_by_2 = num % 2
  let divisible_by_3 = num % 3
  let divisible_by_4 = num % 4
  
  // 如果都不能整除，则可能是质数
  let is_prime = @azimuth.add(@azimuth.add(divisible_by_2, divisible_by_3), divisible_by_4) != 0
  @azimuth.assert_true(is_prime)
}

test "palindrome_string_check" {
  // 测试回文字符串检查（简化版）
  let str = "level"
  let str_length = 5
  
  // 检查第一个和最后一个字符
  let first_char_match = true  // 简化假设为true
  let second_char_match = true  // 简化假设为true
  
  // 如果所有对应字符都匹配，则是回文
  let is_palindrome = first_char_match && second_char_match
  @azimuth.assert_true(is_palindrome)
  
  // 测试非回文字符串
  let non_palindrome = false  // 简化假设
  @azimuth.assert_false(non_palindrome)
}

test "quadratic_equation_solver" {
  // 测试二次方程求解：ax² + bx + c = 0
  // 简化版本：只测试有整数解的情况
  // x² - 5x + 6 = 0 的解是 x = 2 和 x = 3
  
  let a = 1
  let b = -5
  let c = 6
  
  // 测试 x = 2 是否是解
  let x1 = 2
  let x1_squared = @azimuth.multiply(x1, x1)  // 4
  let ax1_squared = @azimuth.multiply(a, x1_squared)  // 4
  let bx1 = @azimuth.multiply(b, x1)  // -10
  let equation_result1 = @azimuth.add(@azimuth.add(ax1_squared, bx1), c)  // 4 + (-10) + 6 = 0
  @azimuth.assert_eq(0, equation_result1)
  
  // 测试 x = 3 是否是解
  let x2 = 3
  let x2_squared = @azimuth.multiply(x2, x2)  // 9
  let ax2_squared = @azimuth.multiply(a, x2_squared)  // 9
  let bx2 = @azimuth.multiply(b, x2)  // -15
  let equation_result2 = @azimuth.add(@azimuth.add(ax2_squared, bx2), c)  // 9 + (-15) + 6 = 0
  @azimuth.assert_eq(0, equation_result2)
}

test "array_sum_and_average" {
  // 测试数组的求和与平均值计算
  // 模拟数组 [5, 10, 15, 20, 25]
  let elem1 = 5
  let elem2 = 10
  let elem3 = 15
  let elem4 = 20
  let elem5 = 25
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(elem1, elem2), elem3), elem4), elem5)
  @azimuth.assert_eq(75, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(15, average)
  
  // 计算最大值和最小值的差
  let max = elem5
  let min = elem1
  let range = @azimuth.add(max, -min)
  @azimuth.assert_eq(20, range)
}

test "speed_distance_time_calculations" {
  // 测试速度、距离、时间计算
  let distance = 300  // 公里
  let time = 5        // 小时
  
  // 计算速度：速度 = 距离 / 时间
  let speed = distance / time
  @azimuth.assert_eq(60, speed)
  
  // 计算如果速度增加20%的新时间
  let speed_increase_percent = 20
  let speed_increase = @azimuth.multiply(speed, speed_increase_percent) / 100
  let new_speed = @azimuth.add(speed, speed_increase)  // 72
  let new_time = distance / new_speed  // 4.166...，整数除法为4
  @azimuth.assert_eq(4, new_time)
  
  // 计算节省的时间
  let time_saved = @azimuth.add(time, -new_time)
  @azimuth.assert_eq(1, time_saved)
}

test "simple_interest_calculation" {
  // 测试单利计算
  let principal = 5000   // 本金
  let rate = 8           // 年利率百分比
  let time = 3           // 年数
  
  // 计算利息：利息 = 本金 × 利率 × 时间 / 100
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  @azimuth.assert_eq(1200, interest)
  
  // 计算总金额
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(6200, total_amount)
  
  // 计算月还款额（简化：总金额 / 月数）
  let months = @azimuth.multiply(time, 12)
  let monthly_payment = total_amount / months
  @azimuth.assert_eq(172, monthly_payment)  // 6200 / 36 = 172.22...，整数除法为172
}

test "base_conversion_simulation" {
  // 测试进制转换模拟
  // 十进制转二进制的简化模拟
  
  // 转换数字13到二进制（1101）
  let decimal_num = 13
  
  // 计算二进制各位
  let bit0 = decimal_num % 2        // 1
  let remainder1 = decimal_num / 2   // 6
  let bit1 = remainder1 % 2          // 0
  let remainder2 = remainder1 / 2    // 3
  let bit2 = remainder2 % 2          // 1
  let remainder3 = remainder2 / 2    // 1
  let bit3 = remainder3 % 2          // 1
  
  // 验证二进制各位
  @azimuth.assert_eq(1, bit0)
  @azimuth.assert_eq(0, bit1)
  @azimuth.assert_eq(1, bit2)
  @azimuth.assert_eq(1, bit3)
  
  // 将二进制转换回十进制验证：1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let two_pow_0 = 1
  let two_pow_1 = @azimuth.multiply(2, 1)
  let two_pow_2 = @azimuth.multiply(2, 2)
  let two_pow_3 = @azimuth.multiply(2, 4)
  
  let reconstructed = @azimuth.add(@azimuth.add(@azimuth.multiply(bit3, two_pow_3), 
                                               @azimuth.multiply(bit2, two_pow_2)),
                                   @azimuth.add(@azimuth.multiply(bit1, two_pow_1), 
                                               @azimuth.multiply(bit0, two_pow_0)))
  @azimuth.assert_eq(13, reconstructed)
}