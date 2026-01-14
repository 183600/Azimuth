// Azimuth 项目额外的 MoonBit 测试用例
// 覆盖各种数学运算、字符串处理和边界条件

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第n项（简化版）
  // F(0) = 0, F(1) = 1, F(2) = 1, F(3) = 2, F(4) = 3, F(5) = 5, F(6) = 8, F(7) = 13
  
  // 计算F(7) = F(6) + F(5) = 8 + 5 = 13
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  
  @azimuth.assert_eq(13, f7)
}

test "prime_number_validation" {
  // 验证质数（通过检查是否能被小于它的数整除）
  let num = 17
  let is_prime = true
  
  // 检查是否能被2到16之间的数整除
  // 这里我们简化检查，只检查几个关键数
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_5 = (num % 5 == 0)
  
  // 17是质数，不能被2、3、5整除
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_5)
  
  // 测试非质数
  let composite_num = 15
  let composite_divisible_by_3 = (composite_num % 3 == 0)
  let composite_divisible_by_5 = (composite_num % 5 == 0)
  
  @azimuth.assert_true(composite_divisible_by_3)
  @azimuth.assert_true(composite_divisible_by_5)
}

test "greatest_common_divisor" {
  // 计算最大公约数（简化版欧几里得算法）
  let a = 48
  let b = 18
  
  // 48 % 18 = 12
  // 18 % 12 = 6
  // 12 % 6 = 0
  // 所以GCD是6
  
  let remainder1 = a % b  // 12
  let remainder2 = b % remainder1  // 6
  let remainder3 = remainder1 % remainder2  // 0
  
  @azimuth.assert_eq(6, remainder2)
  
  // 验证6确实是48和18的公约数
  let a_divisible = (a % 6 == 0)
  let b_divisible = (b % 6 == 0)
  
  @azimuth.assert_true(a_divisible)
  @azimuth.assert_true(b_divisible)
}

test "binary_number_simulation" {
  // 模拟二进制运算
  // 将二进制1010转换为十进制：1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 0
  
  // 计算2的幂
  let two_power_3 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)  // 8
  let two_power_2 = @azimuth.multiply(2, 2)  // 4
  let two_power_1 = 2  // 2
  let two_power_0 = 1  // 1
  
  // 计算十进制值
  let decimal_value = @azimuth.add(@azimuth.add(
    @azimuth.multiply(bit3, two_power_3), 
    @azimuth.multiply(bit2, two_power_2)
  ), @azimuth.add(
    @azimuth.multiply(bit1, two_power_1), 
    @azimuth.multiply(bit0, two_power_0)
  ))
  
  @azimuth.assert_eq(10, decimal_value)
}

test "string_length_analysis" {
  // 字符串长度分析（通过字符串拼接模拟）
  let base = "Hello"
  let target = @azimuth.greet(base)
  
  // "Hello, World!" 的长度应该是13个字符
  // 我们通过验证字符串内容来间接验证长度
  @azimuth.assert_eq_string("Hello, Hello!", target)
  
  // 测试不同长度的字符串
  let empty = ""
  let single = "A"
  let medium = "MoonBit"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium))
}

test "quadratic_equation_solver" {
  // 解二次方程 ax² + bx + c = 0（简化版，只计算判别式）
  // 方程：x² - 5x + 6 = 0
  // 判别式：b² - 4ac = 25 - 24 = 1 > 0，有两个实数解
  
  let a = 1
  let b = -5
  let c = 6
  
  // 计算判别式：b² - 4ac
  let b_squared = @azimuth.multiply(b, b)  // 25
  let four_ac = @azimuth.multiply(@azimuth.multiply(4, a), c)  // 24
  let discriminant = @azimuth.add(b_squared, -four_ac)  // 1
  
  @azimuth.assert_eq(1, discriminant)
  
  // 测试无实数解的情况：x² + x + 1 = 0
  // 判别式：1 - 4 = -3 < 0
  let a2 = 1
  let b2 = 1
  let c2 = 1
  
  let b2_squared = @azimuth.multiply(b2, b2)  // 1
  let four_a2_c2 = @azimuth.multiply(@azimuth.multiply(4, a2), c2)  // 4
  let discriminant2 = @azimuth.add(b2_squared, -four_a2_c2)  // -3
  
  @azimuth.assert_true(discriminant2 < 0)
}

test "array_sum_and_average" {
  // 数组求和与平均值计算
  // 模拟数组：[5, 10, 15, 20, 25]
  
  let element1 = 5
  let element2 = 10
  let element3 = 15
  let element4 = 20
  let element5 = 25
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(element1, element2), element3), element4), element5)
  @azimuth.assert_eq(75, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(15, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(element1, -average)  // -10
  let diff2 = @azimuth.add(element2, -average)  // -5
  let diff3 = @azimuth.add(element3, -average)  // 0
  let diff4 = @azimuth.add(element4, -average)  // 5
  let diff5 = @azimuth.add(element5, -average)  // 10
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)  // 100
  let squared_diff2 = @azimuth.multiply(diff2, diff2)  // 25
  let squared_diff3 = @azimuth.multiply(diff3, diff3)  // 0
  let squared_diff4 = @azimuth.multiply(diff4, diff4)  // 25
  let squared_diff5 = @azimuth.multiply(diff5, diff5)  // 100
  
  let sum_squared_diffs = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5)
  let variance = sum_squared_diffs / count
  
  @azimuth.assert_eq(50, variance)
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算
  let distance = 300  // 公里
  let speed = 60      // 公里/小时
  
  // 计算时间：时间 = 距离 / 速度
  let time = distance / speed
  @azimuth.assert_eq(5, time)  // 5小时
  
  // 计算平均速度（多段路程）
  let distance1 = 120
  let time1 = 2
  let distance2 = 180
  let time2 = 3
  
  let total_distance = @azimuth.add(distance1, distance2)  // 300
  let total_time = @azimuth.add(time1, time2)  // 5
  let average_speed = total_distance / total_time
  
  @azimuth.assert_eq(60, average_speed)
}

test "discount_price_calculation" {
  // 折扣价格计算
  let original_price = 1000
  let discount_percentage = 20  // 20%折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(200, discount_amount)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(800, discounted_price)
  
  // 计算税费（10%）
  let tax_percentage = 10
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  @azimuth.assert_eq(80, tax_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(880, final_price)
}

test "leap_year_validation" {
  // 闰年判断
  // 闰年规则：能被4整除但不能被100整除，或者能被400整除
  
  let year2024 = 2024
  let year1900 = 1900
  let year2000 = 2000
  let year2023 = 2023
  
  // 2024年是闰年（能被4整除，不能被100整除）
  let divisible_by_4_2024 = (year2024 % 4 == 0)
  let divisible_by_100_2024 = (year2024 % 100 == 0)
  let is_leap_2024 = divisible_by_4_2024 && !divisible_by_100_2024
  
  @azimuth.assert_true(is_leap_2024)
  
  // 1900年不是闰年（能被100整除但不能被400整除）
  let divisible_by_100_1900 = (year1900 % 100 == 0)
  let divisible_by_400_1900 = (year1900 % 400 == 0)
  let is_leap_1900 = divisible_by_100_1900 && divisible_by_400_1900
  
  @azimuth.assert_false(is_leap_1900)
  
  // 2000年是闰年（能被400整除）
  let divisible_by_400_2000 = (year2000 % 400 == 0)
  let is_leap_2000 = divisible_by_400_2000
  
  @azimuth.assert_true(is_leap_2000)
  
  // 2023年不是闰年（不能被4整除）
  let divisible_by_4_2023 = (year2023 % 4 == 0)
  
  @azimuth.assert_false(divisible_by_4_2023)
}