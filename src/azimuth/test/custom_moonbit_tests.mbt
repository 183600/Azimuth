// 自定义 MoonBit 测试用例
// 包含各种实用的测试场景

test "simple_addition_test" {
  // 简单的加法测试
  let result = azimuth::add(2, 3)
  azimuth::assert_eq(5, result)
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  let result = azimuth::multiply(4, 5)
  azimuth::assert_eq(20, result)
}

test "simple_greet_test" {
  // 简单的问候测试
  let result = azimuth::greet("World")
  azimuth::assert_eq_string("Hello, World!", result)
}

test "number_system_conversion" {
  // 数字系统转换测试：十进制转二进制（简化版）
  let decimal = 13
  
  // 模拟二进制转换：13 = 1101₂ = 1×2³ + 1×2² + 0×2¹ + 1×2⁰
  let bit3 = azimuth::multiply(1, azimuth::multiply(azimuth::multiply(2, 2), 2))  // 1×2³ = 8
  let bit2 = azimuth::multiply(1, azimuth::multiply(2, 2))                    // 1×2² = 4
  let bit1 = azimuth::multiply(0, 2)                                          // 0×2¹ = 0
  let bit0 = azimuth::multiply(1, 1)                                          // 1×2⁰ = 1
  
  let reconstructed = azimuth::add(azimuth::add(azimuth::add(bit3, bit2), bit1), bit0)
  azimuth::assert_eq(decimal, reconstructed)
}

test "greatest_common_divisor" {
  // 最大公约数算法测试（欧几里得算法的简化版）
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法：gcd(48, 18) = gcd(18, 48%18) = gcd(18, 12) = gcd(12, 6) = 6
  let remainder1 = 48 % 18  // 12
  let remainder2 = 18 % remainder1  // 6
  let remainder3 = remainder1 % remainder2  // 0
  
  // 当余数为0时，当前除数就是GCD
  let gcd = remainder2
  azimuth::assert_eq(6, gcd)
  
  // 验证：两个数都能被GCD整除
  azimuth::assert_eq(0, a % gcd)
  azimuth::assert_eq(0, b % gcd)
}

test "fibonacci_sequence" {
  // 斐波那契数列测试
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = azimuth::add(f1, f0)  // 1
  let f3 = azimuth::add(f2, f1)  // 2
  let f4 = azimuth::add(f3, f2)  // 3
  let f5 = azimuth::add(f4, f3)  // 5
  let f6 = azimuth::add(f5, f4)  // 8
  let f7 = azimuth::add(f6, f5)  // 13
  let f8 = azimuth::add(f7, f6)  // 21
  let f9 = azimuth::add(f8, f7)  // 34
  let f10 = azimuth::add(f9, f8) // 55
  
  azimuth::assert_eq(0, f0)
  azimuth::assert_eq(1, f1)
  azimuth::assert_eq(1, f2)
  azimuth::assert_eq(2, f3)
  azimuth::assert_eq(3, f4)
  azimuth::assert_eq(5, f5)
  azimuth::assert_eq(8, f6)
  azimuth::assert_eq(13, f7)
  azimuth::assert_eq(21, f8)
  azimuth::assert_eq(34, f9)
  azimuth::assert_eq(55, f10)
  
  // 验证黄金比例近似：F(n+1)/F(n) ≈ 1.618
  // 使用整数验证：F(n+1) ≈ F(n) + F(n)/2
  let _approximation = azimuth::add(f10, f10 / 2)
  azimuth::assert_true(azimuth::multiply(f9, 2) > f10)  // 34*2 > 55
  azimuth::assert_true(azimuth::multiply(f9, 2) < azimuth::add(f10, f10))  // 34*2 < 110
}

test "prime_number_check" {
  // 质数检查测试
  // 使用试除法检查一个数是否为质数
  
  // 检查17是否为质数
  let _num = 17
  let divisor2 = 17 % 2  // 1
  let divisor3 = 17 % 3  // 2
  let divisor4 = 17 % 4  // 1
  
  // 如果所有余数都不为0，则可能是质数
  let check1 = divisor2 != 0
  let check2 = divisor3 != 0
  let check3 = divisor4 != 0
  let final_check = check1 && check2 && check3
  
  azimuth::assert_true(final_check)
  
  // 检查15是否为质数（应该不是）
  let _num2 = 15
  let divisor2_2 = 15 % 2  // 1
  let divisor3_2 = 15 % 3  // 0
  let _divisor4_2 = 15 % 4  // 3
  
  // 如果有余数为0，则不是质数
  let not_prime = divisor3_2 == 0
  azimuth::assert_true(not_prime)
}

test "calendar_calculation" {
  // 日历计算测试：计算某年某月的天数
  // 使用简化规则：不考虑闰年，假设2月总是28天
  
  let _year = 2023
  let month = 2  // 二月
  
  // 模拟月份天数计算
  let days_in_month = 
    if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) {
      31
    } else if (month == 4 || month == 6 || month == 9 || month == 11) {
      30
    } else {
      28  // 二月（不考虑闰年）
    }
  
  azimuth::assert_eq(28, days_in_month)
  
  // 计算某年中的第几天（2023年3月15日）
  let day = 15
  let days_before_march = azimuth::add(31, 28)  // 一月和二月
  let day_of_year = azimuth::add(days_before_march, day)  // 31 + 28 + 15 = 74
  azimuth::assert_eq(74, day_of_year)
}

test "bank_interest_calculation" {
  // 银行利息计算测试
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率百分比
  let years = 3          // 年数
  
  // 简单利息计算
  let simple_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years)) / 100
  let simple_total = azimuth::add(principal, simple_interest)
  azimuth::assert_eq(16000, simple_total)  // 10000 + 10000*5*3/100 = 16000
  
  // 复利计算（简化版）
  // 第一年：10000 * (1 + 5%) = 10500
  let year1_end = azimuth::multiply(principal, azimuth::add(100, annual_rate)) / 100
  
  // 第二年：10500 * (1 + 5%) = 11025
  let year2_end = azimuth::multiply(year1_end, azimuth::add(100, annual_rate)) / 100
  
  // 第三年：11025 * (1 + 5%) = 11576.25 ≈ 11576（整数）
  let year3_end = azimuth::multiply(year2_end, azimuth::add(100, annual_rate)) / 100
  
  azimuth::assert_eq(11576, year3_end)
  
  // 比较简单利息和复利的差异
  let difference = year3_end - simple_total
  azimuth::assert_true(difference > 0)  // 复利应该比简单利息多
}