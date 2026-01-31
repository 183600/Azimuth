// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "string_reverse_simulation" {
  // 模拟字符串反转功能
  let input = "hello"
  let char1 = "h"
  let char2 = "e"
  let char3 = "l"
  let char4 = "l"
  let char5 = "o"
  
  // 模拟反转后的字符串
  let reversed = char5 + char4 + char3 + char2 + char1
  let expected = "olleh"
  
  // 使用 greet 函数验证字符串处理
  let greeting = @azimuth.greet(reversed)
  @azimuth.assert_eq_string("Hello, olleh!", greeting)
}

test "array_sum_simulation" {
  // 模拟数组求和功能
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值（向上取整）
  let average = @azimuth.divide_with_ceil(sum, 5)
  @azimuth.assert_eq(30, average)
}

test "factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n1 = 1
  let n2 = 2
  let n3 = 3
  let n4 = 4
  let n5 = 5
  
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(n1, n2), n3), n4), n5)
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算 4! = 4 * 3 * 2 * 1 = 24
  let factorial_4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(n1, n2), n3), n4)
  @azimuth.assert_eq(24, factorial_4)
}

test "geometric_calculations" {
  // 几何计算：矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
  
  // 计算对角线长度的平方（勾股定理）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(100, diagonal_squared)
}

test "temperature_conversion" {
  // 温度转换：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  
  // 使用整数运算模拟温度转换
  let step1 = @azimuth.multiply(celsius, 9)
  let step2 = @azimuth.divide_with_ceil(step1, 5)
  let fahrenheit = @azimuth.add(step2, 32)
  
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(freezing_celsius, 9), 5), 32)
  @azimuth.assert_eq(32, freezing_fahrenheit)
}

test "compound_interest_calculation" {
  // 复利计算：A = P * (1 + r)^n
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年：1000 * 1.05 = 1050（使用向上取整）
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  
  // 第二年：1050 * 1.05 = 1103（使用向上取整）
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  
  // 第三年：1103 * 1.05 = 1159（使用向上取整）
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1103, year2)
  @azimuth.assert_eq(1159, year3)
}

test "prime_number_check" {
  // 质数检查：检查 17 是否为质数
  let number = 17
  let divisor1 = 2
  let divisor2 = 3
  let divisor3 = 4
  let divisor4 = 5
  
  // 检查是否能被这些数整除（通过检查余数）
  let remainder1 = number % divisor1
  let remainder2 = number % divisor2
  let remainder3 = number % divisor3
  let remainder4 = number % divisor4
  
  // 17 是质数，所以余数都不为0
  let is_prime = @azimuth.add(remainder1, @azimuth.add(remainder2, @azimuth.add(remainder3, remainder4)))
  @azimuth.assert_eq(5, is_prime)  // 1 + 2 + 1 + 1 = 5（都不为0）
}

test "binary_conversion_simulation" {
  // 模拟二进制转换：将二进制 1011 转换为十进制
  let bit3 = 1  // 2^3 = 8
  let bit2 = 0  // 2^2 = 4
  let bit1 = 1  // 2^1 = 2
  let bit0 = 1  // 2^0 = 1
  
  // 计算十进制值
  let decimal_value = @azimuth.add(@azimuth.multiply(bit3, 8), @azimuth.add(@azimuth.multiply(bit2, 4), @azimuth.add(@azimuth.multiply(bit1, 2), @azimuth.multiply(bit0, 1))))
  @azimuth.assert_eq(11, decimal_value)  // 8 + 0 + 2 + 1 = 11
  
  // 测试另一个二进制数 1100 = 12
  let binary_1100 = @azimuth.add(@azimuth.multiply(1, 8), @azimuth.add(@azimuth.multiply(1, 4), @azimuth.add(@azimuth.multiply(0, 2), @azimuth.multiply(0, 1))))
  @azimuth.assert_eq(12, binary_1100)
}

test "greatest_common_divisor" {
  // 最大公约数算法（欧几里得算法的简化版本）
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法的步骤
  let step1 = a % b  // 48 % 18 = 12
  let step2 = b % step1  // 18 % 12 = 6
  let step3 = step1 % step2  // 12 % 6 = 0
  
  // 当余数为0时，除数就是最大公约数
  let gcd = step2
  @azimuth.assert_eq(6, gcd)
  
  // 测试另一对数字：56 和 15
  let a2 = 56
  let b2 = 15
  let step1_2 = a2 % b2  // 56 % 15 = 11
  let step2_2 = b2 % step1_2  // 15 % 11 = 4
  let step3_2 = step1_2 % step2_2  // 11 % 4 = 3
  let step4_2 = step2_2 % step3_2  // 4 % 3 = 1
  let step5_2 = step3_2 % step4_2  // 3 % 1 = 0
  
  let gcd2 = step4_2
  @azimuth.assert_eq(1, gcd2)
}

test "loan_amortization" {
  // 贷款分期计算
  let loan_amount = 10000
  let annual_rate = 6  // 6%
  let loan_term_years = 3
  let monthly_payment = 300
  
  // 计算总还款额
  let total_payment = @azimuth.multiply(monthly_payment, @azimuth.multiply(loan_term_years, 12))
  
  // 计算总利息
  let total_interest = @azimuth.add(total_payment, -loan_amount)
  
  // 计算月利率（简化计算）
  let monthly_rate = @azimuth.divide_with_ceil(annual_rate, 12)
  
  @azimuth.assert_eq(10800, total_payment)  // 300 * 36 = 10800
  @azimuth.assert_eq(800, total_interest)   // 10800 - 10000 = 800
  @azimuth.assert_eq(1, monthly_rate)       // ceil(6/12) = 1
  
  // 验证贷款信息的字符串处理
  let loan_info = @azimuth.greet("Loan Amount: " + loan_amount.to_string())
  @azimuth.assert_eq_string("Hello, Loan Amount: 10000!", loan_info)
}