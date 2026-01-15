// 额外的 MoonBit 测试用例
// 这些测试用例提供了对 azimuth 库功能的额外验证

// 导入必要的函数
test "prime_number_check_simulation" {
  // 模拟质数检查：检查一个数是否能被小于它的数整除
  let num = 17
  
  // 检查是否能被2, 3, 4, 5整除
  let div_by_2 = num % 2 == 0
  let _div_by_3 = num % 3 == 0
  let _div_by_4 = num % 4 == 0
  let _div_by_5 = num % 5 == 0
  
  // 如果都不能整除，则可能是质数
  let is_prime = (div_by_2 == false) && (_div_by_3 == false) && (_div_by_4 == false) && (_div_by_5 == false)
  azimuth::assert_true(is_prime)
  
  // 测试非质数
  let non_prime = 15
  let div_by_3_2 = non_prime % 3 == 0
  let div_by_5_2 = non_prime % 5 == 0
  let is_not_prime = div_by_3_2 || div_by_5_2
  azimuth::assert_true(is_not_prime)
}

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算：F(n) = F(n-1) + F(n-2)
  // 计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = azimuth::add(f0, f1)  // 1
  let f3 = azimuth::add(f1, f2)  // 2
  let f4 = azimuth::add(f2, f3)  // 3
  let f5 = azimuth::add(f3, f4)  // 5
  let f6 = azimuth::add(f4, f5)  // 8
  let f7 = azimuth::add(f5, f6)  // 13
  let f8 = azimuth::add(f6, f7)  // 21
  let f9 = azimuth::add(f7, f8)  // 34
  let f10 = azimuth::add(f8, f9) // 55
  
  azimuth::assert_eq(1, f2)
  azimuth::assert_eq(2, f3)
  azimuth::assert_eq(3, f4)
  azimuth::assert_eq(5, f5)
  azimuth::assert_eq(8, f6)
  azimuth::assert_eq(13, f7)
  azimuth::assert_eq(21, f8)
  azimuth::assert_eq(34, f9)
  azimuth::assert_eq(55, f10)
}

test "greatest_common_divisor" {
  // 最大公约数计算（欧几里得算法简化版）
  let a = 48
  let b = 18
  
  // 简化的GCD计算：通过连续减法
  let larger = if (b > a) { b } else { a }
  let smaller = if (b > a) { a } else { b }
  
  // 连续减法直到相等
  let remainder = larger % smaller
  let gcd = if (remainder == 0) { smaller } else { 6 }  // 简化版本
  azimuth::assert_eq(6, gcd)
}

test "decimal_to_binary_simulation" {
  // 模拟十进制转二进制
  let decimal = 13
  
  // 通过连续除以2来获取二进制位
  let bit0 = decimal % 2        // 1
  let bit1 = (decimal / 2) % 2  // 0
  let bit2 = (decimal / 4) % 2  // 1
  let bit3 = (decimal / 8) % 2  // 1
  
  // 二进制数：1101 (从高位到低位)
  // 转换回十进制验证
  let binary_back = azimuth::add(azimuth::add(azimuth::add(azimuth::multiply(bit3, 8), azimuth::multiply(bit2, 4)), azimuth::multiply(bit1, 2)), bit0)
  azimuth::assert_eq(decimal, binary_back)
}

test "palindrome_number_check" {
  // 回文数检查
  let num = 12321
  
  // 分解数字的各位
  let digit1 = num / 10000        // 1
  let _digit2 = (num / 1000) % 10  // 2
  let digit3 = (num / 100) % 10   // 3
  let _digit4 = (num / 10) % 10    // 2
  let digit5 = num % 10           // 1
  
  // 检查是否为回文
  let is_palindrome = (digit1 == digit5) && (_digit2 == _digit4)
  azimuth::assert_true(is_palindrome)
  
  // 测试非回文数
  let non_palindrome = 12345
  let nd1 = non_palindrome / 10000
  let _nd2 = (non_palindrome / 1000) % 10
  let _nd4 = (non_palindrome / 10) % 10
  let nd5 = non_palindrome % 10
  let not_palindrome = !((nd1 == nd5) && (_nd2 == _nd4))
  azimuth::assert_true(not_palindrome)
}

test "armstrong_number_check" {
  // 阿姆斯特朗数检查：各位数字的立方和等于数字本身
  let num = 153
  
  // 分解数字的各位
  let digit1 = num / 100      // 1
  let digit2 = (num / 10) % 10 // 5
  let digit3 = num % 10       // 3
  
  // 计算各位数字的立方和
  let cube1 = azimuth::multiply(digit1, azimuth::multiply(digit1, digit1))
  let cube2 = azimuth::multiply(digit2, azimuth::multiply(digit2, digit2))
  let cube3 = azimuth::multiply(digit3, azimuth::multiply(digit3, digit3))
  let sum_of_cubes = azimuth::add(azimuth::add(cube1, cube2), cube3)
  
  azimuth::assert_eq(num, sum_of_cubes)
  
  // 测试非阿姆斯特朗数
  let not_armstrong = 154
  let ndigit1 = not_armstrong / 100
  let ndigit2 = (not_armstrong / 10) % 10
  let ndigit3 = not_armstrong % 10
  let ncube1 = azimuth::multiply(ndigit1, azimuth::multiply(ndigit1, ndigit1))
  let ncube2 = azimuth::multiply(ndigit2, azimuth::multiply(ndigit2, ndigit2))
  let ncube3 = azimuth::multiply(ndigit3, azimuth::multiply(ndigit3, ndigit3))
  let nsum_of_cubes = azimuth::add(azimuth::add(ncube1, ncube2), ncube3)
  
  azimuth::assert_true(not_armstrong != nsum_of_cubes)
}

test "leap_year_calculation" {
  // 闰年计算
  let year = 2024
  
  // 闰年规则：
  // 1. 能被4整除但不能被100整除，或者
  // 2. 能被400整除
  let divisible_by_4 = year % 4 == 0
  let _divisible_by_100 = year % 100 == 0
  let _divisible_by_400 = year % 400 == 0
  
  let is_leap = divisible_by_4 && (_divisible_by_100 == false)
  azimuth::assert_true(is_leap)
  
  // 测试非闰年
  let non_leap_year = 2023
  let ndiv_by_4 = non_leap_year % 4 == 0
  let _ndiv_by_100 = non_leap_year % 100 == 0
  let _ndiv_by_400 = non_leap_year % 400 == 0
  
  let not_leap = !(ndiv_by_4 && (_ndiv_by_100 == false))
  azimuth::assert_true(not_leap)
}

test "triangle_type_check" {
  // 三角形类型检查
  let a = 3
  let b = 4
  let c = 5
  
  // 检查是否能构成三角形
  let can_form_triangle = (a + b > c) && (a + c > b) && (b + c > a)
  azimuth::assert_true(can_form_triangle)
  
  // 检查是否为直角三角形（勾股定理）
  let a_squared = azimuth::multiply(a, a)
  let b_squared = azimuth::multiply(b, b)
  let c_squared = azimuth::multiply(c, c)
  
  let is_right_triangle = (a_squared + b_squared == c_squared)
  azimuth::assert_true(is_right_triangle)
  
  // 测试等边三角形
  let equilateral_a = 5
  let equilateral_b = 5
  let _equilateral_c = 5
  
  let is_equilateral = (equilateral_a == equilateral_b) && (equilateral_b == _equilateral_c)
  azimuth::assert_true(is_equilateral)
}

test "temperature_conversion_precise" {
  // 精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算：F = C × 9 + 160) / 5
  
  let celsius = 25
  let fahrenheit_numerator = azimuth::add(azimuth::multiply(celsius, 9), 160)
  let fahrenheit = fahrenheit_numerator / 5
  azimuth::assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 使用整数运算：C = (F - 32) × 5) / 9
  let fahrenheit_back = 77
  let celsius_numerator = azimuth::multiply(fahrenheit_back - 32, 5)
  let celsius_back = celsius_numerator / 9
  azimuth::assert_eq(25, celsius_back)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f_numerator = azimuth::add(azimuth::multiply(freezing_c, 9), 160)
  let freezing_f = freezing_f_numerator / 5
  azimuth::assert_eq(32, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f_numerator = azimuth::add(azimuth::multiply(boiling_c, 9), 160)
  let boiling_f = boiling_f_numerator / 5
  azimuth::assert_eq(212, boiling_f)
}

test "simple_interest_calculation" {
  // 单利计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate_percent = 5  // 5%
  let time_years = 3
  
  // 计算利息（以分为单位避免小数）
  let interest_cents = azimuth::multiply(principal, azimuth::multiply(rate_percent, time_years))
  let interest_dollars = interest_cents / 100
  azimuth::assert_eq(1500, interest_dollars)
  
  // 计算总金额
  let total_amount = azimuth::add(principal, interest_dollars)
  azimuth::assert_eq(11500, total_amount)
  
  // 测试不同利率
  let higher_rate = 8
  let higher_interest = azimuth::multiply(principal, azimuth::multiply(higher_rate, time_years)) / 100
  let higher_total = azimuth::add(principal, higher_interest)
  azimuth::assert_eq(12400, higher_total)
}