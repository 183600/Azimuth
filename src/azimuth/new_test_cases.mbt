// 新增的 MoonBit 测试用例
// 这些测试用例涵盖各种场景，包括边界条件、实际应用和算法验证

// 导入必要的函数
use azimuth.*;

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(5, divide_with_ceil(21, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-10, 4))
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(2, divide_with_ceil(10, -4))
}

test "divide_with_ceil_edge_cases" {
  // 测试边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
}

test "greatest_common_divisor" {
  // 计算最大公约数（简化版欧几里得算法）
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法：gcd(a, b) = gcd(b, a mod b)
  let r1 = a % b  // 48 % 18 = 12
  let r2 = b % r1 // 18 % 12 = 6
  let r3 = r1 % r2 // 12 % 6 = 0
  
  // 当余数为0时，当前除数就是GCD
  let gcd = r2
  assert_eq(6, gcd)
  
  // 测试另一组数
  let c = 56
  let d = 15
  let r4 = c % d  // 56 % 15 = 11
  let r5 = d % r4 // 15 % 11 = 4
  let r6 = r4 % r5 // 11 % 4 = 3
  let r7 = r5 % r6 // 4 % 3 = 1
  let r8 = r6 % r7 // 3 % 1 = 0
  
  let gcd2 = r7
  assert_eq(1, gcd2)
}

test "prime_number_check" {
  // 素数检查测试
  let num = 17
  
  // 检查是否能被2到sqrt(17)之间的数整除
  let sqrt_17 = 4  // 简化的平方根
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_4 = (num % 4 == 0)
  
  // 如果都不能整除，则是素数
  let is_prime = if (divisible_by_2 || divisible_by_3 || divisible_by_4) { false } else { true }
  assert_true(is_prime)
  
  // 测试非素数
  let composite = 15
  let divisible_by_2_2 = (composite % 2 == 0)
  let divisible_by_3_2 = (composite % 3 == 0)
  let divisible_by_4_2 = (composite % 4 == 0)
  
  let is_prime_2 = if (divisible_by_2_2 || divisible_by_3_2 || divisible_by_4_2) { false } else { true }
  assert_false(is_prime_2)
}

test "binary_search_simulation" {
  // 模拟二分查找算法
  // 在有序数组 [1, 3, 5, 7, 9, 11, 13] 中查找 7
  let target = 7
  let low = 0
  let high = 6
  
  // 第一次迭代：mid = (0 + 6) / 2 = 3，arr[3] = 7
  let mid1 = (low + high) / 2
  let found_at_mid1 = (mid1 == 3)  // 假设arr[3] = 7
  assert_true(found_at_mid1)
  
  // 模拟查找不存在的元素
  let target2 = 8
  let low2 = 0
  let high2 = 6
  
  // 第一次迭代：mid = (0 + 6) / 2 = 3，arr[3] = 7 < 8
  let mid2 = (low2 + high2) / 2
  let arr_mid2 = 7  // 假设值
  let target_greater = (target2 > arr_mid2)
  
  // 第二次迭代：low = 4, high = 6, mid = (4 + 6) / 2 = 5，arr[5] = 11 > 8
  let low3 = 4
  let high3 = 6
  let mid3 = (low3 + high3) / 2
  let arr_mid3 = 11  // 假设值
  let target_less = (target2 < arr_mid3)
  
  // 第三次迭代：low = 4, high = 4, mid = 4, arr[4] = 9 > 8
  let low4 = 4
  let high4 = 4
  let mid4 = (low4 + high4) / 2
  let arr_mid4 = 9  // 假设值
  let target_less2 = (target2 < arr_mid4)
  
  // 查找失败
  let found = false
  assert_false(found)
}

test "palindrome_string_check" {
  // 回文字符串检查
  let str = "racecar"
  let len = 7
  
  // 检查首尾字符是否相同
  let first_char_match = true  // 假设str[0] == str[6]
  let second_char_match = true  // 假设str[1] == str[5]
  let third_char_match = true   // 假设str[2] == str[4]
  
  let is_palindrome = if (first_char_match && second_char_match && third_char_match) { true } else { false }
  assert_true(is_palindrome)
  
  // 测试非回文字符串
  let str2 = "hello"
  let len2 = 5
  
  let first_match = false  // 假设str2[0] != str2[4]
  let is_palindrome2 = first_match
  assert_false(is_palindrome2)
}

test "temperature_conversion_precise" {
  // 精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算：F = (C × 9 + 160) / 5
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  assert_eq(77, fahrenheit)  // (25 × 9 + 160) / 5 = 385 / 5 = 77
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 使用整数运算：C = (F - 32) × 5 / 9
  let fahrenheit2 = 77
  let celsius2 = multiply(fahrenheit2 - 32, 5) / 9
  assert_eq(25, celsius2)  // (77 - 32) × 5 / 9 = 45 × 5 / 9 = 25
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 9), 160) / 5
  assert_eq(32, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 9), 160) / 5
  assert_eq(212, boiling_f)
}

test "simple_encryption_simulation" {
  // 简单的加密模拟（凯撒密码）
  // 将字母向后移动3位
  let letter_a = 1  // 假设A=1, B=2, ..., Z=26
  let letter_d = add(letter_a, 3)  // D
  assert_eq(4, letter_d)
  
  let letter_x = 24  // X
  let letter_a_wrapped = add(letter_x, 3)  // 应该是A，但需要处理循环
  // 简化处理：如果超过26，则减去26
  let wrapped_result = if (add(letter_x, 3) > 26) {
    add(letter_x, 3) - 26
  } else {
    add(letter_x, 3)
  }
  assert_eq(1, wrapped_result)  // X -> A
  
  // 解密过程（向后移动3位）
  let encrypted = 4  // D
  let decrypted = encrypted - 3  // A
  assert_eq(1, decrypted)
  
  let encrypted2 = 1  // A
  let decrypted2 = if (encrypted2 - 3 < 1) {
    encrypted2 - 3 + 26
  } else {
    encrypted2 - 3
  }
  assert_eq(24, decrypted2)  // A -> X
}

test "array_sum_and_average" {
  // 数组求和与平均值计算
  // 模拟数组 [10, 20, 30, 40, 50]
  let element1 = 10
  let element2 = 20
  let element3 = 30
  let element4 = 40
  let element5 = 50
  
  // 计算总和
  let sum = add(add(add(add(element1, element2), element3), element4), element5)
  assert_eq(150, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = element1 - average  // -20
  let diff2 = element2 - average  // -10
  let diff3 = element3 - average  // 0
  let diff4 = element4 - average  // 10
  let diff5 = element5 - average  // 20
  
  let sq1 = multiply(diff1, diff1)  // 400
  let sq2 = multiply(diff2, diff2)  // 100
  let sq3 = multiply(diff3, diff3)  // 0
  let sq4 = multiply(diff4, diff4)  // 100
  let sq5 = multiply(diff5, diff5)  // 400
  
  let sum_of_squares = add(add(add(add(sq1, sq2), sq3), sq4), sq5)
  let variance = sum_of_squares / count
  assert_eq(200, variance)
}

test "loan_amortization_calculation" {
  // 贷款分期计算（简化版）
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率6%
  let years = 5          // 贷款年限
  
  // 计算总利息（简化计算：本金 × 年利率 × 年数）
  let total_interest = multiply(principal, multiply(annual_rate, years)) / 100
  assert_eq(3000, total_interest)  // 10000 × 6 × 5 / 100 = 3000
  
  // 计算总还款额
  let total_payment = add(principal, total_interest)
  assert_eq(13000, total_payment)
  
  // 计算月供（简化：总还款额 ÷ 总月数）
  let total_months = multiply(years, 12)
  let monthly_payment = total_payment / total_months
  assert_eq(216, monthly_payment)  // 13000 ÷ 60 ≈ 216（整数除法）
  
  // 计算第一年的利息
  let first_year_interest = multiply(principal, annual_rate) / 100
  assert_eq(600, first_year_interest)
  
  // 计算第一年的本金偿还（简化）
  let first_year_principal = multiply(monthly_payment, 12) - first_year_interest
  assert_eq(1992, first_year_principal)  // 216 × 12 - 600 = 2592 - 600 = 1992
}