// 独特的测试用例集合

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
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  assert_eq(13, fib7)
  assert_eq(21, fib8)
}

// 测试用例 2: 测试阶乘计算
///|
test "factorial_calculation" {
  // 使用乘法计算阶乘
  let fact1 = 1  // 1!
  let fact2 = @azimuth.multiply(2, fact1)  // 2! = 2
  let fact3 = @azimuth.multiply(3, fact2)  // 3! = 6
  let fact4 = @azimuth.multiply(4, fact3)  // 4! = 24
  let fact5 = @azimuth.multiply(5, fact4)  // 5! = 120
  let fact6 = @azimuth.multiply(6, fact5)  // 6! = 720
  
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  assert_eq(720, fact6)
}

// 测试用例 3: 测试幂运算（通过重复乘法）
///|
test "power_operation" {
  // 计算 2^n
  let power2_1 = 2  // 2^1
  let power2_2 = @azimuth.multiply(2, power2_1)  // 2^2 = 4
  let power2_3 = @azimuth.multiply(2, power2_2)  // 2^3 = 8
  let power2_4 = @azimuth.multiply(2, power2_3)  // 2^4 = 16
  let power2_5 = @azimuth.multiply(2, power2_4)  // 2^5 = 32
  
  assert_eq(2, power2_1)
  assert_eq(4, power2_2)
  assert_eq(8, power2_3)
  assert_eq(16, power2_4)
  assert_eq(32, power2_5)
  
  // 计算 3^n
  let power3_1 = 3  // 3^1
  let power3_2 = @azimuth.multiply(3, power3_1)  // 3^2 = 9
  let power3_3 = @azimuth.multiply(3, power3_2)  // 3^3 = 27
  
  assert_eq(3, power3_1)
  assert_eq(9, power3_2)
  assert_eq(27, power3_3)
}

// 测试用例 4: 测试最大公约数（通过减法模拟）
///|
test "gcd_simulation" {
  // 使用减法模拟欧几里得算法
  // GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 48 - 18 = 30
  let step1 = @azimuth.add(a, @azimuth.multiply(-1, b))
  // 30 - 18 = 12
  let step2 = @azimuth.add(step1, @azimuth.multiply(-1, b))
  // 18 - 12 = 6
  let step3 = @azimuth.add(b, @azimuth.multiply(-1, step2))
  // 12 - 6 = 6
  let step4 = @azimuth.add(step2, @azimuth.multiply(-1, step3))
  // 6 - 6 = 0
  let step5 = @azimuth.add(step3, @azimuth.multiply(-1, step4))
  
  assert_eq(6, step3)  // GCD is 6
  assert_eq(0, step5)  // Remainder is 0
}

// 测试用例 5: 测试百分比计算
///|
test "percentage_calculation" {
  // 计算 25% of 80
  // 25% = 25/100 = 1/4
  let value = 80
  let percentage_numerator = 25
  let percentage_denominator = 100
  
  // 结果 * 100 = 值 * 百分比分子
  let result_times_100 = @azimuth.multiply(value, percentage_numerator)
  assert_eq(2000, result_times_100)  // 80 * 25 = 2000, 所以结果是 20
  
  // 计算 150% of 40
  let value2 = 40
  let percentage2_numerator = 150
  let result2_times_100 = @azimuth.multiply(value2, percentage2_numerator)
  assert_eq(6000, result2_times_100)  // 40 * 150 = 6000, 所以结果是 60
}

// 测试用例 6: 测试复利计算
///|
test "compound_interest_calculation" {
  // 本金 = 1000，年利率 = 10%，年限 = 3年
  // 复利公式：A = P * (1 + r)^n
  // 我们避免小数，使用整数计算
  
  let principal = 1000
  let rate_percent = 10
  let years = 3
  
  // (100 + 10) = 110，表示 1.10
  let growth_factor = @azimuth.add(100, rate_percent)
  
  // 第一年后
  let year1 = @azimuth.multiply(principal, growth_factor)  // 1000 * 110 = 110000
  // 第二年后
  let year2 = @azimuth.multiply(year1, growth_factor)     // 110000 * 110 = 12100000
  // 第三年后
  let year3 = @azimuth.multiply(year2, growth_factor)     // 12100000 * 110 = 133100000
  
  // 验证结果：133100000 / 100^3 = 1331
  assert_eq(133100000, year3)
}

// 测试用例 7: 测试字符串长度相关的计算
///|
test "string_length_calculations" {
  // 测试不同长度字符串的拼接
  let empty = ""
  let single_char = "A"
  let word = "Hello"
  let sentence = "This is a test"
  
  // 测试空字符串
  let empty_greeting = @azimuth.greet(empty)
  assert_eq("Hello, !", empty_greeting)
  
  // 测试单字符
  let single_greeting = @azimuth.greet(single_char)
  assert_eq("Hello, A!", single_greeting)
  
  // 测试单词
  let word_greeting = @azimuth.greet(word)
  assert_eq("Hello, Hello!", word_greeting)
  
  // 测试句子
  let sentence_greeting = @azimuth.greet(sentence)
  assert_eq("Hello, This is a test!", sentence_greeting)
  
  // 验证长度递增
  assert_eq(true, empty_greeting.length() < single_greeting.length())
  assert_eq(true, single_greeting.length() < word_greeting.length())
  assert_eq(true, word_greeting.length() < sentence_greeting.length())
}

// 测试用例 8: 测试数字系统转换（二进制到十进制）
///|
test "binary_to_decimal_conversion" {
  // 二进制 1011 = 1*2^3 + 0*2^2 + 1*2^1 + 1*2^0 = 8 + 0 + 2 + 1 = 11
  
  let bit0 = 1  // 2^0
  let bit1 = @azimuth.multiply(2, 1)  // 2^1 = 2
  let bit2 = @azimuth.multiply(2, bit1)  // 2^2 = 4
  let bit3 = @azimuth.multiply(2, bit2)  // 2^3 = 8
  
  // 1011 (从右到左)
  let decimal = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(1, bit0),  // 1 * 1 = 1
    @azimuth.multiply(1, bit1)   // 1 * 2 = 2
  ), @azimuth.multiply(0, bit2)), @azimuth.multiply(1, bit3))  // 0 * 4 = 0, 1 * 8 = 8
  
  assert_eq(11, decimal)
  
  // 二进制 1101 = 1*2^3 + 1*2^2 + 0*2^1 + 1*2^0 = 8 + 4 + 0 + 1 = 13
  let decimal2 = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(1, bit0),  // 1 * 1 = 1
    @azimuth.multiply(0, bit1)   // 0 * 2 = 0
  ), @azimuth.multiply(1, bit2)), @azimuth.multiply(1, bit3))  // 1 * 4 = 4, 1 * 8 = 8
  
  assert_eq(13, decimal2)
}

// 测试用例 9: 测试几何级数求和
///|
test "geometric_series_sum" {
  // 几何级数：1 + 2 + 4 + 8 + 16 = 31
  // 公比 r = 2，项数 n = 5
  
  let term1 = 1
  let term2 = @azimuth.multiply(2, term1)  // 2
  let term3 = @azimuth.multiply(2, term2)  // 4
  let term4 = @azimuth.multiply(2, term3)  // 8
  let term5 = @azimuth.multiply(2, term4)  // 16
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  assert_eq(31, sum)
  
  // 另一个几何级数：3 + 6 + 12 + 24 = 45
  // 公比 r = 2，首项 a = 3
  
  let geo_term1 = 3
  let geo_term2 = @azimuth.multiply(2, geo_term1)  // 6
  let geo_term3 = @azimuth.multiply(2, geo_term2)  // 12
  let geo_term4 = @azimuth.multiply(2, geo_term3)  // 24
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(geo_term1, geo_term2), geo_term3), geo_term4)
  assert_eq(45, geo_sum)
}

// 测试用例 10: 测试排列组合计算
///|
test "permutation_calculation" {
  // 排列 P(n,k) = n! / (n-k)!
  // P(5,3) = 5! / 2! = 120 / 2 = 60
  
  let n = 5
  let k = 3
  
  // 计算 5! = 120
  let fact5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  
  // 计算 2! = 2
  let fact2 = @azimuth.multiply(2, 1)
  
  // P(5,3) = 60
  // 我们通过验证 60 * 2! = 5! 来确认
  let permutation_times_fact2 = @azimuth.multiply(60, fact2)
  assert_eq(fact5, permutation_times_fact2)
  
  // 组合 C(n,k) = n! / (k! * (n-k)!)
  // C(5,2) = 5! / (2! * 3!) = 120 / (2 * 6) = 10
  
  // 计算 3! = 6
  let fact3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  
  // C(5,2) = 10
  // 我们通过验证 10 * 2! * 3! = 5! 来确认
  let combination_times_fact2_fact3 = @azimuth.multiply(10, @azimuth.multiply(fact2, fact3))
  assert_eq(fact5, combination_times_fact2_fact3)
}