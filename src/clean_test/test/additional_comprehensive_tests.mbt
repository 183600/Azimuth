// 额外的综合测试用例 - 补充现有测试覆盖范围

// 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 使用加法计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  assert_eq(34, f9)
  assert_eq(55, f10)
}

// 测试阶乘计算
///|
test "factorial_calculation" {
  // n! = n * (n-1) * (n-2) * ... * 1
  // 0! = 1, 1! = 1, 2! = 2, 3! = 6, 4! = 24, 5! = 120
  
  let fact0 = 1  // 0! = 1
  let fact1 = 1  // 1! = 1
  let fact2 = @azimuth.multiply(2, fact1)  // 2! = 2
  let fact3 = @azimuth.multiply(3, fact2)  // 3! = 6
  let fact4 = @azimuth.multiply(4, fact3)  // 4! = 24
  let fact5 = @azimuth.multiply(5, fact4)  // 5! = 120
  let fact6 = @azimuth.multiply(6, fact5)  // 6! = 720
  let fact7 = @azimuth.multiply(7, fact6)  // 7! = 5040
  
  assert_eq(1, fact0)
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  assert_eq(720, fact6)
  assert_eq(5040, fact7)
}

// 测试最大公约数（GCD）的欧几里得算法模拟
///|
test "gcd_euclidean_algorithm" {
  // 模拟欧几里得算法：gcd(a, b) = gcd(b, a mod b)
  // 我们用减法来模拟模运算
  
  // gcd(48, 18) = 6
  let a1 = 48
  let b1 = 18
  // 48 mod 18 = 48 - 2*18 = 12
  let remainder1 = @azimuth.add(a1, @azimuth.multiply(-2, b1))
  // 18 mod 12 = 18 - 1*12 = 6
  let remainder2 = @azimuth.add(b1, @azimuth.multiply(-1, remainder1))
  // 12 mod 6 = 12 - 2*6 = 0
  let remainder3 = @azimuth.add(remainder1, @azimuth.multiply(-2, remainder2))
  
  assert_eq(12, remainder1)
  assert_eq(6, remainder2)
  assert_eq(0, remainder3)
  
  // gcd(56, 15) = 1
  let a2 = 56
  let b2 = 15
  // 56 mod 15 = 56 - 3*15 = 11
  let remainder4 = @azimuth.add(a2, @azimuth.multiply(-3, b2))
  // 15 mod 11 = 15 - 1*11 = 4
  let remainder5 = @azimuth.add(b2, @azimuth.multiply(-1, remainder4))
  // 11 mod 4 = 11 - 2*4 = 3
  let remainder6 = @azimuth.add(remainder4, @azimuth.multiply(-2, remainder5))
  // 4 mod 3 = 4 - 1*3 = 1
  let remainder7 = @azimuth.add(remainder5, @azimuth.multiply(-1, remainder6))
  
  assert_eq(11, remainder4)
  assert_eq(4, remainder5)
  assert_eq(3, remainder6)
  assert_eq(1, remainder7)
}

// 测试幂运算（通过重复乘法）
///|
test "power_operation" {
  // a^n = a * a * ... * a (n times)
  
  // 2^1 = 2
  let power1 = @azimuth.multiply(2, 1)
  
  // 2^2 = 4
  let power2 = @azimuth.multiply(2, 2)
  
  // 2^3 = 8
  let power3 = @azimuth.multiply(2, @azimuth.multiply(2, 2))
  
  // 2^4 = 16
  let power4 = @azimuth.multiply(2, power3)
  
  // 3^1 = 3
  let power5 = @azimuth.multiply(3, 1)
  
  // 3^2 = 9
  let power6 = @azimuth.multiply(3, 3)
  
  // 3^3 = 27
  let power7 = @azimuth.multiply(3, power6)
  
  // 5^2 = 25
  let power8 = @azimuth.multiply(5, 5)
  
  assert_eq(2, power1)
  assert_eq(4, power2)
  assert_eq(8, power3)
  assert_eq(16, power4)
  assert_eq(3, power5)
  assert_eq(9, power6)
  assert_eq(27, power7)
  assert_eq(25, power8)
}

// 测试更复杂的字符串场景
///|
test "complex_string_scenarios" {
  // 测试包含数字和字母混合的字符串
  assert_eq("Hello, user123!", @azimuth.greet("user123"))
  assert_eq("Hello, Test_2024!", @azimuth.greet("Test_2024"))
  assert_eq("Hello, aBcDeF!", @azimuth.greet("aBcDeF"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  assert_eq("Hello, Multiple  Spaces!", @azimuth.greet("Multiple  Spaces"))
  
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightBeUsedInSomeRealWorldApplication"
  let long_greeting = @azimuth.greet(long_name)
  assert_eq(true, long_greeting.contains("ThisIsAVeryLongNameThatMightBeUsedInSomeRealWorldApplication"))
  
  // 测试只包含数字的字符串
  assert_eq("Hello, 123456789!", @azimuth.greet("123456789"))
}

// 测试递归运算模拟（计算数列的和）
///|
test "recursive_sum_simulation" {
  // 模拟递归计算 1 + 2 + 3 + ... + n
  
  // 计算 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, @azimuth.add(2, @azimuth.add(3, @azimuth.add(4, 5))))
  assert_eq(15, sum1)
  
  // 计算 10 + 9 + 8 + ... + 1 = 55
  let sum2 = @azimuth.add(10, @azimuth.add(9, @azimuth.add(8, @azimuth.add(7, @azimuth.add(6, @azimuth.add(5, @azimuth.add(4, @azimuth.add(3, @azimuth.add(2, 1)))))))))
  assert_eq(55, sum2)
  
  // 计算偶数和：2 + 4 + 6 + 8 + 10 = 30
  let even_sum = @azimuth.add(2, @azimuth.add(4, @azimuth.add(6, @azimuth.add(8, 10))))
  assert_eq(30, even_sum)
  
  // 计算奇数和：1 + 3 + 5 + 7 + 9 = 25
  let odd_sum = @azimuth.add(1, @azimuth.add(3, @azimuth.add(5, @azimuth.add(7, 9))))
  assert_eq(25, odd_sum)
}

// 测试位运算相关的计算（2的幂次）
///|
test "bitwise_related_calculations" {
  // 测试2的幂次：2^0, 2^1, 2^2, 2^3, ...
  
  let power2_0 = 1   // 2^0 = 1
  let power2_1 = 2   // 2^1 = 2
  let power2_2 = 4   // 2^2 = 4
  let power2_3 = 8   // 2^3 = 8
  let power2_4 = 16  // 2^4 = 16
  let power2_5 = 32  // 2^5 = 32
  let power2_6 = 64  // 2^6 = 64
  let power2_7 = 128 // 2^7 = 128
  
  // 通过重复加法验证
  let sum_2_1 = @azimuth.add(1, 1)
  let sum_2_2 = @azimuth.add(@azimuth.add(1, 1), @azimuth.add(1, 1))
  let sum_2_3 = @azimuth.add(sum_2_2, sum_2_2)
  let sum_2_4 = @azimuth.add(sum_2_3, sum_2_3)
  
  assert_eq(1, power2_0)
  assert_eq(2, power2_1)
  assert_eq(4, power2_2)
  assert_eq(8, power2_3)
  assert_eq(16, power2_4)
  assert_eq(32, power2_5)
  assert_eq(64, power2_6)
  assert_eq(128, power2_7)
  
  assert_eq(2, sum_2_1)
  assert_eq(4, sum_2_2)
  assert_eq(8, sum_2_3)
  assert_eq(16, sum_2_4)
}

// 测试百分比计算
///|
test "percentage_calculations" {
  // 百分比计算：x% of y = (x * y) / 100
  // 我们通过乘法来避免除法
  
  // 计算 25% of 200 = 50
  let percent1 = 25
  let value1 = 200
  let result1_times_100 = @azimuth.multiply(percent1, value1)
  assert_eq(5000, result1_times_100)  // 50 * 100 = 5000
  
  // 计算 50% of 150 = 75
  let percent2 = 50
  let value2 = 150
  let result2_times_100 = @azimuth.multiply(percent2, value2)
  assert_eq(7500, result2_times_100)  // 75 * 100 = 7500
  
  // 计算 10% of 300 = 30
  let percent3 = 10
  let value3 = 300
  let result3_times_100 = @azimuth.multiply(percent3, value3)
  assert_eq(3000, result3_times_100)  // 30 * 100 = 3000
  
  // 计算 100% of 250 = 250
  let percent4 = 100
  let value4 = 250
  let result4_times_100 = @azimuth.multiply(percent4, value4)
  assert_eq(25000, result4_times_100)  // 250 * 100 = 25000
}

// 测试比例和分数运算
///|
test "ratio_and_fraction_operations" {
  // 比例计算：a:b = c:d，验证 ad = bc
  
  // 验证 2:3 = 4:6
  let a1 = 2
  let b1 = 3
  let c1 = 4
  let d1 = 6
  let ad1 = @azimuth.multiply(a1, d1)
  let bc1 = @azimuth.multiply(b1, c1)
  assert_eq(ad1, bc1)  // 2*6 = 3*4 = 12
  
  // 验证 3:4 = 9:12
  let a2 = 3
  let b2 = 4
  let c2 = 9
  let d2 = 12
  let ad2 = @azimuth.multiply(a2, d2)
  let bc2 = @azimuth.multiply(b2, c2)
  assert_eq(ad2, bc2)  // 3*12 = 4*9 = 36
  
  // 分数加法：1/2 + 1/3 = 5/6
  // 通分：1/2 = 3/6, 1/3 = 2/6, 3/6 + 2/6 = 5/6
  let numerator1 = 1
  let denominator1 = 2
  let numerator2 = 1
  let denominator2 = 3
  
  // 通分后的分子
  let common_numerator1 = @azimuth.multiply(numerator1, 3)  // 1*3 = 3
  let common_numerator2 = @azimuth.multiply(numerator2, 2)  // 1*2 = 2
  let result_numerator = @azimuth.add(common_numerator1, common_numerator2)  // 3 + 2 = 5
  let common_denominator = 6
  
  assert_eq(3, common_numerator1)
  assert_eq(2, common_numerator2)
  assert_eq(5, result_numerator)
  assert_eq(6, common_denominator)
}

// 测试统计计算（方差）
///|
test "statistical_variance_calculation" {
  // 方差计算：Var = Σ(xi - μ)² / n
  // 其中 μ 是平均值，n 是数据点的数量
  // 我们通过计算平方和来验证
  
  // 数据集：[2, 4, 4, 4, 5, 5, 7, 9]
  // 平均值：5
  // 方差：4
  
  // 计算平均值 * 数据点数量 = 总和
  let mean = 5
  let count = 8
  let sum = @azimuth.multiply(mean, count)
  assert_eq(40, sum)  // 5 * 8 = 40
  
  // 验证实际总和
  let actual_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), @azimuth.add(4, 4)), @azimuth.add(@azimuth.add(5, 5), @azimuth.add(7, 9)))
  assert_eq(40, actual_sum)
  
  // 计算平方差和：(2-5)² + (4-5)² + (4-5)² + (4-5)² + (5-5)² + (5-5)² + (7-5)² + (9-5)²
  // = 9 + 1 + 1 + 1 + 0 + 0 + 4 + 16 = 32
  
  let diff1 = @azimuth.multiply(@azimuth.add(2, @azimuth.multiply(-1, 5)), @azimuth.add(2, @azimuth.multiply(-1, 5)))  // (2-5)² = 9
  let diff2 = @azimuth.multiply(@azimuth.add(4, @azimuth.multiply(-1, 5)), @azimuth.add(4, @azimuth.multiply(-1, 5)))  // (4-5)² = 1
  let diff3 = @azimuth.multiply(@azimuth.add(5, @azimuth.multiply(-1, 5)), @azimuth.add(5, @azimuth.multiply(-1, 5)))  // (5-5)² = 0
  let diff4 = @azimuth.multiply(@azimuth.add(7, @azimuth.multiply(-1, 5)), @azimuth.add(7, @azimuth.multiply(-1, 5)))  // (7-5)² = 4
  let diff5 = @azimuth.multiply(@azimuth.add(9, @azimuth.multiply(-1, 5)), @azimuth.add(9, @azimuth.multiply(-1, 5)))  // (9-5)² = 16
  
  // 平方差和
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(diff1, @azimuth.add(diff2, diff2)), @azimuth.add(diff2, diff3)), @azimuth.add(diff3, @azimuth.add(diff4, diff5)))
  
  assert_eq(9, diff1)
  assert_eq(1, diff2)
  assert_eq(0, diff3)
  assert_eq(4, diff4)
  assert_eq(16, diff5)
  assert_eq(32, sum_of_squares)  // 9 + 1 + 1 + 1 + 0 + 0 + 4 + 16 = 32
  
  // 方差 * 数据点数量 = 平方差和
  let variance_times_count = sum_of_squares
  assert_eq(32, variance_times_count)  // 4 * 8 = 32
}