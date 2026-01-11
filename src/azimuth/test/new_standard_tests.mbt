// 新增的测试用例 - 补充现有测试覆盖范围

// 测试用例 1: 测试大数相加的边界情况
///|
test "large_number_addition_boundaries" {
  // 测试接近最大值的大数相加
  let near_max1 = 2147483640
  let near_max2 = 7
  
  // 应该溢出到最大值
  assert_eq(2147483647, @azimuth.add(near_max1, near_max2))
  
  // 测试接近最小值的大数相加
  let near_min1 = -2147483640
  let near_min2 = -7
  
  // 应该溢出到最小值
  assert_eq(-2147483648, @azimuth.add(near_min1, near_min2))
  
  // 测试不溢出的大数相加
  let large1 = 1000000000
  let large2 = 1000000000
  assert_eq(2000000000, @azimuth.add(large1, large2))
}

// 测试用例 2: 测试乘法中的负数边界情况
///|
test "negative_multiplication_boundaries" {
  // 测试负数乘以负数得到正数的边界
  let neg1 = -46340
  let neg2 = -46340
  
  // 应该接近但不溢出
  assert_eq(2147395600, @azimuth.multiply(neg1, neg2))
  
  // 测试负数乘以负数溢出
  let neg3 = -46341
  let neg4 = -46341
  
  // 应该溢出到最大值
  assert_eq(2147483647, @azimuth.multiply(neg3, neg4))
  
  // 测试大负数乘以小正数
  let large_neg = -2147483648
  let small_pos = 2
  
  // 应该溢出到最小值
  assert_eq(-2147483648, @azimuth.multiply(large_neg, small_pos))
}

// 测试用例 3: 测试字符串处理的高级场景
///|
test "advanced_string_processing" {
  // 测试包含数字和特殊字符的字符串
  assert_eq("Hello, 12345!", @azimuth.greet("12345"))
  assert_eq("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  assert_eq("Hello, C:\\Program Files!", @azimuth.greet("C:\\Program Files"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  assert_eq("Hello,   Multiple   Spaces!", @azimuth.greet("   Multiple   Spaces"))
  
  // 测试Unicode emoji
  assert_eq("Hello, 👋🌍!", @azimuth.greet("👋🌍"))
  assert_eq("Hello, 🚀🎉!", @azimuth.greet("🚀🎉"))
}

// 测试用例 4: 测试数学运算的幂次计算
///|
test "power_calculation_simulation" {
  // 通过重复乘法模拟幂次运算
  
  // 2^4 = 16
  let two_to_four = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  assert_eq(16, two_to_four)
  
  // 3^3 = 27
  let three_to_three = @azimuth.multiply(@azimuth.multiply(3, 3), 3)
  assert_eq(27, three_to_three)
  
  // (-2)^3 = -8
  let neg_two_to_three = @azimuth.multiply(@azimuth.multiply(-2, -2), -2)
  assert_eq(-8, neg_two_to_three)
  
  // (-2)^4 = 16
  let neg_two_to_four = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(-2, -2), -2), -2)
  assert_eq(16, neg_two_to_four)
}

// 测试用例 5: 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 计算前几个斐波那契数
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  let f9 = @azimuth.add(f8, f7)  // 34
  let f10 = @azimuth.add(f9, f8) // 55
  
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

// 测试用例 6: 测试阶乘计算
///|
test "factorial_calculation" {
  // 通过重复乘法计算阶乘
  // n! = n * (n-1) * ... * 1
  
  // 1! = 1
  let fact1 = 1
  assert_eq(1, fact1)
  
  // 2! = 2
  let fact2 = @azimuth.multiply(2, 1)
  assert_eq(2, fact2)
  
  // 3! = 6
  let fact3 = @azimuth.multiply(@azimuth.multiply(3, 2), 1)
  assert_eq(6, fact3)
  
  // 4! = 24
  let fact4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(4, 3), 2), 1)
  assert_eq(24, fact4)
  
  // 5! = 120
  let fact5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  assert_eq(120, fact5)
}

// 测试用例 7: 测试最大公约数计算（模拟）
///|
test "gcd_calculation_simulation" {
  // 通过减法模拟最大公约数计算
  // gcd(a, b) = gcd(b, a mod b)
  // 这里我们用减法来模拟模运算
  
  // gcd(48, 18) = 6
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
  // 6 - 6 = 0，所以gcd是6
  
  assert_eq(6, step3)
  assert_eq(6, step4)
}

// 测试用例 8: 测试百分比计算
///|
test "percentage_calculation" {
  // 百分比计算：部分 = 总数 * 百分比
  // 我们通过乘以100来避免小数
  
  let total = 200
  let percentage_25 = 25  // 25%
  let percentage_50 = 50  // 50%
  let percentage_75 = 75  // 75%
  
  // 25% of 200 = 50
  let part25_times_100 = @azimuth.multiply(total, percentage_25)
  assert_eq(5000, part25_times_100)  // 50 * 100 = 5000
  
  // 50% of 200 = 100
  let part50_times_100 = @azimuth.multiply(total, percentage_50)
  assert_eq(10000, part50_times_100)  // 100 * 100 = 10000
  
  // 75% of 200 = 150
  let part75_times_100 = @azimuth.multiply(total, percentage_75)
  assert_eq(15000, part75_times_100)  // 150 * 100 = 15000
}

// 测试用例 9: 测试复利计算
///|
test "compound_interest_calculation" {
  // 复利计算：最终金额 = 本金 * (1 + 利率)^时间
  // 我们通过避免小数来计算
  
  let principal = 1000
  let rate_percent = 5  // 5%
  let years = 3
  
  // (1 + 5%)^3 = 1.05^3 ≈ 1.157625
  // 我们通过乘以1000000来避免小数
  
  // 105% = 105000/100000
  let rate_plus_100 = 105000
  
  // 第一年后的金额（乘以100000）
  let year1_times_100000 = @azimuth.multiply(principal, rate_plus_100)
  
  // 第二年后的金额（乘以100000）
  let year2_times_100000 = @azimuth.multiply(year1_times_100000, rate_plus_100)
  
  // 第三年后的金额（乘以100000）
  let year3_times_100000 = @azimuth.multiply(year2_times_100000, rate_plus_100)
  
  // 验证计算过程正确
  assert_eq(105000000, year1_times_100000)  // 1000 * 105000
  assert_eq(11025000000, year2_times_100000)  // 105000000 * 105000
}

// 测试用例 10: 测试统计学中的方差计算
///|
test "variance_calculation" {
  // 方差计算：Var = Σ(x - μ)² / n
  // 其中μ是平均值，n是数据点数量
  
  // 数据集：[2, 4, 4, 4, 5, 5, 7, 9]
  // 平均值：5
  
  let mean = 5
  let n = 8
  
  // 计算每个数据点与平均值的差的平方
  let diff1 = @azimuth.multiply(@azimuth.add(2, @azimuth.multiply(-1, mean)), @azimuth.add(2, @azimuth.multiply(-1, mean)))  // (2-5)² = 9
  let diff2 = @azimuth.multiply(@azimuth.add(4, @azimuth.multiply(-1, mean)), @azimuth.add(4, @azimuth.multiply(-1, mean)))  // (4-5)² = 1
  let diff3 = diff2  // (4-5)² = 1
  let diff4 = diff2  // (4-5)² = 1
  let diff5 = @azimuth.multiply(@azimuth.add(5, @azimuth.multiply(-1, mean)), @azimuth.add(5, @azimuth.multiply(-1, mean)))  // (5-5)² = 0
  let diff6 = diff5  // (5-5)² = 0
  let diff7 = @azimuth.multiply(@azimuth.add(7, @azimuth.multiply(-1, mean)), @azimuth.add(7, @azimuth.multiply(-1, mean)))  // (7-5)² = 4
  let diff8 = @azimuth.multiply(@azimuth.add(9, @azimuth.multiply(-1, mean)), @azimuth.add(9, @azimuth.multiply(-1, mean)))  // (9-5)² = 16
  
  // 计算平方和
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(diff1, diff2), diff3), diff4), diff5), diff6), diff7), diff8)
  
  // 验证计算结果
  assert_eq(9, diff1)
  assert_eq(1, diff2)
  assert_eq(0, diff5)
  assert_eq(4, diff7)
  assert_eq(16, diff8)
  assert_eq(32, sum_of_squares)  // 9 + 1 + 1 + 1 + 0 + 0 + 4 + 16 = 32
}