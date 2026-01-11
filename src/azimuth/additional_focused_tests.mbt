// 额外的重点测试用例
// 专注于特定场景和边界情况

// 测试连续运算的稳定性
///|
test "continuous_operations_stability" {
  // 测试连续加法
  let result1 = add(add(add(1, 2), 3), 4)
  assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
}

// 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 计算前几个斐波那契数
  let fib1 = add(0, 1)  // F1 = 1
  let fib2 = add(1, 1)  // F2 = 2
  let fib3 = add(fib1, fib2)  // F3 = 3
  let fib4 = add(fib2, fib3)  // F4 = 5
  let fib5 = add(fib3, fib4)  // F5 = 8
  
  assert_eq(1, fib1)
  assert_eq(2, fib2)
  assert_eq(3, fib3)
  assert_eq(5, fib4)
  assert_eq(8, fib5)
}

// 测试阶乘计算
///|
test "factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let fact1 = multiply(5, 4)
  let fact2 = multiply(fact1, 3)
  let fact3 = multiply(fact2, 2)
  let fact4 = multiply(fact3, 1)
  
  assert_eq(120, fact4)
  
  // 计算 0! = 1 (特殊情况)
  assert_eq(1, multiply(1, 1))
}

// 测试平方差公式：a² - b² = (a + b)(a - b)
///|
test "difference_of_squares" {
  let a = 10
  let b = 3
  
  // 直接计算 a² - b²
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let direct_result = add(a_squared, multiply(-1, b_squared))
  
  // 使用公式 (a + b)(a - b)
  let sum_ab = add(a, b)
  let diff_ab = add(a, multiply(-1, b))
  let formula_result = multiply(sum_ab, diff_ab)
  
  assert_eq(direct_result, formula_result)
  assert_eq(91, direct_result)
}

// 测试二进制运算相关的十进制计算
///|
test "binary_related_calculations" {
  // 测试 2 的幂次方
  let power2_1 = multiply(2, 1)     // 2^1
  let power2_2 = multiply(2, 2)     // 2^2
  let power2_3 = multiply(2, 4)     // 2^3
  let power2_4 = multiply(2, 8)     // 2^4
  let power2_5 = multiply(2, 16)    // 2^5
  
  assert_eq(2, power2_1)
  assert_eq(4, power2_2)
  assert_eq(8, power2_3)
  assert_eq(16, power2_4)
  assert_eq(32, power2_5)
  
  // 测试位运算相关的十进制计算
  // 左移相当于乘以2的幂次方
  let shift_left_3 = multiply(5, 8)  // 5 << 3 = 5 * 2^3 = 40
  assert_eq(40, shift_left_3)
}

// 测试平均数计算
///|
test "average_calculation" {
  // 计算两个数的平均数：(a + b) / 2
  // 我们通过计算 (a + b) 然后验证结果
  let a = 15
  let b = 25
  let sum = add(a, b)
  
  // 验证和是否正确
  assert_eq(40, sum)
  
  // 对于奇数和的情况
  let c = 10
  let d = 21
  let sum2 = add(c, d)
  assert_eq(31, sum2)
}

// 测试复利计算
///|
test "compound_interest_calculation" {
  // 本金
  let principal = 1000
  // 年利率 5%
  let rate_percent = 5
  // 第一年后的金额：本金 + 本金 * 利率
  let year1_gain = multiply(principal, rate_percent)
  let year1_total_times_100 = add(multiply(principal, 100), year1_gain)
  
  // 验证计算结果
  assert_eq(105000, year1_total_times_100)  // 1050 * 100 = 105000
  
  // 第二年基于第一年的总额计算
  // 第一年总额是1050，第二年利息是1050 * 5% = 52.5
  // 第二年总额是1050 + 52.5 = 1102.5
  // 1102.5 * 10000 = 11025000
  let year2_gain = multiply(year1_total_times_100, rate_percent)
  let year2_total_times_10000 = add(multiply(year1_total_times_100, 100), year2_gain)
  
  // 验证第二年结果
  assert_eq(11025000, year2_total_times_10000)  // 1102.5 * 10000
}

// 测试字符串处理：多语言支持
///|
test "multilingual_string_processing" {
  // 测试不同语言的问候
  assert_eq("Hello, 中文!", greet("中文"))
  assert_eq("Hello, English!", greet("English"))
  assert_eq("Hello, 日本語!", greet("日本語"))
  assert_eq("Hello, 한국어!", greet("한국어"))
  assert_eq("Hello, Español!", greet("Español"))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq("Hello, #hashtag!", greet("#hashtag"))
  assert_eq("Hello, $money!", greet("$money"))
}

// 测试数论基础：最大公约数相关的运算
///|
test "number_theory_basics" {
  // 测试欧几里得算法的相关运算
  // 辗转相除法中的减法运算（用加法实现）
  let a = 48
  let b = 18
  
  // a - b = a + (-1 * b)
  let a_minus_b = add(a, multiply(-1, b))
  assert_eq(30, a_minus_b)
  
  // 继续辗转相除
  let b_minus_remainder = add(b, multiply(-2, 12))
  assert_eq(-6, b_minus_remainder)
}

// 测试几何计算：三角形面积
///|
test "triangle_area_calculation" {
  // 使用海伦公式：面积 = √(s(s-a)(s-b)(s-c))
  // 其中 s = (a + b + c) / 2 是半周长
  // 我们只计算中间步骤，不涉及开方
  
  let a = 3
  let b = 4
  let c = 5
  
  // 计算周长
  let perimeter = add(add(a, b), c)
  assert_eq(12, perimeter)
  
  // 计算半周长的2倍（避免除法）
  let semi_perimeter_times_2 = perimeter
  assert_eq(12, semi_perimeter_times_2)
  
  // 计算各项
  let s_minus_a = add(multiply(perimeter, 1), multiply(-2, a))
  let s_minus_b = add(multiply(perimeter, 1), multiply(-2, b))
  let s_minus_c = add(multiply(perimeter, 1), multiply(-2, c))
  
  assert_eq(6, s_minus_a)
  assert_eq(4, s_minus_b)
  assert_eq(2, s_minus_c)
}