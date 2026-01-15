// 额外的 MoonBit 测试用例
// 覆盖不同的数学运算和字符串处理场景

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(25, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "calculation_pyramid_pattern" {
  // 测试金字塔模式计算：1 + 2 + 3 + 2 + 1
  let level1 = 1
  let level2 = 2
  let level3 = 3
  
  let pyramid_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(level1, level2), level3), level2), level1)
  @azimuth.assert_eq(9, pyramid_sum)
  
  // 更大的金字塔：1 + 2 + 3 + 4 + 3 + 2 + 1
  let level4 = 4
  let larger_pyramid = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 3), 2), 1)
  @azimuth.assert_eq(16, larger_pyramid)
}

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  
  // 验证斐波那契性质：fib(n) * fib(n+2) = fib(n+1)² + (-1)^(n+1)
  let left_side = @azimuth.multiply(fib4, fib6)  // fib(4) * fib(6) = 3 * 8 = 24
  let right_side = @azimuth.add(@azimuth.multiply(fib5, fib5), -1)  // fib(5)² - 1 = 25 - 1 = 24
  @azimuth.assert_eq(left_side, right_side)
}

test "string_pattern_processing" {
  // 测试字符串模式处理
  // 测试重复字符模式
  let repeated_chars = "AAA"
  @azimuth.assert_eq_string("Hello, AAA!", @azimuth.greet(repeated_chars))
  
  // 测试交替模式
  let alternating_pattern = "ABABAB"
  @azimuth.assert_eq_string("Hello, ABABAB!", @azimuth.greet(alternating_pattern))
  
  // 测试数字字母混合模式
  let alphanumeric_pattern = "A1B2C3"
  @azimuth.assert_eq_string("Hello, A1B2C3!", @azimuth.greet(alphanumeric_pattern))
  
  // 测试递增模式
  let incrementing_pattern = "12345"
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(incrementing_pattern))
}

test "geometric_series_sum" {
  // 测试等比数列求和：2 + 6 + 18 + 54 = 80
  let first_term = 2
  let ratio = 3
  let term2 = @azimuth.multiply(first_term, ratio)  // 6
  let term3 = @azimuth.multiply(term2, ratio)       // 18
  let term4 = @azimuth.multiply(term3, ratio)       // 54
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(first_term, term2), term3), term4)
  @azimuth.assert_eq(80, series_sum)
  
  // 验证等比数列性质：相邻两项的比值相等
  @azimuth.assert_eq(term2 / first_term, term3 / term2)
  @azimuth.assert_eq(term3 / term2, term4 / term3)
}

test "number_base_conversion_simulation" {
  // 测试数字进制转换模拟（二进制转十进制）
  // 二进制 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let power_of_2_0 = 1
  let power_of_2_1 = @azimuth.multiply(2, 1)      // 2
  let power_of_2_2 = @azimuth.multiply(2, 2)      // 4
  let power_of_2_3 = @azimuth.multiply(2, 4)      // 8
  
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(bit3, power_of_2_3), @azimuth.multiply(bit2, power_of_2_2)), @azimuth.multiply(bit1, power_of_2_1)), @azimuth.multiply(bit0, power_of_2_0))
  @azimuth.assert_eq(11, decimal_value)
  
  // 二进制 1111 = 1×2³ + 1×2² + 1×2¹ + 1×2⁰ = 8 + 4 + 2 + 1 = 15
  let all_ones_decimal = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.multiply(1, 2)), @azimuth.multiply(1, 1))
  @azimuth.assert_eq(15, all_ones_decimal)
}

test "matrix_multiplication_simulation" {
  // 测试矩阵乘法模拟（2x2 矩阵）
  // 矩阵 A: [[a, b], [c, d]], 矩阵 B: [[e, f], [g, h]]
  // 结果矩阵 C: [[a×e + b×g, a×f + b×h], [c×e + d×g, c×f + d×h]]
  
  let a = 1; let b = 2; let c = 3; let d = 4
  let e = 5; let f = 6; let g = 7; let h = 8
  
  // 计算结果矩阵的各个元素
  let c11 = @azimuth.add(@azimuth.multiply(a, e), @azimuth.multiply(b, g))  // 1×5 + 2×7 = 5 + 14 = 19
  let c12 = @azimuth.add(@azimuth.multiply(a, f), @azimuth.multiply(b, h))  // 1×6 + 2×8 = 6 + 16 = 22
  let c21 = @azimuth.add(@azimuth.multiply(c, e), @azimuth.multiply(d, g))  // 3×5 + 4×7 = 15 + 28 = 43
  let c22 = @azimuth.add(@azimuth.multiply(c, f), @azimuth.multiply(d, h))  // 3×6 + 4×8 = 18 + 32 = 50
  
  @azimuth.assert_eq(19, c11)
  @azimuth.assert_eq(22, c12)
  @azimuth.assert_eq(43, c21)
  @azimuth.assert_eq(50, c22)
  
  // 验证矩阵乘法的行列式性质：det(AB) = det(A) × det(B)
  let det_a = @azimuth.add(@azimuth.multiply(a, d), @azimuth.multiply(-b, c))  // 1×4 - 2×3 = 4 - 6 = -2
  let det_b = @azimuth.add(@azimuth.multiply(e, h), @azimuth.multiply(-f, g))  // 5×8 - 6×7 = 40 - 42 = -2
  let det_ab = @azimuth.add(@azimuth.multiply(c11, c22), @azimuth.multiply(-c12, c21))  // 19×50 - 22×43 = 950 - 946 = 4
  
  @azimuth.assert_eq(@azimuth.multiply(det_a, det_b), det_ab)
}

test "prime_number_validation" {
  // 测试质数验证（通过试除法）
  let num_to_test = 17
  
  // 检查是否能被小于等于其平方根的质数整除
  let sqrt_17 = 4  // sqrt(17) ≈ 4.12，取整数部分
  
  // 检查能否被2整除
  let divisible_by_2 = num_to_test % 2 == 0
  @azimuth.assert_false(divisible_by_2)
  
  // 检查能否被3整除
  let divisible_by_3 = num_to_test % 3 == 0
  @azimuth.assert_false(divisible_by_3)
  
  // 检查能否被4整除
  let divisible_by_4 = num_to_test % 4 == 0
  @azimuth.assert_false(divisible_by_4)
  
  // 由于17不能被2、3、4整除，所以它是质数
  @azimuth.assert_true(true)  // 17是质数
  
  // 测试合数18
  let num_to_test_2 = 18
  let divisible_by_2_2 = num_to_test_2 % 2 == 0
  @azimuth.assert_true(divisible_by_2_2)  // 18能被2整除，所以是合数
}

test "polynomial_evaluation" {
  // 测试多项式求值：f(x) = 2x³ - 3x² + 4x - 5，求f(3)
  let x = 3
  let a = 2  // x³的系数
  let b = -3  // x²的系数
  let c = 4  // x的系数
  let d = -5  // 常数项
  
  // 计算 x³
  let x_cubed = @azimuth.multiply(@azimuth.multiply(x, x), x)  // 27
  
  // 计算 x²
  let x_squared = @azimuth.multiply(x, x)  // 9
  
  // 计算各项
  let term1 = @azimuth.multiply(a, x_cubed)    // 2×27 = 54
  let term2 = @azimuth.multiply(b, x_squared)  // -3×9 = -27
  let term3 = @azimuth.multiply(c, x)          // 4×3 = 12
  let term4 = d                                // -5
  
  // 计算多项式值
  let polynomial_value = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(34, polynomial_value)  // 54 - 27 + 12 - 5 = 34
  
  // 使用霍纳法则验证：f(x) = ((2x - 3)x + 4)x - 5
  let horner_step1 = @azimuth.add(@azimuth.multiply(2, x), -3)  // 2×3 - 3 = 3
  let horner_step2 = @azimuth.add(@azimuth.multiply(horner_step1, x), 4)  // 3×3 + 4 = 13
  let horner_result = @azimuth.add(@azimuth.multiply(horner_step2, x), -5)  // 13×3 - 5 = 34
  
  @azimuth.assert_eq(polynomial_value, horner_result)
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  // 计算排列数 P(n, k) = n! / (n-k)!
  // 计算 P(5, 3) = 5! / 2! = 120 / 2 = 60
  
  let n = 5
  let k = 3
  
  // 计算 5! = 5×4×3×2×1 = 120
  let five_factorial = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  
  // 计算 2! = 2×1 = 2
  let two_factorial = @azimuth.multiply(2, 1)
  
  // 计算 P(5, 3) = 5! / 2! = 120 / 2 = 60
  let permutation = five_factorial / two_factorial
  @azimuth.assert_eq(60, permutation)
  
  // 计算组合数 C(n, k) = P(n, k) / k!
  // 计算 C(5, 3) = P(5, 3) / 3! = 60 / 6 = 10
  
  // 计算 3! = 3×2×1 = 6
  let three_factorial = @azimuth.multiply(@azimuth.multiply(3, 2), 1)
  
  // 计算 C(5, 3) = 60 / 6 = 10
  let combination = permutation / three_factorial
  @azimuth.assert_eq(10, combination)
  
  // 验证组合数性质：C(n, k) = C(n, n-k)
  // C(5, 3) = C(5, 2) = 10
  let n_minus_k = @azimuth.add(n, -k)  // 5 - 3 = 2
  
  // 计算 P(5, 2) = 5! / 3! = 120 / 6 = 20
  let permutation_5_2 = five_factorial / three_factorial
  
  // 计算 C(5, 2) = P(5, 2) / 2! = 20 / 2 = 10
  let combination_5_2 = permutation_5_2 / two_factorial
  
  @azimuth.assert_eq(combination, combination_5_2)
}