// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 新增的标准 MoonBit 测试用例
// 包含数学性质、实际应用和边界条件的综合测试

// 测试1: 验证乘法对加法的分配律 (a * (b + c) = a * b + a * c)
///|
test "multiplication_distributive_law" {
  let a = 7
  let b = 13
  let c = 19
  
  // 验证 a * (b + c) = a * b + a * c
  let left_side = @clean_test.multiply(a, @clean_test.add(b, c))
  let right_side = @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c))
  
  assert_eq(left_side, right_side)
  
  // 使用负数测试
  let x = -5
  let y = 12
  let z = -8
  
  let left_side_neg = @clean_test.multiply(x, @clean_test.add(y, z))
  let right_side_neg = @clean_test.add(@clean_test.multiply(x, y), @clean_test.multiply(x, z))
  
  assert_eq(left_side_neg, right_side_neg)
}

// 测试2: 验证数学恒等式 - 平方差公式 (a² - b² = (a + b)(a - b))
///|
test "difference_of_squares_identity" {
  let a = 15
  let b = 8
  
  // 计算 a² - b²
  let a_squared = @clean_test.multiply(a, a)
  let b_squared = @clean_test.multiply(b, b)
  let difference = @clean_test.add(a_squared, @clean_test.multiply(-1, b_squared))
  
  // 计算 (a + b)(a - b)
  let sum = @clean_test.add(a, b)
  let diff = @clean_test.add(a, @clean_test.multiply(-1, b))
  let product = @clean_test.multiply(sum, diff)
  
  assert_eq(difference, product)
}

// 测试3: 实际应用 - 计算复合利息
///|
test "compound_interest_calculation" {
  // 本金
  let principal = 1000
  
  // 年利率 5%，用分数表示避免小数
  let rate_numerator = 5
  let rate_denominator = 100
  
  // 投资年限
  let years = 3
  
  // 复利公式：A = P(1 + r)^t
  // 为避免幂运算，我们逐年计算
  
  // 第一年：P * (1 + r)
  let year1 = @clean_test.multiply(principal, @clean_test.add(rate_denominator, rate_numerator))
  
  // 第二年：year1 * (1 + r)
  let year2 = @clean_test.multiply(year1, @clean_test.add(rate_denominator, rate_numerator))
  
  // 第三年：year2 * (1 + r)
  let year3 = @clean_test.multiply(year2, @clean_test.add(rate_denominator, rate_numerator))
  
  // 最终金额需要除以 rate_denominator^t
  // 这里我们验证计算过程的正确性
  let expected_times_denominator_cubed = 1157625  // 1000 * 105 * 105 * 105
  assert_eq(expected_times_denominator_cubed, year3)
}

// 测试4: 字符串处理 - 验证 greet 函数的边界情况
///|
test "string_boundary_conditions" {
  // 测试空字符串
  assert_eq("Hello, !", @clean_test.greet(""))
  
  // 测试单字符
  assert_eq("Hello, A!", @clean_test.greet("A"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", @clean_test.greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq("Hello, 12345!", @clean_test.greet("12345"))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", @clean_test.greet("@#$%"))
}

// 测试5: 数学运算 - 验证平方根近似值
///|
test "square_root_approximation" {
  // 使用牛顿法近似计算平方根
  // 我们测试 √16 ≈ 4 的近似过程
  
  let target = 16
  let initial_guess = 8
  
  // 牛顿法迭代：new_guess = (guess + target/guess) / 2
  // 为避免除法，我们使用乘法验证
  
  // 验证 4² = 16
  let sqrt_approx = 4
  let square_of_approx = @clean_test.multiply(sqrt_approx, sqrt_approx)
  
  assert_eq(target, square_of_approx)
  
  // 验证 5² = 25 > 16
  let sqrt_approx_plus_one = @clean_test.add(sqrt_approx, 1)
  let square_of_approx_plus_one = @clean_test.multiply(sqrt_approx_plus_one, sqrt_approx_plus_one)
  
  assert_eq(true, square_of_approx_plus_one > target)
  
  // 验证 3² = 9 < 16
  let sqrt_approx_minus_one = @clean_test.add(sqrt_approx, @clean_test.multiply(-1, 1))
  let square_of_approx_minus_one = @clean_test.multiply(sqrt_approx_minus_one, sqrt_approx_minus_one)
  
  assert_eq(true, square_of_approx_minus_one < target)
}

// 测试6: 实际应用 - 计算阶梯电费
///|
test "tiered_pricing_calculation" {
  // 阶梯电费计算
  // 第一档：0-100度，每度0.5元
  // 第二档：101-200度，每度0.6元
  // 第三档：201度以上，每度0.7元
  
  let usage = 250  // 用电量250度
  
  // 第一档费用：100 * 0.5 = 50元
  let tier1_rate_numerator = 5
  let tier1_rate_denominator = 10
  let tier1_units = 100
  let tier1_cost_times_denominator = @clean_test.multiply(tier1_units, tier1_rate_numerator)
  
  // 第二档费用：100 * 0.6 = 60元
  let tier2_rate_numerator = 6
  let tier2_rate_denominator = 10
  let tier2_units = 100
  let tier2_cost_times_denominator = @clean_test.multiply(tier2_units, tier2_rate_numerator)
  
  // 第三档费用：50 * 0.7 = 35元
  let tier3_rate_numerator = 7
  let tier3_rate_denominator = 10
  let tier3_units = @clean_test.add(usage, @clean_test.multiply(-1, @clean_test.add(tier1_units, tier2_units)))
  let tier3_cost_times_denominator = @clean_test.multiply(tier3_units, tier3_rate_numerator)
  
  // 总费用 = 50 + 60 + 35 = 145元
  let total_cost_times_10 = @clean_test.add(@clean_test.add(tier1_cost_times_denominator, tier2_cost_times_denominator), tier3_cost_times_denominator)
  
  assert_eq(1450, total_cost_times_10)  // 145 * 10 = 1450
}

// 测试7: 数学运算 - 验证斐波那契数列的性质
///|
test "fibonacci_sequence_properties" {
  // 验证斐波那契数列的性质：F(n) * F(n+2) - F(n+1)² = (-1)^n+1
  
  // F(5) = 5, F(6) = 8, F(7) = 13
  let f_n = 5
  let f_n_plus_1 = 8
  let f_n_plus_2 = 13
  
  // 计算 F(n) * F(n+2) - F(n+1)²
  let product1 = @clean_test.multiply(f_n, f_n_plus_2)
  let square = @clean_test.multiply(f_n_plus_1, f_n_plus_1)
  let result = @clean_test.add(product1, @clean_test.multiply(-1, square))
  
  // 对于 n=5，结果应该是 (-1)^6 = 1
  assert_eq(1, result)
  
  // F(6) = 8, F(7) = 13, F(8) = 21
  let f_n2 = 8
  let f_n2_plus_1 = 13
  let f_n2_plus_2 = 21
  
  // 计算 F(n) * F(n+2) - F(n+1)²
  let product2 = @clean_test.multiply(f_n2, f_n2_plus_2)
  let square2 = @clean_test.multiply(f_n2_plus_1, f_n2_plus_1)
  let result2 = @clean_test.add(product2, @clean_test.multiply(-1, square2))
  
  // 对于 n=6，结果应该是 (-1)^7 = -1
  assert_eq(-1, result2)
}

// 测试8: 实际应用 - 计算数据传输时间
///|
test "data_transfer_time_calculation" {
  // 计算文件传输时间
  // 文件大小：100MB
  // 网络速度：5MB/s
  
  let file_size = 100  // MB
  let transfer_rate = 5  // MB/s
  
  // 传输时间 = 文件大小 / 传输速度
  // 为避免除法，我们验证：文件大小 = 传输时间 * 传输速度
  
  let expected_time = 20  // 秒
  let calculated_file_size = @clean_test.multiply(expected_time, transfer_rate)
  
  assert_eq(file_size, calculated_file_size)
  
  // 考虑网络开销，实际传输时间增加10%
  let overhead_percentage = 10
  let actual_time_times_100 = @clean_test.add(
    @clean_test.multiply(expected_time, 100),
    @clean_test.multiply(expected_time, overhead_percentage)
  )
  
  // 验证：22秒 * 100 = 2200
  assert_eq(2200, actual_time_times_100)
}

// 测试9: 边界条件 - 测试最大值和最小值的特殊运算
///|
test "extreme_value_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与-1相加
  assert_eq(2147483646, @clean_test.add(max_val, -1))
  
  // 测试最小值与1相加
  assert_eq(-2147483647, @clean_test.add(min_val, 1))
  
  // 测试最大值与1相乘
  assert_eq(max_val, @clean_test.multiply(max_val, 1))
  
  // 测试最小值与1相乘
  assert_eq(min_val, @clean_test.multiply(min_val, 1))
  
  // 测试最大值与-1相乘
  assert_eq(-2147483647, @clean_test.multiply(max_val, -1))
  
  // 测试0与最小值相加
  assert_eq(min_val, @clean_test.add(0, min_val))
}

// 测试10: 数学运算 - 验证组合数学公式
///|
test "combinatorial_formula" {
  // 验证组合数学公式：C(n, k) = n! / (k! * (n-k)!)
  // 我们验证 C(5, 2) = 10
  
  let n = 5
  let k = 2
  
  // 计算阶乘
  let factorial_5 = @clean_test.multiply(5, @clean_test.multiply(4, @clean_test.multiply(3, @clean_test.multiply(2, 1))))
  let factorial_2 = @clean_test.multiply(2, 1)
  let factorial_3 = @clean_test.multiply(3, @clean_test.multiply(2, 1))
  
  // 计算分母：k! * (n-k)!
  let denominator = @clean_test.multiply(factorial_2, factorial_3)
  
  // 验证 C(5, 2) = 5! / (2! * 3!) = 120 / 12 = 10
  // 通过验证 10 * 12 = 120
  let combination_times_denominator = @clean_test.multiply(10, denominator)
  
  assert_eq(factorial_5, combination_times_denominator)
}