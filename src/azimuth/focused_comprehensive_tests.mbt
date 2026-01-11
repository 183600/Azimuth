// 专注的综合测试用例 - 覆盖核心功能和边界条件

// 测试1: 加法结合律验证 (a + b) + c = a + (b + c)
///|
test "addition_associative_law" {
  let a = 12345
  let b = -6789
  let c = 10101
  
  // 验证 (a + b) + c = a + (b + c)
  let left_side = add(add(a, b), c)
  let right_side = add(a, add(b, c))
  
  assert_eq(left_side, right_side)
  
  // 使用边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 验证 (max_val + min_val) + zero = max_val + (min_val + zero)
  let left_boundary = add(add(max_val, min_val), zero)
  let right_boundary = add(max_val, add(min_val, zero))
  
  assert_eq(left_boundary, right_boundary)
}

// 测试2: 乘法结合律验证 (a * b) * c = a * (b * c)
///|
test "multiplication_associative_law" {
  let a = 12
  let b = 34
  let c = 56
  
  // 验证 (a * b) * c = a * (b * c)
  let left_side = multiply(multiply(a, b), c)
  let right_side = multiply(a, multiply(b, c))
  
  assert_eq(left_side, right_side)
  
  // 使用负数测试
  let x = -7
  let y = 13
  let z = -19
  
  let left_negative = multiply(multiply(x, y), z)
  let right_negative = multiply(x, multiply(y, z))
  
  assert_eq(left_negative, right_negative)
}

// 测试3: 零元素性质验证
///|
test "zero_element_properties" {
  // 加法零元素：a + 0 = a
  let test_values = [0, 1, -1, 42, -42, 2147483647, -2147483648]
  
  for value in test_values {
    assert_eq(value, add(value, 0))
    assert_eq(value, add(0, value))
  }
  
  // 乘法零元素：a * 0 = 0
  for value in test_values {
    assert_eq(0, multiply(value, 0))
    assert_eq(0, multiply(0, value))
  }
}

// 测试4: 负数运算性质
///|
test "negative_number_operations" {
  let positive = 42
  let negative = -42
  
  // 验证相反数性质：a + (-a) = 0
  assert_eq(0, add(positive, negative))
  
  // 验证负数乘法：(-a) * (-b) = a * b
  let a = 7
  let b = 13
  let negative_a = -a
  let negative_b = -b
  
  let negative_product = multiply(negative_a, negative_b)
  let positive_product = multiply(a, b)
  
  assert_eq(negative_product, positive_product)
  
  // 验证符号规则：(-a) * b = -(a * b)
  let mixed_negative = multiply(negative_a, b)
  let negated_positive = multiply(-1, positive_product)
  
  assert_eq(mixed_negative, negated_positive)
}

// 测试5: 大数运算测试
///|
test "large_number_operations" {
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 测试大数加法
  let sum_result = add(large_num1, large_num2)
  assert_eq(3000000, sum_result)
  
  // 测试大数乘法
  let product_result = multiply(large_num1, 2)
  assert_eq(2000000, product_result)
  
  // 测试接近边界的运算
  let near_max = 2147483640
  let small_increment = 7
  
  let near_max_result = add(near_max, small_increment)
  assert_eq(2147483647, near_max_result)
}

// 测试6: 字符串拼接综合测试
///|
test "comprehensive_string_concatenation" {
  // 测试基本拼接
  assert_eq("Hello, World!", greet("World"))
  
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  assert_eq("Hello, VeryLongNameThatExceedsNormalLength!", greet(long_name))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
  
  // 测试Unicode字符（如果支持）
  let unicode_name = "测试"
  assert_eq("Hello, 测试!", greet(unicode_name))
}

// 测试7: 数学恒等式验证 - 完全平方公式
///|
test "perfect_square_identity" {
  // 验证 (a + b)² = a² + 2ab + b²
  let a = 15
  let b = 8
  
  // 计算 (a + b)²
  let sum = add(a, b)
  let left_side = multiply(sum, sum)
  
  // 计算 a² + 2ab + b²
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let two_ab = multiply(2, multiply(a, b))
  let right_side = add(add(a_squared, two_ab), b_squared)
  
  assert_eq(left_side, right_side)
  
  // 使用负数验证
  let x = -12
  let y = 7
  
  let sum_neg = add(x, y)
  let left_neg = multiply(sum_neg, sum_neg)
  
  let x_squared = multiply(x, x)
  let y_squared = multiply(y, y)
  let two_xy = multiply(2, multiply(x, y))
  let right_neg = add(add(x_squared, two_xy), y_squared)
  
  assert_eq(left_neg, right_neg)
}

// 测试8: 实际应用 - 计算平均分
///|
test "average_score_calculation" {
  // 计算学生平均分
  let scores = [85, 92, 78, 96, 89]
  
  // 计算总分
  let total1 = add(scores[0], scores[1])
  let total2 = add(total1, scores[2])
  let total3 = add(total2, scores[3])
  let total_score = add(total3, scores[4])
  
  // 验证总分
  assert_eq(440, total_score)
  
  // 计算平均分（总分/5）
  // 通过验证：平均分 * 5 = 总分
  let average = 88
  let average_times_five = multiply(average, 5)
  
  assert_eq(total_score, average_times_five)
}

// 测试9: 边界条件溢出处理
///|
test "overflow_boundary_conditions" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数加法溢出
  let overflow_add = add(max_val, 1)
  assert_eq(max_val, overflow_add)  // 应该被限制为最大值
  
  // 测试负数加法溢出
  let negative_overflow = add(min_val, -1)
  assert_eq(min_val, negative_overflow)  // 应该被限制为最小值
  
  // 测试乘法溢出
  let large_positive = 46341  // sqrt(max_val) 的近似值
  let overflow_multiply = multiply(large_positive, large_positive)
  assert_eq(max_val, overflow_multiply)  // 应该被限制为最大值
}

// 测试10: 数学序列验证 - 等差数列
///|
test "arithmetic_sequence_properties" {
  // 验证等差数列性质：第n项 = a + (n-1)d
  // 验证前n项和 = n/2 * (2a + (n-1)d)
  
  let first_term = 3      // 首项 a
  let difference = 5      // 公差 d
  let n = 6              // 项数
  
  // 计算第6项：a + (n-1)d = 3 + 5*5 = 28
  let nth_term = add(first_term, multiply(difference, add(n, -1)))
  assert_eq(28, nth_term)
  
  // 计算前6项和：n/2 * (2a + (n-1)d) = 6/2 * (6 + 25) = 3 * 31 = 93
  // 避免除法：验证 93 * 2 = 6 * 31
  let sum_times_2 = multiply(93, 2)
  let n_times_formula = multiply(n, add(multiply(2, first_term), multiply(difference, add(n, -1))))
  
  assert_eq(sum_times_2, n_times_formula)
}