// 用户额外测试用例
// 覆盖核心功能的额外测试场景

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 2))
  
  // 边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  
  // 负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
}

test "string_with_unicode_characters" {
  // 测试包含Unicode字符的字符串处理
  // 中文字符
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  
  // 表情符号
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  
  // 混合Unicode字符
  @azimuth.assert_eq_string("Hello, Hello🌍!", @azimuth.greet("Hello🌍"))
  
  // 特殊Unicode符号
  @azimuth.assert_eq_string("Hello, ∑π!", @azimuth.greet("∑π"))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  
  // 大数相乘
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 大数与边界值运算
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, large_num1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 100))
}

test "zero_value_operations" {
  // 测试零值运算的各种情况
  // 零值加法
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 零值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  
  // 零值在复杂表达式中的使用
  @azimuth.assert_eq(10, @azimuth.add(@azimuth.multiply(5, 2), @azimuth.multiply(3, 0)))
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 边界值与-1的运算
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算多项式：3x² + 2x + 1，当x=5时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)
  let term1 = @azimuth.multiply(3, x_squared)
  let term2 = @azimuth.multiply(2, x)
  let polynomial_result = @azimuth.add(@azimuth.add(term1, term2), 1)
  @azimuth.assert_eq(86, polynomial_result)
  
  // 计算平均值：(10 + 20 + 30) / 3
  let sum = @azimuth.add(@azimuth.add(10, 20), 30)
  let average = sum / 3
  @azimuth.assert_eq(20, average)
  
  // 计算百分比：500的25%
  let total = 500
  let percentage = @azimuth.multiply(total, 25) / 100
  @azimuth.assert_eq(125, percentage)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 非常长的字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedInputLength"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c !", @azimuth.greet("a b c "))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  
  // 数字和字母混合
  @azimuth.assert_eq_string("Hello, abc123!", @azimuth.greet("abc123"))
  @azimuth.assert_eq_string("Hello, 123abc!", @azimuth.greet("123abc"))
}

test "mathematical_series" {
  // 测试数学级数计算
  // 计算等差数列前5项和：1 + 3 + 5 + 7 + 9
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, sum1)
  
  // 计算等比数列前4项积：2 × 4 × 8 × 16
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 4), 8), 16)
  @azimuth.assert_eq(1024, product1)
  
  // 计算平方和：1² + 2² + 3² + 4²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4)
  @azimuth.assert_eq(30, sum_of_squares)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算贷款月供（简化版）
  let principal = 100000
  let annual_rate = 6
  let years = 30
  // 简化计算：月供 = (本金 + 本金×年利率×年数) / (年数×12)
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(2777, monthly_payment)
  
  // 计算BMI（简化版，整数计算）
  let weight_kg = 70
  let height_m = 175
  // BMI = 体重(kg) / (身高(m) / 100)²
  let height_cm = height_m
  let height_squared = @azimuth.multiply(height_cm, height_cm)
  let bmi = @azimuth.multiply(weight_kg, 10000) / height_squared
  @azimuth.assert_eq(22, bmi)
}