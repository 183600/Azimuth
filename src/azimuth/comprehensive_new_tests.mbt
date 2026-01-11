// 新增的综合测试用例
// 包含性能、实际应用和数学性质的测试

// 测试大数运算的性能场景
///|
test "performance_large_number_calculations" {
  // 测试连续的大数加法
  let result1 = @azimuth.add(1000000, 2000000)
  let result2 = @azimuth.add(result1, 3000000)
  let result3 = @azimuth.add(result2, 4000000)
  assert_eq(10000000, result3)
  
  // 测试连续的大数乘法（在溢出范围内）
  let mult1 = @azimuth.multiply(1000, 1000)
  let mult2 = @azimuth.multiply(mult1, 1000)
  assert_eq(1000000000, mult2)
}

// 测试复利计算的实际应用
///|
test "financial_compound_interest_calculation" {
  // 本金
  let principal = 10000
  // 年利率（百分比）
  let rate_percent = 5
  // 年数
  let years = 3
  
  // 计算第一年的利息：principal * rate / 100
  let year1_interest = @azimuth.multiply(principal, rate_percent)
  let year1_total = @azimuth.add(principal, @azimuth.multiply(year1_interest, 1))
  
  // 简化计算：直接计算最终金额
  let final_amount = @azimuth.add(principal, @azimuth.multiply(years, @azimuth.multiply(principal, rate_percent)))
  assert_eq(115000, final_amount)
}

// 测试物理公式应用：自由落体距离
///|
test "physics_free_fall_distance" {
  // 重力加速度 (简化为 10 m/s²)
  let g = 10
  // 时间（秒）
  let t1 = 1
  let t2 = 2
  let t3 = 3
  
  // 距离公式：s = 0.5 * g * t²
  // 由于我们的函数只支持整数，我们简化计算
  let distance1 = @azimuth.multiply(@azimuth.multiply(g, t1), t1)
  let distance2 = @azimuth.multiply(@azimuth.multiply(g, t2), t2)
  let distance3 = @azimuth.multiply(@azimuth.multiply(g, t3), t3)
  
  assert_eq(10, distance1)   // 10 * 1 * 1 = 10
  assert_eq(40, distance2)   // 10 * 2 * 2 = 40
  assert_eq(90, distance3)   // 10 * 3 * 3 = 90
}

// 测试字符串处理的边界情况
///|
test "string_processing_edge_cases" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", @azimuth.greet("@#$%"))
  assert_eq("Hello,   !", @azimuth.greet("  "))  // 空格
  assert_eq("Hello, 中文!", @azimuth.greet("中文"))  // 中文字符
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))  // emoji
  
  // 测试长字符串
  let long_name = "VeryLongNameThatMightCauseIssuesInSomeSystems"
  let result = @azimuth.greet(long_name)
  assert_eq(true, result.contains("VeryLongNameThatMightCauseIssuesInSomeSystems"))
}

// 测试数学恒等式：平方差公式
///|
test "math_difference_of_squares" {
  // 测试 a² - b² = (a+b)(a-b)
  let a = 15
  let b = 8
  
  // 计算 a² - b²
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let difference = @azimuth.add(a_squared, -b_squared)
  
  // 计算 (a+b)(a-b)
  let sum_ab = @azimuth.add(a, b)
  let diff_ab = @azimuth.add(a, -b)
  let factored = @azimuth.multiply(sum_ab, diff_ab)
  
  assert_eq(difference, factored)
  
  // 测试另一组数值
  let x = 100
  let y = 50
  let x_squared = @azimuth.multiply(x, x)
  let y_squared = @azimuth.multiply(y, y)
  let diff2 = @azimuth.add(x_squared, -y_squared)
  
  let sum_xy = @azimuth.add(x, y)
  let diff_xy = @azimuth.add(x, -y)
  let factored2 = @azimuth.multiply(sum_xy, diff_xy)
  
  assert_eq(diff2, factored2)
}

// 测试数组索引计算（模拟）
///|
test "array_index_calculations" {
  // 模拟二维数组的索引计算
  let rows = 10
  let cols = 20
  let row = 3
  let col = 5
  
  // 一维索引 = row * cols + col
  let index = @azimuth.add(@azimuth.multiply(row, cols), col)
  assert_eq(65, index)
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  let last_index = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), cols), @azimuth.add(cols, -1))
  
  assert_eq(0, first_index)
  assert_eq(199, last_index)
}

// 测试温度转换（简化版）
///|
test "temperature_conversion_simplified" {
  // 摄氏度到华氏度的转换：F = C * 9/5 + 32
  // 由于我们只有整数运算，我们简化计算
  let celsius = 100
  
  // 简化计算：C * 2 + 30 （近似值）
  let fahrenheit_approx = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  assert_eq(230, fahrenheit_approx)
  
  // 0°C 的转换
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  assert_eq(30, freezing)
}

// 测试组合运算的优先级
///|
test "combined_operations_priority" {
  // 测试 (a + b) * c 与 a + b * c 的区别
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum_then_mult = @azimuth.multiply(@azimuth.add(a, b), c)
  
  // a + b * c
  let mult_then_add = @azimuth.add(a, @azimuth.multiply(b, c))
  
  assert_eq(16, sum_then_mult)   // (5 + 3) * 2 = 16
  assert_eq(11, mult_then_add)   // 5 + 3 * 2 = 11
}

// 测试错误恢复机制
///|
test "error_recovery_scenarios" {
  // 测试溢出后的运算是否能正确处理
  let overflow_result = @azimuth.add(2147483647, 1)
  assert_eq(2147483647, overflow_result)
  
  // 溢出后继续运算
  let after_overflow = @azimuth.add(overflow_result, 1)
  assert_eq(2147483647, after_overflow)
  
  // 测试负溢出后的运算
  let neg_overflow = @azimuth.add(-2147483648, -1)
  assert_eq(-2147483648, neg_overflow)
  
  let after_neg_overflow = @azimuth.add(neg_overflow, 1)
  assert_eq(-2147483647, after_neg_overflow)
}

// 测试边界值的组合运算
///|
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的各种组合
  assert_eq(-1, @azimuth.add(max_val, min_val))
  assert_eq(-1, @azimuth.add(min_val, max_val))
  
  // 测试最大值与自身的加法（溢出）
  assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值与自身的加法（溢出）
  assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值与0的运算
  assert_eq(max_val, @azimuth.add(max_val, 0))
  assert_eq(max_val, @azimuth.add(0, max_val))
  
  // 测试最小值与0的运算
  assert_eq(min_val, @azimuth.add(min_val, 0))
  assert_eq(min_val, @azimuth.add(0, min_val))
}