// 额外的 MoonBit 测试用例
// 专注于边界条件和特殊情况

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值减1
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  
  // 最小值加1
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 最大值除以2
  @azimuth.assert_eq(1073741823, @azimuth.multiply(max_val, 0))
}

test "complex_string_combinations" {
  // 测试复杂字符串组合
  @azimuth.assert_eq_string("Hello, 🌟✨!", @azimuth.greet("🌟✨"))
  @azimuth.assert_eq_string("Hello, Line1!", @azimuth.greet("Line1"))
  @azimuth.assert_eq_string("Hello, TabSeparated!", @azimuth.greet("TabSeparated"))
}

test "mathematical_series" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 等比数列求和：2 + 4 + 8 = 14
  let sum2 = @azimuth.add(@azimuth.add(2, 4), 8)
  @azimuth.assert_eq(14, sum2)
}

test "negative_zero_handling" {
  // 测试负零处理（在整数中-0和0是相同的）
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(-0, 5))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  // 使用较小的数以避免溢出
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "sequential_function_calls" {
  // 测试连续函数调用
  // 计算 ((5 + 3) * 2) - 4
  let step1 = @azimuth.add(5, 3)  // 8
  let step2 = @azimuth.multiply(step1, 2)  // 16
  let result = @azimuth.add(step2, -4)  // 12
  
  @azimuth.assert_eq(12, result)
}

test "assert_function_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_true(@azimuth.add(1, 1) == 2)
  @azimuth.assert_false(@azimuth.add(1, 1) == 3)
  @azimuth.assert_eq(@azimuth.multiply(3, 7), 21)
  @azimuth.assert_eq_string(@azimuth.greet("Test"), "Hello, Test!")
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 1000
  let discount_rate = 20  // 20%
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate))
  @azimuth.assert_eq(80000, discounted_price)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出保护正常工作
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}