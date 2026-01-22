// 最终的 MoonBit 测试用例
// 测试 azimuth 包的核心功能
test "add_basic" {
  // 基本加法测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
}
test "multiply_basic" {
  // 基本乘法测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
}
test "greet_basic" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, Moon!", @azimuth.greet("Moon"))
}
// 新增的高级测试用例
test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  
  // 测试负数的交换律
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
}
test "multiplication_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let left_side = @azimuth.multiply(5, @azimuth.add(3, 7))
  let right_side = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(5, 7))
  @azimuth.assert_eq(left_side, right_side)
}
test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}
test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
}
test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(10000, @azimuth.multiply(5, 3))
  let total = @azimuth.add(10000, interest)
  
  // 验证：10000 + 10000×5×3 = 10000 + 150000 = 160000
  @azimuth.assert_eq(160000, total)
}
test "add_with_zero" {
  // 与零相加测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
}
test "multiply_with_zero" {
  // 与零相乘测试
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}
test "add_negative_numbers" {
  // 负数加法测试
  @azimuth.assert_eq(-1, @azimuth.add(3, -4))
  @azimuth.assert_eq(-7, @azimuth.add(-3, -4))
}
test "multiply_negative_numbers" {
  // 负数乘法测试
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}
test "boolean_true" {
  // 布尔真值测试
  @azimuth.assert_eq(true, 3 < 4)
}
test "boolean_false" {
  // 布尔假值测试
  @azimuth.assert_eq(0, 1)(3 > 4)
}
test "complex_expression" {
  // 复杂表达式测试
  // (2 + 3) * 4 = 20
  let result = @azimuth.multiply(@azimuth.add(2, 3), 4)
  @azimuth.assert_eq(20, result)
}