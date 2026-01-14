// 简单的新测试用例
test "simple_addition_test" {
  @azimuth.@azimuth.assert_eq(5, @azimuth.@azimuth.add(2, 3))
}
test "simple_multiplication_test" {
  @azimuth.@azimuth.assert_eq(6, @azimuth.@azimuth.multiply(2, 3))
}
test "simple_greeting_test" {
  @azimuth.@azimuth.assert_eq_string("Hello, World!", @azimuth.@azimuth.greet("World"))
}
// 新增的高级测试用例
test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(15, 27), @azimuth.@azimuth.add(27, 15))
  
  // 测试负数的交换律
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-10, 20), @azimuth.@azimuth.add(20, -10))
}
test "multiplication_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let left_side = @azimuth.@azimuth.multiply(5, @azimuth.@azimuth.add(3, 7))
  let right_side = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(5, 3), @azimuth.@azimuth.multiply(5, 7))
  @azimuth.@azimuth.assert_eq(left_side, right_side)
}
test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.@azimuth.assert_eq(-1, @azimuth.@azimuth.add(max_val, min_val))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, -1))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, 1))
}
test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.@azimuth.greet("@#$%^&*()"))
  @azimuth.@azimuth.assert_eq_string("Hello,   !", @azimuth.@azimuth.greet("  "))
  @azimuth.@azimuth.assert_eq_string("Hello, \n\t!", @azimuth.@azimuth.greet("\n\t"))
}
test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.@azimuth.multiply(10000, @azimuth.@azimuth.multiply(5, 3))
  let total = @azimuth.@azimuth.add(10000, interest)
  
  // 验证：10000 + 10000×5×3 = 10000 + 150000 = 160000
  @azimuth.@azimuth.assert_eq(160000, total)
}