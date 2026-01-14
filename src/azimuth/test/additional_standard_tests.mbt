// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能
test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-50, 50))
}
test "add_large_positive_numbers" {
  // 测试大正数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
}
test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(-40, @azimuth.multiply(8, -5))
}
test "multiply_by_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(81000000, @azimuth.multiply(9000, 9000))
}
test "greet_with_various_names" {
  // 测试不同类型的名称
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie123!", @azimuth.greet("Charlie123"))
}
test "greet_with_special_characters" {
  // 测试包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, John-Doe!", @azimuth.greet("John-Doe"))
  @azimuth.assert_eq_string("Hello, O'Connor!", @azimuth.greet("O'Connor"))
  @azimuth.assert_eq_string("Hello, Jean-Luc!", @azimuth.greet("Jean-Luc"))
}
test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (5 + 3) * (10 - 2) = 8 * 8 = 64
  let sum = @azimuth.add(5, 3)
  let difference = @azimuth.add(10, -2)
  let result = @azimuth.multiply(sum, difference)
  @azimuth.assert_eq(64, result)
}
test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的计算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}