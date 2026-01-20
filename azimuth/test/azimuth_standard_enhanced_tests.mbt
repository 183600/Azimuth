// Azimuth 额外标准测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多功能和边界情况

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
}

test "add_mixed_signs" {
  // 测试混合符号加法
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483647, -1))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(0, @azimuth.multiply(1000000, 0))
}

test "multiply_fractional_logic" {
  // 测试乘法的分数逻辑（虽然都是整数，但测试类似分数的概念）
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(36, @azimuth.multiply(6, 6))
  @azimuth.assert_eq(49, @azimuth.multiply(7, 7))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_unicode_and_spaces" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit 语言!", @azimuth.greet("MoonBit 语言"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, test user!", @azimuth.greet("test user"))
}

test "divide_with_ceil_negative_dividend" {
  // 测试向上取整除法的负数被除数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
}

test "divide_with_ceil_negative_divisor" {
  // 测试向上取整除法的负数除数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(10, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(5, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(11, -4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(8, -4))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let result1 = @azimuth.add(1, 1)  // 2
  let result2 = @azimuth.multiply(result1, 2)  // 4
  let result3 = @azimuth.add(result2, 1)  // 5
  let result4 = @azimuth.multiply(result3, 3)  // 15
  let result5 = @azimuth.divide_with_ceil(result4, 4)  // 4
  @azimuth.assert_eq(4, result5)
}

test "edge_case_min_max_values" {
  // 测试最小值和最大值的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加1应该溢出到最大值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 最小值减1应该溢出到最小值
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 最大值乘以1应该还是最大值
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值乘以1应该还是最小值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}