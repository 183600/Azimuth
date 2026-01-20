// Azimuth 新增综合测试用例
// 使用标准的 MoonBit 测试语法

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
}

test "add_mixed_signs" {
  // 测试混合符号加法
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))
  @azimuth.assert_eq(1, @azimuth.add(-2147483647, 2147483648))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "multiply_comprehensive" {
  // 测试乘法的综合情况
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
}

test "greet_unicode_and_spaces" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("   spaced   "))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
}

test "add_sequence_operations" {
  // 测试序列加法运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(@azimuth.add(-1, -2), @azimuth.add(-3, -4))
  @azimuth.assert_eq(-10, result2)
  
  let result3 = @azimuth.add(@azimuth.add(100, -50), @azimuth.add(25, -25))
  @azimuth.assert_eq(50, result3)
}

test "multiply_sequence_operations" {
  // 测试序列乘法运算
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result2)
  
  let result3 = @azimuth.multiply(@azimuth.multiply(10, 0), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(0, result3)
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算 (a + b) * (c - d) 的各种情况
  let a = 10; let b = 5; let c = 20; let d = 8
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)  // 使用加法实现减法
  let result = @azimuth.multiply(sum_ab, diff_cd)
  @azimuth.assert_eq(105, result)
  
  // 测试负数情况
  let a2 = -10; let b2 = 5; let c2 = -20; let d2 = 8
  let sum_ab2 = @azimuth.add(a2, b2)
  let diff_cd2 = @azimuth.add(c2, -d2)
  let result2 = @azimuth.multiply(sum_ab2, diff_cd2)
  @azimuth.assert_eq(30, result2)
}

test "edge_case_combinations" {
  // 测试边界情况的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值与1的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 混合运算
  let temp = @azimuth.add(max_val, -max_val)
  @azimuth.assert_eq(0, temp)
  let result = @azimuth.multiply(temp, max_val)
  @azimuth.assert_eq(0, result)
}