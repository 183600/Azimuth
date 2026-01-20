// 综合单元测试 - 覆盖所有主要功能和边界情况

test "add_function_edge_cases" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 相同数相加
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(1000000, @azimuth.add(1500000, -500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值特殊处理测试
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  
  // 大数乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))  // 接近sqrt(max_val)
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-3, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "greet_function_edge_cases" {
  // 测试字符串拼接的边界情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))  // 空格测试
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, -3))
}

test "divide_with_ceil_special_cases" {
  // 测试特殊情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  @azimuth.assert_eq(-10, @azimuth.divide_with_ceil(-10, 1))
  @azimuth.assert_eq(-10, @azimuth.divide_with_ceil(10, -1))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(-10, -1))
}

test "assert_functions_comprehensive" {
  // 综合测试所有断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  @azimuth.assert_true(true)
  
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(false)
  
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}