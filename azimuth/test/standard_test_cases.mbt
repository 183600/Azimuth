// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic_positive_numbers" {
  // 测试基本正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
}

test "add_function_with_zero" {
  // 测试包含零的加法
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(8, @azimuth.add(8, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(3, @azimuth.add(5, -2))
  @azimuth.assert_eq(-3, @azimuth.add(-5, 2))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "greet_function_basic" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, -3))
}

test "edge_case_overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}