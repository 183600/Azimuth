// Azimuth 增强单元测试
// 测试 azimuth 包中的核心功能

test "add_function_zero_cases" {
  // 测试 add 函数的零值情况
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(0, -10))
  @azimuth.assert_eq(-10, @azimuth.add(-10, 0))
}

test "add_function_positive_numbers" {
  // 测试 add 函数的正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1000, @azimuth.add(600, 400))
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
}

test "add_function_negative_numbers" {
  // 测试 add 函数的负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  @azimuth.assert_eq(-15, @azimuth.add(-10, -5))
}

test "add_function_mixed_numbers" {
  // 测试 add 函数的正负数混合相加
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-100, @azimuth.add(100, -200))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "multiply_function_positive_numbers" {
  // 测试 multiply 函数的正数相乘
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(72, @azimuth.multiply(8, 9))
}

test "multiply_function_negative_numbers" {
  // 测试 multiply 函数的负数相乘
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "greet_function_various_names" {
  // 测试 greet 函数的各种名称
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function" {
  // 测试 divide_with_ceil 函数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}