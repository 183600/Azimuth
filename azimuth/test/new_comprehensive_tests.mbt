test "add_function_edge_cases" {
  // 测试边界值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
}

test "multiply_function_edge_cases" {
  // 测试边界值乘法
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(1, -2147483648))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(4, -3))
}

test "greet_function_special_characters" {
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, 😊!", @azimuth.greet("😊"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "add_function_large_numbers" {
  // 测试大数运算
  @azimuth.assert_eq(2147483647, @azimuth.add(1073741823, 1073741824))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46340, 46341))
}

test "divide_with_ceil_exact_division" {
  // 测试整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-10, 2))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(10, -2))
}

test "mixed_operations" {
  // 测试混合运算
  let a = @azimuth.add(5, 3)
  let b = @azimuth.multiply(a, 2)
  @azimuth.assert_eq(16, b)
  @azimuth.assert_eq_string("Hello, 16!", @azimuth.greet("16"))
}

test "add_function_symmetry" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(10, 20), @azimuth.add(20, 10))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  @azimuth.assert_eq(@azimuth.add(-10, -20), @azimuth.add(-20, -10))
}

test "multiply_function_symmetry" {
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(10, 20), @azimuth.multiply(20, 10))
  @azimuth.assert_eq(@azimuth.multiply(-10, 20), @azimuth.multiply(20, -10))
  @azimuth.assert_eq(@azimuth.multiply(-10, -20), @azimuth.multiply(-20, -10))
}