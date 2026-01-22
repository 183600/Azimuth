// 标准 MoonBit 测试套件
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(4, @azimuth.multiply(-2, -2))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "edge_case_zero_operations" {
  // 测试零值运算的边界情况
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-1, @azimuth.add(3, -4))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "large_number_operations" {
  // 测试大数运算
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(334, @azimuth.divide_with_ceil(1000, 3))
}

test "complex_combined_operations" {
  // 测试复合运算
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(16, result)
}