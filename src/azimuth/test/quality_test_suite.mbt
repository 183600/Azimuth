// 高质量的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_tests" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
}

test "add_function_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(Int.max_value(), @azimuth.add(Int.max_value(), 0))
  @azimuth.assert_eq(Int.min_value(), @azimuth.add(Int.min_value(), 0))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
}

test "multiply_function_basic_tests" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(Int.max_value(), @azimuth.multiply(1, Int.max_value()))
  @azimuth.assert_eq(0, @azimuth.multiply(0, Int.min_value()))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "greet_function_basic_tests" {
  // 基本问候语测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
}

test "divide_with_ceil_basic_tests" {
  // 基本向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "divide_with_ceil_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "assert_functions_tests" {
  // 断言函数测试
  // 这些测试验证断言函数不会崩溃
  @azimuth.assert_eq(5, 5)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-5, -5)
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("中文", "中文")
}