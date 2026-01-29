// 简单的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_add_function" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "basic_multiply_function" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "basic_greet_function" {
  // 测试基本字符串问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "basic_divide_with_ceil" {
  // 测试基本向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "basic_subtract_function" {
  // 测试基本减法运算
  @azimuth.assert_eq(1, @azimuth.subtract(3, 2))
  @azimuth.assert_eq(-1, @azimuth.subtract(2, 3))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}