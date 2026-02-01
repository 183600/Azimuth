// Azimuth 新增测试用例 - 10个标准测试
// 使用标准 MoonBit 测试语法

test "add_function_test" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "multiply_function_test" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "subtract_function_test" {
  // 基本减法功能测试
  @azimuth.assert_eq(1, @azimuth.subtract(3, 2))
}

test "greet_function_test" {
  // 字符串处理测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "add_negative_test" {
  // 负数加法测试
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
}

test "multiply_negative_test" {
  // 负数乘法测试
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "add_zero_test" {
  // 零加法测试
  @azimuth.assert_eq(3, @azimuth.add(0, 3))
}

test "multiply_zero_test" {
  // 零乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "subtract_negative_test" {
  // 负数减法测试
  @azimuth.assert_eq(5, @azimuth.subtract(3, -2))
}

test "divide_with_ceil_test" {
  // 向上取整除法测试
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
}