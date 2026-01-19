// 用户标准测试用例 - 使用标准的 MoonBit 测试语法
// 包含不超过10个测试用例，覆盖项目的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
}

test "add_function_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(@azimuth.add(5, 0), 5)
  @azimuth.assert_eq(@azimuth.add(0, 5), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(@azimuth.multiply(4, 5), 20)
}

test "multiply_function_with_zero" {
  // 测试与0相乘的情况
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
}

test "greet_function_empty_string" {
  // 测试空字符串的问候
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

test "add_function_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(@azimuth.add(-5, -3), -8)
  @azimuth.assert_eq(@azimuth.add(10, -3), 7)
  @azimuth.assert_eq(@azimuth.add(-10, 3), -7)
}

test "multiply_function_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(@azimuth.multiply(-4, 5), -20)
  @azimuth.assert_eq(@azimuth.multiply(4, -5), -20)
  @azimuth.assert_eq(@azimuth.multiply(-4, -5), 20)
}

test "boundary_values_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值处理正确
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
}

test "complex_calculation" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  
  // 计算矩形周长：2 * length + 2 * width
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(perimeter, 30)
}