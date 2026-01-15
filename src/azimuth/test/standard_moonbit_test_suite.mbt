// 标准 MoonBit 测试套件
// 包含基本的测试用例，验证核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(-2, -3), -5)
}

test "add_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(@azimuth.add(10, 0), 10)
  @azimuth.assert_eq(@azimuth.add(0, 10), 10)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(@azimuth.add(5, -5), 0)
  @azimuth.assert_eq(@azimuth.add(-3, -5), -8)
  @azimuth.assert_eq(@azimuth.add(3, -5), -2)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(1, 1), 1)
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(@azimuth.multiply(2, -3), -6)
  @azimuth.assert_eq(@azimuth.multiply(-2, -3), 6)
  @azimuth.assert_eq(@azimuth.multiply(-5, 0), 0)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
  @azimuth.assert_eq_string(@azimuth.greet("User123"), "Hello, User123!")
  @azimuth.assert_eq_string(@azimuth.greet("@#$"), "Hello, @#$!")
}

test "boundary_values_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
}

test "boundary_values_multiply" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
}