// 简单的增强测试用例

test "simple_add_test" {
  // 测试基本加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiply_test" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greet_test" {
  // 测试基本问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}