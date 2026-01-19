// 简单的增强测试用例
// 使用标准的 MoonBit 测试语法

test "simple_add_test" {
  // 简单的加法测试
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
}

test "simple_multiply_test" {
  // 简单的乘法测试
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "simple_greet_test" {
  // 简单的问候测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "simple_assert_test" {
  // 简单的断言测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}