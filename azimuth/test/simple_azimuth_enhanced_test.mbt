// 简单的 Azimuth 增强测试
// 使用标准 MoonBit 测试语法编写的测试用例

test "simple_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}