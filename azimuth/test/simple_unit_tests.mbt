// 简单的单元测试用例
// 使用标准的 MoonBit 测试语法

test "add_simple_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "multiply_simple_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "greet_simple_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}