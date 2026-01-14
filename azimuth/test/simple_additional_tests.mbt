// 简单的 MoonBit 测试用例
// 测试 azimuth 库的基本功能

test "simple_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}