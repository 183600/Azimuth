// 简单的测试文件，用于验证 MoonBit 测试语法
use azimuth

test "simple_add_test" {
  let result = azimuth.add(2, 3)
  @assertion.assert_eq(5, result)?
}

test "simple_multiply_test" {
  let result = azimuth.multiply(4, 5)
  @assertion.assert_eq(20, result)?
}

test "simple_greet_test" {
  let result = azimuth.greet("Test")
  @assertion.assert_eq_string("Hello, Test!", result)?
}