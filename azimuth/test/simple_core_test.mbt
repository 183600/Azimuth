// 简单的 MoonBit 测试用例
// 测试 azimuth 核心功能

test "simple_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "simple_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "simple_greeting" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}