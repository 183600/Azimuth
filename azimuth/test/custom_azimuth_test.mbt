// 自定义 Azimuth 测试

test "custom_add_test" {
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
}

test "custom_multiply_test" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "custom_greet_test" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}