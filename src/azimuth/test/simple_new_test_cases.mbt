// 简单的 MoonBit 测试用例
// 测试基本功能
test "simple_addition" {
  @azimuth.@azimuth.assert_eq(5, @azimuth.@azimuth.add(2, 3))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.add(-2, 2))
}
test "simple_multiplication" {
  @azimuth.@azimuth.assert_eq(6, @azimuth.@azimuth.multiply(2, 3))
  @azimuth.@azimuth.assert_eq(-6, @azimuth.@azimuth.multiply(-2, 3))
}
test "simple_greeting" {
  @azimuth.@azimuth.assert_eq_string("Hello, World!", @azimuth.@azimuth.greet("World"))
  @azimuth.@azimuth.assert_eq_string("Hello, !", @azimuth.@azimuth.greet(""))
}