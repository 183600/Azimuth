// 简单的新 MoonBit 测试用例
// 测试 azimuth 包的核心功能
test "simple_add_test" {
  // 简单的加法测试
  @azimuth.@azimuth.assert_eq(5, @azimuth.@azimuth.add(2, 3))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.add(-2, 2))
}
test "simple_multiply_test" {
  // 简单的乘法测试
  @azimuth.@azimuth.assert_eq(6, @azimuth.@azimuth.multiply(2, 3))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.multiply(5, 0))
}
test "simple_greet_test" {
  // 简单的问候测试
  @azimuth.@azimuth.assert_eq_string("Hello, World!", @azimuth.@azimuth.greet("World"))
  @azimuth.@azimuth.assert_eq_string("Hello, !", @azimuth.@azimuth.greet(""))
}
test "boundary_test" {
  // 边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.@azimuth.assert_eq(max_val, @azimuth.@azimuth.add(max_val, 1))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.add(min_val, -1))
}
test "assert_functions_test" {
  // 断言函数测试
  @azimuth.@azimuth.assert_true(true)
  @azimuth.@azimuth.assert_false(false)
  @azimuth.@azimuth.assert_eq(42, 42)
  @azimuth.@azimuth.assert_eq_string("test", "test")
}