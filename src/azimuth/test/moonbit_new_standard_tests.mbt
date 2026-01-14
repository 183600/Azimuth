// 新的标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能
test "standard_add_functionality" {
  // 测试基本的加法功能
  @azimuth.@azimuth.assert_eq(10, @azimuth.@azimuth.add(3, 7))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.add(-5, 5))
  @azimuth.@azimuth.assert_eq(-10, @azimuth.@azimuth.add(-3, -7))
}
test "standard_add_overflow" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.@azimuth.assert_eq(max_val, @azimuth.@azimuth.add(max_val, 1))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.add(min_val, -1))
}
test "standard_multiply_functionality" {
  // 测试基本的乘法功能
  @azimuth.@azimuth.assert_eq(15, @azimuth.@azimuth.multiply(3, 5))
  @azimuth.@azimuth.assert_eq(-15, @azimuth.@azimuth.multiply(3, -5))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.multiply(0, 123))
}
test "standard_multiply_overflow" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.@azimuth.assert_eq(max_val, @azimuth.@azimuth.multiply(46341, 46341))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(-46341, 46341))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, -1))
}
test "standard_greet_functionality" {
  // 测试问候功能
  @azimuth.@azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.@azimuth.greet("MoonBit"))
  @azimuth.@azimuth.assert_eq_string("Hello, !", @azimuth.@azimuth.greet(""))
  @azimuth.@azimuth.assert_eq_string("Hello, 123!", @azimuth.@azimuth.greet("123"))
}