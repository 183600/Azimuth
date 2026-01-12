test "lib_add_comprehensive" {
  // 基础功能测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  
  // 零值测试
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

///|
test "lib_multiply_comprehensive" {
  // 基础功能测试
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-21, @azimuth.multiply(7, -3))
  @azimuth.assert_eq(21, @azimuth.multiply(-7, -3))
  
  // 特殊值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}

///|
test "lib_greet_comprehensive" {
  // 基础功能测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test_case!", @azimuth.greet("test_case"))
}


