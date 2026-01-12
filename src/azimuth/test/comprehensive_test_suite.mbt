test "comprehensive_add_various_cases" {
  // 正数 + 正数
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  // 负数 + 负数
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  // 正数 + 负数
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  @azimuth.assert_eq(30, @azimuth.add(80, -50))
}

///|
test "comprehensive_multiply_various_cases" {
  // 正数 * 正数
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  // 负数 * 负数
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  // 正数 * 负数
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(-10, 10))
}

///|
test "comprehensive_greet_various_inputs" {
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, aBcDeF!", @azimuth.greet("aBcDeF"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

///|
test "comprehensive_mixed_operations" {
  let a = 10
  let b = 20
  let c = -5
  
  @azimuth.assert_eq(30, @azimuth.add(a, b))
  @azimuth.assert_eq(5, @azimuth.add(a, c))
  @azimuth.assert_eq(200, @azimuth.multiply(a, b))
  @azimuth.assert_eq(-50, @azimuth.multiply(a, c))
  
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}


