test "add_zero_identity" {
  // 测试加法的零元素特性：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_zero_identity" {
  // 测试乘法的零元素特性：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_one_identity" {
  // 测试乘法的单位元素特性：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
}

test "multiply_negative_one" {
  // 测试乘以-1：a * -1 = -a
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(17, @azimuth.multiply(-17, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "add_commutative" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
}

test "multiply_commutative" {
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-5, 8), @azimuth.multiply(8, -5))
  @azimuth.assert_eq(@azimuth.multiply(-4, -9), @azimuth.multiply(-9, -4))
}

test "greet_empty_string" {
  // 测试空字符串的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val/2, max_val/2 + 1))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}