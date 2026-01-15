test "add_zero_properties" {
  // 测试加法的零性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的性质
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "add_large_numbers" {
  // 测试大数相加
  let million = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(million, million))
  @azimuth.assert_eq(3000000, @azimuth.add(million, @azimuth.add(million, million)))
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
}

test "add_mixed_signs" {
  // 测试正负数混合相加
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
}

test "complex_calculation" {
  // 测试复合计算：(a+b)*(c-d)
  let a = 10
  let b = 20
  let c = 30
  let d = 15
  let sum = @azimuth.add(a, b)
  let diff = @azimuth.add(c, -d)
  let result = @azimuth.multiply(sum, diff)
  @azimuth.assert_eq(150, result) // (10+20)*(30-15) = 30*15 = 450
}

test "add_sequence" {
  // 测试连续加法：1+2+3+4+5
  let result = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, result)
}

test "greet_unicode" {
  // 测试Unicode字符
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, ñiño!", @azimuth.greet("ñiño"))
}