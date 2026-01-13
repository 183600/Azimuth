// 扩展测试用例 - 测试边界情况和特殊场景

// 测试 add 函数的边界情况
test "add_with_zero" {
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_positive_overflow" {
  // 测试正数相加溢出情况
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
}

test "add_negative_overflow" {
  // 测试负数相加溢出情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1500000000))
}

test "add_with_min_value" {
  // 测试包含最小值的加法
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -5))
  @azimuth.assert_eq(10, @azimuth.add(min_val, 10))
}

// 测试 multiply 函数的边界情况
test "multiply_with_zero_and_one" {
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
}

test "multiply_with_negative_one" {
  // 测试乘以-1的情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况：最小值乘以-1会溢出
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
}

test "multiply_overflow" {
  // 测试乘法溢出
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val)的平方会溢出
}

// 测试 greet 函数
test "greet_with_empty_string" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_with_special_characters" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}