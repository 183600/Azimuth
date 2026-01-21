// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库函数的额外测试

test "add_positive_numbers" {
  @azimuth.assert_eq(15, @azimuth.add(2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-60, -40))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_by_zero" {
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_negative_numbers" {
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "greet_with_special_characters" {
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "divide_with_ceil_exact_division" {
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
}

test "divide_with_ceil_remainder" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
}

test "divide_with_ceil_zero_divisor" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_calculation_scenario" {
  // 复合计算：计算购物车总价
  let item1_total = @azimuth.multiply(25, 3)  // 25元 × 3件
  let item2_total = @azimuth.multiply(15, 2)  // 15元 × 2件
  let subtotal = @azimuth.add(item1_total, item2_total)
  @azimuth.assert_eq(105, subtotal)  // 75 + 30 = 105
}