// 用户添加的 MoonBit 测试用例
test "user_add_basic" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "user_subtract_basic" {
  let result = @azimuth.subtract(10, 3)
  @azimuth.assert_eq(7, result)
}

test "user_multiply_basic" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "user_greet_basic" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "user_divide_with_ceil_basic" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}