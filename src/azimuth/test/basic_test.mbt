// 基础测试用例
///|
test "basic_add_positive" {
  let result = @azimuth.add(5, 10)
  @azimuth.assert_eq(15, result)
}

///|
test "basic_add_negative" {
  let result = @azimuth.add(-5, -10)
  @azimuth.assert_eq(-15, result)
}

///|
test "basic_multiply_positive" {
  let result = @azimuth.multiply(3, 7)
  @azimuth.assert_eq(21, result)
}

///|
test "basic_greet" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}


