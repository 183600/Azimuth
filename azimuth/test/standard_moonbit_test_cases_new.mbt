// 标准 MoonBit 测试用例 - 新增10个高质量测试用例

test "simple_add_test" {
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}

test "simple_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}

test "simple_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "simple_divide_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "simple_subtract_test" {
  let result = @azimuth.subtract(5, 3)
  @azimuth.assert_eq(2, result)
}