// 简单测试用例

test "simple_addition" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

// 测试乘法功能
///|
test "simple_multiplication" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

// 测试字符串功能
///|
test "simple_greet" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}
