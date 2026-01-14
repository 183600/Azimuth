// 简单的新测试用例

test "basic_addition" {
  let result = azimuth::add(2, 3)
  assert_eq(5, result)
}

test "basic_multiplication" {
  let result = azimuth::multiply(4, 5)
  assert_eq(20, result)
}

test "basic_greeting" {
  let result = azimuth::greet("World")
  assert_eq_string("Hello, World!", result)
}