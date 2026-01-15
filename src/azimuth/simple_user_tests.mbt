test "simple_user_test" {
  // 简单的用户测试
  let result = azimuth::add(2, 3)
  assert_eq(5, result)
}

test "simple_user_test2" {
  // 简单的用户测试2
  let result = azimuth::multiply(4, 5)
  assert_eq(20, result)
}

test "simple_user_test3" {
  // 简单的用户测试3
  let result = azimuth::greet("User")
  assert_eq_string("Hello, User!", result)
}