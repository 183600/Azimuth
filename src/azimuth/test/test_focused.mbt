// 简单的专注测试用例
test "simple_add_test" {
  assert_eq(5, azimuth::add(2, 3))
}

test "simple_multiply_test" {
  assert_eq(6, azimuth::multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", azimuth::greet("World"))
}