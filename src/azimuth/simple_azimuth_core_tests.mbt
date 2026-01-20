// 简单的 Azimuth 测试用例

test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}