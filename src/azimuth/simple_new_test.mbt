// 简单的新测试文件
test "simple_new_test_1" {
  assert_eq(5, add(2, 3))
}

test "simple_new_test_2" {
  assert_eq(6, multiply(2, 3))
}

test "simple_new_test_3" {
  assert_eq_string("Hello, World!", greet("World"))
}