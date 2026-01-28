// 简单验证测试 - 测试新文件是否能被系统识别

test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}