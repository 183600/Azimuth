// 简单测试文件 - 验证测试系统是否正常工作

test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}