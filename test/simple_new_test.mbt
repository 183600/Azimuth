// 简单的新测试文件
test "simple_add" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, Test!", greet("Test"))
}