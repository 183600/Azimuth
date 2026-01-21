// 简单测试文件，验证我们的测试语法
test "simple_test" {
  assert_true(1 == 1)
  assert_false(1 == 2)
}

test "string_test" {
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
}

test "math_test" {
  assert_eq(5, 2 + 3)
  assert_eq(0, 5 - 5)
}
