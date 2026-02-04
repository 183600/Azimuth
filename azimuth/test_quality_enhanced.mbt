// 简单测试验证文件

test "simple_verification_test" {
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(2, subtract(5, 3))
  assert_true(5 > 3)
  assert_false(3 > 5)
}