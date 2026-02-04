// 简单的测试文件，用于验证测试系统是否工作

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(5, 5))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(15, multiply(3, 5))
}