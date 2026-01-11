// 额外的测试文件

test "extra_test" {
  assert_eq(1, 1)?
}

test "add_boundary_test" {
  assert_eq(2147483647, add(2147483647, 1))?
}

test "multiply_boundary_test" {
  assert_eq(2147483647, multiply(46342, 46342))?
}