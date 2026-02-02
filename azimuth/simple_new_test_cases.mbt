// 简单的新增测试用例 - 包含5个基本测试

test "basic_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "basic_divide_with_ceil_test" {
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "basic_subtract_test" {
  assert_eq(-1, subtract(2, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
}