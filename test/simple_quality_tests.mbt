// 简单质量测试用例

test "add_identity" {
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
}

test "multiply_identity" {
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(0, multiply(5, 0))
}

test "divide_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_simple" {
  assert_eq_string("Hello, World!", greet("World"))
}