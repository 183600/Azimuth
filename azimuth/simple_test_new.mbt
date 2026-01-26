// 简单的新测试用例

test "basic_add" {
  assert_eq(5, add(2, 3))
}

test "basic_multiply" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greet" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "basic_divide" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "basic_subtract" {
  assert_eq(2, subtract(5, 3))
}