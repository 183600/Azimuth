// 测试基本功能

test "add_basic" {
  assert_eq(5, add(2, 3))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}
