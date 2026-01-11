// 简单验证测试

///|
test "simple_add_verify" {
  assert_eq(3, add(1, 2))
}

///|
test "simple_multiply_verify" {
  assert_eq(6, multiply(2, 3))
}

///|
test "simple_greet_verify" {
  assert_eq("Hello, World!", greet("World"))
}