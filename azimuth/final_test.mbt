// 最终测试用例 - 确保能运行的简单测试

test "final_addition" {
  assert_eq(5, add(2, 3))
}

test "final_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "final_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "final_division" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "final_subtraction" {
  assert_eq(2, subtract(5, 3))
}