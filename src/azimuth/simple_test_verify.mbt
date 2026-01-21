// 简单的测试验证文件
test "simple_addition_test" {
  let result = add(2, 3)
  assert_eq(5, result)
}

test "simple_multiplication_test" {
  let result = multiply(4, 5)
  assert_eq(20, result)
}

test "simple_greet_test" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}