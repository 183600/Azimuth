// 导入需要的函数
// 注意：在MoonBit中，测试文件通常不需要显式导入同包的函数

test "our_test_add_function_works" {
  assert_eq(add(2, 3), 5)
}

test "our_test_multiply_function_works" {
  assert_eq(multiply(4, 5), 20)
}

test "our_test_greet_function_works" {
  assert_eq_string(greet("World"), "Hello, World!")
}

test "our_test_divide_with_ceil_function_works" {
  assert_eq(divide_with_ceil(10, 3), 4)
}