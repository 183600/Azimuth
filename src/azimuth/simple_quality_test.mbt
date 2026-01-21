// 简单的测试套件 - 验证基本功能

test "simple_addition_test" {
  // 简单的加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
}

test "simple_greet_test" {
  // 简单的问候测试
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_with_ceil_test" {
  // 简单的向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}