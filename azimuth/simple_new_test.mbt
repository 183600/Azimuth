// 简单的新测试文件
// 测试系统是否正常工作

test "simple_addition_test" {
  // 简单的加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "simple_greeting_test" {
  // 简单的问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}