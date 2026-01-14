// 简单的测试文件，用于验证测试系统
// 使用azimuth模块的函数进行测试

test "simple_addition_test" {
  // 简单的加法测试
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
}

test "simple_multiplication_test" {
  // 简单的乘法测试
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(0, 5))
  assert_eq(-6, azimuth::multiply(-2, 3))
}

test "simple_greet_test" {
  // 简单的问候测试
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}