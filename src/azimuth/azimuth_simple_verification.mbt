// 简单测试文件 - 验证测试系统
test "simple_verification_test" {
  assert_eq(5, add(2, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "azimuth_custom_test_one" {
  let a = 10
  let b = 20
  assert_eq(30, add(a, b))
  assert_eq(200, multiply(a, b))
  assert_eq(1, divide_with_ceil(a, b))
}

test "azimuth_custom_test_two" {
  assert_eq(0, multiply(5, 0))
  assert_eq(10, subtract(15, 5))
  assert_eq(4, divide_with_ceil(13, 4))
}