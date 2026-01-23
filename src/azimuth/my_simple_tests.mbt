// 我的简单 MoonBit 测试用例
// 针对 azimuth 库的基本测试

test "simple_addition" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "simple_greet" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_with_ceil" {
  // 测试基本向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}