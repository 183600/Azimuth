// 简化的新测试用例
// 测试 azimuth 包的基本功能

test "simple_add_test" {
  // 测试基本加法
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(5, -5))
}

test "simple_multiply_test" {
  // 测试基本乘法
  azimuth::assert_eq(12, azimuth::multiply(3, 4))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
}

test "simple_greet_test" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}