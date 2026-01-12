
// 导入测试辅助函数

test "test_1_basic_add_original" {
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(5, -5))
  azimuth::assert_eq(-10, azimuth::add(-7, -3))
}

test "test_1_basic_multiply_original" {
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
  azimuth::assert_eq(21, azimuth::multiply(-7, -3))
}

test "test_1_basic_greet_original" {
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, Azimuth!", azimuth::greet("Azimuth"))
}