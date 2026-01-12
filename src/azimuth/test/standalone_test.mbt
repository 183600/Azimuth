


// 独立测试文件，使用 test_helper 中的函数定义
test "add_basic_standalone" {
  azimuth::assert_eq(5, azimuth::add(2, 3))
}

test "add_negative_standalone" {
  azimuth::assert_eq(0, azimuth::add(5, -5))
}

test "multiply_basic_standalone" {
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
}

test "multiply_negative_standalone" {
  azimuth::assert_eq(-6, azimuth::multiply(2, -3))
}

test "greet_basic_standalone" {
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
}

test "greet_empty_standalone" {
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}