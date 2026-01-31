// 简单的标准 MoonBit 测试用例
// 包含 5 个基本测试用例

test "simple_add_test" {
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-1, azimuth::add(-2, 1))
}

test "simple_multiply_test" {
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-4, azimuth::multiply(-2, 2))
}

test "simple_greet_test" {
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "simple_divide_ceil_test" {
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
}

test "simple_subtract_test" {
  azimuth::assert_eq(2, azimuth::subtract(5, 3))
  azimuth::assert_eq(-3, azimuth::subtract(2, 5))
  azimuth::assert_eq(0, azimuth::subtract(4, 4))
}