// Azimuth 新增简单测试用例 - 5个基本测试
// 使用标准的 MoonBit 测试语法

test "add_basic_test" {
  // 基本加法测试
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
}

test "multiply_basic_test" {
  // 基本乘法测试
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
}

test "greet_basic_test" {
  // 基本问候测试
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "divide_with_ceil_basic_test" {
  // 基本向上取整除法测试
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
}

test "subtract_basic_test" {
  // 基本减法测试
  azimuth::assert_eq(2, azimuth::subtract(5, 3))
  azimuth::assert_eq(0, azimuth::subtract(5, 5))
  azimuth::assert_eq(-2, azimuth::subtract(3, 5))
}