// 简单的 Azimuth 测试用例
// 包含 5 个基本测试用例

test "simple_add_test" {
  // 简单加法测试
  let result = azimuth::add(2, 3)
  azimuth::assert_eq(5, result)
}

test "simple_multiply_test" {
  // 简单乘法测试
  let result = azimuth::multiply(4, 5)
  azimuth::assert_eq(20, result)
}

test "simple_subtract_test" {
  // 简单减法测试
  let result = azimuth::subtract(10, 4)
  azimuth::assert_eq(6, result)
}

test "simple_divide_test" {
  // 简单除法测试
  let result = azimuth::divide_with_ceil(10, 3)
  azimuth::assert_eq(4, result)
}

test "simple_greet_test" {
  // 简单问候测试
  let result = azimuth::greet("MoonBit")
  azimuth::assert_eq_string("Hello, MoonBit!", result)
}