// 简洁的 MoonBit 测试用例
// 针对 azimuth 库的核心功能

test "add_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_calculation" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}