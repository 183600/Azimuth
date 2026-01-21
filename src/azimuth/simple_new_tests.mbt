// 简单的新测试用例
// 测试 azimuth 包的基本功能

test "add_simple_test" {
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
}

test "multiply_simple_test" {
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
}

test "divide_with_ceil_simple_test" {
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
}

test "greet_simple_test" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "combined_operations_test" {
  let result = add(multiply(2, 3), divide_with_ceil(10, 4))
  assert_eq(9, result)  // 2*3 + ceil(10/4) = 6 + 3 = 9
}