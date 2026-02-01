// 边界情况测试 - 5个测试用例

test "azimuth_add_boundary_unique_2024" {
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(500000, 500000))
}

test "azimuth_multiply_boundary_unique_2024" {
  assert_eq(0, multiply(0, 12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "azimuth_divide_boundary_unique_2024" {
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "azimuth_greet_boundary_unique_2024" {
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, World!", greet("World"))
}

test "azimuth_subtract_boundary_unique_2024" {
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(5, 10))
}