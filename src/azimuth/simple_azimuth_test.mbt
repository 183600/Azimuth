// 简单的 Azimuth 测试用例
// 测试基本功能

test "simple_addition_test" {
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
}

test "simple_multiplication_test" {
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(0, 5), 0)
}

test "simple_division_test" {
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "simple_subtraction_test" {
  assert_eq(subtract(5, 3), 2)
  assert_eq(subtract(3, 5), -2)
  assert_eq(subtract(0, 0), 0)
}

test "simple_greet_test" {
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
}