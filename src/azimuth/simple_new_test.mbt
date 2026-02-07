// 简单的新测试用例
// 测试基本算术运算

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(5, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_subtraction_test" {
  assert_eq(2, subtract(5, 3))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(-5, subtract(0, 5))
}

test "simple_division_test" {
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_greeting_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}