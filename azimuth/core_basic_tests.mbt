// 核心 MoonBit 测试用例
// 测试 azimuth 包的基本功能

test "add_basic" {
  assert_eq(5, add(2, 3))
}

test "add_zero" {
  assert_eq(0, add(0, 0))
}

test "add_negative" {
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
}

test "multiply_zero" {
  assert_eq(0, multiply(0, 5))
}

test "multiply_negative" {
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_empty" {
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(11, 3))
}

test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-8, 3))
}