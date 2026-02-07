// 简单的 Azimuth 测试用例
// 包含5个基本的标准 MoonBit 测试用例

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(4, 6))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(12, multiply(3, 4))
}

test "basic_subtraction" {
  assert_eq(3, subtract(5, 2))
  assert_eq(1, subtract(7, 6))
}

test "basic_division_ceil" {
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
}

test "greet_function" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}