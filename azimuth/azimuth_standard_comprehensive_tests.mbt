// 标准 MoonBit 测试用例
// 测试核心功能

test "add_function_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(-1, add(2, -3))
}

test "multiply_function_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_function" {
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(3, 10))
}

test "subtract_function_basic" {
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(3, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(8, subtract(5, -3))
}

test "assert_functions" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}