// 简单的 MoonBit 测试用例
// 包含5个基本测试用例

test "add_simple_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_simple_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "greet_simple_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_simple_test" {
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_test" {
  let a = add(2, 3)
  let b = multiply(a, 2)
  assert_eq(10, b)
}