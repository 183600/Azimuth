// 简单的 MoonBit 测试套件
// 包含 5 个基本测试用例

test "add_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_basic" {
  assert_eq(2, subtract(5, 3))
}