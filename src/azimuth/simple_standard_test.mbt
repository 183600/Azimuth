// 简单的标准 MoonBit 测试用例

test "simple_add" {
  assert_eq(5, add(2, 3))
}

test "simple_multiply" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_with_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
}