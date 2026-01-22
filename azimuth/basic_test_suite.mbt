// 基本 MoonBit 测试套件
// 包含5个简单测试用例

test "basic_addition" {
  assert_eq(3, add(1, 2))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greeting" {
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "basic_division" {
  assert_eq(2, divide_with_ceil(5, 3))
}

test "zero_operations" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
}