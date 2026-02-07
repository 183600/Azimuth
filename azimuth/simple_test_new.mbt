// 简单的 MoonBit 测试用例 - Azimuth 项目
// 包含 3 个基本测试用例

test "simple_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
}

test "simple_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}