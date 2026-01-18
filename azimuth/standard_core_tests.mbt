// Azimuth 项目聚焦测试用例
// 使用标准 MoonBit 测试语法

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
}

test "basic_greet" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "basic_assertions" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
}