// 简单的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "simple_add_test" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
}

test "simple_multiply_test" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_assert_test" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
}

test "simple_calculation_test" {
  // 测试简单计算
  let a = 10
  let b = 20
  let result = add(a, b)
  assert_eq(30, result)
}