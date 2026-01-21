// 简单的 MoonBit 测试用例
// 测试基本功能

test "simple_addition_test" {
  let result = 2 + 3
  assert_eq(5, result)
}

test "simple_multiplication_test" {
  let result = 4 * 5
  assert_eq(20, result)
}

test "simple_string_test" {
  let greeting = "Hello, MoonBit!"
  assert_eq_string("Hello, MoonBit!", greeting)
}

test "simple_boolean_test" {
  assert_true(true)
  assert_false(false)
}