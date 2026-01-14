// 简单的标准 MoonBit 测试
// 测试基本功能

test "simple_addition" {
  // 测试基本加法
  let result = 2 + 3
  assert_eq(5, result)
}

test "simple_multiplication" {
  // 测试基本乘法
  let result = 2 * 3
  assert_eq(6, result)
}

test "simple_string" {
  // 测试字符串拼接
  let result = "Hello" + ", " + "World"
  assert_eq_string("Hello, World", result)
}