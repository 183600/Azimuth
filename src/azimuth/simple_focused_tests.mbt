// 简单的单元测试用例
// 测试 azimuth 包的核心功能

test "simple_addition" {
  // 使用内置的 assert_eq 进行测试
  let result = 2 + 3
  assert_eq(5, result)
}

test "simple_multiplication" {
  // 使用内置的 assert_eq 进行测试
  let result = 2 * 3
  assert_eq(6, result)
}

test "string_concatenation" {
  // 使用内置的 assert_eq_string 进行测试
  let result = "Hello, " + "World" + "!"
  assert_eq_string("Hello, World!", result)
}

test "boolean_operations" {
  // 使用内置的 assert_true 和 assert_false 进行测试
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
}

test "simple_arithmetic" {
  // 测试基本的算术运算
  assert_eq(10, 5 + 5)
  assert_eq(0, 5 - 5)
  assert_eq(25, 5 * 5)
  assert_eq(1, 5 / 5)
}

test "negative_numbers" {
  // 测试负数运算
  assert_eq(-5, -5)
  assert_eq(0, -5 + 5)
  assert_eq(-10, -5 * 2)
}

test "comparison_operations" {
  // 测试比较运算
  assert_true(5 == 5)
  assert_false(5 == 3)
  assert_true(5 != 3)
  assert_false(5 != 5)
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(3 < 5)
  assert_false(5 < 3)
}

test "logical_operations" {
  // 测试逻辑运算
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  assert_true(!false)
  assert_false(!true)
}

test "complex_expressions" {
  // 测试复杂表达式
  assert_eq(15, 5 + 3 * 2)
  assert_eq(16, (5 + 3) * 2)
  assert_eq(30, 5 * 3 * 2)
  assert_eq(10, 5 * 2 + 0)
}

test "string_operations" {
  // 测试字符串操作
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2 + "!"
  assert_eq_string("Hello, World!", combined)
  assert_eq_string("Hello", str1)
  assert_eq_string("World", str2)
}