// 最终的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic" {
  // 基本加法测试
  assert_eq(7, add(3, 4))
}

test "multiply_basic" {
  // 基本乘法测试
  assert_eq(12, multiply(3, 4))
}

test "greet_basic" {
  // 基本问候测试
  assert_eq_string("Hello, Moon!", greet("Moon"))
}

test "add_with_zero" {
  // 与零相加测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
}

test "multiply_with_zero" {
  // 与零相乘测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
}

test "add_negative_numbers" {
  // 负数加法测试
  assert_eq(-1, add(3, -4))
  assert_eq(-7, add(-3, -4))
}

test "multiply_negative_numbers" {
  // 负数乘法测试
  assert_eq(-12, multiply(3, -4))
  assert_eq(12, multiply(-3, -4))
}

test "boolean_true" {
  // 布尔真值测试
  assert_true(3 < 4)
}

test "boolean_false" {
  // 布尔假值测试
  assert_false(3 > 4)
}

test "complex_expression" {
  // 复杂表达式测试
  // (2 + 3) * 4 = 20
  let result = multiply(add(2, 3), 4)
  assert_eq(20, result)
}