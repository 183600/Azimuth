// 简单测试文件

// 测试 1: 验证加法的零元素特性
///|
test "add_zero_property" {
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(-42, add(-42, 0))
  assert_eq(-42, add(0, -42))
}

// 测试 2: 验证乘法的零元素特性
///|
test "multiply_zero_property" {
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(-42, 0))
  assert_eq(0, multiply(0, -42))
}

// 测试 3: 验证乘法的单位元特性
///|
test "multiply_one_property" {
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(-42, multiply(1, -42))
}

// 测试 4: 验证字符串拼接的基本属性
///|
test "string_concatenation_properties" {
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, A!", greet("A"))
  assert_eq("Hello, 123!", greet("123"))
}