// 简单的增强测试用例
test "simple_enhanced_addition" {
  // 简单加法增强测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_enhanced_multiplication" {
  // 简单乘法增强测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_enhanced_greeting" {
  // 简单问候增强测试
  assert_eq_string("Hello, Enhanced!", greet("Enhanced"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_enhanced_divide_ceil" {
  // 简单向上取整除法增强测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}