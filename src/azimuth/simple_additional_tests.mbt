// 简单的标准 MoonBit 测试用例
// 为 azimuth 项目添加的基本测试

test "simple_addition" {
  // 测试简单加法
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication" {
  // 测试简单乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "simple_greeting" {
  // 测试简单问候
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_divide_ceil" {
  // 测试简单向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "simple_subtraction" {
  // 测试简单减法
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
}