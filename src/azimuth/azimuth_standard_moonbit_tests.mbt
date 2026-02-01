// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "simple_addition" {
  // 测试简单加法
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  // 测试简单乘法
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  // 测试简单问候
  assert_eq_string("Hello, World!", greet("World"))
}