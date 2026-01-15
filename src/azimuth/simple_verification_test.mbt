// 简单的测试文件，用于验证测试系统
// 测试 azimuth 模块的基本功能

test "simple_addition_test" {
  // 测试简单的加法运算
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}

test "simple_multiplication_test" {
  // 测试简单的乘法运算
  let result = @azimuth.multiply(4, 6)
  @azimuth.assert_eq(24, result)
}

test "simple_greet_test" {
  // 测试简单的字符串拼接
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "simple_assertions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}