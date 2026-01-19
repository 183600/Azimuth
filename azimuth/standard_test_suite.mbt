// Azimuth 标准测试套件
// 使用标准的 MoonBit 测试语法

test "add_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(3, @azimuth.add(5, -2))
  @azimuth.assert_eq(-10, @azimuth.add(-15, 5))
}

test "add_overflow_cases" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_cases" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_special_values" {
  // 测试特殊值乘法
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_overflow_cases" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "assertion_helpers" {
  // 测试断言辅助函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}