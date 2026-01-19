// Azimuth 综合测试用例
// 使用标准的 MoonBit 测试语法

test "add_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(45, 55))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-45, -55))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  @azimuth.assert_eq(-4, @azimuth.add(-7, 3))
  @azimuth.assert_eq(50, @azimuth.add(100, -50))
  @azimuth.assert_eq(-20, @azimuth.add(-30, 10))
}

test "multiply_basic_tests" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_with_one_and_zero" {
  // 测试与1和0的乘法
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "greet_functionality" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "overflow_protection_add" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val/2, max_val/2 + 1))
}

test "overflow_protection_multiply" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
}