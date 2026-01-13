// 综合测试用例 - 覆盖 Azimuth 库的主要功能
// 测试安全加法函数的各种边界情况
test "add_positive_numbers" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}

test "add_with_zero" {
  let result1 = @azimuth.add(0, 42)
  let result2 = @azimuth.add(42, 0)
  @azimuth.assert_eq(42, result1)
  @azimuth.assert_eq(42, result2)
}

test "add_negative_numbers" {
  let result = @azimuth.add(-10, -20)
  @azimuth.assert_eq(-30, result)
}

test "add_mixed_numbers" {
  let result = @azimuth.add(15, -5)
  @azimuth.assert_eq(10, result)
}

test "add_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

// 测试安全乘法函数
test "multiply_basic" {
  let result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result)
}

test "multiply_with_zero_and_one" {
  let result1 = @azimuth.multiply(0, 42)
  let result2 = @azimuth.multiply(42, 1)
  let result3 = @azimuth.multiply(-1, 42)
  @azimuth.assert_eq(0, result1)
  @azimuth.assert_eq(42, result2)
  @azimuth.assert_eq(-42, result3)
}

// 测试字符串拼接函数
test "greet_basic" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

// 测试断言函数
test "assert_true_function" {
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
}