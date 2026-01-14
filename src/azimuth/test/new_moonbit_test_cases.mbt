// 新增的 MoonBit 测试用例
// 包含10个测试用例，覆盖各种场景

test "add_large_positive_numbers" {
  // 测试大正数相加
  let result = @azimuth.add(1000000, 2000000)
  @azimuth.assert_eq(3000000, result)
}

test "add_large_negative_numbers" {
  // 测试大负数相加
  let result = @azimuth.add(-1000000, -2000000)
  @azimuth.assert_eq(-3000000, result)
}

test "add_mixed_large_numbers" {
  // 测试大数混合符号相加
  let result = @azimuth.add(5000000, -3000000)
  @azimuth.assert_eq(2000000, result)
}

test "multiply_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

test "multiply_with_large_numbers" {
  // 测试大数相乘
  let result = @azimuth.multiply(50000, 20000)
  @azimuth.assert_eq(1000000000, result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  let result = @azimuth.greet("John@123")
  @azimuth.assert_eq_string("Hello, John@123!", result)
}

test "greet_with_unicode_characters" {
  // 测试包含Unicode字符的问候
  let result = @azimuth.greet("张三")
  @azimuth.assert_eq_string("Hello, 张三!", result)
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let step1 = @azimuth.add(10, 20)
  let step2 = @azimuth.multiply(step1, 3)
  let step3 = @azimuth.add(step2, 15)
  @azimuth.assert_eq(105, step3)
}

test "assert_true_function" {
  // 测试断言函数 assert_true
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(10 == 10)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
}

test "assert_false_function" {
  // 测试断言函数 assert_false
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(10 != 10)
  @azimuth.assert_false(@azimuth.add(2, 3) != 5)
}