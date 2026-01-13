// 新增的 MoonBit 测试用例
// 覆盖更多边界情况和功能测试

// 加法边界测试 - 负数溢出
test "add_negative_overflow" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 加法边界测试 - 最小值加正数
test "add_min_value_with_positive" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, result)
}

// 乘法边界测试 - 大数相乘溢出
test "multiply_large_numbers_overflow" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)
}

// 乘法边界测试 - 最小值乘以-1
test "multiply_min_value_by_negative_one" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 字符串处理测试 - 特殊字符
test "greet_with_special_characters" {
  let result = @azimuth.greet("张三")
  @azimuth.assert_eq_string("Hello, 张三!", result)
}

// 字符串处理测试 - 数字和符号
test "greet_with_numbers_and_symbols" {
  let result = @azimuth.greet("User123@#$")
  @azimuth.assert_eq_string("Hello, User123@#$!", result)
}

// 混合运算测试 - 先乘后加
test "mixed_operations_multiply_then_add" {
  let mult_result = @azimuth.multiply(10, 5)
  let final_result = @azimuth.add(mult_result, 20)
  @azimuth.assert_eq(70, final_result)
}

// 边界值组合测试 - 接近最大值
test "near_maximum_values" {
  let near_max = 2147483640
  let result1 = @azimuth.add(near_max, 5)
  let result2 = @azimuth.multiply(near_max, 1)
  @azimuth.assert_eq(2147483647, result1)
  @azimuth.assert_eq(near_max, result2)
}

// 复杂场景测试 - 多步计算
test "complex_calculation_sequence" {
  let step1 = @azimuth.add(100, 200)
  let step2 = @azimuth.multiply(step1, 3)
  let step3 = @azimuth.add(step2, -100)
  let final_result = @azimuth.multiply(step3, 2)
  @azimuth.assert_eq(1800, final_result)
}

// 错误处理测试 - 验证断言函数正常工作
test "assertion_functions_verification" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq_string("", "")
}