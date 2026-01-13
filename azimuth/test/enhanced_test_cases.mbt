// 增强的测试用例，覆盖更多边界情况和实际应用场景

// 测试加法函数的边界情况
test "add_min_value_handling" {
  let min_val = -2147483648
  // 测试最小值与正数相加
  let result1 = @azimuth.add(min_val, 10)
  @azimuth.assert_eq(min_val + 10, result1)
  
  // 测试最小值与负数相加
  let result2 = @azimuth.add(min_val, -10)
  @azimuth.assert_eq(min_val, result2) // 应该返回最小值，防止溢出
}

// 测试乘法函数的最小值处理
test "multiply_min_value_edge_case" {
  let min_val = -2147483648
  // 测试最小值乘以-1的特殊情况
  let result1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result1) // 应该返回最小值，防止溢出
  
  // 测试最小值乘以1
  let result2 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result2)
}

// 测试乘法函数的大数处理
test "multiply_large_numbers" {
  let large_num = 1000000
  let result = @azimuth.multiply(large_num, 1000)
  @azimuth.assert_eq(1000000000, result)
}

// 测试greet函数的特殊字符处理
test "greet_special_characters" {
  let result1 = @azimuth.greet("MoonBit-2026")
  @azimuth.assert_eq_string("Hello, MoonBit-2026!", result1)
  
  let result2 = @azimuth.greet("测试中文")
  @azimuth.assert_eq_string("Hello, 测试中文!", result2)
}

// 测试加法函数的混合符号运算
test "add_mixed_sign_numbers" {
  let result1 = @azimuth.add(100, -50)
  @azimuth.assert_eq(50, result1)
  
  let result2 = @azimuth.add(-100, 50)
  @azimuth.assert_eq(-50, result2)
  
  let result3 = @azimuth.add(-100, -50)
  @azimuth.assert_eq(-150, result3)
}

// 测试乘法函数的混合符号运算
test "multiply_mixed_sign_numbers" {
  let result1 = @azimuth.multiply(10, -5)
  @azimuth.assert_eq(-50, result1)
  
  let result2 = @azimuth.multiply(-10, 5)
  @azimuth.assert_eq(-50, result2)
  
  let result3 = @azimuth.multiply(-10, -5)
  @azimuth.assert_eq(50, result3)
}

// 测试greet函数的长字符串处理
test "greet_long_string" {
  let long_name = "这是一个非常长的名字用来测试字符串处理功能是否正常工作"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
}

// 测试断言函数的布尔值处理
test "assert_boolean_operations" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_true("hello" == "hello")
  @azimuth.assert_false("hello" == "world")
}