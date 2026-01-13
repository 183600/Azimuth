// 增强的测试用例，覆盖更多边界情况和实际应用场景

// 测试加法函数的边界情况
test "add_min_value_handling" {
  let min_val = -2147483648
  // 测试最小值与正数相加
  let result1 = @clean_test.add(min_val, 10)
  @clean_test.assert_eq(min_val + 10, result1)
  
  // 测试最小值与负数相加
  let result2 = @clean_test.add(min_val, -10)
  @clean_test.assert_eq(min_val, result2) // 应该返回最小值，防止溢出
}

// 测试乘法函数的最小值处理
test "multiply_min_value_edge_case" {
  let min_val = -2147483648
  // 测试最小值乘以-1的特殊情况
  let result1 = @clean_test.multiply(min_val, -1)
  @clean_test.assert_eq(min_val, result1) // 应该返回最小值，防止溢出
  
  // 测试最小值乘以1
  let result2 = @clean_test.multiply(min_val, 1)
  @clean_test.assert_eq(min_val, result2)
}

// 测试乘法函数的大数处理
test "multiply_large_numbers" {
  let large_num = 1000000
  let result = @clean_test.multiply(large_num, 1000)
  @clean_test.assert_eq(1000000000, result)
}

// 测试greet函数的特殊字符处理
test "greet_special_characters" {
  let result1 = @clean_test.greet("MoonBit-2026")
  @clean_test.assert_eq_string("Hello, MoonBit-2026!", result1)
  
  let result2 = @clean_test.greet("测试中文")
  @clean_test.assert_eq_string("Hello, 测试中文!", result2)
}

// 测试加法函数的混合符号运算
test "add_mixed_sign_numbers" {
  let result1 = @clean_test.add(100, -50)
  @clean_test.assert_eq(50, result1)
  
  let result2 = @clean_test.add(-100, 50)
  @clean_test.assert_eq(-50, result2)
  
  let result3 = @clean_test.add(-100, -50)
  @clean_test.assert_eq(-150, result3)
}

// 测试乘法函数的混合符号运算
test "multiply_mixed_sign_numbers" {
  let result1 = @clean_test.multiply(10, -5)
  @clean_test.assert_eq(-50, result1)
  
  let result2 = @clean_test.multiply(-10, 5)
  @clean_test.assert_eq(-50, result2)
  
  let result3 = @clean_test.multiply(-10, -5)
  @clean_test.assert_eq(50, result3)
}

// 测试greet函数的长字符串处理
test "greet_long_string" {
  let long_name = "这是一个非常长的名字用来测试字符串处理功能是否正常工作"
  let result = @clean_test.greet(long_name)
  @clean_test.assert_eq_string("Hello, " + long_name + "!", result)
}

// 测试断言函数的布尔值处理
test "assert_boolean_operations" {
  @clean_test.assert_true(true)
  @clean_test.assert_false(false)
  @clean_test.assert_true(1 == 1)
  @clean_test.assert_false(1 == 2)
  @clean_test.assert_true("hello" == "hello")
  @clean_test.assert_false("hello" == "world")
}