// Azimuth 额外标准测试用例
// 包含溢出处理、特殊字符和复杂运算的测试

test "add_function_positive_overflow" {
  // 测试加法函数的正数溢出处理
  let max_val = 2147483647
  
  // 两个大正数相加应该返回最大值
  @azimuth.assert_eq(max_val, @azimuth.add(1500000000, 1500000000))
  @azimuth.assert_eq(max_val, @azimuth.add(2000000000, 2000000000))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
}

test "add_function_negative_overflow" {
  // 测试加法函数的负数溢出处理
  let min_val = -2147483648
  
  // 两个大负数相加应该返回最小值
  @azimuth.assert_eq(min_val, @azimuth.add(-1500000000, -1500000000))
  @azimuth.assert_eq(min_val, @azimuth.add(-2000000000, -2000000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_overflow_handling" {
  // 测试乘法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘应该返回边界值
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-50000, 50000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-100000, 30000))
}

test "greet_function_empty_string" {
  // 测试问候函数的空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, &*()!", @azimuth.greet("&*()"))
  @azimuth.assert_eq_string("Hello, []{}!", @azimuth.greet("[]{}"))
  @azimuth.assert_eq_string("Hello, \\|/-!", @azimuth.greet("\\|/-"))
}

test "add_function_chained_operations" {
  // 测试加法函数的连续运算
  let result1 = @azimuth.add(10, 20)
  let result2 = @azimuth.add(result1, 30)
  let result3 = @azimuth.add(result2, 40)
  
  @azimuth.assert_eq(100, result3)
  
  // 测试负数的连续运算
  let neg_result1 = @azimuth.add(-10, -20)
  let neg_result2 = @azimuth.add(neg_result1, -30)
  let neg_result3 = @azimuth.add(neg_result2, 40)
  
  @azimuth.assert_eq(-20, neg_result3)
}

test "multiply_function_chained_operations" {
  // 测试乘法函数的连续运算
  let result1 = @azimuth.multiply(2, 3)
  let result2 = @azimuth.multiply(result1, 4)
  let result3 = @azimuth.multiply(result2, 5)
  
  @azimuth.assert_eq(120, result3)
  
  // 测试包含负数的连续运算
  let neg_result1 = @azimuth.multiply(-2, 3)
  let neg_result2 = @azimuth.multiply(neg_result1, -4)
  let neg_result3 = @azimuth.multiply(neg_result2, 5)
  
  @azimuth.assert_eq(120, neg_result3)
}

test "complex_mixed_arithmetic" {
  // 测试混合运算的复杂场景
  let a = 15
  let b = -5
  let c = 8
  let d = -3
  
  // 计算 (a * b) + (c * d)
  let term1 = @azimuth.multiply(a, b)
  let term2 = @azimuth.multiply(c, d)
  let result1 = @azimuth.add(term1, term2)
  
  @azimuth.assert_eq(-99, result1)
  
  // 计算 (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result2 = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(50, result2)
}

test "boundary_value_special_cases" {
  // 测试边界值的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值与2的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值与-2的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试最大值与最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值与最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试布尔表达式的断言
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_false(1 < 0)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(0 != 0)
}