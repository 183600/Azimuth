test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  let result1 = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(3, result1)
  
  let result2 = @azimuth.divide_with_ceil(11, 3)
  @azimuth.assert_eq(4, result2)
  
  let result3 = @azimuth.divide_with_ceil(12, 3)
  @azimuth.assert_eq(4, result3)
  
  let result4 = @azimuth.divide_with_ceil(6, 3)
  @azimuth.assert_eq(2, result4)
  
  let result5 = @azimuth.divide_with_ceil(0, 5)
  @azimuth.assert_eq(0, result5)
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法的负数情况
  let result1 = @azimuth.divide_with_ceil(-10, 3)
  @azimuth.assert_eq(-3, result1)
  
  let result2 = @azimuth.divide_with_ceil(-11, 3)
  @azimuth.assert_eq(-3, result2)
  
  let result3 = @azimuth.divide_with_ceil(-12, 3)
  @azimuth.assert_eq(-4, result3)
  
  let result4 = @azimuth.divide_with_ceil(10, -3)
  @azimuth.assert_eq(3, result4)
  
  let result5 = @azimuth.divide_with_ceil(11, -3)
  @azimuth.assert_eq(4, result5)
  
  let result6 = @azimuth.divide_with_ceil(12, -3)
  @azimuth.assert_eq(4, result6)
}

test "add_zero_handling" {
  // 测试加法函数的零值处理
  let result1 = @azimuth.add(0, 5)
  @azimuth.assert_eq(5, result1)
  
  let result2 = @azimuth.add(5, 0)
  @azimuth.assert_eq(5, result2)
  
  let result3 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result3)
  
  let max_val = 2147483647
  let result4 = @azimuth.add(0, max_val)
  @azimuth.assert_eq(max_val, result4)
  
  let min_val = -2147483648
  let result5 = @azimuth.add(0, min_val)
  @azimuth.assert_eq(min_val, result5)
}

test "multiply_special_values" {
  // 测试乘法函数的特殊值处理
  let result1 = @azimuth.multiply(0, 0)
  @azimuth.assert_eq(0, result1)
  
  let max_val = 2147483647
  let result2 = @azimuth.multiply(0, max_val)
  @azimuth.assert_eq(0, result2)
  
  let min_val = -2147483648
  let result3 = @azimuth.multiply(0, min_val)
  @azimuth.assert_eq(0, result3)
  
  let result4 = @azimuth.multiply(1, 5)
  @azimuth.assert_eq(5, result4)
  
  let result5 = @azimuth.multiply(5, 1)
  @azimuth.assert_eq(5, result5)
  
  let result6 = @azimuth.multiply(-1, 5)
  @azimuth.assert_eq(-5, result6)
  
  let result7 = @azimuth.multiply(1, -5)
  @azimuth.assert_eq(-5, result7)
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  let result1 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result1)
  
  let result2 = @azimuth.greet("a")
  @azimuth.assert_eq_string("Hello, a!", result2)
  
  let result3 = @azimuth.greet("中文")
  @azimuth.assert_eq_string("Hello, 中文!", result3)
  
  let result4 = @azimuth.greet("🌙")
  @azimuth.assert_eq_string("Hello, 🌙!", result4)
  
  let result5 = @azimuth.greet("   ")
  @azimuth.assert_eq_string("Hello,   !", result5)
}

test "complex_arithmetic" {
  // 测试复合算术运算
  let a = 100
  let b = -50
  let c = 25
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(1250, product)
  
  // 计算 a * b + c
  let product2 = @azimuth.multiply(a, b)
  let sum2 = @azimuth.add(product2, c)
  @azimuth.assert_eq(-4975, sum2)
}

test "assert_with_expressions" {
  // 测试断言函数与表达式
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  
  let result1 = @azimuth.multiply(5, 5)
  @azimuth.assert_eq(25, result1)
  
  let result2 = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", result2)
  
  let result3 = @azimuth.add(2, 3)
  @azimuth.assert_true(result3 == 5)
  
  let result4 = @azimuth.multiply(2, 3)
  @azimuth.assert_false(result4 == 7)
}

test "divide_with_ceil_large_numbers" {
  // 测试向上取整除法的大数情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = @azimuth.divide_with_ceil(max_val, max_val)
  @azimuth.assert_eq(1, result1)
  
  let result2 = @azimuth.divide_with_ceil(max_val - 1, max_val)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.divide_with_ceil(min_val, min_val)
  @azimuth.assert_eq(1, result3)
  
  let result4 = @azimuth.divide_with_ceil(min_val, min_val - 1)
  @azimuth.assert_eq(2, result4)
}