// Azimuth 增强测试用例
// 包含边界值、特殊情况和组合运算的测试

test "add_function_zero_handling" {
  // 测试加法函数的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(0, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
}

test "add_function_mixed_values" {
  // 测试加法函数的混合正负值
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(25, -75))
  @azimuth.assert_eq(75, @azimuth.add(-25, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_power_of_two" {
  // 测试乘法函数的2的幂次方
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "multiply_function_large_values" {
  // 测试乘法函数的大值处理
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "greet_function_unicode_handling" {
  // 测试问候函数的Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "greet_function_whitespace_handling" {
  // 测试问候函数的空白字符处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 	!", @azimuth.greet("	"))
  @azimuth.assert_eq_string("Hello, 
!", @azimuth.greet("
"))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列
  let a = 5
  let b = 10
  let c = -3
  
  // 计算 a * b + a * c
  let term1 = @azimuth.multiply(a, b)
  let term2 = @azimuth.multiply(a, c)
  let result = @azimuth.add(term1, term2)
  
  @azimuth.assert_eq(35, result)
  
  // 计算 (a + b) * (b + c)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, c)
  let product = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(105, product)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的加法
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试最大值与-1的乘法
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与1的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试最大值与0的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}