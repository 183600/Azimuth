// MoonBit 测试用例集合
// 测试 azimuth 包中的核心功能和数学特性

// 测试基本算术运算
test "basic_arithmetic_operations" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

// 测试数学恒等式
test "mathematical_identities" {
  // 加法恒等式
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法恒等式
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  
  // 负数恒等式
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

// 测试边界值的特殊组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试接近边界值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

// 测试字符串处理功能
test "string_processing_functions" {
  // 基本字符串测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 边界情况测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

// 测试溢出保护机制
test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试数学交换律
test "commutative_properties" {
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
}

// 测试复杂表达式计算
test "complex_expression_evaluation" {
  // 嵌套运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)
  
  // 多层嵌套
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result2)
  
  // 混合正负数
  let result3 = @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-26, result3)
}

// 测试断言函数
test "assertion_functions" {
  // 测试整数断言
  @azimuth.assert_eq(10, 10)
  
  // 测试字符串断言
  @azimuth.assert_eq_string("test", "test")
  
  // 测试布尔断言
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
}