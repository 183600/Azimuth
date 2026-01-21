// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_function_basic_cases" {
  // 基本加法测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "add_function_overflow_protection" {
  // 加法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_special_values" {
  // 乘法特殊值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function" {
  // 向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
}

test "complex_combined_operations" {
  // 复杂组合运算测试
  let a = 10
  let b = 3
  
  // (a + b) * 2
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(26, result1)
  
  // a * b + 5
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, 5)
  @azimuth.assert_eq(35, result2)
}

test "mathematical_properties" {
  // 数学性质测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  
  // 加法单位元
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}