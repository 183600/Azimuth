// 综合测试用例 - 覆盖 azimuth 库的主要功能

test "add_function_comprehensive" {
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "greet_function_edge_cases" {
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  
  // 长字符串测试
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "divide_with_ceil_positive_numbers" {
  // 正数向上取整测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(11, 2))
}

test "divide_with_ceil_mixed_signs" {
  // 混合符号测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, -2))
}

test "divide_with_ceil_boundary_values" {
  // 边界值测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}

test "complex_operations_combination" {
  // 组合操作测试
  let a = 5
  let b = 3
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(a, b)
  let divided = @azimuth.divide_with_ceil(sum, b)
  
  @azimuth.assert_eq(8, sum)
  @azimuth.assert_eq(15, product)
  @azimuth.assert_eq(3, divided)
}

test "mathematical_properties" {
  // 数学性质测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(2, 3), @azimuth.add(3, 2))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(2, 3), @azimuth.multiply(3, 2))
  
  // 加法单位元
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  
  // 乘法单位元
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
}