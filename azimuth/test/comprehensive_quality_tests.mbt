test "add_function_extreme_boundary_values" {
  // 测试极值边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
}

test "add_function_symmetric_properties" {
  // 测试加法对称性
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  @azimuth.assert_eq(@azimuth.add(-5, -3), @azimuth.add(-3, -5))
  
  // 测试加法结合律的简单情况
  let a = 10; let b = 20; let c = 30
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

test "multiply_function_power_operations" {
  // 测试幂运算相关
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(27, @azimuth.multiply(3, 9))
  @azimuth.assert_eq(16, @azimuth.multiply(-2, -8))
  @azimuth.assert_eq(-16, @azimuth.multiply(-2, 8))
}

test "multiply_function_min_value_special_cases" {
  // 测试最小值的特殊处理
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_unicode_and_special_chars" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, a@b.com!", @azimuth.greet("a@b.com"))
}

test "greet_function_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100))
  
  // 零和负除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "combined_operations_complex_scenarios" {
  // 测试组合运算的复杂场景
  let x = 15; let y = -8; let z = 4
  
  // 复杂表达式测试
  let result1 = @azimuth.add(@azimuth.multiply(x, y), z)
  let expected1 = @azimuth.add(@azimuth.multiply(15, -8), 4)
  @azimuth.assert_eq(result1, expected1)
  
  // 嵌套运算测试
  let result2 = @azimuth.multiply(@azimuth.add(x, y), z)
  let expected2 = @azimuth.multiply(@azimuth.add(15, -8), 4)
  @azimuth.assert_eq(result2, expected2)
  
  // 链式运算测试
  let chain_result = @azimuth.add(@azimuth.add(x, y), @azimuth.multiply(z, 2))
  let chain_expected = @azimuth.add(@azimuth.add(15, -8), @azimuth.multiply(4, 2))
  @azimuth.assert_eq(chain_result, chain_expected)
}