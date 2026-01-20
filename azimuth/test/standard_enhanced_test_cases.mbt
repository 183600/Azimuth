// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_overflow_handling" {
  // 加法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_cases" {
  // 基本乘法功能测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
}

test "multiply_function_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值特殊处理测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 大数乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
}

test "greet_function_various_inputs" {
  // greet 函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  
  // 混合符号测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, 3))
}

test "complex_combined_operations" {
  // 复杂组合运算测试
  let a = 10
  let b = 3
  
  // 组合运算测试
  @azimuth.assert_eq(39, @azimuth.multiply(@azimuth.add(a, b), 3))
  @azimuth.assert_eq(33, @azimuth.add(@azimuth.multiply(a, b), 3))
  @azimuth.assert_eq(16, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.divide_with_ceil(10, 3)))
  
  // 嵌套运算测试
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(10, 2), 2), 5))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  // 加法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  
  // 乘法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(@azimuth.multiply(-5, 3), @azimuth.multiply(3, -5))
  
  // 分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}

test "boundary_conditions_comprehensive" {
  // 综合边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}