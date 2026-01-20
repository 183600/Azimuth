// 增强的核心功能测试用例
// 测试 azimuth 包的主要功能

test "add_function_edge_cases" {
  // 测试边界值和特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 对称性测试
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
}

test "multiply_function_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 负一乘法测试
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(-1, -5))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val)) // 特殊情况：最小值乘以-1会溢出
}

test "greet_function_unicode_handling" {
  // 测试字符串处理和Unicode字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit🚀!", @azimuth.greet("MoonBit🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   ")) // 空格测试
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
  
  // 混合符号测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, 3))
}

test "combined_operations_test" {
  // 测试组合操作
  let a = 10
  let b = 3
  
  // 加法后乘法
  @azimuth.assert_eq(39, @azimuth.multiply(@azimuth.add(a, b), 3))
  
  // 乘法后加法
  @azimuth.assert_eq(33, @azimuth.add(@azimuth.multiply(a, b), 3))
  
  // 复杂表达式
  @azimuth.assert_eq(16, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.divide_with_ceil(10, 3)))
}

test "mathematical_properties" {
  // 测试数学性质
  
  // 加法结合律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(2, 3), 4), @azimuth.add(2, @azimuth.add(3, 4)))
  
  // 乘法结合律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(2, 3), 4), @azimuth.multiply(2, @azimuth.multiply(3, 4)))
  
  // 分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}

test "boundary_comprehensive_test" {
  // 综合边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  
  // 接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341)) // 接近sqrt(max_val)
  
  // 最小值处理
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "string_edge_cases" {
  // 字符串边界情况测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}