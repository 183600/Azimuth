test "add_function_basic_tests" {
  // 基本加法测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_overflow_tests" {
  // 加法溢出测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 溢出保护测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_function_basic_tests" {
  // 基本乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "multiply_function_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 1和-1的特殊情况
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))  // 最小值乘以-1的特殊情况
  
  // 边界值测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
}

test "greet_function_tests" {
  // greet函数测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function_tests" {
  // 向上取整除法测试
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  
  // 零测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "combined_operations_tests" {
  // 组合运算测试
  // 加法和乘法组合
  @azimuth.assert_eq(25, @azimuth.add(@azimuth.multiply(3, 5), @azimuth.multiply(2, 5)))
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(3, 2), 4))
  
  // 复杂表达式
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(2, 3), 2), 2))
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(10, 2), 2), 5))
  
  // 链式运算
  @azimuth.assert_eq(16, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.divide_with_ceil(10, 3)))
}

test "assert_functions_tests" {
  // 断言函数测试
  // 布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 == 2)
  
  // 相等断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  // 字符串断言
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

test "mathematical_properties_tests" {
  // 数学性质测试
  // 加法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  
  // 乘法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(@azimuth.multiply(-5, 3), @azimuth.multiply(3, -5))
  
  // 分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, max_val))
  
  // 极值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 接近边界的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
}