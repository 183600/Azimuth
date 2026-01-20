// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，覆盖更多边界情况和复杂场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值附近的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  
  // 测试最小值附近的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
  
  // 测试大数值相加但不溢出
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
}

test "add_function_mixed_signs" {
  // 测试不同符号数的加法
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
  
  // 测试零的各种组合
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(46340, @azimuth.multiply(46340, 1))  // sqrt(max_val)
  @azimuth.assert_eq(-46340, @azimuth.multiply(-46340, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与特殊值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊：最小值乘以-1会溢出
}

test "divide_with_ceil_function" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2, 3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 3))
}

test "greet_function_special_cases" {
  // 测试字符串函数的特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))  // 测试 Unicode
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 测试空格
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = -5
  
  // 组合运算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)  // 10 * 20 + (-5) = 195
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)  // (10 + 20) * (-5) = -150
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(a, b), c)  // (10 + 20) / -5 向上取整 = -6
  
  // 验证结果
  @azimuth.assert_eq(195, result1)
  @azimuth.assert_eq(-150, result2)
  @azimuth.assert_eq(-6, result3)
}