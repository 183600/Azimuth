// 综合测试用例 - 测试各种边界情况和特殊场景

test "add_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加1应该溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 最小值加-1应该溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 最大值加最小值
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最小值加1
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值乘以2应该溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值乘以2应该溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 最小值乘以-1的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试大数相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(2, @azimuth.add(-5, 7))
  
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-2, 9))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  
  // 除数为负数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(13, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(15, -3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-15, -3))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 特殊字符!", @azimuth.greet("特殊字符"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = -3
  
  // 加法和乘法混合
  @azimuth.assert_eq(50, @azimuth.multiply(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(20, @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 连续加法
  @azimuth.assert_eq(12, @azimuth.add(@azimuth.add(a, b), c))
  
  // 连续乘法
  @azimuth.assert_eq(-150, @azimuth.multiply(@azimuth.multiply(a, b), c))
}

test "zero_and_one_operations" {
  // 测试0和1的特殊运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let another_large = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num, another_large))
  // 注意：2000000000000超出了Int范围，所以不测试这个乘法
  
  // 接近边界的运算
  let near_max = 2147483000
  @azimuth.assert_eq(2147483647, @azimuth.add(near_max, 647))
}

test "assert_functions_comprehensive" {
  // 测试各种断言函数
  @azimuth.assert_true(1 < 2)
  @azimuth.assert_true(0 <= 0)
  @azimuth.assert_true(-5 < 0)
  
  @azimuth.assert_false(1 > 2)
  @azimuth.assert_false(0 < 0)
  @azimuth.assert_false(-5 > 0)
  
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}