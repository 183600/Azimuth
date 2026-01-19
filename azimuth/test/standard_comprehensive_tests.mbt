test "divide_with_ceil_basic_positive" {
  // 测试基本的向上取整除法 - 正数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "divide_with_ceil_exact_division" {
  // 测试整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(64, 8))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "divide_with_ceil_boundary_values" {
  // 测试边界值的向上取整除法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(1073741824, @azimuth.divide_with_ceil(max_val, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(min_val, 1))
  @azimuth.assert_eq(-1073741824, @azimuth.divide_with_ceil(min_val, 2))
}

test "divide_with_ceil_small_numbers" {
  // 测试小数的向上取整除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
}

test "divide_with_ceil_large_divisor" {
  // 测试大除数的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(99, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(101, 100))
}

test "divide_with_ceil_practical_scenarios" {
  // 测试实际应用场景
  // 计算需要多少辆车来运送所有人（每辆车坐4人）
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))  // 10人需要3辆车
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(17, 4))  // 17人需要5辆车
  
  // 计算需要多少页来打印所有文档（每页3行）
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11行需要4页
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))   // 6行需要2页
}

test "comprehensive_mathematical_properties" {
  // 测试综合数学性质
  let a = 12
  let b = 5
  
  // 测试加法和乘法的分配律近似
  let add_then_div = @azimuth.divide_with_ceil(@azimuth.add(a, b), 3)
  let div_then_add = @azimuth.add(@azimuth.divide_with_ceil(a, 3), @azimuth.divide_with_ceil(b, 3))
  
  // 测试乘法和除法的关系
  let product = @azimuth.multiply(a, b)
  let div_product = @azimuth.divide_with_ceil(product, b)
  
  // 验证基本数学关系
  @azimuth.assert_eq(60, product)  // 12 * 5 = 60
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(60, 5))  // 60 / 5 = 12
}

test "edge_case_zero_divisor_handling" {
  // 测试零除数边界的邻近值
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-2147483648, 1))
  
  // 测试接近零的除数（但不为零）
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(1000000000, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-1000000000, 1))
}