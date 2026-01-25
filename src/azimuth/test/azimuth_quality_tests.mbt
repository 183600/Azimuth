// Azimuth 高质量测试用例
// 包含8个全面的测试用例，覆盖核心功能和边界情况

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, empty!", @azimuth.greet(""))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base = 10
  let doubled = @azimuth.multiply(base, 2)
  let incremented = @azimuth.add(doubled, 5)
  let divided = @azimuth.divide_with_ceil(incremented, 3)
  @azimuth.assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_business_logic" {
  // 真实世界业务逻辑测试
  // 场景：计算活动所需的桌子数量
  let guests = 47
  let chairs_per_table = 6
  let tables_needed = @azimuth.divide_with_ceil(guests, chairs_per_table)
  
  @azimuth.assert_eq(8, tables_needed)  // ceil(47/6) = 8
  
  // 验证总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(48, total_chairs)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界条件测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}