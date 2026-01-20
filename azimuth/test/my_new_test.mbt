// 新增的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "azimuth_add_basic_operations" {
  // 测试加法基本操作
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
}

test "azimuth_multiply_basic_operations" {
  // 测试乘法基本操作
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "azimuth_greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "azimuth_divide_with_ceil_operations" {
  // 测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
}

test "azimuth_overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "azimuth_complex_expressions" {
  // 测试复杂表达式
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.add(2, 3), 4)
  @azimuth.assert_eq(20, result2)
  
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(10, 5), 4)
  @azimuth.assert_eq(4, result3)
}

test "azimuth_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "azimuth_mathematical_properties" {
  // 测试数学性质
  // 加法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  
  // 乘法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  
  // 分配律（在安全范围内）
  let distributive1 = @azimuth.multiply(2, @azimuth.add(3, 4))
  let distributive2 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4))
  @azimuth.assert_eq(distributive1, distributive2)
}