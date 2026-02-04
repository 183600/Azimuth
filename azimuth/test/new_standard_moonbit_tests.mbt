// 新的标准 MoonBit 测试用例
// 包含 8 个核心功能测试，使用标准的 MoonBit 测试语法

test "moonbit_basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(10, @azimuth.add(5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "moonbit_identity_properties" {
  // 测试单位元性质
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
}

test "moonbit_commutative_properties" {
  // 测试交换律
  @azimuth.assert_eq(@azimuth.add(3, 5), @azimuth.add(5, 3))
  @azimuth.assert_eq(@azimuth.add(-2, 8), @azimuth.add(8, -2))
  @azimuth.assert_eq(@azimuth.multiply(3, 5), @azimuth.multiply(5, 3))
  @azimuth.assert_eq(@azimuth.multiply(-2, 8), @azimuth.multiply(8, -2))
}

test "moonbit_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))  // 避免溢出
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "moonbit_negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "moonbit_large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large1, large2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large1, large2))
  
  let large_negative = -1000000
  @azimuth.assert_eq(0, @azimuth.add(large1, large_negative))
  @azimuth.assert_eq(-1000000000000, @azimuth.multiply(large1, large_negative))
}

test "moonbit_greeting_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "moonbit_divide_with_ceil_operations" {
  // 测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 测试负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  
  // 测试除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}