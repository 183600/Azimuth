// Azimuth 简单增强测试
// 使用标准的 MoonBit 测试语法

test "enhanced_add_overflow" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "enhanced_multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试与1的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "enhanced_divide_with_ceil" {
  // 测试向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 3))
}

test "enhanced_greet_unicode" {
  // 测试Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "enhanced_complex_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(900, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(230, result2)
}