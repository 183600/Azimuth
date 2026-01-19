// Azimuth 增强单元测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况

test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_large_numbers" {
  // 测试大数加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
}

test "multiply_identity_elements" {
  // 测试乘法的单位元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(1, -42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "multiply_negative_one" {
  // 测试乘以-1的情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(25, 5))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-25, 5))
}

test "greet_unicode_handling" {
  // 测试问候函数的Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 15
  let b = -4
  let c = 7
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(77, product)
  
  // 计算 a * b + c
  let product2 = @azimuth.multiply(a, b)
  let sum2 = @azimuth.add(product2, c)
  @azimuth.assert_eq(-53, sum2)
  
  // 使用向上取整除法
  let ceil_result = @azimuth.divide_with_ceil(product, 10)
  @azimuth.assert_eq(8, ceil_result)
}

test "assert_functions_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(1 < 0)
  @azimuth.assert_false(-1 > 0)
  @azimuth.assert_false(0 != 0)
}