// Azimuth 简洁测试用例
// 使用标准的 MoonBit 测试语法，覆盖主要功能

test "add_basic_operations" {
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "add_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 2000))
}

test "multiply_basic_operations" {
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "multiply_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
}

test "greet_function_tests" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_tests" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "assertion_functions" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_false(3 > 7)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_arithmetic" {
  let a = 15
  let b = 4
  let c = -2
  
  // (a * b) + c
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(58, result1)
  
  // a * (b + c)
  let result2 = @azimuth.multiply(a, @azimuth.add(b, c))
  @azimuth.assert_eq(30, result2)
  
  // 结合除法
  let result3 = @azimuth.divide_with_ceil(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(-30, result3)
}

test "mathematical_properties" {
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  
  // 乘法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(@azimuth.multiply(-5, 3), @azimuth.multiply(3, -5))
  
  // 分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}