// 新的标准 MoonBit 测试用例
// 测试 azimuth 包的额外功能

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-15, 27), @azimuth.add(27, -15))
  @azimuth.assert_eq(@azimuth.add(-15, -27), @azimuth.add(-27, -15))
  @azimuth.assert_eq(@azimuth.add(0, 42), @azimuth.add(42, 0))
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-6, 7), @azimuth.multiply(7, -6))
  @azimuth.assert_eq(@azimuth.multiply(-6, -7), @azimuth.multiply(-7, -6))
  @azimuth.assert_eq(@azimuth.multiply(0, 42), @azimuth.multiply(42, 0))
}

test "add_function_associative_property" {
  // 测试加法结合律
  let a = 10
  let b = 20
  let c = 30
  
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(-a, b), c), @azimuth.add(-a, @azimuth.add(b, c)))
}

test "multiply_function_with_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(99980001, @azimuth.multiply(9999, 9999))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1999, 2))
}

test "greet_function_with_various_input_types" {
  // 测试问候函数的各种输入类型
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
  @azimuth.assert_eq_string("Hello, A+B=C!", @azimuth.greet("A+B=C"))
}

test "complex_nested_operations" {
  // 测试复杂的嵌套操作
  // ((a + b) * c) + d
  let a = 5
  let b = 3
  let c = 4
  let d = 7
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, d)
  
  @azimuth.assert_eq(35, result)
}

test "edge_case_zero_operations" {
  // 测试零值的边缘情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  
  // 乘法恒等：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  
  // 乘法零性质：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-42, 0))
}

test "boundary_value_safety" {
  // 测试边界值的安全性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值操作不会导致意外结果
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}