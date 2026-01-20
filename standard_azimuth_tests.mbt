// 标准 Azimuth 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_zero_cases" {
  // 测试加法函数中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-42, @azimuth.add(0, -42))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
}

test "add_function_extreme_values" {
  // 测试加法函数的极值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近极值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(123456789, @azimuth.multiply(12345, 10001))
}

test "multiply_function_extreme_values" {
  // 测试乘法函数的极值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与极值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 4))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(22, 4))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(23, 4))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-16, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-15, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-14, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-13, 4))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483647, 2147483647))
}

test "greet_function_unicode" {
  // 测试问候函数的Unicode字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "greet_function_whitespace" {
  // 测试问候函数的空白字符处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello,  a !", @azimuth.greet(" a "))
}

test "combined_operations_complex" {
  // 测试复合运算的复杂情况
  let a = 100
  let b = -50
  let c = 25
  let d = -10
  
  // 计算 ((a + b) * c) + d
  let sum1 = @azimuth.add(a, b)
  let product1 = @azimuth.multiply(sum1, c)
  let result1 = @azimuth.add(product1, d)
  @azimuth.assert_eq(-1260, result1)
  
  // 计算 (a * b) + (c * d)
  let product2 = @azimuth.multiply(a, b)
  let product3 = @azimuth.multiply(c, d)
  let result2 = @azimuth.add(product2, product3)
  @azimuth.assert_eq(-5250, result2)
}

test "assert_functions_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_true(true && true)
  @azimuth.assert_true(true || false)
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_true(0 == 0)
  
  @azimuth.assert_false(false && true)
  @azimuth.assert_false(false || false)
  @azimuth.assert_false(1 < 0)
  @azimuth.assert_false(-1 > 0)
  @azimuth.assert_false(1 == 0)
  
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}