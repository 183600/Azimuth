// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "add_function_zero_cases" {
  // 测试加法函数的零值处理
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_function_overflow_cases" {
  // 测试加法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(8, @azimuth.multiply(-2, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
}

test "multiply_function_identity_cases" {
  // 测试乘法函数的单位元处理
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(-18, @azimuth.multiply(-18, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
}

test "multiply_function_negative_one_cases" {
  // 测试乘法函数的-1处理
  @azimuth.assert_eq(-9, @azimuth.multiply(9, -1))
  @azimuth.assert_eq(13, @azimuth.multiply(-13, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "multiply_function_overflow_cases" {
  // 测试乘法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_function_basic_cases" {
  // 测试问候函数的基本情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "complex_combined_operations" {
  // 测试复合运算
  let x = 10
  let y = 20
  let z = -5
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-150, result)
  
  // 计算 x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(195, result2)
}