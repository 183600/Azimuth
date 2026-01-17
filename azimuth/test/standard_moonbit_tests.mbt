// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_positive_numbers" {
  // 测试基本正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-15, 5))
}

test "add_function_overflow_boundary" {
  // 测试加法溢出边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "multiply_function_overflow_boundary" {
  // 测试乘法溢出边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试负溢出保护
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
}

test "combined_operations_complex_test" {
  // 测试组合运算的复杂情况
  let a = 10
  let b = -5
  let c = 3
  let d = 20
  
  // 计算 (a + b) * c - d
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(-35, result)
  
  // 测试嵌套运算
  let x = @azimuth.multiply(@azimuth.add(100, -50), @azimuth.add(2, 3))
  @azimuth.assert_eq(250, x)
}