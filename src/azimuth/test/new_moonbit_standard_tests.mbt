// 新增的 MoonBit 标准测试用例
// 测试基本功能和边界情况

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(@azimuth.add(3, 7), 10)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(-3, -7), -10)
}

test "add_function_zero_cases" {
  // 测试 add 函数的零值情况
  @azimuth.assert_eq(@azimuth.add(5, 0), 5)
  @azimuth.assert_eq(@azimuth.add(0, 5), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(@azimuth.multiply(3, 5), 15)
  @azimuth.assert_eq(@azimuth.multiply(3, -5), -15)
  @azimuth.assert_eq(@azimuth.multiply(-3, -5), 15)
}

test "multiply_function_identity_cases" {
  // 测试 multiply 函数的单位元情况
  @azimuth.assert_eq(@azimuth.multiply(5, 1), 5)
  @azimuth.assert_eq(@azimuth.multiply(1, 5), 5)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
}

test "greet_function_basic_functionality" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  @azimuth.assert_eq_string(@azimuth.greet("@#$"), "Hello, @#$!")
}

test "add_function_overflow_handling" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
}

test "multiply_function_overflow_handling" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)
  @azimuth.assert_eq(@azimuth.multiply(-46341, 46341), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(result, 75)  // (10 + 20) * 3 - 15 = 75
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string(@azimuth.greet("  "), "Hello,   !")
  @azimuth.assert_eq_string(@azimuth.greet("a"), "Hello, a!")
  @azimuth.assert_eq_string(@azimuth.greet("ABCdef"), "Hello, ABCdef!")
}