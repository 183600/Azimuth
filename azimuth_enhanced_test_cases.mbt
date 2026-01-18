// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，测试项目的核心功能

test "add_function_positive_numbers" {
  // 测试 add 函数处理正数
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试 add 函数处理负数
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "add_function_mixed_numbers" {
  // 测试 add 函数处理正负数混合
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
}

test "multiply_function_positive_numbers" {
  // 测试 multiply 函数处理正数
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "multiply_function_negative_numbers" {
  // 测试 multiply 函数处理负数
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "multiply_function_mixed_numbers" {
  // 测试 multiply 函数处理正负数混合
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "greet_function_standard_names" {
  // 测试 greet 函数处理标准名称
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet("Charlie"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数处理边缘情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, A B!", @azimuth.greet("A B"))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 2
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(17, result2)
}