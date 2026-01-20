// 增强的单元测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(1, 99))
}

test "add_function_zero_cases" {
  // 测试与0相加的情况
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_basic_cases" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
}

test "multiply_function_identity_cases" {
  // 测试乘法单位元情况
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(25, @azimuth.multiply(1, 25))
  @azimuth.assert_eq(-25, @azimuth.multiply(-25, 1))
  @azimuth.assert_eq(-25, @azimuth.multiply(1, -25))
  @azimuth.assert_eq(-25, @azimuth.multiply(25, -1))
  @azimuth.assert_eq(-25, @azimuth.multiply(-1, 25))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
}

test "boolean_assertions_various_cases" {
  // 测试布尔断言的各种情况
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(-2 < 0)
  @azimuth.assert_true(10 == 10)
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_false(-2 > 0)
  @azimuth.assert_false(10 == 5)
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 12
  let b = 8
  let c = 3
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(99, result)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(132, result2)
}