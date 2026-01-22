// Azimuth 库的标准测试用例
// 使用标准的 MoonBit 测试语法，覆盖所有主要功能

test "basic_addition_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0)) // 除数为0的情况
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "complex_arithmetic_combinations" {
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

test "boolean_assertions_various_cases" {
  // 测试布尔断言的各种情况
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(-2 < 0)
  @azimuth.assert_true(10 == 10)
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_false(-2 > 0)
  @azimuth.assert_false(10 == 5)
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  @azimuth.assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 特殊值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}