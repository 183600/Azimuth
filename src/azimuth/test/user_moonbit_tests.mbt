// 用户请求的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "basic_addition_test" {
  // 测试基本加法功能
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}

test "addition_with_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(-5, 3)
  @azimuth.assert_eq(-2, result1)
  
  let result2 = @azimuth.add(-5, -3)
  @azimuth.assert_eq(-8, result2)
}

test "basic_multiplication_test" {
  // 测试基本乘法功能
  let result = @azimuth.multiply(4, 6)
  @azimuth.assert_eq(24, result)
}

test "multiplication_with_zero" {
  // 测试与零相乘
  let result1 = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result1)
  
  let result2 = @azimuth.multiply(0, 5)
  @azimuth.assert_eq(0, result2)
}

test "greet_function_test" {
  // 测试问候函数
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_empty_string" {
  // 测试空字符串问候
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "complex_calculation_test" {
  // 测试复合计算：(a + b) * c
  let a = 3
  let b = 4
  let c = 5
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(35, result)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 0)
  @azimuth.assert_eq(0, result)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 11
  
  // 加法交换律
  let add_result1 = @azimuth.add(a, b)
  let add_result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(add_result1, add_result2)
  
  // 乘法交换律
  let mul_result1 = @azimuth.multiply(a, b)
  let mul_result2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(mul_result1, mul_result2)
}