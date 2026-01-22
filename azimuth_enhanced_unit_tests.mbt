// Azimuth 增强单元测试
// 使用标准的 MoonBit 测试语法，包含高质量测试用例

test "add_function_zero_identity" {
  // 测试加法的零元素特性
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_zero_property" {
  // 测试乘法的零元素特性
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -17))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_function_identity_element" {
  // 测试乘法的单位元素特性
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "divide_with_ceil_perfect_division" {
  // 测试向上取整除法的完全整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 7))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(12, -4))
}

test "divide_with_ceil_fractional_positive" {
  // 测试向上取整除法的正数分数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))
}

test "divide_with_ceil_mixed_signs" {
  // 测试向上取整除法的混合符号情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(2, -3))
}

test "greet_function_empty_and_special_cases" {
  // 测试问候函数的空字符串和特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("!"))
  @azimuth.assert_eq_string("Hello, Hello, World!!", @azimuth.greet("Hello, World!"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  let x = 15
  let y = 7
  let z = 3
  
  // (x + y) * z
  let sum = @azimuth.add(x, y)
  let result1 = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(66, result1)
  
  // x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(108, result2)
  
  // x + y + z (测试加法结合性)
  let result3 = @azimuth.add(@azimuth.add(x, y), z)
  @azimuth.assert_eq(25, result3)
}

test "mathematical_commutativity_verification" {
  // 验证数学交换律
  let a = 123
  let b = -456
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(-5, 8), @azimuth.multiply(8, -5))
  
  // 零元素交换律测试
  @azimuth.assert_eq(@azimuth.add(42, 0), @azimuth.add(0, 42))
  @azimuth.assert_eq(@azimuth.multiply(42, 0), @azimuth.multiply(0, 42))
}
