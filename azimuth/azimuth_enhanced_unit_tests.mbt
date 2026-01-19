// Azimuth 增强单元测试
// 使用标准的 MoonBit 测试语法

test "add_zero_handling" {
  // 测试加法中0的处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-8, @azimuth.add(12, -20))
}

test "multiply_by_zero" {
  // 测试乘以0的情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(-456, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_by_one_and_minus_one" {
  // 测试乘以1和-1的情况
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-456, @azimuth.multiply(-456, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  @azimuth.assert_eq(456, @azimuth.multiply(-456, -1))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 	\n!", @azimuth.greet("\t\n"))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
}

test "divide_with_ceil_exact_division" {
  // 测试整除的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(16, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let x = 5
  let y = 3
  let z = 2
  
  // 计算 (x * y) + (x * z)
  let term1 = @azimuth.multiply(x, y)
  let term2 = @azimuth.multiply(x, z)
  let result = @azimuth.add(term1, term2)
  @azimuth.assert_eq(25, result)
  
  // 计算 x * (y + z)
  let sum = @azimuth.add(y, z)
  let result2 = @azimuth.multiply(x, sum)
  @azimuth.assert_eq(25, result2)
}

test "assert_functions_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_true(-1 < 0)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq_string("", "")
}