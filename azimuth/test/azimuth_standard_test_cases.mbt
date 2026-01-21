// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试加法基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_basic_functionality" {
  // 测试问候函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "boundary_value_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 乘法边界测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "zero_value_operations" {
  // 测试零值操作
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "negative_number_operations" {
  // 测试负数操作
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = -3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-45, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(47, result2)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}