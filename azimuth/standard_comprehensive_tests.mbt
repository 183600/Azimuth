// 标准综合测试用例 - 使用 MoonBit 标准测试语法
// 包含10个测试用例，覆盖所有主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-10 < -5)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(0 != 0)
  @azimuth.assert_false(-10 > -5)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "overflow_edge_cases" {
  // 测试溢出边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "special_values_handling" {
  // 测试特殊值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 与-1的运算
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算
  let a = 12
  let b = -8
  let c = 5
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(20, result)
  
  // a * (b + c)
  let sum2 = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum2)
  @azimuth.assert_eq(-36, result2)
  
  // (a * c) + b
  let product = @azimuth.multiply(a, c)
  let result3 = @azimuth.add(product, b)
  @azimuth.assert_eq(52, result3)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-15, @azimuth.add(-7, -8))
  @azimuth.assert_eq(-20, @azimuth.add(-30, 10))
  @azimuth.assert_eq(10, @azimuth.add(30, -20))
  @azimuth.assert_eq(56, @azimuth.multiply(-7, -8))
  @azimuth.assert_eq(-30, @azimuth.multiply(-3, 10))
  @azimuth.assert_eq(-40, @azimuth.multiply(4, -10))
}

test "comprehensive_function_integration" {
  // 综合功能集成测试
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
  
  let x = 15
  let y = 25
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(x, y)
  let ceil_div = @azimuth.divide_with_ceil(sum, 10)
  
  @azimuth.assert_eq(40, sum)
  @azimuth.assert_eq(375, product)
  @azimuth.assert_eq(4, ceil_div)
  @azimuth.assert_true(sum < product)
  @azimuth.assert_false(sum > product)
}