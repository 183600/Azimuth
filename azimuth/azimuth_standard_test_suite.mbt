// Azimuth 标准测试用例集
// 使用标准的 MoonBit 测试语法

test "add_function_basic_tests" {
  // 测试基本加法功能
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试边界情况
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 测试基本乘法功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "divide_with_ceil_function_tests" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "assertion_functions_tests" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_false(3 > 10)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 15
  let b = 25
  let c = -10
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-400, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(365, result2)
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试-1的乘法
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 123
  let y = 456
  let z = 789
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 测试分配律 a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}