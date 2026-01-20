// Azimuth 综合标准测试用例
// 使用标准的 MoonBit 测试语法，覆盖主要功能

test "add_function_basic_tests" {
  // 测试加法的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(42, @azimuth.add(100, -58))
}

test "add_function_overflow_tests" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试边界情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_function_basic_tests" {
  // 测试乘法的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
}

test "multiply_function_overflow_tests" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试与1和-1的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
}

test "greet_function_tests" {
  // 测试字符串拼接功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "divide_with_ceil_tests" {
  // 测试向上取整除法
  // 正数测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  
  // 混合符号测试
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, 3))
}

test "assertion_functions_tests" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

test "complex_arithmetic_tests" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = -5
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(195, result)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(150, result2)
  
  // 更复杂的表达式
  let temp1 = @azimuth.multiply(3, 4)
  let temp2 = @azimuth.add(temp1, 5)
  let temp3 = @azimuth.multiply(temp2, 2)
  let final_result = @azimuth.add(temp3, -10)
  @azimuth.assert_eq(26, final_result)
}

test "edge_cases_tests" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界情况
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  
  // 乘法边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "mathematical_properties_tests" {
  // 测试数学性质（在安全范围内）
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(2, 3), @azimuth.add(3, 2))
  @azimuth.assert_eq(@azimuth.add(-2, 3), @azimuth.add(3, -2))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(2, 3), 4), @azimuth.add(2, @azimuth.add(3, 4)))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(2, 3), @azimuth.multiply(3, 2))
  @azimuth.assert_eq(@azimuth.multiply(-2, 3), @azimuth.multiply(3, -2))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(2, 3), 4), @azimuth.multiply(2, @azimuth.multiply(3, 4)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}