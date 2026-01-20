// Azimuth 综合标准测试用例
// 使用标准的 MoonBit 测试语法，覆盖主要功能

test "add_function_basic_tests" {
  // 测试加法的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(42, add(100, -58))
}

test "add_function_overflow_tests" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试边界情况
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic_tests" {
  // 测试乘法的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-30, multiply(6, -5))
}

test "multiply_function_overflow_tests" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试与1和-1的乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(-max_val, multiply(max_val, -1))
}

test "greet_function_tests" {
  // 测试字符串拼接功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "divide_with_ceil_tests" {
  // 测试向上取整除法
  // 正数测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-1, 3))
  
  // 混合符号测试
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, 3))
}

test "assertion_functions_tests" {
  // 测试断言函数
  assert_true(true)
  assert_true(1 == 1)
  assert_true(0 == 0)
  assert_true(-1 == -1)
  
  assert_false(false)
  assert_false(1 == 0)
  assert_false(0 == 1)
  assert_false(-1 == 1)
  
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}

test "complex_arithmetic_tests" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = -5
  
  // 计算 (a * b) + c
  let product = multiply(a, b)
  let result = add(product, c)
  assert_eq(195, result)
  
  // 计算 a * (b + c)
  let sum = add(b, c)
  let result2 = multiply(a, sum)
  assert_eq(150, result2)
  
  // 更复杂的表达式
  let temp1 = multiply(3, 4)
  let temp2 = add(temp1, 5)
  let temp3 = multiply(temp2, 2)
  let final_result = add(temp3, -10)
  assert_eq(26, final_result)
}

test "edge_cases_tests" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界情况
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val - 1, add(max_val, -1))
  assert_eq(min_val + 1, add(min_val, 1))
  
  // 乘法边界情况
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
}

test "mathematical_properties_tests" {
  // 测试数学性质（在安全范围内）
  // 加法交换律
  assert_eq(add(2, 3), add(3, 2))
  assert_eq(add(-2, 3), add(3, -2))
  
  // 加法结合律
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  
  // 乘法交换律
  assert_eq(multiply(2, 3), multiply(3, 2))
  assert_eq(multiply(-2, 3), multiply(3, -2))
  
  // 乘法结合律
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))
  
  // 分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}