test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试相同符号的边界情况
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // 测试最小值加1
  assert_eq(min_val + 1, add(min_val, 1))
  
  // 测试最大值减1
  assert_eq(max_val - 1, add(max_val, -1))
  
  // 测试两个负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘
  assert_eq(max_val, multiply(46340, 46341))  // 接近sqrt(max_val)
  
  // 测试负数乘法
  assert_eq(-100, multiply(-10, 10))
  assert_eq(100, multiply(-10, -10))
  
  // 测试与2的幂次相乘
  assert_eq(16, multiply(4, 4))
  assert_eq(-32, multiply(-8, 4))
}

test "greet_function_special_cases" {
  // 测试字符串函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(5, divide_with_ceil(13, 3))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "add_function_negative_overflow" {
  // 测试负数溢出的特定情况
  let min_val = -2147483648
  
  // 测试两个较大的负数相加
  assert_eq(min_val, add(-1073741824, -1073741824))
  assert_eq(min_val, add(-2000000000, -147483648))
  
  // 测试接近边界的负数相加
  assert_eq(min_val, add(min_val + 100, -200))
}

test "multiply_function_min_value_special" {
  // 测试最小值乘法的特殊情况
  let min_val = -2147483648
  
  // 最小值与不同值的乘法
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}

test "complex_arithmetic_combinations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = -5
  
  // 测试加法和乘法的组合
  assert_eq(200, multiply(a, b))
  assert_eq(30, add(a, b))
  assert_eq(5, add(a, c))
  
  // 测试链式运算
  assert_eq(50, multiply(add(a, b), c))
  assert_eq(25, add(multiply(a, c), multiply(b, c)))
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  let x = 42
  let y = "MoonBit"
  let flag = true
  let flag_false = false
  
  // 测试各种断言
  assert_true(flag)
  assert_false(flag_false)
  assert_eq(x, 42)
  assert_eq_string(y, "MoonBit")
  
  // 测试计算结果的断言
  assert_eq(add(10, 20), 30)
  assert_eq(multiply(5, 6), 30)
  assert_eq_string(greet("Test"), "Hello, Test!")
}