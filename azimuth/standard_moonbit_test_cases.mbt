// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(-1, add(2, -3))
}

test "multiply_function_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_function_basic_cases" {
  // 基本问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 基本向上取整除法测试
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(2, divide_with_ceil(5, 2))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "add_overflow_protection" {
  // 加法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 乘法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负溢出
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "special_value_handling" {
  // 特殊值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法特殊值
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(5, add(0, 5))
  
  // 乘法特殊值
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(1, min_val))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 10
  let b = 5
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律（在安全范围内）
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律（在安全范围内）
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 空字符串和单字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "complex_operation_combinations" {
  // 复杂运算组合测试
  let a = 8
  let b = 3
  let c = 2
  
  // 组合运算1: (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(22, result1)
  
  // 组合运算2: a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(26, result2)
  
  // 组合运算3: ceil(a * b / c)
  let result3 = divide_with_ceil(multiply(a, b), c)
  assert_eq(12, result3)
  
  // 组合运算4: ceil((a + b) / c) * b
  let result4 = multiply(divide_with_ceil(add(a, b), c), b)
  assert_eq(15, result4)
}