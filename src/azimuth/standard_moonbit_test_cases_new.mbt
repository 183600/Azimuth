// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-5, 2))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 123))
  assert_eq(1, multiply(1, 1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试零值
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试特殊值
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(0, max_val))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)      // 8 * 5 = 40
  assert_eq(26, perimeter) // 2*8 + 2*5 = 26
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTesting"
  assert_eq_string("Hello, ThisIsAVeryLongStringForTesting!", greet(long_string))
  
  // 测试包含特殊字符的字符串
  let special_string = "test@123#"
  assert_eq_string("Hello, test@123#!", greet(special_string))
  
  // 测试包含空格的字符串
  let spaced_string = "John Doe"
  assert_eq_string("Hello, John Doe!", greet(spaced_string))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 验证加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 验证乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}