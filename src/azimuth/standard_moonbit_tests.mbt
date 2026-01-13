// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-5, -5))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-10, 10))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-50, multiply(-5, 10))
  assert_eq(-50, multiply(5, -10))
}

test "multiply_by_negative_two" {
  // 测试乘以-2的情况
  assert_eq(-10, multiply(5, -2))
  assert_eq(-20, multiply(10, -2))
  assert_eq(10, multiply(-5, -2))
  assert_eq(20, multiply(-10, -2))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, Test123!", greet("Test123"))
  assert_eq_string("Hello, 123Test!", greet("123Test"))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  !", greet("  "))
  assert_eq_string("Hello, A B C!", greet("A B C"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(900, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加-1
  assert_eq(2147483646, add(max_val, -1))
  
  // 最小值加1
  assert_eq(-2147483647, add(min_val, 1))
  
  // 最大值加最小值
  assert_eq(-1, add(max_val, min_val))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试包含特殊符号
  assert_eq_string("Hello, $%#@!", greet("$%#@"))
  
  // 测试包含HTML标签
  assert_eq_string("Hello, <div>!</div>", greet("<div></div>"))
}