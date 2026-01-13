// 新增的 MoonBit 测试用例
// 测试 add 函数的各种场景

test "add_negative_and_positive" {
  // 测试负数和正数相加
  assert_eq(-5, add(-10, 5))
  assert_eq(5, add(-3, 8))
  assert_eq(0, add(-100, 100))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000000, add(1000000000, 1000000000))
}

test "add_large_negative_numbers" {
  // 测试大负数相加
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
}

// 测试 multiply 函数的各种场景

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(25, multiply(-5, -5))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(-30, multiply(5, -6))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "multiply_with_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

// 测试 greet 函数的各种场景

test "greet_with_numbers" {
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 2023!", greet("2023"))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   spaced   out!", greet("  spaced   out"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, user+tag!", greet("user+tag"))
  assert_eq_string("Hello, path/to/file!", greet("path/to/file"))
}

// 综合测试

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(900, result)
}