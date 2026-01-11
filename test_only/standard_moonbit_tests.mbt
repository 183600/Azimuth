// 标准MoonBit测试用例 - 针对azimuth库的核心功能

// 测试用例1: 基础加法功能
///|
test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(7, 3))
}

// 测试用例2: 基础乘法功能
///|
test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-6, multiply(-2, 3))
}

// 测试用例3: 字符串拼接功能
///|
test "string_greeting" {
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  assert_eq("Hello, !", greet(""))
}

// 测试用例4: 加法边界值测试
///|
test "addition_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的情况
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
}

// 测试用例5: 乘法边界值测试
///|
test "multiplication_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(-1, min_val))
}

// 测试用例6: 数学运算组合测试
///|
test "combined_operations" {
  // 测试 (a + b) * c
  let a = 10
  let b = 5
  let c = 3
  let result = multiply(add(a, b), c)
  assert_eq(45, result)  // (10 + 5) * 3 = 45
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)  // 10 * 5 + 3 = 53
}

// 测试用例7: 负数运算测试
///|
test "negative_number_operations" {
  // 负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(-10, 8))
  
  // 负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-5, 0))
}

// 测试用例8: 特殊值处理测试
///|
test "special_values_handling" {
  // 测试0的特殊处理
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(42, 0))
  
  // 测试1的特殊处理
  assert_eq(42, multiply(1, 42))
  assert_eq(42, multiply(42, 1))
  
  // 测试-1的特殊处理
  assert_eq(-42, multiply(-1, 42))
  assert_eq(-42, multiply(42, -1))
}

// 测试用例9: 大数运算测试
///|
test "large_number_operations" {
  let large_num = 1000000
  let another_large = 2000000
  
  // 大数加法
  assert_eq(3000000, add(large_num, another_large))
  
  // 大数乘法（但不会溢出）
  assert_eq(1000000, multiply(large_num, 1))
  assert_eq(2000000, multiply(large_num, 2))
}

// 测试用例10: 字符串边界情况测试
///|
test "string_edge_cases" {
  // 测试长字符串
  let long_name = "VeryLongNameThatContainsManyCharacters"
  assert_eq("Hello, VeryLongNameThatContainsManyCharacters!", greet(long_name))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", greet("John Doe"))
}