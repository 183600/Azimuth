// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-50, 50))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000000, add(1000000000, 1000000000))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(-40, multiply(8, -5))
}

test "multiply_by_large_numbers" {
  // 测试大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(81000000, multiply(9000, 9000))
}

test "greet_with_various_names" {
  // 测试不同类型的名称
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Charlie123!", greet("Charlie123"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的名称
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Connor!", greet("O'Connor"))
  assert_eq_string("Hello, Jean-Luc!", greet("Jean-Luc"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (5 + 3) * (10 - 2) = 8 * 8 = 64
  let sum = add(5, 3)
  let difference = add(10, -2)
  let result = multiply(sum, difference)
  assert_eq(64, result)
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的计算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}