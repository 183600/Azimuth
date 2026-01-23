// 增强的单元测试用例 - 专注于边界条件和特殊情况

test "add_overflow_edge_cases" {
  // 测试大数加法
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_with_zero_edge_cases" {
  // 测试与零的乘法
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, multiply(-1000000, 0))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负被除数的情况
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  assert_eq(-1, divide_with_ceil(-1, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负除数的情况
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-3, divide_with_ceil(9, -3))
  assert_eq(0, divide_with_ceil(1, -100))
}

test "divide_with_ceil_both_negative" {
  // 测试两个负数的情况
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(1, divide_with_ceil(-1, -100))
}

test "greet_with_whitespace_names" {
  // 测试包含空格的名称
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello,  Alice  !", greet("  Alice  "))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let step1 = add(10, 5)
  let step2 = multiply(step1, 2)
  let step3 = divide_with_ceil(step2, 3)
  assert_eq(10, step3)  // ((10+5)*2)/3 = 30/3 = 10
}

test "divide_with_ceil_precision_edge" {
  // 测试精度边界情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "add_with_large_negative_numbers" {
  // 测试大负数加法
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(-500000, add(-1000000, 500000))
  assert_eq(500000, add(-500000, 1000000))
}