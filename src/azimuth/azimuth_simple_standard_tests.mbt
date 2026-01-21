// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_positive_numbers" {
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(0, 1))
}

test "add_negative_numbers" {
  assert_eq(-10, add(-3, -7))
  assert_eq(-100, add(-50, -50))
  assert_eq(-1, add(0, -1))
}

test "add_mixed_numbers" {
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_positive_numbers" {
  assert_eq(21, multiply(3, 7))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(0, 5))
}

test "multiply_negative_numbers" {
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(0, multiply(0, -5))
}

test "greet_standard_names" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Charlie!", greet("Charlie"))
}

test "divide_with_ceil_positive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "divide_with_ceil_edge_cases" {
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "complex_calculations" {
  // 组合函数测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)
  
  let result2 = divide_with_ceil(add(10, 5), 4)
  assert_eq(4, result2)
  
  let result3 = multiply(add(2, 3), divide_with_ceil(10, 3))
  assert_eq(20, result3)
}