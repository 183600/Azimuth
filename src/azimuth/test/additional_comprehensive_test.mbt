// 测试函数和断言函数从 azimuth 包导入

// 测试函数和断言函数从 azimuth 包导入


test "test_2_edge_cases_add_additional" {
  // Test with zero
  assert_eq(5, add(0, 5))
  assert_eq(5, add(5, 0))
  
  // Test with large numbers
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "test_2_edge_cases_multiply_additional" {
  // Test with zero
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  
  // Test with one
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  
  // Test with negative one
  assert_eq(-5, multiply(-1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-1, -5))
}

test "test_2_edge_cases_greet_additional" {
  // Test with special characters
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}