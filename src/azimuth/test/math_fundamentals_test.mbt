// 测试函数和断言函数从 azimuth 包导入

// 测试函数和断言函数从 azimuth 包导入


test "test_3_math_properties_add_fundamentals" {
  // Test commutative property
  assert_eq(add(2, 3), add(3, 2))
  assert_eq(add(-5, 10), add(10, -5))
  
  // Test associative property
  assert_eq(add(add(1, 2), 3), add(1, add(2, 3)))
  assert_eq(add(add(-1, -2), -3), add(-1, add(-2, -3)))
  
  // Test identity property
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
}

test "test_3_math_properties_multiply_fundamentals" {
  // Test commutative property
  assert_eq(multiply(2, 3), multiply(3, 2))
  assert_eq(multiply(-5, 10), multiply(10, -5))
  
  // Test associative property
  assert_eq(multiply(multiply(1, 2), 3), multiply(1, multiply(2, 3)))
  assert_eq(multiply(multiply(-1, -2), -3), multiply(-1, multiply(-2, -3)))
  
  // Test identity property
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-5, 1))
  
  // Test distributive property
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "test_3_boundary_values_fundamentals" {
  // Test with maximum values
  let max_val = 2147483647
  let min_val = -2147483648
  
  // These should not overflow
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  
  // These should handle overflow gracefully
  let result1 = add(max_val, 1)
  let result2 = add(min_val, -1)
  
  // Just make sure they don't crash
  if !(result1 >= min_val && result1 <= max_val) {
    raise @moonbitlang/core/builtin.Failure("Result1 out of bounds")
  }
  if !(result2 >= min_val && result2 <= max_val) {
    raise @moonbitlang/core/builtin.Failure("Result2 out of bounds")
  }
}