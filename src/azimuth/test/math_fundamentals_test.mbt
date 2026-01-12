


test "test_3_math_properties_add_fundamentals" {
  // Test commutative property
  azimuth::assert_eq(azimuth::add(2, 3), azimuth::add(3, 2))
  azimuth::assert_eq(azimuth::add(-5, 10), azimuth::add(10, -5))
  
  // Test associative property
  azimuth::assert_eq(azimuth::add(azimuth::add(1, 2), 3), azimuth::add(1, azimuth::add(2, 3)))
  azimuth::assert_eq(azimuth::add(azimuth::add(-1, -2), -3), azimuth::add(-1, azimuth::add(-2, -3)))
  
  // Test identity property
  azimuth::assert_eq(5, azimuth::add(5, 0))
  azimuth::assert_eq(-5, azimuth::add(-5, 0))
}

test "test_3_math_properties_multiply_fundamentals" {
  // Test commutative property
  azimuth::assert_eq(azimuth::multiply(2, 3), azimuth::multiply(3, 2))
  azimuth::assert_eq(azimuth::multiply(-5, 10), azimuth::multiply(10, -5))
  
  // Test associative property
  azimuth::assert_eq(azimuth::multiply(azimuth::multiply(1, 2), 3), azimuth::multiply(1, azimuth::multiply(2, 3)))
  azimuth::assert_eq(azimuth::multiply(azimuth::multiply(-1, -2), -3), azimuth::multiply(-1, azimuth::multiply(-2, -3)))
  
  // Test identity property
  azimuth::assert_eq(5, azimuth::multiply(5, 1))
  azimuth::assert_eq(-5, azimuth::multiply(-5, 1))
  
  // Test distributive property
  azimuth::assert_eq(azimuth::multiply(2, azimuth::add(3, 4)), azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(2, 4)))
}

test "test_3_boundary_values_fundamentals" {
  // Test with maximum values
  let max_val = 2147483647
  let min_val = -2147483648
  
  // These should not overflow
  azimuth::assert_eq(max_val, azimuth::add(max_val, 0))
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  
  // These should handle overflow gracefully
  let result1 = azimuth::add(max_val, 1)
  let result2 = azimuth::add(min_val, -1)
  
  // Just make sure they don't crash
  if !(result1 >= min_val && result1 <= max_val) {
    raise @moonbitlang/core/builtin.Failure("Result1 out of bounds")
  }
  if !(result2 >= min_val && result2 <= max_val) {
    raise @moonbitlang/core/builtin.Failure("Result2 out of bounds")
  }
}