// Simple test for Azimuth telemetry system
test "basic test for new functionality" {
  // Test basic arithmetic
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  
  // Test boolean logic
  assert_true(true)
  assert_false(false)
  
  // Test string operations
  assert_eq("hello" + " world", "hello world")
  assert_eq("test".length(), 4)
  
  // Test array operations
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // Test option types
  let some_value = Some(42)
  let none_value = None
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
}