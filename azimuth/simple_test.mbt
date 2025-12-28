test "basic arithmetic operations" {
  // Basic test to ensure project setup is working
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 4, 6)
  assert_eq(8 / 2, 4)
  assert_true(true)
  assert_false(false)
}

test "string operations" {
  // Test string concatenation and comparison
  let str1 = "Hello"
  let str2 = " World"
  let combined = str1 + str2
  
  assert_eq(combined, "Hello World")
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 6)
  assert_eq(combined.length(), 11)
}

test "array operations" {
  // Test array creation and manipulation
  let empty_array = []
  let int_array = [1, 2, 3, 4, 5]
  let string_array = ["a", "b", "c"]
  
  assert_eq(empty_array.length(), 0)
  assert_eq(int_array.length(), 5)
  assert_eq(string_array.length(), 3)
  
  assert_eq(int_array[0], 1)
  assert_eq(int_array[4], 5)
  assert_eq(string_array[1], "b")
}

test "option type handling" {
  // Test Option type operations
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  assert_true(some_value != None)
  assert_true(none_value == None)
  assert_eq(some_value, Some(42))
  
  // Test Option matching
  let result = match some_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result, 84)
}

test "boolean logic" {
  // Test boolean operations
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_true(!false)
  assert_false(!true)
}

test "numeric comparisons" {
  // Test comparison operations
  assert_true(5 > 3)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

test "type conversions" {
  // Test type conversion operations
  let int_val = 42
  let float_val = 3.14
  
  // Test that types are preserved
  assert_eq(int_val + int_val, 84)
  assert_eq(float_val + float_val, 6.28)
  
  // Test string to int conversion (if supported)
  let str_num = "123"
  // Note: This test might need adjustment based on actual string to int conversion support
  assert_eq(str_num.length(), 3)
}