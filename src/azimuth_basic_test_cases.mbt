// Azimuth Telemetry System - Basic Test Cases
// This file contains basic test cases using fundamental MoonBit functionality

// Test 1: Basic arithmetic operations
test "basic arithmetic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(5 * 4, 20)
  assert_eq(10 / 2, 5)
  assert_eq(10 - 3, 7)
}

// Test 2: Boolean logic operations
test "boolean logic operations" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

// Test 3: Simple numeric comparisons
test "simple numeric comparisons" {
  assert_true(1 < 2)
  assert_true(2 > 1)
  assert_true(5 <= 5)
  assert_true(5 >= 5)
  assert_true(10 != 5)
  assert_eq(10, 10)
}

// Test 4: String concatenation
test "string concatenation" {
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  assert_eq(combined, "hello world")
}

// Test 5: Array operations
test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
}

// Test 6: Option type handling
test "option type handling" {
  let some_value = Some(42)
  let none_value = None
  
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Conditional expressions
test "conditional expressions" {
  let x = 10
  let result = if x > 5 { "greater" } else { "less" }
  assert_eq(result, "greater")
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less" }
  assert_eq(result2, "less")
}

// Test 8: Loop operations
test "loop operations" {
  let mut sum = 0
  for i in 0..=5 {
    sum = sum + i
  }
  assert_eq(sum, 15)
  
  let mut product = 1
  for i in 1..=4 {
    product = product * i
  }
  assert_eq(product, 24)
}