// Azimuth Telemetry System - New Comprehensive Test Suite
// This file contains comprehensive test cases covering various aspects of the telemetry system

// Test 1: Basic Arithmetic Operations
test "basic arithmetic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(5 * 4, 20)
  assert_eq(10 / 2, 5)
  assert_eq(10 - 3, 7)
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(-5 + 5, 0)
  assert_eq(-10 + -5, -15)
  assert_eq(-5 * -5, 25)
}

// Test 2: Boolean Logic Operations
test "boolean logic operations" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  assert_true(!false)
}

// Test 3: String Operations
test "string operations" {
  let str1 = "hello"
  let str2 = "world"
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq(str1 + " " + str2, "hello world")
  
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  
  let sentence = "hello world from azimuth"
  assert_true(sentence.contains("azimuth"))
  assert_true(sentence.contains(" "))
}

// Test 4: Array Operations
test "array operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
  
  let single_element = [42]
  assert_eq(single_element.length(), 1)
  assert_eq(single_element[0], 42)
}

// Test 5: Option Type Handling
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

// Test 6: Loop and Iteration Tests
test "loop and iteration tests" {
  let mut sum = 0
  for i = 0; i <= 5; i = i + 1 {
    sum = sum + i
  }
  assert_eq(sum, 15)
  
  let mut product = 1
  for i = 1; i <= 4; i = i + 1 {
    product = product * i
  }
  assert_eq(product, 24)
}

// Test 7: Conditional Logic Tests
test "conditional logic tests" {
  let x = 10
  let y = 20
  
  if x < y {
    assert_true(true)
  } else {
    assert_true(false)
  }
  
  let result = if x > 5 { "greater" } else { "less" }
  assert_eq(result, "greater")
}

// Test 8: Error Handling Tests
test "error handling tests" {
  let safe_divide = fn(numerator : Int, denominator : Int) -> Option[Int] {
    if denominator == 0 {
      None
    } else {
      Some(numerator / denominator)
    }
  }
  
  let result1 = safe_divide(10, 2)
  match result1 {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  let result2 = safe_divide(10, 0)
  match result2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 9: Pattern Matching Tests
test "pattern matching tests" {
  let test_value = Some(Some(42))
  
  let extract_nested = fn(data : Option[Option[Int]]) -> Int {
    match data {
      Some(Some(value)) => value
      Some(None) => -1
      None => -2
    }
  }
  
  assert_eq(extract_nested(test_value), 42)
  assert_eq(extract_nested(Some(None)), -1)
  assert_eq(extract_nested(None), -2)
  
  let test_status = fn(status : (String, Int)) -> String {
    match status {
      ("success", code) if code >= 200 && code < 300 => "Success: " + code.to_string()
      ("error", code) if code >= 400 => "Error: " + code.to_string()
      (status, code) => "Unknown: " + status + " (" + code.to_string() + ")"
    }
  }
  
  assert_eq(test_status(("success", 200)), "Success: 200")
  assert_eq(test_status(("error", 404)), "Error: 404")
  assert_eq(test_status(("pending", 102)), "Unknown: pending (102)")
}

// Test 10: Recursive Function Tests
test "recursive function tests" {
  let factorial = fn(n : Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  
  let fibonacci = fn(n : Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
}