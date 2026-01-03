// Azimuth Telemetry System - Simple Test Cases
// This file contains simple test cases using basic MoonBit functionality

// Test 1: Basic arithmetic and numeric operations
test "basic arithmetic and numeric operations" {
  // Test addition, subtraction, multiplication, division
  assert_eq(1 + 2, 3)
  assert_eq(10 - 5, 5)
  assert_eq(3 * 4, 12)
  assert_eq(20 / 4, 5)
  
  // Test modulo operation
  assert_eq(10 % 3, 1)
  assert_eq(15 % 5, 0)
  
  // Test exponentiation
  assert_eq(2^3, 8)
  assert_eq(5^2, 25)
  
  // Test negative numbers
  assert_eq(-5 + 10, 5)
  assert_eq(-3 * -3, 9)
}

// Test 2: String manipulation and operations
test "string manipulation and operations" {
  // Test string concatenation
  let str1 = "hello"
  let str2 = "world"
  assert_eq(str1 + " " + str2, "hello world")
  
  // Test string length
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq("hello world".length(), 11)
  
  // Test string comparison
  assert_true("apple" < "banana")
  assert_true("xyz" > "abc")
  assert_eq("test", "test")
  
  // Test substring operations
  let long_str = "abcdefghijklmnopqrstuvwxyz"
  assert_eq(long_str.substring(0, 5), "abcde")
  assert_eq(long_str.substring(10, 15), "klmno")
}

// Test 3: Array operations and manipulations
test "array operations and manipulations" {
  // Test array creation and access
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  // Test array concatenation
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let combined = arr1 + arr2
  assert_eq(combined.length(), 6)
  assert_eq(combined[3], 4)
  
  // Test empty array
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
  
  // Test array slicing
  let sliced = arr.slice(1, 4)
  assert_eq(sliced.length(), 3)
  assert_eq(sliced[0], 2)
}

// Test 4: Boolean logic and conditional operations
test "boolean logic and conditional operations" {
  // Test basic boolean values
  assert_true(true)
  assert_false(false)
  
  // Test logical operations
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  // Test negation
  assert_true(!false)
  assert_false(!true)
  
  // Test conditional expressions
  let x = 10
  let result = if x > 5 { "greater" } else { "less" }
  assert_eq(result, "greater")
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less" }
  assert_eq(result2, "less")
}

// Test 5: Option type handling
test "option type handling" {
  // Test Some value
  let some_value = Some(42)
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  // Test None value
  let none_value = None
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test Option operations
  let opt1 = Some(10)
  let opt2 = None
  
  assert_true(Option::is_some(opt1))
  assert_false(Option::is_none(opt1))
  
  assert_false(Option::is_some(opt2))
  assert_true(Option::is_none(opt2))
  
  // Test Option map
  let mapped = Option::map(opt1, fn(x) { x * 2 })
  match mapped {
    Some(v) => assert_eq(v, 20)
    None => assert_true(false)
  }
}

// Test 6: Loop and iteration operations
test "loop and iteration operations" {
  // Test for loop with range
  let mut sum = 0
  for i in 0..=5 {
    sum = sum + i
  }
  assert_eq(sum, 15) // 0+1+2+3+4+5 = 15
  
  // Test for loop with array
  let arr = [1, 2, 3, 4, 5]
  let mut product = 1
  for i in 0..<arr.length() {
    product = product * arr[i]
  }
  assert_eq(product, 120) // 1*2*3*4*5 = 120
  
  // Test while loop
  let mut counter = 0
  while counter < 5 {
    counter = counter + 1
  }
  assert_eq(counter, 5)
}

// Test 7: Function definitions and calls
test "function definitions and calls" {
  // Define a simple function
  let add = fn(a, b) { a + b }
  assert_eq(add(3, 4), 7)
  
  // Define a recursive function
  let factorial = fn(n) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  assert_eq(factorial(5), 120)
  assert_eq(factorial(1), 1)
  
  // Define a higher-order function
  let apply_twice = fn(f, x) { f(f(x)) }
  let double = fn(x) { x * 2 }
  assert_eq(apply_twice(double, 3), 12) // double(double(3)) = double(6) = 12
}

// Test 8: Pattern matching
test "pattern matching" {
  // Test pattern matching with integers
  let match_int = fn(x) {
    match x {
      0 => "zero"
      1 => "one"
      2 => "two"
      _ => "many"
    }
  }
  assert_eq(match_int(0), "zero")
  assert_eq(match_int(1), "one")
  assert_eq(match_int(2), "two")
  assert_eq(match_int(5), "many")
  
  // Test pattern matching with tuples
  let match_tuple = fn(t) {
    match t {
      (0, 0) => "origin"
      (x, 0) => "on x-axis"
      (0, y) => "on y-axis"
      (x, y) => "other"
    }
  }
  assert_eq(match_tuple((0, 0)), "origin")
  assert_eq(match_tuple((5, 0)), "on x-axis")
  assert_eq(match_tuple((0, 3)), "on y-axis")
  assert_eq(match_tuple((2, 3)), "other")
}