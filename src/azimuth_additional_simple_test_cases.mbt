// Azimuth Telemetry System - Additional Simple Test Cases
// This file contains additional simple test cases using basic MoonBit functionality

// Test 1: Advanced arithmetic operations
test "advanced arithmetic operations" {
  // Test exponentiation
  assert_eq(2^3, 8)
  assert_eq(5^2, 25)
  assert_eq(10^0, 1)
  
  // Test modulo operations
  assert_eq(10 % 3, 1)
  assert_eq(15 % 5, 0)
  assert_eq(7 % 4, 3)
  
  // Test negative number operations
  assert_eq(-5 + 10, 5)
  assert_eq(-3 * -3, 9)
  assert_eq(-10 / 2, -5)
}

// Test 2: String operations and comparisons
test "string operations and comparisons" {
  // Test string concatenation
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  assert_eq(full_name, "John Doe")
  
  // Test string length
  assert_eq(first_name.length(), 4)
  assert_eq(last_name.length(), 3)
  assert_eq(full_name.length(), 8)
  
  // Test string comparisons
  assert_true("apple" < "banana")
  assert_true("xyz" > "abc")
  assert_eq("test", "test")
  assert_false("hello" == "world")
}

// Test 3: Array manipulations
test "array manipulations" {
  // Test array creation and access
  let numbers = [10, 20, 30, 40, 50]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 10)
  assert_eq(numbers[4], 50)
  
  // Test array with different types
  let mixed = ["hello", 42, true]
  assert_eq(mixed.length(), 3)
  assert_eq(mixed[0], "hello")
  assert_eq(mixed[1], 42)
  assert_eq(mixed[2], true)
  
  // Test empty array
  let empty = []
  assert_eq(empty.length(), 0)
}

// Test 4: Boolean logic and conditional expressions
test "boolean logic and conditional expressions" {
  // Test complex boolean expressions
  assert_true(true && (true || false))
  assert_false(false && (true || false))
  assert_true((true && false) || true)
  
  // Test negation
  assert_true(!false)
  assert_false(!true)
  assert_true(!(!true))
  
  // Test conditional expressions
  let x = 15
  let result = if x > 10 { "greater than 10" } else { "less than or equal to 10" }
  assert_eq(result, "greater than 10")
  
  let y = 5
  let result2 = if y > 10 { "greater than 10" } else { "less than or equal to 10" }
  assert_eq(result2, "less than or equal to 10")
}

// Test 5: Option type handling
test "option type handling" {
  // Test Some value
  let some_number = Some(42)
  match some_number {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false) // This should not execute
  }
  
  // Test None value
  let none_value = None
  match none_value {
    Some(value) => assert_true(false) // This should not execute
    None => assert_true(true) // This should execute
  }
  
  // Test nested option
  let nested_some = Some(Some(10))
  match nested_some {
    Some(inner_opt) => {
      match inner_opt {
        Some(value) => assert_eq(value, 10)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 6: Loop operations
test "loop operations" {
  // Test for loop with range
  let mut sum = 0
  for i in 0..=5 {
    sum = sum + i
  }
  assert_eq(sum, 15) // 0+1+2+3+4+5 = 15
  
  // Test for loop with array
  let arr = [2, 4, 6, 8, 10]
  let mut product = 1
  for i in 0..<arr.length() {
    product = product * arr[i]
  }
  assert_eq(product, 3840) // 2*4*6*8*10 = 3840
  
  // Test while loop
  let mut counter = 10
  let mut iterations = 0
  while counter > 0 {
    counter = counter - 1
    iterations = iterations + 1
  }
  assert_eq(iterations, 10)
  assert_eq(counter, 0)
}

// Test 7: Function definitions and calls
test "function definitions and calls" {
  // Define a simple addition function
  let add = fn(a, b) { a + b }
  assert_eq(add(5, 7), 12)
  assert_eq(add(-3, 10), 7)
  
  // Define a multiplication function
  let multiply = fn(a, b) { a * b }
  assert_eq(multiply(4, 5), 20)
  assert_eq(multiply(-2, -3), 6)
  
  // Define a function with conditional logic
  let absolute = fn(x) {
    if x >= 0 {
      x
    } else {
      -x
    }
  }
  assert_eq(absolute(5), 5)
  assert_eq(absolute(-5), 5)
  assert_eq(absolute(0), 0)
}

// Test 8: Pattern matching with different types
test "pattern matching with different types" {
  // Test pattern matching with integers
  let describe_number = fn(x) {
    match x {
      0 => "zero"
      1 => "one"
      2 => "two"
      _ => "many"
    }
  }
  assert_eq(describe_number(0), "zero")
  assert_eq(describe_number(1), "one")
  assert_eq(describe_number(2), "two")
  assert_eq(describe_number(5), "many")
  
  // Test pattern matching with tuples
  let describe_point = fn(point) {
    match point {
      (0, 0) => "origin"
      (x, 0) => "on x-axis"
      (0, y) => "on y-axis"
      (x, y) => "other point"
    }
  }
  assert_eq(describe_point((0, 0)), "origin")
  assert_eq(describe_point((5, 0)), "on x-axis")
  assert_eq(describe_point((0, 3)), "on y-axis")
  assert_eq(describe_point((2, 4)), "other point")
}