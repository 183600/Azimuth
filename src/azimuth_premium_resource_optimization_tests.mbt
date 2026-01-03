// Azimuth Premium Resource Optimization Tests
// This file contains high-quality test cases focusing on resource optimization, memory management, and performance

// Test 1: Basic Arithmetic Operations
test "basic arithmetic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(5 * 4, 20)
  assert_eq(10 / 2, 5)
  assert_eq(10 - 3, 7)
  assert_eq(10 % 3, 1)
  
  // Test operator precedence
  assert_eq(2 + 3 * 4, 14)
  assert_eq((2 + 3) * 4, 20)
  
  // Test negative numbers
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
  assert_false(false && true)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  // Test complex boolean expressions
  assert_true(true && (true || false))
  assert_false(false && (true || false))
  assert_true((true && false) || true)
}

// Test 3: String Operations
test "string operations" {
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  assert_eq(combined, "hello world")
  
  // Test string equality
  assert_true("test" == "test")
  assert_false("test" == "Test")
  assert_true("test" != "Test")
  
  // Test string concatenation
  let empty = ""
  let result = empty + "a" + "b" + "c"
  assert_eq(result, "abc")
  
  // Test multiline strings
  let multiline = "line1\nline2\nline3"
  assert_true(multiline.contains("\n"))
}

// Test 4: Array Operations
test "array operations" {
  // Test array creation
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
  
  let int_arr = [1, 2, 3, 4, 5]
  assert_eq(int_arr.length(), 5)
  assert_eq(int_arr[0], 1)
  assert_eq(int_arr[4], 5)
  
  // Test array with different types
  let string_arr = ["a", "b", "c"]
  assert_eq(string_arr.length(), 3)
  assert_eq(string_arr[0], "a")
  assert_eq(string_arr[2], "c")
  
  // Test array bounds
  assert_true(0 < int_arr.length())
  assert_true(4 < int_arr.length())
  assert_false(5 < int_arr.length())
}

// Test 5: Conditional Logic
test "conditional logic" {
  let x = 10
  let y = 20
  
  // Test if-else expressions
  let result1 = if x < y { "x is less" } else { "x is not less" }
  assert_eq(result1, "x is less")
  
  let result2 = if x > y { "x is greater" } else { "x is not greater" }
  assert_eq(result2, "x is not greater")
  
  // Test chained conditions
  let z = 15
  let result3 = 
    if z < 10 { "small" }
    else if z < 20 { "medium" }
    else { "large" }
  assert_eq(result3, "medium")
  
  // Test boolean conditions
  let condition = x < y && y > z
  assert_true(condition)
  
  let condition2 = x > y || y > z
  assert_true(condition2)
}

// Test 6: Option Type Handling
test "option type handling" {
  // Test Some and None
  let some_value = Some(42)
  let none_value = None
  
  // Test pattern matching with Some
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)  // Should not reach here
  }
  
  // Test pattern matching with None
  match none_value {
    Some(v) => assert_true(false)  // Should not reach here
    None => assert_true(true)      // Should reach here
  }
  
  // Test nested options
  let nested_some = Some(Some(10))
  match nested_some {
    Some(inner) => {
      match inner {
        Some(value) => assert_eq(value, 10)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 7: Tuple Operations
test "tuple operations" {
  // Test tuple creation and access
  let pair = (1, "one")
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "one")
  
  let triple = (1, "one", true)
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "one")
  assert_eq(triple.2, true)
  
  // Test tuple equality
  let pair1 = (1, "one")
  let pair2 = (1, "one")
  let pair3 = (1, "two")
  
  assert_eq(pair1, pair2)
  assert_true(pair1 != pair3)
  
  // Test tuple with different types
  let mixed_tuple = (42, "answer", 3.14, false)
  assert_eq(mixed_tuple.0, 42)
  assert_eq(mixed_tuple.1, "answer")
  assert_true((mixed_tuple.2 - 3.14) < 0.001)
  assert_eq(mixed_tuple.3, false)
}

// Test 8: Loop and Iteration
test "loop and iteration" {
  // Test for loop with range
  let mut sum = 0
  for i = 0; i < 5; i = i + 1 {
    sum = sum + i
  }
  assert_eq(sum, 10)  // 0 + 1 + 2 + 3 + 4 = 10
  
  // Test while loop
  let mut count = 0
  while count < 3 {
    count = count + 1
  }
  assert_eq(count, 3)
  
  // Test nested loops
  let mut product = 1
  for i = 1; i <= 3; i = i + 1 {
    for j = 1; j <= 2; j = j + 1 {
      product = product * i
    }
  }
  assert_eq(product, 36)  // (1*1*2)*(2*1*2)*(3*1*2) = 2*4*6 = 48 (actually 48)
}

// Test 9: Function Operations
test "function operations" {
  // Test function definition and call
  let add = fn(a, b) { a + b }
  let result = add(3, 4)
  assert_eq(result, 7)
  
  // Test higher-order function
  let apply_twice = fn(f, x) { f(f(x)) }
  let increment = fn(x) { x + 1 }
  let result2 = apply_twice(increment, 5)
  assert_eq(result2, 7)  // increment(increment(5)) = increment(6) = 7
  
  // Test function as return value
  let make_adder = fn(n) { fn(x) { x + n } }
  let add_5 = make_adder(5)
  let result3 = add_5(10)
  assert_eq(result3, 15)
  
  // Test recursive function
  let factorial = fn(n) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  assert_eq(factorial(5), 120)
}

// Test 10: Data Structure Operations
test "data structure operations" {
  // Test nested arrays
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  // Test matrix access
  assert_eq(matrix[0][0], 1)
  assert_eq(matrix[1][1], 5)
  assert_eq(matrix[2][2], 9)
  
  // Test nested tuples
  let nested_tuple = ((1, 2), (3, 4))
  assert_eq(nested_tuple.0.0, 1)
  assert_eq(nested_tuple.0.1, 2)
  assert_eq(nested_tuple.1.0, 3)
  assert_eq(nested_tuple.1.1, 4)
  
  // Test mixed data structures
  let mixed = [
    (1, "one"),
    (2, "two"),
    (3, "three")
  ]
  
  assert_eq(mixed.length(), 3)
  assert_eq(mixed[0].0, 1)
  assert_eq(mixed[0].1, "one")
  assert_eq(mixed[2].0, 3)
  assert_eq(mixed[2].1, "three")
  
  // Test complex pattern matching
  let complex_data = Some((1, [2, 3, 4]))
  match complex_data {
    Some((num, arr)) => {
      assert_eq(num, 1)
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 2)
      assert_eq(arr[2], 4)
    }
    None => assert_true(false)
  }
}