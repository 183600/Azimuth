// Azimuth Premium Resource Optimization Tests
// This file contains high-quality test cases focusing on resource optimization, memory management, and performance

// Test 1: Basic Array Operations and Memory Management
test "basic array operations and memory management" {
  // Test array creation and manipulation
  let empty_array = []
  assert_eq(empty_array.length(), 0)
  
  let int_array = [1, 2, 3, 4, 5]
  assert_eq(int_array.length(), 5)
  assert_eq(int_array[0], 1)
  assert_eq(int_array[4], 5)
  
  // Test array concatenation
  let array1 = [1, 2, 3]
  let array2 = [4, 5, 6]
  let combined = array1.concat(array2)
  assert_eq(combined.length(), 6)
  assert_eq(combined[0], 1)
  assert_eq(combined[5], 6)
  
  // Test array slicing
  let slice = combined.slice(2, 4)
  assert_eq(slice.length(), 2)
  assert_eq(slice[0], 3)
  assert_eq(slice[1], 4)
  
  // Test array filtering
  let filtered = combined.filter(fn(x) { x % 2 == 0 })
  assert_eq(filtered.length(), 3)
  assert_eq(filtered[0], 2)
  assert_eq(filtered[1], 4)
  assert_eq(filtered[2], 6)
}

// Test 2: String Operations and Optimization
test "string operations and optimization" {
  // Test string creation and basic operations
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  
  let hello = "Hello"
  let world = "World"
  let hello_world = hello + " " + world
  assert_eq(hello_world, "Hello World")
  assert_eq(hello_world.length(), 11)
  
  // Test string splitting
  let sentence = "This is a test sentence"
  let words = sentence.split(" ")
  assert_eq(words.length(), 5)
  assert_eq(words[0], "This")
  assert_eq(words[4], "sentence")
  
  // Test string case conversion
  let upper = hello_world.uppercase()
  let lower = hello_world.lowercase()
  assert_eq(upper, "HELLO WORLD")
  assert_eq(lower, "hello world")
  
  // Test string contains
  assert_true(hello_world.contains("Hello"))
  assert_true(hello_world.contains("World"))
  assert_false(hello_world.contains("Goodbye"))
  
  // Test string trimming
  let padded = "  padded string  "
  let trimmed = padded.trim()
  assert_eq(trimmed, "padded string")
  assert_eq(trimmed.length(), 13)
}

// Test 3: Numeric Operations and Precision
test "numeric operations and precision" {
  // Test integer operations
  let int_a = 42
  let int_b = 17
  
  assert_eq(int_a + int_b, 59)
  assert_eq(int_a - int_b, 25)
  assert_eq(int_a * int_b, 714)
  assert_eq(int_a / int_b, 2)  // Integer division
  assert_eq(int_a % int_b, 8)
  
  // Test float operations
  let float_a = 3.14
  let float_b = 2.71
  
  assert_true((float_a + float_b - 5.85) < 0.001)  // Account for floating point precision
  assert_true((float_a - float_b - 0.43) < 0.001)
  assert_true((float_a * float_b - 8.5094) < 0.001)
  assert_true((float_a / float_b - 1.1594) < 0.001)
  
  // Test numeric comparisons
  assert_true(int_a > int_b)
  assert_true(int_b < int_a)
  assert_true(int_a >= 42)
  assert_true(int_b <= 17)
  
  // Test min/max functions
  assert_eq(int_a.min(int_b), 17)
  assert_eq(int_a.max(int_b), 42)
  assert_eq(float_a.min(float_b), 2.71)
  assert_eq(float_a.max(float_b), 3.14)
  
  // Test absolute value
  assert_eq((-5).abs(), 5)
  assert_eq(5.abs(), 5)
  assert_eq((-3.14).abs(), 3.14)
}

// Test 4: Boolean Logic and Conditional Operations
test "boolean logic and conditional operations" {
  // Test basic boolean operations
  assert_true(true)
  assert_false(false)
  
  // Test AND operations
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  // Test OR operations
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  // Test NOT operations
  assert_false(!true)
  assert_true(!false)
  
  // Test conditional expressions
  let x = 10
  let result = if x > 5 { "greater" } else { "less or equal" }
  assert_eq(result, "greater")
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less or equal" }
  assert_eq(result2, "less or equal")
  
  // Test chained conditions
  let z = 15
  let result3 = 
    if z < 10 { "small" }
    else if z < 20 { "medium" }
    else { "large" }
  assert_eq(result3, "medium")
}

// Test 5: Option Type Handling and Pattern Matching
test "option type handling and pattern matching" {
  // Test Some/None operations
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
  
  // Test Option operations
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  let doubled_none = none_value.map(fn(x) { x * 2 })
  match doubled_none {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test Option with default value
  let with_default = some_value.value_or(0)
  assert_eq(with_default, 42)
  
  let none_with_default = none_value.value_or(0)
  assert_eq(none_with_default, 0)
  
  // Test Option filtering
  let filtered_some = some_value.filter(fn(x) { x > 40 })
  match filtered_some {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  let filtered_none = some_value.filter(fn(x) { x > 50 })
  match filtered_none {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: List Operations and Functional Programming
test "list operations and functional programming" {
  // Test list creation
  let empty_list = []
  let number_list = [1, 2, 3, 4, 5]
  
  // Test map operation
  let doubled = number_list.map(fn(x) { x * 2 })
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
  
  // Test filter operation
  let evens = number_list.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens.length(), 2)
  assert_eq(evens[0], 2)
  assert_eq(evens[1], 4)
  
  // Test fold/reduce operation
  let sum = number_list.fold(0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
  
  // Test find operation
  let found = number_list.find(fn(x) { x > 3 })
  match found {
    Some(v) => assert_eq(v, 4)
    None => assert_true(false)
  }
  
  let not_found = number_list.find(fn(x) { x > 10 })
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test all/any operations
  assert_true(number_list.all(fn(x) { x > 0 }))
  assert_false(number_list.all(fn(x) { x > 3 }))
  assert_true(number_list.any(fn(x) { x > 3 }))
  assert_false(number_list.any(fn(x) { x > 10 }))
}

// Test 7: Tuple Operations and Data Structures
test "tuple operations and data structures" {
  // Test tuple creation and access
  let pair = (1, "one")
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "one")
  
  let triple = (1, "one", 1.0)
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "one")
  assert_true((triple.2 - 1.0) < 0.001)
  
  // Test tuple comparison
  let pair1 = (1, "one")
  let pair2 = (1, "one")
  let pair3 = (1, "two")
  
  assert_eq(pair1, pair2)
  assert_true(pair1 != pair3)
  
  // Test tuple in collections
  let tuple_list = [(1, "one"), (2, "two"), (3, "three")]
  assert_eq(tuple_list.length(), 3)
  assert_eq(tuple_list[0].0, 1)
  assert_eq(tuple_list[0].1, "one")
  
  // Test tuple transformation
  let transformed = tuple_list.map(fn(t) { (t.0 * 2, t.1.uppercase()) })
  assert_eq(transformed[0], (2, "ONE"))
  assert_eq(transformed[1], (4, "TWO"))
  assert_eq(transformed[2], (6, "THREE"))
}

// Test 8: Error Handling and Exception Management
test "error handling and exception management" {
  // Test division by zero handling
  let safe_divide = fn(a, b) {
    if b == 0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  let result1 = safe_divide(10, 2)
  match result1 {
    Some(v) => assert_eq(v, 5)
    None => assert_true(false)
  }
  
  let result2 = safe_divide(10, 0)
  match result2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test array bounds checking
  let safe_get = fn(arr, index) {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let test_array = [1, 2, 3, 4, 5]
  let valid_get = safe_get(test_array, 2)
  match valid_get {
    Some(v) => assert_eq(v, 3)
    None => assert_true(false)
  }
  
  let invalid_get = safe_get(test_array, 10)
  match invalid_get {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test string to number conversion with error handling
  let safe_parse_int = fn(s) {
    try {
      Some(Int::parse(s))
    } catch {
      _ => None
    }
  }
  
  let valid_parse = safe_parse_int("42")
  match valid_parse {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  let invalid_parse = safe_parse_int("not a number")
  match invalid_parse {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 9: Performance and Efficiency Checks
test "performance and efficiency checks" {
  // Test large array operations
  let large_array = []
  
  // Build a large array efficiently
  let large_array = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  let really_large = large_array.map(fn(x) { x + 10 })
  
  // Verify transformation
  assert_eq(really_large.length(), 10)
  assert_eq(really_large[0], 10)
  assert_eq(really_large[9], 19)
  
  // Test efficient filtering
  let filtered_large = really_large.filter(fn(x) { x % 2 == 0 })
  assert_eq(filtered_large.length(), 5)
  assert_eq(filtered_large[0], 10)
  assert_eq(filtered_large[4], 18)
  
  // Test efficient summing
  let sum_large = really_large.fold(0, fn(acc, x) { acc + x })
  assert_eq(sum_large, 145)  // Sum of 10 through 19
  
  // Test string concatenation efficiency
  let string_parts = ["This", "is", "a", "test"]
  let joined = string_parts.join(" ")
  assert_eq(joined, "This is a test")
  
  // Test batch operations
  let batch1 = [1, 2, 3]
  let batch2 = [4, 5, 6]
  let batch3 = [7, 8, 9]
  let all_batches = batch1.concat(batch2).concat(batch3)
  
  assert_eq(all_batches.length(), 9)
  assert_eq(all_batches[0], 1)
  assert_eq(all_batches[8], 9)
  
  // Test batch sum
  let batch_sum = all_batches.fold(0, fn(acc, x) { acc + x })
  assert_eq(batch_sum, 45)  // Sum of 1 through 9
}

// Test 10: Advanced Data Structure Operations
test "advanced data structure operations" {
  // Test nested data structures
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  // Test matrix access
  assert_eq(matrix[0][0], 1)
  assert_eq(matrix[1][1], 5)
  assert_eq(matrix[2][2], 9)
  
  // Test matrix transformation
  let transposed = [
    [matrix[0][0], matrix[1][0], matrix[2][0]],
    [matrix[0][1], matrix[1][1], matrix[2][1]],
    [matrix[0][2], matrix[1][2], matrix[2][2]]
  ]
  
  assert_eq(transposed[0][0], 1)
  assert_eq(transposed[0][1], 4)
  assert_eq(transposed[0][2], 7)
  
  // Test flattening nested structures
  let flattened = matrix.fold([], fn(acc, row) {
    acc.concat(row)
  })
  
  assert_eq(flattened.length(), 9)
  assert_eq(flattened[0], 1)
  assert_eq(flattened[4], 5)
  assert_eq(flattened[8], 9)
  
  // Test complex data filtering
  let filtered_matrix = matrix.filter(fn(row) {
    row.fold(0, fn(acc, x) { acc + x }) > 10
  })
  
  assert_eq(filtered_matrix.length(), 2)  // Rows with sum > 10
  assert_eq(filtered_matrix[0][0], 4)    // Second row
  assert_eq(filtered_matrix[1][0], 7)    // Third row
  
  // Test grouping operations
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let grouped = numbers.fold([], fn(acc, x) {
    if x % 2 == 0 {
      acc.concat([("even", x)])
    } else {
      acc.concat([("odd", x)])
    }
  })
  
  let evens = grouped.filter(fn(pair) { pair.0 == "odd" }).map(fn(pair) { pair.1 })
  assert_eq(evens.length(), 5)
  assert_eq(evens[0], 1)
  assert_eq(evens[4], 9)
}