// Azimuth Premium Test Suite - High Quality MoonBit Language Features
// This file contains comprehensive test cases showcasing advanced MoonBit language features

// Test 1: Advanced Pattern Matching and Type Coercion
test "advanced pattern matching with type coercion" {
  // Test pattern matching with different numeric types
  let test_values = [42, 3.14, -10, 0.0, 100, -5.5]
  
  for value in test_values {
    let result = match value {
      x when x > 0 && x < 50 => "small positive"
      x when x >= 50 => "large positive" 
      x when x < 0 => "negative"
      _ => "zero"
    }
    
    // Validate pattern matching logic
    if value == 42 {
      assert_eq(result, "small positive")
    } else if value == 100 {
      assert_eq(result, "large positive")
    } else if value == -10 || value == -5.5 {
      assert_eq(result, "negative")
    } else if value == 0.0 {
      assert_eq(result, "zero")
    }
  }
  
  // Test nested pattern matching with option types
  let nested_options = [
    Some(Some(42)),
    Some(None),
    None,
    Some(Some(-10)),
    Some(Some(0))
  ]
  
  for opt in nested_options {
    let description = match opt {
      Some(Some(value)) when value > 0 => "positive nested value"
      Some(Some(value)) when value < 0 => "negative nested value"
      Some(Some(0)) => "zero nested value"
      Some(None) => "inner none"
      None => "outer none"
    }
    
    // Validate nested pattern matching
    match opt {
      Some(Some(42)) => assert_eq(description, "positive nested value")
      Some(None) => assert_eq(description, "inner none")
      None => assert_eq(description, "outer none")
      Some(Some(-10)) => assert_eq(description, "negative nested value")
      Some(Some(0)) => assert_eq(description, "zero nested value")
      _ => assert_true(false)
    }
  }
}

// Test 2: Higher-Order Functions and Functional Composition
test "higher-order functions and functional composition" {
  // Test function composition
  let add_one = |x| x + 1
  let double = |x| x * 2
  let square = |x| x * x
  
  // Compose functions: square(double(add_one(x)))
  let composed = |x| square(double(add_one(x)))
  
  assert_eq(composed(3), 16)  // (3 + 1) * 2 = 8, 8^2 = 64
  assert_eq(composed(5), 36)  // (5 + 1) * 2 = 12, 12^2 = 144
  
  // Test higher-order functions with collections
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Filter even numbers, then transform, then reduce
  let sum_of_squares_of_evens = numbers
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .reduce(|acc, val| acc + val, 0)
  
  assert_eq(sum_of_squares_of_evens, 220) // 4 + 16 + 36 + 64 + 100 = 220
  
  // Test currying and partial application
  let multiply = |x| |y| x * y
  let double = multiply(2)
  let triple = multiply(3)
  
  assert_eq(double(5), 10)
  assert_eq(triple(5), 15)
  
  // Test function as parameter
  let apply_operation = |x, operation| operation(x)
  
  assert_eq(apply_operation(10, double), 20)
  assert_eq(apply_operation(10, triple), 30)
  assert_eq(apply_operation(10, square), 100)
}

// Test 3: Advanced Type System Features
test "advanced type system features" {
  // Test type inference and generic operations
  let int_list = [1, 2, 3, 4, 5]
  let float_list = [1.1, 2.2, 3.3, 4.4, 5.5]
  let string_list = ["a", "b", "c", "d", "e"]
  
  // Generic operations on different types
  let int_sum = int_list.reduce(|acc, val| acc + val, 0)
  let float_sum = float_list.reduce(|acc, val| acc + val, 0.0)
  let string_concat = string_list.reduce(|acc, val| acc + val, "")
  
  assert_eq(int_sum, 15)
  assert_true(float_sum > 16.0 && float_sum < 17.0)
  assert_eq(string_concat, "abcde")
  
  // Test type constraints and bounds
  let process_numbers = |numbers| {
    let mut count = 0
    let mut sum = 0
    
    for num in numbers {
      count = count + 1
      sum = sum + num
    }
    
    if count > 0 {
      sum / count
    } else {
      0
    }
  }
  
  assert_eq(process_numbers([10, 20, 30]), 20)
  assert_eq(process_numbers([]), 0)
  assert_eq(process_numbers([5]), 5)
  
  // Test union types and variant handling
  let mixed_values = [1, 2.5, "hello", 3, 4.7, "world"]
  
  let type_counts = mixed_values.reduce(
    |acc, val| {
      match val {
        x when x is Int => { int_count: acc.int_count + 1, float_count: acc.float_count, string_count: acc.string_count }
        x when x is Float => { int_count: acc.int_count, float_count: acc.float_count + 1, string_count: acc.string_count }
        x when x is String => { int_count: acc.int_count, float_count: acc.float_count, string_count: acc.string_count + 1 }
        _ => acc
      }
    },
    { int_count: 0, float_count: 0, string_count: 0 }
  )
  
  assert_eq(type_counts.int_count, 2)
  assert_eq(type_counts.float_count, 2)
  assert_eq(type_counts.string_count, 2)
}

// Test 4: Mutable State and Reference Handling
test "mutable state and reference handling" {
  // Test mutable variables and scope
  let mut counter = 0
  
  // Create closure that captures mutable variable
  let incrementer = || {
    counter = counter + 1
    counter
  }
  
  assert_eq(incrementer(), 1)
  assert_eq(incrementer(), 2)
  assert_eq(incrementer(), 3)
  assert_eq(counter, 3)
  
  // Test mutable array operations
  let mut mutable_array = [1, 2, 3, 4, 5]
  
  // In-place transformation
  for i = 0; i < mutable_array.length(); i = i + 1 {
    mutable_array[i] = mutable_array[i] * 2
  }
  
  assert_eq(mutable_array, [2, 4, 6, 8, 10])
  
  // Test mutable record fields
  let mut point = { x: 10, y: 20 }
  
  point.x = point.x + 5
  point.y = point.y - 5
  
  assert_eq(point.x, 15)
  assert_eq(point.y, 15)
  
  // Test reference sharing and aliasing
  let data = [1, 2, 3, 4, 5]
  let reference1 = data
  let reference2 = data
  
  // All references should point to the same data
  assert_eq(reference1.length(), 5)
  assert_eq(reference2.length(), 5)
  assert_eq(reference1[0], 1)
  assert_eq(reference2[0], 1)
}

// Test 5: Advanced Control Flow and Exception Handling
test "advanced control flow and exception handling" {
  // Test early returns and guard clauses
  let process_value = |value| {
    // Guard clause for invalid input
    if value < 0 {
      return "invalid: negative value"
    }
    
    // Guard clause for zero
    if value == 0 {
      return "special case: zero"
    }
    
    // Normal processing
    if value % 2 == 0 {
      "even number"
    } else {
      "odd number"
    }
  }
  
  assert_eq(process_value(-5), "invalid: negative value")
  assert_eq(process_value(0), "special case: zero")
  assert_eq(process_value(4), "even number")
  assert_eq(process_value(7), "odd number")
  
  // Test try-catch style error handling with result types
  let divide_safely = |numerator, denominator| {
    if denominator == 0 {
      Err("Division by zero")
    } else {
      Ok(numerator / denominator)
    }
  }
  
  let valid_result = divide_safely(10, 2)
  let invalid_result = divide_safely(10, 0)
  
  match valid_result {
    Ok(value) => assert_eq(value, 5)
    Err(_) => assert_true(false)
  }
  
  match invalid_result {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "Division by zero")
  }
  
  // Test chained operations with error propagation
  let chained_operation = |input| {
    match divide_safely(input, 2) {
      Ok(step1) => match divide_safely(step1, 5) {
        Ok(step2) => step2 * 10
        Err(_) => -1  // Error in second division
      }
      Err(_) => -2  // Error in first division
    }
  }
  
  assert_eq(chained_operation(100), 100)  // 100/2=50, 50/5=10, 10*10=100
  assert_eq(chained_operation(5), -2)     // 5/2 fails in integer division
  assert_eq(chained_operation(0), -2)     // 0/2 fails in integer division
}

// Test 6: Advanced String Manipulation and Unicode Handling
test "advanced string manipulation and unicode handling" {
  // Test string slicing and indexing
  let test_string = "Hello, 世界! 🌍"
  
  // Test basic string operations
  assert_eq(test_string.length(), 14)  // Counting code points
  assert_eq(test_string.contains("Hello"), true)
  assert_eq(test_string.contains("世界"), true)
  assert_eq(test_string.contains("🌍"), true)
  
  // Test string transformations
  let uppercase = test_string.to_uppercase()
  let lowercase = test_string.to_lowercase()
  
  assert_eq(uppercase.contains("HELLO"), true)
  assert_eq(lowercase.contains("hello"), true)
  
  // Test string splitting and joining
  let csv_string = "apple,banana,cherry,date"
  let fruits = csv_string.split(",")
  
  assert_eq(fruits.length(), 4)
  assert_eq(fruits[0], "apple")
  assert_eq(fruits[1], "banana")
  assert_eq(fruits[2], "cherry")
  assert_eq(fruits[3], "date")
  
  let rejoined = fruits.join("|")
  assert_eq(rejoined, "apple|banana|cherry|date")
  
  // Test string trimming and padding
  let padded_string = "   trimmed content   "
  let trimmed = padded_string.trim()
  
  assert_eq(trimmed, "trimmed content")
  
  // Test string formatting and interpolation
  let name = "Azimuth"
  let version = "2.0"
  let formatted = "System: {name}, Version: {version}"
  
  assert_eq(formatted.contains(name), true)
  assert_eq(formatted.contains(version), true)
  
  // Test Unicode normalization and comparison
  let unicode_string1 = "café"
  let unicode_string2 = "cafe\u{0301}"  // Same character with combining accent
  
  // These should be equal after normalization
  assert_eq(unicode_string1.normalize(), unicode_string2.normalize())
}

// Test 7: Memory Management and Resource Cleanup
test "memory management and resource cleanup" {
  // Test resource acquisition and release patterns
  let create_resource = |id| {
    { resource_id: id, is_active: true }
  }
  
  let use_resource = |resource| {
    // Simulate resource usage
    assert_true(resource.is_active)
    resource.resource_id * 2
  }
  
  let cleanup_resource = |resource| {
    // Simulate resource cleanup
    { resource_id: resource.resource_id, is_active: false }
  }
  
  // Test resource lifecycle
  let resource = create_resource(42)
  let result = use_resource(resource)
  let cleaned_resource = cleanup_resource(resource)
  
  assert_eq(result, 84)
  assert_false(cleaned_resource.is_active)
  assert_eq(cleaned_resource.resource_id, 42)
  
  // Test memory-efficient operations with large datasets
  let large_dataset = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].repeat(1000)  // 10,000 elements
  
  // Process dataset in chunks to avoid memory issues
  let chunk_size = 1000
  let mut chunk_sum = 0
  let mut processed_chunks = 0
  
  for i = 0; i < large_dataset.length(); i = i + chunk_size {
    let end_index = if i + chunk_size > large_dataset.length() {
      large_dataset.length()
    } else {
      i + chunk_size
    }
    
    let chunk = large_dataset.slice(i, end_index)
    chunk_sum = chunk_sum + chunk.reduce(|acc, val| acc + val, 0)
    processed_chunks = processed_chunks + 1
  }
  
  assert_eq(processed_chunks, 10)
  assert_eq(chunk_sum, 55000)  // Sum of 1..10 repeated 1000 times
  
  // Test lazy evaluation and deferred computation
  let expensive_computation = || {
    // Simulate expensive operation
    [1, 2, 3, 4, 5].map(|x| x * x * x).reduce(|acc, val| acc + val, 0)
  }
  
  // Computation should only happen when called
  let lazy_result = expensive_computation
  assert_eq(lazy_result(), 225)  // 1^3 + 2^3 + 3^3 + 4^3 + 5^3 = 225
}

// Test 8: Advanced Numeric Operations and Precision
test "advanced numeric operations and precision" {
  // Test floating-point precision and comparison
  let precise_calculations = [
    (0.1 + 0.2, 0.3, "Basic floating point addition"),
    (1.0 - 0.9, 0.1, "Floating point subtraction"),
    (0.1 * 0.2, 0.02, "Floating point multiplication"),
    (1.0 / 3.0, 0.3333333333333333, "Floating point division")
  ]
  
  for (result, expected, description) in precise_calculations {
    // Use epsilon comparison for floating-point numbers
    let epsilon = 0.0001
    let diff = if result > expected { result - expected } else { expected - result }
    assert_true(diff < epsilon, description + " should be approximately equal")
  }
  
  // Test large number operations
  let large_numbers = [
    1000000, 2000000, 3000000, 4000000, 5000000
  ]
  
  let large_sum = large_numbers.reduce(|acc, val| acc + val, 0)
  let large_product = large_numbers.reduce(|acc, val| acc * val, 1)
  
  assert_eq(large_sum, 15000000)
  assert_eq(large_product, 12000000000000000000000)  // Very large number
  
  // Test numeric type conversions and boundaries
  let max_int = 2147483647
  let min_int = -2147483648
  
  // Test boundary conditions
  assert_eq(max_int + 1, min_int)  // Overflow should wrap around
  assert_eq(min_int - 1, max_int)  // Underflow should wrap around
  
  // Test scientific notation and very small numbers
  let scientific_numbers = [
    1.23e-10,  // Very small
    4.56e+20,  // Very large
    7.89e-5,   // Small decimal
    1.0e+0     // One in scientific notation
  ]
  
  for num in scientific_numbers {
    assert_true(num > 0.0)  // All should be positive
    assert_true(num.is_finite())  // All should be finite numbers
  }
  
  // Test bit manipulation operations
  let bit_operations = [
    (5 & 3, 1, "Bitwise AND"),
    (5 | 3, 7, "Bitwise OR"),
    (5 ^ 3, 6, "Bitwise XOR"),
    (5 << 2, 20, "Left shift"),
    (5 >> 1, 2, "Right shift"),
    (~5, -6, "Bitwise NOT")
  ]
  
  for (result, expected, description) in bit_operations {
    assert_eq(result, expected, description + " should match expected result")
  }
}

// Test 9: Advanced Collection Operations and Algorithms
test "advanced collection operations and algorithms" {
  // Test complex collection transformations
  let data = [
    { name: "Alice", age: 25, scores: [85, 90, 78] },
    { name: "Bob", age: 30, scores: [92, 88, 95] },
    { name: "Charlie", age: 35, scores: [78, 82, 80] },
    { name: "Diana", age: 28, scores: [95, 98, 92] }
  ]
  
  // Filter by age, then calculate average score, then sort by average
  let filtered_averages = data
    .filter(|person| person.age >= 25 && person.age <= 30)
    .map(|person| {
      let sum = person.scores.reduce(|acc, val| acc + val, 0)
      let avg = sum / person.scores.length()
      { name: person.name, average_score: avg }
    })
    .sort_by(|a, b| if a.average_score > b.average_score { 1 } else if a.average_score < b.average_score { -1 } else { 0 })
  
  assert_eq(filtered_averages.length(), 3)  // Alice, Bob, Diana
  
  // Alice: (85+90+78)/3 = 84.33
  // Diana: (95+98+92)/3 = 95.0
  // Bob: (92+88+95)/3 = 91.67
  
  assert_eq(filtered_averages[0].name, "Alice")    // Lowest average
  assert_eq(filtered_averages[2].name, "Diana")    // Highest average
  
  // Test grouping and aggregation
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let grouped_by_parity = numbers.reduce(
    |acc, val| {
      if val % 2 == 0 {
        { even: acc.even + [val], odd: acc.odd }
      } else {
        { even: acc.even, odd: acc.odd + [val] }
      }
    },
    { even: [], odd: [] }
  )
  
  assert_eq(grouped_by_parity.even, [2, 4, 6, 8, 10])
  assert_eq(grouped_by_parity.odd, [1, 3, 5, 7, 9])
  
  // Test advanced sorting algorithms
  let unsorted_data = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  
  // Implement bubble sort for testing
  let bubble_sort = |arr| {
    let mut sorted_arr = arr.copy()
    let n = sorted_arr.length()
    
    for i = 0; i < n; i = i + 1 {
      for j = 0; j < n - i - 1; j = j + 1 {
        if sorted_arr[j] > sorted_arr[j + 1] {
          let temp = sorted_arr[j]
          sorted_arr[j] = sorted_arr[j + 1]
          sorted_arr[j + 1] = temp
        }
      }
    }
    
    sorted_arr
  }
  
  let sorted_data = bubble_sort(unsorted_data)
  
  // Verify sorting
  for i = 0; i < sorted_data.length() - 1; i = i + 1 {
    assert_true(sorted_data[i] <= sorted_data[i + 1])
  }
  
  assert_eq(sorted_data[0], 11)   // Minimum
  assert_eq(sorted_data[sorted_data.length() - 1], 90)  // Maximum
  
  // Test set operations
  let set1 = [1, 2, 3, 4, 5]
  let set2 = [4, 5, 6, 7, 8]
  
  // Union
  let union = set1 + set2.filter(|x| !set1.contains(x))
  assert_eq(union.sort(), [1, 2, 3, 4, 5, 6, 7, 8])
  
  // Intersection
  let intersection = set1.filter(|x| set2.contains(x))
  assert_eq(intersection.sort(), [4, 5])
  
  // Difference
  let difference = set1.filter(|x| !set2.contains(x))
  assert_eq(difference.sort(), [1, 2, 3])
}

// Test 10: Advanced Error Recovery and Resilience Patterns
test "advanced error recovery and resilience patterns" {
  // Test circuit breaker pattern
  let mut circuit_state = "closed"
  let mut failure_count = 0
  let mut last_failure_time = 0
  let failure_threshold = 3
  let recovery_timeout = 60000  // 1 minute in milliseconds
  
  let circuit_breaker = |operation| {
    let current_time = 1640995200000L  // Mock current time
    
    match circuit_state {
      "open" when current_time - last_failure_time > recovery_timeout => {
        // Try to close circuit
        circuit_state = "half-open"
        operation()
      }
      "open" => {
        Err("Circuit is open")
      }
      "half-open" => {
        match operation() {
          Ok(result) => {
            circuit_state = "closed"
            failure_count = 0
            Ok(result)
          }
          Err(e) => {
            circuit_state = "open"
            last_failure_time = current_time
            Err(e)
          }
        }
      }
      "closed" => {
        match operation() {
          Ok(result) => Ok(result)
          Err(e) => {
            failure_count = failure_count + 1
            if failure_count >= failure_threshold {
              circuit_state = "open"
              last_failure_time = current_time
            }
            Err(e)
          }
        }
      }
      _ => Err("Unknown circuit state")
    }
  }
  
  // Test circuit breaker behavior
  let failing_operation = || {
    failure_count = failure_count + 1
    if failure_count < 4 {
      Err("Operation failed")
    } else {
      Ok("Success after failures")
    }
  }
  
  // First few failures
  assert_eq(circuit_breaker(failing_operation).is_err(), true)
  assert_eq(circuit_breaker(failing_operation).is_err(), true)
  assert_eq(circuit_breaker(failing_operation).is_err(), true)
  
  // Circuit should now be open
  assert_eq(circuit_state, "open")
  assert_eq(circuit_breaker(failing_operation).is_err(), true)
  
  // Test retry pattern with exponential backoff
  let retry_with_backoff = |operation, max_retries| {
    let mut attempt = 0
    let mut delay = 1000  // Start with 1 second delay
    
    while attempt < max_retries {
      match operation() {
        Ok(result) => return Ok(result),
        Err(e) => {
          attempt = attempt + 1
          if attempt >= max_retries {
            return Err(e)
          }
          // In real implementation, would wait for delay
          delay = delay * 2  // Exponential backoff
        }
      }
    }
    
    Err("Max retries exceeded")
  }
  
  let mut attempt_count = 0
  let eventual_success_operation = || {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  }
  
  let retry_result = retry_with_backoff(eventual_success_operation, 5)
  assert_eq(retry_result.is_ok(), true)
  
  // Test timeout pattern
  let with_timeout = |operation, timeout_ms| {
    let start_time = 1640995200000L  // Mock start time
    let current_time = 1640995200000L  // Mock current time (no passage of time in test)
    
    if current_time - start_time > timeout_ms {
      Err("Operation timed out")
    } else {
      operation()
    }
  }
  
  let quick_operation = || Ok("Quick result")
  let timeout_result = with_timeout(quick_operation, 1000)
  assert_eq(timeout_result.is_ok(), true)
  
  // Test fallback pattern
  let with_fallback = |primary_operation, fallback_operation| {
    match primary_operation() {
      Ok(result) => Ok(result),
      Err(_) => fallback_operation()
    }
  }
  
  let failing_primary = || Err("Primary operation failed")
  let working_fallback = || Ok("Fallback result")
  
  let fallback_result = with_fallback(failing_primary, working_fallback)
  assert_eq(fallback_result.is_ok(), true)
  assert_eq(fallback_result.unwrap(), "Fallback result")
}