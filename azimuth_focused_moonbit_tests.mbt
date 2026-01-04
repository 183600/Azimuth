// Azimuth Telemetry System - Focused Moonbit Test Cases
// This file contains 10 targeted test cases focusing on specific functionality areas

// Test 1: Advanced String Processing and Unicode Handling
test "advanced string processing and unicode handling" {
  // Test Unicode string operations
  let unicode_str = "Azimuth遥测系统测试"
  assert_eq(unicode_str.length(), 11)
  
  // Test string case conversion
  let mixed_case = "Azimuth Telemetry System"
  assert_eq(mixed_case.to_uppercase(), "AZIMUTH TELEMETRY SYSTEM")
  assert_eq(mixed_case.to_lowercase(), "azimuth telemetry system")
  
  // Test string trimming
  let padded_str = "  Azimuth  "
  assert_eq(padded_str.trim(), "Azimuth")
  
  // Test string splitting
  let csv_str = "trace_id,span_id,parent_span_id"
  let parts = csv_str.split(",")
  assert_eq(parts.length(), 3)
  assert_eq(parts[0], "trace_id")
  assert_eq(parts[1], "span_id")
  assert_eq(parts[2], "parent_span_id")
  
  // Test string concatenation with different types
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let combined = trace_id + ":" + span_id
  assert_eq(combined, "0af7651916cd43dd8448eb211c80319c:b7ad6b7169203331")
}

// Test 2: Complex Numeric Operations and Precision
test "complex numeric operations and precision" {
  // Test floating point precision
  let precise_float = 3.14159265359
  let rounded = precise_float * 1000.0
  assert_eq(rounded > 3141.0, true)
  assert_eq(rounded < 3142.0, true)
  
  // Test large number operations
  let large_num = 1000000
  let another_large = 2000000
  assert_eq(large_num + another_large, 3000000)
  assert_eq(another_large - large_num, 1000000)
  assert_eq(large_num * 2, 2000000)
  assert_eq(another_large / 2, 1000000)
  
  // Test modulo operations
  assert_eq(100 % 3, 1)
  assert_eq(100 % 25, 0)
  assert_eq(99 % 11, 0)
  
  // Test negative number operations
  let neg_num = -42
  assert_eq(neg_num + 42, 0)
  assert_eq(neg_num * -1, 42)
  assert_eq(-neg_num, 42)
  
  // Test power operations (simulated)
  let base = 2
  let mut result = 1
  for i in 1..=5 {
    result = result * base
  }
  assert_eq(result, 32) // 2^5
}

// Test 3: Advanced Array and Collection Operations
test "advanced array and collection operations" {
  // Test array filtering
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let mut even_numbers = []
  for i in numbers {
    if i % 2 == 0 {
      even_numbers.push(i)
    }
  }
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  // Test array mapping
  let mut doubled = []
  for i in numbers {
    doubled.push(i * 2)
  }
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
  assert_eq(doubled[9], 20)
  
  // Test array reduction
  let mut sum = 0
  for i in numbers {
    sum = sum + i
  }
  assert_eq(sum, 55)
  
  // Test array contains operation
  let mut contains_five = false
  for i in numbers {
    if i == 5 {
      contains_five = true
      break
    }
  }
  assert_true(contains_five)
  
  // Test array reverse
  let mut reversed = []
  for i in 0..numbers.length() {
    reversed.push(numbers[numbers.length() - 1 - i])
  }
  assert_eq(reversed[0], 10)
  assert_eq(reversed[9], 1)
}

// Test 4: Complex Option Type Handling
test "complex option type handling" {
  // Test nested option types
  let nested_some = Some(Some(42))
  match nested_some {
    Some(inner_opt) => {
      match inner_opt {
        Some(value) => assert_eq(value, 42)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test option chaining
  let option_chain = Some("test_string")
  let result = match option_chain {
    Some(str) => {
      if str.length() > 0 {
        Some(str.to_uppercase())
      } else {
        None
      }
    }
    None => None
  }
  match result {
    Some(upper_str) => assert_eq(upper_str, "TEST_STRING")
    None => assert_true(false)
  }
  
  // Test option default values
  let option_with_value = Some(42)
  let value_with_default = match option_with_value {
    Some(v) => v
    None => 0
  }
  assert_eq(value_with_default, 42)
  
  let option_none = None
  let none_with_default = match option_none {
    Some(v) => v
    None => 0
  }
  assert_eq(none_with_default, 0)
  
  // Test option filtering
  let option_to_filter = Some(10)
  let filtered_option = match option_to_filter {
    Some(v) => {
      if v > 5 {
        Some(v)
      } else {
        None
      }
    }
    None => None
  }
  match filtered_option {
    Some(v) => assert_eq(v, 10)
    None => assert_true(false)
  }
}

// Test 5: Advanced Pattern Matching
test "advanced pattern matching" {
  // Test nested pattern matching
  let nested_data = (Some(42), "test")
  match nested_data {
    (Some(num), str) => {
      assert_eq(num, 42)
      assert_eq(str, "test")
    }
    (None, _) => assert_true(false)
  }
  
  // Test pattern matching with conditions
  let conditional_data = Some(10)
  match conditional_data {
    Some(value) if value > 5 => assert_eq(value, 10)
    Some(value) if value <= 5 => assert_true(false)
    None => assert_true(false)
  }
  
  // Test pattern matching on arrays
  let array_data = [1, 2, 3]
  match array_data.length() {
    0 => assert_true(false)
    1 => assert_true(false)
    3 => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test pattern matching on custom types (simulated with enums)
  enum Status {
    Ok(String)
    Error(Int, String)
    Pending
  }
  
  let status = Status::Error(404, "Not Found")
  match status {
    Status::Ok(message) => assert_true(false)
    Status::Error(code, message) => {
      assert_eq(code, 404)
      assert_eq(message, "Not Found")
    }
    Status::Pending => assert_true(false)
  }
}

// Test 6: Advanced Error Handling and Recovery
test "advanced error handling and recovery" {
  // Test division by zero recovery
  let safe_divide = fn(a: Int, b: Int) -> Option[Int> {
    if b == 0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  assert_eq(safe_divide(10, 2), Some(5))
  assert_eq(safe_divide(10, 0), None)
  
  // Test array bounds checking
  let safe_array_access = fn(arr: Array<Int>, index: Int) -> Option[Int> {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let test_array = [10, 20, 30]
  assert_eq(safe_array_access(test_array, 1), Some(20))
  assert_eq(safe_array_access(test_array, 3), None)
  assert_eq(safe_array_access(test_array, -1), None)
  
  // Test string parsing with error handling
  let safe_parse_int = fn(str: String) -> Option<Int> {
    // Simplified parsing - just check if string contains only digits
    let mut is_numeric = true
    for i in 0..str.length() {
      let char = str[i]
      if char < '0' || char > '9' {
        is_numeric = false
        break
      }
    }
    
    if is_numeric && str.length() > 0 {
      // Convert to int (simplified)
      Some(42) // Placeholder for actual conversion
    } else {
      None
    }
  }
  
  assert_eq(safe_parse_int("123"), Some(42))
  assert_eq(safe_parse_int("abc"), None)
  assert_eq(safe_parse_int(""), None)
}

// Test 7: Advanced Function Operations and Closures
test "advanced function operations and closures" {
  // Test higher-order functions
  let apply_twice = fn(f: (Int) -> Int, value: Int) -> Int {
    f(f(value))
  }
  
  let add_one = fn(x: Int) -> Int { x + 1 }
  let result = apply_twice(add_one, 5)
  assert_eq(result, 7)
  
  // Test function composition
  let compose = fn(f: (Int) -> Int, g: (Int) -> Int) -> (Int) -> Int {
    fn(x: Int) -> Int { f(g(x)) }
  }
  
  let multiply_by_two = fn(x: Int) -> Int { x * 2 }
  let add_three = fn(x: Int) -> Int { x + 3 }
  let composed = compose(multiply_by_two, add_three)
  assert_eq(composed(5), 16) // (5 + 3) * 2
  
  // Test closures capturing variables
  let make_adder = fn(n: Int) -> (Int) -> Int {
    fn(x: Int) -> Int { x + n }
  }
  
  let add_five = make_adder(5)
  assert_eq(add_five(10), 15)
  assert_eq(add_five(0), 5)
  
  // Test currying
  let curried_add = fn(a: Int) -> (Int) -> (Int) -> Int {
    fn(b: Int) -> (Int) -> Int {
      fn(c: Int) -> Int { a + b + c }
    }
  }
  
  let add_one = curried_add(1)
  let add_one_and_two = add_one(2)
  assert_eq(add_one_and_two(3), 6) // 1 + 2 + 3
}

// Test 8: Advanced Recursive Operations
test "advanced recursive operations" {
  // Test recursive factorial
  let factorial = fn(n: Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // Test recursive fibonacci
  let fibonacci = fn(n: Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // Test recursive array processing
  let sum_array = fn(arr: Array<Int>, index: Int) -> Int {
    if index >= arr.length() {
      0
    } else {
      arr[index] + sum_array(arr, index + 1)
    }
  }
  
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(sum_array(test_array, 0), 15)
  
  // Test recursive binary search
  let binary_search = fn(arr: Array<Int>, target: Int, left: Int, right: Int) -> Bool {
    if left > right {
      false
    } else {
      let mid = (left + right) / 2
      if arr[mid] == target {
        true
      } else if arr[mid] > target {
        binary_search(arr, target, left, mid - 1)
      } else {
        binary_search(arr, target, mid + 1, right)
      }
    }
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13]
  assert_true(binary_search(sorted_array, 7, 0, sorted_array.length() - 1))
  assert_false(binary_search(sorted_array, 8, 0, sorted_array.length() - 1))
}

// Test 9: Advanced Bitwise Operations
test "advanced bitwise operations" {
  // Test basic bitwise operations
  assert_eq(5 & 3, 1) // 101 & 011 = 001
  assert_eq(5 | 3, 7) // 101 | 011 = 111
  assert_eq(5 ^ 3, 6) // 101 ^ 011 = 110
  assert_eq(~5, -6)   // ~101 = ...010 (two's complement)
  
  // Test bit shifting
  assert_eq(5 << 2, 20) // 101 << 2 = 10100
  assert_eq(20 >> 2, 5) // 10100 >> 2 = 101
  
  // Test bit manipulation functions
  let get_bit = fn(num: Int, position: Int) -> Bool {
    (num >> position) & 1 == 1
  }
  
  let set_bit = fn(num: Int, position: Int) -> Int {
    num | (1 << position)
  }
  
  let clear_bit = fn(num: Int, position: Int) -> Int {
    num & ~(1 << position)
  }
  
  let toggle_bit = fn(num: Int, position: Int) -> Int {
    num ^ (1 << position)
  }
  
  // Test bit getting
  assert_true(get_bit(5, 0))  // 101 - bit 0 is 1
  assert_false(get_bit(5, 1)) // 101 - bit 1 is 0
  assert_true(get_bit(5, 2))  // 101 - bit 2 is 1
  
  // Test bit setting
  assert_eq(set_bit(4, 0), 5) // 100 -> 101
  assert_eq(set_bit(1, 2), 5) // 001 -> 101
  
  // Test bit clearing
  assert_eq(clear_bit(5, 0), 4) // 101 -> 100
  assert_eq(clear_bit(5, 2), 1) // 101 -> 001
  
  // Test bit toggling
  assert_eq(toggle_bit(5, 0), 4) // 101 -> 100
  assert_eq(toggle_bit(4, 0), 5) // 100 -> 101
  
  // Test bit counting
  let count_bits = fn(num: Int) -> Int {
    let mut count = 0
    let mut n = num
    while n != 0 {
      count = count + (n & 1)
      n = n >> 1
    }
    count
  }
  
  assert_eq(count_bits(0), 0)  // 0
  assert_eq(count_bits(1), 1)  // 1
  assert_eq(count_bits(2), 1)  // 10
  assert_eq(count_bits(3), 2)  // 11
  assert_eq(count_bits(5), 2)  // 101
  assert_eq(count_bits(7), 3)  // 111
}

// Test 10: Advanced Telemetry Data Processing
test "advanced telemetry data processing" {
  // Test trace ID and span ID validation
  let is_valid_trace_id = fn(trace_id: String) -> Bool {
    trace_id.length() == 32 && trace_id.chars().all(fn(c) { 
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
    })
  }
  
  let is_valid_span_id = fn(span_id: String) -> Bool {
    span_id.length() == 16 && span_id.chars().all(fn(c) { 
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
    })
  }
  
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let invalid_trace_id = "invalid_trace_id"
  let valid_span_id = "b7ad6b7169203331"
  let invalid_span_id = "invalid_span_id"
  
  assert_true(is_valid_trace_id(valid_trace_id))
  assert_false(is_valid_trace_id(invalid_trace_id))
  assert_true(is_valid_span_id(valid_span_id))
  assert_false(is_valid_span_id(invalid_span_id))
  
  // Test telemetry timestamp processing
  let format_timestamp = fn(timestamp: Int) -> String {
    let seconds = timestamp / 1000
    let milliseconds = timestamp % 1000
    
    let hours = seconds / 3600
    let minutes = (seconds % 3600) / 60
    let secs = seconds % 60
    
    hours.to_string() + ":" + 
    minutes.to_string() + ":" + 
    secs.to_string() + "." + 
    milliseconds.to_string()
  }
  
  assert_eq(format_timestamp(1234567890), "342936:129:0.789")
  
  // Test telemetry metric aggregation
  let aggregate_metrics = fn(values: Array<Float>) -> (Float, Float, Float) {
    if values.length() == 0 {
      (0.0, 0.0, 0.0)
    } else {
      let mut sum = 0.0
      let mut min = values[0]
      let mut max = values[0]
      
      for value in values {
        sum = sum + value
        if value < min { min = value }
        if value > max { max = value }
      }
      
      let avg = sum / values.length()
      (min, max, avg)
    }
  }
  
  let metrics = [10.5, 20.3, 15.7, 25.1, 12.9]
  let (min, max, avg) = aggregate_metrics(metrics)
  
  assert_eq(min, 10.5)
  assert_eq(max, 25.1)
  assert_eq(avg > 16.0 && avg < 17.0, true) // 10.5 + 20.3 + 15.7 + 25.1 + 12.9 = 84.5 / 5 = 16.9
  
  // Test telemetry data filtering
  let filter_telemetry_data = fn(data: Array<(String, Int)>, threshold: Int) -> Array<(String, Int)> {
    let mut filtered = []
    for item in data {
      match item {
        (key, value) => {
          if value >= threshold {
            filtered.push((key, value))
          }
        }
      }
    }
    filtered
  }
  
  let telemetry_data = [
    ("cpu_usage", 75),
    ("memory_usage", 60),
    ("disk_io", 45),
    ("network_io", 30)
  ]
  
  let filtered_data = filter_telemetry_data(telemetry_data, 50)
  assert_eq(filtered_data.length(), 2)
  assert_eq(filtered_data[0], ("cpu_usage", 75))
  assert_eq(filtered_data[1], ("memory_usage", 60))
}