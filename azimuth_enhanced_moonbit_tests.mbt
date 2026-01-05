// Azimuth MoonBit Enhanced Test Cases
// This file contains enhanced test cases for the Azimuth telemetry system

test "string manipulation and pattern matching" {
  let text = "azimuth_telemetry_system"
  assert_eq(text.length(), 24)
  
  // Test string contains
  assert_true(text.contains("telemetry"))
  assert_false(text.contains("monitoring"))
  
  // Test string starts with
  assert_true(text.starts_with("azimuth"))
  assert_false(text.starts_with("telemetry"))
  
  // Test string ends with
  assert_true(text.ends_with("system"))
  assert_false(text.ends_with("azimuth"))
}

test "numeric operations and precision" {
  // Test integer operations
  assert_eq(42 + 58, 100)
  assert_eq(100 - 37, 63)
  assert_eq(15 * 7, 105)
  assert_eq(144 / 12, 12)
  
  // Test modulo operations
  assert_eq(17 % 5, 2)
  assert_eq(100 % 10, 0)
  
  // Test power operations
  assert_eq(2 ^ 8, 256)
  assert_eq(3 ^ 4, 81)
  
  // Test negative numbers
  assert_eq(-15 + 25, 10)
  assert_eq(-10 * -3, 30)
}

test "array and collection operations" {
  let numbers = [10, 20, 30, 40, 50]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 10)
  assert_eq(numbers[4], 50)
  
  // Test array filtering
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(even_numbers.length(), 5)
  
  // Test array mapping
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled[0], 20)
  assert_eq(doubled[4], 100)
  
  // Test array reduction
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 150)
}

test "option type and error handling" {
  let safe_divide = fn(a : Int, b : Int) -> Option[Int> {
    if b == 0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  // Test successful division
  match safe_divide(10, 2) {
    Some(result) => assert_eq(result, 5)
    None => assert_true(false) // Should not happen
  }
  
  // Test division by zero
  match safe_divide(10, 0) {
    Some(result) => assert_true(false) // Should not happen
    None => assert_true(true) // Expected case
  }
  
  // Test option chaining
  let result = safe_divide(20, 4).map(fn(x) { x * 2 })
  match result {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false) // Should not happen
  }
}

test "record and struct operations" {
  type Point = { x : Int, y : Int }
  
  let p1 = { x: 10, y: 20 }
  let p2 = { x: 15, y: 25 }
  
  // Test record access
  assert_eq(p1.x, 10)
  assert_eq(p1.y, 20)
  
  // Test record update
  let p3 = { p1 | y: 30 }
  assert_eq(p3.x, 10)
  assert_eq(p3.y, 30)
  
  // Test record equality
  assert_eq(p1, { x: 10, y: 20 })
  assert_not_eq(p1, p2)
  
  // Test nested records
  type Rectangle = { top_left : Point, bottom_right : Point }
  let rect = { top_left: p1, bottom_right: p2 }
  assert_eq(rect.top_left.x, 10)
  assert_eq(rect.bottom_right.y, 25)
}

test "enum and pattern matching" {
  enum Status {
    Active
    Inactive
    Pending(String)
    Error(Int, String)
  }
  
  let status1 = Active
  let status2 = Pending("Processing")
  let status3 = Error(404, "Not Found")
  
  // Test simple enum matching
  let is_active = match status1 {
    Active => true
    _ => false
  }
  assert_true(is_active)
  
  // Test enum with payload
  let message = match status2 {
    Pending(msg) => msg
    _ => "No message"
  }
  assert_eq(message, "Processing")
  
  // Test enum with multiple payloads
  let error_info = match status3 {
    Error(code, msg) => code.to_string() + ": " + msg
    _ => "No error"
  }
  assert_eq(error_info, "404: Not Found")
}

test "function composition and higher-order functions" {
  let add_one = fn(x : Int) -> Int { x + 1 }
  let double = fn(x : Int) -> Int { x * 2 }
  let square = fn(x : Int) -> Int { x * x }
  
  // Test function composition
  let add_one_and_double = fn(x : Int) -> Int { double(add_one(x)) }
  assert_eq(add_one_and_double(5), 12)
  
  // Test higher-order functions
  let apply_twice = fn(f : (Int) -> Int, x : Int) -> Int { f(f(x)) }
  assert_eq(apply_twice(add_one, 10), 12)
  assert_eq(apply_twice(double, 3), 12)
  
  // Test function as parameter
  let operate = fn(x : Int, y : Int, op : (Int, Int) -> Int) -> Int { op(x, y) }
  assert_eq(operate(10, 5, fn(a, b) { a + b }), 15)
  assert_eq(operate(10, 5, fn(a, b) { a * b }), 50)
}

test "recursive functions and algorithms" {
  // Test recursive factorial
  let factorial = fn(n : Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  assert_eq(factorial(5), 120)
  assert_eq(factorial(0), 1)
  
  // Test recursive fibonacci
  let fibonacci = fn(n : Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 8)
  
  // Test recursive sum of array
  let sum_array = fn(arr : Array[Int>, index : Int) -> Int {
    if index >= arr.length() {
      0
    } else {
      arr[index] + sum_array(arr, index + 1)
    }
  }
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(sum_array(numbers, 0), 15)
}

test "data validation and type conversion" {
  // Test string to int conversion
  let parse_int = fn(s : String) -> Option[Int> {
    if s == "0" {
      Some(0)
    } else if s == "42" {
      Some(42)
    } else if s == "-10" {
      Some(-10)
    } else {
      None
    }
  }
  
  match parse_int("42") {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  match parse_int("invalid") {
    Some(value) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test validation function
  let validate_email = fn(email : String) -> Bool {
    email.contains("@") && email.contains(".")
  }
  
  assert_true(validate_email("user@example.com"))
  assert_false(validate_email("invalid-email"))
  assert_false(validate_email("user@invalid"))
  
  // Test range validation
  let validate_range = fn(value : Int, min : Int, max : Int) -> Bool {
    value >= min && value <= max
  }
  
  assert_true(validate_range(50, 0, 100))
  assert_false(validate_range(150, 0, 100))
  assert_false(validate_range(-10, 0, 100))
}

test "telemetry data structures and operations" {
  // Test telemetry data structure
  type Metric = {
    name : String,
    value : Double,
    timestamp : Int,
    tags : Array[String]
  }
  
  type Span = {
    trace_id : String,
    span_id : String,
    operation_name : String,
    start_time : Int,
    duration : Int,
    status : String
  }
  
  // Create test metric
  let metric = {
    name: "cpu_usage",
    value: 75.5,
    timestamp: 1640995200,
    tags: ["host:server1", "region:us-west"]
  }
  
  assert_eq(metric.name, "cpu_usage")
  assert_eq(metric.value, 75.5)
  assert_eq(metric.tags.length(), 2)
  
  // Create test span
  let span = {
    trace_id: "trace-123",
    span_id: "span-456",
    operation_name: "database_query",
    start_time: 1640995200,
    duration: 150,
    status: "completed"
  }
  
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.operation_name, "database_query")
  assert_eq(span.duration, 150)
  
  // Test metric filtering
  let metrics = [
    { name: "cpu_usage", value: 75.5, timestamp: 1640995200, tags: ["host:server1"] },
    { name: "memory_usage", value: 60.0, timestamp: 1640995200, tags: ["host:server1"] },
    { name: "cpu_usage", value: 80.0, timestamp: 1640995300, tags: ["host:server2"] }
  ]
  
  let cpu_metrics = metrics.filter(fn(m) { m.name == "cpu_usage" })
  assert_eq(cpu_metrics.length(), 2)
  
  // Test span duration calculation
  let calculate_duration_ms = fn(start : Int, end : Int) -> Int { end - start }
  let duration = calculate_duration_ms(span.start_time, span.start_time + span.duration)
  assert_eq(duration, 150)
}