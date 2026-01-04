// Azimuth Simple New Test Suite
// This file contains simple test cases for MoonBit language features

// Test 1: Basic arithmetic and logic
test "basic arithmetic and logic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(5 * 4, 20)
  assert_eq(10 / 2, 5)
  assert_eq(10 - 3, 7)
  
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_true(10 > 5)
  assert_false(10 < 5)
  assert_true(10 >= 10)
  assert_true(10 <= 10)
}

// Test 2: String operations
test "string operations" {
  let str1 = "hello"
  let str2 = "world"
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq(str1 + " " + str2, "hello world")
  
  let combined = "azimuth-telemetry"
  assert_true(combined.contains("telemetry"))
  assert_false(combined.contains("metrics"))
  assert_eq(combined.substring(0, 7), "azimuth")
}

// Test 3: Array operations
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
  
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
}

// Test 4: Option type handling
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
  
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

// Test 5: Loop and iteration
test "loop and iteration patterns" {
  let mut sum = 0
  for i in 0..=5 {
    sum = sum + i
  }
  assert_eq(sum, 15)  // 0 + 1 + 2 + 3 + 4 + 5 = 15
  
  let mut product = 1
  for i in 1..=4 {
    product = product * i
  }
  assert_eq(product, 24)  // 1 * 2 * 3 * 4 = 24
  
  let mut squares = []
  for i in 1..=5 {
    squares = squares.push(i * i)
  }
  assert_eq(squares, [1, 4, 9, 16, 25])
}

// Test 6: Conditional logic
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
  
  let ternary_like = if x > y { "x is greater" } else if x < y { "y is greater" } else { "equal" }
  assert_eq(ternary_like, "y is greater")
}

// Test 7: Type conversion
test "type conversion tests" {
  let int_val = 42
  let float_val = 3.14
  let bool_val = true
  
  // Test string conversion
  let int_str = int_val.to_string()
  assert_eq(int_str, "42")
  
  // Test boolean operations
  assert_true(bool_val && true)
  assert_false(bool_val && false)
  assert_true(!false)
  
  // Test numeric operations
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(float_val + 1.0, 4.14)
  assert_eq(float_val * 2.0, 6.28)
}

// Test 8: Error handling
test "error handling patterns" {
  // Test division by zero handling
  let numerator = 10
  let denominator = 0
  
  // In MoonBit, we need to handle this case
  if denominator != 0 {
    let result = numerator / denominator
    assert_eq(result, 5)
  } else {
    assert_true(true) // Expected case for denominator = 0
  }
  
  // Test array bounds
  let arr = [1, 2, 3]
  if arr.length() > 2 {
    assert_eq(arr[2], 3)
  } else {
    assert_true(false)
  }
  
  // Test Option error handling
  let safe_divide = fn(a: Int, b: Int) {
    if b != 0 {
      Some(a / b)
    } else {
      None
    }
  }
  
  let good_result = safe_divide(10, 2)
  match good_result {
    Some(v) => assert_eq(v, 5)
    None => assert_true(false)
  }
  
  let bad_result = safe_divide(10, 0)
  match bad_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 9: Basic telemetry concepts
test "basic telemetry concepts" {
  // Simulate a span
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    start_time: Int,
    end_time: Option[Int]
  }
  
  let span = {
    name: "database_query",
    trace_id: "trace-123",
    span_id: "span-456",
    start_time: 1640995200,
    end_time: Some(1640995250)
  }
  
  assert_eq(span.name, "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.end_time, Some(1640995250))
  
  // Calculate duration
  let duration = match span.end_time {
    Some(end) => end - span.start_time
    None => 0
  }
  assert_eq(duration, 50)
  
  // Create a running span
  let running_span = {
    name: "api_call",
    trace_id: "trace-789",
    span_id: "span-101",
    start_time: 1640995300,
    end_time: None
  }
  
  assert_eq(running_span.name, "api_call")
  assert_eq(running_span.end_time, None)
  
  let running_duration = match running_span.end_time {
    Some(end) => end - running_span.start_time
    None => 0
  }
  assert_eq(running_duration, 0)
}

// Test 10: Metric simulation
test "metric simulation" {
  // Simulate a metric
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int
  }
  
  let cpu_metric = {
    name: "cpu_usage",
    value: 75.5,
    unit: "percent",
    timestamp: 1640995200
  }
  
  assert_eq(cpu_metric.name, "cpu_usage")
  assert_eq(cpu_metric.value, 75.5)
  assert_eq(cpu_metric.unit, "percent")
  assert_eq(cpu_metric.timestamp, 1640995200)
  
  // Simulate metric aggregation
  let metrics = [
    { name: "cpu_usage", value: 50.0, unit: "percent", timestamp: 1640995200 },
    { name: "cpu_usage", value: 60.0, unit: "percent", timestamp: 1640995260 },
    { name: "cpu_usage", value: 70.0, unit: "percent", timestamp: 1640995320 },
    { name: "cpu_usage", value: 80.0, unit: "percent", timestamp: 1640995380 },
    { name: "cpu_usage", value: 75.0, unit: "percent", timestamp: 1640995440 }
  ]
  
  let mut sum = 0.0
  for metric in metrics {
    sum = sum + metric.value
  }
  let average = sum / metrics.length().to_float()
  
  assert_eq(average, 67.0)
  
  // Find max value
  let mut max_value = 0.0
  for metric in metrics {
    if metric.value > max_value {
      max_value = metric.value
    }
  }
  assert_eq(max_value, 80.0)
  
  // Find min value
  let mut min_value = 100.0
  for metric in metrics {
    if metric.value < min_value {
      min_value = metric.value
    }
  }
  assert_eq(min_value, 50.0)
}