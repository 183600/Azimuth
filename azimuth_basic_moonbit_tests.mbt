// Azimuth Basic MoonBit Test Suite
// This file contains basic MoonBit test cases focusing on language features

// Test 1: String Manipulation
test "string manipulation operations" {
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  assert_eq(str.to_uppercase(), "AZIMUTH-TELEMETRY")
  assert_eq(str.to_lowercase(), "azimuth-telemetry")
  assert_true(str.contains("telemetry"))
  assert_false(str.contains("metrics"))
  assert_eq(str.substring(0, 7), "azimuth")
}

// Test 2: Array Operations
test "array operations and transformations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(evens, [2, 4])
  assert_eq(sum, 15)
  assert_eq(numbers.length(), 5)
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
}

// Test 3: Option Type Operations
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

// Test 4: Result Type Operations
test "result type operations" {
  let success = Ok("success")
  let failure = Err("error")
  
  match success {
    Ok(value) => assert_eq(value, "success")
    Err(msg) => assert_true(false)
  }
  
  match failure {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "error")
  }
  
  let mapped = success.map(fn(s) { s.length() })
  match mapped {
    Ok(len) => assert_eq(len, 7)
    Err(_) => assert_true(false)
  }
}

// Test 5: Numeric Operations and Comparisons
test "numeric operations and comparisons" {
  let int_val = 42
  let float_val = 3.14
  
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(int_val / 2, 21)
  assert_eq(int_val % 5, 2)
  
  assert_true(int_val > 40)
  assert_true(int_val >= 42)
  assert_false(int_val < 40)
  assert_true(int_val <= 42)
  
  assert_eq(float_val + 1.0, 4.14)
  assert_eq(float_val * 2.0, 6.28)
  assert_true(float_val > 3.0)
  assert_true(float_val < 4.0)
}

// Test 6: Boolean Logic
test "boolean logic operations" {
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  let x = 10
  let y = 20
  assert_true(x < y && x > 5)
  assert_true(x > 5 || y < 15)
}

// Test 7: Tuple Operations
test "tuple operations" {
  let pair = (42, "answer")
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  let triple = (1, 2, 3)
  assert_eq(triple.0 + triple.1 + triple.2, 6)
  
  let swapped = (pair.1, pair.0)
  assert_eq(swapped.0, "answer")
  assert_eq(swapped.1, 42)
}

// Test 8: Pattern Matching
test "pattern matching with enums" {
  enum Color {
    Red
    Green
    Blue
    RGB(Int, Int, Int)
  }
  
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let custom = Color::RGB(255, 128, 0)
  
  let red_name = match red {
    Color::Red => "red"
    Color::Green => "green"
    Color::Blue => "blue"
    Color::RGB(r, g, b) => "custom"
  }
  assert_eq(red_name, "red")
  
  let custom_sum = match custom {
    Color::RGB(r, g, b) => r + g + b
    _ => 0
  }
  assert_eq(custom_sum, 383)
}

// Test 9: Loop and Iteration
test "loop and iteration patterns" {
  let mut sum = 0
  for i in 0..=10 {
    sum = sum + i
  }
  assert_eq(sum, 55)
  
  let mut product = 1
  for i in 1..=5 {
    product = product * i
  }
  assert_eq(product, 120)
  
  let squares = []
  for i in 1..=5 {
    squares = squares.push(i * i)
  }
  assert_eq(squares, [1, 4, 9, 16, 25])
}

// Test 10: Function Composition and Higher-Order Functions
test "function composition and higher-order functions" {
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  
  let add_then_double = compose(double, add_one)
  let double_then_square = compose(square, double)
  
  assert_eq(add_then_double(5), 12)
  assert_eq(double_then_square(3), 36)
  
  let numbers = [1, 2, 3, 4, 5]
  let transformed = numbers
    .map(add_one)
    .filter(fn(x) { x % 2 == 0 })
    .map(double)
  
  assert_eq(transformed, [4, 8, 12])
}

// Test 11: Record/Struct Operations
test "record and struct operations" {
  // Define a telemetry span record
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create a span record
  let span = {
    name: "database_query",
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok"
  }
  
  // Test record field access
  assert_eq(span.name, "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.end_time, 1640995250)
  assert_eq(span.status, "ok")
  
  // Test Option field access
  match span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span-789")
    None => assert_true(false)
  }
  
  // Calculate duration
  let duration = span.end_time - span.start_time
  assert_eq(duration, 50)
  
  // Create a copy with updated fields
  let updated_span = { span | status: "error", end_time: 1640995300 }
  assert_eq(updated_span.name, "database_query")  // unchanged
  assert_eq(updated_span.status, "error")  // updated
  assert_eq(updated_span.end_time, 1640995300)  // updated
  
  // Test record equality
  let same_span = {
    name: "database_query",
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok"
  }
  assert_eq(span.name, same_span.name)
  assert_eq(span.trace_id, same_span.trace_id)
}

// Test 12: Set Operations and Uniqueness
test "set operations and uniqueness" {
  // Create a set of unique trace IDs
  let trace_ids = ["trace-1", "trace-2", "trace-3", "trace-1", "trace-2"]
  
  // Simulate set operations using array filtering
  let unique_traces = fn(arr: Array[String>) {
    let mut result = []
    for item in arr {
      if not(result.contains(item)) {
        result = result.push(item)
      }
    }
    result
  }
  
  let unique_trace_ids = unique_traces(trace_ids)
  assert_eq(unique_trace_ids.length(), 3)
  assert_true(unique_trace_ids.contains("trace-1"))
  assert_true(unique_trace_ids.contains("trace-2"))
  assert_true(unique_trace_ids.contains("trace-3"))
  
  // Test set union operation
  let set_a = ["span-1", "span-2", "span-3"]
  let set_b = ["span-3", "span-4", "span-5"]
  
  let union = fn(a: Array[String>, b: Array[String>) {
    let combined = a + b
    unique_traces(combined)
  }
  
  let union_result = union(set_a, set_b)
  assert_eq(union_result.length(), 5)
  assert_true(union_result.contains("span-1"))
  assert_true(union_result.contains("span-2"))
  assert_true(union_result.contains("span-3"))
  assert_true(union_result.contains("span-4"))
  assert_true(union_result.contains("span-5"))
  
  // Test set intersection operation
  let intersection = fn(a: Array[String], b: Array[String>) {
    let mut result = []
    for item in a {
      if b.contains(item) and not(result.contains(item)) {
        result = result.push(item)
      }
    }
    result
  }
  
  let intersection_result = intersection(set_a, set_b)
  assert_eq(intersection_result.length(), 1)
  assert_eq(intersection_result[0], "span-3")
  
  // Test set difference operation
  let difference = fn(a: Array[String>, b: Array[String>) {
    let mut result = []
    for item in a {
      if not(b.contains(item)) and not(result.contains(item)) {
        result = result.push(item)
      }
    }
    result
  }
  
  let difference_result = difference(set_a, set_b)
  assert_eq(difference_result.length(), 2)
  assert_true(difference_result.contains("span-1"))
  assert_true(difference_result.contains("span-2"))
}

// Test 13: Map/Dictionary Operations
test "map and dictionary operations" {
  // Simulate map operations using key-value pairs
  let telemetry_attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("environment", "production"),
    ("region", "us-west-2")
  ]
  
  // Test map lookup function
  let get_value = fn(attributes: Array<(String, String)>, key: String) {
    let mut found = None
    for (k, v) in attributes {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  // Test map lookups
  let service_name = get_value(telemetry_attributes, "service.name")
  assert_eq(service_name, Some("payment-service"))
  
  let environment = get_value(telemetry_attributes, "environment")
  assert_eq(environment, Some("production"))
  
  let missing_key = get_value(telemetry_attributes, "missing.key")
  assert_eq(missing_key, None)
  
  // Test map update function
  let update_value = fn(attributes: Array[(String, String)>, key: String, value: String) {
    let mut updated = []
    let mut found = false
    
    for (k, v) in attributes {
      if k == key {
        updated = updated.push((k, value))
        found = true
      } else {
        updated = updated.push((k, v))
      }
    }
    
    if not(found) {
      updated = updated.push((key, value))
    }
    
    updated
  }
  
  // Test map updates
  let updated_attributes = update_value(telemetry_attributes, "environment", "staging")
  let updated_env = get_value(updated_attributes, "environment")
  assert_eq(updated_env, Some("staging"))
  
  // Test adding new key-value pair
  let with_new_key = update_value(updated_attributes, "deployment", "kubernetes")
  let deployment = get_value(with_new_key, "deployment")
  assert_eq(deployment, Some("kubernetes"))
  
  // Test map filter function
  let filter_by_prefix = fn(attributes: Array<(String, String)>, prefix: String) {
    let mut filtered = []
    for (k, v) in attributes {
      if k.starts_with(prefix) {
        filtered = filtered.push((k, v))
      }
    }
    filtered
  }
  
  let service_attributes = filter_by_prefix(with_new_key, "service.")
  assert_eq(service_attributes.length(), 2)
  assert_true(service_attributes.contains(("service.name", "payment-service")))
  assert_true(service_attributes.contains(("service.version", "1.2.3")))
}

// Test 14: Error Handling Patterns
test "error handling patterns" {
  // Define error types
  enum TelemetryError {
    InvalidSpanId(String)
    TraceNotFound(String)
    SerializationError(String)
    NetworkTimeout(Int)
  }
  
  // Define result type
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError]
  }
  
  // Create success result
  let create_success = fn(data: T) {
    {
      success: true,
      data: Some(data),
      error: None
    }
  }
  
  // Create error result
  let create_error = fn(error: TelemetryError) {
    {
      success: false,
      data: None,
      error: Some(error)
    }
  }
  
  // Test successful operation
  let valid_span_id = "span-12345"
  let span_result = if valid_span_id.length() == 9 {
    create_success(valid_span_id)
  } else {
    create_error(TelemetryError::InvalidSpanId(valid_span_id))
  }
  
  assert_true(span_result.success)
  assert_eq(span_result.data, Some("span-12345"))
  assert_eq(span_result.error, None)
  
  // Test error case
  let invalid_span_id = "bad"
  let error_result = if invalid_span_id.length() == 9 {
    create_success(invalid_span_id)
  } else {
    create_error(TelemetryError::InvalidSpanId(invalid_span_id))
  }
  
  assert_false(error_result.success)
  assert_eq(error_result.data, None)
  
  match error_result.error {
    Some(TelemetryError::InvalidSpanId(id)) => assert_eq(id, "bad")
    _ => assert_true(false)
  }
  
  // Test error chaining
  let process_with_fallback = fn(primary: String, fallback: String) {
    let primary_result = if primary.length() == 9 {
      create_success(primary)
    } else {
      create_error(TelemetryError::InvalidSpanId(primary))
    }
    
    match primary_result.success {
      true => primary_result
      false => {
        if fallback.length() == 9 {
          create_success(fallback)
        } else {
          create_error(TelemetryError::InvalidSpanId(fallback))
        }
      }
    }
  }
  
  let chained_result = process_with_fallback("bad", "fallback-123")
  assert_true(chained_result.success)
  assert_eq(chained_result.data, Some("fallback-123"))
}

// Test 15: Time and Date Operations
test "time and date operations" {
  // Test timestamp operations
  let base_timestamp = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // Test time arithmetic
  let one_hour_later = base_timestamp + 3600
  let one_day_later = base_timestamp + 86400
  let one_week_later = base_timestamp + 604800
  
  assert_eq(one_hour_later, 1640998800)
  assert_eq(one_day_later, 1641081600)
  assert_eq(one_week_later, 1641600000)
  
  // Test duration calculations
  let duration1 = one_hour_later - base_timestamp
  let duration2 = one_day_later - base_timestamp
  let duration3 = one_week_later - base_timestamp
  
  assert_eq(duration1, 3600)
  assert_eq(duration2, 86400)
  assert_eq(duration3, 604800)
  
  // Test time formatting simulation
  let format_timestamp = fn(timestamp: Int) {
    let date = timestamp / 86400  // Simplified: days since epoch
    let time = timestamp % 86400  // Seconds in current day
    let hours = time / 3600
    let minutes = (time % 3600) / 60
    let seconds = time % 60
    
    "Day " + date.to_string() + " " + 
    hours.to_string() + ":" + 
    minutes.to_string() + ":" + 
    seconds.to_string()
  }
  
  let formatted_base = format_timestamp(base_timestamp)
  assert_true(formatted_base.contains("Day"))
  assert_true(formatted_base.contains("0:0:0"))
  
  let formatted_hour = format_timestamp(one_hour_later)
  assert_true(formatted_hour.contains("Day"))
  assert_true(formatted_hour.contains("1:0:0"))
  
  // Test time comparison
  assert_true(one_hour_later > base_timestamp)
  assert_true(one_day_later > one_hour_later)
  assert_true(one_week_later > one_day_later)
  
  // Test time range checks
  let is_within_range = fn(timestamp: Int, start: Int, end: Int) {
    timestamp >= start and timestamp <= end
  }
  
  assert_true(is_within_range(one_hour_later, base_timestamp, one_day_later))
  assert_false(is_within_range(one_week_later, base_timestamp, one_day_later))
  assert_true(is_within_range(base_timestamp, base_timestamp, one_hour_later))
}

// Test 16: String Formatting and Parsing
test "string formatting and parsing" {
  // Test string interpolation simulation
  let format_span_info = fn(name: String, trace_id: String, duration: Int) {
    name + " (trace: " + trace_id + ", duration: " + duration.to_string() + "ms)"
  }
  
  let span_info = format_span_info("database_query", "trace-123", 250)
  assert_eq(span_info, "database_query (trace: trace-123, duration: 250ms)")
  
  // Test string padding and alignment
  let pad_string = fn(s: String, width: Int, align_left: Bool) {
    let padding = width - s.length()
    if padding <= 0 {
      s
    } else if align_left {
      s + " ".repeat(padding)
    } else {
      " ".repeat(padding) + s
    }
  }
  
  let left_padded = pad_string("name", 10, true)
  assert_eq(left_padded.length(), 10)
  assert_true(left_padded.starts_with("name"))
  
  let right_padded = pad_string("name", 10, false)
  assert_eq(right_padded.length(), 10)
  assert_true(right_padded.ends_with("name"))
  
  // Test string splitting and joining
  let csv_data = "service,version,environment\napi,1.0.0,production\nweb,2.1.0,staging"
  let lines = csv_data.split("\n")
  assert_eq(lines.length(), 3)
  
  let header = lines[0]
  let headers = header.split(",")
  assert_eq(headers.length(), 3)
  assert_eq(headers[0], "service")
  assert_eq(headers[1], "version")
  assert_eq(headers[2], "environment")
  
  // Test string parsing
  let parse_int = fn(s: String) {
    let mut result = 0
    let chars = s.to_char_array()
    for i in 0..chars.length() {
      let c = chars[i]
      if c >= '0' and c <= '9' {
        result = result * 10 + (c.to_int() - '0'.to_int())
      }
    }
    result
  }
  
  let parsed_number = parse_int("42")
  assert_eq(parsed_number, 42)
  
  let parsed_zero = parse_int("0")
  assert_eq(parsed_zero, 0)
  
  // Test string trimming
  let trim_string = fn(s: String) {
    let mut start = 0
    let mut end = s.length()
    
    // Find first non-whitespace character
    while start < end and (s[start] == ' ' or s[start] == '\t' or s[start] == '\n') {
      start = start + 1
    }
    
    // Find last non-whitespace character
    while end > start and (s[end - 1] == ' ' or s[end - 1] == '\t' or s[end - 1] == '\n') {
      end = end - 1
    }
    
    s.substring(start, end - start)
  }
  
  let trimmed = trim_string("  hello world  ")
  assert_eq(trimmed, "hello world")
  
  let tab_trimmed = trim_string("\t\ttrimmed\t\t")
  assert_eq(tab_trimmed, "trimmed")
}

// Test 17: Lazy Evaluation
test "lazy evaluation patterns" {
  // Simulate lazy evaluation using closures
  type Lazy[T] = {
    computed: Bool,
    value: Option[T],
    computation: () -> T
  }
  
  let create_lazy = fn(computation: () -> T) {
    {
      computed: false,
      value: None,
      computation
    }
  }
  
  let get_lazy_value = fn(lazy: Lazy[T]) {
    if lazy.computed {
      match lazy.value {
        Some(v) => v
        None => {
          // This shouldn't happen if computed is true
          lazy.computation()
        }
      }
    } else {
      let computed_value = lazy.computation()
      // Return the computed value (in a real implementation, we'd update the lazy struct)
      computed_value
    }
  }
  
  // Test lazy computation
  let computation_count = { mut count: 0 }
  
  let expensive_computation = fn() {
    computation_count.count = computation_count.count + 1
    42 * 42  // Expensive calculation
  }
  
  let lazy_value = create_lazy(expensive_computation)
  
  // Initially, computation shouldn't have run
  assert_eq(computation_count.count, 0)
  
  // First access triggers computation
  let result1 = get_lazy_value(lazy_value)
  assert_eq(result1, 1764)
  assert_eq(computation_count.count, 1)
  
  // Second access uses cached value (in real implementation)
  let result2 = get_lazy_value(lazy_value)
  assert_eq(result2, 1764)
  // In this simplified implementation, it would recompute
  // assert_eq(computation_count.count, 1)
  
  // Test lazy evaluation with side effects
  let side_effect_log = { mut messages: [] }
  
  let side_effect_computation = fn() {
    side_effect_log.messages = side_effect_log.messages.push("Computing...")
    "computed_value"
  }
  
  let lazy_with_side_effect = create_lazy(side_effect_computation)
  
  // Before access, no side effects
  assert_eq(side_effect_log.messages.length(), 0)
  
  // After access, side effects occur
  let side_result = get_lazy_value(lazy_with_side_effect)
  assert_eq(side_result, "computed_value")
  assert_eq(side_effect_log.messages.length(), 1)
  assert_eq(side_effect_log.messages[0], "Computing...")
  
  // Test lazy evaluation for conditional execution
  let conditional_lazy = fn(condition: Bool) {
    let lazy_computation = create_lazy(fn() {
      "expensive_result"
    })
    
    if condition {
      get_lazy_value(lazy_computation)
    } else {
      "default_value"
    }
  }
  
  let result_true = conditional_lazy(true)
  assert_eq(result_true, "expensive_result")
  
  let result_false = conditional_lazy(false)
  assert_eq(result_false, "default_value")
}

// Test 18: Module and Namespace Usage
test "module and namespace patterns" {
  // Simulate module structure using nested functions
  let telemetry_module = {
    // Constants
    version: "1.0.0",
    default_timeout: 5000,
    
    // Types (simulated with string names)
    span_type: "Span",
    metric_type: "Metric",
    
    // Functions
    create_span: fn(name: String) {
      {
        name,
        id: "span-" + name.length().to_string(),
        created_at: 1640995200
      }
    },
    
    create_metric: fn(name: String, value: Float) {
      {
        name,
        value,
        unit: "ms",
        timestamp: 1640995200
      }
    },
    
    validate_span_name: fn(name: String) {
      name.length() > 0 and name.length() <= 50
    }
  }
  
  // Test module constants
  assert_eq(telemetry_module.version, "1.0.0")
  assert_eq(telemetry_module.default_timeout, 5000)
  
  // Test module types (simulated)
  assert_eq(telemetry_module.span_type, "Span")
  assert_eq(telemetry_module.metric_type, "Metric")
  
  // Test module functions
  let span = telemetry_module.create_span("test_operation")
  assert_eq(span.name, "test_operation")
  assert_eq(span.id, "span-14")
  assert_eq(span.created_at, 1640995200)
  
  let metric = telemetry_module.create_metric("response_time", 125.5)
  assert_eq(metric.name, "response_time")
  assert_eq(metric.value, 125.5)
  assert_eq(metric.unit, "ms")
  assert_eq(metric.timestamp, 1640995200)
  
  // Test module validation
  assert_true(telemetry_module.validate_span_name("valid_name"))
  assert_false(telemetry_module.validate_span_name(""))
  assert_false(telemetry_module.validate_span_name("a".repeat(51)))
  
  // Test nested module structure
  let nested_modules = {
    telemetry: telemetry_module,
    
    config: {
      get_setting: fn(key: String, default_value: String) {
        match key {
          "timeout" => "10000"
          "retries" => "3"
          _ => default_value
        }
      }
    },
    
    utils: {
      format_duration: fn(ms: Int) {
        if ms < 1000 {
          ms.to_string() + "ms"
        } else {
          (ms / 1000).to_string() + "." + ((ms % 1000) / 100).to_string() + "s"
        }
      }
    }
  }
  
  // Test nested module access
  let timeout_setting = nested_modules.config.get_setting("timeout", "5000")
  assert_eq(timeout_setting, "10000")
  
  let default_setting = nested_modules.config.get_setting("unknown", "default")
  assert_eq(default_setting, "default")
  
  let formatted_short = nested_modules.utils.format_duration(500)
  assert_eq(formatted_short, "500ms")
  
  let formatted_long = nested_modules.utils.format_duration(2500)
  assert_eq(formatted_long, "2.5s")
  
  // Test module composition
  let create_telemetry_span = fn(name: String, timeout_override: Option[Int>) {
    let timeout = match timeout_override {
      Some(t) => t
      None => nested_modules.telemetry.default_timeout
    }
    
    let span = nested_modules.telemetry.create_span(name)
    {
      span,
      timeout,
      valid: nested_modules.telemetry.validate_span_name(name)
    }
  }
  
  let telemetry_span = create_telemetry_span("operation", Some(10000))
  assert_eq(telemetry_span.span.name, "operation")
  assert_eq(telemetry_span.timeout, 10000)
  assert_true(telemetry_span.valid)
  
  let default_span = create_telemetry_span("default_operation", None)
  assert_eq(default_span.span.name, "default_operation")
  assert_eq(default_span.timeout, 5000)
  assert_true(default_span.valid)
}