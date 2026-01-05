// Azimuth New MoonBit Test Cases
// This file contains 10 new test cases focusing on various MoonBit language features and telemetry scenarios

// Test 1: Advanced Pattern Matching with Guards
test "advanced pattern matching with guards" {
  enum TelemetryEvent {
    SpanStart(String, Int)
    SpanEnd(String, Int, Int)
    MetricRecord(String, Float, String)
    Error(String, String, Int)
  }
  
  let process_event = fn(event: TelemetryEvent) {
    match event {
      TelemetryEvent::SpanStart(name, timestamp) if name.contains("db") => 
        "Database span started: " + name + " at " + timestamp.to_string()
      TelemetryEvent::SpanStart(name, timestamp) => 
        "Span started: " + name + " at " + timestamp.to_string()
      TelemetryEvent::SpanEnd(name, start, end) if end - start > 1000 => 
        "Long span completed: " + name + " took " + (end - start).to_string() + "ms"
      TelemetryEvent::SpanEnd(name, start, end) => 
        "Span completed: " + name + " took " + (end - start).to_string() + "ms"
      TelemetryEvent::MetricRecord(name, value, unit) if value > 100.0 => 
        "High metric: " + name + " = " + value.to_string() + unit
      TelemetryEvent::MetricRecord(name, value, unit) => 
        "Metric: " + name + " = " + value.to_string() + unit
      TelemetryEvent::Error(message, severity, code) if code >= 500 => 
        "Server error: " + message + " (severity: " + severity + ")"
      TelemetryEvent::Error(message, severity, code) => 
        "Error: " + message + " (severity: " + severity + ")"
    }
  }
  
  let db_span = TelemetryEvent::SpanStart("db_query_user", 1640995200)
  let normal_span = TelemetryEvent::SpanStart("api_request", 1640995300)
  let long_span = TelemetryEvent::SpanEnd("data_processing", 1640995200, 1640996300)
  let normal_end = TelemetryEvent::SpanEnd("cache_lookup", 1640995400, 1640995450)
  let high_metric = TelemetryEvent::MetricRecord("response_time", 250.5, "ms")
  let normal_metric = TelemetryEvent::MetricRecord("cpu_usage", 45.2, "%")
  let server_error = TelemetryEvent::Error("Database connection failed", "critical", 503)
  let client_error = TelemetryEvent::Error("Invalid request format", "warning", 400)
  
  assert_eq(process_event(db_span), "Database span started: db_query_user at 1640995200")
  assert_eq(process_event(normal_span), "Span started: api_request at 1640995300")
  assert_eq(process_event(long_span), "Long span completed: data_processing took 1100ms")
  assert_eq(process_event(normal_end), "Span completed: cache_lookup took 50ms")
  assert_eq(process_event(high_metric), "High metric: response_time = 250.5ms")
  assert_eq(process_event(normal_metric), "Metric: cpu_usage = 45.2%")
  assert_eq(process_event(server_error), "Server error: Database connection failed (severity: critical)")
  assert_eq(process_event(client_error), "Error: Invalid request format (severity: warning)")
}

// Test 2: Recursive Data Structures and Algorithms
test "recursive data structures and algorithms" {
  // Define a tree structure for telemetry hierarchy
  enum TelemetryTree {
    Leaf(String, Int)
    Node(String, Array[TelemetryTree])
  }
  
  // Calculate total duration of a telemetry tree
  let calculate_total_duration = fn(tree: TelemetryTree) {
    match tree {
      TelemetryTree::Leaf(_, duration) => duration
      TelemetryTree::Node(_, children) => {
        let mut total = 0
        for child in children {
          total = total + calculate_total_duration(child)
        }
        total
      }
    }
  }
  
  // Find the longest path in the tree
  let find_longest_path = fn(tree: TelemetryTree) {
    let helper = fn(t: TelemetryTree) {
      match t {
        TelemetryTree::Leaf(name, duration) => (name, duration)
        TelemetryTree::Node(name, children) => {
          let mut max_duration = 0
          let mut longest_child = ""
          
          for child in children {
            let (child_name, child_duration) = helper(child)
            if child_duration > max_duration {
              max_duration = child_duration
              longest_child = child_name
            }
          }
          
          (name + " -> " + longest_child, max_duration)
        }
      }
    }
    
    let (path, _) = helper(tree)
    path
  }
  
  // Build a sample telemetry tree
  let leaf1 = TelemetryTree::Leaf("database_query", 150)
  let leaf2 = TelemetryTree::Leaf("cache_lookup", 25)
  let leaf3 = TelemetryTree::Leaf("api_call", 80)
  let leaf4 = TelemetryTree::Leaf("data_processing", 200)
  
  let subtree1 = TelemetryTree::Node("service_operations", [leaf1, leaf2])
  let subtree2 = TelemetryTree::Node("external_calls", [leaf3, leaf4])
  
  let root = TelemetryTree::Node("request_processing", [subtree1, subtree2])
  
  // Test calculations
  assert_eq(calculate_total_duration(leaf1), 150)
  assert_eq(calculate_total_duration(subtree1), 175)  // 150 + 25
  assert_eq(calculate_total_duration(subtree2), 280)  // 80 + 200
  assert_eq(calculate_total_duration(root), 455)     // 175 + 280
  
  // Test longest path finding
  assert_eq(find_longest_path(leaf1), "database_query")
  assert_eq(find_longest_path(subtree1), "service_operations -> database_query")
  assert_eq(find_longest_path(subtree2), "external_calls -> data_processing")
  assert_eq(find_longest_path(root), "request_processing -> external_calls -> data_processing")
}

// Test 3: Generic Types and Polymorphism
test "generic types and polymorphism" {
  // Define generic container types
  type Container[T] = {
    items: Array[T],
    capacity: Int
  }
  
  type TelemetryData[T] = {
    timestamp: Int,
    source: String,
    data: T
  }
  
  // Generic functions for Container
  let create_container = fn[T](capacity: Int) {
    { items: [], capacity }
  }
  
  let add_item = fn[T](container: Container[T], item: T) {
    if container.items.length() < container.capacity {
      { container | items: container.items.push(item) }
    } else {
      container  // Full container, return unchanged
    }
  }
  
  let find_item = fn[T](container: Container[T], predicate: T -> Bool) {
    let mut found = None
    for item in container.items {
      if predicate(item) {
        found = Some(item)
      }
    }
    found
  }
  
  // Generic functions for TelemetryData
  let create_telemetry_data = fn[T](timestamp: Int, source: String, data: T) {
    { timestamp, source, data }
  }
  
  let filter_by_source = fn[T](telemetry_items: Array[TelemetryData[T]], source: String) {
    let mut filtered = []
    for item in telemetry_items {
      if item.source == source {
        filtered = filtered.push(item)
      }
    }
    filtered
  }
  
  // Test with string data
  let string_container = create_container[String](3)
    |> add_item("span-1")
    |> add_item("span-2")
    |> add_item("span-3")
    |> add_item("span-4")  // Should be ignored due to capacity limit
  
  assert_eq(string_container.items.length(), 3)
  assert_eq(string_container.items, ["span-1", "span-2", "span-3"])
  
  let found_span = find_item(string_container, fn(s) { s.contains("2") })
  assert_eq(found_span, Some("span-2"))
  
  let not_found = find_item(string_container, fn(s) { s.contains("5") })
  assert_eq(not_found, None)
  
  // Test with numeric data
  let int_container = create_container[Int](2)
    |> add_item(100)
    |> add_item(200)
  
  assert_eq(int_container.items.length(), 2)
  assert_eq(int_container.items, [100, 200])
  
  let found_number = find_item(int_container, fn(n) { n > 150 })
  assert_eq(found_number, Some(200))
  
  // Test with TelemetryData
  let telemetry_items = [
    create_telemetry_data(1640995200, "service-a", "request_processed"),
    create_telemetry_data(1640995250, "service-b", "database_connected"),
    create_telemetry_data(1640995300, "service-a", "response_sent"),
    create_telemetry_data(1640995350, "service-c", "cache_updated")
  ]
  
  let service_a_items = filter_by_source(telemetry_items, "service-a")
  assert_eq(service_a_items.length(), 2)
  assert_eq(service_a_items[0].data, "request_processed")
  assert_eq(service_a_items[1].data, "response_sent")
  
  let service_c_items = filter_by_source(telemetry_items, "service-c")
  assert_eq(service_c_items.length(), 1)
  assert_eq(service_c_items[0].data, "cache_updated")
  
  // Test with complex data structures
  type MetricValue = {
    name: String,
    value: Float,
    unit: String
  }
  
  let metric_telemetry = [
    create_telemetry_data(1640995200, "metrics", { name: "cpu", value: 45.2, unit: "%" }),
    create_telemetry_data(1640995250, "metrics", { name: "memory", value: 1024.5, unit: "MB" }),
    create_telemetry_data(1640995300, "metrics", { name: "disk", value: 85.7, unit: "%" })
  ]
  
  let metrics_data = filter_by_source(metric_telemetry, "metrics")
  assert_eq(metrics_data.length(), 3)
  assert_eq(metrics_data[1].data.name, "memory")
  assert_eq(metrics_data[1].data.value, 1024.5)
}

// Test 4: State Machine Implementation
test "state machine implementation" {
  // Define states and events for a telemetry span lifecycle
  enum SpanState {
    NotStarted
    InProgress(String, Int)  // (name, start_time)
    Completed(String, Int, Int)  // (name, start_time, end_time)
    Failed(String, String, Int)  // (name, error_message, timestamp)
  }
  
  enum SpanEvent {
    Start(String, Int)
    Complete(Int)
    Fail(String)
  }
  
  // State transition function
  let transition = fn(state: SpanState, event: SpanEvent) {
    match (state, event) {
      (SpanState::NotStarted, SpanEvent::Start(name, time)) => 
        SpanState::InProgress(name, time)
      (SpanState::InProgress(name, start_time), SpanEvent::Complete(end_time)) => 
        SpanState::Completed(name, start_time, end_time)
      (SpanState::InProgress(name, start_time), SpanEvent::Fail(error)) => 
        SpanState::Failed(name, error, start_time)
      (SpanState::Completed(_, _, _), _) => 
        state  // Already completed, ignore further events
      (SpanState::Failed(_, _, _), _) => 
        state  // Already failed, ignore further events
      _ => 
        state  // Invalid transition, stay in current state
    }
  }
  
  // Helper function to get current status
  let get_status = fn(state: SpanState) {
    match state {
      SpanState::NotStarted => "not_started"
      SpanState::InProgress(name, _) => "in_progress: " + name
      SpanState::Completed(name, start, end) => 
        "completed: " + name + " (" + (end - start).to_string() + "ms)"
      SpanState::Failed(name, error, _) => "failed: " + name + " (" + error + ")"
    }
  }
  
  // Test state machine transitions
  let initial_state = SpanState::NotStarted
  assert_eq(get_status(initial_state), "not_started")
  
  // Start a span
  let started_state = transition(initial_state, SpanEvent::Start("database_query", 1640995200))
  assert_eq(get_status(started_state), "in_progress: database_query")
  
  // Complete the span
  let completed_state = transition(started_state, SpanEvent::Complete(1640995250))
  assert_eq(get_status(completed_state), "completed: database_query (50ms)")
  
  // Try to transition from completed state (should be ignored)
  let ignored_transition = transition(completed_state, SpanEvent::Start("new_span", 1640995300))
  assert_eq(get_status(ignored_transition), "completed: database_query (50ms)")
  
  // Test failure path
  let new_initial = SpanState::NotStarted
  let new_started = transition(new_initial, SpanEvent::Start("api_call", 1640995400))
  assert_eq(get_status(new_started), "in_progress: api_call")
  
  let failed_state = transition(new_started, SpanEvent::Fail("timeout"))
  assert_eq(get_status(failed_state), "failed: api_call (timeout)")
  
  // Try to transition from failed state (should be ignored)
  let failed_ignored = transition(failed_state, SpanEvent::Complete(1640995500))
  assert_eq(get_status(failed_ignored), "failed: api_call (timeout)")
  
  // Test invalid transitions
  let invalid1 = transition(SpanState::NotStarted, SpanEvent::Complete(1640995600))
  assert_eq(get_status(invalid1), "not_started")
  
  let invalid2 = transition(SpanState::InProgress("test", 1640995700), SpanEvent::Start("another", 1640995800))
  assert_eq(get_status(invalid2), "in_progress: test")
}

// Test 5: Memoization and Caching
test "memoization and caching" {
  // Define a cache structure
  type Cache[K, V] = {
    data: Array[(K, V)],
    max_size: Int
  }
  
  // Create a new cache
  let create_cache = fn[K, V](max_size: Int) {
    { data: [], max_size }
  }
  
  // Get value from cache
  let cache_get = fn[K, V](cache: Cache[K, V], key: K, key_eq: K -> K -> Bool) {
    let mut found = None
    for (k, v) in cache.data {
      if key_eq(k, key) {
        found = Some(v)
      }
    }
    found
  }
  
  // Set value in cache with LRU eviction
  let cache_set = fn[K, V](cache: Cache[K, V], key: K, value: V, key_eq: K -> K -> Bool) {
    // Check if key already exists
    let mut exists = false
    let mut updated_data = []
    
    for (k, v) in cache.data {
      if key_eq(k, key) {
        updated_data = updated_data.push((k, value))
        exists = true
      } else {
        updated_data = updated_data.push((k, v))
      }
    }
    
    if not(exists) {
      // Add new key-value pair
      updated_data = updated_data.push((key, value))
      
      // Evict oldest if over capacity
      if updated_data.length() > cache.max_size {
        updated_data = updated_data.slice(1, updated_data.length())
      }
    }
    
    { cache | data: updated_data }
  }
  
  // Memoization helper
  let memoize = fn[K, V](cache: Cache[K, V], key: K, computation: () -> V, key_eq: K -> K -> Bool) {
    match cache_get(cache, key, key_eq) {
      Some(value) => (value, cache)
      None => {
        let computed_value = computation()
        let updated_cache = cache_set(cache, key, computed_value, key_eq)
        (computed_value, updated_cache)
      }
    }
  }
  
  // Test cache operations
  let string_cache = create_cache[String, Int](3)
  
  // Add values to cache
  let cache1 = cache_set(string_cache, "span-1", 100, fn(a, b) { a == b })
  let cache2 = cache_set(cache1, "span-2", 200, fn(a, b) { a == b })
  let cache3 = cache_set(cache2, "span-3", 300, fn(a, b) { a == b })
  
  // Test retrieval
  assert_eq(cache_get(cache3, "span-1", fn(a, b) { a == b }), Some(100))
  assert_eq(cache_get(cache3, "span-2", fn(a, b) { a == b }), Some(200))
  assert_eq(cache_get(cache3, "span-3", fn(a, b) { a == b }), Some(300))
  assert_eq(cache_get(cache3, "span-4", fn(a, b) { a == b }), None)
  
  // Test LRU eviction
  let cache4 = cache_set(cache3, "span-4", 400, fn(a, b) { a == b })
  assert_eq(cache_get(cache4, "span-1", fn(a, b) { a == b }), None)  // Should be evicted
  assert_eq(cache_get(cache4, "span-2", fn(a, b) { a == b }), Some(200))
  assert_eq(cache_get(cache4, "span-4", fn(a, b) { a == b }), Some(400))
  
  // Test memoization
  let computation_count = { mut count: 0 }
  
  let expensive_computation = fn(input: Int) {
    computation_count.count = computation_count.count + 1
    input * input  // Simple square computation
  }
  
  let int_cache = create_cache[Int, Int](5)
  
  // First computation should calculate
  let (result1, cache_after1) = memoize(int_cache, 5, fn() { expensive_computation(5) }, fn(a, b) { a == b })
  assert_eq(result1, 25)
  assert_eq(computation_count.count, 1)
  
  // Second computation should use cache
  let (result2, _) = memoize(cache_after1, 5, fn() { expensive_computation(5) }, fn(a, b) { a == b })
  assert_eq(result2, 25)
  assert_eq(computation_count.count, 1)  // Should not increment
  
  // Different input should calculate
  let (result3, _) = memoize(cache_after1, 7, fn() { expensive_computation(7) }, fn(a, b) { a == b })
  assert_eq(result3, 49)
  assert_eq(computation_count.count, 2)  // Should increment
}

// Test 6: Functional Composition and Pipeline
test "functional composition and pipeline" {
  // Define pipeline operators
  let pipe = fn[T, U](value: T, fn: T -> U) {
    fn(value)
  }
  
  let compose = fn[T, U, V](f: U -> V, g: T -> U) {
    fn(x: T) { f(g(x)) }
  }
  
  // Telemetry data processing functions
  let parse_duration = fn(duration_str: String) {
    if duration_str.ends_with("ms") {
      let num_part = duration_str.substring(0, duration_str.length() - 2)
      num_part.to_int()
    } else if duration_str.ends_with("s") {
      let num_part = duration_str.substring(0, duration_str.length() - 1)
      let seconds = num_part.to_int()
      seconds * 1000
    } else {
      0  // Default case
    }
  }
  
  let categorize_duration = fn(duration_ms: Int) {
    if duration_ms < 100 {
      "fast"
    } else if duration_ms < 500 {
      "normal"
    } else if duration_ms < 1000 {
      "slow"
    } else {
      "very_slow"
    }
  }
  
  let format_result = fn(category: String) {
    "Operation categorized as: " + category
  }
  
  // Test individual functions
  assert_eq(parse_duration("150ms"), 150)
  assert_eq(parse_duration("2s"), 2000)
  assert_eq(categorize_duration(50), "fast")
  assert_eq(categorize_duration(250), "normal")
  assert_eq(categorize_duration(750), "slow")
  assert_eq(categorize_duration(1500), "very_slow")
  assert_eq(format_result("fast"), "Operation categorized as: fast")
  
  // Test function composition
  let parse_and_categorize = compose(categorize_duration, parse_duration)
  assert_eq(parse_and_categorize("75ms"), "fast")
  assert_eq(parse_and_categorize("300ms"), "normal")
  assert_eq(parse_and_categorize("1s"), "slow")
  
  let full_pipeline = compose(format_result, parse_and_categorize)
  assert_eq(full_pipeline("75ms"), "Operation categorized as: fast")
  assert_eq(full_pipeline("1s"), "Operation categorized as: slow")
  
  // Test pipeline operator
  let result1 = "150ms"
    |> parse_duration
    |> categorize_duration
    |> format_result
  
  assert_eq(result1, "Operation categorized as: normal")
  
  let result2 = "2s"
    |> parse_duration
    |> categorize_duration
    |> format_result
  
  assert_eq(result2, "Operation categorized as: very_slow")
  
  // Test more complex pipeline with array operations
  let durations = ["50ms", "150ms", "750ms", "2s", "25ms"]
  
  let processed_durations = durations
    .map(parse_duration)
    .map(categorize_duration)
    .filter(fn(cat) { cat != "very_slow" })
    .map(format_result)
  
  assert_eq(processed_durations.length(), 4)
  assert_eq(processed_durations[0], "Operation categorized as: fast")
  assert_eq(processed_durations[1], "Operation categorized as: normal")
  assert_eq(processed_durations[2], "Operation categorized as: slow")
  assert_eq(processed_durations[3], "Operation categorized as: fast")
  
  // Test pipeline with conditional logic
  let process_with_validation = fn(duration_str: String) {
    if duration_str.length() > 0 {
      duration_str
        |> parse_duration
        |> categorize_duration
        |> format_result
    } else {
      "Invalid duration: empty string"
    }
  }
  
  assert_eq(process_with_validation("300ms"), "Operation categorized as: normal")
  assert_eq(process_with_validation(""), "Invalid duration: empty string")
}

// Test 7: Advanced Error Handling with Result Types
test "advanced error handling with result types" {
  // Define error types
  enum ValidationError {
    EmptyField(String)
    InvalidFormat(String, String)
    OutOfRange(String, Int, Int, Int)  // field, value, min, max
  }
  
  enum ProcessingError {
    NetworkTimeout(Int)
    SerializationError(String)
    ValidationError(ValidationError)
  }
  
  // Result type
  type Result[T, E] = {
    success: Bool,
    value: Option[T],
    error: Option[E]
  }
  
  // Result constructors
  let ok = fn[T, E](value: T) {
    { success: true, value: Some(value), error: None }
  }
  
  let err = fn[T, E](error: E) {
    { success: false, value: None, error: Some(error) }
  }
  
  // Result combinators
  let map = fn[T, U, E](result: Result[T, E], fn: T -> U) {
    if result.success {
      match result.value {
        Some(v) => ok(fn(v))
        None => err(ProcessingError::SerializationError("Missing value in successful result"))
      }
    } else {
      match result.error {
        Some(e) => err(e)
        None => err(ProcessingError::SerializationError("Missing error in failed result"))
      }
    }
  }
  
  let flat_map = fn[T, U, E](result: Result[T, E], fn: T -> Result[U, E]) {
    if result.success {
      match result.value {
        Some(v) => fn(v)
        None => err(ProcessingError::SerializationError("Missing value in successful result"))
      }
    } else {
      result
    }
  }
  
  let with_default = fn[T, E](result: Result[T, E], default_value: T) {
    if result.success {
      match result.value {
        Some(v) => v
        None => default_value
      }
    } else {
      default_value
    }
  }
  
  // Validation functions
  let validate_non_empty = fn(field_name: String, value: String) {
    if value.length() > 0 {
      ok(value)
    } else {
      err(ProcessingError::ValidationError(ValidationError::EmptyField(field_name)))
    }
  }
  
  let validate_span_id = fn(span_id: String) {
    if span_id.length() == 9 and span_id.starts_with("span-") {
      ok(span_id)
    } else {
      err(ProcessingError::ValidationError(ValidationError::InvalidFormat(
        "span_id", 
        "Must be 9 characters starting with 'span-'"
      )))
    }
  }
  
  let validate_timestamp = fn(timestamp: Int) {
    if timestamp >= 1600000000 and timestamp <= 1700000000 {
      ok(timestamp)
    } else {
      err(ProcessingError::ValidationError(ValidationError::OutOfRange(
        "timestamp",
        timestamp,
        1600000000,
        1700000000
      )))
    }
  }
  
  // Processing functions
  let process_span = fn(name: String, span_id: String, timestamp: Int) {
    validate_non_empty("name", name)
      |> flat_map(fn(valid_name) {
        validate_span_id(span_id)
          |> flat_map(fn(valid_span_id) {
            validate_timestamp(timestamp)
              |> map(fn(valid_timestamp) {
                {
                  name: valid_name,
                  span_id: valid_span_id,
                  timestamp: valid_timestamp
                }
              })
          })
      })
  }
  
  // Test successful processing
  let valid_result = process_span("database_query", "span-12345", 1640995200)
  assert_true(valid_result.success)
  
  match valid_result.value {
    Some(span) => {
      assert_eq(span.name, "database_query")
      assert_eq(span.span_id, "span-12345")
      assert_eq(span.timestamp, 1640995200)
    }
    None => assert_true(false)
  }
  
  // Test validation errors
  let empty_name_result = process_span("", "span-12345", 1640995200)
  assert_false(empty_name_result.success)
  
  match empty_name_result.error {
    Some(ProcessingError::ValidationError(ValidationError::EmptyField(field))) => {
      assert_eq(field, "name")
    }
    _ => assert_true(false)
  }
  
  let invalid_span_result = process_span("api_call", "invalid", 1640995200)
  assert_false(invalid_span_result.success)
  
  match invalid_span_result.error {
    Some(ProcessingError::ValidationError(ValidationError::InvalidFormat(field, msg))) => {
      assert_eq(field, "span_id")
      assert_true(msg.contains("Must be 9 characters"))
    }
    _ => assert_true(false)
  }
  
  let invalid_timestamp_result = process_span("cache_lookup", "span-12345", 1500000000)
  assert_false(invalid_timestamp_result.success)
  
  match invalid_timestamp_result.error {
    Some(ProcessingError::ValidationError(ValidationError::OutOfRange(field, value, min, max))) => {
      assert_eq(field, "timestamp")
      assert_eq(value, 1500000000)
      assert_eq(min, 1600000000)
      assert_eq(max, 1700000000)
    }
    _ => assert_true(false)
  }
  
  // Test with_default
  let default_span = {
    name: "default_span",
    span_id: "span-00000",
    timestamp: 1600000000
  }
  
  let with_default_result = with_default(valid_result, default_span)
  assert_eq(with_default_result.name, "database_query")
  
  let with_default_error = with_default(empty_name_result, default_span)
  assert_eq(with_default_error.name, "default_span")
}

// Test 8: Lazy Evaluation and Infinite Structures
test "lazy evaluation and infinite structures" {
  // Define a lazy stream structure
  type Stream[T] = {
    head: T,
    tail: () -> Stream[T]
  }
  
  // Create infinite streams
  let naturals = fn(start: Int) {
    {
      head: start,
      tail: fn() { naturals(start + 1) }
    }
  }
  
  let powers_of_two = fn(exp: Int) {
    {
      head: 2 ^ exp,
      tail: fn() { powers_of_two(exp + 1) }
    }
  }
  
  // Stream operations
  let take = fn[T](stream: Stream[T], n: Int) {
    if n <= 0 {
      []
    } else {
      [stream.head] + take(stream.tail(), n - 1)
    }
  }
  
  let filter = fn[T](stream: Stream[T], predicate: T -> Bool) {
    if predicate(stream.head) {
      {
        head: stream.head,
        tail: fn() { filter(stream.tail(), predicate) }
      }
    } else {
      filter(stream.tail(), predicate)
    }
  }
  
  let map = fn[T, U](stream: Stream[T], fn: T -> U) {
    {
      head: fn(stream.head),
      tail: fn() { map(stream.tail(), fn) }
    }
  }
  
  let fold = fn[T, U](stream: Stream[T], initial: U, fn: U -> T -> U, limit: Int) {
    if limit <= 0 {
      initial
    } else {
      fold(stream.tail(), fn(initial, stream.head), fn, limit - 1)
    }
  }
  
  // Test infinite streams
  let first_10_naturals = take(naturals(1), 10)
  assert_eq(first_10_naturals, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  let first_5_powers = take(powers_of_two(0), 5)
  assert_eq(first_5_powers, [1, 2, 4, 8, 16])
  
  // Test filtering
  let even_naturals = filter(naturals(1), fn(n) { n % 2 == 0 })
  let first_5_evens = take(even_naturals, 5)
  assert_eq(first_5_evens, [2, 4, 6, 8, 10])
  
  let large_numbers = filter(naturals(1), fn(n) { n > 100 })
  let first_3_large = take(large_numbers, 3)
  assert_eq(first_3_large, [101, 102, 103])
  
  // Test mapping
  let squared_naturals = map(naturals(1), fn(n) { n * n })
  let first_5_squares = take(squared_naturals, 5)
  assert_eq(first_5_squares, [1, 4, 9, 16, 25])
  
  // Test folding
  let sum_first_10 = fold(naturals(1), 0, fn(acc, n) { acc + n }, 10)
  assert_eq(sum_first_10, 55)  // 1+2+3+...+10
  
  let product_first_5 = fold(naturals(1), 1, fn(acc, n) { acc * n }, 5)
  assert_eq(product_first_5, 120)  // 1*2*3*4*5
  
  // Test complex stream operations
  let fibonacci = {
    let fib_pair = fn(a: Int, b: Int) {
      {
        head: a,
        tail: fn() { fib_pair(b, a + b) }
      }
    }
    fib_pair(0, 1)
  }
  
  let first_10_fib = take(fibonacci, 10)
  assert_eq(first_10_fib, [0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
  
  // Test telemetry-specific lazy evaluation
  let telemetry_timestamps = fn(start: Int, interval: Int) {
    {
      head: start,
      tail: fn() { telemetry_timestamps(start + interval, interval) }
    }
  }
  
  let timestamps_5s = telemetry_timestamps(1640995200, 5)
  let first_5_timestamps = take(timestamps_5s, 5)
  assert_eq(first_5_timestamps, [1640995200, 1640995205, 1640995210, 1640995215, 1640995220])
  
  // Test lazy filtering with complex predicate
  let prime_numbers = {
    let is_prime = fn(n: Int) {
      if n <= 1 {
        false
      } else if n == 2 {
        true
      } else {
        let mut has_divisor = false
        for i in 2..=(n / 2) {
          if n % i == 0 {
            has_divisor = true
          }
        }
        not(has_divisor)
      }
    }
    
    filter(naturals(2), is_prime)
  }
  
  let first_5_primes = take(prime_numbers, 5)
  assert_eq(first_5_primes, [2, 3, 5, 7, 11])
}

// Test 9: Advanced Type System Features
test "advanced type system features" {
  // Define phantom types for type safety
  type Tagged[T, Tag] = {
    value: T
  }
  
  type SpanId = Tagged[String, SpanIdTag]
  type TraceId = Tagged[String, TraceIdTag]
  type MetricName = Tagged[String, MetricNameTag]
  
  // Type markers (phantom types)
  type SpanIdTag
  type TraceIdTag
  type MetricNameTag
  
  // Safe constructors
  let create_span_id = fn(id: String) {
    if id.length() == 9 and id.starts_with("span-") {
      Some({ value: id })
    } else {
      None
    }
  }
  
  let create_trace_id = fn(id: String) {
    if id.length() == 10 and id.starts_with("trace-") {
      Some({ value: id })
    } else {
      None
    }
  }
  
  let create_metric_name = fn(name: String) {
    if name.length() > 0 and name.length() <= 50 {
      Some({ value: name })
    } else {
      None
    }
  }
  
  // Type-safe operations
  let format_span_info = fn(span_id: SpanId, trace_id: TraceId) {
    "Span " + span_id.value + " in trace " + trace_id.value
  }
  
  let create_metric_record = fn(name: MetricName, value: Float) {
    {
      name: name.value,
      value: value,
      timestamp: 1640995200
    }
  }
  
  // Test phantom types
  let valid_span_id = create_span_id("span-12345")
  let invalid_span_id = create_span_id("invalid")
  
  match valid_span_id {
    Some(sid) => {
      let valid_trace_id = create_trace_id("trace-12345")
      match valid_trace_id {
        Some(tid) => {
          let info = format_span_info(sid, tid)
          assert_eq(info, "Span span-12345 in trace trace-12345")
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  assert_eq(invalid_span_id, None)
  
  // Test dependent types simulation
  type Vector[T, N] = {
    data: Array[T],
    length: N
  }
  
  let create_vector = fn[T](items: Array[T]) {
    { data: items, length: items.length() }
  }
  
  let safe_get = fn[T, N](vector: Vector[T, N], index: Int) {
    if index >= 0 and index < vector.data.length() {
      Some(vector.data[index])
    } else {
      None
    }
  }
  
  let vector3 = create_vector([1, 2, 3])
  assert_eq(vector3.length, 3)
  assert_eq(safe_get(vector3, 1), Some(2))
  assert_eq(safe_get(vector3, 5), None)
  
  // Test type-level constraints
  type PositiveInt = Int
  type NonEmptyString = String
  
  let validate_positive = fn(n: Int) {
    if n > 0 {
      Some(n)
    } else {
      None
    }
  }
  
  let validate_non_empty = fn(s: String) {
    if s.length() > 0 {
      Some(s)
    } else {
      None
    }
  }
  
  let create_metric_with_validation = fn(
    name: NonEmptyString, 
    value: PositiveInt, 
    unit: NonEmptyString
  ) {
    match (validate_non_empty(name), validate_positive(value), validate_non_empty(unit)) {
      (Some(valid_name), Some(valid_value), Some(valid_unit)) => {
        Some({
          name: valid_name,
          value: valid_value,
          unit: valid_unit
        })
      }
      _ => None
    }
  }
  
  let valid_metric = create_metric_with_validation("cpu_usage", 75, "%")
  match valid_metric {
    Some(m) => {
      assert_eq(m.name, "cpu_usage")
      assert_eq(m.value, 75)
      assert_eq(m.unit, "%")
    }
    None => assert_true(false)
  }
  
  let invalid_metric1 = create_metric_with_validation("", 75, "%")
  assert_eq(invalid_metric1, None)
  
  let invalid_metric2 = create_metric_with_validation("cpu_usage", -5, "%")
  assert_eq(invalid_metric2, None)
  
  let invalid_metric3 = create_metric_with_validation("cpu_usage", 75, "")
  assert_eq(invalid_metric3, None)
  
  // Test type classes simulation
  type Comparable[T] = {
    compare: T -> T -> Int  // -1 if a < b, 0 if a == b, 1 if a > b
  }
  
  let int_comparable: Comparable[Int] = {
    compare: fn(a, b) {
      if a < b { -1 } else if a > b { 1 } else { 0 }
    }
  }
  
  let string_comparable: Comparable[String] = {
    compare: fn(a, b) {
      if a < b { -1 } else if a > b { 1 } else { 0 }
    }
  }
  
  let generic_max = fn[T](comparable: Comparable[T], a: T, b: T) {
    if comparable.compare(a, b) >= 0 {
      a
    } else {
      b
    }
  }
  
  let generic_sort = fn[T](comparable: Comparable[T], arr: Array[T]) {
    let mut sorted = arr
    let n = sorted.length()
    
    for i in 0..<(n - 1) {
      for j in 0..<(n - i - 1) {
        if comparable.compare(sorted[j], sorted[j + 1]) > 0 {
          let temp = sorted[j]
          sorted[j] = sorted[j + 1]
          sorted[j + 1] = temp
        }
      }
    }
    
    sorted
  }
  
  // Test type classes
  assert_eq(generic_max(int_comparable, 5, 10), 10)
  assert_eq(generic_max(int_comparable, 20, 15), 20)
  assert_eq(generic_max(string_comparable, "apple", "banana"), "banana")
  assert_eq(generic_max(string_comparable, "zebra", "monkey"), "zebra")
  
  let sorted_ints = generic_sort(int_comparable, [5, 2, 8, 1, 9])
  assert_eq(sorted_ints, [1, 2, 5, 8, 9])
  
  let sorted_strings = generic_sort(string_comparable, ["zebra", "apple", "monkey"])
  assert_eq(sorted_strings, ["apple", "monkey", "zebra"])
}

// Test 10: Advanced Concurrency Patterns
test "advanced concurrency patterns" {
  // Simulate concurrent operations with futures/promises
  type Future[T] = {
    completed: Bool,
    value: Option[T],
    computation: () -> T
  }
  
  // Create a future
  let create_future = fn[T](computation: () -> T) {
    {
      completed: false,
      value: None,
      computation
    }
  }
  
  // Execute the computation
  let execute = fn[T](future: Future[T]) {
    if not(future.completed) {
      let result = future.computation()
      {
        completed: true,
        value: Some(result),
        computation: future.computation
      }
    } else {
      future
    }
  }
  
  // Get value (executing if needed)
  let get = fn[T](future: Future[T]) {
    if future.completed {
      match future.value {
        Some(v) => v
        None => {
          // This shouldn't happen if completed is true
          future.computation()
        }
      }
    } else {
      let executed = execute(future)
      match executed.value {
        Some(v) => v
        None => {
          // This shouldn't happen after execution
          executed.computation()
        }
      }
    }
  }
  
  // Map over future
  let map_future = fn[T, U](future: Future[T], fn: T -> U) {
    if future.completed {
      match future.value {
        Some(v) => {
          {
            completed: true,
            value: Some(fn(v)),
            computation: fn() { fn(v) }
          }
        }
        None => {
          {
            completed: false,
            value: None,
            computation: fn() { fn(future.computation()) }
          }
        }
      }
    } else {
      {
        completed: false,
        value: None,
        computation: fn() { fn(future.computation()) }
      }
    }
  }
  
  // Combine futures
  let combine = fn[T, U, V](future1: Future[T], future2: Future[U], combiner: T -> U -> V) {
    let get_value1 = get(future1)
    let get_value2 = get(future2)
    {
      completed: true,
      value: Some(combiner(get_value1, get_value2)),
      computation: fn() { combiner(get_value1, get_value2) }
    }
  }
  
  // Test futures
  let computation_count = { mut count: 0 }
  
  let expensive_computation = fn() {
    computation_count.count = computation_count.count + 1
    42 * 42
  }
  
  let future1 = create_future(expensive_computation)
  assert_false(future1.completed)
  assert_eq(computation_count.count, 0)
  
  let result1 = get(future1)
  assert_eq(result1, 1764)
  assert_eq(computation_count.count, 1)
  
  // Test mapping
  let string_computation = fn() {
    "hello"
  }
  
  let future2 = create_future(string_computation)
  let mapped_future = map_future(future2, fn(s) { s.length() })
  
  let mapped_result = get(mapped_future)
  assert_eq(mapped_result, 5)
  
  // Test combining
  let future3 = create_future(fn() { 10 })
  let future4 = create_future(fn() { 20 })
  
  let combined_future = combine(future3, future4, fn(a, b) { a + b })
  let combined_result = get(combined_future)
  assert_eq(combined_result, 30)
  
  // Simulate concurrent batch processing
  type BatchProcessor[T, R] = {
    items: Array[T],
    processor: T -> R,
    batch_size: Int
  }
  
  let create_batch_processor = fn[T, R](items: Array[T], processor: T -> R, batch_size: Int) {
    { items, processor, batch_size }
  }
  
  let process_batches = fn[T, R](batch_processor: BatchProcessor[T, R]) {
    let batches = batch_processor.items.length() / batch_processor.batch_size
    let mut results = []
    
    for i in 0..=batches {
      let start = i * batch_processor.batch_size
      let end = (i + 1) * batch_processor.batch_size
      
      if end > batch_processor.items.length() {
        break
      }
      
      let batch = batch_processor.items.slice(start, end)
      let batch_results = batch.map(batch_processor.processor)
      results = results.concat(batch_results)
    }
    
    results
  }
  
  // Test batch processing
  let telemetry_data = [
    "span-1", "span-2", "span-3", "span-4", "span-5", 
    "span-6", "span-7", "span-8", "span-9", "span-10"
  ]
  
  let processor = create_batch_processor(
    telemetry_data,
    fn(span_id) {
      "processed_" + span_id
    },
    3
  )
  
  let processed_data = process_batches(processor)
  assert_eq(processed_data.length(), 9)  // 3 batches of 3 items each
  assert_eq(processed_data[0], "processed_span-1")
  assert_eq(processed_data[8], "processed_span-9")
  
  // Test concurrent pipeline
  type Pipeline[T, U, V] = {
    stage1: T -> U,
    stage2: U -> V
  }
  
  let create_pipeline = fn[T, U, V](stage1: T -> U, stage2: U -> V) {
    { stage1, stage2 }
  }
  
  let process_with_pipeline = fn[T, U, V](pipeline: Pipeline[T, U, V], input: T) {
    let stage1_result = pipeline.stage1(input)
    let stage2_result = pipeline.stage2(stage1_result)
    stage2_result
  }
  
  let telemetry_pipeline = create_pipeline(
    fn(duration_str: String) {
      if duration_str.ends_with("ms") {
        let num_part = duration_str.substring(0, duration_str.length() - 2)
        num_part.to_int()
      } else {
        0
      }
    },
    fn(duration_ms: Int) {
      if duration_ms > 1000 {
        "slow"
      } else if duration_ms > 100 {
        "normal"
      } else {
        "fast"
      }
    }
  )
  
  let pipeline_result1 = process_with_pipeline(telemetry_pipeline, "50ms")
  assert_eq(pipeline_result1, "fast")
  
  let pipeline_result2 = process_with_pipeline(telemetry_pipeline, "500ms")
  assert_eq(pipeline_result2, "normal")
  
  let pipeline_result3 = process_with_pipeline(telemetry_pipeline, "1500ms")
  assert_eq(pipeline_result3, "slow")
  
  // Test concurrent aggregation
  type Aggregator[T, R] = {
    items: Array[T],
    aggregator: Array[T] -> R,
    chunk_size: Int
  }
  
  let create_aggregator = fn[T, R](items: Array[T], aggregator: Array[T] -> R, chunk_size: Int) {
    { items, aggregator, chunk_size }
  }
  
  let aggregate_in_chunks = fn[T, R](aggregator: Aggregator[T, R]) {
    let chunks = aggregator.items.length() / aggregator.chunk_size
    let mut chunk_results = []
    
    for i in 0..=chunks {
      let start = i * aggregator.chunk_size
      let end = (i + 1) * aggregator.chunk_size
      
      if end > aggregator.items.length() {
        break
      }
      
      let chunk = aggregator.items.slice(start, end)
      let chunk_result = aggregator.aggregator(chunk)
      chunk_results = chunk_results.push(chunk_result)
    }
    
    chunk_results
  }
  
  let metric_values = [10.5, 20.3, 15.7, 30.1, 25.8, 18.2, 22.6, 12.9]
  
  let sum_aggregator = create_aggregator(
    metric_values,
    fn(values) {
      let mut sum = 0.0
      for v in values {
        sum = sum + v
      }
      sum
    },
    4
  )
  
  let chunk_sums = aggregate_in_chunks(sum_aggregator)
  assert_eq(chunk_sums.length(), 2)
  assert_eq(chunk_sums[0], 10.5 + 20.3 + 15.7 + 30.1)  // First 4 values
  assert_eq(chunk_sums[1], 25.8 + 18.2 + 22.6 + 12.9)  // Next 4 values
  
  let avg_aggregator = create_aggregator(
    metric_values,
    fn(values) {
      let mut sum = 0.0
      for v in values {
        sum = sum + v
      }
      sum / values.length().to_float()
    },
    4
  )
  
  let chunk_avgs = aggregate_in_chunks(avg_aggregator)
  assert_eq(chunk_avgs.length(), 2)
  assert_eq(chunk_avgs[0], (10.5 + 20.3 + 15.7 + 30.1) / 4.0)
  assert_eq(chunk_avgs[1], (25.8 + 18.2 + 22.6 + 12.9) / 4.0)
}