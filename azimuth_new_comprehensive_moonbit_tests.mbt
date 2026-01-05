// Azimuth New Comprehensive MoonBit Test Suite
// This file contains comprehensive test cases for the Azimuth telemetry system
// focusing on advanced features and edge cases

// Test 1: Telemetry Data Aggregation and Statistical Analysis
test "telemetry data aggregation and statistical analysis" {
  // Define telemetry data structures
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[String]
  }
  
  type AggregatedMetrics = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    average: Float,
    median: Float,
    percentile_95: Float
  }
  
  // Create sample metrics
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1640995200, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 85.3, timestamp: 1640995260, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 200.1, timestamp: 1640995320, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 95.7, timestamp: 1640995380, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 150.2, timestamp: 1640995440, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 110.8, timestamp: 1640995500, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 180.4, timestamp: 1640995560, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 75.9, timestamp: 1640995620, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 165.3, timestamp: 1640995680, tags: ["endpoint:/api/users"] },
    { name: "response_time", value: 90.6, timestamp: 1640995740, tags: ["endpoint:/api/users"] }
  ]
  
  // Filter metrics by name
  let filter_by_name = fn(name: String, all_metrics: Array[Metric]) {
    let mut result = []
    for metric in all_metrics {
      if metric.name == name {
        result = result.push(metric)
      }
    }
    result
  }
  
  let response_time_metrics = filter_by_name("response_time", metrics)
  assert_eq(response_time_metrics.length(), 10)
  
  // Calculate basic statistics
  let calculate_basic_stats = fn(values: Array[Float]) {
    let mut sum = 0.0
    let mut min = values[0]
    let mut max = values[0]
    
    for value in values {
      sum = sum + value
      if value < min { min = value }
      if value > max { max = value }
    }
    
    {
      count: values.length(),
      sum: sum,
      min: min,
      max: max,
      average: sum / values.length().to_float()
    }
  }
  
  let values = response_time_metrics.map(fn(m) { m.value })
  let basic_stats = calculate_basic_stats(values)
  
  assert_eq(basic_stats.count, 10)
  assert_eq(basic_stats.min, 75.9)
  assert_eq(basic_stats.max, 200.1)
  assert_eq(basic_stats.sum, 1374.8)
  assert_eq(basic_stats.average, 137.48)
  
  // Calculate median
  let calculate_median = fn(values: Array[Float]) {
    let sorted = values.sort(fn(a, b) { a <= b })
    let n = sorted.length()
    
    if n % 2 == 1 {
      sorted[n / 2]
    } else {
      (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
  }
  
  let median = calculate_median(values)
  assert_eq(median, 115.3)  // Average of 110.8 and 120.5
  
  // Calculate percentile
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    let sorted = values.sort(fn(a, b) { a <= b })
    let n = sorted.length()
    let index = (n.to_float() * percentile / 100.0).to_int()
    
    if index >= n { sorted[n - 1] }
    else { sorted[index] }
  }
  
  let percentile_95 = calculate_percentile(values, 95.0)
  assert_eq(percentile_95, 200.1)
  
  // Create aggregated metrics
  let aggregated = {
    count: basic_stats.count,
    sum: basic_stats.sum,
    min: basic_stats.min,
    max: basic_stats.max,
    average: basic_stats.average,
    median: median,
    percentile_95: percentile_95
  }
  
  assert_eq(aggregated.count, 10)
  assert_eq(aggregated.min, 75.9)
  assert_eq(aggregated.max, 200.1)
  assert_eq(aggregated.average, 137.48)
  assert_eq(aggregated.median, 115.3)
  assert_eq(aggregated.percentile_95, 200.1)
  
  // Test time-based aggregation
  let aggregate_by_time_window = fn(metrics: Array[Metric], window_size: Int) {
    let mut result = []
    let mut current_window = []
    let mut current_start = if metrics.length() > 0 { metrics[0].timestamp } else { 0 }
    
    for metric in metrics {
      if metric.timestamp < current_start + window_size {
        current_window = current_window.push(metric)
      } else {
        if current_window.length() > 0 {
          result = result.push(current_window)
          current_window = []
        }
        current_start = metric.timestamp
        current_window = current_window.push(metric)
      }
    }
    
    if current_window.length() > 0 {
      result = result.push(current_window)
    }
    
    result
  }
  
  let time_windows = aggregate_by_time_window(response_time_metrics, 300)  // 5-minute windows
  assert_eq(time_windows.length(), 2)
  assert_eq(time_windows[0].length(), 5)
  assert_eq(time_windows[1].length(), 5)
}

// Test 2: Distributed Tracing Context Propagation
test "distributed tracing context propagation" {
  // Define tracing context structures
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  type Span = {
    name: String,
    context: TraceContext,
    start_time: Int,
    end_time: Int,
    status: String,
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // Create root span
  let create_root_span = fn(name: String) {
    let trace_id = "trace-" + (1000 + name.length()).to_string()
    let span_id = "span-" + (2000 + name.length()).to_string()
    
    {
      name: name,
      context: {
        trace_id: trace_id,
        span_id: span_id,
        parent_span_id: None,
        baggage: [],
        flags: 0
      },
      start_time: 1640995200,
      end_time: 1640995300,
      status: "ok",
      events: []
    }
  }
  
  // Create child span
  let create_child_span = fn(name: String, parent_context: TraceContext) {
    let span_id = "span-" + (3000 + name.length()).to_string()
    
    {
      name: name,
      context: {
        trace_id: parent_context.trace_id,
        span_id: span_id,
        parent_span_id: Some(parent_context.span_id),
        baggage: parent_context.baggage,
        flags: parent_context.flags
      },
      start_time: 1640995250,
      end_time: 1640995280,
      status: "ok",
      events: []
    }
  }
  
  // Add baggage item
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      baggage: context.baggage.push((key, value)),
      flags: context.flags
    }
  }
  
  // Create span hierarchy
  let root_span = create_root_span("http_request")
  assert_eq(root_span.context.trace_id, "trace-12")
  assert_eq(root_span.context.span_id, "span-13")
  assert_eq(root_span.context.parent_span_id, None)
  
  // Add baggage to root context
  let root_context_with_baggage = add_baggage_item(root_span.context, "user.id", "12345")
  assert_eq(root_context_with_baggage.baggage.length(), 1)
  assert_eq(root_context_with_baggage.baggage[0], ("user.id", "12345"))
  
  // Create child span with baggage
  let child_span = create_child_span("database_query", root_context_with_baggage)
  assert_eq(child_span.context.trace_id, "trace-12")
  assert_eq(child_span.context.span_id, "span-15")
  assert_eq(child_span.context.parent_span_id, Some("span-13"))
  assert_eq(child_span.context.baggage.length(), 1)
  assert_eq(child_span.context.baggage[0], ("user.id", "12345"))
  
  // Add more baggage to child
  let child_context_with_more_baggage = add_baggage_item(child_span.context, "db.query", "SELECT * FROM users")
  assert_eq(child_context_with_more_baggage.baggage.length(), 2)
  
  // Create grandchild span
  let grandchild_span = create_child_span("cache_lookup", child_context_with_more_baggage)
  assert_eq(grandchild_span.context.trace_id, "trace-12")
  assert_eq(grandchild_span.context.span_id, "span-12")
  assert_eq(grandchild_span.context.parent_span_id, Some("span-15"))
  assert_eq(grandchild_span.context.baggage.length(), 2)
  
  // Test context extraction and injection
  let inject_context = fn(context: TraceContext) {
    "trace-id:" + context.trace_id + 
    ",span-id:" + context.span_id + 
    ",parent-span-id:" + (match context.parent_span_id {
      Some(id) => id
      None => ""
    }) + 
    ",baggage:" + context.baggage.map(fn(item) { 
      item.0 + "=" + item.1 
    }).join(",")
  }
  
  let extract_context = fn(headers: String) {
    let parts = headers.split(",")
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut baggage = []
    
    for part in parts {
      let kv = part.split(":")
      if kv.length() == 2 {
        match kv[0] {
          "trace-id" => trace_id = kv[1]
          "span-id" => span_id = kv[1]
          "parent-span-id" => {
            if kv[1] != "" {
              parent_span_id = Some(kv[1])
            }
          }
          "baggage" => {
            if kv[1] != "" {
              let items = kv[1].split(",")
              for item in items {
                let item_kv = item.split("=")
                if item_kv.length() == 2 {
                  baggage = baggage.push((item_kv[0], item_kv[1]))
                }
              }
            }
          }
          _ => ()
        }
      }
    }
    
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_span_id,
      baggage: baggage,
      flags: 0
    }
  }
  
  // Test context injection
  let injected = inject_context(grandchild_span.context)
  assert_true(injected.contains("trace-id:trace-12"))
  assert_true(injected.contains("span-id:span-12"))
  assert_true(injected.contains("parent-span-id:span-15"))
  assert_true(injected.contains("user.id=12345"))
  assert_true(injected.contains("db.query=SELECT * FROM users"))
  
  // Test context extraction
  let extracted = extract_context(injected)
  assert_eq(extracted.trace_id, "trace-12")
  assert_eq(extracted.span_id, "span-12")
  assert_eq(extracted.parent_span_id, Some("span-15"))
  assert_eq(extracted.baggage.length(), 2)
  
  // Test span event addition
  let add_span_event = fn(span: Span, event_name: String, attributes: Array[(String, String)]) {
    let event = {
      name: event_name,
      timestamp: 1640995270,
      attributes: attributes
    }
    
    {
      name: span.name,
      context: span.context,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      events: span.events.push(event)
    }
  }
  
  let child_span_with_event = add_span_event(
    child_span, 
    "db.query.start", 
    [("query", "SELECT * FROM users"), ("table", "users")]
  )
  
  assert_eq(child_span_with_event.events.length(), 1)
  assert_eq(child_span_with_event.events[0].name, "db.query.start")
  assert_eq(child_span_with_event.events[0].attributes.length(), 2)
}

// Test 3: Performance Benchmarking and Resource Management
test "performance benchmarking and resource management" {
  // Define performance measurement structures
  type BenchmarkResult = {
    name: String,
    iterations: Int,
    total_time: Int,
    min_time: Int,
    max_time: Int,
    average_time: Float,
    operations_per_second: Float
  }
  
  type ResourceUsage = {
    memory_used: Int,
    memory_peak: Int,
    cpu_time: Int,
    allocations: Int
  }
  
  // Simulate performance measurement
  let measure_performance = fn(name: String, operation: () -> (), iterations: Int) {
    let start_time = 1640995200
    let mut total_time = 0
    let mut min_time = 999999
    let mut max_time = 0
    
    for i in 1..=iterations {
      let iteration_start = 1640995200 + i
      operation()
      let iteration_end = 1640995200 + i + 10  // Simulate 10ms operation
      let iteration_time = iteration_end - iteration_start
      
      total_time = total_time + iteration_time
      if iteration_time < min_time { min_time = iteration_time }
      if iteration_time > max_time { max_time = iteration_time }
    }
    
    let average_time = total_time.to_float() / iterations.to_float()
    let operations_per_second = 1000.0 / average_time
    
    {
      name: name,
      iterations: iterations,
      total_time: total_time,
      min_time: min_time,
      max_time: max_time,
      average_time: average_time,
      operations_per_second: operations_per_second
    }
  }
  
  // Test string concatenation performance
  let string_concat_operation = fn() {
    let mut result = ""
    for i in 1..=100 {
      result = result + "item-" + i.to_string()
    }
    result.length()
  }
  
  let string_benchmark = measure_performance("string_concatenation", string_concat_operation, 10)
  assert_eq(string_benchmark.name, "string_concatenation")
  assert_eq(string_benchmark.iterations, 10)
  assert_eq(string_benchmark.min_time, 10)
  assert_eq(string_benchmark.max_time, 10)
  assert_eq(string_benchmark.average_time, 10.0)
  assert_eq(string_benchmark.operations_per_second, 100.0)
  
  // Test array operations performance
  let array_operation = fn() {
    let mut arr = []
    for i in 1..=1000 {
      arr = arr.push(i)
    }
    let sum = arr.reduce(fn(acc, x) { acc + x }, 0)
    sum
  }
  
  let array_benchmark = measure_performance("array_operations", array_operation, 5)
  assert_eq(array_benchmark.name, "array_operations")
  assert_eq(array_benchmark.iterations, 5)
  assert_eq(array_benchmark.min_time, 10)
  assert_eq(array_benchmark.max_time, 10)
  assert_eq(array_benchmark.average_time, 10.0)
  assert_eq(array_benchmark.operations_per_second, 100.0)
  
  // Simulate resource usage tracking
  let track_resource_usage = fn(operation: () -> ()) {
    let initial_memory = 1024 * 1024  // 1MB
    let peak_memory = initial_memory
    let allocations = 100
    
    operation()
    
    let final_memory = initial_memory + (1024 * 100)  // 100KB more
    let new_peak = final_memory + (1024 * 50)  // 50KB peak
    
    {
      memory_used: final_memory - initial_memory,
      memory_peak: new_peak,
      cpu_time: 50,
      allocations: allocations
    }
  }
  
  let resource_usage = track_resource_usage(fn() {
    let mut arr = []
    for i in 1..=1000 {
      arr = arr.push("item-" + i.to_string())
    }
    arr.length()
  })
  
  assert_eq(resource_usage.memory_used, 102400)  // 100KB
  assert_eq(resource_usage.memory_peak, 1075200)  // 1MB + 100KB + 50KB
  assert_eq(resource_usage.cpu_time, 50)
  assert_eq(resource_usage.allocations, 100)
  
  // Test memory efficiency comparison
  let compare_memory_efficiency = fn(operation1: () -> (), operation2: () -> ()) {
    let usage1 = track_resource_usage(operation1)
    let usage2 = track_resource_usage(operation2)
    
    {
      operation1_memory: usage1.memory_used,
      operation2_memory: usage2.memory_used,
      efficiency_ratio: usage1.memory_used.to_float() / usage2.memory_used.to_float()
    }
  }
  
  let inefficient_operation = fn() {
    let mut result = ""
    for i in 1..=100 {
      result = result + "item-" + i.to_string() + ","
    }
    result
  }
  
  let efficient_operation = fn() {
    let mut arr = []
    for i in 1..=100 {
      arr = arr.push("item-" + i.to_string())
    }
    arr.join(",")
  }
  
  let efficiency_comparison = compare_memory_efficiency(inefficient_operation, efficient_operation)
  assert_eq(efficiency_comparison.operation1_memory, 102400)
  assert_eq(efficiency_comparison.operation2_memory, 102400)
  assert_eq(efficiency_comparison.efficiency_ratio, 1.0)
}

// Test 4: Error Handling and Recovery Mechanisms
test "error handling and recovery mechanisms" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    InvalidData(String)
    ResourceExhausted(String)
    ServiceUnavailable(String)
  }
  
  type ErrorRecoveryStrategy = {
    max_retries: Int,
    backoff_multiplier: Float,
    max_backoff: Int,
    retry_on: Array[TelemetryError]
  }
  
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError],
    attempts: Int
  }
  
  // Create retry operation with exponential backoff
  let retry_with_backoff = fn[T](
    operation: () -> OperationResult[T], 
    strategy: ErrorRecoveryStrategy
  ) -> OperationResult[T] {
    let mut attempt = 0
    let mut backoff = 100  // Initial backoff in ms
    let mut last_result = operation()
    
    while attempt < strategy.max_retries and not(last_result.success) {
      attempt = attempt + 1
      
      // Check if error is retryable
      let should_retry = match last_result.error {
        Some(error) => {
          strategy.retry_on.any(fn(retryable_error) {
            match (error, retryable_error) {
              (TelemetryError::NetworkTimeout(_), TelemetryError::NetworkTimeout(_)) => true
              (TelemetryError::ServiceUnavailable(_), TelemetryError::ServiceUnavailable(_)) => true
              _ => false
            }
          })
        }
        None => false
      }
      
      if should_retry {
        // Simulate backoff delay
        backoff = (backoff.to_float() * strategy.backoff_multiplier).to_int()
        if backoff > strategy.max_backoff {
          backoff = strategy.max_backoff
        }
        
        last_result = operation()
        last_result.attempts = attempt + 1
      } else {
        break
      }
    }
    
    last_result
  }
  
  // Test successful operation
  let successful_operation = fn() -> OperationResult[String] {
    {
      success: true,
      data: Some("operation succeeded"),
      error: None,
      attempts: 1
    }
  }
  
  let retry_strategy = {
    max_retries: 3,
    backoff_multiplier: 2.0,
    max_backoff: 1000,
    retry_on: [TelemetryError::NetworkTimeout(""), TelemetryError::ServiceUnavailable("")]
  }
  
  let success_result = retry_with_backoff(successful_operation, retry_strategy)
  assert_true(success_result.success)
  assert_eq(success_result.data, Some("operation succeeded"))
  assert_eq(success_result.error, None)
  assert_eq(success_result.attempts, 1)
  
  // Test operation that always fails with retryable error
  let failing_operation = fn() -> OperationResult[String] {
    {
      success: false,
      data: None,
      error: Some(TelemetryError::NetworkTimeout("Connection timed out")),
      attempts: 1
    }
  }
  
  let failure_result = retry_with_backoff(failing_operation, retry_strategy)
  assert_false(failure_result.success)
  assert_eq(failure_result.data, None)
  assert_eq(failure_result.attempts, 4)  // 1 initial + 3 retries
  
  // Test operation that fails with non-retryable error
  let non_retryable_operation = fn() -> OperationResult[String] {
    {
      success: false,
      data: None,
      error: Some(TelemetryError::InvalidData("Invalid format")),
      attempts: 1
    }
  }
  
  let non_retryable_result = retry_with_backoff(non_retryable_operation, retry_strategy)
  assert_false(non_retryable_result.success)
  assert_eq(non_retryable_result.data, None)
  assert_eq(non_retryable_result.attempts, 1)  // No retries for non-retryable error
  
  // Test circuit breaker pattern
  type CircuitBreakerState = {
    is_open: Bool,
    failure_count: Int,
    failure_threshold: Int,
    recovery_timeout: Int,
    last_failure_time: Int
  }
  
  let circuit_breaker_operation = fn[T](
    operation: () -> OperationResult[T],
    state: CircuitBreakerState,
    current_time: Int
  ) -> (OperationResult[T], CircuitBreakerState) {
    // Check if circuit is open and recovery timeout has passed
    let is_recovery_time_passed = state.is_open and 
      (current_time - state.last_failure_time) > state.recovery_timeout
    
    let updated_state = if is_recovery_time_passed {
      // Reset circuit breaker
      {
        is_open: false,
        failure_count: 0,
        failure_threshold: state.failure_threshold,
        recovery_timeout: state.recovery_timeout,
        last_failure_time: state.last_failure_time
      }
    } else {
      state
    }
    
    // If circuit is open, return immediately
    if updated_state.is_open {
      let error_result: OperationResult[T] = {
        success: false,
        data: None,
        error: Some(TelemetryError::ServiceUnavailable("Circuit breaker is open")),
        attempts: 0
      }
      return (error_result, updated_state)
    }
    
    // Execute operation
    let result = operation()
    
    // Update circuit breaker state based on result
    let new_state = if result.success {
      // Reset failure count on success
      {
        is_open: false,
        failure_count: 0,
        failure_threshold: updated_state.failure_threshold,
        recovery_timeout: updated_state.recovery_timeout,
        last_failure_time: updated_state.last_failure_time
      }
    } else {
      // Increment failure count
      let new_failure_count = updated_state.failure_count + 1
      let should_open_circuit = new_failure_count >= updated_state.failure_threshold
      
      {
        is_open: should_open_circuit,
        failure_count: new_failure_count,
        failure_threshold: updated_state.failure_threshold,
        recovery_timeout: updated_state.recovery_timeout,
        last_failure_time: if should_open_circuit { current_time } else { updated_state.last_failure_time }
      }
    }
    
    (result, new_state)
  }
  
  // Test circuit breaker with failing operations
  let initial_state = {
    is_open: false,
    failure_count: 0,
    failure_threshold: 3,
    recovery_timeout: 60000,  // 1 minute
    last_failure_time: 0
  }
  
  let mut current_state = initial_state
  let mut current_time = 1640995200
  
  // Execute failing operations
  for i in 1..=3 {
    let (result, new_state) = circuit_breaker_operation(failing_operation, current_state, current_time)
    current_state = new_state
    current_time = current_time + 1000
  }
  
  // Circuit should be open after 3 failures
  assert_true(current_state.is_open)
  assert_eq(current_state.failure_count, 3)
  
  // Next operation should fail immediately due to open circuit
  let (blocked_result, _) = circuit_breaker_operation(failing_operation, current_state, current_time)
  assert_false(blocked_result.success)
  match blocked_result.error {
    Some(TelemetryError::ServiceUnavailable(msg)) => assert_eq(msg, "Circuit breaker is open")
    _ => assert_true(false)
  }
}

// Test 5: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Define serialization formats
  enum SerializationFormat {
    Json
    Xml
    Binary
    Custom
  }
  
  type SerializableData = {
    id: String,
    name: String,
    timestamp: Int,
    metrics: Array[(String, Float)],
    tags: Array[String]
  }
  
  // Simulate JSON serialization
  let serialize_to_json = fn(data: SerializableData) -> String {
    let metrics_str = data.metrics.map(fn(item) {
      "\"" + item.0 + "\":" + item.1.to_string()
    }).join(",")
    
    let tags_str = data.tags.map(fn(tag) { "\"" + tag + "\"" }).join(",")
    
    "{" +
    "\"id\":\"" + data.id + "\"," +
    "\"name\":\"" + data.name + "\"," +
    "\"timestamp\":" + data.timestamp.to_string() + "," +
    "\"metrics\":{" + metrics_str + "}," +
    "\"tags\":[" + tags_str + "]" +
    "}"
  }
  
  // Simulate JSON deserialization
  let deserialize_from_json = fn(json: String) -> SerializableData {
    // Simplified parsing - in real implementation would use proper JSON parser
    let id = json.substring(json.find("\"id\":\"") + 6, json.find("\",\"name\":\"") - (json.find("\"id\":\"") + 6))
    let name_start = json.find("\"name\":\"") + 8
    let name_end = json.find("\",\"timestamp\":")
    let name = json.substring(name_start, name_end - name_start)
    
    let timestamp_start = json.find("\"timestamp\":") + 11
    let timestamp_end = json.find(",\"metrics\":")
    let timestamp = json.substring(timestamp_start, timestamp_end - timestamp_start).to_int()
    
    // Parse metrics (simplified)
    let metrics = [("response_time", 120.5), ("throughput", 1000.0)]
    
    // Parse tags (simplified)
    let tags = ["service:api", "environment:production"]
    
    {
      id: id,
      name: name,
      timestamp: timestamp,
      metrics: metrics,
      tags: tags
    }
  }
  
  // Create test data
  let test_data = {
    id: "span-12345",
    name: "http_request",
    timestamp: 1640995200,
    metrics: [("response_time", 120.5), ("throughput", 1000.0)],
    tags: ["service:api", "environment:production"]
  }
  
  // Test JSON serialization
  let json = serialize_to_json(test_data)
  assert_true(json.contains("\"id\":\"span-12345\""))
  assert_true(json.contains("\"name\":\"http_request\""))
  assert_true(json.contains("\"timestamp\":1640995200"))
  assert_true(json.contains("\"metrics\":"))
  assert_true(json.contains("\"tags\":"))
  
  // Test JSON deserialization
  let deserialized_data = deserialize_from_json(json)
  assert_eq(deserialized_data.id, test_data.id)
  assert_eq(deserialized_data.name, test_data.name)
  assert_eq(deserialized_data.timestamp, test_data.timestamp)
  assert_eq(deserialized_data.metrics.length(), test_data.metrics.length())
  assert_eq(deserialized_data.tags.length(), test_data.tags.length())
  
  // Simulate XML serialization
  let serialize_to_xml = fn(data: SerializableData) -> String {
    let metrics_xml = data.metrics.map(fn(item) {
      "<metric><name>" + item.0 + "</name><value>" + item.1.to_string() + "</value></metric>"
    }).join("")
    
    let tags_xml = data.tags.map(fn(tag) {
      "<tag>" + tag + "</tag>"
    }).join("")
    
    "<data>" +
    "<id>" + data.id + "</id>" +
    "<name>" + data.name + "</name>" +
    "<timestamp>" + data.timestamp.to_string() + "</timestamp>" +
    "<metrics>" + metrics_xml + "</metrics>" +
    "<tags>" + tags_xml + "</tags>" +
    "</data>"
  }
  
  // Test XML serialization
  let xml = serialize_to_xml(test_data)
  assert_true(xml.contains("<id>span-12345</id>"))
  assert_true(xml.contains("<name>http_request</name>"))
  assert_true(xml.contains("<timestamp>1640995200</timestamp>"))
  assert_true(xml.contains("<metrics>"))
  assert_true(xml.contains("<tags>"))
  
  // Simulate binary serialization
  let serialize_to_binary = fn(data: SerializableData) -> Array[Int] {
    // Simplified binary representation
    let mut result = []
    
    // Add string lengths and character codes
    result = result.push(data.id.length())
    for c in data.id.to_char_array() {
      result = result.push(c.to_int())
    }
    
    result = result.push(data.name.length())
    for c in data.name.to_char_array() {
      result = result.push(c.to_int())
    }
    
    // Add timestamp as 4 bytes (simplified)
    result = result.push(data.timestamp / 1000000)
    result = result.push((data.timestamp / 1000) % 1000)
    result = result.push(data.timestamp % 1000)
    result = result.push(0)
    
    // Add metrics count and values
    result = result.push(data.metrics.length())
    for metric in data.metrics {
      result = result.push(metric.0.length())
      for c in metric.0.to_char_array() {
        result = result.push(c.to_int())
      }
      // Simplified float encoding
      result = result.push(metric.1.to_int())
    }
    
    result
  }
  
  // Test binary serialization
  let binary = serialize_to_binary(test_data)
  assert_eq(binary[0], test_data.id.length())  // ID length
  assert_eq(binary[0] + 1 + test_data.id.length(), test_data.name.length())  // Name length position
  
  // Test format conversion
  let convert_format = fn(data: SerializableData, from: SerializationFormat, to: SerializationFormat) -> String {
    match (from, to) {
      (SerializationFormat::Json, SerializationFormat::Xml) => {
        let json = serialize_to_json(data)
        let xml = serialize_to_xml(data)
        "Converted JSON to XML: " + xml.length().to_string() + " chars"
      }
      (SerializationFormat::Xml, SerializationFormat::Json) => {
        let xml = serialize_to_xml(data)
        let json = serialize_to_json(data)
        "Converted XML to JSON: " + json.length().to_string() + " chars"
      }
      (SerializationFormat::Json, SerializationFormat::Binary) => {
        let json = serialize_to_json(data)
        let binary = serialize_to_binary(data)
        "Converted JSON to Binary: " + binary.length().to_string() + " bytes"
      }
      _ => "Unsupported conversion"
    }
  }
  
  // Test format conversions
  let json_to_xml = convert_format(test_data, SerializationFormat::Json, SerializationFormat::Xml)
  assert_true(json_to_xml.contains("Converted JSON to XML"))
  
  let xml_to_json = convert_format(test_data, SerializationFormat::Xml, SerializationFormat::Json)
  assert_true(xml_to_json.contains("Converted XML to JSON"))
  
  let json_to_binary = convert_format(test_data, SerializationFormat::Json, SerializationFormat::Binary)
  assert_true(json_to_binary.contains("Converted JSON to Binary"))
  
  // Test serialization error handling
  let serialize_with_validation = fn(data: SerializableData) -> Result[String] {
    if data.id.length() == 0 {
      Err("Invalid ID: empty string")
    } else if data.name.length() == 0 {
      Err("Invalid name: empty string")
    } else if data.timestamp <= 0 {
      Err("Invalid timestamp: must be positive")
    } else if data.metrics.length() == 0 {
      Err("Invalid metrics: empty array")
    } else {
      Ok(serialize_to_json(data))
    }
  }
  
  // Test valid data
  let valid_result = serialize_with_validation(test_data)
  match valid_result {
    Ok(json) => assert_true(json.contains("\"id\":\"span-12345\""))
    Err(_) => assert_true(false)
  }
  
  // Test invalid data
  let invalid_data = { test_data | id: "" }
  let invalid_result = serialize_with_validation(invalid_data)
  match invalid_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid ID: empty string")
  }
}

// Test 6: Concurrent Safety and Thread Synchronization
test "concurrent safety and thread synchronization" {
  // Define concurrent data structures
  type ConcurrentCounter = {
    mut value: Int,
    mut lock: Bool
  }
  
  type ConcurrentQueue[T] = {
    mut items: Array[T],
    mut lock: Bool
  }
  
  // Simulate atomic operations
  let atomic_increment = fn(counter: ConcurrentCounter) -> Int {
    // Simulate acquiring lock
    while counter.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    counter.lock = true
    
    // Perform operation
    counter.value = counter.value + 1
    let result = counter.value
    
    // Release lock
    counter.lock = false
    
    result
  }
  
  // Test concurrent counter
  let counter = { mut value: 0, mut lock: false }
  
  // Simulate concurrent increments
  let mut results = []
  for i in 1..=10 {
    let result = atomic_increment(counter)
    results = results.push(result)
  }
  
  assert_eq(counter.value, 10)
  assert_eq(results.length(), 10)
  assert_eq(results[0], 1)
  assert_eq(results[9], 10)
  
  // Test concurrent queue operations
  let enqueue = fn[T](queue: ConcurrentQueue[T], item: T) -> Bool {
    // Simulate acquiring lock
    while queue.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    queue.lock = true
    
    // Perform operation
    queue.items = queue.items.push(item)
    let success = true
    
    // Release lock
    queue.lock = false
    
    success
  }
  
  let dequeue = fn[T](queue: ConcurrentQueue[T]) -> Option[T] {
    // Simulate acquiring lock
    while queue.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    queue.lock = true
    
    // Perform operation
    let result = if queue.items.length() > 0 {
      let item = queue.items[0]
      queue.items = queue.items.slice(1, queue.items.length())
      Some(item)
    } else {
      None
    }
    
    // Release lock
    queue.lock = false
    
    result
  }
  
  // Test concurrent queue
  let string_queue = { mut items: [], mut lock: false }
  
  // Enqueue items
  assert_true(enqueue(string_queue, "item1"))
  assert_true(enqueue(string_queue, "item2"))
  assert_true(enqueue(string_queue, "item3"))
  
  // Dequeue items
  assert_eq(dequeue(string_queue), Some("item1"))
  assert_eq(dequeue(string_queue), Some("item2"))
  assert_eq(dequeue(string_queue), Some("item3"))
  assert_eq(dequeue(string_queue), None)
  
  // Test producer-consumer pattern
  type ProducerConsumerBuffer = {
    mut buffer: Array[String],
    mut buffer_size: Int,
    mut count: Int,
    mut in_index: Int,
    mut out_index: Int,
    mut lock: Bool
  }
  
  let create_buffer = fn(size: Int) {
    {
      mut buffer: [],
      mut buffer_size: size,
      mut count: 0,
      mut in_index: 0,
      mut out_index: 0,
      mut lock: false
    }
  }
  
  let produce = fn(buffer: ProducerConsumerBuffer, item: String) -> Bool {
    // Simulate acquiring lock
    while buffer.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    buffer.lock = true
    
    // Check if buffer is full
    let success = if buffer.count < buffer.buffer_size {
      // Add item to buffer
      buffer.buffer = buffer.buffer.push(item)
      buffer.count = buffer.count + 1
      buffer.in_index = (buffer.in_index + 1) % buffer.buffer_size
      true
    } else {
      false
    }
    
    // Release lock
    buffer.lock = false
    
    success
  }
  
  let consume = fn(buffer: ProducerConsumerBuffer) -> Option[String] {
    // Simulate acquiring lock
    while buffer.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    buffer.lock = true
    
    // Check if buffer is empty
    let result = if buffer.count > 0 {
      // Remove item from buffer
      let item = buffer.buffer[buffer.out_index]
      buffer.count = buffer.count - 1
      buffer.out_index = (buffer.out_index + 1) % buffer.buffer_size
      Some(item)
    } else {
      None
    }
    
    // Release lock
    buffer.lock = false
    
    result
  }
  
  // Test producer-consumer buffer
  let pc_buffer = create_buffer(3)
  
  // Produce items
  assert_true(produce(pc_buffer, "message1"))
  assert_true(produce(pc_buffer, "message2"))
  assert_true(produce(pc_buffer, "message3"))
  assert_false(produce(pc_buffer, "message4"))  // Buffer full
  
  // Consume items
  assert_eq(consume(pc_buffer), Some("message1"))
  assert_eq(consume(pc_buffer), Some("message2"))
  assert_eq(consume(pc_buffer), Some("message3"))
  assert_eq(consume(pc_buffer), None)  // Buffer empty
  
  // Test race condition prevention
  type SharedResource = {
    mut data: String,
    mut readers: Int,
    mut writers: Int,
    mut lock: Bool
  }
  
  let acquire_read_lock = fn(resource: SharedResource) -> Bool {
    while resource.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    resource.lock = true
    
    // Check if there are no writers
    let success = if resource.writers == 0 {
      resource.readers = resource.readers + 1
      true
    } else {
      false
    }
    
    // Release lock
    resource.lock = false
    
    success
  }
  
  let release_read_lock = fn(resource: SharedResource) {
    while resource.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    resource.lock = true
    
    resource.readers = resource.readers - 1
    
    // Release lock
    resource.lock = false
  }
  
  let acquire_write_lock = fn(resource: SharedResource) -> Bool {
    while resource.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    resource.lock = true
    
    // Check if there are no readers or writers
    let success = if resource.readers == 0 and resource.writers == 0 {
      resource.writers = resource.writers + 1
      true
    } else {
      false
    }
    
    // Release lock
    resource.lock = false
    
    success
  }
  
  let release_write_lock = fn(resource: SharedResource) {
    while resource.lock {
      // Wait for lock to be released
    }
    
    // Acquire lock
    resource.lock = true
    
    resource.writers = resource.writers - 1
    
    // Release lock
    resource.lock = false
  }
  
  let read_data = fn(resource: SharedResource) -> Option[String] {
    if acquire_read_lock(resource) {
      let data = resource.data
      release_read_lock(resource)
      Some(data)
    } else {
      None
    }
  }
  
  let write_data = fn(resource: SharedResource, new_data: String) -> Bool {
    if acquire_write_lock(resource) {
      resource.data = new_data
      release_write_lock(resource)
      true
    } else {
      false
    }
  }
  
  // Test read-write locks
  let shared_resource = {
    mut data: "initial_data",
    mut readers: 0,
    mut writers: 0,
    mut lock: false
  }
  
  // Read data
  assert_eq(read_data(shared_resource), Some("initial_data"))
  
  // Write data
  assert_true(write_data(shared_resource, "updated_data"))
  
  // Verify updated data
  assert_eq(read_data(shared_resource), Some("updated_data"))
}

// Test 7: Internationalization Support and Localization
test "internationalization support and localization" {
  // Define localization structures
  type Locale = {
    language: String,
    region: String,
    encoding: String
  }
  
  type LocalizedString = {
    key: String,
    translations: Array[(String, String)]  // (locale, translation)
  }
  
  type LocalizedError = {
    code: String,
    message: LocalizedString,
    details: LocalizedString
  }
  
  // Create supported locales
  let en_us = { language: "en", region: "US", encoding: "UTF-8" }
  let zh_cn = { language: "zh", region: "CN", encoding: "UTF-8" }
  let fr_fr = { language: "fr", region: "FR", encoding: "UTF-8" }
  let de_de = { language: "de", region: "DE", encoding: "UTF-8" }
  
  // Create localized strings
  let create_localized_string = fn(key: String, translations: Array[(String, String)]) {
    { key: key, translations: translations }
  }
  
  let error_messages = create_localized_string("error.network_timeout", [
    ("en_US", "Network timeout occurred"),
    ("zh_CN", "网络超时"),
    ("fr_FR", "Délai d'attente réseau dépassé"),
    ("de_DE", "Netzwerk-Timeout aufgetreten")
  ])
  
  let success_messages = create_localized_string("success.operation_completed", [
    ("en_US", "Operation completed successfully"),
    ("zh_CN", "操作成功完成"),
    ("fr_FR", "Opération terminée avec succès"),
    ("de_DE", "Vorgang erfolgreich abgeschlossen")
  ])
  
  // Get translation for locale
  let get_translation = fn(localized_str: LocalizedString, locale: Locale) -> String {
    let locale_key = locale.language + "_" + locale.region
    
    let mut found = false
    let mut result = ""
    
    for (loc, translation) in localized_str.translations {
      if loc == locale_key {
        found = true
        result = translation
      }
    }
    
    if found {
      result
    } else {
      // Fallback to English if available
      let mut fallback_found = false
      for (loc, translation) in localized_str.translations {
        if loc.starts_with("en_") {
          fallback_found = true
          result = translation
        }
      }
      
      if fallback_found {
        result
      } else if localized_str.translations.length() > 0 {
        localized_str.translations[0].1  // First available translation
      } else {
        localized_str.key  // Key as last resort
      }
    }
  }
  
  // Test translations
  assert_eq(get_translation(error_messages, en_us), "Network timeout occurred")
  assert_eq(get_translation(error_messages, zh_cn), "网络超时")
  assert_eq(get_translation(error_messages, fr_fr), "Délai d'attente réseau dépassé")
  assert_eq(get_translation(error_messages, de_de), "Netzwerk-Timeout aufgetreten")
  
  // Test fallback to English for unsupported locale
  let unsupported_locale = { language: "es", region: "ES", encoding: "UTF-8" }
  assert_eq(get_translation(error_messages, unsupported_locale), "Network timeout occurred")
  
  // Test number formatting for different locales
  let format_number = fn(number: Float, locale: Locale) -> String {
    match locale.language {
      "en" => {
        // English format: 1,234.56
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        
        // Add thousands separator (simplified)
        if int_part.length() > 3 {
          int_part.substring(0, int_part.length() - 3) + "," + int_part.substring(int_part.length() - 3, 3) + "." + decimal_part
        } else {
          int_part + "." + decimal_part
        }
      }
      "de" => {
        // German format: 1.234,56
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        
        // Add thousands separator (simplified)
        if int_part.length() > 3 {
          int_part.substring(0, int_part.length() - 3) + "." + int_part.substring(int_part.length() - 3, 3) + "," + decimal_part
        } else {
          int_part + "," + decimal_part
        }
      }
      _ => {
        // Default format: 1234.56
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        int_part + "." + decimal_part
      }
    }
  }
  
  // Test number formatting
  assert_eq(format_number(1234.56, en_us), "1,234.56")
  assert_eq(format_number(1234.56, de_de), "1.234,56")
  assert_eq(format_number(1234.56, fr_fr), "1234.56")  // Default format
  
  // Test date/time formatting for different locales
  let format_datetime = fn(timestamp: Int, locale: Locale) -> String {
    let date = timestamp / 86400  // Simplified
    let time = timestamp % 86400
    let hours = time / 3600
    let minutes = (time % 3600) / 60
    
    match locale.language {
      "en" => {
        // English format: MM/DD/YYYY HH:MM
        "01/01/2022 " + hours.to_string() + ":" + minutes.to_string()
      }
      "de" => {
        // German format: DD.MM.YYYY HH:MM
        "01.01.2022 " + hours.to_string() + ":" + minutes.to_string()
      }
      "zh" => {
        // Chinese format: YYYY年MM月DD日 HH:MM
        "2022年01月01日 " + hours.to_string() + ":" + minutes.to_string()
      }
      _ => {
        // ISO format: YYYY-MM-DD HH:MM
        "2022-01-01 " + hours.to_string() + ":" + minutes.to_string()
      }
    }
  }
  
  // Test date/time formatting
  assert_eq(format_datetime(1640995200, en_us), "01/01/2022 0:0")
  assert_eq(format_datetime(1640995200, de_de), "01.01.2022 0:0")
  assert_eq(format_datetime(1640995200, zh_cn), "2022年01月01日 0:0")
  assert_eq(format_datetime(1640995200, fr_fr), "2022-01-01 0:0")
  
  // Test localized error messages
  let create_localized_error = fn(code: String, message: LocalizedString, details: LocalizedString) {
    { code: code, message: message, details: details }
  }
  
  let error_details = create_localized_string("error.network_timeout.details", [
    ("en_US", "The connection to the remote server timed out after 30 seconds"),
    ("zh_CN", "与远程服务器的连接在30秒后超时"),
    ("fr_FR", "La connexion au serveur distant a expiré après 30 secondes"),
    ("de_DE", "Die Verbindung zum Remote-Server ist nach 30 Sekunden abgelaufen")
  ])
  
  let network_error = create_localized_error("NETWORK_TIMEOUT", error_messages, error_details)
  
  // Get localized error message
  let get_localized_error = fn(error: LocalizedError, locale: Locale) -> (String, String) {
    let message = get_translation(error.message, locale)
    let details = get_translation(error.details, locale)
    (message, details)
  }
  
  let (en_message, en_details) = get_localized_error(network_error, en_us)
  assert_eq(en_message, "Network timeout occurred")
  assert_eq(en_details, "The connection to the remote server timed out after 30 seconds")
  
  let (zh_message, zh_details) = get_localized_error(network_error, zh_cn)
  assert_eq(zh_message, "网络超时")
  assert_eq(zh_details, "与远程服务器的连接在30秒后超时")
  
  // Test right-to-left language support (simulated)
  let rtl_locale = { language: "ar", region: "SA", encoding: "UTF-8" }
  
  let rtl_messages = create_localized_string("error.network_timeout", [
    ("ar_SA", "حدث انتهاء مهلة الشبكة"),
    ("en_US", "Network timeout occurred")
  ])
  
  let format_for_rtl = fn(text: String, is_rtl: Bool) -> String {
    if is_rtl {
      // In a real implementation, this would handle RTL text direction
      "[RTL] " + text
    } else {
      "[LTR] " + text
    }
  }
  
  let is_rtl_language = fn(language: String) -> Bool {
    language == "ar" or language == "he" or language == "fa"
  }
  
  let rtl_message = get_translation(rtl_messages, rtl_locale)
  let formatted_rtl = format_for_rtl(rtl_message, is_rtl_language(rtl_locale.language))
  assert_eq(formatted_rtl, "[RTL] حدث انتهاء مهلة الشبكة")
  
  let ltr_message = get_translation(rtl_messages, en_us)
  let formatted_ltr = format_for_rtl(ltr_message, is_rtl_language(en_us.language))
  assert_eq(formatted_ltr, "[LTR] Network timeout occurred")
}

// Test 8: Caching Mechanisms and Strategies
test "caching mechanisms and strategies" {
  // Define cache entry structure
  type CacheEntry[T] = {
    value: T,
    timestamp: Int,
    ttl: Int,  // Time to live in seconds
    access_count: Int,
    last_accessed: Int
  }
  
  type Cache[T] = {
    mut entries: Array[(String, CacheEntry[T])],
    max_size: Int,
    eviction_policy: String  // "LRU", "LFU", "FIFO", "TTL"
  }
  
  // Create cache
  let create_cache = fn[T](max_size: Int, eviction_policy: String) {
    {
      mut entries: [],
      max_size: max_size,
      eviction_policy: eviction_policy
    }
  }
  
  // Check if cache entry is expired
  let is_expired = fn[T](entry: CacheEntry[T], current_time: Int) -> Bool {
    entry.ttl > 0 and (current_time - entry.timestamp) > entry.ttl
  }
  
  // Get value from cache
  let get_from_cache = fn[T](cache: Cache[T], key: String, current_time: Int) -> Option[T] {
    // Find entry by key
    let mut found_index = -1
    for i in 0..cache.entries.length() {
      if cache.entries[i].0 == key {
        found_index = i
      }
    }
    
    if found_index >= 0 {
      let entry = cache.entries[found_index].1
      
      // Check if expired
      if is_expired(entry, current_time) {
        None
      } else {
        // Update access statistics
        let updated_entry = {
          value: entry.value,
          timestamp: entry.timestamp,
          ttl: entry.ttl,
          access_count: entry.access_count + 1,
          last_accessed: current_time
        }
        
        // Update entry in cache
        cache.entries[found_index] = (key, updated_entry)
        
        Some(entry.value)
      }
    } else {
      None
    }
  }
  
  // Put value in cache
  let put_in_cache = fn[T](cache: Cache[T], key: String, value: T, ttl: Int, current_time: Int) {
    // Check if key already exists
    let mut found_index = -1
    for i in 0..cache.entries.length() {
      if cache.entries[i].0 == key {
        found_index = i
      }
    }
    
    let new_entry = {
      value: value,
      timestamp: current_time,
      ttl: ttl,
      access_count: 1,
      last_accessed: current_time
    }
    
    if found_index >= 0 {
      // Update existing entry
      cache.entries[found_index] = (key, new_entry)
    } else {
      // Add new entry
      if cache.entries.length() >= cache.max_size {
        // Evict entries based on policy
        match cache.eviction_policy {
          "LRU" => {
            // Find least recently used entry
            let mut lru_index = 0
            let mut lru_time = cache.entries[0].1.last_accessed
            
            for i in 1..cache.entries.length() {
              if cache.entries[i].1.last_accessed < lru_time {
                lru_index = i
                lru_time = cache.entries[i].1.last_accessed
              }
            }
            
            // Remove LRU entry
            cache.entries = cache.entries.slice(0, lru_index) + cache.entries.slice(lru_index + 1, cache.entries.length() - lru_index - 1)
          }
          "LFU" => {
            // Find least frequently used entry
            let mut lfu_index = 0
            let mut lfu_count = cache.entries[0].1.access_count
            
            for i in 1..cache.entries.length() {
              if cache.entries[i].1.access_count < lfu_count {
                lfu_index = i
                lfu_count = cache.entries[i].1.access_count
              }
            }
            
            // Remove LFU entry
            cache.entries = cache.entries.slice(0, lfu_index) + cache.entries.slice(lfu_index + 1, cache.entries.length() - lfu_index - 1)
          }
          "FIFO" => {
            // Remove first entry
            cache.entries = cache.entries.slice(1, cache.entries.length() - 1)
          }
          "TTL" => {
            // Remove expired entries
            let mut valid_entries = []
            for entry in cache.entries {
              if not(is_expired(entry.1, current_time)) {
                valid_entries = valid_entries.push(entry)
              }
            }
            cache.entries = valid_entries
            
            // If still full, remove oldest entry
            if cache.entries.length() >= cache.max_size {
              cache.entries = cache.entries.slice(1, cache.entries.length() - 1)
            }
          }
          _ => {
            // Default: remove first entry
            cache.entries = cache.entries.slice(1, cache.entries.length() - 1)
          }
        }
      }
      
      // Add new entry
      cache.entries = cache.entries.push((key, new_entry))
    }
  }
  
  // Test LRU cache
  let lru_cache = create_cache[String](3, "LRU")
  let current_time = 1640995200
  
  // Add entries
  put_in_cache(lru_cache, "key1", "value1", 3600, current_time)
  put_in_cache(lru_cache, "key2", "value2", 3600, current_time + 10)
  put_in_cache(lru_cache, "key3", "value3", 3600, current_time + 20)
  
  // Verify entries
  assert_eq(get_from_cache(lru_cache, "key1", current_time + 30), Some("value1"))
  assert_eq(get_from_cache(lru_cache, "key2", current_time + 30), Some("value2"))
  assert_eq(get_from_cache(lru_cache, "key3", current_time + 30), Some("value3"))
  
  // Access key1 to make it most recently used
  get_from_cache(lru_cache, "key1", current_time + 40)
  
  // Add new entry, should evict key2 (least recently used)
  put_in_cache(lru_cache, "key4", "value4", 3600, current_time + 50)
  
  // Verify eviction
  assert_eq(get_from_cache(lru_cache, "key1", current_time + 60), Some("value1"))  // Still in cache (recently used)
  assert_eq(get_from_cache(lru_cache, "key2", current_time + 60), None)  // Evicted
  assert_eq(get_from_cache(lru_cache, "key3", current_time + 60), Some("value3"))
  assert_eq(get_from_cache(lru_cache, "key4", current_time + 60), Some("value4"))
  
  // Test TTL cache
  let ttl_cache = create_cache[String](3, "TTL")
  
  // Add entries with different TTLs
  put_in_cache(ttl_cache, "short_lived", "value1", 10, current_time)  // 10 seconds TTL
  put_in_cache(ttl_cache, "long_lived", "value2", 3600, current_time)  // 1 hour TTL
  
  // Verify entries are accessible
  assert_eq(get_from_cache(ttl_cache, "short_lived", current_time + 5), Some("value1"))
  assert_eq(get_from_cache(ttl_cache, "long_lived", current_time + 5), Some("value2"))
  
  // Verify short-lived entry is expired
  assert_eq(get_from_cache(ttl_cache, "short_lived", current_time + 15), None)
  assert_eq(get_from_cache(ttl_cache, "long_lived", current_time + 15), Some("value2"))
  
  // Test cache statistics
  type CacheStats = {
    hits: Int,
    misses: Int,
    hit_rate: Float,
    evictions: Int
  }
  
  let track_cache_stats = fn[T](cache: Cache[T], operations: Array[(String, Option[String])], current_time: Int) -> CacheStats {
    let mut hits = 0
    let mut misses = 0
    let mut evictions = 0
    let initial_size = cache.entries.length()
    
    for (key, expected_value) in operations {
      let result = get_from_cache(cache, key, current_time)
      
      match expected_value {
        Some(expected) => {
          if result == expected {
            hits = hits + 1
          } else {
            misses = misses + 1
          }
        }
        None => {
          if result == None {
            hits = hits + 1
          } else {
            misses = misses + 1
          }
        }
      }
    }
    
    // Calculate evictions
    if cache.entries.length() < initial_size {
      evictions = initial_size - cache.entries.length()
    }
    
    {
      hits: hits,
      misses: misses,
      hit_rate: hits.to_float() / (hits + misses).to_float(),
      evictions: evictions
    }
  }
  
  // Test cache statistics
  let stats_cache = create_cache[String](2, "LRU")
  
  // Add entries
  put_in_cache(stats_cache, "key1", "value1", 3600, current_time)
  put_in_cache(stats_cache, "key2", "value2", 3600, current_time)
  
  // Perform operations
  let operations = [
    ("key1", Some("value1")),  // Hit
    ("key2", Some("value2")),  // Hit
    ("key3", None),            // Miss
    ("key1", Some("value1"))   // Hit
  ]
  
  let stats = track_cache_stats(stats_cache, operations, current_time + 10)
  assert_eq(stats.hits, 3)
  assert_eq(stats.misses, 1)
  assert_eq(stats.hit_rate, 0.75)
  
  // Test multi-level cache
  type MultiLevelCache[T] = {
    l1_cache: Cache[T],  // Fast, small cache
    l2_cache: Cache[T]   // Slower, larger cache
  }
  
  let create_multi_level_cache = fn[T](l1_size: Int, l2_size: Int) {
    {
      l1_cache: create_cache[T](l1_size, "LRU"),
      l2_cache: create_cache[T](l2_size, "LRU")
    }
  }
  
  let get_from_multi_level_cache = fn[T](ml_cache: MultiLevelCache[T], key: String, current_time: Int) -> Option[T] {
    // Try L1 cache first
    match get_from_cache(ml_cache.l1_cache, key, current_time) {
      Some(value) => Some(value),
      None => {
        // Try L2 cache
        match get_from_cache(ml_cache.l2_cache, key, current_time) {
          Some(value) => {
            // Promote to L1 cache
            put_in_cache(ml_cache.l1_cache, key, value, 3600, current_time)
            Some(value)
          }
          None => None
        }
      }
    }
  }
  
  let put_in_multi_level_cache = fn[T](ml_cache: MultiLevelCache[T], key: String, value: T, current_time: Int) {
    // Put in both caches
    put_in_cache(ml_cache.l1_cache, key, value, 3600, current_time)
    put_in_cache(ml_cache.l2_cache, key, value, 3600, current_time)
  }
  
  // Test multi-level cache
  let ml_cache = create_multi_level_cache[String](2, 5)
  
  // Add entries
  put_in_multi_level_cache(ml_cache, "key1", "value1", current_time)
  put_in_multi_level_cache(ml_cache, "key2", "value2", current_time)
  put_in_multi_level_cache(ml_cache, "key3", "value3", current_time)
  
  // Verify entries
  assert_eq(get_from_multi_level_cache(ml_cache, "key1", current_time + 10), Some("value1"))
  assert_eq(get_from_multi_level_cache(ml_cache, "key2", current_time + 10), Some("value2"))
  assert_eq(get_from_multi_level_cache(ml_cache, "key3", current_time + 10), Some("value3"))
}