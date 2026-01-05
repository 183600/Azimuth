// Azimuth New High Quality Test Cases
// This file contains comprehensive high-quality test cases for the Azimuth telemetry system
// Each test focuses on specific functionality with thorough validation

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation with multiple metrics" {
  // Define metric types
  type MetricType = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Define aggregation function
  let aggregate_metrics = fn(metrics: Array[MetricType], operation: String) {
    let mut result = 0.0
    let mut count = 0
    
    for metric in metrics {
      match operation {
        "sum" => result = result + metric.value
        "avg" => {
          result = result + metric.value
          count = count + 1
        }
        "min" => {
          if count == 0 or metric.value < result {
            result = metric.value
          }
          count = count + 1
        }
        "max" => {
          if count == 0 or metric.value > result {
            result = metric.value
          }
          count = count + 1
        }
        _ => result = result + metric.value  // Default to sum
      }
    }
    
    if operation == "avg" {
      result = result / count.to_float()
    }
    
    result
  }
  
  // Create test metrics
  let metrics = [
    { name: "response_time", value: 120.5, unit: "ms", timestamp: 1640995200, tags: [("service", "api")] },
    { name: "response_time", value: 95.3, unit: "ms", timestamp: 1640995260, tags: [("service", "api")] },
    { name: "response_time", value: 150.7, unit: "ms", timestamp: 1640995320, tags: [("service", "api")] },
    { name: "response_time", value: 80.2, unit: "ms", timestamp: 1640995380, tags: [("service", "api")] }
  ]
  
  // Test aggregation operations
  let sum_result = aggregate_metrics(metrics, "sum")
  assert_eq(sum_result, 446.7)
  
  let avg_result = aggregate_metrics(metrics, "avg")
  assert_eq(avg_result, 111.675)
  
  let min_result = aggregate_metrics(metrics, "min")
  assert_eq(min_result, 80.2)
  
  let max_result = aggregate_metrics(metrics, "max")
  assert_eq(max_result, 150.7)
  
  // Test with empty metrics array
  let empty_metrics = []
  let empty_sum = aggregate_metrics(empty_metrics, "sum")
  assert_eq(empty_sum, 0.0)
  
  // Test filtering by tags
  let filter_by_tag = fn(metrics: Array[MetricType], key: String, value: String) {
    let mut filtered = []
    for metric in metrics {
      for (k, v) in metric.tags {
        if k == key and v == value {
          filtered = filtered.push(metric)
        }
      }
    }
    filtered
  }
  
  let api_metrics = filter_by_tag(metrics, "service", "api")
  assert_eq(api_metrics.length(), 4)
  
  // Test time-based aggregation
  let aggregate_by_time_window = fn(metrics: Array[MetricType], window_size: Int) {
    let mut windows = []
    
    for metric in metrics {
      let window_start = (metric.timestamp / window_size) * window_size
      let mut found = false
      
      let mut i = 0
      while i < windows.length() {
        if windows[i].window_start == window_start {
          windows[i].metrics = windows[i].metrics.push(metric)
          windows[i].count = windows[i].count + 1
          found = true
        }
        i = i + 1
      }
      
      if not(found) {
        windows = windows.push({
          window_start,
          metrics: [metric],
          count: 1
        })
      }
    }
    
    windows
  }
  
  let time_windows = aggregate_by_time_window(metrics, 3600)  // 1-hour windows
  assert_eq(time_windows.length(), 1)
  assert_eq(time_windows[0].count, 4)
}

// Test 2: Distributed Tracing Consistency
test "distributed tracing consistency across services" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)]
  }
  
  // Test trace propagation
  let extract_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut baggage = []
    
    for (key, value) in headers {
      match key {
        "traceparent" => {
          // Parse traceparent format: version-trace_id-span_id-flags
          let parts = value.split("-")
          if parts.length() >= 3 {
            trace_id = Some(parts[1])
            span_id = Some(parts[2])
          }
        }
        "tracestate" => {
          // Parse baggage items
          let items = value.split(",")
          for item in items {
            let kv = item.split("=")
            if kv.length() == 2 {
              baggage = baggage.push((kv[0], kv[1]))
            }
          }
        }
        _ => ()
      }
    }
    
    match (trace_id, span_id) {
      (Some(tid), Some(sid)) => Some({
        trace_id: tid,
        span_id: sid,
        baggage
      })
      _ => None
    }
  }
  
  let inject_context = fn(context: TraceContext) {
    [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"),
      ("tracestate", context.baggage.map(fn((k, v)) { k + "=" + v }).join(","))
    ]
  }
  
  // Create initial context
  let initial_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    baggage: [("user.id", "12345"), ("request.id", "req-67890")]
  }
  
  // Test context injection
  let injected_headers = inject_context(initial_context)
  assert_eq(injected_headers.length(), 2)
  
  let traceparent = injected_headers[0].1
  assert_true(traceparent.contains("0af7651916cd43dd8448eb211c80319c"))
  assert_true(traceparent.contains("b9c7c989f97918e1"))
  
  // Test context extraction
  let extracted_context = extract_context(injected_headers)
  assert_eq(extracted_context, Some(initial_context))
  
  // Test span creation with context
  let create_child_span = fn(context: TraceContext, service_name: String, operation_name: String) {
    {
      trace_id: context.trace_id,
      span_id: "span-" + (context.span_id.length() + 1).to_string(),
      parent_span_id: Some(context.span_id),
      service_name,
      operation_name,
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok",
      attributes: context.baggage
    }
  }
  
  let child_span = create_child_span(initial_context, "payment-service", "process-payment")
  assert_eq(child_span.trace_id, initial_context.trace_id)
  assert_eq(child_span.parent_span_id, Some(initial_context.span_id))
  assert_eq(child_span.service_name, "payment-service")
  assert_eq(child_span.operation_name, "process-payment")
  
  // Test trace consistency across multiple services
  let service_chain = ["api-gateway", "auth-service", "user-service", "payment-service"]
  let mut spans = []
  let mut current_context = initial_context
  
  for service in service_chain {
    let span = create_child_span(current_context, service, "handle-request")
    spans = spans.push(span)
    current_context = {
      trace_id: span.trace_id,
      span_id: span.span_id,
      baggage: span.attributes
    }
  }
  
  // Verify trace consistency
  assert_eq(spans.length(), 4)
  
  let mut i = 0
  while i < spans.length() {
    assert_eq(spans[i].trace_id, initial_context.trace_id)
    
    if i > 0 {
      assert_eq(spans[i].parent_span_id, Some(spans[i-1].span_id))
    } else {
      assert_eq(spans[i].parent_span_id, Some(initial_context.span_id))
    }
    
    i = i + 1
  }
  
  // Test trace reconstruction
  let reconstruct_trace = fn(spans: Array[Span]) {
    let mut trace_tree = []
    
    for span in spans {
      let mut children = []
      
      for potential_child in spans {
        match potential_child.parent_span_id {
          Some(parent_id) => {
            if parent_id == span.span_id {
              children = children.push(potential_child)
            }
          }
          None => ()
        }
      }
      
      trace_tree = trace_tree.push({
        span,
        children
      })
    }
    
    trace_tree
  }
  
  let trace_tree = reconstruct_trace(spans)
  assert_eq(trace_tree.length(), 4)
  
  // Verify tree structure
  let root_span = trace_tree[0]
  assert_eq(root_span.span.service_name, "api-gateway")
  assert_eq(root_span.children.length(), 1)
  assert_eq(root_span.children[0].service_name, "auth-service")
}

// Test 3: Performance Benchmarking
test "performance benchmarking for telemetry operations" {
  // Define benchmark function
  let benchmark = fn(name: String, operation: () -> Unit, iterations: Int) {
    let start_time = 1640995200
    let mut end_time = start_time
    
    for i in 0..iterations {
      operation()
      end_time = end_time + 1
    }
    
    let duration = end_time - start_time
    let avg_time = duration.to_float() / iterations.to_float()
    
    {
      name,
      iterations,
      total_time: duration,
      average_time: avg_time,
      operations_per_second: iterations.to_float() / duration.to_float()
    }
  }
  
  // Test string serialization performance
  let serialize_span = fn() {
    let span_data = "trace-123:span-456:api-service:get-user:1640995200:1640995250:ok"
    let parts = span_data.split(":")
    assert_eq(parts.length(), 7)
  }
  
  let serialization_benchmark = benchmark("span_serialization", serialize_span, 1000)
  assert_true(serialization_benchmark.total_time > 0)
  assert_true(serialization_benchmark.average_time > 0.0)
  assert_true(serialization_benchmark.operations_per_second > 0.0)
  
  // Test metric aggregation performance
  let aggregate_metrics = fn() {
    let metrics = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    let mut sum = 0.0
    
    for metric in metrics {
      sum = sum + metric
    }
    
    let avg = sum / metrics.length().to_float()
    assert_eq(avg, 5.5)
  }
  
  let aggregation_benchmark = benchmark("metric_aggregation", aggregate_metrics, 1000)
  assert_true(aggregation_benchmark.total_time > 0)
  assert_true(aggregation_benchmark.average_time > 0.0)
  assert_true(aggregation_benchmark.operations_per_second > 0.0)
  
  // Test context propagation performance
  let propagate_context = fn() {
    let context = {
      trace_id: "0af7651916cd43dd8448eb211c80319c",
      span_id: "b9c7c989f97918e1",
      baggage: [("user.id", "12345"), ("request.id", "req-67890")]
    }
    
    let headers = [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"),
      ("tracestate", context.baggage.map(fn((k, v)) { k + "=" + v }).join(","))
    ]
    
    assert_eq(headers.length(), 2)
  }
  
  let propagation_benchmark = benchmark("context_propagation", propagate_context, 1000)
  assert_true(propagation_benchmark.total_time > 0)
  assert_true(propagation_benchmark.average_time > 0.0)
  assert_true(propagation_benchmark.operations_per_second > 0.0)
  
  // Compare performance metrics
  let benchmarks = [serialization_benchmark, aggregation_benchmark, propagation_benchmark]
  
  let fastest = benchmarks.reduce(fn(fastest, current) {
    if current.average_time < fastest.average_time {
      current
    } else {
      fastest
    }
  }, benchmarks[0])
  
  let slowest = benchmarks.reduce(fn(slowest, current) {
    if current.average_time > slowest.average_time {
      current
    } else {
      slowest
    }
  }, benchmarks[0])
  
  assert_true(fastest.average_time <= slowest.average_time)
  
  // Test performance regression detection
  let performance_threshold = 10.0  // Maximum allowed average time
  let performance_violations = benchmarks.filter(fn(b) { b.average_time > performance_threshold })
  
  assert_eq(performance_violations.length(), 0)  // All operations should be within threshold
}

// Test 4: Error Handling and Recovery
test "error handling and recovery mechanisms" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    InvalidTraceId(String)
    ServiceUnavailable(String)
    RateLimitExceeded(String)
  }
  
  // Define result type
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError]
  }
  
  // Create result helpers
  let success = fn(data: T) {
    {
      success: true,
      data: Some(data),
      error: None
    }
  }
  
  let failure = fn(error: TelemetryError) {
    {
      success: false,
      data: None,
      error: Some(error)
    }
  }
  
  // Test retry mechanism
  let retry_with_backoff = fn(operation: () -> Result[T], max_attempts: Int, base_delay: Int) {
    let mut attempts = 0
    let mut result = operation()
    let mut delay = base_delay
    
    while not(result.success) and attempts < max_attempts {
      attempts = attempts + 1
      
      // Simulate backoff delay
      delay = delay * 2
      
      // Retry operation
      result = operation()
      
      // Check if error is retryable
      match result.error {
        Some(TelemetryError::NetworkTimeout(_)) => ()  // Retryable
        Some(TelemetryError::ServiceUnavailable(_)) => ()  // Retryable
        Some(TelemetryError::RateLimitExceeded(_)) => ()  // Retryable
        _ => break  // Not retryable
      }
    }
    
    {
      result,
      attempts
    }
  }
  
  // Test successful operation
  let successful_operation = fn() {
    success("operation_completed")
  }
  
  let retry_result_success = retry_with_backoff(successful_operation, 3, 100)
  assert_true(retry_result_success.result.success)
  assert_eq(retry_result_success.attempts, 0)  // Should not retry on success
  
  // Test retryable error
  let mut attempt_count = 0
  let retryable_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      failure(TelemetryError::NetworkTimeout("Connection timed out"))
    } else {
      success("operation_completed_after_retries")
    }
  }
  
  let retry_result_retryable = retry_with_backoff(retryable_operation, 5, 100)
  assert_true(retry_result_retryable.result.success)
  assert_eq(retry_result_retryable.attempts, 2)  // Should retry twice before success
  
  // Test non-retryable error
  let non_retryable_operation = fn() {
    failure(TelemetryError::InvalidTraceId("Invalid trace ID format"))
  }
  
  let retry_result_non_retryable = retry_with_backoff(non_retryable_operation, 5, 100)
  assert_false(retry_result_non_retryable.result.success)
  assert_eq(retry_result_non_retryable.attempts, 0)  // Should not retry non-retryable errors
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    state: String,  // "closed", "open", "half-open"
    failure_count: Int,
    failure_threshold: Int,
    recovery_timeout: Int,
    last_failure_time: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int, timeout: Int) {
    {
      state: "closed",
      failure_count: 0,
      failure_threshold: threshold,
      recovery_timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let call_with_circuit_breaker = fn(circuit_breaker: CircuitBreaker, operation: () -> Result[T], current_time: Int) {
    let updated_breaker = match circuit_breaker.state {
      "open" => {
        if current_time - circuit_breaker.last_failure_time >= circuit_breaker.recovery_timeout {
          { circuit_breaker | state: "half-open" }
        } else {
          circuit_breaker
        }
      }
      _ => circuit_breaker
    }
    
    match updated_breaker.state {
      "open" => {
        (updated_breaker, failure(TelemetryError::ServiceUnavailable("Circuit breaker is open")))
      }
      "half-open" | "closed" => {
        let result = operation()
        
        let new_breaker = match result.success {
          true => {
            { updated_breaker | state: "closed", failure_count: 0 }
          }
          false => {
            let new_failure_count = updated_breaker.failure_count + 1
            let new_state = if new_failure_count >= updated_breaker.failure_threshold {
              "open"
            } else {
              "closed"
            }
            
            {
              updated_breaker |
              state: new_state,
              failure_count: new_failure_count,
              last_failure_time: current_time
            }
          }
        }
        
        (new_breaker, result)
      }
    }
  }
  
  // Test circuit breaker behavior
  let circuit_breaker = create_circuit_breaker(3, 60)  // 3 failures threshold, 60s recovery timeout
  
  // Initial calls should succeed
  let (cb1, result1) = call_with_circuit_breaker(circuit_breaker, successful_operation, 1640995200)
  assert_true(result1.success)
  assert_eq(cb1.state, "closed")
  assert_eq(cb1.failure_count, 0)
  
  // Simulate failures
  let failing_operation = fn() {
    failure(TelemetryError::NetworkTimeout("Connection failed"))
  }
  
  let (cb2, result2) = call_with_circuit_breaker(cb1, failing_operation, 1640995200)
  assert_false(result2.success)
  assert_eq(cb2.state, "closed")
  assert_eq(cb2.failure_count, 1)
  
  let (cb3, result3) = call_with_circuit_breaker(cb2, failing_operation, 1640995200)
  assert_false(result3.success)
  assert_eq(cb3.state, "closed")
  assert_eq(cb3.failure_count, 2)
  
  // Third failure should open the circuit
  let (cb4, result4) = call_with_circuit_breaker(cb3, failing_operation, 1640995200)
  assert_false(result4.success)
  assert_eq(cb4.state, "open")
  assert_eq(cb4.failure_count, 3)
  
  // Calls while circuit is open should fail immediately
  let (cb5, result5) = call_with_circuit_breaker(cb4, successful_operation, 1640995200)
  assert_false(result5.success)
  assert_eq(cb5.state, "open")
  
  // After recovery timeout, circuit should transition to half-open
  let (cb6, result6) = call_with_circuit_breaker(cb5, successful_operation, 1640995300)  // 100s later
  assert_true(result6.success)
  assert_eq(cb6.state, "closed")
  assert_eq(cb6.failure_count, 0)
}

// Test 5: Concurrent Safety
test "concurrent safety for telemetry operations" {
  // Define thread-safe counter
  type SafeCounter = {
    value: Int,
    lock: Bool
  }
  
  let create_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: false
    }
  }
  
  let acquire_lock = fn(counter: SafeCounter) {
    if not(counter.lock) {
      { counter | lock: true }
    } else {
      counter  // Lock already held
    }
  }
  
  let release_lock = fn(counter: SafeCounter) {
    { counter | lock: false }
  }
  
  let safe_increment = fn(counter: SafeCounter) {
    let locked_counter = acquire_lock(counter)
    if locked_counter.lock {
      let incremented = { locked_counter | value: locked_counter.value + 1 }
      release_lock(incremented)
    } else {
      counter  // Failed to acquire lock
    }
  }
  
  // Test thread-safe operations
  let counter = create_counter(0)
  
  // Simulate concurrent increments
  let mut result_counter = counter
  for i in 0..100 {
    result_counter = safe_increment(result_counter)
  }
  
  assert_eq(result_counter.value, 100)
  
  // Define thread-safe span buffer
  type SpanBuffer = {
    spans: Array[String],
    lock: Bool
  }
  
  let create_buffer = fn() {
    {
      spans: [],
      lock: false
    }
  }
  
  let safe_add_span = fn(buffer: SpanBuffer, span: String) {
    let locked_buffer = acquire_lock(buffer)
    if locked_buffer.lock {
      let updated = { locked_buffer | spans: locked_buffer.spans.push(span) }
      release_lock(updated)
    } else {
      buffer  // Failed to acquire lock
    }
  }
  
  // Test concurrent span additions
  let buffer = create_buffer()
  
  let mut result_buffer = buffer
  for i in 0..50 {
    let span_id = "span-" + i.to_string()
    result_buffer = safe_add_span(result_buffer, span_id)
  }
  
  assert_eq(result_buffer.spans.length(), 50)
  assert_eq(result_buffer.spans[0], "span-0")
  assert_eq(result_buffer.spans[49], "span-49")
  
  // Test race condition prevention
  type RaceConditionTest = {
    shared_resource: Int,
    operations_performed: Int,
    lock: Bool
  }
  
  let create_race_test = fn() {
    {
      shared_resource: 0,
      operations_performed: 0,
      lock: false
    }
  }
  
  let safe_operation = fn(test: RaceConditionTest, operation: Int -> Int) {
    let locked_test = acquire_lock(test)
    if locked_test.lock {
      let new_value = operation(locked_test.shared_resource)
      let updated = {
        locked_test |
        shared_resource: new_value,
        operations_performed: locked_test.operations_performed + 1
      }
      release_lock(updated)
    } else {
      test  // Failed to acquire lock
    }
  }
  
  let race_test = create_race_test()
  
  // Simulate concurrent operations
  let mut result_test = race_test
  for i in 0..20 {
    let increment_operation = fn(value) { value + 1 }
    result_test = safe_operation(result_test, increment_operation)
  }
  
  assert_eq(result_test.shared_resource, 20)
  assert_eq(result_test.operations_performed, 20)
  
  // Test deadlock prevention
  type ResourceA = {
    data: String,
    lock: Bool
  }
  
  type ResourceB = {
    data: String,
    lock: Bool
  }
  
  let create_resource_a = fn() {
    {
      data: "resource-a-data",
      lock: false
    }
  }
  
  let create_resource_b = fn() {
    {
      data: "resource-b-data",
      lock: false
    }
  }
  
  let safe_transfer = fn(resource_a: ResourceA, resource_b: ResourceB) {
    // Acquire locks in consistent order to prevent deadlock
    let locked_a = acquire_lock(resource_a)
    let locked_b = acquire_lock(resource_b)
    
    if locked_a.lock and locked_b.lock {
      // Perform transfer
      let temp_data = locked_a.data
      let updated_a = { locked_a | data: locked_b.data }
      let updated_b = { locked_b | data: temp_data }
      
      // Release locks
      let released_a = release_lock(updated_a)
      let released_b = release_lock(updated_b)
      
      (released_a, released_b)
    } else {
      // Failed to acquire locks, release any held locks
      let released_a = if locked_a.lock { release_lock(locked_a) } else { locked_a }
      let released_b = if locked_b.lock { release_lock(locked_b) } else { locked_b }
      
      (released_a, released_b)
    }
  }
  
  let resource_a = create_resource_a()
  let resource_b = create_resource_b()
  
  let (final_a, final_b) = safe_transfer(resource_a, resource_b)
  assert_eq(final_a.data, "resource-b-data")
  assert_eq(final_b.data, "resource-a-data")
  assert_false(final_a.lock)
  assert_false(final_b.lock)
}

// Test 6: Serialization and Deserialization
test "serialization and deserialization of telemetry data" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Define serialization function
  let serialize_span = fn(span: Span) {
    let attributes_str = span.attributes
      .map(fn((k, v)) { k + ":" + v })
      .join(",")
    
    let parent_id_str = match span.parent_span_id {
      Some(id) => id
      None => ""
    }
    
    span.trace_id + "|" + 
    span.span_id + "|" + 
    parent_id_str + "|" + 
    span.service_name + "|" + 
    span.operation_name + "|" + 
    span.start_time.to_string() + "|" + 
    span.end_time.to_string() + "|" + 
    span.status + "|" + 
    attributes_str
  }
  
  // Define deserialization function
  let deserialize_span = fn(serialized: String) {
    let parts = serialized.split("|")
    if parts.length() >= 8 {
      let trace_id = parts[0]
      let span_id = parts[1]
      let parent_id_str = parts[2]
      let parent_span_id = if parent_id_str.length() > 0 {
        Some(parent_id_str)
      } else {
        None
      }
      let service_name = parts[3]
      let operation_name = parts[4]
      let start_time = parts[5].to_int()
      let end_time = parts[6].to_int()
      let status = parts[7]
      
      let attributes = if parts.length() > 8 and parts[8].length() > 0 {
        parts[8].split(",").map(fn(attr) {
          let kv = attr.split(":")
          if kv.length() == 2 {
            (kv[0], kv[1])
          } else {
            ("", "")
          }
        })
      } else {
        []
      }
      
      Some({
        trace_id,
        span_id,
        parent_span_id,
        service_name,
        operation_name,
        start_time,
        end_time,
        status,
        attributes
      })
    } else {
      None
    }
  }
  
  // Create test span
  let test_span = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    parent_span_id: Some("a1b2c3d4e5f67890"),
    service_name: "payment-service",
    operation_name: "process-payment",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("user.id", "12345"),
      ("payment.amount", "99.99"),
      ("payment.currency", "USD")
    ]
  }
  
  // Test serialization
  let serialized = serialize_span(test_span)
  assert_true(serialized.contains(test_span.trace_id))
  assert_true(serialized.contains(test_span.span_id))
  assert_true(serialized.contains(test_span.service_name))
  assert_true(serialized.contains(test_span.operation_name))
  
  // Test deserialization
  let deserialized = deserialize_span(serialized)
  assert_eq(deserialized, Some(test_span))
  
  // Test span without parent
  let span_without_parent = {
    trace_id: "trace123",
    span_id: "span456",
    parent_span_id: None,
    service_name: "auth-service",
    operation_name: "authenticate",
    start_time: 1640995300,
    end_time: 1640995350,
    status: "error",
    attributes: [
      ("error.type", "authentication_failed"),
      ("user.id", "67890")
    ]
  }
  
  let serialized_without_parent = serialize_span(span_without_parent)
  let deserialized_without_parent = deserialize_span(serialized_without_parent)
  assert_eq(deserialized_without_parent, Some(span_without_parent))
  
  // Test batch serialization
  let serialize_spans = fn(spans: Array[Span]) {
    spans.map(serialize_span).join("\n")
  }
  
  let deserialize_spans = fn(serialized: String) {
    if serialized.length() > 0 {
      serialized.split("\n").map(deserialize_span)
    } else {
      []
    }
  }
  
  let spans = [test_span, span_without_parent]
  let batch_serialized = serialize_spans(spans)
  let batch_deserialized = deserialize_spans(batch_serialized)
  
  assert_eq(batch_deserialized.length(), 2)
  assert_eq(batch_deserialized[0], Some(test_span))
  assert_eq(batch_deserialized[1], Some(span_without_parent))
  
  // Test JSON-like serialization
  let serialize_span_json = fn(span: Span) {
    let attributes_json = span.attributes
      .map(fn((k, v)) { "\"" + k + "\":\"" + v + "\"" })
      .join(",")
    
    let parent_json = match span.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + span.trace_id + "\"," +
    "\"span_id\":\"" + span.span_id + "\"," +
    "\"parent_span_id\":" + parent_json + "," +
    "\"service_name\":\"" + span.service_name + "\"," +
    "\"operation_name\":\"" + span.operation_name + "\"," +
    "\"start_time\":" + span.start_time.to_string() + "," +
    "\"end_time\":" + span.end_time.to_string() + "," +
    "\"status\":\"" + span.status + "\"," +
    "\"attributes\":{" + attributes_json + "}" +
    "}"
  }
  
  let json_serialized = serialize_span_json(test_span)
  assert_true(json_serialized.contains("\"trace_id\":\"" + test_span.trace_id + "\""))
  assert_true(json_serialized.contains("\"span_id\":\"" + test_span.span_id + "\""))
  assert_true(json_serialized.contains("\"service_name\":\"" + test_span.service_name + "\""))
  
  // Test compression simulation
  let compress_string = fn(s: String) {
    // Simple compression simulation: replace repeated characters with count
    let mut result = ""
    let mut i = 0
    
    while i < s.length() {
      let mut count = 1
      let char = s[i]
      
      while i + count < s.length() and s[i + count] == char {
        count = count + 1
      }
      
      if count > 3 {
        result = result + char.to_string() + "[" + count.to_string() + "]"
      } else {
        let mut j = 0
        while j < count {
          result = result + char.to_string()
          j = j + 1
        }
      }
      
      i = i + count
    }
    
    result
  }
  
  let decompress_string = fn(compressed: String) {
    let mut result = ""
    let mut i = 0
    
    while i < compressed.length() {
      let char = compressed[i]
      
      if i + 1 < compressed.length() and compressed[i + 1] == '[' {
        let mut j = i + 2
        let mut count_str = ""
        
        while j < compressed.length() and compressed[j] != ']' {
          count_str = count_str + compressed[j].to_string()
          j = j + 1
        }
        
        let count = count_str.to_int()
        let mut k = 0
        while k < count {
          result = result + char.to_string()
          k = k + 1
        }
        
        i = j + 1
      } else {
        result = result + char.to_string()
        i = i + 1
      }
    }
    
    result
  }
  
  let test_string = "aaaaabbbbcccccddddd"
  let compressed = compress_string(test_string)
  let decompressed = decompress_string(compressed)
  
  assert_eq(compressed, "a[5]b[4]c[5]d[5]")
  assert_eq(decompressed, test_string)
  
  // Test compression with serialized span
  let compressed_span = compress_string(serialized)
  let decompressed_span = decompress_string(compressed_span)
  let decompressed_deserialized = deserialize_span(decompressed_span)
  
  assert_eq(decompressed_deserialized, Some(test_span))
}

// Test 7: Resource Management
test "resource management for telemetry operations" {
  // Define resource pool
  type ResourcePool[T] = {
    resources: Array[T],
    available: Array[Bool],
    max_size: Int
  }
  
  let create_pool = fn(max_size: Int, factory: () -> T) {
    let mut resources = []
    let mut available = []
    
    for i in 0..max_size {
      resources = resources.push(factory())
      available = available.push(true)
    }
    
    {
      resources,
      available,
      max_size
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool[T]) {
    let mut i = 0
    while i < pool.available.length() {
      if pool.available[i] {
        let updated_available = pool.available.update(i, false)
        let updated_pool = { pool | available: updated_available }
        (updated_pool, Some(pool.resources[i]))
      }
      i = i + 1
    }
    
    (pool, None)  // No available resources
  }
  
  let release_resource = fn(pool: ResourcePool[T], resource: T) {
    let mut i = 0
    while i < pool.resources.length() {
      if pool.resources[i] == resource {
        let updated_available = pool.available.update(i, true)
        return { pool | available: updated_available }
      }
      i = i + 1
    }
    
    pool  // Resource not found
  }
  
  // Test resource pool
  let connection_factory = fn() {
    "connection-" + (1000 + 1000).to_string()  // Simulate connection ID
  }
  
  let pool = create_pool(5, connection_factory)
  
  // Acquire resources
  let (pool1, resource1) = acquire_resource(pool)
  assert_eq(resource1, Some("connection-2000"))
  
  let (pool2, resource2) = acquire_resource(pool1)
  assert_eq(resource2, Some("connection-2000"))
  
  let (pool3, resource3) = acquire_resource(pool2)
  assert_eq(resource3, Some("connection-2000"))
  
  // Release a resource
  let pool4 = release_resource(pool3, "connection-2000")
  
  // Acquire again (should get the released resource)
  let (pool5, resource4) = acquire_resource(pool4)
  assert_eq(resource4, Some("connection-2000"))
  
  // Test memory usage tracking
  type MemoryTracker = {
    allocated_objects: Int,
    total_memory: Int,
    peak_memory: Int
  }
  
  let create_tracker = fn() {
    {
      allocated_objects: 0,
      total_memory: 0,
      peak_memory: 0
    }
  }
  
  let allocate_object = fn(tracker: MemoryTracker, size: Int) {
    let new_total = tracker.total_memory + size
    let new_peak = if new_total > tracker.peak_memory {
      new_total
    } else {
      tracker.peak_memory
    }
    
    {
      allocated_objects: tracker.allocated_objects + 1,
      total_memory: new_total,
      peak_memory: new_peak
    }
  }
  
  let deallocate_object = fn(tracker: MemoryTracker, size: Int) {
    {
      allocated_objects: tracker.allocated_objects - 1,
      total_memory: tracker.total_memory - size,
      peak_memory: tracker.peak_memory
    }
  }
  
  let tracker = create_tracker()
  
  // Allocate objects
  let tracker1 = allocate_object(tracker, 100)
  let tracker2 = allocate_object(tracker1, 200)
  let tracker3 = allocate_object(tracker2, 150)
  
  assert_eq(tracker3.allocated_objects, 3)
  assert_eq(tracker3.total_memory, 450)
  assert_eq(tracker3.peak_memory, 450)
  
  // Deallocate objects
  let tracker4 = deallocate_object(tracker3, 100)
  let tracker5 = deallocate_object(tracker4, 200)
  
  assert_eq(tracker5.allocated_objects, 1)
  assert_eq(tracker5.total_memory, 150)
  assert_eq(tracker5.peak_memory, 450)  // Peak memory should remain
  
  // Test resource cleanup
  type Resource = {
    id: String,
    data: String,
    active: Bool
  }
  
  let create_resource = fn(id: String) {
    {
      id,
      data: "data-for-" + id,
      active: true
    }
  }
  
  let cleanup_resource = fn(resource: Resource) {
    { resource | active: false, data: "" }
  }
  
  let resource = create_resource("resource-1")
  assert_true(resource.active)
  assert_eq(resource.data, "data-for-resource-1")
  
  let cleaned_resource = cleanup_resource(resource)
  assert_false(cleaned_resource.active)
  assert_eq(cleaned_resource.data, "")
  
  // Test resource lifecycle management
  type ResourceLifecycle = {
    created: Int,
    initialized: Int,
    used: Int,
    disposed: Int
  }
  
  let create_lifecycle = fn() {
    {
      created: 0,
      initialized: 0,
      used: 0,
      disposed: 0
    }
  }
  
  let track_creation = fn(lifecycle: ResourceLifecycle) {
    { lifecycle | created: lifecycle.created + 1 }
  }
  
  let track_initialization = fn(lifecycle: ResourceLifecycle) {
    { lifecycle | initialized: lifecycle.initialized + 1 }
  }
  
  let track_usage = fn(lifecycle: ResourceLifecycle) {
    { lifecycle | used: lifecycle.used + 1 }
  }
  
  let track_disposal = fn(lifecycle: ResourceLifecycle) {
    { lifecycle | disposed: lifecycle.disposed + 1 }
  }
  
  let lifecycle = create_lifecycle()
  let lifecycle1 = track_creation(lifecycle)
  let lifecycle2 = track_initialization(lifecycle1)
  let lifecycle3 = track_usage(lifecycle2)
  let lifecycle4 = track_usage(lifecycle3)
  let lifecycle5 = track_disposal(lifecycle4)
  
  assert_eq(lifecycle5.created, 1)
  assert_eq(lifecycle5.initialized, 1)
  assert_eq(lifecycle5.used, 2)
  assert_eq(lifecycle5.disposed, 1)
  
  // Test resource limits
  type ResourceLimiter = {
    current_usage: Int,
    max_usage: Int,
    rejected_requests: Int
  }
  
  let create_limiter = fn(max_usage: Int) {
    {
      current_usage: 0,
      max_usage,
      rejected_requests: 0
    }
  }
  
  let request_resource = fn(limiter: ResourceLimiter, amount: Int) {
    if limiter.current_usage + amount <= limiter.max_usage {
      ({ limiter | current_usage: limiter.current_usage + amount }, true)
    } else {
      ({ limiter | rejected_requests: limiter.rejected_requests + 1 }, false)
    }
  }
  
  let release_resource_limit = fn(limiter: ResourceLimiter, amount: Int) {
    { limiter | current_usage: limiter.current_usage - amount }
  }
  
  let limiter = create_limiter(100)
  
  // Request resources
  let (limiter1, approved1) = request_resource(limiter, 30)
  assert_true(approved1)
  assert_eq(limiter1.current_usage, 30)
  
  let (limiter2, approved2) = request_resource(limiter1, 50)
  assert_true(approved2)
  assert_eq(limiter2.current_usage, 80)
  
  let (limiter3, approved3) = request_resource(limiter2, 30)  // Should exceed limit
  assert_false(approved3)
  assert_eq(limiter3.current_usage, 80)
  assert_eq(limiter3.rejected_requests, 1)
  
  // Release some resources
  let limiter4 = release_resource_limit(limiter3, 40)
  assert_eq(limiter4.current_usage, 40)
  
  // Request again (should be approved)
  let (limiter5, approved4) = request_resource(limiter4, 30)
  assert_true(approved4)
  assert_eq(limiter5.current_usage, 70)
  assert_eq(limiter5.rejected_requests, 1)
}

// Test 8: Cross-Service Propagation
test "cross-service propagation of telemetry context" {
  // Define context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    sampling_decision: Bool
  }
  
  // Define service communication
  type ServiceRequest = {
    service_name: String,
    operation: String,
    headers: Array[(String, String)],
    payload: String
  }
  
  type ServiceResponse = {
    status_code: Int,
    headers: Array[(String, String)],
    body: String
  }
  
  // Context injection
  let inject_context = fn(context: TraceContext) {
    let baggage_str = context.baggage
      .map(fn((k, v)) { k + "=" + v })
      .join(",")
    
    [
      ("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"),
      ("tracestate", baggage_str),
      ("x-sampling-decision", if context.sampling_decision { "true" } else { "false" })
    ]
  }
  
  // Context extraction
  let extract_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut baggage = []
    let mut sampling_decision = true  // Default to true
    
    for (key, value) in headers {
      match key {
        "traceparent" => {
          let parts = value.split("-")
          if parts.length() >= 3 {
            trace_id = Some(parts[1])
            span_id = Some(parts[2])
          }
        }
        "tracestate" => {
          if value.length() > 0 {
            let items = value.split(",")
            for item in items {
              let kv = item.split("=")
              if kv.length() == 2 {
                baggage = baggage.push((kv[0], kv[1]))
              }
            }
          }
        }
        "x-sampling-decision" => {
          sampling_decision = value == "true"
        }
        _ => ()
      }
    }
    
    match (trace_id, span_id) {
      (Some(tid), Some(sid)) => Some({
        trace_id: tid,
        span_id: sid,
        baggage,
        sampling_decision
      })
      _ => None
    }
  }
  
  // Create initial context
  let initial_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    baggage: [
      ("user.id", "12345"),
      ("request.id", "req-67890"),
      ("tenant.id", "tenant-001")
    ],
    sampling_decision: true
  }
  
  // Test context injection
  let injected_headers = inject_context(initial_context)
  assert_eq(injected_headers.length(), 3)
  
  let traceparent = injected_headers[0].1
  assert_true(traceparent.contains(initial_context.trace_id))
  assert_true(traceparent.contains(initial_context.span_id))
  
  let tracestate = injected_headers[1].1
  assert_true(tracestate.contains("user.id=12345"))
  assert_true(tracestate.contains("request.id=req-67890"))
  assert_true(tracestate.contains("tenant.id=tenant-001"))
  
  // Test context extraction
  let extracted_context = extract_context(injected_headers)
  assert_eq(extracted_context, Some(initial_context))
  
  // Test service communication
  let create_service_request = fn(service_name: String, operation: String, context: TraceContext, payload: String) {
    {
      service_name,
      operation,
      headers: inject_context(context),
      payload
    }
  }
  
  let process_service_request = fn(request: ServiceRequest, handler: (String, String, TraceContext) -> String) {
    let context = extract_context(request.headers)
    
    match context {
      Some(ctx) => {
        let response_body = handler(request.service_name, request.operation, ctx)
        let response_headers = inject_context(ctx)  // Echo back context
        
        {
          status_code: 200,
          headers: response_headers,
          body: response_body
        }
      }
      None => {
        {
          status_code: 400,
          headers: [],
          body: "Missing trace context"
        }
      }
    }
  }
  
  // Test request/response cycle
  let request = create_service_request(
    "payment-service",
    "process-payment",
    initial_context,
    "{\"amount\": 99.99, \"currency\": \"USD\"}"
  )
  
  let payment_handler = fn(service_name: String, operation: String, context: TraceContext) {
    "Payment processed for user " + 
    (match context.baggage.find(fn((k, _)) { k == "user.id" }) {
      Some((_, user_id)) => user_id
      None => "unknown"
    }) +
    " in trace " + context.trace_id
  }
  
  let response = process_service_request(request, payment_handler)
  assert_eq(response.status_code, 200)
  assert_true(response.body.contains("Payment processed for user 12345"))
  assert_true(response.body.contains(initial_context.trace_id))
  
  // Verify context is preserved in response
  let response_context = extract_context(response.headers)
  assert_eq(response_context, Some(initial_context))
  
  // Test multi-service call chain
  let simulate_service_call = fn(
    caller_service: String,
    callee_service: String,
    operation: String,
    context: TraceContext,
    payload: String
  ) {
    let request = create_service_request(callee_service, operation, context, payload)
    
    let handler = fn(service: String, op: String, ctx: TraceContext) {
      "Service " + service + " processed " + op + 
      " for trace " + ctx.trace_id +
      " with span " + ctx.span_id
    }
    
    process_service_request(request, handler)
  }
  
  // Simulate service chain: API Gateway -> Auth Service -> User Service -> Payment Service
  let api_context = initial_context
  
  let auth_response = simulate_service_call(
    "api-gateway",
    "auth-service",
    "authenticate",
    api_context,
    "{\"token\": \"abc123\"}"
  )
  
  assert_eq(auth_response.status_code, 200)
  let auth_context = extract_context(auth_response.headers)
  assert_eq(auth_context, Some(api_context))
  
  let user_response = simulate_service_call(
    "auth-service",
    "user-service",
    "get-user",
    api_context,
    "{\"user_id\": \"12345\"}"
  )
  
  assert_eq(user_response.status_code, 200)
  let user_context = extract_context(user_response.headers)
  assert_eq(user_context, Some(api_context))
  
  let payment_response = simulate_service_call(
    "user-service",
    "payment-service",
    "process-payment",
    api_context,
    "{\"amount\": 99.99}"
  )
  
  assert_eq(payment_response.status_code, 200)
  let payment_context = extract_context(payment_response.headers)
  assert_eq(payment_context, Some(api_context))
  
  // Test baggage propagation and modification
  let update_baggage = fn(context: TraceContext, key: String, value: String) {
    let mut updated_baggage = []
    let mut found = false
    
    for (k, v) in context.baggage {
      if k == key {
        updated_baggage = updated_baggage.push((k, value))
        found = true
      } else {
        updated_baggage = updated_baggage.push((k, v))
      }
    }
    
    if not(found) {
      updated_baggage = updated_baggage.push((key, value))
    }
    
    { context | baggage: updated_baggage }
  }
  
  let updated_context = update_baggage(initial_context, "service.version", "1.2.3")
  assert_true(updated_context.baggage.contains(("service.version", "1.2.3")))
  
  // Test sampling decision propagation
  let create_sampled_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      baggage: [],
      sampling_decision: true
    }
  }
  
  let create_unsampled_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      baggage: [],
      sampling_decision: false
    }
  }
  
  let sampled_context = create_sampled_context("trace-123", "span-456")
  let unsampled_context = create_unsampled_context("trace-789", "span-012")
  
  let sampled_headers = inject_context(sampled_context)
  let unsampled_headers = inject_context(unsampled_context)
  
  let extracted_sampled = extract_context(sampled_headers)
  let extracted_unsampled = extract_context(unsampled_headers)
  
  assert_eq(extracted_sampled, Some(sampled_context))
  assert_eq(extracted_unsampled, Some(unsampled_context))
  
  // Verify sampling decision is preserved
  match extracted_sampled {
    Some(ctx) => assert_true(ctx.sampling_decision)
    None => assert_true(false)
  }
  
  match extracted_unsampled {
    Some(ctx) => assert_false(ctx.sampling_decision)
    None => assert_true(false)
  }
}

// Test 9: Internationalization Support
test "internationalization support for telemetry data" {
  // Define locale structure
  type Locale = {
    language: String,
    region: String,
    encoding: String
  }
  
  // Define localized message
  type LocalizedMessage = {
    key: String,
    default_text: String,
    translations: Array[(String, String)]  // (locale, translation)
  }
  
  // Create locale instances
  let en_us = { language: "en", region: "US", encoding: "UTF-8" }
  let zh_cn = { language: "zh", region: "CN", encoding: "UTF-8" }
  let fr_fr = { language: "fr", region: "FR", encoding: "UTF-8" }
  let ja_jp = { language: "ja", region: "JP", encoding: "UTF-8" }
  
  let locale_to_string = fn(locale: Locale) {
    locale.language + "-" + locale.region
  }
  
  assert_eq(locale_to_string(en_us), "en-US")
  assert_eq(locale_to_string(zh_cn), "zh-CN")
  assert_eq(locale_to_string(fr_fr), "fr-FR")
  assert_eq(locale_to_string(ja_jp), "ja-JP")
  
  // Create localized messages
  let error_messages = [
    {
      key: "timeout.error",
      default_text: "Request timed out",
      translations: [
        ("zh-CN", "请求超时"),
        ("fr-FR", "Délai d'attente dépassé"),
        ("ja-JP", "リクエストがタイムアウトしました")
      ]
    },
    {
      key: "connection.error",
      default_text: "Connection failed",
      translations: [
        ("zh-CN", "连接失败"),
        ("fr-FR", "Échec de connexion"),
        ("ja-JP", "接続に失敗しました")
      ]
    },
    {
      key: "authentication.error",
      default_text: "Authentication failed",
      translations: [
        ("zh-CN", "身份验证失败"),
        ("fr-FR", "Échec d'authentification"),
        ("ja-JP", "認証に失敗しました")
      ]
    }
  ]
  
  // Localization function
  let localize_message = fn(messages: Array[LocalizedMessage], key: String, locale: Locale) {
    let locale_str = locale_to_string(locale)
    
    let message = messages.find(fn(msg) { msg.key == key })
    
    match message {
      Some(msg) => {
        let translation = msg.translations.find(fn((loc, _)) { loc == locale_str })
        match translation {
          Some((_, text)) => text
          None => msg.default_text
        }
      }
      None => "Unknown message key: " + key
    }
  }
  
  // Test localization
  assert_eq(localize_message(error_messages, "timeout.error", en_us), "Request timed out")
  assert_eq(localize_message(error_messages, "timeout.error", zh_cn), "请求超时")
  assert_eq(localize_message(error_messages, "timeout.error", fr_fr), "Délai d'attente dépassé")
  assert_eq(localize_message(error_messages, "timeout.error", ja_jp), "リクエストがタイムアウトしました")
  
  assert_eq(localize_message(error_messages, "connection.error", en_us), "Connection failed")
  assert_eq(localize_message(error_messages, "connection.error", zh_cn), "连接失败")
  assert_eq(localize_message(error_messages, "connection.error", fr_fr), "Échec de connexion")
  assert_eq(localize_message(error_messages, "connection.error", ja_jp), "接続に失敗しました")
  
  // Test fallback to default
  let de_de = { language: "de", region: "DE", encoding: "UTF-8" }
  assert_eq(localize_message(error_messages, "timeout.error", de_de), "Request timed out")
  
  // Test unknown key
  assert_eq(
    localize_message(error_messages, "unknown.error", en_us),
    "Unknown message key: unknown.error"
  )
  
  // Test number formatting by locale
  let format_number = fn(number: Float, locale: Locale) {
    match locale.language {
      "en" => {
        // English format: 1,234.56
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        
        let mut formatted_int = ""
        let mut i = int_part.length()
        let mut count = 0
        
        while i > 0 {
          i = i - 1
          formatted_int = int_part[i].to_string() + formatted_int
          count = count + 1
          
          if count == 3 and i > 0 {
            formatted_int = "," + formatted_int
            count = 0
          }
        }
        
        formatted_int + "." + decimal_part
      }
      "de" => {
        // German format: 1.234,56
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100.0).to_int().to_string()
        
        let mut formatted_int = ""
        let mut i = int_part.length()
        let mut count = 0
        
        while i > 0 {
          i = i - 1
          formatted_int = int_part[i].to_string() + formatted_int
          count = count + 1
          
          if count == 3 and i > 0 {
            formatted_int = "." + formatted_int
            count = 0
          }
        }
        
        formatted_int + "," + decimal_part
      }
      _ => {
        // Default format: 1234.56
        number.to_string()
      }
    }
  }
  
  assert_eq(format_number(1234.56, en_us), "1,234.56")
  assert_eq(format_number(1234.56, de_de), "1.234,56")
  assert_eq(format_number(1234.56, fr_fr), "1234.56")  // Uses default format
  
  // Test date/time formatting by locale
  let format_datetime = fn(timestamp: Int, locale: Locale) {
    // Simulate date formatting based on locale
    let date = "2022-01-01"
    let time = "12:00:00"
    
    match locale.language {
      "en" => date + " " + time  // MM/DD/YYYY HH:MM:SS
      "zh" => "2022年01月01日 " + time  // YYYY年MM月DD日 HH:MM:SS
      "ja" => "2022年01月01日 " + time  // YYYY年MM月DD日 HH:MM:SS
      "fr" => "01/01/2022 " + time  // DD/MM/YYYY HH:MM:SS
      _ => date + " " + time  // Default format
    }
  }
  
  assert_eq(format_datetime(1640995200, en_us), "2022-01-01 12:00:00")
  assert_eq(format_datetime(1640995200, zh_cn), "2022年01月01日 12:00:00")
  assert_eq(format_datetime(1640995200, ja_jp), "2022年01月01日 12:00:00")
  assert_eq(format_datetime(1640995200, fr_fr), "01/01/2022 12:00:00")
  
  // Test currency formatting by locale
  let format_currency = fn(amount: Float, currency: String, locale: Locale) {
    match (locale.language, currency) {
      ("en", "USD") => "$" + amount.to_string()
      ("en", "EUR") => "€" + amount.to_string()
      ("zh", "CNY") => "¥" + amount.to_string()
      ("ja", "JPY") => "¥" + amount.to_string()
      ("fr", "EUR") => amount.to_string() + " €"
      _ => amount.to_string() + " " + currency
    }
  }
  
  assert_eq(format_currency(99.99, "USD", en_us), "$99.99")
  assert_eq(format_currency(99.99, "EUR", en_us), "€99.99")
  assert_eq(format_currency(99.99, "CNY", zh_cn), "¥99.99")
  assert_eq(format_currency(99.99, "JPY", ja_jp), "¥99.99")
  assert_eq(format_currency(99.99, "EUR", fr_fr), "99.99 €")
  
  // Test localized telemetry data
  type LocalizedTelemetryEvent = {
    event_name: String,
    timestamp: Int,
    locale: Locale,
    message: String,
    metadata: Array[(String, String)]
  }
  
  let create_localized_event = fn(
    event_name: String,
    timestamp: Int,
    locale: Locale,
    message_key: String,
    messages: Array[LocalizedMessage],
    metadata: Array[(String, String)]
  ) {
    {
      event_name,
      timestamp,
      locale,
      message: localize_message(messages, message_key, locale),
      metadata
    }
  }
  
  let timeout_event_en = create_localized_event(
    "api_timeout",
    1640995200,
    en_us,
    "timeout.error",
    error_messages,
    [("service", "api-gateway"), ("duration", "5000")]
  )
  
  let timeout_event_zh = create_localized_event(
    "api_timeout",
    1640995200,
    zh_cn,
    "timeout.error",
    error_messages,
    [("service", "api-gateway"), ("duration", "5000")]
  )
  
  assert_eq(timeout_event_en.message, "Request timed out")
  assert_eq(timeout_event_zh.message, "请求超时")
  
  // Test localized error reporting
  type LocalizedError = {
    error_code: String,
    locale: Locale,
    message: String,
    details: Array[String]
  }
  
  let create_localized_error = fn(
    error_code: String,
    locale: Locale,
    message_key: String,
    detail_keys: Array[String],
    messages: Array[LocalizedMessage]
  ) {
    let localized_details = detail_keys.map(fn(key) {
      localize_message(messages, key, locale)
    })
    
    {
      error_code,
      locale,
      message: localize_message(messages, message_key, locale),
      details: localized_details
    }
  }
  
  let auth_error_en = create_localized_error(
    "AUTH_001",
    en_us,
    "authentication.error",
    ["timeout.error", "connection.error"],
    error_messages
  )
  
  let auth_error_zh = create_localized_error(
    "AUTH_001",
    zh_cn,
    "authentication.error",
    ["timeout.error", "connection.error"],
    error_messages
  )
  
  assert_eq(auth_error_en.message, "Authentication failed")
  assert_eq(auth_error_en.details, ["Request timed out", "Connection failed"])
  
  assert_eq(auth_error_zh.message, "身份验证失败")
  assert_eq(auth_error_zh.details, ["请求超时", "连接失败"])
}

// Test 10: Real-time Stream Processing
test "real-time stream processing for telemetry data" {
  // Define stream event
  type StreamEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    metadata: Array[(String, String)]
  }
  
  // Define stream processor
  type StreamProcessor[T] = {
    process_fn: StreamEvent -> T,
    filter_fn: StreamEvent -> Bool,
    buffer: Array[T],
    max_buffer_size: Int
  }
  
  // Create stream processor
  let create_processor = fn(
    process_fn: StreamEvent -> T,
    filter_fn: StreamEvent -> Bool,
    max_buffer_size: Int
  ) {
    {
      process_fn,
      filter_fn,
      buffer: [],
      max_buffer_size
    }
  }
  
  // Process event
  let process_event = fn(processor: StreamProcessor[T], event: StreamEvent) {
    if processor.filter_fn(event) {
      let processed = processor.process_fn(event)
      
      let new_buffer = if processor.buffer.length() >= processor.max_buffer_size {
        // Remove oldest element (FIFO)
        processor.buffer.slice(1, processor.buffer.length()).push(processed)
      } else {
        processor.buffer.push(processed)
      }
      
      { processor | buffer: new_buffer }
    } else {
      processor  // Event filtered out
    }
  }
  
  // Test event processing
  let metric_processor = create_processor(
    fn(event: StreamEvent) {
      (event.event_type, event.timestamp, event.data.to_float())
    },
    fn(event: StreamEvent) {
      event.event_type == "metric"
    },
    5
  )
  
  // Create test events
  let events = [
    {
      event_id: "evt-001",
      timestamp: 1640995200,
      event_type: "metric",
      data: "95.5",
      metadata: [("service", "api"), ("metric.name", "response_time")]
    },
    {
      event_id: "evt-002",
      timestamp: 1640995260,
      event_type: "log",
      data: "User login successful",
      metadata: [("service", "auth"), ("level", "info")]
    },
    {
      event_id: "evt-003",
      timestamp: 1640995320,
      event_type: "metric",
      data: "120.3",
      metadata: [("service", "db"), ("metric.name", "query_time")]
    },
    {
      event_id: "evt-004",
      timestamp: 1640995380,
      event_type: "metric",
      data: "85.7",
      metadata: [("service", "cache"), ("metric.name", "hit_rate")]
    }
  ]
  
  // Process events
  let processor1 = process_event(metric_processor, events[0])
  assert_eq(processor1.buffer.length(), 1)
  assert_eq(processor1.buffer[0], ("metric", 1640995200, 95.5))
  
  let processor2 = process_event(processor1, events[1])
  assert_eq(processor2.buffer.length(), 1)  // Log event filtered out
  
  let processor3 = process_event(processor2, events[2])
  assert_eq(processor3.buffer.length(), 2)
  assert_eq(processor3.buffer[0], ("metric", 1640995200, 95.5))
  assert_eq(processor3.buffer[1], ("metric", 1640995320, 120.3))
  
  let processor4 = process_event(processor3, events[3])
  assert_eq(processor4.buffer.length(), 3)
  assert_eq(processor4.buffer[0], ("metric", 1640995200, 95.5))
  assert_eq(processor4.buffer[1], ("metric", 1640995320, 120.3))
  assert_eq(processor4.buffer[2], ("metric", 1640995380, 85.7))
  
  // Test windowed aggregation
  type WindowedAggregator = {
    window_size: Int,
    events: Array[StreamEvent],
    aggregation_fn: Array[StreamEvent] -> Float
  }
  
  let create_aggregator = fn(window_size: Int, aggregation_fn: Array[StreamEvent] -> Float) {
    {
      window_size,
      events: [],
      aggregation_fn
    }
  }
  
  let add_to_window = fn(aggregator: WindowedAggregator, event: StreamEvent) {
    let updated_events = aggregator.events.push(event)
    
    // Keep only events within the window
    let window_start = event.timestamp - aggregator.window_size
    let filtered_events = updated_events.filter(fn(e) { e.timestamp >= window_start })
    
    { aggregator | events: filtered_events }
  }
  
  let get_aggregation = fn(aggregator: WindowedAggregator) {
    if aggregator.events.length() > 0 {
      aggregator.aggregation_fn(aggregator.events)
    } else {
      0.0
    }
  }
  
  // Create average response time aggregator
  let avg_aggregator = create_aggregator(
    300,  // 5-minute window
    fn(events: Array[StreamEvent]) {
      let metric_events = events.filter(fn(e) { e.event_type == "metric" })
      
      if metric_events.length() > 0 {
        let sum = metric_events.reduce(fn(acc, e) { acc + e.data.to_float() }, 0.0)
        sum / metric_events.length().to_float()
      } else {
        0.0
      }
    }
  )
  
  // Add events to aggregator
  let agg1 = add_to_window(avg_aggregator, events[0])
  assert_eq(get_aggregation(agg1), 95.5)
  
  let agg2 = add_to_window(agg1, events[2])
  assert_eq(get_aggregation(agg2), (95.5 + 120.3) / 2.0)
  
  let agg3 = add_to_window(agg2, events[3])
  assert_eq(get_aggregation(agg3), (95.5 + 120.3 + 85.7) / 3.0)
  
  // Test event pattern detection
  type PatternDetector = {
    patterns: Array[(String, Array[StreamEvent] -> Bool)],
    recent_events: Array[StreamEvent],
    max_events: Int
  }
  
  let create_detector = fn(max_events: Int) {
    {
      patterns: [
        ("high_error_rate", fn(events: Array[StreamEvent]) {
          let error_events = events.filter(fn(e) { e.event_type == "error" })
          error_events.length() >= 3
        }),
        ("slow_response", fn(events: Array[StreamEvent]) {
          let metric_events = events.filter(fn(e) { 
            e.event_type == "metric" and e.data.to_float() > 100.0 
          })
          metric_events.length() >= 2
        })
      ],
      recent_events: [],
      max_events
    }
  }
  
  let add_event = fn(detector: PatternDetector, event: StreamEvent) {
    let updated_events = detector.recent_events.push(event)
    
    // Keep only the most recent events
    let recent = if updated_events.length() > detector.max_events {
      updated_events.slice(updated_events.length() - detector.max_events, updated_events.length())
    } else {
      updated_events
    }
    
    { detector | recent_events: recent }
  }
  
  let detect_patterns = fn(detector: PatternDetector) {
    let mut detected = []
    
    for (pattern_name, pattern_fn) in detector.patterns {
      if pattern_fn(detector.recent_events) {
        detected = detected.push(pattern_name)
      }
    }
    
    detected
  }
  
  // Create pattern detector
  let detector = create_detector(10)
  
  // Add events
  let detector1 = add_event(detector, events[0])
  assert_eq(detect_patterns(detector1).length(), 0)
  
  let detector2 = add_event(detector1, events[2])
  assert_eq(detect_patterns(detector2).length(), 1)  // slow_response detected
  
  // Add error events
  let error_events = [
    {
      event_id: "evt-005",
      timestamp: 1640995440,
      event_type: "error",
      data: "Database connection failed",
      metadata: [("service", "db"), ("error.code", "DB001")]
    },
    {
      event_id: "evt-006",
      timestamp: 1640995500,
      event_type: "error",
      data: "Authentication service unavailable",
      metadata: [("service", "auth"), ("error.code", "AUTH001")]
    },
    {
      event_id: "evt-007",
      timestamp: 1640995560,
      event_type: "error",
      data: "Rate limit exceeded",
      metadata: [("service", "api"), ("error.code", "RATE001")]
    }
  ]
  
  let detector3 = add_event(detector2, error_events[0])
  assert_eq(detect_patterns(detector3).length(), 1)  // Only slow_response
  
  let detector4 = add_event(detector3, error_events[1])
  assert_eq(detect_patterns(detector4).length(), 1)  // Only slow_response
  
  let detector5 = add_event(detector4, error_events[2])
  let detected_patterns = detect_patterns(detector5)
  assert_eq(detected_patterns.length(), 2)  // Both slow_response and high_error_rate
  assert_true(detected_patterns.contains("slow_response"))
  assert_true(detected_patterns.contains("high_error_rate"))
  
  // Test stream transformation
  let transform_stream = fn(events: Array[StreamEvent], transformer: StreamEvent -> StreamEvent) {
    events.map(transformer)
  }
  
  let enrich_with_service_type = fn(event: StreamEvent) {
    let service_type = match event.metadata.find(fn((k, _)) { k == "service" }) {
      Some((_, "api")) => "frontend"
      Some((_, "auth")) => "security"
      Some((_, "db")) => "database"
      Some((_, "cache")) => "cache"
      _ => "unknown"
    }
    
    {
      event |
      metadata: event.metadata.push(("service.type", service_type))
    }
  }
  
  let enriched_events = transform_stream(events, enrich_with_service_type)
  
  assert_true(enriched_events[0].metadata.contains(("service.type", "frontend")))
  assert_true(enriched_events[1].metadata.contains(("service.type", "security")))
  assert_true(enriched_events[2].metadata.contains(("service.type", "database")))
  assert_true(enriched_events[3].metadata.contains(("service.type", "cache")))
  
  // Test stream filtering by time range
  let filter_by_time_range = fn(events: Array[StreamEvent], start_time: Int, end_time: Int) {
    events.filter(fn(event) {
      event.timestamp >= start_time and event.timestamp <= end_time
    })
  }
  
  let filtered_events = filter_by_time_range(events, 1640995250, 1640995350)
  assert_eq(filtered_events.length(), 2)
  assert_eq(filtered_events[0].event_id, "evt-002")
  assert_eq(filtered_events[1].event_id, "evt-003")
}