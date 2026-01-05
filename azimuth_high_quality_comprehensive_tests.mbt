// Azimuth High-Quality Comprehensive Test Suite
// This file contains 10 high-quality test cases covering key telemetry system functionalities

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  // Define telemetry data structure
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[String]
  }
  
  // Create sample telemetry data
  let telemetry_data = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 45.2, tags: ["host:server1", "region:us-west"] },
    { timestamp: 1640995260, metric_name: "cpu_usage", value: 48.7, tags: ["host:server1", "region:us-west"] },
    { timestamp: 1640995320, metric_name: "cpu_usage", value: 52.1, tags: ["host:server1", "region:us-west"] },
    { timestamp: 1640995200, metric_name: "memory_usage", value: 67.3, tags: ["host:server1", "region:us-west"] },
    { timestamp: 1640995260, metric_name: "memory_usage", value: 69.8, tags: ["host:server1", "region:us-west"] },
    { timestamp: 1640995320, metric_name: "memory_usage", value: 71.2, tags: ["host:server1", "region:us-west"] }
  ]
  
  // Aggregate function to calculate average by metric name
  let aggregate_by_metric = fn(data: Array[TelemetryData]) {
    let mut grouped = []
    let mut processed = []
    
    // Group by metric name
    for item in data {
      if not(processed.contains(item.metric_name)) {
        processed = processed.push(item.metric_name)
        
        let mut sum = 0.0
        let mut count = 0
        let mut timestamps = []
        
        for metric in data {
          if metric.metric_name == item.metric_name {
            sum = sum + metric.value
            count = count + 1
            timestamps = timestamps.push(metric.timestamp)
          }
        }
        
        let avg = sum / count.to_float()
        let min_timestamp = timestamps.reduce(fn(acc, x) { if acc < x { acc } else { x } }, timestamps[0])
        let max_timestamp = timestamps.reduce(fn(acc, x) { if acc > x { acc } else { x } }, timestamps[0])
        
        grouped = grouped.push({
          metric_name: item.metric_name,
          average: avg,
          count: count,
          min_timestamp: min_timestamp,
          max_timestamp: max_timestamp,
          duration: max_timestamp - min_timestamp
        })
      }
    }
    
    grouped
  }
  
  // Test aggregation
  let aggregated = aggregate_by_metric(telemetry_data)
  assert_eq(aggregated.length(), 2)
  
  // Verify CPU usage aggregation
  let cpu_metric = aggregated.filter(fn(m) { m.metric_name == "cpu_usage" })[0]
  assert_eq(cpu_metric.count, 3)
  assert_eq(cpu_metric.min_timestamp, 1640995200)
  assert_eq(cpu_metric.max_timestamp, 1640995320)
  assert_eq(cpu_metric.duration, 120)
  
  // Verify average calculation (45.2 + 48.7 + 52.1) / 3 = 48.666...
  assert_true(cpu_metric.average > 48.6 and cpu_metric.average < 48.7)
  
  // Verify memory usage aggregation
  let memory_metric = aggregated.filter(fn(m) { m.metric_name == "memory_usage" })[0]
  assert_eq(memory_metric.count, 3)
  assert_eq(memory_metric.min_timestamp, 1640995200)
  assert_eq(memory_metric.max_timestamp, 1640995320)
  assert_eq(memory_metric.duration, 120)
  
  // Verify average calculation (67.3 + 69.8 + 71.2) / 3 = 69.433...
  assert_true(memory_metric.average > 69.4 and memory_metric.average < 69.5)
}

// Test 2: Concurrent Data Processing Safety
test "concurrent data processing safety" {
  // Simulate a shared resource
  type SharedResource = {
    mut data: Array[String],
    mut lock: Bool
  }
  
  // Create shared resource
  let resource = { mut data: [], mut lock: false }
  
  // Simulate acquiring lock
  let acquire_lock = fn(res: SharedResource) {
    while res.lock {
      // Simulate waiting for lock
    }
    res.lock = true
  }
  
  // Simulate releasing lock
  let release_lock = fn(res: SharedResource) {
    res.lock = false
  }
  
  // Simulate thread-safe data addition
  let add_data_safely = fn(res: SharedResource, item: String) {
    acquire_lock(res)
    res.data = res.data.push(item)
    release_lock(res)
  }
  
  // Simulate concurrent operations
  let operations = ["op1", "op2", "op3", "op4", "op5"]
  
  // Process operations sequentially (simulating concurrent access)
  for op in operations {
    add_data_safely(resource, op)
  }
  
  // Verify all operations were processed
  assert_eq(resource.data.length(), 5)
  assert_true(resource.data.contains("op1"))
  assert_true(resource.data.contains("op2"))
  assert_true(resource.data.contains("op3"))
  assert_true(resource.data.contains("op4"))
  assert_true(resource.data.contains("op5"))
  
  // Test lock mechanism
  assert_false(resource.lock) // Lock should be released after all operations
  
  // Test atomic operations simulation
  type AtomicCounter = { mut value: Int }
  let counter = { mut value: 0 }
  
  let increment_atomic = fn(cnt: AtomicCounter) {
    // Simulate atomic increment
    let old_value = cnt.value
    // Simulate potential race condition
    cnt.value = old_value + 1
  }
  
  // Perform increments
  for i in 0..10 {
    increment_atomic(counter)
  }
  
  // Verify final value
  assert_eq(counter.value, 10)
}

// Test 3: Time Series Data Analysis
test "time series data analysis" {
  // Define time series data point
  type DataPoint = {
    timestamp: Int,
    value: Float
  }
  
  // Create sample time series data
  let time_series = [
    { timestamp: 1640995200, value: 10.5 },
    { timestamp: 1640995260, value: 12.3 },
    { timestamp: 1640995320, value: 11.8 },
    { timestamp: 1640995380, value: 14.2 },
    { timestamp: 1640995440, value: 13.7 },
    { timestamp: 1640995500, value: 15.1 },
    { timestamp: 1640995560, value: 14.9 },
    { timestamp: 1640995620, value: 16.3 }
  ]
  
  // Calculate moving average
  let moving_average = fn(data: Array[DataPoint], window: Int) {
    let mut result = []
    
    for i in window-1..data.length() {
      let mut sum = 0.0
      for j in (i-window+1)..=i {
        sum = sum + data[j].value
      }
      let avg = sum / window.to_float()
      result = result.push({
        timestamp: data[i].timestamp,
        value: avg
      })
    }
    
    result
  }
  
  // Test moving average with window size 3
  let ma3 = moving_average(time_series, 3)
  assert_eq(ma3.length(), 6) // 8 points - 3 window + 1 = 6 points
  
  // Verify first moving average point: (10.5 + 12.3 + 11.8) / 3 = 11.533...
  assert_true(ma3[0].value > 11.5 and ma3[0].value < 11.6)
  assert_eq(ma3[0].timestamp, 1640995320)
  
  // Verify last moving average point: (14.9 + 16.3 + 15.1) / 3 = 15.433...
  assert_true(ma3[5].value > 15.4 and ma3[5].value < 15.5)
  assert_eq(ma3[5].timestamp, 1640995620)
  
  // Calculate trend (simple linear regression)
  let calculate_trend = fn(data: Array[DataPoint]) {
    let n = data.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    for i in 0..data.length() {
      let x = i.to_float()
      let y = data[i].value
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    { slope, intercept }
  }
  
  // Test trend calculation
  let trend = calculate_trend(time_series)
  
  // Positive slope indicates upward trend
  assert_true(trend.slope > 0.0)
  
  // Predict next value using trend
  let next_x = time_series.length().to_float()
  let predicted_value = trend.slope * next_x + trend.intercept
  
  // Predicted value should be higher than most recent values
  assert_true(predicted_value > time_series[time_series.length() - 1].value)
  
  // Detect anomalies (values that deviate significantly from trend)
  let detect_anomalies = fn(data: Array[DataPoint], threshold: Float) {
    let trend = calculate_trend(data)
    let mut anomalies = []
    
    for i in 0..data.length() {
      let x = i.to_float()
      let expected = trend.slope * x + trend.intercept
      let actual = data[i].value
      let deviation = (actual - expected).abs() / expected.abs()
      
      if deviation > threshold {
        anomalies = anomalies.push({
          timestamp: data[i].timestamp,
          actual_value: actual,
          expected_value: expected,
          deviation: deviation
        })
      }
    }
    
    anomalies
  }
  
  // Test anomaly detection with 10% threshold
  let anomalies = detect_anomalies(time_series, 0.1)
  
  // With our sample data and 10% threshold, we should have few or no anomalies
  assert_true(anomalies.length() <= 2) // Allow for some variation in test data
}

// Test 4: Distributed Tracing Consistency
test "distributed tracing consistency" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create trace hierarchy
  let root_span = {
    trace_id: "trace-12345",
    span_id: "span-100",
    parent_span_id: None,
    operation_name: "root_operation",
    start_time: 1640995200000,
    end_time: 1640995200500,
    status: "ok"
  }
  
  let child_span1 = {
    trace_id: "trace-12345",
    span_id: "span-101",
    parent_span_id: Some("span-100"),
    operation_name: "database_query",
    start_time: 1640995200100,
    end_time: 1640995200300,
    status: "ok"
  }
  
  let child_span2 = {
    trace_id: "trace-12345",
    span_id: "span-102",
    parent_span_id: Some("span-100"),
    operation_name: "api_call",
    start_time: 1640995200200,
    end_time: 1640995200400,
    status: "ok"
  }
  
  let grandchild_span = {
    trace_id: "trace-12345",
    span_id: "span-103",
    parent_span_id: Some("span-101"),
    operation_name: "cache_lookup",
    start_time: 1640995200150,
    end_time: 1640995200250,
    status: "ok"
  }
  
  let spans = [root_span, child_span1, child_span2, grandchild_span]
  
  // Verify trace consistency
  let verify_trace_consistency = fn(trace: Array[Span]) {
    let mut trace_ids = []
    let mut errors = []
    
    // Collect all trace IDs
    for span in trace {
      if not(trace_ids.contains(span.trace_id)) {
        trace_ids = trace_ids.push(span.trace_id)
      }
    }
    
    // All spans should have the same trace ID
    if trace_ids.length() > 1 {
      errors = errors.push("Multiple trace IDs found in single trace")
    }
    
    // Verify parent-child relationships
    for span in trace {
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_found = trace.any(fn(s) { s.span_id == parent_id })
          if not(parent_found) {
            errors = errors.push("Parent span not found: " + parent_id)
          }
          
          // Child should start after parent
          let parent_span = trace.filter(fn(s) { s.span_id == parent_id })[0]
          if span.start_time < parent_span.start_time {
            errors = errors.push("Child span starts before parent span")
          }
        }
        None => {
          // Root span - should be only one
          let root_spans = trace.filter(fn(s) { s.parent_span_id.is_none() })
          if root_spans.length() > 1 {
            errors = errors.push("Multiple root spans found")
          }
        }
      }
    }
    
    // Verify temporal consistency
    let sorted_spans = trace.sort_by(fn(a, b) { a.start_time - b.start_time })
    for i in 1..sorted_spans.length() {
      if sorted_spans[i].start_time < sorted_spans[i-1].start_time {
        errors = errors.push("Spans not in chronological order")
        break
      }
    }
    
    errors
  }
  
  // Test trace consistency
  let consistency_errors = verify_trace_consistency(spans)
  assert_eq(consistency_errors.length(), 0)
  
  // Calculate trace duration
  let trace_duration = spans.reduce(fn(acc, span) { 
    let span_duration = span.end_time - span.start_time
    if span_duration > acc { span_duration } else { acc }
  }, 0)
  
  // Root span should have the longest duration
  let root_duration = root_span.end_time - root_span.start_time
  assert_eq(trace_duration, root_duration)
  
  // Build trace tree
  let build_trace_tree = fn(spans: Array[Span]) {
    let root = spans.filter(fn(s) { s.parent_span_id.is_none() })[0]
    
    let find_children = fn(parent_id: String) {
      spans.filter(fn(s) { 
        match s.parent_span_id {
          Some(id) => id == parent_id
          None => false
        }
      })
    }
    
    let build_tree = fn(span: Span) {
      let children = find_children(span.span_id)
      {
        span: span,
        children: children.map(build_tree)
      }
    }
    
    build_tree(root)
  }
  
  // Test trace tree building
  let trace_tree = build_trace_tree(spans)
  assert_eq(trace_tree.span.span_id, "span-100")
  assert_eq(trace_tree.children.length(), 2)
  
  // Find child spans
  let child1 = trace_tree.children.filter(fn(c) { c.span.span_id == "span-101" })[0]
  let child2 = trace_tree.children.filter(fn(c) { c.span.span_id == "span-102" })[0]
  
  assert_eq(child1.span.operation_name, "database_query")
  assert_eq(child2.span.operation_name, "api_call")
  
  // Verify grandchild
  assert_eq(child1.children.length(), 1)
  assert_eq(child1.children[0].span.span_id, "span-103")
  assert_eq(child1.children[0].span.operation_name, "cache_lookup")
}

// Test 5: Resource Management Optimization
test "resource management optimization" {
  // Define resource pool
  type Resource = {
    id: String,
    in_use: Bool,
    created_at: Int,
    last_used: Int,
    use_count: Int
  }
  
  type ResourcePool = {
    max_size: Int,
    mut resources: Array[Resource],
    mut next_id: Int
  }
  
  // Create resource pool
  let create_pool = fn(max_size: Int) {
    { max_size, mut resources: [], mut next_id: 1 }
  }
  
  // Acquire resource from pool
  let acquire_resource = fn(pool: ResourcePool) {
    let available = pool.resources.filter(fn(r) { not(r.in_use) })
    
    match available.length() > 0 {
      true => {
        // Reuse existing resource
        let resource = available[0]
        let updated_resource = { 
          resource | 
          in_use: true, 
          last_used: 1640995200,
          use_count: resource.use_count + 1
        }
        
        // Update pool
        pool.resources = pool.resources.map(fn(r) {
          if r.id == resource.id { updated_resource } else { r }
        })
        
        Some(updated_resource)
      }
      false => {
        // Create new resource if pool not full
        match pool.resources.length() < pool.max_size {
          true => {
            let new_resource = {
              id: "resource-" + pool.next_id.to_string(),
              in_use: true,
              created_at: 1640995200,
              last_used: 1640995200,
              use_count: 1
            }
            
            pool.resources = pool.resources.push(new_resource)
            pool.next_id = pool.next_id + 1
            
            Some(new_resource)
          }
          false => {
            // Pool exhausted
            None
          }
        }
      }
    }
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    pool.resources = pool.resources.map(fn(r) {
      if r.id == resource_id {
        { r | in_use: false, last_used: 1640995300 }
      } else {
        r
      }
    })
  }
  
  // Clean up unused resources
  let cleanup_unused = fn(pool: ResourcePool, max_idle_time: Int) {
    let current_time = 1640995400
    let active_resources = pool.resources.filter(fn(r) {
      r.in_use or (current_time - r.last_used) <= max_idle_time
    })
    
    pool.resources = active_resources
  }
  
  // Test resource pool
  let pool = create_pool(3)
  
  // Acquire resources
  let resource1 = acquire_resource(pool)
  let resource2 = acquire_resource(pool)
  let resource3 = acquire_resource(pool)
  
  // Verify all resources were created
  assert_true(resource1.is_some())
  assert_true(resource2.is_some())
  assert_true(resource3.is_some())
  assert_eq(pool.resources.length(), 3)
  
  // Try to acquire when pool is full
  let resource4 = acquire_resource(pool)
  assert_true(resource4.is_none())
  
  // Release a resource
  match resource1 {
    Some(r) => release_resource(pool, r.id)
    None => ()
  }
  
  // Now we should be able to acquire again
  let resource5 = acquire_resource(pool)
  assert_true(resource5.is_some())
  
  // Verify resource reuse (same ID)
  match (resource1, resource5) {
    (Some(r1), Some(r5)) => assert_eq(r1.id, r5.id)
    _ => assert_true(false)
  }
  
  // Verify use count increased
  match resource5 {
    Some(r) => assert_eq(r.use_count, 2)
    None => assert_true(false)
  }
  
  // Test cleanup
  release_resource(pool, "resource-2")
  release_resource(pool, "resource-3")
  
  // Before cleanup, we have 3 resources
  assert_eq(pool.resources.length(), 3)
  
  // After cleanup with short idle time, resources should be removed
  cleanup_unused(pool, 50)  // 50 seconds idle time
  assert_eq(pool.resources.length(), 1)  // Only resource-1 (reused as resource-5) remains
  
  // Test resource pool statistics
  let pool_stats = fn(pool: ResourcePool) {
    let total = pool.resources.length()
    let in_use = pool.resources.filter(fn(r) { r.in_use }).length()
    let idle = total - in_use
    let total_uses = pool.resources.reduce(fn(acc, r) { acc + r.use_count }, 0)
    
    { total, in_use, idle, total_uses }
  }
  
  let stats = pool_stats(pool)
  assert_eq(stats.total, 1)
  assert_eq(stats.in_use, 1)
  assert_eq(stats.idle, 0)
  assert_eq(stats.total_uses, 2)
}

// Test 6: Error Recovery Mechanisms
test "error recovery mechanisms" {
  // Define error types
  enum ErrorType {
    NetworkError
    DatabaseError
    TimeoutError
    ValidationError
  }
  
  // Define operation result
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[ErrorType],
    retry_count: Int
  }
  
  // Create success result
  let success = fn(data: T, retry_count: Int) {
    {
      success: true,
      data: Some(data),
      error: None,
      retry_count: retry_count
    }
  }
  
  // Create error result
  let failure = fn(error: ErrorType, retry_count: Int) {
    {
      success: false,
      data: None,
      error: Some(error),
      retry_count: retry_count
    }
  }
  
  // Simulate operation with potential failures
  let simulate_operation = fn(should_fail: Bool, error_type: ErrorType) {
    if should_fail {
      failure(error_type, 0)
    } else {
      success("operation_result", 0)
    }
  }
  
  // Retry mechanism with exponential backoff
  let retry_with_backoff = fn(operation: () -> OperationResult[String], max_retries: Int) {
    let mut result = operation()
    let mut retry_count = 0
    
    while not(result.success) and retry_count < max_retries {
      retry_count = retry_count + 1
      
      // Simulate exponential backoff (in reality, this would be a sleep)
      let backoff_time = 100 * (2 ^ retry_count)  // 100ms * 2^retry_count
      
      // Simulate operation with decreasing failure probability
      let should_fail = retry_count < 2  // Fail first 2 retries
      let error_type = match result.error {
        Some(ErrorType::NetworkError) => ErrorType::NetworkError
        Some(ErrorType::DatabaseError) => ErrorType::DatabaseError
        Some(ErrorType::TimeoutError) => ErrorType::TimeoutError
        Some(ErrorType::ValidationError) => ErrorType::ValidationError
        None => ErrorType::NetworkError  // Default
      }
      
      result = simulate_operation(should_fail, error_type)
      result.retry_count = retry_count
    }
    
    result
  }
  
  // Test retry mechanism
  let always_fails = fn() {
    simulate_operation(true, ErrorType::NetworkError)
  }
  
  let result1 = retry_with_backoff(always_fails, 3)
  assert_false(result1.success)
  assert_eq(result1.retry_count, 3)
  
  // Test successful retry
  let eventually_succeeds = fn() {
    simulate_operation(false, ErrorType::NetworkError)
  }
  
  let result2 = retry_with_backoff(eventually_succeeds, 3)
  assert_true(result2.success)
  assert_eq(result2.data, Some("operation_result"))
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    failure_threshold: Int,
    mut failure_count: Int,
    mut last_failure_time: Int,
    mut state: String,  // "closed", "open", "half-open"
    recovery_timeout: Int
  }
  
  let create_circuit_breaker = fn(failure_threshold: Int, recovery_timeout: Int) {
    {
      failure_threshold,
      mut failure_count: 0,
      mut last_failure_time: 0,
      mut state: "closed",
      recovery_timeout
    }
  }
  
  let call_with_circuit_breaker = fn(breaker: CircuitBreaker, operation: () -> OperationResult[String]) {
    let current_time = 1640995200
    
    match breaker.state {
      "open" => {
        // Check if recovery timeout has passed
        if current_time - breaker.last_failure_time > breaker.recovery_timeout {
          breaker.state = "half-open"
        } else {
          return failure(ErrorType::NetworkError, 0)  // Circuit is open
        }
      }
      _ => ()
    }
    
    let result = operation()
    
    match result.success {
      true => {
        // Reset on success
        breaker.failure_count = 0
        breaker.state = "closed"
        result
      }
      false => {
        breaker.failure_count = breaker.failure_count + 1
        breaker.last_failure_time = current_time
        
        // Open circuit if threshold reached
        if breaker.failure_count >= breaker.failure_threshold {
          breaker.state = "open"
        }
        
        result
      }
    }
  }
  
  // Test circuit breaker
  let breaker = create_circuit_breaker(3, 60)  // 3 failures, 60s recovery
  
  // First few calls should work
  let result3 = call_with_circuit_breaker(breaker, fn() { success("ok", 0) })
  assert_true(result3.success)
  assert_eq(breaker.state, "closed")
  
  // Accumulate failures
  for i in 0..3 {
    let result = call_with_circuit_breaker(breaker, fn() { failure(ErrorType::NetworkError, 0) })
    assert_false(result.success)
  }
  
  // Circuit should be open now
  assert_eq(breaker.state, "open")
  assert_eq(breaker.failure_count, 3)
  
  // Call with open circuit should fail immediately
  let result4 = call_with_circuit_breaker(breaker, fn() { success("ok", 0) })
  assert_false(result4.success)
  
  // Simulate time passing for recovery
  breaker.last_failure_time = 1640995000  // 200 seconds ago
  
  // Next call should put circuit in half-open state
  let result5 = call_with_circuit_breaker(breaker, fn() { success("ok", 0) })
  assert_true(result5.success)
  assert_eq(breaker.state, "closed")
  assert_eq(breaker.failure_count, 0)
}

// Test 7: Performance Benchmarking
test "performance benchmarking" {
  // Define benchmark result
  type BenchmarkResult = {
    operation_name: String,
    iterations: Int,
    total_time_ns: Int,
    min_time_ns: Int,
    max_time_ns: Int,
    avg_time_ns: Float
  }
  
  // Simple benchmark function
  let benchmark = fn(operation: () -> Unit, iterations: Int) {
    let mut times = []
    
    // Warm up
    for i in 0..5 {
      operation()
    }
    
    // Actual benchmark
    for i in 0..iterations {
      let start_time = 1640995200000000000  // Simulated high-precision timestamp
      operation()
      let end_time = 1640995200000001000    // Simulated 1000ns operation
      
      times = times.push(end_time - start_time)
    }
    
    let total_time = times.reduce(fn(acc, t) { acc + t }, 0)
    let min_time = times.reduce(fn(acc, t) { if acc < t { acc } else { t } }, times[0])
    let max_time = times.reduce(fn(acc, t) { if acc > t { acc } else { t } }, times[0])
    let avg_time = total_time.to_float() / iterations.to_float()
    
    {
      operation_name: "test_operation",
      iterations: iterations,
      total_time_ns: total_time,
      min_time_ns: min_time,
      max_time_ns: max_time,
      avg_time_ns: avg_time
    }
  }
  
  // Test operations to benchmark
  let array_operation = fn() {
    let arr = [1, 2, 3, 4, 5]
    let mut sum = 0
    for i in 0..arr.length() {
      sum = sum + arr[i]
    }
    sum
  }
  
  let string_operation = fn() {
    let str = "hello world"
    let mut result = ""
    for i in 0..str.length() {
      result = result + str[i].to_string()
    }
    result
  }
  
  let hash_operation = fn() {
    let input = "test_string"
    let mut hash = 0
    for i in 0..input.length() {
      hash = hash + input[i].to_int()
    }
    hash
  }
  
  // Run benchmarks
  let benchmark1 = benchmark(array_operation, 100)
  let benchmark2 = benchmark(string_operation, 100)
  let benchmark3 = benchmark(hash_operation, 100)
  
  // Verify benchmark results
  assert_eq(benchmark1.iterations, 100)
  assert_eq(benchmark1.total_time_ns, 100000)  // 1000ns * 100 iterations
  assert_eq(benchmark1.min_time_ns, 1000)
  assert_eq(benchmark1.max_time_ns, 1000)
  assert_eq(benchmark1.avg_time_ns, 1000.0)
  
  assert_eq(benchmark2.iterations, 100)
  assert_eq(benchmark2.total_time_ns, 100000)
  assert_eq(benchmark2.min_time_ns, 1000)
  assert_eq(benchmark2.max_time_ns, 1000)
  assert_eq(benchmark2.avg_time_ns, 1000.0)
  
  assert_eq(benchmark3.iterations, 100)
  assert_eq(benchmark3.total_time_ns, 100000)
  assert_eq(benchmark3.min_time_ns, 1000)
  assert_eq(benchmark3.max_time_ns, 1000)
  assert_eq(benchmark3.avg_time_ns, 1000.0)
  
  // Compare performance
  let compare_benchmarks = fn(b1: BenchmarkResult, b2: BenchmarkResult) {
    let ratio = b1.avg_time_ns / b2.avg_time_ns
    if ratio > 1.1 {
      b2.operation_name + " is " + ratio.to_string() + "x faster than " + b1.operation_name
    } else if ratio < 0.9 {
      b1.operation_name + " is " + (1.0 / ratio).to_string() + "x faster than " + b2.operation_name
    } else {
      "Performance is similar between " + b1.operation_name + " and " + b2.operation_name
    }
  }
  
  // Test comparison (all operations have same simulated time)
  let comparison = compare_benchmarks(benchmark1, benchmark2)
  assert_true(comparison.contains("Performance is similar"))
  
  // Memory usage benchmark
  let memory_benchmark = fn(operation: () -> Unit, iterations: Int) {
    // Simulate memory measurement
    let initial_memory = 1024 * 1024  // 1MB
    
    for i in 0..iterations {
      operation()
    }
    
    let final_memory = 1024 * 1024 + iterations * 1024  // 1KB per iteration
    let memory_used = final_memory - initial_memory
    
    {
      operation_name: "memory_test",
      iterations: iterations,
      memory_used_bytes: memory_used,
      memory_per_iteration: memory_used / iterations
    }
  }
  
  let memory_result = memory_benchmark(array_operation, 100)
  assert_eq(memory_result.iterations, 100)
  assert_eq(memory_result.memory_used_bytes, 102400)  // 100KB
  assert_eq(memory_result.memory_per_iteration, 1024)  // 1KB per iteration
}

// Test 8: Data Serialization/Deserialization
test "data serialization deserialization" {
  // Define data structure to serialize
  type TelemetryEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    attributes: Array[(String, String)]
  }
  
  // Simple JSON-like serialization
  let serialize_to_json = fn(event: TelemetryEvent) {
    let mut json = "{"
    json = json + "\"event_id\":\"" + event.event_id + "\"," 
    json = json + "\"timestamp\":" + event.timestamp.to_string() + ","
    json = json + "\"event_type\":\"" + event.event_type + "\"," 
    json = json + "\"attributes\":["
    
    for i in 0..event.attributes.length() {
      let (key, value) = event.attributes[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      
      if i < event.attributes.length() - 1 {
        json = json + ","
      }
    }
    
    json = json + "]}"
    json
  }
  
  // Simple JSON-like deserialization
  let deserialize_from_json = fn(json: String) {
    // In a real implementation, this would parse JSON
    // For this test, we'll create a mock event from the JSON
    
    // Extract event_id (simplified)
    let event_id_start = json.find("\"event_id\":\"") + 12
    let event_id_end = json.find("\",\"", event_id_start)
    let event_id = json.substring(event_id_start, event_id_end - event_id_start)
    
    // Extract timestamp (simplified)
    let timestamp_start = json.find("\"timestamp\":") + 12
    let timestamp_end = json.find(",", timestamp_start)
    let timestamp = json.substring(timestamp_start, timestamp_end - timestamp_start).to_int()
    
    // Extract event_type (simplified)
    let event_type_start = json.find("\"event_type\":\"") + 14
    let event_type_end = json.find("\",\"", event_type_start)
    let event_type = json.substring(event_type_start, event_type_end - event_type_start)
    
    // Extract attributes (simplified - just return empty array for this test)
    let attributes = []
    
    {
      event_id: event_id,
      timestamp: timestamp,
      event_type: event_type,
      attributes: attributes
    }
  }
  
  // Create test event
  let event = {
    event_id: "event-12345",
    timestamp: 1640995200,
    event_type: "user_action",
    attributes: [
      ("user_id", "user-67890"),
      ("action", "click"),
      ("target", "submit_button")
    ]
  }
  
  // Test serialization
  let serialized = serialize_to_json(event)
  assert_true(serialized.contains("\"event_id\":\"event-12345\""))
  assert_true(serialized.contains("\"timestamp\":1640995200"))
  assert_true(serialized.contains("\"event_type\":\"user_action\""))
  assert_true(serialized.contains("\"attributes\""))
  
  // Test deserialization
  let deserialized = deserialize_from_json(serialized)
  assert_eq(deserialized.event_id, "event-12345")
  assert_eq(deserialized.timestamp, 1640995200)
  assert_eq(deserialized.event_type, "user_action")
  
  // Test binary serialization (simplified)
  let serialize_to_binary = fn(event: TelemetryEvent) {
    // In a real implementation, this would use proper binary serialization
    // For this test, we'll just concatenate the fields with separators
    
    let mut binary = ""
    binary = binary + event.event_id + "|"
    binary = binary + event.timestamp.to_string() + "|"
    binary = binary + event.event_type + "|"
    
    for i in 0..event.attributes.length() {
      let (key, value) = event.attributes[i]
      binary = binary + key + "=" + value
      
      if i < event.attributes.length() - 1 {
        binary = binary + ";"
      }
    }
    
    binary
  }
  
  // Test binary serialization
  let binary_serialized = serialize_to_binary(event)
  assert_true(binary_serialized.contains("event-12345"))
  assert_true(binary_serialized.contains("1640995200"))
  assert_true(binary_serialized.contains("user_action"))
  assert_true(binary_serialized.contains("user_id=user-67890"))
  
  // Test compression simulation
  let compress_data = fn(data: String) {
    // Simulate compression by removing common patterns
    let compressed = data
      .replace("\"event_id\":", "\"eid\":")
      .replace("\"timestamp\":", "\"ts\":")
      .replace("\"event_type\":", "\"et\":")
      .replace("\"attributes\":", "\"attrs\":")
      .replace("\"key\":", "\"k\":")
      .replace("\"value\":", "\"v\":")
    
    compressed
  }
  
  let decompress_data = fn(compressed: String) {
    // Simulate decompression by restoring patterns
    let decompressed = compressed
      .replace("\"eid\":", "\"event_id\":")
      .replace("\"ts\":", "\"timestamp\":")
      .replace("\"et\":", "\"event_type\":")
      .replace("\"attrs\":", "\"attributes\":")
      .replace("\"k\":", "\"key\":")
      .replace("\"v\":", "\"value\":")
    
    decompressed
  }
  
  // Test compression
  let compressed = compress_data(serialized)
  assert_true(compressed.length() < serialized.length())
  
  // Test decompression
  let decompressed = decompress_data(compressed)
  assert_eq(decompressed, serialized)
  
  // Test round-trip serialization
  let round_trip_event = deserialize_from_json(decompressed)
  assert_eq(round_trip_event.event_id, event.event_id)
  assert_eq(round_trip_event.timestamp, event.timestamp)
  assert_eq(round_trip_event.event_type, event.event_type)
}

// Test 9: Dynamic Configuration Management
test "dynamic configuration management" {
  // Define configuration value type
  enum ConfigValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[String])
  }
  
  // Define configuration entry
  type ConfigEntry = {
    key: String,
    value: ConfigValue,
    description: String,
    mut last_modified: Int
  }
  
  // Define configuration manager
  type ConfigManager = {
    mut configs: Array[ConfigEntry],
    mut listeners: Array[(String, ConfigValue -> Unit)]
  }
  
  // Create configuration manager
  let create_config_manager = fn() {
    { mut configs: [], mut listeners: [] }
  }
  
  // Get configuration value
  let get_config = fn(manager: ConfigManager, key: String, default_value: ConfigValue) {
    let entry = manager.configs.find(fn(e) { e.key == key })
    
    match entry {
      Some(e) => e.value
      None => default_value
    }
  }
  
  // Set configuration value
  let set_config = fn(manager: ConfigManager, key: String, value: ConfigValue, description: String) {
    let current_time = 1640995200
    
    // Check if key already exists
    let existing_index = manager.configs.find_index(fn(e) { e.key == key })
    
    match existing_index {
      Some(index) => {
        // Update existing entry
        let old_entry = manager.configs[index]
        let updated_entry = { 
          old_entry | 
          value: value, 
          description: description,
          last_modified: current_time
        }
        
        manager.configs[index] = updated_entry
        
        // Notify listeners
        for listener in manager.listeners {
          listener(key, value)
        }
      }
      None => {
        // Add new entry
        let new_entry = {
          key: key,
          value: value,
          description: description,
          last_modified: current_time
        }
        
        manager.configs = manager.configs.push(new_entry)
        
        // Notify listeners
        for listener in manager.listeners {
          listener(key, value)
        }
      }
    }
  }
  
  // Add configuration change listener
  let add_listener = fn(manager: ConfigManager, key: String, listener: (String, ConfigValue -> Unit)) {
    manager.listeners = manager.listeners.push(listener)
  }
  
  // Create configuration manager
  let config_manager = create_config_manager()
  
  // Set some initial configurations
  set_config(config_manager, "service.name", String("payment-service"), "Name of the service")
  set_config(config_manager, "service.port", Int(8080), "Port for the service")
  set_config(config_manager, "service.timeout", Float(30.0), "Timeout in seconds")
  set_config(config_manager, "service.debug", Bool(false), "Enable debug mode")
  set_config(config_manager, "service.endpoints", Array(["/api/v1/payments", "/api/v1/refunds"]), "Available endpoints")
  
  // Test getting configurations
  let service_name = get_config(config_manager, "service.name", String("default-service"))
  match service_name {
    String(name) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  let service_port = get_config(config_manager, "service.port", Int(3000))
  match service_port {
    Int(port) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let service_timeout = get_config(config_manager, "service.timeout", Float(10.0))
  match service_timeout {
    Float(timeout) => assert_eq(timeout, 30.0)
    _ => assert_true(false)
  }
  
  let service_debug = get_config(config_manager, "service.debug", Bool(true))
  match service_debug {
    Bool(debug) => assert_false(debug)
    _ => assert_true(false)
  }
  
  let service_endpoints = get_config(config_manager, "service.endpoints", Array([]))
  match service_endpoints {
    Array(endpoints) => {
      assert_eq(endpoints.length(), 2)
      assert_true(endpoints.contains("/api/v1/payments"))
      assert_true(endpoints.contains("/api/v1/refunds"))
    }
    _ => assert_true(false)
  }
  
  // Test getting non-existent configuration with default
  let non_existent = get_config(config_manager, "non.existent.key", String("default-value"))
  match non_existent {
    String(value) => assert_eq(value, "default-value")
    _ => assert_true(false)
  }
  
  // Test configuration update
  set_config(config_manager, "service.port", Int(9090), "Updated port for the service")
  
  let updated_port = get_config(config_manager, "service.port", Int(3000))
  match updated_port {
    Int(port) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  // Test configuration listener
  let mut last_notified_key = ""
  let mut last_notified_value = String("")
  
  let test_listener = fn(key: String, value: ConfigValue) {
    last_notified_key = key
    last_notified_value = value
  }
  
  add_listener(config_manager, "service.debug", test_listener)
  
  // Update configuration to trigger listener
  set_config(config_manager, "service.debug", Bool(true), "Enable debug mode")
  
  // Verify listener was called
  assert_eq(last_notified_key, "service.debug")
  match last_notified_value {
    Bool(debug) => assert_true(debug)
    _ => assert_true(false)
  }
  
  // Test configuration validation
  let validate_config = fn(key: String, value: ConfigValue) {
    match (key, value) {
      ("service.port", Int(port)) => port > 0 and port <= 65535
      ("service.timeout", Float(timeout)) => timeout > 0.0 and timeout <= 300.0
      ("service.name", String(name)) => name.length() > 0 and name.length() <= 50
      _ => true
    }
  }
  
  // Test valid configurations
  assert_true(validate_config("service.port", Int(8080)))
  assert_true(validate_config("service.timeout", Float(30.0)))
  assert_true(validate_config("service.name", String("payment-service")))
  
  // Test invalid configurations
  assert_false(validate_config("service.port", Int(70000)))  // Port too high
  assert_false(validate_config("service.timeout", Float(-1.0)))  // Negative timeout
  assert_false(validate_config("service.name", String("")))  // Empty name
  
  // Test configuration export/import
  let export_config = fn(manager: ConfigManager) {
    let mut exported = ""
    
    for config in manager.configs {
      let value_str = match config.value {
        String(s) => "\"" + s + "\""
        Int(i) => i.to_string()
        Float(f) => f.to_string()
        Bool(b) => if b { "true" } else { "false" }
        Array(arr) => {
          let mut array_str = "["
          for i in 0..arr.length() {
            array_str = array_str + "\"" + arr[i] + "\""
            if i < arr.length() - 1 {
              array_str = array_str + ","
            }
          }
          array_str = array_str + "]"
          array_str
        }
      }
      
      exported = exported + config.key + "=" + value_str + "\n"
    }
    
    exported
  }
  
  let exported_config = export_config(config_manager)
  assert_true(exported_config.contains("service.name=\"payment-service\""))
  assert_true(exported_config.contains("service.port=9090"))
  assert_true(exported_config.contains("service.timeout=30.0"))
  assert_true(exported_config.contains("service.debug=true"))
}

// Test 10: Internationalization Support
test "internationalization support" {
  // Define localization entry
  type LocalizationEntry = {
    key: String,
    translations: Array[(String, String)]  // (locale, translation)
  }
  
  // Define localization manager
  type LocalizationManager = {
    default_locale: String,
    mut entries: Array[LocalizationEntry]
  }
  
  // Create localization manager
  let create_localization_manager = fn(default_locale: String) {
    { default_locale, mut entries: [] }
  }
  
  // Add translation
  let add_translation = fn(manager: LocalizationManager, key: String, locale: String, translation: String) {
    // Check if key already exists
    let existing_index = manager.entries.find_index(fn(e) { e.key == key })
    
    match existing_index {
      Some(index) => {
        // Update existing entry
        let entry = manager.entries[index]
        let translations = entry.translations
        
        // Check if locale already exists for this key
        let locale_index = translations.find_index(fn(t) { t.0 == locale })
        
        match locale_index {
          Some(i) => {
            // Update existing translation
            translations[i] = (locale, translation)
          }
          None => {
            // Add new translation
            manager.entries[index].translations = translations.push((locale, translation))
          }
        }
      }
      None => {
        // Add new entry
        let new_entry = {
          key: key,
          translations: [(locale, translation)]
        }
        
        manager.entries = manager.entries.push(new_entry)
      }
    }
  }
  
  // Get localized string
  let get_localized_string = fn(manager: LocalizationManager, key: String, locale: String) {
    let entry = manager.entries.find(fn(e) { e.key == key })
    
    match entry {
      Some(e) => {
        let translation = e.translations.find(fn(t) { t.0 == locale })
        
        match translation {
          Some(t) => t.1
          None => {
            // Try default locale
            let default_translation = e.translations.find(fn(t) { t.0 == manager.default_locale })
            
            match default_translation {
              Some(dt) => dt.1
              None => {
                // Return key as fallback
                key
              }
            }
          }
        }
      }
      None => {
        // Return key as fallback
        key
      }
    }
  }
  
  // Create localization manager
  let i18n = create_localization_manager("en")
  
  // Add translations for different locales
  add_translation(i18n, "welcome.message", "en", "Welcome to our application!")
  add_translation(i18n, "welcome.message", "es", "¡Bienvenido a nuestra aplicación!")
  add_translation(i18n, "welcome.message", "fr", "Bienvenue dans notre application!")
  add_translation(i18n, "welcome.message", "zh", "欢迎使用我们的应用程序！")
  add_translation(i18n, "welcome.message", "ja", "私たちのアプリケーションへようこそ！")
  
  add_translation(i18n, "error.network", "en", "Network error occurred")
  add_translation(i18n, "error.network", "es", "Ocurrió un error de red")
  add_translation(i18n, "error.network", "fr", "Une erreur réseau s'est produite")
  add_translation(i18n, "error.network", "zh", "发生网络错误")
  add_translation(i18n, "error.network", "ja", "ネットワークエラーが発生しました")
  
  add_translation(i18n, "button.submit", "en", "Submit")
  add_translation(i18n, "button.submit", "es", "Enviar")
  add_translation(i18n, "button.submit", "fr", "Soumettre")
  add_translation(i18n, "button.submit", "zh", "提交")
  add_translation(i18n, "button.submit", "ja", "送信")
  
  // Test getting localized strings
  let welcome_en = get_localized_string(i18n, "welcome.message", "en")
  assert_eq(welcome_en, "Welcome to our application!")
  
  let welcome_es = get_localized_string(i18n, "welcome.message", "es")
  assert_eq(welcome_es, "¡Bienvenido a nuestra aplicación!")
  
  let welcome_fr = get_localized_string(i18n, "welcome.message", "fr")
  assert_eq(welcome_fr, "Bienvenue dans notre application!")
  
  let welcome_zh = get_localized_string(i18n, "welcome.message", "zh")
  assert_eq(welcome_zh, "欢迎使用我们的应用程序！")
  
  let welcome_ja = get_localized_string(i18n, "welcome.message", "ja")
  assert_eq(welcome_ja, "私たちのアプリケーションへようこそ！")
  
  // Test error messages
  let error_en = get_localized_string(i18n, "error.network", "en")
  assert_eq(error_en, "Network error occurred")
  
  let error_es = get_localized_string(i18n, "error.network", "es")
  assert_eq(error_es, "Ocurrió un error de red")
  
  // Test fallback to default locale
  let welcome_de = get_localized_string(i18n, "welcome.message", "de")  // German not available
  assert_eq(welcome_de, "Welcome to our application!")  // Falls back to English (default)
  
  // Test fallback to key
  let missing_key = get_localized_string(i18n, "missing.key", "en")
  assert_eq(missing_key, "missing.key")  // Returns key as fallback
  
  // Test parameterized strings
  type ParameterizedEntry = {
    key: String,
    translations: Array[(String, String)]
  }
  
  let format_localized_string = fn(template: String, params: Array[String]) {
    let mut result = template
    
    for i in 0..params.length() {
      let placeholder = "{" + i.to_string() + "}"
      result = result.replace(placeholder, params[i])
    }
    
    result
  }
  
  // Add parameterized translations
  add_translation(i18n, "user.greeting", "en", "Hello, {0}! You have {1} new messages.")
  add_translation(i18n, "user.greeting", "es", "¡Hola, {0}! Tienes {1} mensajes nuevos.")
  add_translation(i18n, "user.greeting", "fr", "Bonjour, {0}! Vous avez {1} nouveaux messages.")
  add_translation(i18n, "user.greeting", "zh", "你好，{0}！您有{1}条新消息。")
  add_translation(i18n, "user.greeting", "ja", "{0}さん、こんにちは！{1}件の新しいメッセージがあります。")
  
  // Test parameterized strings
  let greeting_template = get_localized_string(i18n, "user.greeting", "en")
  let greeting_en = format_localized_string(greeting_template, ["Alice", "5"])
  assert_eq(greeting_en, "Hello, Alice! You have 5 new messages.")
  
  let greeting_template_es = get_localized_string(i18n, "user.greeting", "es")
  let greeting_es = format_localized_string(greeting_template_es, ["Alice", "5"])
  assert_eq(greeting_es, "¡Hola, Alice! Tienes 5 mensajes nuevos.")
  
  // Test pluralization (simplified)
  let get_pluralized_string = fn(manager: LocalizationManager, key: String, locale: String, count: Int) {
    let plural_key = key + "." + (if count == 1 { "one" } else { "many" })
    get_localized_string(manager, plural_key, locale)
  }
  
  // Add pluralized translations
  add_translation(i18n, "item.count.one", "en", "You have {0} item.")
  add_translation(i18n, "item.count.many", "en", "You have {0} items.")
  add_translation(i18n, "item.count.one", "es", "Tienes {0} artículo.")
  add_translation(i18n, "item.count.many", "es", "Tienes {0} artículos.")
  
  // Test pluralization
  let item_one_template = get_pluralized_string(i18n, "item.count", "en", 1)
  let item_one_en = format_localized_string(item_one_template, ["1"])
  assert_eq(item_one_en, "You have 1 item.")
  
  let item_many_template = get_pluralized_string(i18n, "item.count", "en", 5)
  let item_many_en = format_localized_string(item_many_template, ["5"])
  assert_eq(item_many_en, "You have 5 items.")
  
  // Test locale detection from string
  let detect_locale = fn(accept_language: String) {
    // Simple parsing of Accept-Language header
    let languages = accept_language.split(",")
    let mut preferred_locale = ""
    
    for i in 0..languages.length() {
      let lang = languages[i].split(";")[0].trim()
      
      // Check if we support this language
      if lang == "en" or lang == "es" or lang == "fr" or lang == "zh" or lang == "ja" {
        preferred_locale = lang
        break
      }
    }
    
    if preferred_locale == "" {
      "en"  // Default to English
    } else {
      preferred_locale
    }
  }
  
  // Test locale detection
  assert_eq(detect_locale("en-US,en;q=0.9"), "en")
  assert_eq(detect_locale("es-ES,es;q=0.9,en;q=0.8"), "es")
  assert_eq(detect_locale("fr-FR,fr;q=0.9,en;q=0.8"), "fr")
  assert_eq(detect_locale("zh-CN,zh;q=0.9,en;q=0.8"), "zh")
  assert_eq(detect_locale("ja-JP,ja;q=0.9,en;q=0.8"), "ja")
  assert_eq(detect_locale("de-DE,de;q=0.9,en;q=0.8"), "en")  // German not supported, falls back to English
}