// Azimuth High Quality Comprehensive Test Suite
// This file contains high-quality test cases covering various aspects of the Azimuth telemetry system

// Test 1: Advanced Telemetry Data Processing
test "advanced telemetry data processing and aggregation" {
  // Define telemetry data structures
  type TelemetryMetric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[String],
    metric_type: String
  }
  
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample metrics
  let metrics = [
    { name: "cpu_usage", value: 75.5, timestamp: 1640995200, tags: ["service:api", "env:prod"], metric_type: "gauge" },
    { name: "memory_usage", value: 1024.0, timestamp: 1640995200, tags: ["service:api", "env:prod"], metric_type: "gauge" },
    { name: "request_count", value: 150.0, timestamp: 1640995200, tags: ["service:api", "env:prod"], metric_type: "counter" },
    { name: "response_time", value: 125.5, timestamp: 1640995200, tags: ["service:api", "env:prod"], metric_type: "histogram" },
    { name: "error_rate", value: 2.5, timestamp: 1640995200, tags: ["service:api", "env:prod"], metric_type: "gauge" }
  ]
  
  // Test metric aggregation by type
  let aggregate_by_type = fn(metrics: Array[TelemetryMetric]) {
    let mut result = []
    let mut processed_types = []
    
    for metric in metrics {
      if not(processed_types.contains(metric.metric_type)) {
        processed_types = processed_types.push(metric.metric_type)
        
        let type_metrics = metrics.filter(fn(m) { m.metric_type == metric.metric_type })
        let total_value = type_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
        let count = type_metrics.length().to_float()
        
        result = result.push({
          metric_type: metric.metric_type,
          count: count,
          total_value: total_value,
          average_value: total_value / count
        })
      }
    }
    
    result
  }
  
  let aggregated = aggregate_by_type(metrics)
  assert_eq(aggregated.length(), 3)
  
  let gauge_metrics = aggregated.filter(fn(a) { a.metric_type == "gauge" })
  assert_eq(gauge_metrics.length(), 1)
  assert_eq(gauge_metrics[0].count, 3.0)
  assert_eq(gauge_metrics[0].total_value, 1802.0)
  assert_eq(gauge_metrics[0].average_value, 600.67)
  
  // Test time window aggregation
  let aggregate_by_time_window = fn(metrics: Array[TelemetryMetric], window_size: Int) {
    let mut result = []
    let mut processed_windows = []
    
    for metric in metrics {
      let window_start = (metric.timestamp / window_size) * window_size
      
      if not(processed_windows.contains(window_start)) {
        processed_windows = processed_windows.push(window_start)
        
        let window_metrics = metrics.filter(fn(m) {
          let m_window_start = (m.timestamp / window_size) * window_size
          m_window_start == window_start
        })
        
        let max_value = window_metrics.reduce(fn(acc, m) { 
          if m.value > acc { m.value } else { acc } 
        }, 0.0)
        
        let min_value = window_metrics.reduce(fn(acc, m) { 
          if m.value < acc { m.value } else { acc } 
        }, 999999.0)
        
        result = result.push({
          window_start: window_start,
          window_end: window_start + window_size,
          metric_count: window_metrics.length(),
          max_value: max_value,
          min_value: min_value
        })
      }
    }
    
    result
  }
  
  let time_aggregated = aggregate_by_time_window(metrics, 3600) // 1 hour window
  assert_eq(time_aggregated.length(), 1)
  assert_eq(time_aggregated[0].window_start, 1640995200)
  assert_eq(time_aggregated[0].metric_count, 5)
  assert_eq(time_aggregated[0].max_value, 1024.0)
  assert_eq(time_aggregated[0].min_value, 2.5)
  
  // Test span duration analysis
  let spans = [
    { trace_id: "trace-1", span_id: "span-1", parent_span_id: None, operation_name: "http_request", start_time: 1640995200, end_time: 1640995250, status: "ok", attributes: [] },
    { trace_id: "trace-1", span_id: "span-2", parent_span_id: Some("span-1"), operation_name: "database_query", start_time: 1640995205, end_time: 1640995230, status: "ok", attributes: [] },
    { trace_id: "trace-1", span_id: "span-3", parent_span_id: Some("span-1"), operation_name: "cache_lookup", start_time: 1640995230, end_time: 1640995240, status: "ok", attributes: [] },
    { trace_id: "trace-2", span_id: "span-4", parent_span_id: None, operation_name: "http_request", start_time: 1640995300, end_time: 1640995350, status: "error", attributes: [] }
  ]
  
  let analyze_span_durations = fn(spans: Array[TelemetrySpan]) {
    let durations = spans.map(fn(s) { s.end_time - s.start_time })
    let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
    let avg_duration = total_duration / durations.length()
    
    let max_duration = durations.reduce(fn(acc, d) { 
      if d > acc { d } else { acc } 
    }, 0)
    
    let min_duration = durations.reduce(fn(acc, d) { 
      if d < acc { d } else { acc } 
    }, 999999)
    
    let error_spans = spans.filter(fn(s) { s.status == "error" })
    let success_rate = ((spans.length() - error_spans.length()).to_float() / spans.length().to_float()) * 100.0
    
    {
      total_spans: spans.length(),
      total_duration: total_duration,
      average_duration: avg_duration,
      max_duration: max_duration,
      min_duration: min_duration,
      error_count: error_spans.length(),
      success_rate: success_rate
    }
  }
  
  let span_analysis = analyze_span_durations(spans)
  assert_eq(span_analysis.total_spans, 4)
  assert_eq(span_analysis.total_duration, 190)
  assert_eq(span_analysis.average_duration, 47)
  assert_eq(span_analysis.max_duration, 50)
  assert_eq(span_analysis.min_duration, 10)
  assert_eq(span_analysis.error_count, 1)
  assert_eq(span_analysis.success_rate, 75.0)
}

// Test 2: Concurrent Safety and Thread Management
test "concurrent safety and thread management" {
  // Simulate concurrent resource access
  type SharedResource = {
    counter: Int,
    max_value: Int,
    access_log: Array[String]
  }
  
  let create_shared_resource = fn(initial_value: Int, max_limit: Int) {
    {
      counter: initial_value,
      max_value: max_limit,
      access_log: []
    }
  }
  
  // Simulate thread-safe increment operation
  let safe_increment = fn(resource: SharedResource, thread_id: String) {
    if resource.counter < resource.max_value {
      let new_counter = resource.counter + 1
      let new_log = resource.access_log.push("Thread " + thread_id + " incremented to " + new_counter.to_string())
      
      {
        counter: new_counter,
        max_value: resource.max_value,
        access_log: new_log
      }
    } else {
      let new_log = resource.access_log.push("Thread " + thread_id + " failed - max reached")
      
      {
        counter: resource.counter,
        max_value: resource.max_value,
        access_log: new_log
      }
    }
  }
  
  // Test concurrent access simulation
  let initial_resource = create_shared_resource(0, 10)
  
  // Simulate multiple threads accessing the resource
  let thread_operations = [
    ("thread-1", safe_increment),
    ("thread-2", safe_increment),
    ("thread-3", safe_increment),
    ("thread-4", safe_increment),
    ("thread-5", safe_increment)
  ]
  
  // Process operations in sequence (simulating concurrent access)
  let mut final_resource = initial_resource
  for (thread_id, operation) in thread_operations {
    final_resource = operation(final_resource, thread_id)
  }
  
  assert_eq(final_resource.counter, 5)
  assert_eq(final_resource.access_log.length(), 5)
  assert_true(final_resource.access_log.contains("Thread thread-1 incremented to 1"))
  assert_true(final_resource.access_log.contains("Thread thread-5 incremented to 5"))
  
  // Test resource pool management
  type ResourcePool = {
    resources: Array[String],
    allocated: Array[String],
    max_size: Int
  }
  
  let create_resource_pool = fn(size: Int) {
    let mut resources = []
    for i in 0..size {
      resources = resources.push("resource-" + i.to_string())
    }
    
    {
      resources: resources,
      allocated: [],
      max_size: size
    }
  }
  
  let allocate_resource = fn(pool: ResourcePool, requester: String) {
    if pool.resources.length() > 0 {
      let resource = pool.resources[0]
      let remaining_resources = pool.resources.slice(1, pool.resources.length())
      
      {
        resources: remaining_resources,
        allocated: pool.allocated.push(resource + "->" + requester),
        max_size: pool.max_size
      }
    } else {
      pool // No available resources
    }
  }
  
  let release_resource = fn(pool: ResourcePool, resource: String) {
    let mut new_allocated = []
    let mut found = false
    
    for allocated_resource in pool.allocated {
      if allocated_resource.contains(resource) and not(found) {
        found = true
      } else {
        new_allocated = new_allocated.push(allocated_resource)
      }
    }
    
    if found {
      {
        resources: pool.resources.push(resource),
        allocated: new_allocated,
        max_size: pool.max_size
      }
    } else {
      pool // Resource not found in allocated list
    }
  }
  
  // Test resource pool operations
  let pool = create_resource_pool(3)
  assert_eq(pool.resources.length(), 3)
  assert_eq(pool.allocated.length(), 0)
  
  let pool_after_alloc1 = allocate_resource(pool, "requester-1")
  assert_eq(pool_after_alloc1.resources.length(), 2)
  assert_eq(pool_after_alloc1.allocated.length(), 1)
  
  let pool_after_alloc2 = allocate_resource(pool_after_alloc1, "requester-2")
  assert_eq(pool_after_alloc2.resources.length(), 1)
  assert_eq(pool_after_alloc2.allocated.length(), 2)
  
  let pool_after_alloc3 = allocate_resource(pool_after_alloc2, "requester-3")
  assert_eq(pool_after_alloc3.resources.length(), 0)
  assert_eq(pool_after_alloc3.allocated.length(), 3)
  
  // Test allocation when pool is empty
  let pool_after_alloc4 = allocate_resource(pool_after_alloc3, "requester-4")
  assert_eq(pool_after_alloc4.resources.length(), 0)
  assert_eq(pool_after_alloc4.allocated.length(), 3)
  
  // Test resource release
  let pool_after_release = release_resource(pool_after_alloc4, "resource-1")
  assert_eq(pool_after_release.resources.length(), 1)
  assert_eq(pool_after_release.allocated.length(), 2)
  
  // Test deadlock prevention simulation
  let detect_deadlock = fn(resources: Array[String], wait_for: Array[(String, String)]) {
    let mut has_cycle = false
    
    // Simple cycle detection
    for (resource, waiter) in wait_for {
      for (other_resource, other_waiter) in wait_for {
        if waiter == other_resource and other_waiter == resource {
          has_cycle = true
        }
      }
    }
    
    has_cycle
  }
  
  let wait_graph = [
    ("resource-1", "thread-2"),
    ("resource-2", "thread-3"),
    ("resource-3", "thread-1")
  ]
  
  // No deadlock in this case
  assert_false(detect_deadlock(["resource-1", "resource-2", "resource-3"], wait_graph))
  
  let deadlock_graph = [
    ("resource-1", "thread-2"),
    ("resource-2", "thread-1")
  ]
  
  // Deadlock detected
  assert_true(detect_deadlock(["resource-1", "resource-2"], deadlock_graph))
}

// Test 3: Error Handling and Recovery Mechanisms
test "error handling and recovery mechanisms" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    DataCorruption(String)
    ResourceExhausted(String)
    ValidationError(String)
    ServiceUnavailable(String)
  }
  
  type RecoveryResult = {
    success: Bool,
    attempts: Int,
    error: Option[TelemetryError],
    recovery_time: Int
  }
  
  // Implement retry mechanism with exponential backoff
  let retry_with_backoff = fn(operation: () -> Result[String, TelemetryError], max_attempts: Int) {
    let mut attempts = 0
    let mut base_delay = 100 // 100ms base delay
    let mut total_time = 0
    let mut last_error = None
    
    while attempts < max_attempts {
      attempts = attempts + 1
      
      match operation() {
        Ok(result) => {
          return {
            success: true,
            attempts: attempts,
            error: None,
            recovery_time: total_time
          }
        }
        Err(error) => {
          last_error = Some(error)
          
          if attempts < max_attempts {
            let delay = base_delay * (2 ^ (attempts - 1)) // Exponential backoff
            total_time = total_time + delay
            
            // In a real implementation, we would wait here
            // For testing, we just simulate the time passing
          }
        }
      }
    }
    
    {
      success: false,
      attempts: attempts,
      error: last_error,
      recovery_time: total_time
    }
  }
  
  // Test successful operation
  let successful_operation = fn() {
    Ok("operation completed successfully")
  }
  
  let success_result = retry_with_backoff(successful_operation, 3)
  assert_true(success_result.success)
  assert_eq(success_result.attempts, 1)
  assert_eq(success_result.error, None)
  assert_eq(success_result.recovery_time, 0)
  
  // Test operation that fails initially but succeeds later
  let mut attempt_count = 0
  let eventually_successful_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(TelemetryError::NetworkTimeout("Connection timeout"))
    } else {
      Ok("operation succeeded after retries")
    }
  }
  
  let eventual_result = retry_with_backoff(eventually_successful_operation, 5)
  assert_true(eventual_result.success)
  assert_eq(eventual_result.attempts, 3)
  assert_eq(eventual_result.error, None)
  assert_true(eventual_result.recovery_time > 0)
  
  // Test operation that always fails
  let always_failing_operation = fn() {
    Err(TelemetryError::ServiceUnavailable("Service down"))
  }
  
  let failure_result = retry_with_backoff(always_failing_operation, 3)
  assert_false(failure_result.success)
  assert_eq(failure_result.attempts, 3)
  
  match failure_result.error {
    Some(TelemetryError::ServiceUnavailable(msg)) => assert_eq(msg, "Service down")
    _ => assert_true(false)
  }
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    state: String, // "closed", "open", "half-open"
    failure_count: Int,
    failure_threshold: Int,
    timeout: Int,
    last_failure_time: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int, timeout: Int) {
    {
      state: "closed",
      failure_count: 0,
      failure_threshold: threshold,
      timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let call_with_circuit_breaker = fn(breaker: CircuitBreaker, operation: () -> Result[String, TelemetryError], current_time: Int) {
    match breaker.state {
      "open" => {
        if current_time - breaker.last_failure_time > breaker.timeout {
          // Try half-open state
          match operation() {
            Ok(result) => {
              (Ok(result), {
                state: "closed",
                failure_count: 0,
                failure_threshold: breaker.failure_threshold,
                timeout: breaker.timeout,
                last_failure_time: 0
              })
            }
            Err(error) => {
              (Err(error), {
                state: "open",
                failure_count: breaker.failure_count + 1,
                failure_threshold: breaker.failure_threshold,
                timeout: breaker.timeout,
                last_failure_time: current_time
              })
            }
          }
        } else {
          // Circuit is still open
          (Err(TelemetryError::ServiceUnavailable("Circuit breaker is open")), breaker)
        }
      }
      "closed" => {
        match operation() {
          Ok(result) => {
            (Ok(result), breaker)
          }
          Err(error) => {
            let new_failure_count = breaker.failure_count + 1
            let new_state = if new_failure_count >= breaker.failure_threshold { "open" } else { "closed" }
            
            (Err(error), {
              state: new_state,
              failure_count: new_failure_count,
              failure_threshold: breaker.failure_threshold,
              timeout: breaker.timeout,
              last_failure_time: current_time
            })
          }
        }
      }
      _ => (Err(TelemetryError::ValidationError("Invalid circuit breaker state")), breaker)
    }
  }
  
  // Test circuit breaker behavior
  let breaker = create_circuit_breaker(3, 5000) // 3 failures threshold, 5 second timeout
  
  // First few calls succeed
  let good_operation = fn() { Ok("success") }
  
  let (result1, breaker1) = call_with_circuit_breaker(breaker, good_operation, 1000)
  assert_true(result1.is_ok())
  assert_eq(breaker1.state, "closed")
  assert_eq(breaker1.failure_count, 0)
  
  // Simulate failures
  let bad_operation = fn() { Err(TelemetryError::NetworkTimeout("Timeout")) }
  
  let (result2, breaker2) = call_with_circuit_breaker(breaker1, bad_operation, 2000)
  assert_true(result2.is_err())
  assert_eq(breaker2.state, "closed")
  assert_eq(breaker2.failure_count, 1)
  
  let (result3, breaker3) = call_with_circuit_breaker(breaker2, bad_operation, 3000)
  assert_true(result3.is_err())
  assert_eq(breaker3.state, "closed")
  assert_eq(breaker3.failure_count, 2)
  
  // This failure should open the circuit
  let (result4, breaker4) = call_with_circuit_breaker(breaker3, bad_operation, 4000)
  assert_true(result4.is_err())
  assert_eq(breaker4.state, "open")
  assert_eq(breaker4.failure_count, 3)
  assert_eq(breaker4.last_failure_time, 4000)
  
  // Subsequent calls should fail immediately when circuit is open
  let (result5, breaker5) = call_with_circuit_breaker(breaker4, good_operation, 4500)
  assert_true(result5.is_err())
  assert_eq(breaker5.state, "open")
  
  // After timeout, circuit should try half-open state
  let (result6, breaker6) = call_with_circuit_breaker(breaker5, good_operation, 10000)
  assert_true(result6.is_ok())
  assert_eq(breaker6.state, "closed")
  assert_eq(breaker6.failure_count, 0)
}

// Test 4: Performance Optimization and Resource Management
test "performance optimization and resource management" {
  // Test memory pool for efficient object allocation
  type MemoryPool = {
    available_objects: Array[String],
    allocated_objects: Array[String],
    total_allocated: Int,
    total_released: Int
  }
  
  let create_memory_pool = fn(initial_size: Int) {
    let mut objects = []
    for i in 0..initial_size {
      objects = objects.push("object-" + i.to_string())
    }
    
    {
      available_objects: objects,
      allocated_objects: [],
      total_allocated: 0,
      total_released: 0
    }
  }
  
  let pool_allocate = fn(pool: MemoryPool) {
    if pool.available_objects.length() > 0 {
      let object = pool.available_objects[0]
      let remaining = pool.available_objects.slice(1, pool.available_objects.length())
      
      {
        available_objects: remaining,
        allocated_objects: pool.allocated_objects.push(object),
        total_allocated: pool.total_allocated + 1,
        total_released: pool.total_released
      }
    } else {
      // Create new object if pool is empty
      let new_object = "object-" + (pool.total_allocated + pool.allocated_objects.length()).to_string()
      
      {
        available_objects: pool.available_objects,
        allocated_objects: pool.allocated_objects.push(new_object),
        total_allocated: pool.total_allocated + 1,
        total_released: pool.total_released
      }
    }
  }
  
  let pool_release = fn(pool: MemoryPool, object: String) {
    let mut new_allocated = []
    let mut found = false
    
    for allocated_object in pool.allocated_objects {
      if allocated_object == object and not(found) {
        found = true
      } else {
        new_allocated = new_allocated.push(allocated_object)
      }
    }
    
    if found {
      {
        available_objects: pool.available_objects.push(object),
        allocated_objects: new_allocated,
        total_allocated: pool.total_allocated,
        total_released: pool.total_released + 1
      }
    } else {
      pool // Object not found in allocated list
    }
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(5)
  assert_eq(pool.available_objects.length(), 5)
  assert_eq(pool.allocated_objects.length(), 0)
  
  let pool_after_alloc1 = pool_allocate(pool)
  assert_eq(pool_after_alloc1.available_objects.length(), 4)
  assert_eq(pool_after_alloc1.allocated_objects.length(), 1)
  assert_eq(pool_after_alloc1.total_allocated, 1)
  
  let pool_after_alloc2 = pool_allocate(pool_after_alloc1)
  assert_eq(pool_after_alloc2.available_objects.length(), 3)
  assert_eq(pool_after_alloc2.allocated_objects.length(), 2)
  assert_eq(pool_after_alloc2.total_allocated, 2)
  
  // Release an object
  let pool_after_release = pool_release(pool_after_alloc2, pool_after_alloc2.allocated_objects[0])
  assert_eq(pool_after_release.available_objects.length(), 4)
  assert_eq(pool_after_release.allocated_objects.length(), 1)
  assert_eq(pool_after_release.total_released, 1)
  
  // Test cache with LRU eviction policy
  type CacheEntry = {
    key: String,
    value: String,
    access_count: Int,
    last_accessed: Int
  }
  
  type LRUCache = {
    entries: Array[CacheEntry],
    max_size: Int,
    current_time: Int
  }
  
  let create_lru_cache = fn(max_size: Int) {
    {
      entries: [],
      max_size: max_size,
      current_time: 0
    }
  }
  
  let cache_get = fn(cache: LRUCache, key: String) {
    let mut found = None
    let mut updated_entries = []
    
    for entry in cache.entries {
      if entry.key == key {
        found = Some(entry.value)
        updated_entries = updated_entries.push({
          key: entry.key,
          value: entry.value,
          access_count: entry.access_count + 1,
          last_accessed: cache.current_time
        })
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    (found, {
      entries: updated_entries,
      max_size: cache.max_size,
      current_time: cache.current_time + 1
    })
  }
  
  let cache_put = fn(cache: LRUCache, key: String, value: String) {
    let mut updated_entries = []
    let mut found = false
    
    // Update existing entry
    for entry in cache.entries {
      if entry.key == key {
        found = true
        updated_entries = updated_entries.push({
          key: entry.key,
          value: value,
          access_count: entry.access_count + 1,
          last_accessed: cache.current_time
        })
      } else {
        updated_entries = updated_entries.push(entry)
      }
    }
    
    if not(found) {
      // Add new entry
      if updated_entries.length() < cache.max_size {
        updated_entries = updated_entries.push({
          key: key,
          value: value,
          access_count: 1,
          last_accessed: cache.current_time
        })
      } else {
        // Evict LRU entry
        let mut lru_index = 0
        let mut lru_time = updated_entries[0].last_accessed
        
        for i in 1..updated_entries.length() {
          if updated_entries[i].last_accessed < lru_time {
            lru_time = updated_entries[i].last_accessed
            lru_index = i
          }
        }
        
        // Remove LRU entry and add new one
        let mut entries_without_lru = []
        for i in 0..updated_entries.length() {
          if i != lru_index {
            entries_without_lru = entries_without_lru.push(updated_entries[i])
          }
        }
        
        updated_entries = entries_without_lru.push({
          key: key,
          value: value,
          access_count: 1,
          last_accessed: cache.current_time
        })
      }
    }
    
    {
      entries: updated_entries,
      max_size: cache.max_size,
      current_time: cache.current_time + 1
    }
  }
  
  // Test LRU cache operations
  let cache = create_lru_cache(3)
  assert_eq(cache.entries.length(), 0)
  
  let cache_after_put1 = cache_put(cache, "key1", "value1")
  assert_eq(cache_after_put1.entries.length(), 1)
  
  let cache_after_put2 = cache_put(cache_after_put1, "key2", "value2")
  assert_eq(cache_after_put2.entries.length(), 2)
  
  let cache_after_put3 = cache_put(cache_after_put2, "key3", "value3")
  assert_eq(cache_after_put3.entries.length(), 3)
  
  // Access key1 to make it most recently used
  let (value1, cache_after_get1) = cache_get(cache_after_put3, "key1")
  assert_eq(value1, Some("value1"))
  
  // Add key4, should evict LRU (key2)
  let cache_after_put4 = cache_put(cache_after_get1, "key4", "value4")
  assert_eq(cache_after_put4.entries.length(), 3)
  
  // Check that key2 was evicted
  let (value2, _) = cache_get(cache_after_put4, "key2")
  assert_eq(value2, None)
  
  // Check that other keys are still there
  let (value1_check, _) = cache_get(cache_after_put4, "key1")
  assert_eq(value1_check, Some("value1"))
  
  // Test batch processing optimization
  type BatchProcessor = {
    batch_size: Int,
    processing_function: (Array[String]) -> Array[String],
    queue: Array[String]
  }
  
  let create_batch_processor = fn(batch_size: Int, processor: (Array[String]) -> Array[String]) {
    {
      batch_size: batch_size,
      processing_function: processor,
      queue: []
    }
  }
  
  let batch_add = fn(processor: BatchProcessor, item: String) {
    let new_queue = processor.queue.push(item)
    
    if new_queue.length() >= processor.batch_size {
      let processed = processor.processing_function(new_queue)
      {
        batch_size: processor.batch_size,
        processing_function: processor.processing_function,
        queue: processed
      }
    } else {
      {
        batch_size: processor.batch_size,
        processing_function: processor.processing_function,
        queue: new_queue
      }
    }
  }
  
  let batch_flush = fn(processor: BatchProcessor) {
    if processor.queue.length() > 0 {
      let processed = processor.processing_function(processor.queue)
      {
        batch_size: processor.batch_size,
        processing_function: processor.processing_function,
        queue: processed
      }
    } else {
      processor
    }
  }
  
  // Test batch processing
  let uppercase_processor = fn(items: Array[String]) {
    items.map(fn(item) { item.to_uppercase() })
  }
  
  let batch_processor = create_batch_processor(3, uppercase_processor)
  
  let processor_after_add1 = batch_add(batch_processor, "item1")
  assert_eq(processor_after_add1.queue.length(), 1)
  
  let processor_after_add2 = batch_add(processor_after_add1, "item2")
  assert_eq(processor_after_add2.queue.length(), 2)
  
  let processor_after_add3 = batch_add(processor_after_add2, "item3")
  assert_eq(processor_after_add3.queue.length(), 3)
  
  // Should trigger batch processing
  let processor_after_add4 = batch_add(processor_after_add3, "item4")
  assert_eq(processor_after_add4.queue.length(), 4)
  
  // Check that items were processed
  assert_true(processor_after_add4.queue.contains("ITEM1"))
  assert_true(processor_after_add4.queue.contains("ITEM2"))
  assert_true(processor_after_add4.queue.contains("ITEM3"))
  assert_eq(processor_after_add4.queue[3], "item4") // Not yet processed
  
  // Flush remaining items
  let processor_after_flush = batch_flush(processor_after_add4)
  assert_eq(processor_after_flush.queue.length(), 4)
  assert_true(processor_after_flush.queue.contains("ITEM4"))
}

// Test 5: Cross-Service Communication and Consistency
test "cross-service communication and consistency" {
  // Define service communication structures
  type ServiceMessage = {
    id: String,
    source_service: String,
    target_service: String,
    message_type: String,
    payload: String,
    timestamp: Int,
    correlation_id: String
  }
  
  type ServiceRegistry = {
    services: Array[String],
    health_status: Array[(String, String)],
    message_routes: Array[(String, String)]
  }
  
  // Create service registry
  let create_service_registry = fn() {
    {
      services: ["auth-service", "user-service", "payment-service", "notification-service"],
      health_status: [
        ("auth-service", "healthy"),
        ("user-service", "healthy"),
        ("payment-service", "degraded"),
        ("notification-service", "healthy")
      ],
      message_routes: [
        ("auth-service", "user-service"),
        ("user-service", "payment-service"),
        ("payment-service", "notification-service"),
        ("auth-service", "payment-service")
      ]
    }
  }
  
  // Test service health checking
  let check_service_health = fn(registry: ServiceRegistry, service_name: String) {
    let mut status = "unknown"
    
    for (service, health) in registry.health_status {
      if service == service_name {
        status = health
      }
    }
    
    status
  }
  
  let registry = create_service_registry()
  assert_eq(check_service_health(registry, "auth-service"), "healthy")
  assert_eq(check_service_health(registry, "payment-service"), "degraded")
  assert_eq(check_service_health(registry, "unknown-service"), "unknown")
  
  // Test message routing
  let find_route = fn(registry: ServiceRegistry, source: String, target: String) {
    let mut found = false
    
    for (src, tgt) in registry.message_routes {
      if src == source and tgt == target {
        found = true
      }
    }
    
    found
  }
  
  assert_true(find_route(registry, "auth-service", "user-service"))
  assert_true(find_route(registry, "user-service", "payment-service"))
  assert_false(find_route(registry, "notification-service", "auth-service"))
  
  // Test message creation and validation
  let create_message = fn(source: String, target: String, message_type: String, payload: String, correlation_id: String) {
    let timestamp = 1640995200 // Fixed timestamp for testing
    
    {
      id: "msg-" + timestamp.to_string() + "-" + source + "-" + target,
      source_service: source,
      target_service: target,
      message_type: message_type,
      payload: payload,
      timestamp: timestamp,
      correlation_id: correlation_id
    }
  }
  
  let validate_message = fn(message: ServiceMessage) {
    let is_valid = 
      message.source_service.length() > 0 and
      message.target_service.length() > 0 and
      message.message_type.length() > 0 and
      message.correlation_id.length() > 0 and
      message.timestamp > 0
    
    is_valid
  }
  
  let message = create_message("auth-service", "user-service", "user_request", "get_user_data", "corr-123")
  assert_true(validate_message(message))
  assert_eq(message.source_service, "auth-service")
  assert_eq(message.target_service, "user-service")
  assert_eq(message.message_type, "user_request")
  assert_eq(message.correlation_id, "corr-123")
  
  // Test distributed transaction coordination
  type TransactionStep = {
    step_id: String,
    service_name: String,
    operation: String,
    status: String, // "pending", "completed", "failed", "compensated"
    compensation: String
  }
  
  type DistributedTransaction = {
    transaction_id: String,
    correlation_id: String,
    steps: Array[TransactionStep],
    current_step: Int,
    status: String // "active", "completed", "failed", "compensating"
  }
  
  let create_transaction = fn(transaction_id: String, correlation_id: String) {
    {
      transaction_id: transaction_id,
      correlation_id: correlation_id,
      steps: [],
      current_step: 0,
      status: "active"
    }
  }
  
  let add_transaction_step = fn(transaction: DistributedTransaction, service_name: String, operation: String, compensation: String) {
    let step = {
      step_id: "step-" + (transaction.steps.length() + 1).to_string(),
      service_name: service_name,
      operation: operation,
      status: "pending",
      compensation: compensation
    }
    
    {
      transaction_id: transaction.transaction_id,
      correlation_id: transaction.correlation_id,
      steps: transaction.steps.push(step),
      current_step: transaction.current_step,
      status: transaction.status
    }
  }
  
  let execute_transaction_step = fn(transaction: DistributedTransaction, step_index: Int, success: Bool) {
    if step_index < transaction.steps.length() {
      let mut updated_steps = []
      
      for i in 0..transaction.steps.length() {
        let step = transaction.steps[i]
        if i == step_index {
          updated_steps = updated_steps.push({
            step_id: step.step_id,
            service_name: step.service_name,
            operation: step.operation,
            status: if success { "completed" } else { "failed" },
            compensation: step.compensation
          })
        } else {
          updated_steps = updated_steps.push(step)
        }
      }
      
      {
        transaction_id: transaction.transaction_id,
        correlation_id: transaction.correlation_id,
        steps: updated_steps,
        current_step: step_index + 1,
        status: if success { "active" } else { "failed" }
      }
    } else {
      transaction
    }
  }
  
  // Test distributed transaction execution
  let transaction = create_transaction("txn-123", "corr-456")
  
  let txn_with_steps = transaction
    |> add_transaction_step("auth-service", "authenticate_user", "revoke_token")
    |> add_transaction_step("user-service", "get_user_profile", "cache_invalidate")
    |> add_transaction_step("payment-service", "process_payment", "refund_payment")
  
  assert_eq(txn_with_steps.steps.length(), 3)
  assert_eq(txn_with_steps.current_step, 0)
  assert_eq(txn_with_steps.status, "active")
  
  // Execute first step successfully
  let txn_after_step1 = execute_transaction_step(txn_with_steps, 0, true)
  assert_eq(txn_after_step1.steps[0].status, "completed")
  assert_eq(txn_after_step1.current_step, 1)
  assert_eq(txn_after_step1.status, "active")
  
  // Execute second step successfully
  let txn_after_step2 = execute_transaction_step(txn_after_step1, 1, true)
  assert_eq(txn_after_step2.steps[1].status, "completed")
  assert_eq(txn_after_step2.current_step, 2)
  assert_eq(txn_after_step2.status, "active")
  
  // Execute third step with failure
  let txn_after_step3 = execute_transaction_step(txn_after_step2, 2, false)
  assert_eq(txn_after_step3.steps[2].status, "failed")
  assert_eq(txn_after_step3.current_step, 3)
  assert_eq(txn_after_step3.status, "failed")
  
  // Test compensation logic
  let compensate_transaction = fn(transaction: DistributedTransaction) {
    let mut compensated_steps = []
    
    // Compensate in reverse order
    for i in (0..transaction.steps.length()).reverse() {
      let step = transaction.steps[i]
      if step.status == "completed" {
        compensated_steps = compensated_steps.push({
          step_id: step.step_id,
          service_name: step.service_name,
          operation: step.operation,
          status: "compensated",
          compensation: step.compensation
        })
      } else {
        compensated_steps = compensated_steps.push(step)
      }
    }
    
    {
      transaction_id: transaction.transaction_id,
      correlation_id: transaction.correlation_id,
      steps: compensated_steps,
      current_step: transaction.current_step,
      status: "compensating"
    }
  }
  
  let compensated_txn = compensate_transaction(txn_after_step3)
  assert_eq(compensated_txn.status, "compensating")
  assert_eq(compensated_txn.steps[1].status, "compensated") // user-service step
  assert_eq(compensated_txn.steps[0].status, "compensated") // auth-service step
  assert_eq(compensated_txn.steps[2].status, "failed") // payment-service step remains failed
  
  // Test message ordering and delivery guarantees
  type MessageQueue = {
    pending_messages: Array[ServiceMessage],
    delivered_messages: Array[ServiceMessage],
    failed_messages: Array[ServiceMessage]
  }
  
  let create_message_queue = fn() {
    {
      pending_messages: [],
      delivered_messages: [],
      failed_messages: []
    }
  }
  
  let enqueue_message = fn(queue: MessageQueue, message: ServiceMessage) {
    {
      pending_messages: queue.pending_messages.push(message),
      delivered_messages: queue.delivered_messages,
      failed_messages: queue.failed_messages
    }
  }
  
  let process_message = fn(queue: MessageQueue, message_index: Int, success: Bool) {
    if message_index < queue.pending_messages.length() {
      let message = queue.pending_messages[message_index]
      let mut new_pending = []
      
      for i in 0..queue.pending_messages.length() {
        if i != message_index {
          new_pending = new_pending.push(queue.pending_messages[i])
        }
      }
      
      if success {
        {
          pending_messages: new_pending,
          delivered_messages: queue.delivered_messages.push(message),
          failed_messages: queue.failed_messages
        }
      } else {
        {
          pending_messages: new_pending,
          delivered_messages: queue.delivered_messages,
          failed_messages: queue.failed_messages.push(message)
        }
      }
    } else {
      queue
    }
  }
  
  // Test message queue operations
  let queue = create_message_queue()
  
  let msg1 = create_message("service-a", "service-b", "request", "payload1", "corr1")
  let msg2 = create_message("service-a", "service-b", "request", "payload2", "corr2")
  let msg3 = create_message("service-a", "service-b", "request", "payload3", "corr3")
  
  let queue_with_messages = queue
    |> enqueue_message(msg1)
    |> enqueue_message(msg2)
    |> enqueue_message(msg3)
  
  assert_eq(queue_with_messages.pending_messages.length(), 3)
  
  // Process messages in order
  let queue_after_first = process_message(queue_with_messages, 0, true)
  assert_eq(queue_after_first.pending_messages.length(), 2)
  assert_eq(queue_after_first.delivered_messages.length(), 1)
  assert_eq(queue_after_first.failed_messages.length(), 0)
  
  let queue_after_second = process_message(queue_after_first, 0, true)
  assert_eq(queue_after_second.pending_messages.length(), 1)
  assert_eq(queue_after_second.delivered_messages.length(), 2)
  
  let queue_after_third = process_message(queue_after_second, 0, false)
  assert_eq(queue_after_third.pending_messages.length(), 0)
  assert_eq(queue_after_third.delivered_messages.length(), 2)
  assert_eq(queue_after_third.failed_messages.length(), 1)
}

// Test 6: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Define serialization formats
  type SerializationFormat = {
    name: String,
    version: String,
    content_type: String
  }
  
  type SerializedData = {
    format: SerializationFormat,
    data: String,
    checksum: String,
    compressed: Bool
  }
  
  // Test JSON-like serialization
  let serialize_to_json = fn(data: (String, String)) {
    let (key, value) = data
    "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
  }
  
  let deserialize_from_json = fn(json_string: String) {
    // Simplified JSON parsing for testing
    if json_string.starts_with("{\"key\":\"") and json_string.contains("\",\"value\":\"") {
      let key_start = 8 // "{\"key\":\"".length()
      let key_end = json_string.index_of("\",\"value\":\"")
      let key = json_string.substring(key_start, key_end - key_start)
      
      let value_start = key_end + 11 // "\",\"value\":\"".length()
      let value_end = json_string.index_of("\"}", value_start)
      let value = json_string.substring(value_start, value_end - value_start)
      
      Some((key, value))
    } else {
      None
    }
  }
  
  // Test JSON serialization
  let test_data = ("user_id", "12345")
  let json_data = serialize_to_json(test_data)
  assert_eq(json_data, "{\"key\":\"user_id\",\"value\":\"12345\"}")
  
  let deserialized_data = deserialize_from_json(json_data)
  assert_eq(deserialized_data, Some(("user_id", "12345")))
  
  // Test invalid JSON
  let invalid_json = "{\"invalid\":json}"
  let invalid_result = deserialize_from_json(invalid_json)
  assert_eq(invalid_result, None)
  
  // Test binary-like serialization
  let serialize_to_binary = fn(data: Array[Int]) {
    let mut result = ""
    
    for value in data {
      // Convert each integer to a 4-byte hex representation
      let hex = value.to_hex()
      let padded_hex = if hex.length() < 8 {
        "0".repeat(8 - hex.length()) + hex
      } else {
        hex
      }
      result = result + padded_hex
    }
    
    result
  }
  
  let deserialize_from_binary = fn(binary_string: String) {
    if binary_string.length() % 8 == 0 {
      let mut result = []
      
      for i in 0..(binary_string.length() / 8) {
        let start = i * 8
        let hex_value = binary_string.substring(start, 8)
        
        // Convert hex to int (simplified)
        let mut int_value = 0
        for j in 0..8 {
          let char = hex_value[j]
          let digit = if char >= '0' and char <= '9' {
            char.to_int() - '0'.to_int()
          } else if char >= 'a' and char <= 'f' {
            10 + (char.to_int() - 'a'.to_int())
          } else if char >= 'A' and char <= 'F' {
            10 + (char.to_int() - 'A'.to_int())
          } else {
            0
          }
          
          int_value = int_value * 16 + digit
        }
        
        result = result.push(int_value)
      }
      
      Some(result)
    } else {
      None
    }
  }
  
  // Test binary serialization
  let int_data = [123, 456, 789]
  let binary_data = serialize_to_binary(int_data)
  
  let deserialized_ints = deserialize_from_binary(binary_data)
  assert_eq(deserialized_ints, Some([123, 456, 789]))
  
  // Test invalid binary
  let invalid_binary = "123" // Not multiple of 8 characters
  let invalid_binary_result = deserialize_from_binary(invalid_binary)
  assert_eq(invalid_binary_result, None)
  
  // Test compression simulation
  let compress_data = fn(data: String) {
    // Simple compression simulation - replace repeated characters
    let mut result = ""
    let mut i = 0
    
    while i < data.length() {
      let char = data[i]
      let mut count = 1
      
      while i + count < data.length() and data[i + count] == char {
        count = count + 1
      }
      
      if count > 3 {
        result = result + "[" + char.to_string() + "x" + count.to_string() + "]"
      } else {
        for j in 0..count {
          result = result + char.to_string()
        }
      }
      
      i = i + count
    }
    
    result
  }
  
  let decompress_data = fn(compressed: String) {
    let mut result = ""
    let mut i = 0
    
    while i < compressed.length() {
      if compressed[i] == '[' {
        let char = compressed[i + 1]
        let mut j = i + 3 // Skip "[charx"
        let mut count_str = ""
        
        while j < compressed.length() and compressed[j] != ']' {
          count_str = count_str + compressed[j].to_string()
          j = j + 1
        }
        
        let count = match count_str.to_int() {
          Some(n) => n
          None => 1
        }
        
        for k in 0..count {
          result = result + char.to_string()
        }
        
        i = j + 1 // Skip "]"
      } else {
        result = result + compressed[i].to_string()
        i = i + 1
      }
    }
    
    result
  }
  
  // Test compression
  let original_data = "aaaaabbbcccaaaaa"
  let compressed_data = compress_data(original_data)
  assert_eq(compressed_data, "[ax5]bbb[ccc][ax5]")
  
  let decompressed_data = decompress_data(compressed_data)
  assert_eq(decompressed_data, original_data)
  
  // Test checksum calculation
  let calculate_checksum = fn(data: String) {
    let mut sum = 0
    
    for i in 0..data.length() {
      sum = sum + data[i].to_int()
    }
    
    sum % 10000 // Simple checksum
  }
  
  let verify_checksum = fn(data: String, expected_checksum: String) {
    let calculated = calculate_checksum(data)
    calculated.to_string() == expected_checksum
  }
  
  // Test checksum
  let test_string = "test data for checksum"
  let checksum = calculate_checksum(test_string).to_string()
  assert_true(verify_checksum(test_string, checksum))
  
  // Test with corrupted data
  let corrupted_string = "test data for checksums"
  assert_false(verify_checksum(corrupted_string, checksum))
  
  // Test complete serialization pipeline
  let serialize_data_pipeline = fn(data: (String, String), format: String, compress: Bool) {
    let serialized = match format {
      "json" => serialize_to_json(data)
      "binary" => serialize_to_binary([123, 456]) // Simplified for binary
      _ => "unknown format"
    }
    
    let processed = if compress {
      compress_data(serialized)
    } else {
      serialized
    }
    
    let checksum = calculate_checksum(processed).to_string()
    
    {
      format: { name: format, version: "1.0", content_type: "application/" + format },
      data: processed,
      checksum: checksum,
      compressed: compress
    }
  }
  
  let deserialize_data_pipeline = fn(serialized: SerializedData) {
    if verify_checksum(serialized.data, serialized.checksum) {
      let decompressed = if serialized.compressed {
        decompress_data(serialized.data)
      } else {
        serialized.data
      }
      
      match serialized.format.name {
        "json" => deserialize_from_json(decompressed)
        "binary" => deserialize_from_binary(decompressed)
        _ => None
      }
    } else {
      None
    }
  }
  
  // Test complete pipeline
  let test_pipeline_data = ("session_id", "abcdef123456")
  let serialized_pipeline = serialize_data_pipeline(test_pipeline_data, "json", true)
  assert_eq(serialized_pipeline.format.name, "json")
  assert_eq(serialized_pipeline.compressed, true)
  
  let deserialized_pipeline = deserialize_data_pipeline(serialized_pipeline)
  assert_eq(deserialized_pipeline, Some(("session_id", "abcdef123456")))
  
  // Test with corrupted checksum
  let corrupted_serialized = {
    format: serialized_pipeline.format,
    data: serialized_pipeline.data,
    checksum: "9999",
    compressed: serialized_pipeline.compressed
  }
  
  let corrupted_result = deserialize_data_pipeline(corrupted_serialized)
  assert_eq(corrupted_result, None)
}

// Test 7: Internationalization and Localization Support
test "internationalization and localization support" {
  // Define localization structures
  type LocalizationEntry = {
    key: String,
    value: String,
    locale: String,
    context: Option[String]
  }
  
  type ResourceBundle = {
    locale: String,
    entries: Array[LocalizationEntry]
  }
  
  // Create resource bundles for different locales
  let create_english_bundle = fn() {
    {
      locale: "en-US",
      entries: [
        { key: "welcome", value: "Welcome", locale: "en-US", context: None },
        { key: "goodbye", value: "Goodbye", locale: "en-US", context: None },
        { key: "error_occurred", value: "An error occurred: {0}", locale: "en-US", context: None },
        { key: "items_count", value: "{0} items", locale: "en-US", context: None },
        { key: "service_status", value: "Service is {0}", locale: "en-US", context: None }
      ]
    }
  }
  
  let create_chinese_bundle = fn() {
    {
      locale: "zh-CN",
      entries: [
        { key: "welcome", value: "欢迎", locale: "zh-CN", context: None },
        { key: "goodbye", value: "再见", locale: "zh-CN", context: None },
        { key: "error_occurred", value: "发生错误：{0}", locale: "zh-CN", context: None },
        { key: "items_count", value: "{0} 项", locale: "zh-CN", context: None },
        { key: "service_status", value: "服务状态：{0}", locale: "zh-CN", context: None }
      ]
    }
  }
  
  let create_spanish_bundle = fn() {
    {
      locale: "es-ES",
      entries: [
        { key: "welcome", value: "Bienvenido", locale: "es-ES", context: None },
        { key: "goodbye", value: "Adiós", locale: "es-ES", context: None },
        { key: "error_occurred", value: "Ocurrió un error: {0}", locale: "es-ES", context: None },
        { key: "items_count", value: "{0} elementos", locale: "es-ES", context: None },
        { key: "service_status", value: "El servicio está {0}", locale: "es-ES", context: None }
      ]
    }
  }
  
  // Test message formatting with parameters
  let format_message = fn(template: String, params: Array[String]) {
    let mut result = template
    
    for i in 0..params.length() {
      let placeholder = "{" + i.to_string() + "}"
      result = result.replace(placeholder, params[i])
    }
    
    result
  }
  
  // Test message lookup in resource bundle
  let get_message = fn(bundle: ResourceBundle, key: String) {
    let mut found = None
    
    for entry in bundle.entries {
      if entry.key == key {
        found = Some(entry.value)
      }
    }
    
    found
  }
  
  // Test basic message lookup
  let english_bundle = create_english_bundle()
  let chinese_bundle = create_chinese_bundle()
  let spanish_bundle = create_spanish_bundle()
  
  assert_eq(get_message(english_bundle, "welcome"), Some("Welcome"))
  assert_eq(get_message(chinese_bundle, "welcome"), Some("欢迎"))
  assert_eq(get_message(spanish_bundle, "welcome"), Some("Bienvenido"))
  
  // Test message formatting
  let english_error = get_message(english_bundle, "error_occurred")
  assert_eq(english_error, Some("An error occurred: {0}"))
  
  let formatted_error = match english_error {
    Some(template) => format_message(template, ["Network timeout"])
    None => ""
  }
  assert_eq(formatted_error, "An error occurred: Network timeout")
  
  let chinese_items = get_message(chinese_bundle, "items_count")
  let formatted_items = match chinese_items {
    Some(template) => format_message(template, ["5"])
    None => ""
  }
  assert_eq(formatted_items, "5 项")
  
  // Test locale negotiation
  type LocaleMatcher = {
    preferred_locales: Array[String],
    supported_locales: Array[String]
  }
  
  let create_locale_matcher = fn(preferred: Array[String], supported: Array[String]) {
    {
      preferred_locales: preferred,
      supported_locales: supported
    }
  }
  
  let match_locale = fn(matcher: LocaleMatcher) {
    for preferred in matcher.preferred_locales {
      for supported in matcher.supported_locales {
        if preferred == supported {
          return Some(supported)
        }
        
        // Check language-only match (e.g., "en" matches "en-US")
        let preferred_lang = preferred.split("-")[0]
        let supported_lang = supported.split("-")[0]
        
        if preferred_lang == supported_lang {
          return Some(supported)
        }
      }
    }
    
    None
  }
  
  // Test locale matching
  let supported_locales = ["en-US", "zh-CN", "es-ES", "fr-FR"]
  
  // Exact match
  let matcher1 = create_locale_matcher(["zh-CN", "en-US"], supported_locales)
  assert_eq(match_locale(matcher1), Some("zh-CN"))
  
  // Language-only match
  let matcher2 = create_locale_matcher(["en", "fr"], supported_locales)
  assert_eq(match_locale(matcher2), Some("en-US"))
  
  // No match
  let matcher3 = create_locale_matcher(["de", "it"], supported_locales)
  assert_eq(match_locale(matcher3), None)
  
  // Test pluralization rules
  type PluralRule = {
    locale: String,
    rule: (Int) -> String // Returns plural category: "one", "other", etc.
  }
  
  let create_plural_rules = fn() {
    [
      {
        locale: "en-US",
        rule: fn(n) { if n == 1 { "one" } else { "other" } }
      },
      {
        locale: "zh-CN",
        rule: fn(_) { "other" } // Chinese doesn't have plural forms
      },
      {
        locale: "es-ES",
        rule: fn(n) { if n == 1 { "one" } else { "other" } }
      }
    ]
  }
  
  let get_plural_category = fn(rules: Array[PluralRule], locale: String, count: Int) {
    for rule in rules {
      if rule.locale == locale {
        return rule.rule(count)
      }
    }
    "other" // Default
  }
  
  // Test pluralization
  let plural_rules = create_plural_rules()
  
  assert_eq(get_plural_category(plural_rules, "en-US", 1), "one")
  assert_eq(get_plural_category(plural_rules, "en-US", 2), "other")
  assert_eq(get_plural_category(plural_rules, "zh-CN", 1), "other")
  assert_eq(get_plural_category(plural_rules, "zh-CN", 2), "other")
  assert_eq(get_plural_category(plural_rules, "es-ES", 1), "one")
  assert_eq(get_plural_category(plural_rules, "es-ES", 2), "other")
  
  // Test localized number formatting
  let format_number = fn(number: Float, locale: String) {
    match locale {
      "en-US" => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100).to_int().to_string()
        int_part + "." + decimal_part
      }
      "zh-CN" => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100).to_int().to_string()
        int_part + "." + decimal_part
      }
      "es-ES" => {
        let int_part = number.to_int().to_string()
        let decimal_part = ((number - number.to_int().to_float()) * 100).to_int().to_string()
        int_part + "," + decimal_part // Use comma as decimal separator
      }
      _ => number.to_string()
    }
  }
  
  // Test number formatting
  assert_eq(format_number(1234.56, "en-US"), "1234.56")
  assert_eq(format_number(1234.56, "zh-CN"), "1234.56")
  assert_eq(format_number(1234.56, "es-ES"), "1234,56")
  
  // Test date/time formatting
  let format_datetime = fn(timestamp: Int, locale: String) {
    // Simplified date formatting for testing
    let date = timestamp / 86400 // Days since epoch
    let time = timestamp % 86400 // Seconds in day
    let hours = time / 3600
    let minutes = (time % 3600) / 60
    
    match locale {
      "en-US" => {
        "Day " + date.to_string() + " at " + hours.to_string() + ":" + minutes.to_string()
      }
      "zh-CN" => {
        "第" + date.to_string() + "天 " + hours.to_string() + "时" + minutes.to_string() + "分"
      }
      "es-ES" => {
        "Día " + date.to_string() + " a las " + hours.to_string() + ":" + minutes.to_string()
      }
      _ => timestamp.to_string()
    }
  }
  
  // Test date/time formatting
  let test_timestamp = 1640995200 // 2022-01-01 00:00:00 UTC
  assert_eq(format_datetime(test_timestamp, "en-US"), "Day 18993 at 0:0")
  assert_eq(format_datetime(test_timestamp, "zh-CN"), "第18993天 0时0分")
  assert_eq(format_datetime(test_timestamp, "es-ES"), "Día 18993 a las 0:0")
  
  // Test right-to-left (RTL) language support
  let is_rtl_language = fn(locale: String) {
    match locale {
      "ar" | "he" | "fa" => true
      _ => false
    }
  }
  
  // Test RTL detection
  assert_false(is_rtl_language("en-US"))
  assert_false(is_rtl_language("zh-CN"))
  assert_false(is_rtl_language("es-ES"))
  assert_true(is_rtl_language("ar"))
  assert_true(is_rtl_language("he"))
  assert_true(is_rtl_language("fa"))
  
  // Test text direction handling
  let apply_text_direction = fn(text: String, locale: String) {
    if is_rtl_language(locale) {
      "\u202B" + text + "\u202C" // Wrap with RTL markers
    } else {
      text
    }
  }
  
  // Test text direction application
  let test_text = "Hello World"
  assert_eq(apply_text_direction(test_text, "en-US"), "Hello World")
  assert_eq(apply_text_direction(test_text, "ar"), "\u202BHello World\u202C")
}

// Test 8: Advanced Telemetry Analytics and Insights
test "advanced telemetry analytics and insights" {
  // Define analytics data structures
  type MetricEvent = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)],
    dimensions: Array[String]
  }
  
  type TimeSeriesData = {
    timestamps: Array[Int],
    values: Array[Float],
    interval: Int // Time interval between data points in seconds
  }
  
  type AnalyticsInsight = {
    insight_type: String,
    description: String,
    confidence: Float, // 0.0 to 1.0
    affected_metrics: Array[String],
    recommendation: Option[String]
  }
  
  // Create sample metric events
  let create_sample_metrics = fn() {
    let base_timestamp = 1640995200
    
    [
      { name: "cpu_usage", value: 45.2, timestamp: base_timestamp, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "cpu_usage", value: 52.1, timestamp: base_timestamp + 300, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "cpu_usage", value: 67.8, timestamp: base_timestamp + 600, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "cpu_usage", value: 78.9, timestamp: base_timestamp + 900, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "cpu_usage", value: 82.3, timestamp: base_timestamp + 1200, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      
      { name: "memory_usage", value: 1024.5, timestamp: base_timestamp, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "memory_usage", value: 1087.2, timestamp: base_timestamp + 300, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "memory_usage", value: 1156.8, timestamp: base_timestamp + 600, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "memory_usage", value: 1234.1, timestamp: base_timestamp + 900, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "memory_usage", value: 1298.7, timestamp: base_timestamp + 1200, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      
      { name: "response_time", value: 120.5, timestamp: base_timestamp, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "response_time", value: 145.2, timestamp: base_timestamp + 300, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "response_time", value: 178.9, timestamp: base_timestamp + 600, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "response_time", value: 234.1, timestamp: base_timestamp + 900, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "response_time", value: 298.7, timestamp: base_timestamp + 1200, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      
      { name: "error_rate", value: 0.5, timestamp: base_timestamp, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "error_rate", value: 1.2, timestamp: base_timestamp + 300, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "error_rate", value: 2.8, timestamp: base_timestamp + 600, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "error_rate", value: 5.1, timestamp: base_timestamp + 900, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] },
      { name: "error_rate", value: 8.9, timestamp: base_timestamp + 1200, tags: [("service", "api"), ("env", "prod")], dimensions: ["service", "env"] }
    ]
  }
  
  // Test time series aggregation
  let aggregate_to_time_series = fn(events: Array[MetricEvent], metric_name: String, interval: Int) {
    let filtered_events = events.filter(fn(e) { e.name == metric_name })
    
    if filtered_events.length() == 0 {
      return {
        timestamps: [],
        values: [],
        interval: interval
      }
    }
    
    // Sort events by timestamp
    let sorted_events = filtered_events.sort(fn(a, b) { a.timestamp - b.timestamp })
    
    // Group by time intervals
    let mut timestamps = []
    let mut values = []
    let mut current_interval_start = sorted_events[0].timestamp
    
    let mut interval_events = []
    for event in sorted_events {
      if event.timestamp < current_interval_start + interval {
        interval_events = interval_events.push(event)
      } else {
        // Process current interval
        if interval_events.length() > 0 {
          let avg_value = interval_events.reduce(fn(acc, e) { acc + e.value }, 0.0) / interval_events.length().to_float()
          timestamps = timestamps.push(current_interval_start)
          values = values.push(avg_value)
        }
        
        // Start new interval
        current_interval_start = event.timestamp
        interval_events = [event]
      }
    }
    
    // Process last interval
    if interval_events.length() > 0 {
      let avg_value = interval_events.reduce(fn(acc, e) { acc + e.value }, 0.0) / interval_events.length().to_float()
      timestamps = timestamps.push(current_interval_start)
      values = values.push(avg_value)
    }
    
    {
      timestamps: timestamps,
      values: values,
      interval: interval
    }
  }
  
  // Test trend analysis
  let calculate_trend = fn(time_series: TimeSeriesData) {
    if time_series.values.length() < 2 {
      return { direction: "stable", slope: 0.0, confidence: 0.0 }
    }
    
    // Simple linear regression to calculate trend
    let n = time_series.values.length().to_float()
    let sum_x = (0..time_series.values.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
    let sum_y = time_series.values.reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_xy = time_series.values.enumerate().reduce(fn(acc, pair) { 
      let (i, v) = pair
      acc + i.to_float() * v 
    }, 0.0)
    let sum_x2 = (0..time_series.values.length()).reduce(fn(acc, i) { acc + i.to_float() * i.to_float() }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    let direction = if slope > 0.1 { 
      "increasing" 
    } else if slope < -0.1 { 
      "decreasing" 
    } else { 
      "stable" 
    }
    
    // Calculate R-squared as confidence measure
    let mean_y = sum_y / n
    let ss_tot = time_series.values.reduce(fn(acc, v) { acc + (v - mean_y) * (v - mean_y) }, 0.0)
    let y_pred = time_series.values.map(fn(v) { mean_y + slope * 0.5 }) // Simplified prediction
    let ss_res = time_series.values.enumerate().reduce(fn(acc, pair) {
      let (i, v) = pair
      acc + (v - y_pred[i]) * (v - y_pred[i])
    }, 0.0)
    
    let r_squared = if ss_tot > 0.0 { 1.0 - (ss_res / ss_tot) } else { 0.0 }
    let confidence = if r_squared < 0.0 { 0.0 } else if r_squared > 1.0 { 1.0 } else { r_squared }
    
    { direction: direction, slope: slope, confidence: confidence }
  }
  
  // Test anomaly detection
  let detect_anomalies = fn(time_series: TimeSeriesData, threshold: Float) {
    if time_series.values.length() < 3 {
      return []
    }
    
    // Calculate moving average and standard deviation
    let window_size = 3
    let mut anomalies = []
    
    for i in window_size..time_series.values.length() {
      let window_start = i - window_size
      let window_values = time_series.values.slice(window_start, window_size)
      
      let mean = window_values.reduce(fn(acc, v) { acc + v }, 0.0) / window_size.to_float()
      let variance = window_values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / window_size.to_float()
      let std_dev = if variance >= 0.0 { variance.sqrt() } else { 0.0 }
      
      let current_value = time_series.values[i]
      let z_score = if std_dev > 0.0 { (current_value - mean) / std_dev } else { 0.0 }
      
      if z_score.abs() > threshold {
        anomalies = anomalies.push({
          timestamp: time_series.timestamps[i],
          value: current_value,
          expected_range: (mean - threshold * std_dev, mean + threshold * std_dev),
          z_score: z_score
        })
      }
    }
    
    anomalies
  }
  
  // Test correlation analysis
  let calculate_correlation = fn(series1: TimeSeriesData, series2: TimeSeriesData) {
    if series1.values.length() != series2.values.length() or series1.values.length() < 2 {
      return { correlation: 0.0, significance: "insufficient_data" }
    }
    
    let n = series1.values.length().to_float()
    let mean1 = series1.values.reduce(fn(acc, v) { acc + v }, 0.0) / n
    let mean2 = series2.values.reduce(fn(acc, v) { acc + v }, 0.0) / n
    
    let sum_xy = series1.values.enumerate().reduce(fn(acc, pair) {
      let (i, v1) = pair
      let v2 = series2.values[i]
      acc + (v1 - mean1) * (v2 - mean2)
    }, 0.0)
    
    let sum_xx = series1.values.reduce(fn(acc, v) { acc + (v - mean1) * (v - mean1) }, 0.0)
    let sum_yy = series2.values.reduce(fn(acc, v) { acc + (v - mean2) * (v - mean2) }, 0.0)
    
    if sum_xx == 0.0 or sum_yy == 0.0 {
      return { correlation: 0.0, significance: "no_variance" }
    }
    
    let correlation = sum_xy / (sum_xx.sqrt() * sum_yy.sqrt())
    
    let significance = if correlation.abs() > 0.7 { 
      "strong" 
    } else if correlation.abs() > 0.3 { 
      "moderate" 
    } else if correlation.abs() > 0.1 { 
      "weak" 
    } else { 
      "negligible" 
    }
    
    { correlation: correlation, significance: significance }
  }
  
  // Test generating insights
  let generate_insights = fn(metrics: Array[MetricEvent]) {
    let mut insights = []
    
    // Analyze each metric
    let metric_names = ["cpu_usage", "memory_usage", "response_time", "error_rate"]
    
    for metric_name in metric_names {
      let time_series = aggregate_to_time_series(metrics, metric_name, 300) // 5-minute intervals
      
      if time_series.values.length() >= 2 {
        let trend = calculate_trend(time_series)
        let anomalies = detect_anomalies(time_series, 2.0) // 2 standard deviations
        
        // Generate trend insight
        if trend.confidence > 0.5 and trend.direction != "stable" {
          let trend_description = metric_name + " is showing a " + trend.direction + " trend"
          let trend_recommendation = match metric_name {
            "cpu_usage" if trend.direction == "increasing" => Some("Consider scaling up or optimizing CPU usage")
            "memory_usage" if trend.direction == "increasing" => Some("Monitor memory usage and consider optimization")
            "response_time" if trend.direction == "increasing" => Some("Investigate performance bottlenecks")
            "error_rate" if trend.direction == "increasing" => Some("Urgent: Investigate and fix root cause of errors")
            _ => None
          }
          
          insights = insights.push({
            insight_type: "trend_analysis",
            description: trend_description,
            confidence: trend.confidence,
            affected_metrics: [metric_name],
            recommendation: trend_recommendation
          })
        }
        
        // Generate anomaly insight
        if anomalies.length() > 0 {
          let anomaly_description = metric_name + " has " + anomalies.length().to_string() + " anomalous data points"
          let anomaly_recommendation = match metric_name {
            "error_rate" => Some("Investigate the cause of these error rate anomalies")
            "response_time" => Some("Investigate performance issues during these time periods")
            "cpu_usage" | "memory_usage" => Some("Monitor resource usage during these anomalies")
            _ => Some("Review these anomalies for potential issues")
          }
          
          insights = insights.push({
            insight_type: "anomaly_detection",
            description: anomaly_description,
            confidence: 0.8,
            affected_metrics: [metric_name],
            recommendation: anomaly_recommendation
          })
        }
      }
    }
    
    // Analyze correlations between metrics
    let cpu_series = aggregate_to_time_series(metrics, "cpu_usage", 300)
    let response_series = aggregate_to_time_series(metrics, "response_time", 300)
    let error_series = aggregate_to_time_series(metrics, "error_rate", 300)
    
    if cpu_series.values.length() == response_series.values.length() and cpu_series.values.length() >= 2 {
      let cpu_response_correlation = calculate_correlation(cpu_series, response_series)
      
      if cpu_response_correlation.significance == "strong" and cpu_response_correlation.correlation > 0.5 {
        insights = insights.push({
          insight_type: "correlation_analysis",
          description: "Strong positive correlation between CPU usage and response time",
          confidence: 0.7,
          affected_metrics: ["cpu_usage", "response_time"],
          recommendation: Some("Consider optimizing CPU usage to improve response times")
        })
      }
    }
    
    if response_series.values.length() == error_series.values.length() and response_series.values.length() >= 2 {
      let response_error_correlation = calculate_correlation(response_series, error_series)
      
      if response_error_correlation.significance == "strong" and response_error_correlation.correlation > 0.5 {
        insights = insights.push({
          insight_type: "correlation_analysis",
          description: "Strong positive correlation between response time and error rate",
          confidence: 0.8,
          affected_metrics: ["response_time", "error_rate"],
          recommendation: Some("High response times may be contributing to increased error rates")
        })
      }
    }
    
    insights
  }
  
  // Run analytics tests
  let metrics = create_sample_metrics()
  
  // Test time series aggregation
  let cpu_time_series = aggregate_to_time_series(metrics, "cpu_usage", 300)
  assert_eq(cpu_time_series.values.length(), 5)
  assert_eq(cpu_time_series.timestamps.length(), 5)
  assert_eq(cpu_time_series.interval, 300)
  
  // Test trend analysis
  let cpu_trend = calculate_trend(cpu_time_series)
  assert_eq(cpu_trend.direction, "increasing")
  assert_true(cpu_trend.slope > 0.0)
  assert_true(cpu_trend.confidence > 0.5)
  
  // Test anomaly detection
  let error_time_series = aggregate_to_time_series(metrics, "error_rate", 300)
  let error_anomalies = detect_anomalies(error_time_series, 1.5) // Lower threshold for testing
  
  // Test correlation analysis
  let response_time_series = aggregate_to_time_series(metrics, "response_time", 300)
  let cpu_response_correlation = calculate_correlation(cpu_time_series, response_time_series)
  assert_eq(cpu_response_correlation.significance, "strong")
  assert_true(cpu_response_correlation.correlation > 0.5)
  
  // Test insights generation
  let insights = generate_insights(metrics)
  assert_true(insights.length() > 0)
  
  // Verify specific insights
  let trend_insights = insights.filter(fn(i) { i.insight_type == "trend_analysis" })
  assert_true(trend_insights.length() > 0)
  
  let correlation_insights = insights.filter(fn(i) { i.insight_type == "correlation_analysis" })
  assert_true(correlation_insights.length() > 0)
  
  // Test that insights have proper structure
  for insight in insights {
    assert_true(insight.description.length() > 0)
    assert_true(insight.confidence >= 0.0 and insight.confidence <= 1.0)
    assert_true(insight.affected_metrics.length() > 0)
  }
}