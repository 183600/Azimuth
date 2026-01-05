// Azimuth High-Quality MoonBit Test Suite
// This file contains high-quality test cases focusing on advanced features and real-world scenarios

// Test 1: Advanced Data Structures and Algorithms
test "advanced data structures and algorithms for telemetry processing" {
  // Implement a priority queue for telemetry data processing
  type PriorityQueue[T] = {
    elements: Array[T],
    priorities: Array[Int],
    size: Int
  }
  
  let create_priority_queue = fn() {
    {
      elements: [],
      priorities: [],
      size: 0
    }
  }
  
  let enqueue = fn(queue: PriorityQueue[T], element: T, priority: Int) {
    let new_elements = queue.elements.push(element)
    let new_priorities = queue.priorities.push(priority)
    
    // Simple insertion sort by priority
    let mut i = new_priorities.length() - 1
    while i > 0 and new_priorities[i-1] < new_priorities[i] {
      // Swap priorities
      let temp_prio = new_priorities[i]
      new_priorities[i] = new_priorities[i-1]
      new_priorities[i-1] = temp_prio
      
      // Swap elements
      let temp_elem = new_elements[i]
      new_elements[i] = new_elements[i-1]
      new_elements[i-1] = temp_elem
      
      i = i - 1
    }
    
    {
      elements: new_elements,
      priorities: new_priorities,
      size: queue.size + 1
    }
  }
  
  let dequeue = fn(queue: PriorityQueue[T]) {
    if queue.size == 0 {
      (None, queue)
    } else {
      let element = queue.elements[0]
      let new_elements = queue.elements.slice(1, queue.elements.length())
      let new_priorities = queue.priorities.slice(1, queue.priorities.length())
      
      (Some(element), {
        elements: new_elements,
        priorities: new_priorities,
        size: queue.size - 1
      })
    }
  }
  
  // Test priority queue operations
  let mut queue = create_priority_queue()
  
  // Enqueue telemetry data with different priorities
  queue = enqueue(queue, "critical_error", 10)
  queue = enqueue(queue, "warning", 5)
  queue = enqueue(queue, "info", 1)
  queue = enqueue(queue, "urgent_alert", 8)
  
  assert_eq(queue.size, 4)
  
  // Dequeue should return elements in priority order
  let (result1, queue1) = dequeue(queue)
  match result1 {
    Some(element) => assert_eq(element, "critical_error")
    None => assert_true(false)
  }
  
  let (result2, queue2) = dequeue(queue1)
  match result2 {
    Some(element) => assert_eq(element, "urgent_alert")
    None => assert_true(false)
  }
  
  let (result3, queue3) = dequeue(queue2)
  match result3 {
    Some(element) => assert_eq(element, "warning")
    None => assert_true(false)
  }
  
  let (result4, queue4) = dequeue(queue3)
  match result4 {
    Some(element) => assert_eq(element, "info")
    None => assert_true(false)
  }
  
  assert_eq(queue4.size, 0)
  
  // Test empty queue
  let (result5, _) = dequeue(queue4)
  match result5 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Advanced Telemetry Data Processing
test "advanced telemetry data processing and transformation" {
  // Define telemetry event structure
  type TelemetryEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    service_name: String,
    attributes: Array[(String, String)],
    metrics: Array[(String, Float)]
  }
  
  // Define event processor
  type EventProcessor = {
    filter_rules: Array[(String, String)],
    transformers: Array[(TelemetryEvent) -> TelemetryEvent],
    aggregators: Array[(Array[TelemetryEvent]) -> Float]
  }
  
  // Create test events
  let events = [
    {
      event_id: "evt-001",
      timestamp: 1640995200,
      event_type: "request",
      service_name: "payment-service",
      attributes: [("method", "POST"), ("status", "200")],
      metrics: [("duration", 120.5), ("size", 1024.0)]
    },
    {
      event_id: "evt-002",
      timestamp: 1640995260,
      event_type: "request",
      service_name: "payment-service",
      attributes: [("method", "GET"), ("status", "404")],
      metrics: [("duration", 45.2), ("size", 256.0)]
    },
    {
      event_id: "evt-003",
      timestamp: 1640995320,
      event_type: "error",
      service_name: "auth-service",
      attributes: [("error_type", "timeout"), ("retry_count", "3")],
      metrics: [("duration", 5000.0), ("size", 128.0)]
    }
  ]
  
  // Create filter function
  let filter_events = fn(events: Array[TelemetryEvent], rules: Array[(String, String)]) {
    let mut filtered = []
    for event in events {
      let mut matches_all = true
      for (key, value) in rules {
        let mut found_match = false
        for (attr_key, attr_value) in event.attributes {
          if attr_key == key and attr_value == value {
            found_match = true
          }
        }
        if not(found_match) {
          matches_all = false
        }
      }
      if matches_all {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  // Create transformer function
  let transform_event = fn(event: TelemetryEvent) {
    // Add computed metrics
    let duration_metric = event.metrics.find(fn(m) { m.0 == "duration" })
    let size_metric = event.metrics.find(fn(m) { m.0 == "size" })
    
    let throughput = match (duration_metric, size_metric) {
      (Some((_, duration)), Some((_, size))) => {
        if duration > 0.0 { size / duration } else { 0.0 }
      }
      _ => 0.0
    }
    
    let new_metrics = event.metrics.push(("throughput", throughput))
    
    // Add service tier based on service name
    let service_tier = if event.service_name.contains("payment") {
      "critical"
    } else if event.service_name.contains("auth") {
      "high"
    } else {
      "standard"
    }
    
    let new_attributes = event.attributes.push(("service_tier", service_tier))
    
    {
      event |
      attributes: new_attributes,
      metrics: new_metrics
    }
  }
  
  // Create aggregator function
  let aggregate_average_duration = fn(events: Array[TelemetryEvent]) {
    if events.length() == 0 {
      0.0
    } else {
      let mut total = 0.0
      let mut count = 0
      
      for event in events {
        match event.metrics.find(fn(m) { m.0 == "duration" }) {
          Some((_, duration)) => {
            total = total + duration
            count = count + 1
          }
          None => ()
        }
      }
      
      if count > 0 { total / (count as Float) } else { 0.0 }
    }
  }
  
  // Test filtering
  let payment_service_events = filter_events(events, [("service_name", "payment-service")])
  assert_eq(payment_service_events.length(), 2)
  
  let successful_requests = filter_events(events, [("status", "200")])
  assert_eq(successful_requests.length(), 1)
  
  // Test transformation
  let transformed_events = events.map(transform_event)
  
  // Check that throughput was added
  let first_event = transformed_events[0]
  match first_event.metrics.find(fn(m) { m.0 == "throughput" }) {
    Some((_, throughput)) => {
      assert_true(throughput > 0.0)
      assert_eq(throughput.round(), 8.0)  // 1024.0 / 120.5 ≈ 8.5
    }
    None => assert_true(false)
  }
  
  // Check that service tier was added
  match first_event.attributes.find(fn(a) { a.0 == "service_tier" }) {
    Some((_, tier)) => assert_eq(tier, "critical")
    None => assert_true(false)
  }
  
  // Test aggregation
  let avg_duration = aggregate_average_duration(events)
  let expected_avg = (120.5 + 45.2 + 5000.0) / 3.0
  assert_true((avg_duration - expected_avg).abs() < 0.01)
}

// Test 3: Concurrent Processing and Synchronization
test "concurrent processing and synchronization patterns" {
  // Simulate concurrent telemetry processing
  type ConcurrentProcessor = {
    worker_count: Int,
    task_queue: Array[String],
    completed_tasks: Array[String],
    in_progress: Array[String]
  }
  
  let create_processor = fn(workers: Int) {
    {
      worker_count: workers,
      task_queue: [],
      completed_tasks: [],
      in_progress: []
    }
  }
  
  let add_tasks = fn(processor: ConcurrentProcessor, tasks: Array[String]) {
    {
      processor |
      task_queue: processor.task_queue + tasks
    }
  }
  
  let simulate_concurrent_processing = fn(processor: ConcurrentProcessor) {
    let mut updated_processor = processor
    let mut batch_size = updated_processor.worker_count
    
    // Process tasks in batches
    while updated_processor.task_queue.length() > 0 {
      // Determine how many tasks to process in this batch
      let current_batch_size = if updated_processor.task_queue.length() < batch_size {
        updated_processor.task_queue.length()
      } else {
        batch_size
      }
      
      // Move tasks from queue to in_progress
      let mut new_in_progress = []
      let mut remaining_queue = []
      
      for i in 0..updated_processor.task_queue.length() {
        if i < current_batch_size {
          new_in_progress = new_in_progress.push(updated_processor.task_queue[i])
        } else {
          remaining_queue = remaining_queue.push(updated_processor.task_queue[i])
        }
      }
      
      // Simulate processing completion
      let mut new_completed = []
      for task in new_in_progress {
        // Simulate task processing by adding a suffix
        let processed_task = task + "_processed"
        new_completed = new_completed.push(processed_task)
      }
      
      // Update processor state
      updated_processor = {
        task_queue: remaining_queue,
        completed_tasks: updated_processor.completed_tasks + new_completed,
        in_progress: []
      }
    }
    
    updated_processor
  }
  
  // Test concurrent processing
  let processor = create_processor(3)
  let tasks = ["task1", "task2", "task3", "task4", "task5", "task6", "task7"]
  let processor_with_tasks = add_tasks(processor, tasks)
  
  assert_eq(processor_with_tasks.task_queue.length(), 7)
  assert_eq(processor_with_tasks.completed_tasks.length(), 0)
  
  let final_processor = simulate_concurrent_processing(processor_with_tasks)
  
  assert_eq(final_processor.task_queue.length(), 0)
  assert_eq(final_processor.completed_tasks.length(), 7)
  assert_true(final_processor.completed_tasks.contains("task1_processed"))
  assert_true(final_processor.completed_tasks.contains("task7_processed"))
  
  // Test with different worker counts
  let single_worker = create_processor(1)
  let single_worker_with_tasks = add_tasks(single_worker, ["a", "b", "c"])
  let single_worker_result = simulate_concurrent_processing(single_worker_with_tasks)
  
  assert_eq(single_worker_result.completed_tasks.length(), 3)
  
  // Test resource pool pattern
  type ResourcePool = {
    resources: Array[String],
    available: Array[Bool],
    allocated: Array[String]
  }
  
  let create_resource_pool = fn(resource_names: Array[String]) {
    {
      resources: resource_names,
      available: resource_names.map(fn(_) { true }),
      allocated: []
    }
  }
  
  let allocate_resource = fn(pool: ResourcePool, requester: String) {
    let mut updated_pool = pool
    let mut allocated = false
    
    for i in 0..pool.resources.length() {
      if pool.available[i] and not(allocated) {
        updated_pool.available[i] = false
        updated_pool.allocated = updated_pool.allocated.push(pool.resources[i] + ":" + requester)
        allocated = true
      }
    }
    
    (allocated, updated_pool)
  }
  
  let release_resource = fn(pool: ResourcePool, resource: String) {
    let mut updated_pool = pool
    let mut found = false
    
    for i in 0..pool.resources.length() {
      if pool.resources[i] == resource and not(found) {
        updated_pool.available[i] = true
        
        // Remove from allocated list
        let mut new_allocated = []
        for allocated_res in pool.allocated {
          if not(allocated_res.contains(resource)) {
            new_allocated = new_allocated.push(allocated_res)
          }
        }
        updated_pool.allocated = new_allocated
        found = true
      }
    }
    
    updated_pool
  }
  
  // Test resource pool
  let pool = create_resource_pool(["db-connection-1", "db-connection-2", "cache-connection-1"])
  
  let (allocated1, pool1) = allocate_resource(pool, "request-1")
  assert_true(allocated1)
  assert_eq(pool1.allocated.length(), 1)
  
  let (allocated2, pool2) = allocate_resource(pool1, "request-2")
  assert_true(allocated2)
  assert_eq(pool2.allocated.length(), 2)
  
  let (allocated3, pool3) = allocate_resource(pool2, "request-3")
  assert_true(allocated3)
  assert_eq(pool3.allocated.length(), 3)
  
  let (allocated4, pool4) = allocate_resource(pool3, "request-4")
  assert_false(allocated4)  // No resources available
  assert_eq(pool4.allocated.length(), 3)
  
  let pool5 = release_resource(pool4, "db-connection-1")
  assert_eq(pool5.allocated.length(), 2)
  
  let (allocated5, pool6) = allocate_resource(pool5, "request-4")
  assert_true(allocated5)  // Now a resource is available
  assert_eq(pool6.allocated.length(), 3)
}

// Test 4: Advanced Error Handling and Recovery
test "advanced error handling and recovery mechanisms" {
  // Define error hierarchy
  enum TelemetryError {
    NetworkError(String, Int)  // message, retry_count
    SerializationError(String, String)  // message, data_type
    ConfigurationError(String, String)  // message, config_key
    RateLimitError(Int, Int)  // retry_after, limit
    CriticalError(String, Array[String])  // message, context
  }
  
  // Define recovery strategy
  enum RecoveryAction {
    Retry(Int, Int)  // max_attempts, backoff_ms
    Fallback(String)  // fallback_value
    CircuitBreaker(Int, Int)  // failure_threshold, timeout_ms
    Escalate  // Escalate to higher level
    Ignore  // Log and continue
  }
  
  // Create error handler
  let handle_error = fn(error: TelemetryError, context: String) {
    match error {
      TelemetryError::NetworkError(msg, retry_count) => {
        if retry_count < 3 {
          RecoveryAction::Retry(3, 1000 * (retry_count + 1))
        } else {
          RecoveryAction::Fallback("offline_mode")
        }
      }
      TelemetryError::SerializationError(msg, data_type) => {
        if data_type == "metrics" {
          RecoveryAction::Fallback("default_metrics")
        } else {
          RecoveryAction::Retry(2, 500)
        }
      }
      TelemetryError::ConfigurationError(msg, config_key) => {
        if config_key == "sampling_rate" {
          RecoveryAction::Fallback("0.1")  // Default 10% sampling
        } else {
          RecoveryAction::Escalate
        }
      }
      TelemetryError::RateLimitError(retry_after, limit) => {
        RecoveryAction::Retry(1, retry_after)
      }
      TelemetryError::CriticalError(msg, context_info) => {
        if context_info.contains("production") {
          RecoveryAction::CircuitBreaker(5, 60000)
        } else {
          RecoveryAction::Ignore
        }
      }
    }
  }
  
  // Test error handling
  let network_error = TelemetryError::NetworkError("Connection timeout", 1)
  let network_action = handle_error(network_error, "sending_telemetry")
  assert_eq(network_action, RecoveryAction::Retry(3, 2000))
  
  let exhausted_network_error = TelemetryError::NetworkError("Connection timeout", 3)
  let exhausted_action = handle_error(exhausted_network_error, "sending_telemetry")
  assert_eq(exhausted_action, RecoveryAction::Fallback("offline_mode"))
  
  let metrics_serialization_error = TelemetryError::SerializationError("Invalid format", "metrics")
  let metrics_action = handle_error(metrics_serialization_error, "processing_metrics")
  assert_eq(metrics_action, RecoveryAction::Fallback("default_metrics"))
  
  let config_error = TelemetryError::ConfigurationError("Missing key", "sampling_rate")
  let config_action = handle_error(config_error, "initializing_sampler")
  assert_eq(config_action, RecoveryAction::Fallback("0.1"))
  
  let rate_limit_error = TelemetryError::RateLimitError(5000, 100)
  let rate_limit_action = handle_error(rate_limit_error, "sending_batch")
  assert_eq(rate_limit_action, RecoveryAction::Retry(1, 5000))
  
  let critical_error = TelemetryError::CriticalError("Database down", ["production", "payments"])
  let critical_action = handle_error(critical_error, "processing_payments")
  assert_eq(critical_action, RecoveryAction::CircuitBreaker(5, 60000))
  
  // Test error recovery state machine
  type RecoveryState = {
    status: String,
    attempts: Int,
    last_error: Option[TelemetryError],
    next_action: Option[RecoveryAction],
    context: String
  }
  
  let process_with_recovery = fn(initial_state: RecoveryState, operation: () -> Result[String, TelemetryError]) {
    let mut state = initial_state
    
    while state.attempts < 5 {
      let result = operation()
      
      match result {
        Ok(value) => {
          state = {
            status: "success",
            attempts: state.attempts + 1,
            last_error: None,
            next_action: None,
            context: state.context
          }
          return (state, Some(value))
        }
        Err(error) => {
          let action = handle_error(error, state.context)
          state = {
            status: "error",
            attempts: state.attempts + 1,
            last_error: Some(error),
            next_action: Some(action),
            context: state.context
          }
          
          match action {
            RecoveryAction::Retry(max_attempts, _) => {
              if state.attempts >= max_attempts {
                break
              }
            }
            RecoveryAction::Fallback(_) => {
              state = { state | status: "fallback" }
              return (state, Some("fallback_value"))
            }
            RecoveryAction::CircuitBreaker(_, _) => {
              state = { state | status: "circuit_breaker_open" }
              return (state, None)
            }
            RecoveryAction::Escalate => {
              state = { state | status: "escalated" }
              return (state, None)
            }
            RecoveryAction::Ignore => {
              state = { state | status: "ignored" }
              return (state, Some("ignored_value"))
            }
          }
        }
      }
    }
    
    (state, None)
  }
  
  // Test recovery state machine
  let initial_state = {
    status: "initial",
    attempts: 0,
    last_error: None,
    next_action: None,
    context: "test_operation"
  }
  
  // Simulate operation that fails twice then succeeds
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count <= 2 {
      Err(TelemetryError::NetworkError("Simulated failure", attempt_count))
    } else {
      Ok("success_value")
    }
  }
  
  let (final_state, result) = process_with_recovery(initial_state, flaky_operation)
  assert_eq(final_state.status, "success")
  assert_eq(final_state.attempts, 3)
  assert_eq(result, Some("success_value"))
}

// Test 5: Performance Optimization and Caching
test "performance optimization and caching strategies" {
  // Define cache structure
  type CacheEntry[T] = {
    value: T,
    timestamp: Int,
    ttl: Int,
    access_count: Int
  }
  
  type Cache[T] = {
    entries: Array[(String, CacheEntry[T])],
    max_size: Int,
    eviction_policy: String
  }
  
  let create_cache = fn(max_size: Int, policy: String) {
    {
      entries: [],
      max_size,
      eviction_policy: policy
    }
  }
  
  let get_from_cache = fn[T](cache: Cache[T], key: String, current_time: Int) {
    match cache.entries.find(fn(entry) { entry.0 == key }) {
      Some((_, entry)) => {
        if current_time - entry.timestamp < entry.ttl {
          // Cache hit - update access count
          let updated_entry = { entry | access_count: entry.access_count + 1 }
          (Some(updated_entry.value), true)
        } else {
          // Cache expired
          (None, false)
        }
      }
      None => (None, false)
    }
  }
  
  let put_in_cache = fn[T](cache: Cache[T], key: String, value: T, ttl: Int, current_time: Int) {
    let new_entry = {
      value,
      timestamp: current_time,
      ttl,
      access_count: 1
    }
    
    // Check if cache is full
    if cache.entries.length() >= cache.max_size {
      // Evict based on policy
      match cache.eviction_policy {
        "lru" => {
          // Find least recently used (lowest access count)
          let mut min_access = cache.entries[0].1.access_count
          let mut min_index = 0
          
          for i in 1..cache.entries.length() {
            if cache.entries[i].1.access_count < min_access {
              min_access = cache.entries[i].1.access_count
              min_index = i
            }
          }
          
          // Remove the least recently used entry
          let mut new_entries = []
          for i in 0..cache.entries.length() {
            if i != min_index {
              new_entries = new_entries.push(cache.entries[i])
            }
          }
          
          // Add new entry
          new_entries.push((key, new_entry))
          new_entries
        }
        "fifo" => {
          // Remove oldest entry (first in array)
          let new_entries = cache.entries.slice(1, cache.entries.length())
          new_entries.push((key, new_entry))
          new_entries
        }
        _ => {
          // Default to FIFO
          let new_entries = cache.entries.slice(1, cache.entries.length())
          new_entries.push((key, new_entry))
          new_entries
        }
      }
    } else {
      // Add to cache
      cache.entries.push((key, new_entry))
    }
  }
  
  // Test cache operations
  let cache = create_cache(3, "lru")
  let base_time = 1640995200
  
  // Add entries to cache
  let cache1 = put_in_cache(cache, "trace-1", "data-1", 1000, base_time)
  let cache2 = put_in_cache(cache1, "trace-2", "data-2", 1000, base_time + 100)
  let cache3 = put_in_cache(cache2, "trace-3", "data-3", 1000, base_time + 200)
  
  assert_eq(cache3.entries.length(), 3)
  
  // Test cache hit
  let (value1, hit1) = get_from_cache(cache3, "trace-2", base_time + 300)
  assert_eq(value1, Some("data-2"))
  assert_true(hit1)
  
  // Test cache miss
  let (value4, hit4) = get_from_cache(cache3, "trace-4", base_time + 300)
  assert_eq(value4, None)
  assert_false(hit4)
  
  // Test cache expiration
  let (value1_expired, hit1_expired) = get_from_cache(cache3, "trace-1", base_time + 1500)
  assert_eq(value1_expired, None)
  assert_false(hit1_expired)
  
  // Test cache eviction
  let cache4 = put_in_cache(cache3, "trace-4", "data-4", 1000, base_time + 300)
  assert_eq(cache4.entries.length(), 3)  // Should still be 3 due to eviction
  
  // Check that least recently used entry was evicted
  let (evicted_value, evicted_hit) = get_from_cache(cache4, "trace-1", base_time + 300)
  assert_eq(evicted_value, None)
  assert_false(evicted_hit)
  
  // Test performance optimization with batch processing
  type BatchProcessor = {
    batch_size: Int,
    flush_interval: Int,
    pending_items: Array[String],
    last_flush: Int
  }
  
  let create_batch_processor = fn(batch_size: Int, flush_interval: Int) {
    {
      batch_size,
      flush_interval,
      pending_items: [],
      last_flush: 0
    }
  }
  
  let add_to_batch = fn(processor: BatchProcessor, item: String, current_time: Int) {
    let new_pending = processor.pending_items.push(item)
    let should_flush = new_pending.length() >= processor.batch_size or
                     (current_time - processor.last_flush) >= processor.flush_interval
    
    if should_flush {
      // Flush the batch
      let flushed_items = new_pending
      let new_processor = {
        batch_size: processor.batch_size,
        flush_interval: processor.flush_interval,
        pending_items: [],
        last_flush: current_time
      }
      (new_processor, Some(flushed_items))
    } else {
      let new_processor = {
        processor |
        pending_items: new_pending
      }
      (new_processor, None)
    }
  }
  
  // Test batch processing
  let processor = create_batch_processor(3, 5000)
  let base_time = 1640995200
  
  let (processor1, batch1) = add_to_batch(processor, "item1", base_time)
  assert_eq(batch1, None)
  assert_eq(processor1.pending_items.length(), 1)
  
  let (processor2, batch2) = add_to_batch(processor1, "item2", base_time + 100)
  assert_eq(batch2, None)
  assert_eq(processor2.pending_items.length(), 2)
  
  let (processor3, batch3) = add_to_batch(processor2, "item3", base_time + 200)
  assert_eq(batch3.unwrap().length(), 3)
  assert_eq(processor3.pending_items.length(), 0)
  assert_eq(processor3.last_flush, base_time + 200)
  
  // Test time-based flush
  let (processor4, batch4) = add_to_batch(processor3, "item4", base_time + 100)
  assert_eq(batch4, None)
  assert_eq(processor4.pending_items.length(), 1)
  
  let (processor5, batch5) = add_to_batch(processor4, "item5", base_time + 5100)
  assert_eq(batch5.unwrap().length(), 2)  // item4 and item5
  assert_eq(processor5.pending_items.length(), 0)
  assert_eq(processor5.last_flush, base_time + 5100)
}

// Test 6: System Integration and Communication
test "system integration and communication patterns" {
  // Define message structure for inter-service communication
  type TelemetryMessage = {
    message_id: String,
    source_service: String,
    target_service: String,
    message_type: String,
    payload: String,
    timestamp: Int,
    correlation_id: String
  }
  
  // Define message router
  type MessageRouter = {
    routes: Array[(String, String)],  // (message_type, target_service)
    message_history: Array[TelemetryMessage],
    delivery_status: Array[(String, String)]  // (message_id, status)
  }
  
  let create_router = fn() {
    {
      routes: [
        ("metric_data", "aggregator-service"),
        ("trace_data", "trace-service"),
        ("log_data", "log-service"),
        ("alert", "alert-service")
      ],
      message_history: [],
      delivery_status: []
    }
  }
  
  let route_message = fn(router: MessageRouter, message: TelemetryMessage) {
    // Find route for message type
    let target_route = router.routes.find(fn(route) { route.0 == message.message_type })
    
    match target_route {
      Some((_, expected_target)) => {
        if expected_target == message.target_service {
          // Valid route
          let new_history = router.message_history.push(message)
          let new_status = router.delivery_status.push((message.message_id, "delivered"))
          
          ({
            routes: router.routes,
            message_history: new_history,
            delivery_status: new_status
          }, "success")
        } else {
          // Invalid target service
          let new_status = router.delivery_status.push((message.message_id, "failed: wrong target"))
          
          ({
            routes: router.routes,
            message_history: router.message_history,
            delivery_status: new_status
          }, "failed: wrong target")
        }
      }
      None => {
        // No route found
        let new_status = router.delivery_status.push((message.message_id, "failed: no route"))
        
        ({
          routes: router.routes,
          message_history: router.message_history,
          delivery_status: new_status
        }, "failed: no route")
      }
    }
  }
  
  // Test message routing
  let router = create_router()
  
  let valid_message = {
    message_id: "msg-001",
    source_service: "api-service",
    target_service: "aggregator-service",
    message_type: "metric_data",
    payload: "{\"metric\": \"cpu\", \"value\": 75.5}",
    timestamp: 1640995200,
    correlation_id: "corr-001"
  }
  
  let (router1, result1) = route_message(router, valid_message)
  assert_eq(result1, "success")
  assert_eq(router1.message_history.length(), 1)
  assert_eq(router1.delivery_status.length(), 1)
  assert_eq(router1.delivery_status[0], ("msg-001", "delivered"))
  
  let invalid_target_message = {
    message_id: "msg-002",
    source_service: "api-service",
    target_service: "wrong-service",
    message_type: "metric_data",
    payload: "{\"metric\": \"memory\", \"value\": 1024}",
    timestamp: 1640995210,
    correlation_id: "corr-002"
  }
  
  let (router2, result2) = route_message(router1, invalid_target_message)
  assert_eq(result2, "failed: wrong target")
  assert_eq(router2.message_history.length(), 1)  // Not added to history
  assert_eq(router2.delivery_status.length(), 2)
  assert_eq(router2.delivery_status[1], ("msg-002", "failed: wrong target"))
  
  let no_route_message = {
    message_id: "msg-003",
    source_service: "api-service",
    target_service: "aggregator-service",
    message_type: "unknown_type",
    payload: "{\"data\": \"test\"}",
    timestamp: 1640995220,
    correlation_id: "corr-003"
  }
  
  let (router3, result3) = route_message(router2, no_route_message)
  assert_eq(result3, "failed: no route")
  assert_eq(router3.message_history.length(), 1)
  assert_eq(router3.delivery_status.length(), 3)
  assert_eq(router3.delivery_status[2], ("msg-003", "failed: no route"))
  
  // Test service discovery and health checking
  type ServiceInstance = {
    instance_id: String,
    service_name: String,
    host: String,
    port: Int,
    health_status: String,
    last_check: Int,
    metadata: Array[(String, String)]
  }
  
  type ServiceRegistry = {
    services: Array[ServiceInstance],
    health_check_interval: Int
  }
  
  let create_registry = fn(check_interval: Int) {
    {
      services: [
        {
          instance_id: "svc-001",
          service_name: "aggregator-service",
          host: "10.0.1.10",
          port: 8080,
          health_status: "healthy",
          last_check: 1640995200,
          metadata: [("version", "1.2.3"), ("region", "us-west-2")]
        },
        {
          instance_id: "svc-002",
          service_name: "trace-service",
          host: "10.0.1.20",
          port: 8081,
          health_status: "healthy",
          last_check: 1640995200,
          metadata: [("version", "1.1.0"), ("region", "us-west-2")]
        },
        {
          instance_id: "svc-003",
          service_name: "aggregator-service",
          host: "10.0.1.11",
          port: 8080,
          health_status: "unhealthy",
          last_check: 1640995200,
          metadata: [("version", "1.2.3"), ("region", "us-west-2")]
        }
      ],
      health_check_interval: check_interval
    }
  }
  
  let discover_healthy_services = fn(registry: ServiceRegistry, service_name: String) {
    let mut healthy_instances = []
    for service in registry.services {
      if service.service_name == service_name and service.health_status == "healthy" {
        healthy_instances = healthy_instances.push(service)
      }
    }
    healthy_instances
  }
  
  let update_service_health = fn(registry: ServiceRegistry, instance_id: String, status: String, check_time: Int) {
    let mut updated_services = []
    for service in registry.services {
      if service.instance_id == instance_id {
        updated_services = updated_services.push({
          service |
          health_status: status,
          last_check: check_time
        })
      } else {
        updated_services = updated_services.push(service)
      }
    }
    
    {
      services: updated_services,
      health_check_interval: registry.health_check_interval
    }
  }
  
  // Test service discovery
  let registry = create_registry(30)
  
  let healthy_aggregators = discover_healthy_services(registry, "aggregator-service")
  assert_eq(healthy_aggregators.length(), 1)  // Only one healthy aggregator
  assert_eq(healthy_aggregators[0].instance_id, "svc-001")
  
  let healthy_tracers = discover_healthy_services(registry, "trace-service")
  assert_eq(healthy_tracers.length(), 1)  // One healthy tracer
  assert_eq(healthy_tracers[0].instance_id, "svc-002")
  
  // Test health status update
  let updated_registry = update_service_health(registry, "svc-003", "healthy", 1640995230)
  let new_healthy_aggregators = discover_healthy_services(updated_registry, "aggregator-service")
  assert_eq(new_healthy_aggregators.length(), 2)  // Now two healthy aggregators
  
  // Test load balancing
  let select_instance_round_robin = fn(instances: Array[ServiceInstance], request_count: Int) {
    if instances.length() == 0 {
      None
    } else {
      let index = request_count % instances.length()
      Some(instances[index])
    }
  }
  
  let select_instance_random = fn(instances: Array[ServiceInstance]) {
    if instances.length() == 0 {
      None
    } else {
      // Simple pseudo-random based on current time
      let index = (Time::now() % instances.length())
      Some(instances[index])
    }
  }
  
  // Test round-robin selection
  let instance1 = select_instance_round_robin(new_healthy_aggregators, 0)
  let instance2 = select_instance_round_robin(new_healthy_aggregators, 1)
  
  match (instance1, instance2) {
    (Some(inst1), Some(inst2)) => {
      assert_not_eq(inst1.instance_id, inst2.instance_id)
    }
    _ => assert_true(false)
  }
}

// Test 7: Security and Access Control
test "security and access control mechanisms" {
  // Define authentication token
  type AuthToken = {
    token_id: String,
    user_id: String,
    roles: Array[String],
    permissions: Array[String],
    expires_at: Int,
    issued_at: Int
  }
  
  // Define access control entry
  type AccessControlEntry = {
    resource: String,
    required_permissions: Array[String],
    allowed_roles: Array[String]
  }
  
  // Define access control system
  type AccessControlSystem = {
    tokens: Array[AuthToken],
    access_rules: Array[AccessControlEntry],
    audit_log: Array[(String, String, String, Int)]  // (user_id, resource, action, timestamp)
  }
  
  let create_access_control = fn() {
    {
      tokens: [
        {
          token_id: "token-001",
          user_id: "user-001",
          roles: ["admin", "operator"],
          permissions: ["read", "write", "delete", "manage"],
          expires_at: 1640998800,  // 1 hour from base time
          issued_at: 1640995200
        },
        {
          token_id: "token-002",
          user_id: "user-002",
          roles: ["operator"],
          permissions: ["read", "write"],
          expires_at: 1640998800,
          issued_at: 1640995200
        },
        {
          token_id: "token-003",
          user_id: "user-003",
          roles: ["viewer"],
          permissions: ["read"],
          expires_at: 1640998800,
          issued_at: 1640995200
        }
      ],
      access_rules: [
        {
          resource: "telemetry_data",
          required_permissions: ["read"],
          allowed_roles: ["admin", "operator", "viewer"]
        },
        {
          resource: "telemetry_config",
          required_permissions: ["read", "write"],
          allowed_roles: ["admin", "operator"]
        },
        {
          resource: "user_management",
          required_permissions: ["read", "write", "delete"],
          allowed_roles: ["admin"]
        }
      ],
      audit_log: []
    }
  }
  
  let validate_token = fn(acs: AccessControlSystem, token_id: String, current_time: Int) {
    match acs.tokens.find(fn(token) { token.token_id == token_id }) {
      Some(token) => {
        if current_time < token.expires_at {
          Some(token)
        } else {
          None  // Token expired
        }
      }
      None => None  // Token not found
    }
  }
  
  let check_permission = fn(acs: AccessControlSystem, user_id: String, resource: String, action: String, current_time: Int) {
    // First, find the user's token
    let user_token_opt = acs.tokens.find(fn(token) { token.user_id == user_id })
    
    match user_token_opt {
      Some(token) => {
        // Check if token is valid
        if current_time >= token.expires_at {
          (false, "Token expired")
        } else {
          // Find access rule for the resource
          match acs.access_rules.find(fn(rule) { rule.resource == resource }) {
            Some(rule) => {
              // Check if user has required permissions
              let has_permission = rule.required_permissions.all(fn(req_perm) {
                token.permissions.contains(req_perm)
              })
              
              // Check if user has allowed role
              let has_role = token.roles.any(fn(role) {
                rule.allowed_roles.contains(role)
              })
              
              if has_permission and has_role {
                (true, "Access granted")
              } else {
                (false, "Insufficient permissions or role")
              }
            }
            None => (false, "Resource not found in access rules")
          }
        }
      }
      None => (false, "User not found")
    }
  }
  
  let log_access_attempt = fn(acs: AccessControlSystem, user_id: String, resource: String, action: String, timestamp: Int, success: Bool) {
    let status = if success { "granted" } else { "denied" }
    let new_entry = (user_id, resource, action + ":" + status, timestamp)
    {
      acs |
      audit_log: acs.audit_log.push(new_entry)
    }
  }
  
  // Test access control
  let acs = create_access_control()
  let current_time = 1640995600  // Within token validity period
  
  // Test admin access
  let (admin_access, admin_msg) = check_permission(acs, "user-001", "telemetry_config", "write", current_time)
  assert_true(admin_access)
  assert_eq(admin_msg, "Access granted")
  
  let acs1 = log_access_attempt(acs, "user-001", "telemetry_config", "write", current_time, admin_access)
  assert_eq(acs1.audit_log.length(), 1)
  
  // Test operator access
  let (operator_access, operator_msg) = check_permission(acs1, "user-002", "telemetry_config", "write", current_time)
  assert_true(operator_access)
  assert_eq(operator_msg, "Access granted")
  
  let acs2 = log_access_attempt(acs1, "user-002", "telemetry_config", "write", current_time, operator_access)
  assert_eq(acs2.audit_log.length(), 2)
  
  // Test viewer access (should be denied)
  let (viewer_access, viewer_msg) = check_permission(acs2, "user-003", "telemetry_config", "write", current_time)
  assert_false(viewer_access)
  assert_eq(viewer_msg, "Insufficient permissions or role")
  
  let acs3 = log_access_attempt(acs2, "user-003", "telemetry_config", "write", current_time, viewer_access)
  assert_eq(acs3.audit_log.length(), 3)
  
  // Test expired token
  let future_time = 1640999000  // After token expiry
  let (expired_access, expired_msg) = check_permission(acs3, "user-001", "telemetry_data", "read", future_time)
  assert_false(expired_access)
  assert_eq(expired_msg, "Token expired")
  
  // Test data encryption simulation
  let encrypt_data = fn(data: String, key: String) {
    // Simple XOR encryption simulation
    let data_chars = data.to_char_array()
    let key_chars = key.to_char_array()
    let mut encrypted = []
    
    for i in 0..data_chars.length() {
      let key_char = key_chars[i % key_chars.length()]
      let encrypted_char = (data_chars[i].to_int() ^ key_char.to_int()).to_char()
      encrypted = encrypted.push(encrypted_char)
    }
    
    encrypted.from_char_array()
  }
  
  let decrypt_data = fn(encrypted_data: String, key: String) {
    // Decryption is the same as encryption for XOR
    encrypt_data(encrypted_data, key)
  }
  
  // Test encryption
  let sensitive_data = "user_password_123"
  let encryption_key = "secret_key_123"
  
  let encrypted = encrypt_data(sensitive_data, encryption_key)
  assert_not_eq(encrypted, sensitive_data)
  
  let decrypted = decrypt_data(encrypted, encryption_key)
  assert_eq(decrypted, sensitive_data)
  
  // Test with wrong key
  let wrong_key = "wrong_key_456"
  let wrong_decrypted = decrypt_data(encrypted, wrong_key)
  assert_not_eq(wrong_decrypted, sensitive_data)
}

// Test 8: Reliability and Fault Tolerance
test "reliability and fault tolerance mechanisms" {
  // Define circuit breaker state
  enum CircuitState {
    Closed    // Normal operation
    Open      // Circuit is open, requests fail immediately
    HalfOpen  // Testing if service has recovered
  }
  
  // Define circuit breaker
  type CircuitBreaker = {
    state: CircuitState,
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,
    failure_count: Int,
    success_count: Int,
    last_failure_time: Int,
    next_attempt: Int
  }
  
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, timeout: Int) {
    {
      state: CircuitState::Closed,
      failure_threshold,
      success_threshold,
      timeout,
      failure_count: 0,
      success_count: 0,
      last_failure_time: 0,
      next_attempt: 0
    }
  }
  
  let call_with_circuit_breaker = fn(cb: CircuitBreaker, operation: () -> Result[String, String], current_time: Int) {
    match cb.state {
      CircuitState::Closed => {
        // Normal operation, try the operation
        let result = operation()
        
        match result {
          Ok(value) => {
            // Success, reset failure count
            let updated_cb = {
              cb |
              failure_count: 0,
              success_count: cb.success_count + 1
            }
            (updated_cb, result)
          }
          Err(error) => {
            // Failure, increment failure count
            let new_failure_count = cb.failure_count + 1
            let new_state = if new_failure_count >= cb.failure_threshold {
              CircuitState::Open
            } else {
              CircuitState::Closed
            }
            
            let updated_cb = {
              state: new_state,
              failure_threshold: cb.failure_threshold,
              success_threshold: cb.success_threshold,
              timeout: cb.timeout,
              failure_count: new_failure_count,
              success_count: 0,
              last_failure_time: current_time,
              next_attempt: current_time + cb.timeout
            }
            
            (updated_cb, result)
          }
        }
      }
      CircuitState::Open => {
        // Circuit is open, check if timeout has passed
        if current_time >= cb.next_attempt {
          // Try half-open state
          let result = operation()
          
          match result {
            Ok(value) => {
              // Success, move to closed state
              let updated_cb = {
                cb |
                state: CircuitState::Closed,
                failure_count: 0,
                success_count: 1
              }
              (updated_cb, result)
            }
            Err(error) => {
              // Still failing, keep circuit open
              let updated_cb = {
                cb |
                last_failure_time: current_time,
                next_attempt: current_time + cb.timeout
              }
              (updated_cb, result)
            }
          }
        } else {
          // Still in timeout period, fail immediately
          (cb, Err("Circuit breaker is open"))
        }
      }
      CircuitState::HalfOpen => {
        // Testing state, try the operation
        let result = operation()
        
        match result {
          Ok(value) => {
            // Success, increment success count
            let new_success_count = cb.success_count + 1
            let new_state = if new_success_count >= cb.success_threshold {
              CircuitState::Closed
            } else {
              CircuitState::HalfOpen
            }
            
            let updated_cb = {
              cb |
              state: new_state,
              success_count: new_success_count
            }
            (updated_cb, result)
          }
          Err(error) => {
            // Failure, back to open state
            let updated_cb = {
              cb |
              state: CircuitState::Open,
              failure_count: cb.failure_threshold,  // Reset to threshold
              success_count: 0,
              last_failure_time: current_time,
              next_attempt: current_time + cb.timeout
            }
            (updated_cb, result)
          }
        }
      }
    }
  }
  
  // Test circuit breaker
  let cb = create_circuit_breaker(3, 2, 10000)  // 3 failures to open, 2 successes to close, 10s timeout
  let base_time = 1640995200
  
  // Simulate operations
  let mut operation_count = 0
  let telemetry_operation = fn() {
    operation_count = operation_count + 1
    if operation_count <= 3 {
      Err("Service unavailable")
    } else if operation_count <= 5 {
      Ok("Telemetry data processed")
    } else {
      Err("Service temporarily unavailable")
    }
  }
  
  // First few operations should fail
  let (cb1, result1) = call_with_circuit_breaker(cb, telemetry_operation, base_time)
  assert_eq(result1, Err("Service unavailable"))
  assert_eq(cb1.state, CircuitState::Closed)
  assert_eq(cb1.failure_count, 1)
  
  let (cb2, result2) = call_with_circuit_breaker(cb1, telemetry_operation, base_time + 100)
  assert_eq(result2, Err("Service unavailable"))
  assert_eq(cb2.state, CircuitState::Closed)
  assert_eq(cb2.failure_count, 2)
  
  let (cb3, result3) = call_with_circuit_breaker(cb2, telemetry_operation, base_time + 200)
  assert_eq(result3, Err("Service unavailable"))
  assert_eq(cb3.state, CircuitState::Open)  // Circuit should open now
  assert_eq(cb3.failure_count, 3)
  
  // Next operation should fail immediately due to open circuit
  let (cb4, result4) = call_with_circuit_breaker(cb3, telemetry_operation, base_time + 300)
  assert_eq(result4, Err("Circuit breaker is open"))
  assert_eq(cb4.state, CircuitState::Open)
  
  // After timeout, should try half-open state
  let (cb5, result5) = call_with_circuit_breaker(cb4, telemetry_operation, base_time + 11000);
  assert_eq(result5, Ok("Telemetry data processed"));
  assert_eq(cb5.state, CircuitState::Closed);  // Should close immediately on first success
  
  // Test retry mechanism with exponential backoff
  type RetryConfig = {
    max_attempts: Int,
    initial_delay: Int,
    max_delay: Int,
    backoff_multiplier: Float
  }
  
  let execute_with_retry = fn(config: RetryConfig, operation: () -> Result[String, String], current_time: Int) {
    let mut attempt = 1
    let mut delay = config.initial_delay
    let mut result = operation()
    let mut total_delay = 0
    
    while attempt < config.max_attempts and result.is_err() {
      total_delay = total_delay + delay
      delay = (delay as Float * config.backoff_multiplier) as Int
      if delay > config.max_delay {
        delay = config.max_delay
      }
      
      attempt = attempt + 1
      result = operation()
    }
    
    (result, attempt, total_delay)
  }
  
  // Test retry mechanism
  let retry_config = {
    max_attempts: 5,
    initial_delay: 1000,
    max_delay: 10000,
    backoff_multiplier: 2.0
  }
  
  let mut flaky_operation_count = 0
  let flaky_operation = fn() {
    flaky_operation_count = flaky_operation_count + 1
    if flaky_operation_count <= 3 {
      Err("Temporary failure")
    } else {
      Ok("Operation succeeded")
    }
  }
  
  let (retry_result, attempts, total_delay) = execute_with_retry(retry_config, flaky_operation, base_time)
  assert_eq(retry_result, Ok("Operation succeeded"))
  assert_eq(attempts, 4)
  assert_eq(total_delay, 1000 + 2000 + 4000)  // 1s + 2s + 4s
  
  // Test bulkhead pattern for resource isolation
  type Bulkhead = {
    name: String,
    max_concurrent: Int,
    max_queue: Int,
    active_count: Int,
    queue_size: Int
  }
  
  let create_bulkhead = fn(name: String, max_concurrent: Int, max_queue: Int) {
    {
      name,
      max_concurrent,
      max_queue,
      active_count: 0,
      queue_size: 0
    }
  }
  
  let try_acquire_bulkhead = fn(bulkhead: Bulkhead) {
    if bulkhead.active_count < bulkhead.max_concurrent {
      // Can execute immediately
      (true, { bulkhead | active_count: bulkhead.active_count + 1 })
    } else if bulkhead.queue_size < bulkhead.max_queue {
      // Can queue
      (true, { bulkhead | queue_size: bulkhead.queue_size + 1 })
    } else {
      // Bulkhead is full
      (false, bulkhead)
    }
  }
  
  let release_bulkhead = fn(bulkhead: Bulkhead) {
    if bulkhead.active_count > 0 {
      { bulkhead | active_count: bulkhead.active_count - 1 }
    } else {
      bulkhead
    }
  }
  
  let process_queue = fn(bulkhead: Bulkhead) {
    if bulkhead.queue_size > 0 and bulkhead.active_count < bulkhead.max_concurrent {
      {
        active_count: bulkhead.active_count + 1,
        queue_size: bulkhead.queue_size - 1,
        name: bulkhead.name,
        max_concurrent: bulkhead.max_concurrent,
        max_queue: bulkhead.max_queue
      }
    } else {
      bulkhead
    }
  }
  
  // Test bulkhead pattern
  let bulkhead = create_bulkhead("telemetry_processor", 3, 5)
  
  // Acquire slots
  let (acquired1, bulkhead1) = try_acquire_bulkhead(bulkhead)
  assert_true(acquired1)
  assert_eq(bulkhead1.active_count, 1)
  
  let (acquired2, bulkhead2) = try_acquire_bulkhead(bulkhead1)
  assert_true(acquired2)
  assert_eq(bulkhead2.active_count, 2)
  
  let (acquired3, bulkhead3) = try_acquire_bulkhead(bulkhead2)
  assert_true(acquired3)
  assert_eq(bulkhead3.active_count, 3)
  
  // Next acquisition should go to queue
  let (acquired4, bulkhead4) = try_acquire_bulkhead(bulkhead3)
  assert_true(acquired4)
  assert_eq(bulkhead4.active_count, 3)
  assert_eq(bulkhead4.queue_size, 1)
  
  let (acquired5, bulkhead5) = try_acquire_bulkhead(bulkhead4)
  assert_true(acquired5)
  assert_eq(bulkhead5.active_count, 3)
  assert_eq(bulkhead5.queue_size, 2)
  
  // Release one slot and process queue
  let bulkhead6 = release_bulkhead(bulkhead5)
  assert_eq(bulkhead6.active_count, 2)
  assert_eq(bulkhead6.queue_size, 2)
  
  let bulkhead7 = process_queue(bulkhead6)
  assert_eq(bulkhead7.active_count, 3)
  assert_eq(bulkhead7.queue_size, 1)
  
  // Fill up the queue
  let (acquired6, bulkhead8) = try_acquire_bulkhead(bulkhead7)
  assert_true(acquired6)
  assert_eq(bulkhead8.queue_size, 2)
  
  let (acquired7, bulkhead9) = try_acquire_bulkhead(bulkhead8)
  assert_true(acquired7)
  assert_eq(bulkhead9.queue_size, 3)
  
  let (acquired8, bulkhead10) = try_acquire_bulkhead(bulkhead9)
  assert_true(acquired8)
  assert_eq(bulkhead10.queue_size, 4)
  
  let (acquired9, bulkhead11) = try_acquire_bulkhead(bulkhead10)
  assert_true(acquired9)
  assert_eq(bulkhead11.queue_size, 5)
  
  // Next acquisition should fail - bulkhead is full
  let (acquired10, bulkhead12) = try_acquire_bulkhead(bulkhead11)
  assert_false(acquired10)
  assert_eq(bulkhead12.active_count, 3)
  assert_eq(bulkhead12.queue_size, 5)
}