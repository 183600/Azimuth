// Azimuth Performance Optimization Test Suite
// This file contains high-quality test cases for performance optimization in the telemetry system

// Test 1: Memory Pool for Telemetry Object Allocation
test "memory pool for telemetry object allocation" {
  // Define telemetry span object
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Define memory pool
  type MemoryPool = {
    available: Array[TelemetrySpan],
    in_use: Array[TelemetrySpan],
    total_created: Int,
    total_reused: Int
  }
  
  // Create memory pool
  let create_memory_pool = fn(initial_size: Int) {
    let mut spans = []
    for i in 0..initial_size {
      spans = spans.push({
        trace_id: "",
        span_id: "",
        parent_span_id: None,
        operation_name: "",
        start_time: 0,
        end_time: 0,
        status: ""
      })
    }
    
    {
      available: spans,
      in_use: [],
      total_created: initial_size,
      total_reused: 0
    }
  }
  
  // Acquire from pool
  let acquire = fn(pool: MemoryPool) {
    if pool.available.length() > 0 {
      let span = pool.available[0]
      let updated_available = pool.available.slice(1)
      let updated_in_use = pool.in_use.push(span)
      
      ({
        available: updated_available,
        in_use: updated_in_use,
        total_created: pool.total_created,
        total_reused: pool.total_reused
      }, span)
    } else {
      // Create new span if pool is empty
      let new_span = {
        trace_id: "",
        span_id: "",
        parent_span_id: None,
        operation_name: "",
        start_time: 0,
        end_time: 0,
        status: ""
      }
      
      let updated_in_use = pool.in_use.push(new_span)
      
      ({
        available: pool.available,
        in_use: updated_in_use,
        total_created: pool.total_created + 1,
        total_reused: pool.total_reused
      }, new_span)
    }
  }
  
  // Release to pool
  let release = fn(pool: MemoryPool, span: TelemetrySpan) {
    // Reset span values
    let reset_span = {
      trace_id: "",
      span_id: "",
      parent_span_id: None,
      operation_name: "",
      start_time: 0,
      end_time: 0,
      status: ""
    }
    
    // Remove from in_use
    let updated_in_use = pool.in_use.filter(fn(s) { s != span })
    
    // Add to available
    let updated_available = pool.available.push(reset_span)
    
    {
      available: updated_available,
      in_use: updated_in_use,
      total_created: pool.total_created,
      total_reused: pool.total_reused + 1
    }
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(5)
  assert_eq(pool.available.length(), 5)
  assert_eq(pool.in_use.length(), 0)
  assert_eq(pool.total_created, 5)
  assert_eq(pool.total_reused, 0)
  
  // Acquire spans
  let (pool1, span1) = acquire(pool)
  let (pool2, span2) = acquire(pool1)
  let (pool3, span3) = acquire(pool2)
  
  assert_eq(pool3.available.length(), 2)
  assert_eq(pool3.in_use.length(), 3)
  assert_eq(pool3.total_created, 5)
  assert_eq(pool3.total_reused, 0)
  
  // Release spans
  let pool4 = release(pool3, span1)
  let pool5 = release(pool4, span2)
  
  assert_eq(pool5.available.length(), 4)
  assert_eq(pool5.in_use.length(), 1)
  assert_eq(pool5.total_created, 5)
  assert_eq(pool5.total_reused, 2)
  
  // Acquire again (should reuse)
  let (pool6, span4) = acquire(pool5)
  
  assert_eq(pool6.available.length(), 3)
  assert_eq(pool6.in_use.length(), 2)
  assert_eq(pool6.total_created, 5)
  assert_eq(pool6.total_reused, 2)
  
  // Acquire more than initial capacity
  let (pool7, _) = acquire(pool6)
  let (pool8, _) = acquire(pool7)
  let (pool9, _) = acquire(pool8)
  let (pool10, _) = acquire(pool9)
  
  assert_eq(pool10.available.length(), 0)
  assert_eq(pool10.in_use.length(), 5)
  assert_eq(pool10.total_created, 7)  // 2 new spans created
  assert_eq(pool10.total_reused, 2)
}

// Test 2: Batch Processing for Telemetry Events
test "batch processing for telemetry events" {
  // Define telemetry event
  type TelemetryEvent = {
    timestamp: Int,
    event_type: String,
    data: String
  }
  
  // Define batch processor
  type BatchProcessor = {
    batch_size: Int,
    current_batch: Array[TelemetryEvent],
    processed_batches: Array[Array[TelemetryEvent]]
  }
  
  // Create batch processor
  let create_batch_processor = fn(batch_size: Int) {
    {
      batch_size: batch_size,
      current_batch: [],
      processed_batches: []
    }
  }
  
  // Add event to batch
  let add_event = fn(processor: BatchProcessor, event: TelemetryEvent) {
    let updated_batch = processor.current_batch.push(event)
    
    if updated_batch.length() >= processor.batch_size {
      // Process batch
      {
        batch_size: processor.batch_size,
        current_batch: [],
        processed_batches: processor.processed_batches.push(updated_batch)
      }
    } else {
      {
        batch_size: processor.batch_size,
        current_batch: updated_batch,
        processed_batches: processor.processed_batches
      }
    }
  }
  
  // Flush remaining events
  let flush = fn(processor: BatchProcessor) {
    if processor.current_batch.length() > 0 {
      {
        batch_size: processor.batch_size,
        current_batch: [],
        processed_batches: processor.processed_batches.push(processor.current_batch)
      }
    } else {
      processor
    }
  }
  
  // Test batch processing
  let processor = create_batch_processor(3)
  
  // Create events
  let event1 = { timestamp: 1000, event_type: "span_start", data: "span1" }
  let event2 = { timestamp: 1001, event_type: "span_end", data: "span1" }
  let event3 = { timestamp: 1002, event_type: "metric", data: "cpu:80" }
  let event4 = { timestamp: 1003, event_type: "log", data: "error:timeout" }
  let event5 = { timestamp: 1004, event_type: "span_start", data: "span2" }
  
  // Add events
  let processor1 = add_event(processor, event1)
  assert_eq(processor1.current_batch.length(), 1)
  assert_eq(processor1.processed_batches.length(), 0)
  
  let processor2 = add_event(processor1, event2)
  assert_eq(processor2.current_batch.length(), 2)
  assert_eq(processor2.processed_batches.length(), 0)
  
  let processor3 = add_event(processor2, event3)
  assert_eq(processor3.current_batch.length(), 0)  // Batch processed
  assert_eq(processor3.processed_batches.length(), 1)
  assert_eq(processor3.processed_batches[0].length(), 3)
  
  let processor4 = add_event(processor3, event4)
  let processor5 = add_event(processor4, event5)
  
  assert_eq(processor5.current_batch.length(), 2)
  assert_eq(processor5.processed_batches.length(), 1)
  
  // Flush remaining events
  let processor6 = flush(processor5)
  assert_eq(processor6.current_batch.length(), 0)
  assert_eq(processor6.processed_batches.length(), 2)
  assert_eq(processor6.processed_batches[1].length(), 2)
}

// Test 3: Sampling Strategy for High-Volume Telemetry
test "sampling strategy for high-volume telemetry" {
  // Define sampling decision
  enum SamplingDecision {
    RecordAndSample
    Record
    Drop
  }
  
  // Define sampling strategy
  type SamplingStrategy = {
    sample_rate: Float,
    max_events_per_second: Int,
    current_events: Int,
    current_second: Int
  }
  
  // Create sampling strategy
  let create_sampling_strategy = fn(sample_rate: Float, max_events_per_second: Int) {
    {
      sample_rate: sample_rate,
      max_events_per_second: max_events_per_second,
      current_events: 0,
      current_second: 0
    }
  }
  
  // Simple hash function for consistent sampling
  let hash_string = fn(str: String) {
    let mut hash = 0
    let chars = str.to_char_array()
    for i in 0..chars.length() {
      hash = (hash * 31 + chars[i].to_int()) % 10000
    }
    if hash < 0 { hash = hash + 10000 }
    hash
  }
  
  // Make sampling decision
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, timestamp: Int) {
    // Check rate limit
    let current_second = timestamp / 1000
    
    let (events_count, updated_strategy) = if current_second == strategy.current_second {
      if strategy.current_events >= strategy.max_events_per_second {
        (strategy.current_events, strategy)
      } else {
        (strategy.current_events + 1, { strategy | current_events: strategy.current_events + 1 })
      }
    } else {
      (1, { strategy | current_events: 1, current_second: current_second })
    }
    
    // Check if we've hit the rate limit
    if events_count > strategy.max_events_per_second {
      (updated_strategy, Drop)
    } else {
      // Check sample rate using consistent hashing
      let hash_value = hash_string(trace_id)
      let threshold = (strategy.sample_rate * 10000.0) as Int
      
      if hash_value < threshold {
        (updated_strategy, RecordAndSample)
      } else {
        (updated_strategy, Record)
      }
    }
  }
  
  // Test sampling strategy
  let strategy = create_sampling_strategy(0.1, 10)  // 10% sample rate, max 10 events/second
  
  // Test with different trace IDs
  let trace_ids = [
    "trace-1", "trace-2", "trace-3", "trace-4", "trace-5",
    "trace-6", "trace-7", "trace-8", "trace-9", "trace-10",
    "trace-11", "trace-12"
  ]
  
  let timestamp = 1000
  let mut strategy_state = strategy
  let mut sampled_count = 0
  let mut recorded_count = 0
  let mut dropped_count = 0
  
  for i in 0..trace_ids.length() {
    let (updated_strategy, decision) = should_sample(strategy_state, trace_ids[i], timestamp)
    strategy_state = updated_strategy
    
    match decision {
      RecordAndSample => sampled_count = sampled_count + 1
      Record => recorded_count = recorded_count + 1
      Drop => dropped_count = dropped_count + 1
    }
  }
  
  // With 10% sample rate, we expect approximately 1-2 sampled events
  assert_true(sampled_count >= 0 and sampled_count <= 3)
  assert_true(recorded_count >= 8)
  assert_eq(dropped_count, 2)  // Last 2 events dropped due to rate limit
  
  // Test with consistent trace ID (should always get same decision)
  let consistent_trace_id = "consistent-trace"
  let (strategy_state1, decision1) = should_sample(strategy_state, consistent_trace_id, timestamp)
  let (strategy_state2, decision2) = should_sample(strategy_state1, consistent_trace_id, timestamp)
  let (strategy_state3, decision3) = should_sample(strategy_state2, consistent_trace_id, timestamp)
  
  // All decisions should be the same for the same trace ID
  assert_eq(decision1, decision2)
  assert_eq(decision2, decision3)
}

// Test 4: Adaptive Buffer Sizing
test "adaptive buffer sizing" {
  // Define buffer with adaptive sizing
  type AdaptiveBuffer = {
    data: Array[String],
    min_size: Int,
    max_size: Int,
    current_size: Int,
    growth_factor: Float,
    shrink_threshold: Float
  }
  
  // Create adaptive buffer
  let create_adaptive_buffer = fn(initial_size: Int, min_size: Int, max_size: Int) {
    {
      data: [],
      min_size: min_size,
      max_size: max_size,
      current_size: initial_size,
      growth_factor: 1.5,
      shrink_threshold: 0.25
    }
  }
  
  // Add item to buffer
  let add_item = fn(buffer: AdaptiveBuffer, item: String) {
    let updated_data = buffer.data.push(item)
    
    // Check if we need to grow
    let new_size = if updated_data.length() > buffer.current_size {
      let proposed_size = (buffer.current_size as Float * buffer.growth_factor) as Int
      if proposed_size <= buffer.max_size {
        proposed_size
      } else {
        buffer.max_size
      }
    } else {
      buffer.current_size
    }
    
    {
      data: updated_data,
      min_size: buffer.min_size,
      max_size: buffer.max_size,
      current_size: new_size,
      growth_factor: buffer.growth_factor,
      shrink_threshold: buffer.shrink_threshold
    }
  }
  
  // Remove item from buffer
  let remove_item = fn(buffer: AdaptiveBuffer) {
    if buffer.data.length() > 0 {
      let updated_data = buffer.data.slice(0, buffer.data.length() - 1)
      
      // Check if we need to shrink
      let utilization = updated_data.length() as Float / buffer.current_size as Float
      let new_size = if utilization < buffer.shrink_threshold and buffer.current_size > buffer.min_size {
        let proposed_size = (buffer.current_size as Float / buffer.growth_factor) as Int
        if proposed_size >= buffer.min_size {
          proposed_size
        } else {
          buffer.min_size
        }
      } else {
        buffer.current_size
      }
      
      ({
        data: updated_data,
        min_size: buffer.min_size,
        max_size: buffer.max_size,
        current_size: new_size,
        growth_factor: buffer.growth_factor,
        shrink_threshold: buffer.shrink_threshold
      }, Some(buffer.data[buffer.data.length() - 1]))
    } else {
      (buffer, None)
    }
  }
  
  // Test adaptive buffer
  let buffer = create_adaptive_buffer(5, 3, 20)
  assert_eq(buffer.current_size, 5)
  assert_eq(buffer.data.length(), 0)
  
  // Add items to trigger growth
  let buffer1 = add_item(buffer, "item1")
  let buffer2 = add_item(buffer1, "item2")
  let buffer3 = add_item(buffer2, "item3")
  let buffer4 = add_item(buffer3, "item4")
  let buffer5 = add_item(buffer4, "item5")
  
  assert_eq(buffer5.current_size, 5)  // No growth yet
  assert_eq(buffer5.data.length(), 5)
  
  // Add one more to trigger growth
  let buffer6 = add_item(buffer5, "item6")
  assert_eq(buffer6.current_size, 7)  // Grown to 7 (5 * 1.5 = 7.5, rounded down)
  assert_eq(buffer6.data.length(), 6)
  
  // Add more items
  let buffer7 = add_item(buffer6, "item7")
  let buffer8 = add_item(buffer7, "item8")
  
  assert_eq(buffer8.current_size, 7)
  assert_eq(buffer8.data.length(), 8)
  
  // Add one more to trigger another growth
  let buffer9 = add_item(buffer8, "item9")
  assert_eq(buffer9.current_size, 10)  // Grown to 10 (7 * 1.5 = 10.5, rounded down)
  assert_eq(buffer9.data.length(), 9)
  
  // Remove items to trigger shrinking
  let (buffer10, _) = remove_item(buffer9)
  let (buffer11, _) = remove_item(buffer10)
  let (buffer12, _) = remove_item(buffer11)
  let (buffer13, _) = remove_item(buffer12)
  let (buffer14, _) = remove_item(buffer13)
  let (buffer15, _) = remove_item(buffer14)
  
  assert_eq(buffer15.data.length(), 3)
  assert_eq(buffer15.current_size, 10)  // No shrinking yet (3/10 = 0.3 > 0.25)
  
  // Remove one more to trigger shrinking
  let (buffer16, _) = remove_item(buffer15)
  assert_eq(buffer16.data.length(), 2)
  assert_eq(buffer16.current_size, 6)  // Shrunk to 6 (10 / 1.5 = 6.67, rounded down)
  
  // Remove more items
  let (buffer17, _) = remove_item(buffer16)
  
  assert_eq(buffer17.data.length(), 1)
  assert_eq(buffer17.current_size, 6)  // No shrinking (1/6 ≈ 0.17 < 0.25, but 6/1.5 = 4 < min_size)
}

// Test 5: Lazy Initialization for Telemetry Components
test "lazy initialization for telemetry components" {
  // Define lazy telemetry component
  type LazyComponent = {
    initialized: Bool,
    data: Option[String],
    init_function: () -> String
  }
  
  // Create lazy component
  let create_lazy_component = fn(init_function: () -> String) {
    {
      initialized: false,
      data: None,
      init_function: init_function
    }
  }
  
  // Get component data (initialize if needed)
  let get_data = fn(component: LazyComponent) {
    if component.initialized {
      match component.data {
        Some(data) => (component, data)
        None => {
          // This shouldn't happen if initialized is true
          let new_data = component.init_function()
          let updated_component = {
            initialized: true,
            data: Some(new_data),
            init_function: component.init_function
          }
          (updated_component, new_data)
        }
      }
    } else {
      let new_data = component.init_function()
      let updated_component = {
        initialized: true,
        data: Some(new_data),
        init_function: component.init_function
      }
      (updated_component, new_data)
    }
  }
  
  // Test lazy initialization
  let init_count = { mut count: 0 }
  
  let expensive_init = fn() {
    init_count.count = init_count.count + 1
    "expensive-data-" + init_count.count.to_string()
  }
  
  let component = create_lazy_component(expensive_init)
  
  // Initially, component should not be initialized
  assert_false(component.initialized)
  assert_eq(component.data, None)
  assert_eq(init_count.count, 0)
  
  // First access should trigger initialization
  let (component1, data1) = get_data(component)
  assert_true(component1.initialized)
  assert_eq(component1.data, Some("expensive-data-1"))
  assert_eq(data1, "expensive-data-1")
  assert_eq(init_count.count, 1)
  
  // Second access should not trigger initialization
  let (component2, data2) = get_data(component1)
  assert_true(component2.initialized)
  assert_eq(component2.data, Some("expensive-data-1"))
  assert_eq(data2, "expensive-data-1")
  assert_eq(init_count.count, 1)  // No additional initialization
  
  // Test with multiple lazy components
  let component_a = create_lazy_component(fn() { "component-a-data" })
  let component_b = create_lazy_component(fn() { "component-b-data" })
  let component_c = create_lazy_component(fn() { "component-c-data" })
  
  // Access only component a and b
  let (_, data_a) = get_data(component_a)
  let (_, data_b) = get_data(component_b)
  
  assert_eq(data_a, "component-a-data")
  assert_eq(data_b, "component-b-data")
  
  // Component c should not be initialized yet
  assert_false(component_c.initialized)
  
  // Now access component c
  let (_, data_c) = get_data(component_c)
  assert_eq(data_c, "component-c-data")
  assert_true(component_c.initialized)
}

// Test 6: Connection Pooling for Telemetry Exporters
test "connection pooling for telemetry exporters" {
  // Define connection
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool
  }
  
  // Define connection pool
  type ConnectionPool = {
    max_connections: Int,
    connections: Array[Connection],
    next_id: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(max_connections: Int) {
    {
      max_connections: max_connections,
      connections: [],
      next_id: 1
    }
  }
  
  // Acquire connection
  let acquire_connection = fn(pool: ConnectionPool, timestamp: Int) {
    // Find available connection
    let available_index = pool.connections.find_index(fn(conn) { not(conn.in_use) })
    
    match available_index {
      Some(index) => {
        let conn = pool.connections[index]
        let updated_conn = { conn | in_use: true, last_used: timestamp }
        let updated_connections = pool.connections.update(index, updated_conn)
        
        ({
          max_connections: pool.max_connections,
          connections: updated_connections,
          next_id: pool.next_id
        }, updated_conn)
      }
      None => {
        if pool.connections.length() < pool.max_connections {
          // Create new connection
          let new_conn = {
            id: "conn-" + pool.next_id.to_string(),
            created_at: timestamp,
            last_used: timestamp,
            in_use: true
          }
          
          let updated_connections = pool.connections.push(new_conn)
          
          ({
            max_connections: pool.max_connections,
            connections: updated_connections,
            next_id: pool.next_id + 1
          }, new_conn)
        } else {
          // No available connections
          (pool, { id: "", created_at: 0, last_used: 0, in_use: false })
        }
      }
    }
  }
  
  // Release connection
  let release_connection = fn(pool: ConnectionPool, connection_id: String) {
    let updated_connections = pool.connections.map(fn(conn) {
      if conn.id == connection_id {
        { conn | in_use: false }
      } else {
        conn
      }
    })
    
    {
      max_connections: pool.max_connections,
      connections: updated_connections,
      next_id: pool.next_id
    }
  }
  
  // Test connection pooling
  let pool = create_connection_pool(3)
  assert_eq(pool.connections.length(), 0)
  
  let timestamp = 1000
  
  // Acquire connections
  let (pool1, conn1) = acquire_connection(pool, timestamp)
  assert_eq(pool1.connections.length(), 1)
  assert_true(conn1.in_use)
  assert_eq(conn1.id, "conn-1")
  
  let (pool2, conn2) = acquire_connection(pool1, timestamp)
  assert_eq(pool2.connections.length(), 2)
  assert_true(conn2.in_use)
  assert_eq(conn2.id, "conn-2")
  
  let (pool3, conn3) = acquire_connection(pool2, timestamp)
  assert_eq(pool3.connections.length(), 3)
  assert_true(conn3.in_use)
  assert_eq(conn3.id, "conn-3")
  
  // Try to acquire when pool is full
  let (pool4, conn4) = acquire_connection(pool3, timestamp)
  assert_eq(pool4.connections.length(), 3)
  assert_false(conn4.in_use)
  assert_eq(conn4.id, "")
  
  // Release a connection
  let pool5 = release_connection(pool4, "conn-1")
  assert_false(pool5.connections[0].in_use)
  
  // Acquire again (should reuse released connection)
  let (pool6, conn5) = acquire_connection(pool5, timestamp + 100)
  assert_eq(pool6.connections.length(), 3)
  assert_true(conn5.in_use)
  assert_eq(conn5.id, "conn-1")  // Reused connection
  assert_eq(conn5.last_used, timestamp + 100)  // Updated last used time
}