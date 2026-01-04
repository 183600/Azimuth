// Azimuth Realtime Stream Processing Comprehensive Tests
// This file contains comprehensive realtime stream processing tests

// Test 1: Stream Data Processing Pipeline
test "stream data processing pipeline for telemetry data" {
  // Define stream event
  type StreamEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    metadata: Array<(String, String)>
  }
  
  // Define stream processor
  type StreamProcessor = {
    processor_id: String,
    input_buffer: Array<StreamEvent>,
    output_buffer: Array<StreamEvent>,
    processing_rules: Array<ProcessingRule>,
    events_processed: Int,
    events_dropped: Int,
    processing_time_ms: Int
  }
  
  // Define processing rule
  type ProcessingRule = {
    rule_id: String,
    condition: String,
    action: String,
    priority: Int
  }
  
  // Create stream processor
  let create_stream_processor = fn(processor_id: String, rules: Array<ProcessingRule>) {
    {
      processor_id: processor_id,
      input_buffer: [],
      output_buffer: [],
      processing_rules: rules,
      events_processed: 0,
      events_dropped: 0,
      processing_time_ms: 0
    }
  }
  
  // Add event to input buffer
  let add_event = fn(processor: StreamProcessor, event: StreamEvent) {
    {
      processor_id: processor.processor_id,
      input_buffer: processor.input_buffer.push(event),
      output_buffer: processor.output_buffer,
      processing_rules: processor.processing_rules,
      events_processed: processor.events_processed,
      events_dropped: processor.events_dropped,
      processing_time_ms: processor.processing_time_ms
    }
  }
  
  // Process events
  let process_events = fn(processor: StreamProcessor) {
    let mut output_events = []
    let mut processed_count = 0
    let mut dropped_count = 0
    let mut processing_time = 0
    
    for event in processor.input_buffer {
      let mut event_processed = false
      let mut should_drop = false
      
      # Apply processing rules in priority order
      let sorted_rules = processor.processing_rules.sort_by(fn(r1, r2) { 
        r1.priority <= r2.priority 
      })
      
      for rule in sorted_rules {
        # Simulate rule evaluation
        let rule_matches = match rule.condition {
          "event_type=telemetry" => event.event_type == "telemetry"
          "event_type=metric" => event.event_type == "metric"
          "event_type=log" => event.event_type == "log"
          "data_contains=error" => event.data.contains("error")
          "timestamp_recent" => event.timestamp > 1640995000
          _ => false
        }
        
        if rule_matches {
          # Apply rule action
          match rule.action {
            "forward" => {
              output_events = output_events.push(event)
              event_processed = true
              processing_time = processing_time + 5
            }
            "transform" => {
              let transformed_event = {
                event_id: event.event_id,
                timestamp: event.timestamp,
                event_type: event.event_type,
                data: "transformed:" + event.data,
                metadata: event.metadata.push(("processed", "true"))
              }
              output_events = output_events.push(transformed_event)
              event_processed = true
              processing_time = processing_time + 10
            }
            "filter" => {
              should_drop = true
              processing_time = processing_time + 3
            }
            "enrich" => {
              let enriched_event = {
                event_id: event.event_id,
                timestamp: event.timestamp,
                event_type: event.event_type,
                data: event.data,
                metadata: event.metadata.push(("enriched", "true"), ("enrichment_time", "1640995300"))
              }
              output_events = output_events.push(enriched_event)
              event_processed = true
              processing_time = processing_time + 8
            }
            _ => {}
          }
          break  # Stop after first matching rule
        }
      }
      
      if event_processed {
        processed_count = processed_count + 1
      } else if should_drop {
        dropped_count = dropped_count + 1
      } else {
        # Default action: forward
        output_events = output_events.push(event)
        processed_count = processed_count + 1
        processing_time = processing_time + 2
      }
    }
    
    {
      processor_id: processor.processor_id,
      input_buffer: [],  # Clear input buffer
      output_buffer: output_events,
      processing_rules: processor.processing_rules,
      events_processed: processor.events_processed + processed_count,
      events_dropped: processor.events_dropped + dropped_count,
      processing_time_ms: processor.processing_time_ms + processing_time
    }
  }
  
  // Test stream processing
  let rules = [
    {
      rule_id: "rule-1",
      condition: "event_type=telemetry",
      action: "enrich",
      priority: 1
    },
    {
      rule_id: "rule-2",
      condition: "data_contains=error",
      action: "transform",
      priority: 2
    },
    {
      rule_id: "rule-3",
      condition: "event_type=metric",
      action: "forward",
      priority: 3
    },
    {
      rule_id: "rule-4",
      condition: "timestamp_recent",
      action: "forward",
      priority: 4
    }
  ]
  
  let processor = create_stream_processor("telemetry-processor", rules)
  
  # Add events
  let event1 = {
    event_id: "event-1",
    timestamp: 1640995200,
    event_type: "telemetry",
    data: "trace-data-123",
    metadata: [("source", "service-a")]
  }
  
  let event2 = {
    event_id: "event-2",
    timestamp: 1640995210,
    event_type: "metric",
    data: "response_time:150ms",
    metadata: [("source", "service-b")]
  }
  
  let event3 = {
    event_id: "event-3",
    timestamp: 1640995220,
    event_type: "log",
    data: "error:connection_failed",
    metadata: [("source", "service-c")]
  }
  
  let event4 = {
    event_id: "event-4",
    timestamp: 1640994900,  # Old timestamp
    event_type: "telemetry",
    data: "trace-data-456",
    metadata: [("source", "service-d")]
  }
  
  let processor1 = add_event(processor, event1)
  let processor2 = add_event(processor1, event2)
  let processor3 = add_event(processor2, event3)
  let processor4 = add_event(processor3, event4)
  
  # Process events
  let processed_processor = process_events(processor4)
  
  # Verify processing results
  assert_eq(processed_processor.events_processed, 4)
  assert_eq(processed_processor.events_dropped, 0)
  assert_eq(processed_processor.output_buffer.length(), 4)
  assert_true(processed_processor.processing_time_ms > 0)
  
  # Verify specific transformations
  let output_event1 = processed_processor.output_buffer.filter(fn(e) { e.event_id == "event-1" })[0]
  assert_eq(output_event1.event_type, "telemetry")
  assert_eq(output_event1.data, "trace-data-123")  # Original data preserved
  assert_true(output_event1.metadata.contains(("enriched", "true")))
  
  let output_event2 = processed_processor.output_buffer.filter(fn(e) { e.event_id == "event-2" })[0]
  assert_eq(output_event2.event_type, "metric")
  assert_eq(output_event2.data, "response_time:150ms")  # Original data preserved
  
  let output_event3 = processed_processor.output_buffer.filter(fn(e) { e.event_id == "event-3" })[0]
  assert_eq(output_event3.event_type, "log")
  assert_eq(output_event3.data, "transformed:error:connection_failed")  # Transformed due to error content
  
  let output_event4 = processed_processor.output_buffer.filter(fn(e) { e.event_id == "event-4" })[0]
  assert_eq(output_event4.event_type, "telemetry")
  assert_eq(output_event4.data, "trace-data-456")  # Original data preserved (no enrichment due to old timestamp)
}

// Test 2: Windowed Stream Aggregation
test "windowed stream aggregation for telemetry metrics" {
  // Define time window
  type TimeWindow = {
    window_id: String,
    start_time: Int,
    end_time: Int,
    events: Array<StreamEvent>,
    aggregated_metrics: Array<AggregatedMetric>
  }
  
  // Define aggregated metric
  type AggregatedMetric = {
    metric_name: String,
    value: Float,
    count: Int,
    min: Float,
    max: Float,
    avg: Float,
    window_id: String
  }
  
  // Define stream event (reuse from previous test)
  type StreamEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    metadata: Array<(String, String)>
  }
  
  // Define window manager
  type WindowManager = {
    window_size_seconds: Int,
    windows: Array<TimeWindow>,
    current_window_start: Int,
    events_processed: Int
  }
  
  // Create window manager
  let create_window_manager = fn(window_size_seconds: Int) {
    {
      window_size_seconds: window_size_seconds,
      windows: [],
      current_window_start: 1640995200,
      events_processed: 0
    }
  }
  
  // Add event to window
  let add_event_to_window = fn(manager: WindowManager, event: StreamEvent) {
    let mut updated_windows = manager.windows
    let current_time = event.timestamp
    
    # Check if we need to create a new window
    let window_start = (current_time / manager.window_size_seconds) * manager.window_size_seconds
    let window_end = window_start + manager.window_size_seconds
    
    # Find or create window
    let mut target_window = None
    for i in 0..updated_windows.length() {
      if updated_windows[i].start_time == window_start {
        target_window = Some(updated_windows[i])
        break
      }
    }
    
    match target_window {
      Some(window) => {
        # Add event to existing window
        let updated_events = window.events.push(event)
        let updated_window = {
          window_id: window.window_id,
          start_time: window.start_time,
          end_time: window.end_time,
          events: updated_events,
          aggregated_metrics: window.aggregated_metrics
        }
        
        for i in 0..updated_windows.length() {
          if updated_windows[i].window_id == window.window_id {
            updated_windows[i] = updated_window
            break
          }
        }
      }
      None => {
        # Create new window
        let new_window = {
          window_id: "window-" + window_start.to_string(),
          start_time: window_start,
          end_time: window_end,
          events: [event],
          aggregated_metrics: []
        }
        updated_windows = updated_windows.push(new_window)
      }
    }
    
    {
      window_size_seconds: manager.window_size_seconds,
      windows: updated_windows,
      current_window_start: manager.current_window_start,
      events_processed: manager.events_processed + 1
    }
  }
  
  // Aggregate metrics in window
  let aggregate_window_metrics = fn(window: TimeWindow) {
    let mut metric_aggregates = {}
    
    # Collect numeric values from events
    for event in window.events {
      if event.event_type == "metric" {
        # Parse metric data (format: "metric_name:value")
        let parts = event.data.split(":")
        if parts.length() == 2 {
          let metric_name = parts[0]
          let metric_value = parts[1].to_float()
          
          let current = metric_aggregates.get_or(metric_name, {
            values: [],
            count: 0
          })
          
          metric_aggregates[metric_name] = {
            values: current.values.push(metric_value),
            count: current.count + 1
          }
        }
      }
    }
    
    # Calculate aggregated metrics
    let mut aggregated_metrics = []
    for (metric_name, aggregate) in metric_aggregates {
      let values = aggregate.values
      let count = aggregate.count
      
      if count > 0 {
        let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
        let min = values.reduce(fn(min, val) { if val < min { val } else { min }, values[0])
        let max = values.reduce(fn(max, val) { if val > max { val } else { max }, values[0])
        let avg = sum / count.to_float()
        
        aggregated_metrics = aggregated_metrics.push({
          metric_name: metric_name,
          value: avg,
          count: count,
          min: min,
          max: max,
          avg: avg,
          window_id: window.window_id
        })
      }
    }
    
    {
      window_id: window.window_id,
      start_time: window.start_time,
      end_time: window.end_time,
      events: window.events,
      aggregated_metrics: aggregated_metrics
    }
  }
  
  # Aggregate all windows
  let aggregate_all_windows = fn(manager: WindowManager) {
    let mut updated_windows = []
    
    for window in manager.windows {
      let aggregated_window = aggregate_window_metrics(window)
      updated_windows = updated_windows.push(aggregated_window)
    }
    
    {
      window_size_seconds: manager.window_size_seconds,
      windows: updated_windows,
      current_window_start: manager.current_window_start,
      events_processed: manager.events_processed
    }
  }
  
  # Test windowed aggregation
  let manager = create_window_manager(60)  # 60-second windows
  
  # Add events across different time windows
  let events = [
    {
      event_id: "event-1",
      timestamp: 1640995200,  # Window 1 (1640995200-1640995260)
      event_type: "metric",
      data: "response_time:100",
      metadata: [("service", "api")]
    },
    {
      event_id: "event-2",
      timestamp: 1640995210,  # Window 1
      event_type: "metric",
      data: "response_time:150",
      metadata: [("service", "api")]
    },
    {
      event_id: "event-3",
      timestamp: 1640995220,  # Window 1
      event_type: "metric",
      data: "throughput:1000",
      metadata: [("service", "api")]
    },
    {
      event_id: "event-4",
      timestamp: 1640995270,  # Window 2 (1640995260-1640995320)
      event_type: "metric",
      data: "response_time:120",
      metadata: [("service", "api")]
    },
    {
      event_id: "event-5",
      timestamp: 1640995280,  # Window 2
      event_type: "metric",
      data: "response_time:80",
      metadata: [("service", "api")]
    },
    {
      event_id: "event-6",
      timestamp: 1640995290,  # Window 2
      event_type: "metric",
      data: "throughput:1200",
      metadata: [("service", "api")]
    }
  ]
  
  # Add events to manager
  let mut manager_with_events = manager
  for event in events {
    manager_with_events = add_event_to_window(manager_with_events, event)
  }
  
  # Verify windows created
  assert_eq(manager_with_events.windows.length(), 2)
  assert_eq(manager_with_events.events_processed, 6)
  
  # Aggregate metrics
  let aggregated_manager = aggregate_all_windows(manager_with_events)
  
  # Verify aggregated metrics
  let window1 = aggregated_manager.windows.filter(fn(w) { w.start_time == 1640995200 })[0]
  let window2 = aggregated_manager.windows.filter(fn(w) { w.start_time == 1640995260 })[0]
  
  # Window 1 metrics
  assert_eq(window1.events.length(), 3)
  assert_eq(window1.aggregated_metrics.length(), 2)
  
  let window1_response_time = window1.aggregated_metrics.filter(fn(m) { m.metric_name == "response_time" })[0]
  assert_eq(window1_response_time.count, 2)
  assert_eq(window1_response_time.min, 100.0)
  assert_eq(window1_response_time.max, 150.0)
  assert_eq(window1_response_time.avg, 125.0)
  
  let window1_throughput = window1.aggregated_metrics.filter(fn(m) { m.metric_name == "throughput" })[0]
  assert_eq(window1_throughput.count, 1)
  assert_eq(window1_throughput.min, 1000.0)
  assert_eq(window1_throughput.max, 1000.0)
  assert_eq(window1_throughput.avg, 1000.0)
  
  # Window 2 metrics
  assert_eq(window2.events.length(), 3)
  assert_eq(window2.aggregated_metrics.length(), 2)
  
  let window2_response_time = window2.aggregated_metrics.filter(fn(m) { m.metric_name == "response_time" })[0]
  assert_eq(window2_response_time.count, 2)
  assert_eq(window2_response_time.min, 80.0)
  assert_eq(window2_response_time.max, 120.0)
  assert_eq(window2_response_time.avg, 100.0)
  
  let window2_throughput = window2.aggregated_metrics.filter(fn(m) { m.metric_name == "throughput" })[0]
  assert_eq(window2_throughput.count, 1)
  assert_eq(window2_throughput.min, 1200.0)
  assert_eq(window2_throughput.max, 1200.0)
  assert_eq(window2_throughput.avg, 1200.0)
}

// Test 3: Stream Backpressure Handling
test "stream backpressure handling and flow control" {
  // Define backpressure manager
  type BackpressureManager = {
    buffer_capacity: Int,
    current_buffer_size: Int,
    drop_policy: String,  # "drop_oldest", "drop_newest", "block"
    events_dropped: Int,
    events_processed: Int,
    backpressure_active: Bool
  }
  
  // Define stream event (reuse from previous test)
  type StreamEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    metadata: Array<(String, String)>
  }
  
  // Create backpressure manager
  let create_backpressure_manager = fn(buffer_capacity: Int, drop_policy: String) {
    {
      buffer_capacity: buffer_capacity,
      current_buffer_size: 0,
      drop_policy: drop_policy,
      events_dropped: 0,
      events_processed: 0,
      backpressure_active: false
    }
  }
  
  # Handle incoming event with backpressure
  let handle_event_with_backpressure = fn(manager: BackpressureManager, event: StreamEvent) {
    let buffer_full = manager.current_buffer_size >= manager.buffer_capacity
    let should_drop = if buffer_full {
      match manager.drop_policy {
        "drop_oldest" => true
        "drop_newest" => true
        "block" => false
        _ => false
      }
    } else {
      false
    }
    
    if should_drop {
      # Drop event
      {
        buffer_capacity: manager.buffer_capacity,
        current_buffer_size: manager.current_buffer_size,
        drop_policy: manager.drop_policy,
        events_dropped: manager.events_dropped + 1,
        events_processed: manager.events_processed,
        backpressure_active: true
      }
    } else {
      # Accept event
      let new_buffer_size = if buffer_full && manager.drop_policy == "block" {
        manager.current_buffer_size  # No change if blocking
      } else {
        manager.current_buffer_size + 1
      }
      
      {
        buffer_capacity: manager.buffer_capacity,
        current_buffer_size: new_buffer_size,
        drop_policy: manager.drop_policy,
        events_dropped: manager.events_dropped,
        events_processed: manager.events_processed + 1,
        backpressure_active: buffer_full
      }
    }
  }
  
  # Process events (simulate processing to free buffer space)
  let process_events = fn(manager: BackpressureManager, processed_count: Int) {
    let new_buffer_size = if manager.current_buffer_size >= processed_count {
      manager.current_buffer_size - processed_count
    } else {
      0
    }
    
    {
      buffer_capacity: manager.buffer_capacity,
      current_buffer_size: new_buffer_size,
      drop_policy: manager.drop_policy,
      events_dropped: manager.events_dropped,
      events_processed: manager.events_processed + processed_count,
      backpressure_active: new_buffer_size >= manager.buffer_capacity
    }
  }
  
  # Test backpressure with drop_oldest policy
  let drop_oldest_manager = create_backpressure_manager(3, "drop_oldest")
  
  # Add events
  let event1 = { event_id: "event-1", timestamp: 1640995200, event_type: "telemetry", data: "data-1", metadata: [] }
  let event2 = { event_id: "event-2", timestamp: 1640995210, event_type: "telemetry", data: "data-2", metadata: [] }
  let event3 = { event_id: "event-3", timestamp: 1640995220, event_type: "telemetry", data: "data-3", metadata: [] }
  let event4 = { event_id: "event-4", timestamp: 1640995230, event_type: "telemetry", data: "data-4", metadata: [] }
  
  let manager1 = handle_event_with_backpressure(drop_oldest_manager, event1)
  let manager2 = handle_event_with_backpressure(manager1, event2)
  let manager3 = handle_event_with_backpressure(manager2, event3)
  
  # Buffer should be full now
  assert_eq(manager3.current_buffer_size, 3)
  assert_false(manager3.backpressure_active)
  assert_eq(manager3.events_dropped, 0)
  assert_eq(manager3.events_processed, 3)
  
  # Add event when buffer is full (should drop)
  let manager4 = handle_event_with_backpressure(manager3, event4)
  assert_eq(manager4.current_buffer_size, 3)  # Still full
  assert_true(manager4.backpressure_active)
  assert_eq(manager4.events_dropped, 1)
  assert_eq(manager4.events_processed, 3)  # No new event processed
  
  # Process some events to free buffer space
  let manager5 = process_events(manager4, 2)
  assert_eq(manager5.current_buffer_size, 1)
  assert_false(manager5.backpressure_active)
  assert_eq(manager5.events_processed, 5)  # 3 initial + 2 processed
  
  # Test backpressure with drop_newest policy
  let drop_newest_manager = create_backpressure_manager(3, "drop_newest")
  
  let manager6 = handle_event_with_backpressure(drop_newest_manager, event1)
  let manager7 = handle_event_with_backpressure(manager6, event2)
  let manager8 = handle_event_with_backpressure(manager7, event3)
  let manager9 = handle_event_with_backpressure(manager8, event4)
  
  assert_eq(manager9.current_buffer_size, 3)  # Still full
  assert_true(manager9.backpressure_active)
  assert_eq(manager9.events_dropped, 1)
  assert_eq(manager9.events_processed, 3)  # No new event processed
  
  # Test backpressure with block policy
  let block_manager = create_backpressure_manager(3, "block")
  
  let manager10 = handle_event_with_backpressure(block_manager, event1)
  let manager11 = handle_event_with_backpressure(manager10, event2)
  let manager12 = handle_event_with_backpressure(manager11, event3)
  
  # Buffer should be full now
  assert_eq(manager12.current_buffer_size, 3)
  assert_false(manager12.backpressure_active)
  assert_eq(manager12.events_dropped, 0)
  assert_eq(manager12.events_processed, 3)
  
  # Add event when buffer is full (should block, not drop)
  let manager13 = handle_event_with_backpressure(manager12, event4)
  assert_eq(manager13.current_buffer_size, 3)  # Still full (blocking)
  assert_true(manager13.backpressure_active)
  assert_eq(manager13.events_dropped, 0)  # No drops
  assert_eq(manager13.events_processed, 4)  # Event processed but blocked
  
  # Test backpressure metrics
  let get_backpressure_metrics = fn(manager: BackpressureManager) {
    let buffer_utilization = if manager.buffer_capacity > 0 {
      (manager.current_buffer_size.to_float() / manager.buffer_capacity.to_float()) * 100.0
    } else {
      0.0
    }
    
    let drop_rate = if manager.events_processed + manager.events_dropped > 0 {
      (manager.events_dropped.to_float() / (manager.events_processed + manager.events_dropped).to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      buffer_capacity: manager.buffer_capacity,
      current_buffer_size: manager.current_buffer_size,
      buffer_utilization_percent: buffer_utilization,
      events_processed: manager.events_processed,
      events_dropped: manager.events_dropped,
      drop_rate_percent: drop_rate,
      backpressure_active: manager.backpressure_active
    }
  }
  
  let metrics = get_backpressure_metrics(manager9)
  assert_eq(metrics.buffer_capacity, 3)
  assert_eq(metrics.current_buffer_size, 3)
  assert_eq(metrics.buffer_utilization_percent, 100.0)
  assert_eq(metrics.events_processed, 3)
  assert_eq(metrics.events_dropped, 1)
  assert_eq(metrics.drop_rate_percent, 25.0)  # 1 drop out of 4 total events
  assert_true(metrics.backpressure_active)
}

// Test 4: Stream Join Operations
test "stream join operations for correlating telemetry data" {
  // Define stream event (reuse from previous test)
  type StreamEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    data: String,
    metadata: Array<(String, String)>
  }
  
  // Define join operation
  type StreamJoin = {
    join_id: String,
    left_stream: Array<StreamEvent>,
    right_stream: Array<StreamEvent>,
    join_condition: String,
    join_window_seconds: Int,
    joined_events: Array<JoinedEvent>
  }
  
  // Define joined event
  type JoinedEvent = {
    join_id: String,
    left_event: StreamEvent,
    right_event: StreamEvent,
    join_timestamp: Int
  }
  
  // Create stream join
  let create_stream_join = fn(join_id: String, join_condition: String, join_window_seconds: Int) {
    {
      join_id: join_id,
      left_stream: [],
      right_stream: [],
      join_condition: join_condition,
      join_window_seconds: join_window_seconds,
      joined_events: []
    }
  }
  
  # Add event to left stream
  let add_left_event = fn(join_op: StreamJoin, event: StreamEvent) {
    {
      join_id: join_op.join_id,
      left_stream: join_op.left_stream.push(event),
      right_stream: join_op.right_stream,
      join_condition: join_op.join_condition,
      join_window_seconds: join_op.join_window_seconds,
      joined_events: join_op.joined_events
    }
  }
  
  # Add event to right stream
  let add_right_event = fn(join_op: StreamJoin, event: StreamEvent) {
    {
      join_id: join_op.join_id,
      left_stream: join_op.left_stream,
      right_stream: join_op.right_stream.push(event),
      join_condition: join_op.join_condition,
      join_window_seconds: join_op.join_window_seconds,
      joined_events: join_op.joined_events
    }
  }
  
  # Perform join operation
  let perform_join = fn(join_op: StreamJoin) {
    let mut joined_events = []
    
    for left_event in join_op.left_stream {
      for right_event in join_op.right_stream {
        # Check join condition
        let condition_met = match join_op.join_condition {
          "trace_id_match" => {
            # Extract trace_id from metadata
            let left_trace_id = left_event.metadata.filter(fn(m) { m[0] == "trace_id" })
            let right_trace_id = right_event.metadata.filter(fn(m) { m[0] == "trace_id" })
            
            left_trace_id.length() > 0 && right_trace_id.length() > 0 &&
            left_trace_id[0][1] == right_trace_id[0][1]
          }
          "service_match" => {
            # Extract service from metadata
            let left_service = left_event.metadata.filter(fn(m) { m[0] == "service" })
            let right_service = right_event.metadata.filter(fn(m) { m[0] == "service" })
            
            left_service.length() > 0 && right_service.length() > 0 &&
            left_service[0][1] == right_service[0][1]
          }
          "time_window" => {
            # Check if events are within time window
            let time_diff = left_event.timestamp - right_event.timestamp
            time_diff >= 0 && time_diff <= join_op.join_window_seconds
          }
          _ => false
        }
        
        if condition_met {
          joined_events = joined_events.push({
            join_id: join_op.join_id,
            left_event: left_event,
            right_event: right_event,
            join_timestamp: 1640995300
          })
        }
      }
    }
    
    {
      join_id: join_op.join_id,
      left_stream: join_op.left_stream,
      right_stream: join_op.right_stream,
      join_condition: join_op.join_condition,
      join_window_seconds: join_op.join_window_seconds,
      joined_events: joined_events
    }
  }
  
  # Test stream join
  let join_op = create_stream_join("trace-join", "trace_id_match", 30)  # 30-second window
  
  # Add left stream events (spans)
  let left_events = [
    {
      event_id: "span-1",
      timestamp: 1640995200,
      event_type: "span",
      data: "http_request",
      metadata: [
        ("trace_id", "trace-123"),
        ("service", "api-gateway"),
        ("operation", "process_request")
      ]
    },
    {
      event_id: "span-2",
      timestamp: 1640995210,
      event_type: "span",
      data: "database_query",
      metadata: [
        ("trace_id", "trace-456"),
        ("service", "user-service"),
        ("operation", "get_user")
      ]
    },
    {
      event_id: "span-3",
      timestamp: 1640995220,
      event_type: "span",
      data: "cache_lookup",
      metadata: [
        ("trace_id", "trace-123"),
        ("service", "cache-service"),
        ("operation", "get_cache")
      ]
    }
  ]
  
  # Add right stream events (logs)
  let right_events = [
    {
      event_id: "log-1",
      timestamp: 1640995205,
      event_type: "log",
      data: "Request received",
      metadata: [
        ("trace_id", "trace-123"),
        ("service", "api-gateway"),
        ("level", "info")
      ]
    },
    {
      event_id: "log-2",
      timestamp: 1640995215,
      event_type: "log",
      data: "User query executed",
      metadata: [
        ("trace_id", "trace-456"),
        ("service", "user-service"),
        ("level", "info")
      ]
    },
    {
      event_id: "log-3",
      timestamp: 1640995225,
      event_type: "log",
      data: "Cache hit",
      metadata: [
        ("trace_id", "trace-789"),
        ("service", "cache-service"),
        ("level", "info")
      ]
    }
  ]
  
  # Add events to join operation
  let mut join_with_left = join_op
  for event in left_events {
    join_with_left = add_left_event(join_with_left, event)
  }
  
  let mut join_with_both = join_with_left
  for event in right_events {
    join_with_both = add_right_event(join_with_both, event)
  }
  
  # Perform join
  let joined_result = perform_join(join_with_both)
  
  # Verify join results
  assert_eq(joined_result.left_stream.length(), 3)
  assert_eq(joined_result.right_stream.length(), 3)
  assert_eq(joined_result.joined_events.length(), 2)  # span-1 with log-1, span-2 with log-2
  
  # Verify specific joins
  let join1 = joined_result.joined_events.filter(fn(j) { j.left_event.event_id == "span-1" })[0]
  assert_eq(join1.right_event.event_id, "log-1")
  assert_eq(join1.left_event.metadata.filter(fn(m) { m[0] == "trace_id" })[0][1], "trace-123")
  assert_eq(join1.right_event.metadata.filter(fn(m) { m[0] == "trace_id" })[0][1], "trace-123")
  
  let join2 = joined_result.joined_events.filter(fn(j) { j.left_event.event_id == "span-2" })[0]
  assert_eq(join2.right_event.event_id, "log-2")
  assert_eq(join2.left_event.metadata.filter(fn(m) { m[0] == "trace_id" })[0][1], "trace-456")
  assert_eq(join2.right_event.metadata.filter(fn(m) { m[0] == "trace_id" })[0][1], "trace-456")
  
  # span-3 should not join with log-3 (different trace_id)
  let span3_joins = joined_result.joined_events.filter(fn(j) { j.left_event.event_id == "span-3" })
  assert_eq(span3_joins.length(), 0)
  
  # Test time window join
  let time_join = create_stream_join("time-join", "time_window", 15)  # 15-second window
  
  # Add events with specific timestamps
  let time_left_events = [
    {
      event_id: "time-span-1",
      timestamp: 1640995200,
      event_type: "span",
      data: "operation_start",
      metadata: [("trace_id", "trace-time-1")]
    },
    {
      event_id: "time-span-2",
      timestamp: 1640995250,
      event_type: "span",
      data: "operation_start",
      metadata: [("trace_id", "trace-time-2")]
    }
  ]
  
  let time_right_events = [
    {
      event_id: "time-log-1",
      timestamp: 1640995210,
      event_type: "log",
      data: "operation_progress",
      metadata: [("trace_id", "trace-time-1")]
    },
    {
      event_id: "time-log-2",
      timestamp: 1640995270,
      event_type: "log",
      data: "operation_progress",
      metadata: [("trace_id", "trace-time-2")]
    }
  ]
  
  # Add events to time join
  let mut time_join_with_left = time_join
  for event in time_left_events {
    time_join_with_left = add_left_event(time_join_with_left, event)
  }
  
  let mut time_join_with_both = time_join_with_left
  for event in time_right_events {
    time_join_with_both = add_right_event(time_join_with_both, event)
  }
  
  # Perform time join
  let time_joined_result = perform_join(time_join_with_both)
  
  # Verify time join results
  assert_eq(time_joined_result.joined_events.length(), 2)  # Both pairs should join (within 15-second window)
  
  # Verify specific time joins
  let time_join1 = time_joined_result.joined_events.filter(fn(j) { j.left_event.event_id == "time-span-1" })[0]
  assert_eq(time_join1.right_event.event_id, "time-log-1")
  assert_eq(time_join1.right_event.timestamp - time_join1.left_event.timestamp, 10)  # 10 seconds difference
  
  let time_join2 = time_joined_result.joined_events.filter(fn(j) { j.left_event.event_id == "time-span-2" })[0]
  assert_eq(time_join2.right_event.event_id, "time-log-2")
  assert_eq(time_join2.right_event.timestamp - time_join2.left_event.timestamp, 20)  # 20 seconds difference
}

// Test 5: Stream State Management
test "stream state management and persistence" {
  // Define stream state
  type StreamState = {
    state_id: String,
    key_value_store: Array<(String, String)>,
    counters: Array<(String, Int)>,
    last_updated: Int,
    version: Int
  }
  
  // Define state manager
  type StateManager = {
    states: Array<StreamState>,
    checkpoint_interval_seconds: Int,
    last_checkpoint_time: Int,
    checkpoints_created: Int
  }
  
  // Create state manager
  let create_state_manager = fn(checkpoint_interval: Int) {
    {
      states: [],
      checkpoint_interval_seconds: checkpoint_interval,
      last_checkpoint_time: 0,
      checkpoints_created: 0
    }
  }
  
  # Get or create state
  let get_or_create_state = fn(manager: StateManager, state_id: String) {
    let existing_state = manager.states.filter(fn(s) { s.state_id == state_id })
    
    match existing_state {
      [state] => state
      _ => {
        {
          state_id: state_id,
          key_value_store: [],
          counters: [],
          last_updated: 1640995200,
          version: 1
        }
      }
    }
  }
  
  # Update state
  let update_state = fn(manager: StateManager, state_id: String, key: String, value: String) {
    let state = get_or_create_state(manager, state_id)
    
    # Update key-value store
    let mut updated_kv = state.key_value_store
    let mut found = false
    
    for i in 0..updated_kv.length() {
      if updated_kv[i][0] == key {
        updated_kv[i] = (key, value)
        found = true
        break
      }
    }
    
    if not(found) {
      updated_kv = updated_kv.push((key, value))
    }
    
    let updated_state = {
      state_id: state.state_id,
      key_value_store: updated_kv,
      counters: state.counters,
      last_updated: 1640995300,
      version: state.version + 1
    }
    
    # Update manager
    let mut updated_states = manager.states
    for i in 0..updated_states.length() {
      if updated_states[i].state_id == state_id {
        updated_states[i] = updated_state
        break
      }
    }
    
    # Add new state if not found
    if not(found) {
      updated_states = updated_states.push(updated_state)
    }
    
    {
      states: updated_states,
      checkpoint_interval_seconds: manager.checkpoint_interval_seconds,
      last_checkpoint_time: manager.last_checkpoint_time,
      checkpoints_created: manager.checkpoints_created
    }
  }
  
  # Increment counter
  let increment_counter = fn(manager: StateManager, state_id: String, counter: String, increment: Int) {
    let state = get_or_create_state(manager, state_id)
    
    # Update counters
    let mut updated_counters = state.counters
    let mut found = false
    
    for i in 0..updated_counters.length() {
      if updated_counters[i][0] == counter {
        let current_value = updated_counters[i][1]
        updated_counters[i] = (counter, current_value + increment)
        found = true
        break
      }
    }
    
    if not(found) {
      updated_counters = updated_counters.push((counter, increment))
    }
    
    let updated_state = {
      state_id: state.state_id,
      key_value_store: state.key_value_store,
      counters: updated_counters,
      last_updated: 1640995300,
      version: state.version + 1
    }
    
    # Update manager
    let mut updated_states = manager.states
    for i in 0..updated_states.length() {
      if updated_states[i].state_id == state_id {
        updated_states[i] = updated_state
        break
      }
    }
    
    # Add new state if not found
    if not(found) {
      updated_states = updated_states.push(updated_state)
    }
    
    {
      states: updated_states,
      checkpoint_interval_seconds: manager.checkpoint_interval_seconds,
      last_checkpoint_time: manager.last_checkpoint_time,
      checkpoints_created: manager.checkpoints_created
    }
  }
  
  # Create checkpoint
  let create_checkpoint = fn(manager: StateManager, current_time: Int) {
    let time_since_last_checkpoint = current_time - manager.last_checkpoint_time
    
    if time_since_last_checkpoint >= manager.checkpoint_interval_seconds {
      # Create checkpoint (in real implementation, would persist to disk)
      {
        states: manager.states,
        checkpoint_interval_seconds: manager.checkpoint_interval_seconds,
        last_checkpoint_time: current_time,
        checkpoints_created: manager.checkpoints_created + 1
      }
    } else {
      manager
    }
  }
  
  # Test state management
  let manager = create_state_manager(60)  # Checkpoint every 60 seconds
  
  # Update state
  let manager1 = update_state(manager, "service-1", "last_request_id", "req-123")
  let manager2 = update_state(manager1, "service-1", "last_request_time", "1640995200")
  let manager3 = update_state(manager2, "service-2", "last_request_id", "req-456")
  
  # Increment counters
  let manager4 = increment_counter(manager3, "service-1", "request_count", 1)
  let manager5 = increment_counter(manager4, "service-1", "request_count", 1)
  let manager6 = increment_counter(manager5, "service-2", "request_count", 1)
  
  # Verify states
  assert_eq(manager6.states.length(), 2)
  
  let service1_state = manager6.states.filter(fn(s) { s.state_id == "service-1" })[0]
  assert_eq(service1_state.key_value_store.length(), 2)
  assert_eq(service1_state.counters.length(), 1)
  assert_eq(service1_state.version, 4)  # 2 updates + 2 counter increments
  
  let service2_state = manager6.states.filter(fn(s) { s.state_id == "service-2" })[0]
  assert_eq(service2_state.key_value_store.length(), 1)
  assert_eq(service2_state.counters.length(), 1)
  assert_eq(service2_state.version, 2)  # 1 update + 1 counter increment
  
  # Verify specific values
  let service1_last_request = service1_state.key_value_store.filter(fn(kv) { kv[0] == "last_request_id" })[0]
  assert_eq(service1_last_request[1], "req-123")
  
  let service1_request_count = service1_state.counters.filter(fn(c) { c[0] == "request_count" })[0]
  assert_eq(service1_request_count[1], 2)
  
  # Test checkpoint creation
  let manager7 = create_checkpoint(manager6, 1640995260)  # 60 seconds after last checkpoint
  assert_eq(manager7.checkpoints_created, 1)
  assert_eq(manager7.last_checkpoint_time, 1640995260)
  
  # Test checkpoint not created if interval not passed
  let manager8 = create_checkpoint(manager7, 1640995290)  # Only 30 seconds after last checkpoint
  assert_eq(manager8.checkpoints_created, 1)  # No new checkpoint
  assert_eq(manager8.last_checkpoint_time, 1640995260)
  
  # Test state recovery (simulation)
  let recover_from_checkpoint = fn(checkpoint_time: Int) {
    # In real implementation, would load from disk
    {
      states: [
        {
          state_id: "service-1",
          key_value_store: [
            ("last_request_id", "req-123"),
            ("last_request_time", "1640995200")
          ],
          counters: [
            ("request_count", 2)
          ],
          last_updated: 1640995300,
          version: 4
        },
        {
          state_id: "service-2",
          key_value_store: [
            ("last_request_id", "req-456")
          ],
          counters: [
            ("request_count", 1)
          ],
          last_updated: 1640995300,
          version: 2
        }
      ],
      checkpoint_interval_seconds: 60,
      last_checkpoint_time: checkpoint_time,
      checkpoints_created: 1
    }
  }
  
  let recovered_manager = recover_from_checkpoint(1640995260)
  assert_eq(recovered_manager.states.length(), 2)
  assert_eq(recovered_manager.checkpoints_created, 1)
  assert_eq(recovered_manager.last_checkpoint_time, 1640995260)
  
  # Verify recovered state
  let recovered_service1 = recovered_manager.states.filter(fn(s) { s.state_id == "service-1" })[0]
  assert_eq(recovered_service1.version, 4)
  assert_eq(recovered_service1.key_value_store.length(), 2)
  assert_eq(recovered_service1.counters.length(), 1)
}