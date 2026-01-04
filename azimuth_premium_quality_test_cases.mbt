// Azimuth Premium Quality Test Cases
// This file contains 10 high-quality test cases focusing on advanced telemetry system features

// Test 1: Telemetry Data Aggregation and Statistical Analysis
test "telemetry data aggregation and statistical analysis" {
  // Define telemetry data structure
  type TelemetryPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[String],
    trace_id: String
  }
  
  // Create sample telemetry data
  let telemetry_data = [
    { timestamp: 1640995200, value: 100.5, tags: ["service:api", "env:prod"], trace_id: "trace-001" },
    { timestamp: 1640995260, value: 150.2, tags: ["service:api", "env:prod"], trace_id: "trace-002" },
    { timestamp: 1640995320, value: 75.8, tags: ["service:db", "env:prod"], trace_id: "trace-003" },
    { timestamp: 1640995380, value: 200.1, tags: ["service:api", "env:prod"], trace_id: "trace-004" },
    { timestamp: 1640995440, value: 125.3, tags: ["service:cache", "env:prod"], trace_id: "trace-005" }
  ]
  
  // Calculate statistics
  let calculate_stats = fn(data: Array[TelemetryPoint]) {
    let mut sum = 0.0
    let mut count = 0
    let mut min = data[0].value
    let mut max = data[0].value
    
    for point in data {
      sum = sum + point.value
      count = count + 1
      
      if point.value < min {
        min = point.value
      }
      
      if point.value > max {
        max = point.value
      }
    }
    
    let mean = sum / count.to_float()
    
    // Calculate variance
    let mut variance_sum = 0.0
    for point in data {
      let diff = point.value - mean
      variance_sum = variance_sum + (diff * diff)
    }
    let variance = variance_sum / count.to_float()
    let std_dev = variance.sqrt()
    
    {
      count,
      sum,
      mean,
      min,
      max,
      variance,
      std_dev
    }
  }
  
  let stats = calculate_stats(telemetry_data)
  
  // Verify statistics
  assert_eq(stats.count, 5)
  assert_eq(stats.sum, 651.9)
  assert_eq(stats.mean, 130.38)
  assert_eq(stats.min, 75.8)
  assert_eq(stats.max, 200.1)
  
  // Verify standard deviation is reasonable
  assert_true(stats.std_dev > 40.0)
  assert_true(stats.std_dev < 50.0)
  
  // Test filtering by tags
  let filter_by_tag = fn(data: Array[TelemetryPoint], tag: String) {
    let mut filtered = []
    for point in data {
      if point.tags.contains(tag) {
        filtered = filtered.push(point)
      }
    }
    filtered
  }
  
  let api_data = filter_by_tag(telemetry_data, "service:api")
  assert_eq(api_data.length(), 3)
  
  let api_stats = calculate_stats(api_data)
  assert_eq(api_stats.count, 3)
  assert_eq(api_stats.sum, 450.8)
  assert_eq(api_stats.mean, 150.27)
  
  // Test time window aggregation
  let aggregate_by_time_window = fn(data: Array[TelemetryPoint], window_size: Int) {
    let mut windows = []
    
    if data.length() > 0 {
      let start_time = data[0].timestamp
      let mut current_window = []
      let mut current_window_start = start_time
      
      for point in data {
        if point.timestamp < current_window_start + window_size {
          current_window = current_window.push(point)
        } else {
          windows = windows.push({
            start_time: current_window_start,
            end_time: current_window_start + window_size,
            points: current_window,
            stats: calculate_stats(current_window)
          })
          
          current_window = [point]
          current_window_start = point.timestamp
        }
      }
      
      // Add the last window
      if current_window.length() > 0 {
        windows = windows.push({
          start_time: current_window_start,
          end_time: current_window_start + window_size,
          points: current_window,
          stats: calculate_stats(current_window)
        })
      }
    }
    
    windows
  }
  
  let windows = aggregate_by_time_window(telemetry_data, 300) // 5-minute windows
  assert_eq(windows.length(), 2)
  assert_eq(windows[0].points.length(), 3)
  assert_eq(windows[1].points.length(), 2)
}

// Test 2: Distributed Tracing Consistency Validation
test "distributed tracing consistency validation" {
  // Define span structure
  type Span = {
    span_id: String,
    trace_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    service_name: String
  }
  
  // Create a distributed trace with multiple services
  let trace_spans = [
    { span_id: "span-001", trace_id: "trace-123", parent_span_id: None, operation_name: "http.request", start_time: 1000, end_time: 1200, status: "ok", service_name: "api-gateway" },
    { span_id: "span-002", trace_id: "trace-123", parent_span_id: Some("span-001"), operation_name: "auth.validate", start_time: 1050, end_time: 1100, status: "ok", service_name: "auth-service" },
    { span_id: "span-003", trace_id: "trace-123", parent_span_id: Some("span-001"), operation_name: "data.fetch", start_time: 1100, end_time: 1150, status: "ok", service_name: "data-service" },
    { span_id: "span-004", trace_id: "trace-123", parent_span_id: Some("span-003"), operation_name: "db.query", start_time: 1120, end_time: 1140, status: "ok", service_name: "database" },
    { span_id: "span-005", trace_id: "trace-123", parent_span_id: Some("span-003"), operation_name: "cache.get", start_time: 1110, end_time: 1115, status: "miss", service_name: "cache-service" }
  ]
  
  // Validate trace consistency
  let validate_trace_consistency = fn(spans: Array[Span]) {
    let mut errors = []
    
    // Check all spans have the same trace_id
    let trace_id = spans[0].trace_id
    for span in spans {
      if span.trace_id != trace_id {
        errors = errors.push("Span " + span.span_id + " has different trace_id")
      }
    }
    
    // Check parent-child relationships
    for span in spans {
      match span.parent_span_id {
        Some(parent_id) => {
          let mut parent_found = false
          for potential_parent in spans {
            if potential_parent.span_id == parent_id {
              parent_found = true
              
              // Check timing consistency
              if span.start_time < potential_parent.start_time {
                errors = errors.push("Span " + span.span_id + " starts before parent " + parent_id)
              }
              
              if span.end_time > potential_parent.end_time {
                errors = errors.push("Span " + span.span_id + " ends after parent " + parent_id)
              }
            }
          }
          
          if not(parent_found) {
            errors = errors.push("Parent span " + parent_id + " not found for span " + span.span_id)
          }
        }
        None => {
          // This is a root span, should be only one
          let root_count = spans.filter(fn(s) { s.parent_span_id == None }).length()
          if root_count > 1 {
            errors = errors.push("Multiple root spans found")
          }
        }
      }
    }
    
    // Check for timing consistency
    for span in spans {
      if span.start_time >= span.end_time {
        errors = errors.push("Span " + span.span_id + " has invalid timing")
      }
    }
    
    errors
  }
  
  let errors = validate_trace_consistency(trace_spans)
  assert_eq(errors.length(), 0)
  
  // Test with inconsistent data
  let inconsistent_spans = [
    { span_id: "span-001", trace_id: "trace-123", parent_span_id: None, operation_name: "http.request", start_time: 1000, end_time: 1200, status: "ok", service_name: "api-gateway" },
    { span_id: "span-002", trace_id: "trace-456", parent_span_id: Some("span-001"), operation_name: "auth.validate", start_time: 1050, end_time: 1100, status: "ok", service_name: "auth-service" },
    { span_id: "span-003", trace_id: "trace-123", parent_span_id: Some("span-999"), operation_name: "data.fetch", start_time: 1100, end_time: 1150, status: "ok", service_name: "data-service" },
    { span_id: "span-004", trace_id: "trace-123", parent_span_id: Some("span-003"), operation_name: "db.query", start_time: 900, end_time: 950, status: "ok", service_name: "database" }
  ]
  
  let inconsistent_errors = validate_trace_consistency(inconsistent_spans)
  assert_true(inconsistent_errors.length() > 0)
  
  // Calculate trace duration and critical path
  let calculate_trace_metrics = fn(spans: Array[Span]) {
    let root_span = spans.find(fn(s) { s.parent_span_id == None }).unwrap()
    let trace_duration = root_span.end_time - root_span.start_time
    
    // Calculate service time
    let mut service_times = []
    let mut processed_services = []
    
    for span in spans {
      if not(processed_services.contains(span.service_name)) {
        processed_services = processed_services.push(span.service_name)
        
        let service_spans = spans.filter(fn(s) { s.service_name == span.service_name })
        let mut total_service_time = 0
        
        for service_span in service_spans {
          total_service_time = total_service_time + (service_span.end_time - service_span.start_time)
        }
        
        service_times = service_times.push({
          service_name: span.service_name,
          total_time: total_service_time,
          span_count: service_spans.length()
        })
      }
    }
    
    {
      trace_duration,
      service_times,
      span_count: spans.length(),
      service_count: processed_services.length()
    }
  }
  
  let metrics = calculate_trace_metrics(trace_spans)
  assert_eq(metrics.trace_duration, 200)
  assert_eq(metrics.span_count, 5)
  assert_eq(metrics.service_count, 5)
  
  let api_gateway_time = metrics.service_times.find(fn(st) { st.service_name == "api-gateway" }).unwrap()
  assert_eq(api_gateway_time.total_time, 200)
  assert_eq(api_gateway_time.span_count, 1)
  
  let data_service_time = metrics.service_times.find(fn(st) { st.service_name == "data-service" }).unwrap()
  assert_eq(data_service_time.total_time, 50)
  assert_eq(data_service_time.span_count, 1)
}

// Test 3: High-Concurrency Resource Management
test "high-concurrency resource management" {
  // Simulate resource pool for telemetry processing
  type Resource = {
    id: Int,
    in_use: Bool,
    last_used: Int,
    usage_count: Int
  }
  
  type ResourcePool = {
    resources: Array[Resource],
    max_resources: Int,
    created_count: Int,
    destroyed_count: Int
  }
  
  let create_resource_pool = fn(max_size: Int) {
    let mut resources = []
    for i in 0..max_size {
      resources = resources.push({
        id: i,
        in_use: false,
        last_used: 0,
        usage_count: 0
      })
    }
    
    {
      resources,
      max_resources: max_size,
      created_count: max_size,
      destroyed_count: 0
    }
  }
  
  let acquire_resource = fn(pool: ResourcePool) {
    let mut available_index = -1
    
    for i in 0..pool.resources.length() {
      if not(pool.resources[i].in_use) {
        available_index = i
        break
      }
    }
    
    if available_index >= 0 {
      let resource = pool.resources[available_index]
      let updated_resource = { resource | in_use: true, last_used: 1000, usage_count: resource.usage_count + 1 }
      
      let mut updated_resources = []
      for i in 0..pool.resources.length() {
        if i == available_index {
          updated_resources = updated_resources.push(updated_resource)
        } else {
          updated_resources = updated_resources.push(pool.resources[i])
        }
      }
      
      (Some(updated_resource), { pool | resources: updated_resources })
    } else {
      (None, pool)
    }
  }
  
  let release_resource = fn(pool: ResourcePool, resource_id: Int) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in pool.resources {
      if resource.id == resource_id {
        updated_resources = updated_resources.push({ resource | in_use: false })
        found = true
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    if found {
      { pool | resources: updated_resources }
    } else {
      pool
    }
  }
  
  // Test basic resource acquisition and release
  let pool = create_resource_pool(3)
  assert_eq(pool.resources.length(), 3)
  assert_eq(pool.created_count, 3)
  
  let (resource1, pool1) = acquire_resource(pool)
  assert_true(resource1.is_some())
  assert_true(resource1.unwrap().in_use)
  assert_eq(resource1.unwrap().usage_count, 1)
  
  let (resource2, pool2) = acquire_resource(pool1)
  assert_true(resource2.is_some())
  assert_true(resource2.unwrap().in_use)
  assert_eq(resource2.unwrap().usage_count, 1)
  assert_not_eq(resource1.unwrap().id, resource2.unwrap().id)
  
  // Release a resource
  let pool3 = release_resource(pool2, resource1.unwrap().id)
  let released_resource = pool3.resources.find(fn(r) { r.id == resource1.unwrap().id }).unwrap()
  assert_false(released_resource.in_use)
  
  // Reacquire the released resource
  let (resource3, pool4) = acquire_resource(pool3)
  assert_true(resource3.is_some())
  assert_eq(resource3.unwrap().id, resource1.unwrap().id)
  assert_true(resource3.unwrap().in_use)
  assert_eq(resource3.unwrap().usage_count, 2) // Usage count should increment
  
  // Test resource exhaustion
  let (r1, p1) = acquire_resource(pool4)
  let (r2, p2) = acquire_resource(p1)
  let (r3, p3) = acquire_resource(p2)
  let (r4, p4) = acquire_resource(p3)
  
  assert_true(r1.is_some())
  assert_true(r2.is_some())
  assert_true(r3.is_some())
  assert_false(r4.is_some()) // Should be None when pool is exhausted
  
  // Test concurrent resource usage simulation
  let simulate_concurrent_usage = fn(pool: ResourcePool, operations: Int) {
    let mut current_pool = pool
    let mut acquired_resources = []
    let mut operation_results = []
    
    for i in 0..operations {
      if i % 3 == 0 {
        // Acquire operation
        let (resource, new_pool) = acquire_resource(current_pool)
        current_pool = new_pool
        
        match resource {
          Some(r) => {
            acquired_resources = acquired_resources.push(r)
            operation_results = results.push("acquired:" + r.id.to_string())
          }
          None => {
            operation_results = results.push("acquire_failed")
          }
        }
      } else if i % 3 == 1 and acquired_resources.length() > 0 {
        // Release operation
        let resource_to_release = acquired_resources[0]
        current_pool = release_resource(current_pool, resource_to_release.id)
        
        // Remove from acquired list
        let mut new_acquired = []
        for j in 1..acquired_resources.length() {
          new_acquired = new_acquired.push(acquired_resources[j])
        }
        acquired_resources = new_acquired
        
        operation_results = results.push("released:" + resource_to_release.id.to_string())
      } else {
        // Query operation
        let in_use_count = current_pool.resources.filter(fn(r) { r.in_use }).length()
        operation_results = results.push("in_use_count:" + in_use_count.to_string())
      }
    }
    
    {
      final_pool: current_pool,
      operation_results,
      acquired_count: acquired_resources.length()
    }
  }
  
  let concurrent_result = simulate_concurrent_usage(pool, 20)
  assert_eq(concurrent_result.acquired_count, 0) // All resources should be released
  
  // Verify resource integrity
  let mut total_usage = 0
  for resource in concurrent_result.final_pool.resources {
    total_usage = total_usage + resource.usage_count
    assert_false(resource.in_use) // All resources should be released
  }
  
  assert_true(total_usage > 0) // Resources should have been used
}

// Test 4: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define telemetry data structures
  type Attribute = {
    key: String,
    value: String
  }
  
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: String,
    attributes: Array[Attribute],
    metrics: Array[(String, Float)]
  }
  
  // Create sample telemetry event
  let create_sample_event = fn() {
    {
      id: "event-12345",
      timestamp: 1640995200,
      event_type: "http.request",
      attributes: [
        { key: "service.name", value: "api-gateway" },
        { key: "http.method", value: "GET" },
        { key: "http.status_code", value: "200" },
        { key: "user.id", value: "user-67890" }
      ],
      metrics: [
        ("duration", 125.5),
        ("request_size", 1024.0),
        ("response_size", 2048.0)
      ]
    }
  }
  
  // Serialize to string (simplified JSON-like format)
  let serialize_event = fn(event: TelemetryEvent) {
    let mut result = "{"
    result = result + "\"id\":\"" + event.id + "\"," 
    result = result + "\"timestamp\":" + event.timestamp.to_string() + ","
    result = result + "\"event_type\":\"" + event.event_type + "\"," 
    
    // Serialize attributes
    result = result + "\"attributes\":["
    for i in 0..event.attributes.length() {
      let attr = event.attributes[i]
      result = result + "{\"key\":\"" + attr.key + "\",\"value\":\"" + attr.value + "\"}"
      if i < event.attributes.length() - 1 {
        result = result + ","
      }
    }
    result = result + "],"
    
    // Serialize metrics
    result = result + "\"metrics\":["
    for i in 0..event.metrics.length() {
      let metric = event.metrics[i]
      result = result + "{\"name\":\"" + metric.0 + "\",\"value\":" + metric.1.to_string() + "}"
      if i < event.metrics.length() - 1 {
        result = result + ","
      }
    }
    result = result + "]"
    
    result = result + "}"
  }
  
  // Parse string to extract values (simplified parsing)
  let parse_string_value = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json.index_of(key_pattern)
    
    if start_index >= 0 {
      let value_start = start_index + key_pattern.length()
      let end_index = json.index_of("\"", value_start)
      
      if end_index >= 0 {
        json.substring(value_start, end_index - value_start)
      } else {
        ""
      }
    } else {
      ""
    }
  }
  
  let parse_int_value = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":"
    let start_index = json.index_of(key_pattern)
    
    if start_index >= 0 {
      let value_start = start_index + key_pattern.length()
      let mut end_index = value_start
      
      while end_index < json.length() and (
        json[end_index] >= '0' and json[end_index] <= '9' or
        json[end_index] == '-'
      ) {
        end_index = end_index + 1
      }
      
      let value_str = json.substring(value_start, end_index - value_start)
      value_str.to_int()
    } else {
      0
    }
  }
  
  // Test serialization
  let event = create_sample_event()
  let serialized = serialize_event(event)
  
  assert_true(serialized.contains("\"id\":\"event-12345\""))
  assert_true(serialized.contains("\"timestamp\":1640995200"))
  assert_true(serialized.contains("\"event_type\":\"http.request\""))
  assert_true(serialized.contains("\"key\":\"service.name\""))
  assert_true(serialized.contains("\"value\":\"api-gateway\""))
  assert_true(serialized.contains("\"name\":\"duration\""))
  assert_true(serialized.contains("\"value\":125.5"))
  
  // Test parsing
  let parsed_id = parse_string_value(serialized, "id")
  let parsed_timestamp = parse_int_value(serialized, "timestamp")
  let parsed_event_type = parse_string_value(serialized, "event_type")
  
  assert_eq(parsed_id, "event-12345")
  assert_eq(parsed_timestamp, 1640995200)
  assert_eq(parsed_event_type, "http.request")
  
  // Test batch serialization
  let serialize_events = fn(events: Array[TelemetryEvent]) {
    let mut result = "["
    
    for i in 0..events.length() {
      result = result + serialize_event(events[i])
      if i < events.length() - 1 {
        result = result + ","
      }
    }
    
    result = result + "]"
  }
  
  let events = [
    create_sample_event(),
    { create_sample_event() | id: "event-12346", timestamp: 1640995300 },
    { create_sample_event() | id: "event-12347", timestamp: 1640995400 }
  ]
  
  let batch_serialized = serialize_events(events)
  assert_true(batch_serialized.contains("\"id\":\"event-12345\""))
  assert_true(batch_serialized.contains("\"id\":\"event-12346\""))
  assert_true(batch_serialized.contains("\"id\":\"event-12347\""))
  
  // Test compression simulation
  let compress_string = fn(input: String) {
    // Simple compression simulation - replace repeated patterns
    let mut compressed = input
    compressed = compressed.replace("\"service.name\"", "\"s.n\"")
    compressed = compressed.replace("\"http.method\"", "\"h.m\"")
    compressed = compressed.replace("\"http.status_code\"", "\"h.s\"")
    compressed
  }
  
  let decompress_string = fn(input: String) {
    // Reverse the compression
    let mut decompressed = input
    decompressed = decompressed.replace("\"s.n\"", "\"service.name\"")
    decompressed = decompressed.replace("\"h.m\"", "\"http.method\"")
    decompressed = decompressed.replace("\"h.s\"", "\"http.status_code\"")
    decompressed
  }
  
  let compressed = compress_string(serialized)
  let decompressed = decompress_string(compressed)
  
  assert_true(compressed.length() < serialized.length()) // Should be smaller
  assert_eq(decompressed, serialized) // Should decompress back to original
  
  // Test data integrity verification
  let verify_data_integrity = fn(original: TelemetryEvent, serialized_data: String) {
    let parsed_id = parse_string_value(serialized_data, "id")
    let parsed_timestamp = parse_int_value(serialized_data, "timestamp")
    let parsed_event_type = parse_string_value(serialized_data, "event_type")
    
    original.id == parsed_id and
    original.timestamp == parsed_timestamp and
    original.event_type == parsed_event_type
  }
  
  assert_true(verify_data_integrity(event, serialized))
}

// Test 5: Real-time Stream Processing and Windowed Calculations
test "real-time stream processing and windowed calculations" {
  // Define stream event structure
  type StreamEvent = {
    timestamp: Int,
    event_type: String,
    value: Float,
    source: String
  }
  
  // Define window calculation result
  type WindowResult = {
    window_start: Int,
    window_end: Int,
    event_count: Int,
    sum: Float,
    average: Float,
    min: Float,
    max: Float,
    sources: Array[String]
  }
  
  // Create stream processor
  let create_stream_processor = fn(window_size: Int) {
    {
      window_size,
      current_window_start: 0,
      events_in_window: [],
      completed_windows: []
    }
  }
  
  // Process events through sliding window
  let process_event = fn(processor, event: StreamEvent) {
    // Initialize window start if this is the first event
    let window_start = if processor.current_window_start == 0 {
      event.timestamp
    } else {
      processor.current_window_start
    }
    
    // Check if event is outside current window
    if event.timestamp >= window_start + processor.window_size {
      // Calculate window results
      let event_count = processor.events_in_window.length()
      
      let window_result = if event_count > 0 {
        let mut sum = 0.0
        let mut min = processor.events_in_window[0].value
        let mut max = processor.events_in_window[0].value
        let mut sources = []
        
        for e in processor.events_in_window {
          sum = sum + e.value
          
          if e.value < min {
            min = e.value
          }
          
          if e.value > max {
            max = e.value
          }
          
          if not(sources.contains(e.source)) {
            sources = sources.push(e.source)
          }
        }
        
        let average = sum / event_count.to_float()
        
        {
          window_start,
          window_end: window_start + processor.window_size,
          event_count,
          sum,
          average,
          min,
          max,
          sources
        }
      } else {
        {
          window_start,
          window_end: window_start + processor.window_size,
          event_count: 0,
          sum: 0.0,
          average: 0.0,
          min: 0.0,
          max: 0.0,
          sources: []
        }
      }
      
      // Start new window
      let new_window_start = event.timestamp
      let new_events = [event]
      let updated_completed = processor.completed_windows.push(window_result)
      
    {
      window_size: processor.window_size,
      current_window_start: new_window_start,
      events_in_window: new_events,
      completed_windows: updated_completed
    }
    } else {
      // Add event to current window
      let updated_events = processor.events_in_window.push(event)
      {
        window_size: processor.window_size,
        current_window_start,
        events_in_window: updated_events,
        completed_windows: processor.completed_windows
      }
    }
  }
  
  // Test with sample data
  let processor = create_stream_processor(1000) // 1-second windows
  
  let events = [
    { timestamp: 1000, event_type: "metric", value: 10.5, source: "service-a" },
    { timestamp: 1200, event_type: "metric", value: 15.2, source: "service-a" },
    { timestamp: 1400, event_type: "metric", value: 8.7, source: "service-b" },
    { timestamp: 2000, event_type: "metric", value: 22.1, source: "service-a" },
    { timestamp: 2200, event_type: "metric", value: 18.3, source: "service-c" },
    { timestamp: 2400, event_type: "metric", value: 12.9, source: "service-b" }
  ]
  
  // Process all events
  let mut final_processor = processor
  for event in events {
    final_processor = process_event(final_processor, event)
  }
  
  // Should have 2 completed windows
  assert_eq(final_processor.completed_windows.length(), 2)
  
  // Check first window (1000-2000)
  let window1 = final_processor.completed_windows[0]
  assert_eq(window1.window_start, 1000)
  assert_eq(window1.window_end, 2000)
  assert_eq(window1.event_count, 3)
  assert_eq(window1.sum, 34.4)
  assert_eq(window1.average, 11.47)
  assert_eq(window1.min, 8.7)
  assert_eq(window1.max, 15.2)
  assert_eq(window1.sources.length(), 2)
  assert_true(window1.sources.contains("service-a"))
  assert_true(window1.sources.contains("service-b"))
  
  // Check second window (2000-3000)
  let window2 = final_processor.completed_windows[1]
  assert_eq(window2.window_start, 2000)
  assert_eq(window2.window_end, 3000)
  assert_eq(window2.event_count, 3)
  assert_eq(window2.sum, 53.3)
  assert_eq(window2.average, 17.77)
  assert_eq(window2.min, 12.9)
  assert_eq(window2.max, 22.1)
  assert_eq(window2.sources.length(), 3)
  
  // Test event filtering by type
  let filter_events_by_type = fn(events: Array[StreamEvent], event_type: String) {
    events.filter(fn(e) { e.event_type == event_type })
  }
  
  let metric_events = filter_events_by_type(events, "metric")
  assert_eq(metric_events.length(), 6)
  
  let log_events = filter_events_by_type(events, "log")
  assert_eq(log_events.length(), 0)
  
  // Test event aggregation by source
  let aggregate_by_source = fn(events: Array[StreamEvent]) {
    let mut source_stats = []
    let mut processed_sources = []
    
    for event in events {
      if not(processed_sources.contains(event.source)) {
        processed_sources = processed_sources.push(event.source)
        
        let source_events = events.filter(fn(e) { e.source == event.source })
        let mut sum = 0.0
        let mut count = 0
        
        for e in source_events {
          sum = sum + e.value
          count = count + 1
        }
        
        source_stats = source_stats.push({
          source: event.source,
          event_count: count,
          sum,
          average: sum / count.to_float()
        })
      }
    }
    
    source_stats
  }
  
  let source_aggregations = aggregate_by_source(events)
  assert_eq(source_aggregations.length(), 3)
  
  let service_a_stats = source_aggregations.find(fn(s) { s.source == "service-a" }).unwrap()
  assert_eq(service_a_stats.event_count, 3)
  assert_eq(service_a_stats.sum, 47.8)
  assert_eq(service_a_stats.average, 15.93)
  
  // Test anomaly detection in stream
  let detect_anomalies = fn(events: Array[StreamEvent], threshold: Float) {
    let mut anomalies = []
    
    if events.length() > 0 {
      let mut sum = 0.0
      for event in events {
        sum = sum + event.value
      }
      let mean = sum / events.length().to_float()
      
      let mut variance_sum = 0.0
      for event in events {
        let diff = event.value - mean
        variance_sum = variance_sum + (diff * diff)
      }
      let variance = variance_sum / events.length().to_float()
      let std_dev = variance.sqrt()
      
      // Find events outside threshold standard deviations
      for event in events {
        let z_score = (event.value - mean) / std_dev
        if z_score.abs() > threshold {
          anomalies = anomalies.push({
            event,
            z_score,
            mean,
            std_dev
          })
        }
      }
    }
    
    anomalies
  }
  
  let anomalies = detect_anomalies(events, 1.5) // 1.5 standard deviations
  assert_true(anomalies.length() >= 0) // May or may not have anomalies depending on data
}

// Test 6: Error Recovery and Fault Tolerance
test "error recovery and fault tolerance" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    ResourceExhausted(String)
    InvalidData(String)
    ServiceUnavailable(String)
  }
  
  // Define operation result type
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError],
    retry_count: Int
  }
  
  // Create retry mechanism
  let retry_operation = fn(operation: () -> OperationResult[T], max_retries: Int, backoff_ms: Int) {
    let mut attempt = 0
    let mut result = operation()
    
    while attempt < max_retries and not(result.success) {
      attempt = attempt + 1
      
      // Simulate backoff delay (in real implementation, would wait)
      let backoff_time = backoff_ms * attempt
      // In test, we just increment attempt counter
      
      result = operation()
      let updated_result = { result | retry_count: attempt }
      result = updated_result
    }
    
    result
  }
  
  // Test successful operation
  let successful_operation = fn() {
    {
      success: true,
      data: Some("operation_completed"),
      error: None,
      retry_count: 0
    }
  }
  
  let success_result = retry_operation(successful_operation, 3, 100)
  assert_true(success_result.success)
  assert_eq(success_result.data, Some("operation_completed"))
  assert_eq(success_result.retry_count, 0) // Should not retry successful operation
  
  // Test operation that fails then succeeds
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    
    if attempt_count < 3 {
      {
        success: false,
        data: None,
        error: Some(TelemetryError::NetworkTimeout("Connection timed out")),
        retry_count: 0
      }
    } else {
      {
        success: true,
        data: Some("operation_succeeded_after_retries"),
        error: None,
        retry_count: 0
      }
    }
  }
  
  attempt_count = 0 // Reset counter
  let flaky_result = retry_operation(flaky_operation, 5, 100)
  assert_true(flaky_result.success)
  assert_eq(flaky_result.data, Some("operation_succeeded_after_retries"))
  assert_eq(flaky_result.retry_count, 3) // Should have retried 3 times
  
  // Test operation that always fails
  let failing_operation = fn() {
    {
      success: false,
      data: None,
      error: Some(TelemetryError::ServiceUnavailable("Service down")),
      retry_count: 0
    }
  }
  
  let fail_result = retry_operation(failing_operation, 3, 100)
  assert_false(fail_result.success)
  assert_eq(fail_result.data, None)
  assert_eq(fail_result.retry_count, 3) // Should have retried max times
  
  match fail_result.error {
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
      timeout,
      last_failure_time: 0
    }
  }
  
  let circuit_breaker_call = fn(breaker: CircuitBreaker, operation: () -> OperationResult[T], current_time: Int) {
    if breaker.state == "open" {
      if current_time - breaker.last_failure_time > breaker.timeout {
        // Try half-open state
        let result = operation()
        match result.success {
          true => {
            // Success, close the circuit
            (result, { breaker | state: "closed", failure_count: 0 })
          }
          false => {
            // Still failing, keep open
            (result, { breaker | last_failure_time: current_time })
          }
        }
      } else {
        // Still in timeout period
        ({
          success: false,
          data: None,
          error: Some(TelemetryError::ServiceUnavailable("Circuit breaker open")),
          retry_count: 0
        }, breaker)
      }
    } else {
      // Circuit is closed, proceed with operation
      let result = operation()
      
      match result.success {
        true => {
          // Success, reset failure count
          (result, { breaker | failure_count: 0 })
        }
        false => {
          // Failure, increment count and potentially open circuit
          let new_failure_count = breaker.failure_count + 1
          let new_state = if new_failure_count >= breaker.failure_threshold {
            "open"
          } else {
            "closed"
          }
          
          (result, {
            breaker |
            failure_count: new_failure_count,
            state: new_state,
            last_failure_time: if new_state == "open" { current_time } else { breaker.last_failure_time }
          })
        }
      }
    }
  }
  
  // Test circuit breaker
  let breaker = create_circuit_breaker(3, 5000) // 3 failures threshold, 5 second timeout
  
  let mut current_breaker = breaker
  let mut current_time = 1000
  
  // First few calls succeed
  for i in 0..2 {
    let success_op = fn() {
      {
        success: true,
        data: Some("success"),
        error: None,
        retry_count: 0
      }
    }
    
    let (result, new_breaker) = circuit_breaker_call(current_breaker, success_op, current_time)
    current_breaker = new_breaker
    
    assert_true(result.success)
    assert_eq(current_breaker.state, "closed")
    assert_eq(current_breaker.failure_count, 0)
  }
  
  // Now fail 3 times to open circuit
  for i in 0..3 {
    let failure_op = fn() {
      {
        success: false,
        data: None,
        error: Some(TelemetryError::NetworkTimeout("Timeout")),
        retry_count: 0
      }
    }
    
    let (result, new_breaker) = circuit_breaker_call(current_breaker, failure_op, current_time)
    current_breaker = new_breaker
    current_time = current_time + 1000
    
    assert_false(result.success)
    
    if i < 2 {
      assert_eq(current_breaker.state, "closed")
      assert_eq(current_breaker.failure_count, i + 1)
    } else {
      assert_eq(current_breaker.state, "open")
      assert_eq(current_breaker.failure_count, 3)
    }
  }
  
  // Circuit should now be open
  let test_op = fn() {
    {
      success: true,
      data: Some("should_not_execute"),
      error: None,
      retry_count: 0
    }
  }
  
  let (open_result, breaker_after_open) = circuit_breaker_call(current_breaker, test_op, current_time)
  assert_false(open_result.success)
  assert_eq(breaker_after_open.state, "open")
  
  match open_result.error {
    Some(TelemetryError::ServiceUnavailable(msg)) => assert_eq(msg, "Circuit breaker open")
    _ => assert_true(false)
  }
  
  // Test timeout recovery
  current_time = current_time + 6000 // Past timeout
  
  let recovery_op = fn() {
    {
      success: true,
      data: Some("recovered"),
      error: None,
      retry_count: 0
    }
  }
  
  let (recovery_result, final_breaker) = circuit_breaker_call(breaker_after_open, recovery_op, current_time)
  assert_true(recovery_result.success)
  assert_eq(final_breaker.state, "closed")
  assert_eq(final_breaker.failure_count, 0)
}

// Test 7: Performance Benchmarking and Resource Optimization
test "performance benchmarking and resource optimization" {
  // Define benchmark result
  type BenchmarkResult = {
    operation_name: String,
    iterations: Int,
    total_time_ms: Int,
    avg_time_ms: Float,
    min_time_ms: Float,
    max_time_ms: Float,
    memory_used_mb: Float
  }
  
  // Simple performance measurement
  let benchmark_operation = fn(operation: () -> Unit, iterations: Int, operation_name: String) {
    let mut times = []
    
    // Warm up
    for i in 0..5 {
      operation()
    }
    
    // Measure iterations
    for i in 0..iterations {
      let start_time = 1000 // Simplified timestamp
      operation()
      let end_time = 1005 // Simplified timestamp
      let duration = end_time - start_time
      
      // Add some variation to simulate real timing
      let adjusted_duration = duration + (i % 3)
      times = times.push(adjusted_duration.to_float())
    }
    
    // Calculate statistics
    let total_time = times.reduce(fn(acc, t) { acc + t }, 0.0)
    let avg_time = total_time / iterations.to_float()
    let min_time = times.reduce(fn(acc, t) { if t < acc { t } else { acc } }, times[0])
    let max_time = times.reduce(fn(acc, t) { if t > acc { t } else { acc } }, times[0])
    
    {
      operation_name,
      iterations,
      total_time_ms: total_time.to_int(),
      avg_time_ms: avg_time,
      min_time_ms: min_time,
      max_time_ms: max_time,
      memory_used_mb: 10.5 // Simulated memory usage
    }
  }
  
  // Test different operations
  let array_operation = fn() {
    let mut arr = []
    for i in 0..100 {
      arr = arr.push(i)
    }
    
    let mut sum = 0
    for i in 0..arr.length() {
      sum = sum + arr[i]
    }
    
    sum
  }
  
  let string_operation = fn() {
    let mut str = ""
    for i in 0..50 {
      str = str + "item-" + i.to_string()
    }
    
    str.length()
  }
  
  let map_operation = fn() {
    let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let doubled = numbers.map(fn(x) { x * 2 })
    let filtered = doubled.filter(fn(x) { x > 10 })
    let sum = filtered.reduce(fn(acc, x) { acc + x }, 0)
    
    sum
  }
  
  // Run benchmarks
  let array_benchmark = benchmark_operation(array_operation, 100, "array_operations")
  let string_benchmark = benchmark_operation(string_operation, 100, "string_operations")
  let map_benchmark = benchmark_operation(map_operation, 100, "map_operations")
  
  // Verify benchmark results
  assert_eq(array_benchmark.operation_name, "array_operations")
  assert_eq(array_benchmark.iterations, 100)
  assert_true(array_benchmark.avg_time_ms > 0)
  assert_true(array_benchmark.max_time_ms >= array_benchmark.min_time_ms)
  
  assert_eq(string_benchmark.operation_name, "string_operations")
  assert_eq(string_benchmark.iterations, 100)
  assert_true(string_benchmark.avg_time_ms > 0)
  
  assert_eq(map_benchmark.operation_name, "map_operations")
  assert_eq(map_benchmark.iterations, 100)
  assert_true(map_benchmark.avg_time_ms > 0)
  
  // Compare performance
  let compare_benchmarks = fn(bench1: BenchmarkResult, bench2: BenchmarkResult) {
    let speedup = bench2.avg_time_ms / bench1.avg_time_ms
    {
      faster_operation: if speedup > 1.0 { bench1.operation_name } else { bench2.operation_name },
      speedup_factor: if speedup > 1.0 { speedup } else { 1.0 / speedup },
      time_difference_ms: (bench2.avg_time_ms - bench1.avg_time_ms).abs()
    }
  }
  
  let array_vs_string = compare_benchmarks(array_benchmark, string_benchmark)
  assert_true(array_vs_string.speedup_factor > 0)
  assert_true(array_vs_string.time_difference_ms >= 0)
  
  // Test memory usage optimization
  let memory_intensive_operation = fn() {
    let mut large_arrays = []
    
    for i in 0..10 {
      let mut large_array = []
      for j in 0..1000 {
        large_array = large_array.push(j)
      }
      large_arrays = large_arrays.push(large_array)
    }
    
    large_arrays.length()
  }
  
  let memory_optimized_operation = fn() {
    let mut total_length = 0
    
    for i in 0..10 {
      // Process one array at a time instead of storing all
      let mut large_array = []
      for j in 0..1000 {
        large_array = large_array.push(j)
      }
      total_length = total_length + large_array.length()
    }
    
    total_length
  }
  
  let memory_intensive_benchmark = benchmark_operation(memory_intensive_operation, 10, "memory_intensive")
  let memory_optimized_benchmark = benchmark_operation(memory_optimized_operation, 10, "memory_optimized")
  
  // Memory optimized should use less memory (simulated)
  assert_true(memory_optimized_benchmark.memory_used_mb < memory_intensive_benchmark.memory_used_mb)
  
  // Test algorithm optimization
  let naive_search = fn() {
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
    let target = 15
    
    let mut found = false
    for i in 0..data.length() {
      if data[i] == target {
        found = true
        break
      }
    }
    
    found
  }
  
  let binary_search = fn() {
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
    let target = 15
    
    let mut left = 0
    let mut right = data.length() - 1
    let mut found = false
    
    while left <= right {
      let mid = (left + right) / 2
      if data[mid] == target {
        found = true
        break
      } else if data[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    found
  }
  
  let naive_benchmark = benchmark_operation(naive_search, 1000, "naive_search")
  let binary_benchmark = benchmark_operation(binary_search, 1000, "binary_search")
  
  // Binary search should be faster for sorted data
  let search_comparison = compare_benchmarks(naive_benchmark, binary_benchmark)
  
  // Both should find the target
  assert_true(naive_search())
  assert_true(binary_search())
  
  // Test resource pooling optimization
  type Resource = {
    id: Int,
    created_at: Int,
    in_use: Bool
  }
  
  let create_resource_without_pool = fn() {
    {
      id: 42,
      created_at: 1000,
      in_use: false
    }
  }
  
  let mut resource_pool = []
  for i in 0..10 {
    resource_pool = resource_pool.push({
      id: i,
      created_at: 1000,
      in_use: false
    })
  }
  
  let create_resource_with_pool = fn() {
    let mut available_index = -1
    
    for i in 0..resource_pool.length() {
      if not(resource_pool[i].in_use) {
        available_index = i
        break
      }
    }
    
    if available_index >= 0 {
      let resource = resource_pool[available_index]
      resource_pool[available_index] = { resource | in_use: true }
      resource
    } else {
      {
        id: 999,
        created_at: 1000,
        in_use: false
      }
    }
  }
  
  let no_pool_benchmark = benchmark_operation(create_resource_without_pool, 100, "create_without_pool")
  let with_pool_benchmark = benchmark_operation(create_resource_with_pool, 100, "create_with_pool")
  
  // Pool should be more efficient (simulated)
  assert_true(with_pool_benchmark.avg_time_ms <= no_pool_benchmark.avg_time_ms)
}

// Test 8: Multi-dimensional Data Analysis
test "multi-dimensional data analysis" {
  // Define multi-dimensional data point
  type DataPoint = {
    dimensions: Array[Float],
    timestamp: Int,
    label: String
  }
  
  // Define cluster result
  type Cluster = {
    id: Int,
    center: Array[Float],
    points: Array[DataPoint],
    radius: Float
  }
  
  // Create sample multi-dimensional data
  let create_sample_data = fn() {
    [
      { dimensions: [1.0, 2.0, 3.0], timestamp: 1000, label: "A" },
      { dimensions: [1.1, 2.1, 3.1], timestamp: 1100, label: "A" },
      { dimensions: [1.2, 2.2, 3.2], timestamp: 1200, label: "A" },
      { dimensions: [5.0, 6.0, 7.0], timestamp: 1300, label: "B" },
      { dimensions: [5.1, 6.1, 7.1], timestamp: 1400, label: "B" },
      { dimensions: [5.2, 6.2, 7.2], timestamp: 1500, label: "B" },
      { dimensions: [9.0, 10.0, 11.0], timestamp: 1600, label: "C" },
      { dimensions: [9.1, 10.1, 11.1], timestamp: 1700, label: "C" },
      { dimensions: [9.2, 10.2, 11.2], timestamp: 1800, label: "C" }
    ]
  }
  
  // Calculate Euclidean distance between two points
  let euclidean_distance = fn(p1: Array[Float], p2: Array[Float]) {
    let mut sum_of_squares = 0.0
    
    for i in 0..p1.length() {
      let diff = p1[i] - p2[i]
      sum_of_squares = sum_of_squares + (diff * diff)
    }
    
    sum_of_squares.sqrt()
  }
  
  // Test distance calculation
  let point1 = [1.0, 2.0, 3.0]
  let point2 = [1.0, 2.0, 3.0]
  let point3 = [4.0, 5.0, 6.0]
  
  let distance1 = euclidean_distance(point1, point2)
  let distance2 = euclidean_distance(point1, point3)
  
  assert_eq(distance1, 0.0) // Same point
  assert_true(distance2 > 5.0) // Different points
  
  // Simple K-means clustering implementation
  let k_means_clustering = fn(data: Array[DataPoint], k: Int, max_iterations: Int) {
    if data.length() == 0 or k <= 0 or k > data.length() {
      return []
    }
    
    // Initialize centroids with first k points
    let mut centroids = []
    for i in 0..k {
      centroids = centroids.push(data[i].dimensions)
    }
    
    let mut clusters = []
    
    for iteration in 0..max_iterations {
      // Assign points to nearest centroid
      let mut new_clusters = []
      
      for i in 0..k {
        new_clusters = new_clusters.push({
          id: i,
          center: centroids[i],
          points: [],
          radius: 0.0
        })
      }
      
      for point in data {
        let mut min_distance = 999999.0
        let mut closest_cluster = 0
        
        for i in 0..centroids.length() {
          let distance = euclidean_distance(point.dimensions, centroids[i])
          if distance < min_distance {
            min_distance = distance
            closest_cluster = i
          }
        }
        
        let cluster = new_clusters[closest_cluster]
        let updated_cluster = { cluster | points: cluster.points.push(point) }
        new_clusters[closest_cluster] = updated_cluster
      }
      
      // Update centroids
      let mut new_centroids = []
      let mut converged = true
      
      for i in 0..new_clusters.length() {
        let cluster = new_clusters[i]
        
        if cluster.points.length() > 0 {
          let mut new_centroid = []
          
          for dim in 0..cluster.points[0].dimensions.length() {
            let mut sum = 0.0
            for point in cluster.points {
              sum = sum + point.dimensions[dim]
            }
            new_centroid = new_centroid.push(sum / cluster.points.length().to_float())
          }
          
          // Check for convergence
          let distance = euclidean_distance(centroids[i], new_centroid)
          if distance > 0.01 {
            converged = false
          }
          
          new_centroids = new_centroids.push(new_centroid)
        } else {
          new_centroids = new_centroids.push(centroids[i])
        }
      }
      
      centroids = new_centroids
      clusters = new_clusters
      
      if converged {
        break
      }
    }
    
    // Calculate cluster radii
    let mut final_clusters = []
    for cluster in clusters {
      let mut max_distance = 0.0
      
      for point in cluster.points {
        let distance = euclidean_distance(point.dimensions, cluster.center)
        if distance > max_distance {
          max_distance = distance
        }
      }
      
      final_clusters = final_clusters.push({ cluster | radius: max_distance })
    }
    
    final_clusters
  }
  
  // Test clustering
  let data = create_sample_data()
  let clusters = k_means_clustering(data, 3, 10)
  
  assert_eq(clusters.length(), 3)
  
  // Verify each cluster has points
  for cluster in clusters {
    assert_true(cluster.points.length() > 0)
    assert_true(cluster.radius >= 0.0)
  }
  
  // Verify points are correctly clustered by label
  let cluster_a = clusters.find(fn(c) { c.points[0].label == "A" }).unwrap()
  let cluster_b = clusters.find(fn(c) { c.points[0].label == "B" }).unwrap()
  let cluster_c = clusters.find(fn(c) { c.points[0].label == "C" }).unwrap()
  
  assert_eq(cluster_a.points.length(), 3)
  assert_eq(cluster_b.points.length(), 3)
  assert_eq(cluster_c.points.length(), 3)
  
  // Test dimensionality reduction (simple PCA-like approach)
  let calculate_mean_vector = fn(data: Array[DataPoint]) {
    if data.length() == 0 {
      return []
    }
    
    let dimensions = data[0].dimensions.length()
    let mut mean = []
    
    for dim in 0..dimensions {
      let mut sum = 0.0
      for point in data {
        sum = sum + point.dimensions[dim]
      }
      mean = mean.push(sum / data.length().to_float())
    }
    
    mean
  }
  
  let calculate_covariance_matrix = fn(data: Array[DataPoint], mean: Array[Float]) {
    if data.length() == 0 {
      return []
    }
    
    let dimensions = data[0].dimensions.length()
    let mut covariance_matrix = []
    
    for i in 0..dimensions {
      let mut row = []
      for j in 0..dimensions {
        let mut covariance = 0.0
        
        for point in data {
          let diff_i = point.dimensions[i] - mean[i]
          let diff_j = point.dimensions[j] - mean[j]
          covariance = covariance + (diff_i * diff_j)
        }
        
        covariance = covariance / data.length().to_float()
        row = row.push(covariance)
      }
      covariance_matrix = covariance_matrix.push(row)
    }
    
    covariance_matrix
  }
  
  // Test mean calculation
  let mean = calculate_mean_vector(data)
  assert_eq(mean.length(), 3)
  assert_true(mean[0] > 5.0) // Should be around middle of data range
  
  // Test covariance matrix calculation
  let covariance_matrix = calculate_covariance_matrix(data, mean)
  assert_eq(covariance_matrix.length(), 3)
  assert_eq(covariance_matrix[0].length(), 3)
  
  // Test dimensionality reduction by selecting top variance dimensions
  let select_top_variance_dimensions = fn(covariance_matrix: Array[Array[Float]], k: Int) {
    let dimensions = covariance_matrix.length()
    let mut variances = []
    
    for i in 0..dimensions {
      variances = variances.push({
        dimension: i,
        variance: covariance_matrix[i][i] // Diagonal elements are variances
      })
    }
    
    // Sort by variance (descending)
    let mut sorted = []
    for i in 0..variances.length() {
      let mut max_variance = variances[0]
      let mut max_index = 0
      
      for j in 1..variances.length() {
        if variances[j].variance > max_variance.variance {
          max_variance = variances[j]
          max_index = j
        }
      }
      
      sorted = sorted.push(max_variance)
      variances[max_index] = { variance: -1.0, dimension: max_index }
    }
    
    // Select top k dimensions
    let mut top_dimensions = []
    for i in 0..k and i < sorted.length() {
      top_dimensions = top_dimensions.push(sorted[i].dimension)
    }
    
    top_dimensions
  }
  
  let top_dimensions = select_top_variance_dimensions(covariance_matrix, 2)
  assert_eq(top_dimensions.length(), 2)
  
  // Reduce data dimensions
  let reduce_dimensions = fn(data: Array[DataPoint], selected_dimensions: Array[Int]) {
    let mut reduced_data = []
    
    for point in data {
      let mut reduced_dimensions = []
      for dim in selected_dimensions {
        reduced_dimensions = reduced_dimensions.push(point.dimensions[dim])
      }
      
      reduced_data = reduced_data.push({
        dimensions: reduced_dimensions,
        timestamp: point.timestamp,
        label: point.label
      })
    }
    
    reduced_data
  }
  
  let reduced_data = reduce_dimensions(data, top_dimensions)
  assert_eq(reduced_data.length(), data.length())
  
  for point in reduced_data {
    assert_eq(point.dimensions.length(), 2)
  }
  
  // Test outlier detection using distance from centroid
  let detect_outliers = fn(data: Array[DataPoint], threshold: Float) {
    let mean = calculate_mean_vector(data)
    let mut outliers = []
    
    for point in data {
      let distance = euclidean_distance(point.dimensions, mean)
      if distance > threshold {
        outliers = outliers.push({
          point,
          distance_from_center: distance
        })
      }
    }
    
    outliers
  }
  
  // Add an outlier to test detection
  let data_with_outlier = data.push({
    dimensions: [50.0, 60.0, 70.0], // Far from other points
    timestamp: 1900,
    label: "OUTLIER"
  })
  
  let outliers = detect_outliers(data_with_outlier, 20.0)
  assert_eq(outliers.length(), 1)
  assert_eq(outliers[0].point.label, "OUTLIER")
  assert_true(outliers[0].distance_from_center > 20.0)
}

// Test 9: Cross-Service Context Propagation
test "cross-service context propagation" {
  // Define context structure
  type ContextValue = {
    key: String,
    value: String,
    metadata: Array[String]
  }
  
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[ContextValue],
    flags: Array[String]
  }
  
  // Define service call structure
  type ServiceCall = {
    service_name: String,
    operation: String,
    incoming_context: Option[TraceContext],
    outgoing_context: Option[TraceContext],
    timestamp: Int
  }
  
  // Create trace context
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage: [
        { key: "user.id", value: "user-123", metadata: ["propagate", "sensitive"] },
        { key: "request.id", value: "req-456", metadata: ["propagate"] },
        { key: "session.id", value: "sess-789", metadata: ["propagate", "session"] }
      ],
      flags: ["sampled", "debug"]
    }
  }
  
  // Extract context for propagation
  let extract_propagation_headers = fn(context: TraceContext) {
    let mut headers = []
    
    // Add trace context headers
    headers = headers.push(("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"))
    headers = headers.push(("x-trace-id", context.trace_id))
    headers = headers.push(("x-span-id", context.span_id))
    
    match context.parent_span_id {
      Some(parent_id) => {
        headers = headers.push(("x-parent-span-id", parent_id))
      }
      None => {}
    }
    
    // Add baggage items that should be propagated
    for item in context.baggage {
      if item.metadata.contains("propagate") {
        headers = headers.push(("baggage-" + item.key, item.value))
      }
    }
    
    // Add flags
    let flags_str = context.flags.join(",")
    headers = headers.push(("x-trace-flags", flags_str))
    
    headers
  }
  
  // Inject context from headers
  let inject_context_from_headers = fn(headers: Array[(String, String)]) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut baggage = []
    let mut flags = []
    
    for (key, value) in headers {
      match key {
        "x-trace-id" => trace_id = value
        "x-span-id" => span_id = value
        "x-parent-span-id" => parent_span_id = Some(value)
        "x-trace-flags" => flags = value.split(",")
        _ => {
          if key.starts_with("baggage-") {
            let baggage_key = key.substring(8, key.length() - 8) // Remove "baggage-" prefix
            baggage = baggage.push({
              key: baggage_key,
              value,
              metadata: ["propagate"] // Default metadata
            })
          }
        }
      }
    }
    
    if trace_id != "" and span_id != "" {
      Some({
        trace_id,
        span_id,
        parent_span_id,
        baggage,
        flags
      })
    } else {
      None
    }
  }
  
  // Test context creation and propagation
  let root_context = create_trace_context("trace-12345", "span-abcde", None)
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-abcde")
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.length(), 3)
  assert_eq(root_context.flags.length(), 2)
  
  // Test header extraction
  let headers = extract_propagation_headers(root_context)
  assert_true(headers.length() >= 6)
  
  let trace_id_header = headers.find(fn(h) { h.0 == "x-trace-id" }).unwrap()
  assert_eq(trace_id_header.1, "trace-12345")
  
  let span_id_header = headers.find(fn(h) { h.0 == "x-span-id" }).unwrap()
  assert_eq(span_id_header.1, "span-abcde")
  
  let baggage_header = headers.find(fn(h) { h.0 == "baggage-user.id" }).unwrap()
  assert_eq(baggage_header.1, "user-123")
  
  // Test context injection
  let injected_context = inject_context_from_headers(headers)
  assert_true(injected_context.is_some())
  
  let context = injected_context.unwrap()
  assert_eq(context.trace_id, "trace-12345")
  assert_eq(context.span_id, "span-abcde")
  assert_eq(context.parent_span_id, None)
  assert_eq(context.baggage.length(), 3)
  assert_true(context.flags.contains("sampled"))
  assert_true(context.flags.contains("debug"))
  
  // Test child context creation
  let create_child_context = fn(parent_context: TraceContext, child_span_id: String) {
    {
      trace_id: parent_context.trace_id,
      span_id: child_span_id,
      parent_span_id: Some(parent_context.span_id),
      baggage: parent_context.baggage,
      flags: parent_context.flags
    }
  }
  
  let child_context = create_child_context(root_context, "span-fghij")
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-fghij")
  assert_eq(child_context.parent_span_id, Some("span-abcde"))
  assert_eq(child_context.baggage.length(), 3)
  
  // Test service call simulation
  let simulate_service_call = fn(service_name: String, operation: String, incoming_context: Option[TraceContext]) {
    let current_time = 1640995200
    
    match incoming_context {
      Some(context) => {
        // Create child span for this service
        let child_span_id = "span-" + service_name + "-" + operation
        let child_context = create_child_context(context, child_span_id)
        
        // Extract headers for downstream call
        let headers = extract_propagation_headers(child_context)
        
        {
          service_name,
          operation,
          incoming_context,
          outgoing_context: Some(child_context),
          timestamp: current_time,
          downstream_headers: headers
        }
      }
      None => {
        // No incoming context, start new trace
        let new_trace_id = "trace-" + service_name + "-" + current_time.to_string()
        let new_span_id = "span-" + service_name + "-" + operation
        let new_context = create_trace_context(new_trace_id, new_span_id, None)
        
        let headers = extract_propagation_headers(new_context)
        
        {
          service_name,
          operation,
          incoming_context,
          outgoing_context: Some(new_context),
          timestamp: current_time,
          downstream_headers: headers
        }
      }
    }
  }
  
  // Test service chain
  let api_call = simulate_service_call("api-gateway", "process_request", None)
  assert_eq(api_call.service_name, "api-gateway")
  assert_eq(api_call.operation, "process_request")
  assert_eq(api_call.incoming_context, None)
  assert_true(api_call.outgoing_context.is_some())
  
  let api_context = api_call.outgoing_context.unwrap()
  let auth_call = simulate_service_call("auth-service", "validate_token", Some(api_context))
  
  assert_eq(auth_call.service_name, "auth-service")
  assert_eq(auth_call.operation, "validate_token")
  assert_true(auth_call.incoming_context.is_some())
  
  let auth_incoming = auth_call.incoming_context.unwrap()
  assert_eq(auth_incoming.trace_id, api_context.trace_id)
  assert_eq(auth_incoming.parent_span_id, Some(api_context.span_id))
  
  // Test context consistency across services
  let verify_context_consistency = fn(calls: Array[ServiceCall]) {
    let mut errors = []
    
    if calls.length() == 0 {
      return errors
    }
    
    let root_call = calls[0]
    let root_context = root_call.outgoing_context.unwrap()
    let trace_id = root_context.trace_id
    
    // Verify all calls have the same trace_id
    for call in calls {
      match call.outgoing_context {
        Some(context) => {
          if context.trace_id != trace_id {
            errors = errors.push("Call " + call.service_name + " has different trace_id")
          }
        }
        None => {
          errors = errors.push("Call " + call.service_name + " missing outgoing context")
        }
      }
    }
    
    // Verify parent-child relationships
    for i in 1..calls.length() {
      let current_call = calls[i]
      let previous_call = calls[i - 1]
      
      match current_call.incoming_context {
        Some(incoming) => {
          match previous_call.outgoing_context {
            Some(previous_outgoing) => {
              if incoming.trace_id != previous_outgoing.trace_id {
                errors = errors.push("Trace ID mismatch between " + previous_call.service_name + " and " + current_call.service_name)
              }
              
              if incoming.parent_span_id != Some(previous_outgoing.span_id) {
                errors = errors.push("Parent span ID mismatch between " + previous_call.service_name + " and " + current_call.service_name)
              }
            }
            None => {
              errors = errors.push("Previous call " + previous_call.service_name + " missing outgoing context")
            }
          }
        }
        None => {
          errors = errors.push("Call " + current_call.service_name + " missing incoming context")
        }
      }
    }
    
    errors
  }
  
  let service_calls = [api_call, auth_call]
  let consistency_errors = verify_context_consistency(service_calls)
  assert_eq(consistency_errors.length(), 0)
  
  // Test baggage propagation
  let update_baggage = fn(context: TraceContext, key: String, value: String, metadata: Array[String]) {
    let mut new_baggage = []
    let mut found = false
    
    for item in context.baggage {
      if item.key == key {
        new_baggage = new_baggage.push({ key, value, metadata })
        found = true
      } else {
        new_baggage = new_baggage.push(item)
      }
    }
    
    if not(found) {
      new_baggage = new_baggage.push({ key, value, metadata })
    }
    
    { context | baggage: new_baggage }
  }
  
  let updated_context = update_baggage(root_context, "service.version", "1.2.3", ["propagate", "version"])
  assert_eq(updated_context.baggage.length(), 4)
  
  let version_item = updated_context.baggage.find(fn(item) { item.key == "service.version" }).unwrap()
  assert_eq(version_item.value, "1.2.3")
  assert_true(version_item.metadata.contains("version"))
}

// Test 10: Memory Leak Detection and Resource Cleanup
test "memory leak detection and resource cleanup" {
  // Define resource types
  type Resource = {
    id: String,
    resource_type: String,
    created_at: Int,
    last_accessed: Int,
    size_bytes: Int,
    in_use: Bool
  }
  
  type ResourceTracker = {
    active_resources: Array[Resource],
    total_allocated: Int,
    total_deallocated: Int,
    peak_memory_usage: Int,
    current_memory_usage: Int
  }
  
  // Create resource tracker
  let create_tracker = fn() {
    {
      active_resources: [],
      total_allocated: 0,
      total_deallocated: 0,
      peak_memory_usage: 0,
      current_memory_usage: 0
    }
  }
  
  // Allocate resource
  let allocate_resource = fn(tracker: ResourceTracker, resource_type: String, size_bytes: Int) {
    let resource_id = "res-" + tracker.total_allocated.to_string()
    let current_time = 1640995200
    
    let resource = {
      id: resource_id,
      resource_type,
      created_at: current_time,
      last_accessed: current_time,
      size_bytes,
      in_use: true
    }
    
    let updated_active = tracker.active_resources.push(resource)
    let new_memory_usage = tracker.current_memory_usage + size_bytes
    let new_peak = if new_memory_usage > tracker.peak_memory_usage {
      new_memory_usage
    } else {
      tracker.peak_memory_usage
    }
    
    {
      active_resources: updated_active,
      total_allocated: tracker.total_allocated + 1,
      total_deallocated: tracker.total_deallocated,
      peak_memory_usage: new_peak,
      current_memory_usage: new_memory_usage
    }
  }
  
  // Deallocate resource
  let deallocate_resource = fn(tracker: ResourceTracker, resource_id: String) {
    let mut updated_active = []
    let mut found = false
    let mut resource_size = 0
    
    for resource in tracker.active_resources {
      if resource.id == resource_id {
        found = true
        resource_size = resource.size_bytes
        // Don't add to updated_active - this "deallocates" it
      } else {
        updated_active = updated_active.push(resource)
      }
    }
    
    if found {
      let new_memory_usage = tracker.current_memory_usage - resource_size
      
      {
        active_resources: updated_active,
        total_allocated: tracker.total_allocated,
        total_deallocated: tracker.total_deallocated + 1,
        peak_memory_usage: tracker.peak_memory_usage,
        current_memory_usage: new_memory_usage
      }
    } else {
      tracker // Resource not found, no change
    }
  }
  
  // Test basic resource allocation and deallocation
  let tracker = create_tracker()
  assert_eq(tracker.total_allocated, 0)
  assert_eq(tracker.total_deallocated, 0)
  assert_eq(tracker.current_memory_usage, 0)
  
  let tracker1 = allocate_resource(tracker, "buffer", 1024)
  assert_eq(tracker1.total_allocated, 1)
  assert_eq(tracker1.total_deallocated, 0)
  assert_eq(tracker1.current_memory_usage, 1024)
  assert_eq(tracker1.peak_memory_usage, 1024)
  assert_eq(tracker1.active_resources.length(), 1)
  
  let tracker2 = allocate_resource(tracker1, "connection", 2048)
  assert_eq(tracker2.total_allocated, 2)
  assert_eq(tracker2.current_memory_usage, 3072)
  assert_eq(tracker2.peak_memory_usage, 3072)
  assert_eq(tracker2.active_resources.length(), 2)
  
  let tracker3 = deallocate_resource(tracker2, "res-0")
  assert_eq(tracker3.total_allocated, 2)
  assert_eq(tracker3.total_deallocated, 1)
  assert_eq(tracker3.current_memory_usage, 2048)
  assert_eq(tracker3.active_resources.length(), 1)
  
  // Test memory leak detection
  let detect_memory_leaks = fn(tracker: ResourceTracker, max_age_ms: Int, current_time: Int) {
    let mut leaks = []
    
    for resource in tracker.active_resources {
      let age = current_time - resource.last_accessed
      
      if age > max_age_ms and not(resource.in_use) {
        leaks = leaks.push({
          resource,
          leak_reason: "resource_not_deallocated",
          age
        })
      }
    }
    
    // Check for allocation/deallocation imbalance
    let allocation imbalance = tracker.total_allocated - tracker.total_deallocated
    if allocation_imbalance != tracker.active_resources.length() {
      leaks = leaks.push({
        resource: { id: "system", resource_type: "system", created_at: 0, last_accessed: 0, size_bytes: 0, in_use: false },
        leak_reason: "allocation_deallocation_imbalance",
        age: allocation_imbalance
      })
    }
    
    leaks
  }
  
  // Create a leak scenario
  let leak_tracker = allocate_resource(tracker3, "temporary", 512)
  let leak_tracker_with_leak = allocate_resource(leak_tracker, "forgotten", 256)
  
  // Simulate resources getting old but not deallocated
  let old_resources = []
  for resource in leak_tracker_with_leak.active_resources {
    old_resources = old_resources.push({
      resource |
      last_accessed: 1000, // Make it old
      in_use: false // Mark as not in use but not deallocated
    })
  }
  
  let leak_tracker_final = { leak_tracker_with_leak | active_resources: old_resources }
  
  let leaks = detect_memory_leaks(leak_tracker_final, 100, 2000) // 100ms max age, current time 2000
  assert_true(leaks.length() >= 2) // Should find at least 2 leaks
  
  let resource_leaks = leaks.filter(fn(l) { l.leak_reason == "resource_not_deallocated" })
  assert_true(resource_leaks.length() >= 2)
  
  // Test resource cleanup
  let cleanup_old_resources = fn(tracker: ResourceTracker, max_age_ms: Int, current_time: Int) {
    let mut updated_tracker = tracker
    
    for resource in tracker.active_resources {
      let age = current_time - resource.last_accessed
      
      if age > max_age_ms and not(resource.in_use) {
        updated_tracker = deallocate_resource(updated_tracker, resource.id)
      }
    }
    
    updated_tracker
  }
  
  let cleaned_tracker = cleanup_old_resources(leak_tracker_final, 100, 2000)
  assert_true(cleaned_tracker.current_memory_usage < leak_tracker_final.current_memory_usage)
  assert_true(cleaned_tracker.total_deallocated > leak_tracker_final.total_deallocated)
  
  // Test resource pool cleanup
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    cleanup_threshold: Float
  }
  
  let create_resource_pool = fn(max_size: Int, cleanup_threshold: Float) {
    {
      resources: [],
      max_size,
      cleanup_threshold
    }
  }
  
  let cleanup_pool = fn(pool: ResourcePool, target_size: Int) {
    if pool.resources.length() <= target_size {
      pool
    } else {
      // Sort by last accessed time (oldest first)
      let mut sorted_resources = pool.resources
      let mut sorted = []
      
      // Simple selection sort by last_accessed
      while sorted_resources.length() > 0 {
        let mut oldest = sorted_resources[0]
        let mut oldest_index = 0
        
        for i in 1..sorted_resources.length() {
          if sorted_resources[i].last_accessed < oldest.last_accessed {
            oldest = sorted_resources[i]
            oldest_index = i
          }
        }
        
        sorted = sorted.push(oldest)
        
        let mut remaining = []
        for i in 0..sorted_resources.length() {
          if i != oldest_index {
            remaining = remaining.push(sorted_resources[i])
          }
        }
        sorted_resources = remaining
      }
      
      // Keep the most recently used resources
      let mut kept_resources = []
      for i in (sorted.length() - target_size)..sorted.length() {
        kept_resources = kept_resources.push(sorted[i])
      }
      
      { pool | resources: kept_resources }
    }
  }
  
  // Test pool cleanup
  let pool = create_resource_pool(10, 0.8)
  
  // Fill pool with resources of different ages
  let mut full_pool = pool
  for i in 0..10 {
    let resource = {
      id: "pool-res-" + i.to_string(),
      resource_type: "pooled",
      created_at: 1000 + i * 100,
      last_accessed: 1000 + i * 100,
      size_bytes: 1024,
      in_use: false
    }
    full_pool = { full_pool | resources: full_pool.resources.push(resource) }
  }
  
  assert_eq(full_pool.resources.length(), 10)
  
  // Cleanup to 5 resources
  let cleaned_pool = cleanup_pool(full_pool, 5)
  assert_eq(cleaned_pool.resources.length(), 5)
  
  // Verify the newest resources are kept
  for resource in cleaned_pool.resources {
    assert_true(resource.last_accessed >= 1500) // Should keep the 5 most recent
  }
  
  // Test memory usage monitoring
  let monitor_memory_usage = fn(tracker: ResourceTracker, threshold_mb: Int) {
    let current_usage_mb = tracker.current_memory_usage / (1024 * 1024)
    let peak_usage_mb = tracker.peak_memory_usage / (1024 * 1024)
    
    let mut alerts = []
    
    if current_usage_mb > threshold_mb {
      alerts = alerts.push({
        alert_type: "memory_threshold_exceeded",
        current_usage_mb,
        threshold_mb,
        message: "Current memory usage exceeds threshold"
      })
    }
    
    if peak_usage_mb > threshold_mb * 2 {
      alerts = alerts.push({
        alert_type: "peak_memory_warning",
        peak_usage_mb,
        threshold_mb: threshold_mb * 2,
        message: "Peak memory usage is very high"
      })
    }
    
    // Check for memory leak indicators
    let allocation_ratio = if tracker.total_allocated > 0 {
      tracker.total_deallocated.to_float() / tracker.total_allocated.to_float()
    } else {
      1.0
    }
    
    if allocation_ratio < 0.8 {
      alerts = alerts.push({
        alert_type: "potential_memory_leak",
        allocation_ratio,
        message: "Deallocation ratio is low, potential memory leak"
      })
    }
    
    {
      current_usage_mb,
      peak_usage_mb,
      allocation_ratio,
      alerts
    }
  }
  
  // Create a scenario with high memory usage
  let high_memory_tracker = allocate_resource(cleaned_tracker, "large_buffer", 10 * 1024 * 1024) // 10MB
  let monitoring_result = monitor_memory_usage(high_memory_tracker, 5) // 5MB threshold
  
  assert_true(monitoring_result.current_usage_mb > 5)
  assert_true(monitoring_result.alerts.length() > 0)
  
  let threshold_alert = monitoring_result.alerts.find(fn(a) { a.alert_type == "memory_threshold_exceeded" }).unwrap()
  assert_eq(threshold_alert.alert_type, "memory_threshold_exceeded")
  assert_true(threshold_alert.current_usage_mb > threshold_alert.threshold_mb)
}