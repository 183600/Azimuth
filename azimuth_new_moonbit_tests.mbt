// Azimuth New MoonBit Test Suite
// This file contains new MoonBit test cases focusing on telemetry system features

// Test 1: Telemetry Data Creation and Validation
test "telemetry data creation and validation" {
  // Define telemetry span type
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
  
  // Create a valid telemetry span
  let span = {
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    parent_span_id: Some("span-123456789"),
    operation_name: "database_query",
    start_time: 1640995200000,
    end_time: 1640995200250,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.instance", "primary")
    ]
  }
  
  // Validate trace ID format (should be 16 characters)
  assert_eq(span.trace_id.length(), 16)
  assert_true(span.trace_id.starts_with("trace-"))
  
  // Validate span ID format (should be 16 characters)
  assert_eq(span.span_id.length(), 16)
  assert_true(span.span_id.starts_with("span-"))
  
  // Validate time range
  assert_true(span.start_time <= span.end_time)
  let duration = span.end_time - span.start_time
  assert_eq(duration, 250)
  
  // Validate status
  assert_true(span.status == "ok" || span.status == "error")
  
  // Validate attributes
  assert_eq(span.attributes.length(), 3)
  assert_true(span.attributes.contains(("db.type", "postgresql")))
  assert_true(span.attributes.contains(("db.statement", "SELECT * FROM users")))
  assert_true(span.attributes.contains(("db.instance", "primary")))
  
  // Test validation function
  let validate_span = fn(s: TelemetrySpan) {
    let valid_trace_id = s.trace_id.length() == 16 && s.trace_id.starts_with("trace-")
    let valid_span_id = s.span_id.length() == 16 && s.span_id.starts_with("span-")
    let valid_time_range = s.start_time <= s.end_time
    let valid_status = s.status == "ok" || s.status == "error"
    
    valid_trace_id && valid_span_id && valid_time_range && valid_status
  }
  
  assert_true(validate_span(span))
  
  // Test invalid span
  let invalid_span = { span | trace_id: "invalid", status: "unknown" }
  assert_false(validate_span(invalid_span))
}

// Test 2: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define telemetry metric type
  type TelemetryMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create a telemetry metric
  let metric = {
    name: "response_time",
    value: 125.5,
    unit: "ms",
    timestamp: 1640995200000,
    tags: [
      ("service", "api"),
      ("endpoint", "/users"),
      ("method", "GET")
    ]
  }
  
  // Simulate serialization to JSON string
  let serialize_metric = fn(m: TelemetryMetric) {
    let tags_str = m.tags.map(fn(t) { 
      "\"" + t.0 + "\":\"" + t.1 + "\"" 
    }).join(",")
    
    "{" +
    "\"name\":\"" + m.name + "\"," +
    "\"value\":" + m.value.to_string() + "," +
    "\"unit\":\"" + m.unit + "\"," +
    "\"timestamp\":" + m.timestamp.to_string() + "," +
    "\"tags\":{" + tags_str + "}" +
    "}"
  }
  
  // Serialize metric
  let serialized = serialize_metric(metric)
  
  // Verify serialization contains expected fields
  assert_true(serialized.contains("\"name\":\"response_time\""))
  assert_true(serialized.contains("\"value\":125.5"))
  assert_true(serialized.contains("\"unit\":\"ms\""))
  assert_true(serialized.contains("\"timestamp\":1640995200000"))
  assert_true(serialized.contains("\"service\":\"api\""))
  assert_true(serialized.contains("\"endpoint\":\"/users\""))
  assert_true(serialized.contains("\"method\":\"GET\""))
  
  // Simulate deserialization from JSON string
  let deserialize_metric = fn(json_str: String) {
    // This is a simplified deserialization for demonstration
    let name_start = json_str.find("\"name\":\"") + 8
    let name_end = json_str.find("\",", name_start)
    let name = json_str.substring(name_start, name_end - name_start)
    
    let value_start = json_str.find("\"value\":") + 8
    let value_end = json_str.find(",", value_start)
    let value_str = json_str.substring(value_start, value_end - value_start)
    let value = value_str.to_float()
    
    let unit_start = json_str.find("\"unit\":\"") + 8
    let unit_end = json_str.find("\",", unit_start)
    let unit = json_str.substring(unit_start, unit_end - unit_start)
    
    let timestamp_start = json_str.find("\"timestamp\":") + 12
    let timestamp_end = json_str.find(",", timestamp_start)
    let timestamp_str = json_str.substring(timestamp_start, timestamp_end - timestamp_start)
    let timestamp = timestamp_str.to_int()
    
    {
      name,
      value,
      unit,
      timestamp,
      tags: [] // Simplified: not parsing tags in this example
    }
  }
  
  // Deserialize metric
  let deserialized = deserialize_metric(serialized)
  
  // Verify deserialization
  assert_eq(deserialized.name, metric.name)
  assert_eq(deserialized.value, metric.value)
  assert_eq(deserialized.unit, metric.unit)
  assert_eq(deserialized.timestamp, metric.timestamp)
  
  // Test round-trip serialization
  let reserialized = serialize_metric(deserialized)
  assert_true(reserialized.contains("\"name\":\"response_time\""))
  assert_true(reserialized.contains("\"value\":125.5"))
}

// Test 3: Concurrent Telemetry Data Processing
test "concurrent telemetry data processing" {
  // Simulate concurrent processing of telemetry data
  type TelemetryEvent = {
    id: String,
    event_type: String,
    timestamp: Int,
    data: String
  }
  
  // Create a batch of telemetry events
  let events = [
    { id: "event-1", event_type: "span_start", timestamp: 1640995200000, data: "span_data_1" },
    { id: "event-2", event_type: "span_end", timestamp: 1640995200100, data: "span_data_2" },
    { id: "event-3", event_type: "metric", timestamp: 1640995200200, data: "metric_data_1" },
    { id: "event-4", event_type: "log", timestamp: 1640995200300, data: "log_data_1" },
    { id: "event-5", event_type: "span_start", timestamp: 1640995200400, data: "span_data_3" }
  ]
  
  // Simulate concurrent processing function
  let process_events_concurrently = fn(event_batch: Array[TelemetryEvent]) {
    let mut results = []
    
    // Simulate processing each event in a separate context
    for event in event_batch {
      let processed = {
        id: event.id,
        event_type: event.event_type,
        timestamp: event.timestamp,
        processed_at: 1640995201000, // Simulated processing timestamp
        processing_time: 50, // Simulated processing time in ms
        data_hash: event.data.length().to_string() // Simulated data hash
      }
      results = results.push(processed)
    }
    
    results
  }
  
  // Process events concurrently
  let processed_events = process_events_concurrently(events)
  
  // Verify all events were processed
  assert_eq(processed_events.length(), events.length())
  
  // Verify processing integrity
  for i in 0..processed_events.length() {
    let original = events[i]
    let processed = processed_events[i]
    
    assert_eq(original.id, processed.id)
    assert_eq(original.event_type, processed.event_type)
    assert_eq(original.timestamp, processed.timestamp)
    assert_eq(processed.processing_time, 50)
    assert_eq(processed.data_hash, original.data.length().to_string())
  }
  
  // Test event type filtering
  let filter_by_type = fn(event_list: Array[TelemetryEvent], event_type: String) {
    let mut filtered = []
    for event in event_list {
      if event.event_type == event_type {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  let span_starts = filter_by_type(events, "span_start")
  assert_eq(span_starts.length(), 2)
  assert_eq(span_starts[0].id, "event-1")
  assert_eq(span_starts[1].id, "event-5")
  
  let metrics = filter_by_type(events, "metric")
  assert_eq(metrics.length(), 1)
  assert_eq(metrics[0].id, "event-3")
  
  // Test time-based ordering
  let sort_by_timestamp = fn(event_list: Array[TelemetryEvent]) {
    // Simple bubble sort for demonstration
    let sorted = event_list.copy()
    let n = sorted.length()
    
    for i in 0..n-1 {
      for j in 0..n-i-1 {
        if sorted[j].timestamp > sorted[j+1].timestamp {
          let temp = sorted[j]
          sorted[j] = sorted[j+1]
          sorted[j+1] = temp
        }
      }
    }
    
    sorted
  }
  
  let sorted_events = sort_by_timestamp(events)
  for i in 0..sorted_events.length()-1 {
    assert_true(sorted_events[i].timestamp <= sorted_events[i+1].timestamp)
  }
}

// Test 4: Telemetry Data Aggregation and Analysis
test "telemetry data aggregation and analysis" {
  // Define telemetry data point
  type DataPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // Create sample telemetry data points
  let data_points = [
    { timestamp: 1640995200000, value: 100.0, tags: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 1640995201000, value: 150.0, tags: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 1640995202000, value: 120.0, tags: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 1640995203000, value: 200.0, tags: [("service", "db"), ("operation", "query")] },
    { timestamp: 1640995204000, value: 180.0, tags: [("service", "db"), ("operation", "query")] },
    { timestamp: 1640995205000, value: 90.0, tags: [("service", "api"), ("endpoint", "/posts")] }
  ]
  
  // Calculate basic statistics
  let calculate_stats = fn(points: Array[DataPoint]) {
    if points.length() == 0 {
      { count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
    } else {
      let mut sum = 0.0
      let mut min = points[0].value
      let mut max = points[0].value
      
      for point in points {
        sum = sum + point.value
        if point.value < min {
          min = point.value
        }
        if point.value > max {
          max = point.value
        }
      }
      
      {
        count: points.length(),
        sum,
        avg: sum / points.length().to_float(),
        min,
        max
      }
    }
  }
  
  // Calculate overall statistics
  let overall_stats = calculate_stats(data_points)
  assert_eq(overall_stats.count, 6)
  assert_eq(overall_stats.sum, 840.0)
  assert_eq(overall_stats.avg, 140.0)
  assert_eq(overall_stats.min, 90.0)
  assert_eq(overall_stats.max, 200.0)
  
  // Filter by tags
  let filter_by_tag = fn(points: Array[DataPoint], key: String, value: String) {
    let mut filtered = []
    for point in points {
      let mut match_found = false
      for tag in point.tags {
        if tag.0 == key && tag.1 == value {
          match_found = true
        }
      }
      if match_found {
        filtered = filtered.push(point)
      }
    }
    filtered
  }
  
  // Filter API service data points
  let api_points = filter_by_tag(data_points, "service", "api")
  assert_eq(api_points.length(), 4)
  
  let api_stats = calculate_stats(api_points)
  assert_eq(api_stats.count, 4)
  assert_eq(api_stats.sum, 460.0)
  assert_eq(api_stats.avg, 115.0)
  
  // Filter database service data points
  let db_points = filter_by_tag(data_points, "service", "db")
  assert_eq(db_points.length(), 2)
  
  let db_stats = calculate_stats(db_points)
  assert_eq(db_stats.count, 2)
  assert_eq(db_stats.sum, 380.0)
  assert_eq(db_stats.avg, 190.0)
  
  // Calculate percentiles (simplified)
  let calculate_percentile = fn(points: Array[DataPoint], percentile: Float) {
    if points.length() == 0 {
      0.0
    } else {
      // Sort by value
      let sorted = points.copy()
      let n = sorted.length()
      
      for i in 0..n-1 {
        for j in 0..n-i-1 {
          if sorted[j].value > sorted[j+1].value {
            let temp = sorted[j]
            sorted[j] = sorted[j+1]
            sorted[j+1] = temp
          }
        }
      }
      
      // Calculate percentile index
      let index = ((percentile / 100.0) * (n.to_float() - 1.0)).to_int()
      sorted[index].value
    }
  }
  
  // Calculate 50th percentile (median)
  let p50 = calculate_percentile(data_points, 50.0)
  assert_eq(p50, 120.0) // Median of [90, 100, 120, 150, 180, 200]
  
  // Calculate 95th percentile
  let p95 = calculate_percentile(data_points, 95.0)
  assert_eq(p95, 200.0) // 95th percentile is close to max
  
  // Calculate rate (events per second)
  let calculate_rate = fn(points: Array[DataPoint]) {
    if points.length() < 2 {
      0.0
    } else {
      let time_span = points[points.length()-1].timestamp - points[0].timestamp
      if time_span > 0 {
        (points.length().to_float() - 1.0) / (time_span.to_float() / 1000.0)
      } else {
        0.0
      }
    }
  }
  
  let rate = calculate_rate(data_points)
  assert_eq(rate, 1.0) // 5 events over 5000ms = 1 event per second
}

// Test 5: Telemetry Data Filtering and Transformation
test "telemetry data filtering and transformation" {
  // Define log entry type
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample log entries
  let logs = [
    { 
      timestamp: 1640995200000, 
      level: "INFO", 
      message: "Request received", 
      source: "api-server",
      attributes: [("request_id", "req-123"), ("method", "GET")]
    },
    { 
      timestamp: 1640995201000, 
      level: "DEBUG", 
      message: "Processing request", 
      source: "api-server",
      attributes: [("request_id", "req-123"), ("step", "validation")]
    },
    { 
      timestamp: 1640995202000, 
      level: "WARN", 
      message: "Slow query detected", 
      source: "database",
      attributes: [("query_time", "500"), ("table", "users")]
    },
    { 
      timestamp: 1640995203000, 
      level: "ERROR", 
      message: "Database connection failed", 
      source: "database",
      attributes: [("error_code", "CONN001"), ("retry_count", "3")]
    },
    { 
      timestamp: 1640995204000, 
      level: "INFO", 
      message: "Request completed", 
      source: "api-server",
      attributes: [("request_id", "req-123"), ("status", "200")]
    }
  ]
  
  // Filter by log level
  let filter_by_level = fn(log_entries: Array[LogEntry], min_level: String) {
    let levels = ["DEBUG", "INFO", "WARN", "ERROR"]
    let mut min_level_index = 0
    
    for i in 0..levels.length() {
      if levels[i] == min_level {
        min_level_index = i
      }
    }
    
    let mut filtered = []
    for log in log_entries {
      let mut log_level_index = 0
      for i in 0..levels.length() {
        if levels[i] == log.level {
          log_level_index = i
        }
      }
      
      if log_level_index >= min_level_index {
        filtered = filtered.push(log)
      }
    }
    
    filtered
  }
  
  // Filter WARN and ERROR logs
  let warn_and_error_logs = filter_by_level(logs, "WARN")
  assert_eq(warn_and_error_logs.length(), 2)
  assert_eq(warn_and_error_logs[0].level, "WARN")
  assert_eq(warn_and_error_logs[1].level, "ERROR")
  
  // Filter by source
  let filter_by_source = fn(log_entries: Array[LogEntry], source: String) {
    let mut filtered = []
    for log in log_entries {
      if log.source == source {
        filtered = filtered.push(log)
      }
    }
    filtered
  }
  
  let api_logs = filter_by_source(logs, "api-server")
  assert_eq(api_logs.length(), 3)
  
  let db_logs = filter_by_source(logs, "database")
  assert_eq(db_logs.length(), 2)
  
  // Transform log entries
  let transform_log = fn(log: LogEntry) {
    {
      timestamp: log.timestamp,
      formatted_time: "2022-01-01T00:00:00." + (log.timestamp % 1000).to_string() + "Z",
      level: log.level,
      message: log.message.to_uppercase(),
      source: "[" + log.source + "]",
      attribute_count: log.attributes.length()
    }
  }
  
  // Transform all logs
  let transformed_logs = logs.map(transform_log)
  
  // Verify transformation
  for i in 0..logs.length() {
    let original = logs[i]
    let transformed = transformed_logs[i]
    
    assert_eq(original.timestamp, transformed.timestamp)
    assert_eq(original.level, transformed.level)
    assert_eq(transformed.message, original.message.to_uppercase())
    assert_eq(transformed.source, "[" + original.source + "]")
    assert_eq(transformed.attribute_count, original.attributes.length())
  }
  
  // Filter by time range
  let filter_by_time_range = fn(log_entries: Array[LogEntry], start_time: Int, end_time: Int) {
    let mut filtered = []
    for log in log_entries {
      if log.timestamp >= start_time && log.timestamp <= end_time {
        filtered = filtered.push(log)
      }
    }
    filtered
  }
  
  let time_filtered_logs = filter_by_time_range(logs, 1640995201000, 1640995203000)
  assert_eq(time_filtered_logs.length(), 3)
  assert_eq(time_filtered_logs[0].level, "DEBUG")
  assert_eq(time_filtered_logs[1].level, "WARN")
  assert_eq(time_filtered_logs[2].level, "ERROR")
  
  // Group by attribute
  let group_by_attribute = fn(log_entries: Array[LogEntry], key: String) {
    let mut groups = []
    
    for log in log_entries {
      let mut found_group = None
      for group in groups {
        if group.0 == key {
          found_group = Some(group.1)
        }
      }
      
      match found_group {
        Some(group_logs) => {
          // Add to existing group (simplified)
          groups = groups.push((key, group_logs.push(log)))
        }
        None => {
          // Create new group (simplified)
          groups = groups.push((key, [log]))
        }
      }
    }
    
    groups
  }
  
  // Group by source
  let grouped_by_source = group_by_attribute(logs, "source")
  assert_eq(grouped_by_source.length(), 2)
}

// Test 6: Telemetry Error Handling and Recovery
test "telemetry error handling and recovery" {
  // Define error types
  enum TelemetryError {
    NetworkTimeout(String)
    SerializationError(String)
    ValidationError(String)
    ResourceExhausted(String)
  }
  
  // Define result type
  type TelemetryResult[T] = {
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
  
  // Test network timeout error
  let network_operation = fn(timeout_ms: Int) {
    if timeout_ms < 1000 {
      create_success("Network operation completed")
    } else {
      create_error(TelemetryError::NetworkTimeout("Operation timed out after " + timeout_ms.to_string() + "ms"))
    }
  }
  
  let fast_result = network_operation(500)
  assert_true(fast_result.success)
  assert_eq(fast_result.data, Some("Network operation completed"))
  
  let slow_result = network_operation(2000)
  assert_false(slow_result.success)
  match slow_result.error {
    Some(TelemetryError::NetworkTimeout(msg)) => {
      assert_true(msg.contains("2000ms"))
    }
    _ => assert_true(false)
  }
  
  // Test serialization error
  let serialize_data = fn(data: String) {
    if data.length() > 0 && data.length() <= 1000 {
      create_success("Serialized: " + data)
    } else {
      create_error(TelemetryError::SerializationError("Data length must be between 1 and 1000 characters"))
    }
  }
  
  let valid_data_result = serialize_data("Valid telemetry data")
  assert_true(valid_data_result.success)
  assert_eq(valid_data_result.data, Some("Serialized: Valid telemetry data"))
  
  let empty_data_result = serialize_data("")
  assert_false(empty_data_result.success)
  match empty_data_result.error {
    Some(TelemetryError::SerializationError(msg)) => {
      assert_true(msg.contains("Data length"))
    }
    _ => assert_true(false)
  }
  
  // Test validation error
  let validate_telemetry_id = fn(id: String) {
    if id.length() == 16 && id.starts_with("tel-") {
      create_success(id)
    } else {
      create_error(TelemetryError::ValidationError("Invalid telemetry ID format"))
    }
  }
  
  let valid_id_result = validate_telemetry_id("tel-1234567890")
  assert_true(valid_id_result.success)
  assert_eq(valid_id_result.data, Some("tel-1234567890"))
  
  let invalid_id_result = validate_telemetry_id("invalid-id")
  assert_false(invalid_id_result.success)
  match invalid_id_result.error {
    Some(TelemetryError::ValidationError(msg)) => {
      assert_true(msg.contains("Invalid telemetry ID"))
    }
    _ => assert_true(false)
  }
  
  // Test resource exhausted error
  let process_batch = fn(batch_size: Int, max_capacity: Int) {
    if batch_size <= max_capacity {
      create_success("Processed " + batch_size.to_string() + " items")
    } else {
      create_error(TelemetryError::ResourceExhausted("Batch size " + batch_size.to_string() + " exceeds max capacity " + max_capacity.to_string()))
    }
  }
  
  let normal_batch_result = process_batch(100, 500)
  assert_true(normal_batch_result.success)
  assert_eq(normal_batch_result.data, Some("Processed 100 items"))
  
  let large_batch_result = process_batch(1000, 500)
  assert_false(large_batch_result.success)
  match large_batch_result.error {
    Some(TelemetryError::ResourceExhausted(msg)) => {
      assert_true(msg.contains("1000"))
      assert_true(msg.contains("500"))
    }
    _ => assert_true(false)
  }
  
  // Test error recovery with fallback
  let operation_with_fallback = fn(primary: String, fallback: String) {
    let primary_result = validate_telemetry_id(primary)
    
    match primary_result.success {
      true => primary_result
      false => {
        let fallback_result = validate_telemetry_id(fallback)
        match fallback_result.success {
          true => fallback_result
          false => create_error(TelemetryError::ValidationError("Both primary and fallback IDs are invalid"))
        }
      }
    }
  }
  
  let recovery_result1 = operation_with_fallback("invalid", "tel-1234567890")
  assert_true(recovery_result1.success)
  assert_eq(recovery_result1.data, Some("tel-1234567890"))
  
  let recovery_result2 = operation_with_fallback("invalid1", "invalid2")
  assert_false(recovery_result2.success)
  match recovery_result2.error {
    Some(TelemetryError::ValidationError(msg)) => {
      assert_true(msg.contains("Both primary and fallback"))
    }
    _ => assert_true(false)
  }
  
  // Test error chaining
  let chain_operations = fn(data: String) {
    let validation_result = validate_telemetry_id(data)
    
    match validation_result.success {
      true => {
        let serialization_result = serialize_data(data)
        match serialization_result.success {
          true => serialization_result
          false => create_error(TelemetryError::SerializationError("Serialization failed after validation"))
        }
      }
      false => validation_result
    }
  }
  
  let chain_result1 = chain_operations("tel-1234567890")
  assert_true(chain_result1.success)
  
  let chain_result2 = chain_operations("invalid-id")
  assert_false(chain_result2.success)
  match chain_result2.error {
    Some(TelemetryError::ValidationError(_)) => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 7: Telemetry Data Caching and Performance Optimization
test "telemetry data caching and performance optimization" {
  // Define cache entry type
  type CacheEntry[T] = {
    key: String,
    value: T,
    timestamp: Int,
    ttl: Int  // Time to live in milliseconds
  }
  
  // Define cache type
  type TelemetryCache[T] = {
    entries: Array[CacheEntry[T]],
    max_size: Int,
    current_time: Int
  }
  
  // Create cache
  let create_cache = fn(max_size: Int, current_time: Int) {
    {
      entries: [],
      max_size,
      current_time
    }
  }
  
  // Get value from cache
  let cache_get = fn(cache: TelemetryCache[T], key: String) {
    let mut found = None
    
    for entry in cache.entries {
      if entry.key == key && (entry.timestamp + entry.ttl) > cache.current_time {
        found = Some(entry.value)
      }
    }
    
    found
  }
  
  // Set value in cache
  let cache_set = fn(cache: TelemetryCache[T], key: String, value: T, ttl: Int) {
    let mut new_entries = []
    
    // Remove expired entries and entries with the same key
    for entry in cache.entries {
      let is_expired = (entry.timestamp + entry.ttl) <= cache.current_time
      let is_same_key = entry.key == key
      
      if not(is_expired) && not(is_same_key) {
        new_entries = new_entries.push(entry)
      }
    }
    
    // Add new entry
    new_entries = new_entries.push({
      key,
      value,
      timestamp: cache.current_time,
      ttl
    })
    
    // If cache is full, remove oldest entry
    if new_entries.length() > cache.max_size {
      new_entries = new_entries.slice(1, new_entries.length() - 1)
    }
    
    { cache | entries: new_entries }
  }
  
  // Create a cache with max size of 3
  let cache = create_cache(3, 1640995200000)
  
  // Test cache set and get
  let cache1 = cache_set(cache, "key1", "value1", 5000)
  let value1 = cache_get(cache1, "key1")
  assert_eq(value1, Some("value1"))
  
  // Test cache miss
  let value2 = cache_get(cache1, "nonexistent")
  assert_eq(value2, None)
  
  // Test cache expiration
  let cache_with_time = { cache1 | current_time: 1640995206000 }  // 6000ms later
  let expired_value = cache_get(cache_with_time, "key1")
  assert_eq(expired_value, None)  // Should be expired
  
  // Test cache eviction
  let cache2 = cache_set(cache, "key1", "value1", 5000)
  let cache3 = cache_set(cache2, "key2", "value2", 5000)
  let cache4 = cache_set(cache3, "key3", "value3", 5000)
  let cache5 = cache_set(cache4, "key4", "value4", 5000)  // Should evict key1
  
  assert_eq(cache_get(cache5, "key1"), None)  // Should be evicted
  assert_eq(cache_get(cache5, "key2"), Some("value2"))
  assert_eq(cache_get(cache5, "key3"), Some("value3"))
  assert_eq(cache_get(cache5, "key4"), Some("value4"))
  
  // Test cache performance optimization
  type TelemetryData = {
    id: String,
    data: String,
    processed: Bool
  }
  
  // Simulate expensive data processing
  let process_data = fn(raw_data: String) {
    // Simulate expensive operation
    let hash = raw_data.length().to_string() + "-" + raw_data[0].to_string()
    {
      id: "processed-" + hash,
      data: raw_data,
      processed: true
    }
  }
  
  // Create processing cache
  let processing_cache = create_cache(10, 1640995200000)
  
  // Process data with caching
  let process_with_cache = fn(cache: TelemetryCache[TelemetryData], raw_data: String) {
    let cache_key = "data-" + raw_data.length().to_string()
    
    match cache_get(cache, cache_key) {
      Some(cached_data) => {
        // Return cached data
        (cached_data, cache)
      }
      None => {
        // Process and cache data
        let processed = process_data(raw_data)
        let updated_cache = cache_set(cache, cache_key, processed, 10000)
        (processed, updated_cache)
      }
    }
  }
  
  // First processing - should compute
  let (result1, cache_after1) = process_with_cache(processing_cache, "test data")
  assert_eq(result1.id, "processed-9-t")
  assert_true(result1.processed)
  
  // Second processing of same data - should use cache
  let (result2, cache_after2) = process_with_cache(cache_after1, "test data")
  assert_eq(result2.id, "processed-9-t")
  assert_true(result2.processed)
  
  // Different data - should compute again
  let (result3, cache_after3) = process_with_cache(cache_after2, "different data")
  assert_eq(result3.id, "processed-14-d")
  assert_true(result3.processed)
  
  // Test cache statistics
  let get_cache_stats = fn(cache: TelemetryCache[T]) {
    let total_entries = cache.entries.length()
    let mut expired_count = 0
    
    for entry in cache.entries {
      if (entry.timestamp + entry.ttl) <= cache.current_time {
        expired_count = expired_count + 1
      }
    }
    
    {
      total_entries,
      active_entries: total_entries - expired_count,
      expired_count,
      max_size: cache.max_size,
      utilization: (total_entries.to_float() / cache.max_size.to_float()) * 100.0
    }
  }
  
  let stats = get_cache_stats(cache_after3)
  assert_eq(stats.total_entries, 2)
  assert_eq(stats.active_entries, 2)
  assert_eq(stats.expired_count, 0)
  assert_eq(stats.max_size, 10)
  assert_eq(stats.utilization, 20.0)
}

// Test 8: Telemetry Data Cross-Platform Compatibility
test "telemetry data cross-platform compatibility" {
  // Define platform-agnostic telemetry data format
  type PlatformTelemetryData = {
    format_version: String,
    platform_id: String,
    timestamp: Int,
    data_type: String,
    payload: String
  }
  
  // Define platform-specific converters
  type PlatformConverter = {
    platform_id: String,
    to_platform: fn(PlatformTelemetryData) -> String,
    from_platform: fn(String) -> PlatformTelemetryData
  }
  
  // Create JSON converter
  let json_converter = {
    platform_id: "json",
    to_platform: fn(data: PlatformTelemetryData) {
      "{" +
      "\"format_version\":\"" + data.format_version + "\"," +
      "\"platform_id\":\"" + data.platform_id + "\"," +
      "\"timestamp\":" + data.timestamp.to_string() + "," +
      "\"data_type\":\"" + data.data_type + "\"," +
      "\"payload\":\"" + data.payload + "\"" +
      "}"
    },
    from_platform: fn(json_str: String) {
      // Simplified JSON parsing
      let version_start = json_str.find("\"format_version\":\"") + 18
      let version_end = json_str.find("\",", version_start)
      let version = json_str.substring(version_start, version_end - version_start)
      
      let platform_start = json_str.find("\"platform_id\":\"") + 15
      let platform_end = json_str.find("\",", platform_start)
      let platform = json_str.substring(platform_start, platform_end - platform_start)
      
      let timestamp_start = json_str.find("\"timestamp\":") + 12
      let timestamp_end = json_str.find(",", timestamp_start)
      let timestamp_str = json_str.substring(timestamp_start, timestamp_end - timestamp_start)
      let timestamp = timestamp_str.to_int()
      
      let type_start = json_str.find("\"data_type\":\"") + 13
      let type_end = json_str.find("\",", type_start)
      let data_type = json_str.substring(type_start, type_end - type_start)
      
      let payload_start = json_str.find("\"payload\":\"") + 11
      let payload_end = json_str.find("\"}", payload_start)
      let payload = json_str.substring(payload_start, payload_end - payload_start)
      
      {
        format_version: version,
        platform_id: platform,
        timestamp,
        data_type,
        payload
      }
    }
  }
  
  // Create XML converter
  let xml_converter = {
    platform_id: "xml",
    to_platform: fn(data: PlatformTelemetryData) {
      "<telemetry>" +
      "<format_version>" + data.format_version + "</format_version>" +
      "<platform_id>" + data.platform_id + "</platform_id>" +
      "<timestamp>" + data.timestamp.to_string() + "</timestamp>" +
      "<data_type>" + data.data_type + "</data_type>" +
      "<payload>" + data.payload + "</payload>" +
      "</telemetry>"
    },
    from_platform: fn(xml_str: String) {
      // Simplified XML parsing
      let version_start = xml_str.find("<format_version>") + 16
      let version_end = xml_str.find("</format_version>", version_start)
      let version = xml_str.substring(version_start, version_end - version_start)
      
      let platform_start = xml_str.find("<platform_id>") + 13
      let platform_end = xml_str.find("</platform_id>", platform_start)
      let platform = xml_str.substring(platform_start, platform_end - platform_start)
      
      let timestamp_start = xml_str.find("<timestamp>") + 11
      let timestamp_end = xml_str.find("</timestamp>", timestamp_start)
      let timestamp_str = xml_str.substring(timestamp_start, timestamp_end - timestamp_start)
      let timestamp = timestamp_str.to_int()
      
      let type_start = xml_str.find("<data_type>") + 11
      let type_end = xml_str.find("</data_type>", type_start)
      let data_type = xml_str.substring(type_start, type_end - type_start)
      
      let payload_start = xml_str.find("<payload>") + 9
      let payload_end = xml_str.find("</payload>", payload_start)
      let payload = xml_str.substring(payload_start, payload_end - payload_start)
      
      {
        format_version: version,
        platform_id: platform,
        timestamp,
        data_type,
        payload
      }
    }
  }
  
  // Create CSV converter
  let csv_converter = {
    platform_id: "csv",
    to_platform: fn(data: PlatformTelemetryData) {
      data.format_version + "," +
      data.platform_id + "," +
      data.timestamp.to_string() + "," +
      data.data_type + "," +
      data.payload
    },
    from_platform: fn(csv_str: String) {
      let fields = csv_str.split(",")
      {
        format_version: fields[0],
        platform_id: fields[1],
        timestamp: fields[2].to_int(),
        data_type: fields[3],
        payload: fields[4]
      }
    }
  }
  
  // Register converters
  let converters = [json_converter, xml_converter, csv_converter]
  
  // Create sample telemetry data
  let telemetry_data = {
    format_version: "1.0",
    platform_id: "azimuth",
    timestamp: 1640995200000,
    data_type: "span",
    payload: "database_query"
  }
  
  // Test conversion to different platforms
  for converter in converters {
    let platform_data = converter.to_platform(telemetry_data)
    assert_true(platform_data.length() > 0)
    
    // Test round-trip conversion
    let round_trip_data = converter.from_platform(platform_data)
    assert_eq(round_trip_data.format_version, telemetry_data.format_version)
    assert_eq(round_trip_data.platform_id, telemetry_data.platform_id)
    assert_eq(round_trip_data.timestamp, telemetry_data.timestamp)
    assert_eq(round_trip_data.data_type, telemetry_data.data_type)
    assert_eq(round_trip_data.payload, telemetry_data.payload)
  }
  
  // Test cross-platform conversion
  let convert_between_platforms = fn(data: PlatformTelemetryData, from_converter: PlatformConverter, to_converter: PlatformConverter) {
    let intermediate_format = from_converter.to_platform(data)
    to_converter.from_platform(intermediate_format)
  }
  
  // Convert from JSON to XML
  let json_to_xml = convert_between_platforms(telemetry_data, json_converter, xml_converter)
  assert_eq(json_to_xml.format_version, telemetry_data.format_version)
  assert_eq(json_to_xml.platform_id, telemetry_data.platform_id)
  assert_eq(json_to_xml.timestamp, telemetry_data.timestamp)
  assert_eq(json_to_xml.data_type, telemetry_data.data_type)
  assert_eq(json_to_xml.payload, telemetry_data.payload)
  
  // Convert from XML to CSV
  let xml_to_csv = convert_between_platforms(telemetry_data, xml_converter, csv_converter)
  assert_eq(xml_to_csv.format_version, telemetry_data.format_version)
  assert_eq(xml_to_csv.platform_id, telemetry_data.platform_id)
  assert_eq(xml_to_csv.timestamp, telemetry_data.timestamp)
  assert_eq(xml_to_csv.data_type, telemetry_data.data_type)
  assert_eq(xml_to_csv.payload, telemetry_data.payload)
  
  // Test platform detection
  let detect_platform = fn(data_str: String) {
    if data_str.starts_with("{") && data_str.ends_with("}") {
      "json"
    } else if data_str.starts_with("<telemetry>") && data_str.ends_with("</telemetry>") {
      "xml"
    } else if data_str.contains(",") {
      "csv"
    } else {
      "unknown"
    }
  }
  
  let json_data = json_converter.to_platform(telemetry_data)
  let xml_data = xml_converter.to_platform(telemetry_data)
  let csv_data = csv_converter.to_platform(telemetry_data)
  
  assert_eq(detect_platform(json_data), "json")
  assert_eq(detect_platform(xml_data), "xml")
  assert_eq(detect_platform(csv_data), "csv")
  
  // Test platform compatibility validation
  let validate_platform_compatibility = fn(data: PlatformTelemetryData, target_platform: String) {
    // Check if format version is supported
    let supported_versions = ["1.0", "1.1"]
    let version_supported = supported_versions.contains(data.format_version)
    
    // Check if data type is supported
    let supported_types = ["span", "metric", "log", "trace"]
    let type_supported = supported_types.contains(data.data_type)
    
    // Check platform-specific requirements
    let platform_requirements = {
      "json": true,  // JSON supports all data
      "xml": true,   // XML supports all data
      "csv": not(data.payload.contains(","))  // CSV doesn't support commas in payload
    }
    
    let platform_supported = match platform_requirements.get(target_platform) {
      Some(supported) => supported,
      None => false
    }
    
    version_supported && type_supported && platform_supported
  }
  
  assert_true(validate_platform_compatibility(telemetry_data, "json"))
  assert_true(validate_platform_compatibility(telemetry_data, "xml"))
  assert_true(validate_platform_compatibility(telemetry_data, "csv"))
  
  // Test with incompatible data
  let incompatible_data = { telemetry_data | payload: "data,with,commas" }
  assert_true(validate_platform_compatibility(incompatible_data, "json"))
  assert_true(validate_platform_compatibility(incompatible_data, "xml"))
  assert_false(validate_platform_compatibility(incompatible_data, "csv"))
}

// Test 9: Telemetry Data Security and Privacy Protection
test "telemetry data security and privacy protection" {
  // Define sensitive data types
  enum SensitiveDataType {
    PersonalInformation
    FinancialData
    HealthInformation
    Credentials
    Custom(String)
  }
  
  // Define privacy level
  enum PrivacyLevel {
    Public
    Internal
    Confidential
    Restricted
  }
  
  // Define telemetry data with privacy metadata
  type PrivateTelemetryData = {
    id: String,
    data_type: String,
    payload: String,
    privacy_level: PrivacyLevel,
    sensitive_fields: Array[String],
    encryption_required: Bool,
    retention_period: Int  // In days
  }
  
  // Define data masking rules
  type MaskingRule = {
    field_name: String,
    masking_function: String -> String
  }
  
  // Create masking functions
  let mask_email = fn(email: String) {
    let parts = email.split("@")
    if parts.length() == 2 {
      let username = parts[0]
      let domain = parts[1]
      let masked_username = if username.length() > 2 {
        username[0] + "*".repeat(username.length() - 2) + username[username.length() - 1]
      } else {
        "*".repeat(username.length())
      }
      masked_username + "@" + domain
    } else {
      "*".repeat(email.length())
    }
  }
  
  let mask_credit_card = fn(card: String) {
    if card.length() >= 4 {
      "*".repeat(card.length() - 4) + card.substring(card.length() - 4, 4)
    } else {
      "*".repeat(card.length())
    }
  }
  
  let mask_phone = fn(phone: String) {
    if phone.length() >= 4 {
      "*".repeat(phone.length() - 4) + phone.substring(phone.length() - 4, 4)
    } else {
      "*".repeat(phone.length())
    }
  }
  
  let mask_generic = fn(data: String) {
    if data.length() > 2 {
      data[0] + "*".repeat(data.length() - 2) + data[data.length() - 1]
    } else {
      "*".repeat(data.length())
    }
  }
  
  // Create masking rules
  let masking_rules = [
    { field_name: "email", masking_function: mask_email },
    { field_name: "credit_card", masking_function: mask_credit_card },
    { field_name: "phone", masking_function: mask_phone },
    { field_name: "ssn", masking_function: mask_generic }
  ]
  
  // Create sample telemetry data with sensitive information
  let sensitive_data = {
    id: "tel-1234567890",
    data_type: "user_registration",
    payload: "email:user@example.com,credit_card:1234567890123456,phone:5551234567,ssn:123456789",
    privacy_level: PrivacyLevel::Confidential,
    sensitive_fields: ["email", "credit_card", "phone", "ssn"],
    encryption_required: true,
    retention_period: 365
  }
  
  // Test data masking
  let mask_sensitive_data = fn(data: PrivateTelemetryData, rules: Array[MaskingRule]) {
    let masked_payload = data.payload
    
    // Apply masking rules to sensitive fields
    let mut result = masked_payload
    for field in data.sensitive_fields {
      let rule = rules.find(fn(r) { r.field_name == field })
      match rule {
        Some(mask_rule) => {
          // Find field value in payload
          let field_start = result.find(field + ":")
          if field_start >= 0 {
            let value_start = field_start + field.length() + 1
            let value_end = result.find(",", value_start)
            let field_value = if value_end >= 0 {
              result.substring(value_start, value_end - value_start)
            } else {
              result.substring(value_start, result.length() - value_start)
            }
            
            // Apply masking
            let masked_value = mask_rule.masking_function(field_value)
            
            // Replace in result
            result = if value_end >= 0 {
              result.substring(0, value_start) + field + ":" + masked_value + result.substring(value_end, result.length() - value_end)
            } else {
              result.substring(0, value_start) + field + ":" + masked_value
            }
          }
        }
        None => ()
      }
    }
    
    { data | payload: result }
  }
  
  // Apply masking
  let masked_data = mask_sensitive_data(sensitive_data, masking_rules)
  
  // Verify masking
  assert_true(masked_data.payload.contains("email:u***r@example.com"))
  assert_true(masked_data.payload.contains("credit_card:************3456"))
  assert_true(masked_data.payload.contains("phone:******4567"))
  assert_true(masked_data.payload.contains("ssn:1*******9"))
  
  // Test privacy level enforcement
  let can_access_data = fn(data: PrivateTelemetryData, user_privacy_level: PrivacyLevel) {
    match data.privacy_level {
      PrivacyLevel::Public => true
      PrivacyLevel::Internal => user_privacy_level != PrivacyLevel::Public
      PrivacyLevel::Confidential => user_privacy_level == PrivacyLevel::Confidential || user_privacy_level == PrivacyLevel::Restricted
      PrivacyLevel::Restricted => user_privacy_level == PrivacyLevel::Restricted
    }
  }
  
  // Test access control
  assert_true(can_access_data(sensitive_data, PrivacyLevel::Restricted))
  assert_true(can_access_data(sensitive_data, PrivacyLevel::Confidential))
  assert_false(can_access_data(sensitive_data, PrivacyLevel::Internal))
  assert_false(can_access_data(sensitive_data, PrivacyLevel::Public))
  
  // Test data encryption requirement
  let requires_encryption = fn(data: PrivateTelemetryData) {
    data.encryption_required || data.privacy_level == PrivacyLevel::Confidential || data.privacy_level == PrivacyLevel::Restricted
  }
  
  assert_true(requires_encryption(sensitive_data))
  
  // Test with less sensitive data
  let public_data = {
    id: "tel-0987654321",
    data_type: "system_metrics",
    payload: "cpu:75,memory:60,disk:40",
    privacy_level: PrivacyLevel::Public,
    sensitive_fields: [],
    encryption_required: false,
    retention_period: 30
  }
  
  assert_false(requires_encryption(public_data))
  
  // Test retention policy
  let is_retention_expired = fn(data: PrivateTelemetryData, current_timestamp: Int) {
    let creation_timestamp = 1640995200000  // Assume creation time
    let retention_ms = data.retention_period * 24 * 60 * 60 * 1000  // Convert days to milliseconds
    let expiration_timestamp = creation_timestamp + retention_ms
    
    current_timestamp > expiration_timestamp
  }
  
  // Test with current time within retention period
  assert_false(is_retention_expired(sensitive_data, 1640995200000 + 100 * 24 * 60 * 60 * 1000))  // 100 days later
  
  // Test with current time beyond retention period
  assert_true(is_retention_expired(sensitive_data, 1640995200000 + 400 * 24 * 60 * 60 * 1000))  // 400 days later
  
  // Test data anonymization
  let anonymize_data = fn(data: PrivateTelemetryData) {
    let anonymized_id = "anon-" + data.id.substring(data.id.length() - 4, 4)
    let anonymized_payload = data.payload
    
    { data | 
      id: anonymized_id,
      payload: anonymized_payload,
      privacy_level: PrivacyLevel::Internal  // Downgrade privacy level
    }
  }
  
  let anonymized = anonymize_data(sensitive_data)
  assert_eq(anonymized.id, "anon-7890")
  assert_eq(anonymized.privacy_level, PrivacyLevel::Internal)
  
  // Test audit logging
  type AuditLog = {
    timestamp: Int,
    user_id: String,
    action: String,
    data_id: String,
    access_granted: Bool,
    reason: String
  }
  
  let mut audit_logs = []
  
  let log_access = fn(logs: Array[AuditLog], user_id: String, data_id: String, action: String, granted: Bool, reason: String) {
    let log = {
      timestamp: 1640995200000,
      user_id,
      action,
      data_id,
      access_granted: granted,
      reason
    }
    logs.push(log)
  }
  
  // Log access attempts
  audit_logs = log_access(audit_logs, "user-123", sensitive_data.id, "read", false, "Insufficient privacy level")
  audit_logs = log_access(audit_logs, "admin-456", sensitive_data.id, "read", true, "Admin access")
  
  assert_eq(audit_logs.length(), 2)
  assert_false(audit_logs[0].access_granted)
  assert_true(audit_logs[1].access_granted)
  assert_eq(audit_logs[0].reason, "Insufficient privacy level")
  assert_eq(audit_logs[1].reason, "Admin access")
}

// Test 10: Advanced Telemetry Pattern Matching
test "advanced telemetry pattern matching" {
  // Define telemetry event types
  enum TelemetryEventType {
    SpanStart
    SpanEnd
    Metric
    Log
    Trace
    Error
  }
  
  // Define telemetry severity levels
  enum SeverityLevel {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // Define telemetry event with complex structure
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: TelemetryEventType,
    severity: SeverityLevel,
    service: String,
    message: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample events with different types and severities
  let events = [
    {
      id: "evt-1",
      timestamp: 1640995200000,
      event_type: TelemetryEventType::SpanStart,
      severity: SeverityLevel::Info,
      service: "api-gateway",
      message: "Request processing started",
      attributes: [("request_id", "req-123"), ("method", "GET")]
    },
    {
      id: "evt-2",
      timestamp: 1640995201000,
      event_type: TelemetryEventType::Metric,
      severity: SeverityLevel::Info,
      service: "database",
      message: "Query execution time",
      attributes: [("duration_ms", "150"), ("query_type", "SELECT")]
    },
    {
      id: "evt-3",
      timestamp: 1640995202000,
      event_type: TelemetryEventType::Log,
      severity: SeverityLevel::Warning,
      service: "cache-service",
      message: "Cache miss rate high",
      attributes: [("miss_rate", "0.25"), ("threshold", "0.20")]
    },
    {
      id: "evt-4",
      timestamp: 1640995203000,
      event_type: TelemetryEventType::Error,
      severity: SeverityLevel::Error,
      service: "payment-service",
      message: "Payment processing failed",
      attributes: [("error_code", "PAY001"), ("retry_count", "3")]
    },
    {
      id: "evt-5",
      timestamp: 1640995204000,
      event_type: TelemetryEventType::SpanEnd,
      severity: SeverityLevel::Info,
      service: "api-gateway",
      message: "Request processing completed",
      attributes: [("request_id", "req-123"), ("status", "500")]
    }
  ]
  
  // Test complex pattern matching
  let categorize_event = fn(event: TelemetryEvent) {
    match (event.event_type, event.severity) {
      (TelemetryEventType::SpanStart, SeverityLevel::Info) => "Normal operation start"
      (TelemetryEventType::SpanEnd, SeverityLevel::Info) => "Normal operation end"
      (TelemetryEventType::Metric, SeverityLevel::Info) => "Performance metric"
      (TelemetryEventType::Log, SeverityLevel::Warning) => "Warning condition"
      (TelemetryEventType::Error, SeverityLevel::Error) => "Error condition"
      (TelemetryEventType::Error, SeverityLevel::Critical) => "Critical error"
      (TelemetryEventType::Log, SeverityLevel::Debug) => "Debug information"
      (event_type, severity) => "Other event: " + event_type.to_string() + " with " + severity.to_string()
    }
  }
  
  // Test categorization
  assert_eq(categorize_event(events[0]), "Normal operation start")
  assert_eq(categorize_event(events[1]), "Performance metric")
  assert_eq(categorize_event(events[2]), "Warning condition")
  assert_eq(categorize_event(events[3]), "Error condition")
  assert_eq(categorize_event(events[4]), "Normal operation end")
  
  // Test pattern matching with guards
  let get_priority = fn(event: TelemetryEvent) {
    match event.severity {
      SeverityLevel::Critical => 1
      SeverityLevel::Error => 2
      SeverityLevel::Warning => 3
      SeverityLevel::Info => {
        if event.event_type == TelemetryEventType::Error {
          2  // Error events are higher priority even if Info severity
        } else {
          4
        }
      }
      SeverityLevel::Debug => 5
    }
  }
  
  // Test priority calculation
  assert_eq(get_priority(events[0]), 4)  // Info severity, not Error
  assert_eq(get_priority(events[1]), 4)  // Info severity, not Error
  assert_eq(get_priority(events[2]), 3)  // Warning severity
  assert_eq(get_priority(events[3]), 2)  // Error severity
  assert_eq(get_priority(events[4]), 4)  // Info severity, not Error
  
  // Test pattern matching with complex conditions
  let analyze_event_pattern = fn(event: TelemetryEvent) {
    match (event.service, event.event_type, event.severity) {
      ("api-gateway", TelemetryEventType::SpanStart, _) => "API request started"
      ("api-gateway", TelemetryEventType::SpanEnd, SeverityLevel::Info) => "API request completed successfully"
      ("api-gateway", TelemetryEventType::SpanEnd, SeverityLevel::Error) => "API request completed with error"
      ("database", TelemetryEventType::Metric, _) => "Database performance metric"
      ("database", TelemetryEventType::Error, _) => "Database error"
      ("payment-service", TelemetryEventType::Error, SeverityLevel::Error) => "Payment processing error"
      ("cache-service", TelemetryEventType::Log, SeverityLevel::Warning) => "Cache performance warning"
      (service, event_type, severity) => service + " " + event_type.to_string() + " with " + severity.to_string()
    }
  }
  
  // Test pattern analysis
  assert_eq(analyze_event_pattern(events[0]), "API request started")
  assert_eq(analyze_event_pattern(events[1]), "Database performance metric")
  assert_eq(analyze_event_pattern(events[2]), "Cache performance warning")
  assert_eq(analyze_event_pattern(events[3]), "Payment processing error")
  assert_eq(analyze_event_pattern(events[4]), "API request completed with error")  // Status 500 indicates error
  
  // Test pattern matching with attribute inspection
  let analyze_by_attributes = fn(event: TelemetryEvent) {
    let get_attr = fn(key: String) {
      let mut found = None
      for attr in event.attributes {
        if attr.0 == key {
          found = Some(attr.1)
        }
      }
      found
    }
    
    match (event.event_type, get_attr("duration_ms")) {
      (TelemetryEventType::Metric, Some(duration)) => {
        let duration_val = duration.to_int()
        if duration_val < 100 {
          "Fast operation"
        } else if duration_val < 500 {
          "Normal operation"
        } else {
          "Slow operation"
        }
      }
      (TelemetryEventType::Log, Some(miss_rate)) => {
        let rate = miss_rate.to_float()
        if rate > 0.3 {
          "Critical cache performance"
        } else if rate > 0.2 {
          "Warning cache performance"
        } else {
          "Normal cache performance"
        }
      }
      (TelemetryEventType::Error, Some(error_code)) => {
        if error_code.starts_with("PAY") {
          "Payment error"
        } else if error_code.starts_with("DB") {
          "Database error"
        } else {
          "Unknown error type"
        }
      }
      _ => "No specific analysis available"
    }
  }
  
  // Test attribute-based analysis
  assert_eq(analyze_by_attributes(events[1]), "Normal operation")  // 150ms duration
  assert_eq(analyze_by_attributes(events[2]), "Warning cache performance")  // 0.25 miss rate
  assert_eq(analyze_by_attributes(events[3]), "Payment error")  // PAY001 error code
  assert_eq(analyze_by_attributes(events[0]), "No specific analysis available")
  
  // Test pattern matching for event correlation
  let correlate_events = fn(events: Array[TelemetryEvent]) {
    let mut correlations = []
    
    // Find matching request IDs
    let request_ids = []
    for event in events {
      for attr in event.attributes {
        if attr.0 == "request_id" {
          if not(request_ids.contains(attr.1)) {
            request_ids = request_ids.push(attr.1)
          }
        }
      }
    }
    
    // Create correlations for each request ID
    for req_id in request_ids {
      let related_events = []
      for event in events {
        for attr in event.attributes {
          if attr.0 == "request_id" && attr.1 == req_id {
            related_events = related_events.push(event)
          }
        }
      }
      
      if related_events.length() > 1 {
        correlations = correlations.push({
          request_id: req_id,
          events: related_events,
          start_time: related_events[0].timestamp,
          end_time: related_events[related_events.length() - 1].timestamp,
          duration: related_events[related_events.length() - 1].timestamp - related_events[0].timestamp
        })
      }
    }
    
    correlations
  }
  
  // Test event correlation
  let correlations = correlate_events(events)
  assert_eq(correlations.length(), 1)
  assert_eq(correlations[0].request_id, "req-123")
  assert_eq(correlations[0].events.length(), 2)
  assert_eq(correlations[0].duration, 4000)  // 4 seconds between start and end
  
  // Test pattern matching for anomaly detection
  let detect_anomaly = fn(event: TelemetryEvent, baseline: Array[TelemetryEvent]) {
    // Get similar events from baseline
    let similar_events = []
    for base_event in baseline {
      if base_event.service == event.service && base_event.event_type == event.event_type {
        similar_events = similar_events.push(base_event)
      }
    }
    
    if similar_events.length() == 0 {
      "No baseline data"
    } else {
      // Check for anomalies based on event type
      match event.event_type {
        TelemetryEventType::Metric => {
          let get_duration = fn(e: TelemetryEvent) {
            for attr in e.attributes {
              if attr.0 == "duration_ms" {
                return Some(attr.1.to_int())
              }
            }
            None
          }
          
          match get_duration(event) {
            Some(current_duration) => {
              let mut baseline_durations = []
              for base_event in similar_events {
                match get_duration(base_event) {
                  Some(duration) => baseline_durations = baseline_durations.push(duration),
                  None => ()
                }
              }
              
              if baseline_durations.length() > 0 {
                let sum = baseline_durations.reduce(fn(acc, x) { acc + x }, 0)
                let avg = sum / baseline_durations.length()
                
                if current_duration > avg * 2 {
                  "Performance anomaly: duration " + current_duration.to_string() + "ms is 2x higher than baseline " + avg.to_string() + "ms"
                } else if current_duration < avg / 2 {
                  "Performance anomaly: duration " + current_duration.to_string() + "ms is 2x lower than baseline " + avg.to_string() + "ms"
                } else {
                  "Normal performance"
                }
              } else {
                "No baseline metrics"
              }
            }
            None => "No duration metric"
          }
        }
        TelemetryEventType::Error => {
          let error_count = similar_events.length()
          if error_count > 5 {
            "High error frequency: " + error_count.to_string() + " errors in baseline"
          } else {
            "Normal error frequency"
          }
        }
        _ => "No anomaly detection for this event type"
      }
    }
  }
  
  // Test anomaly detection
  let baseline_events = [
    {
      id: "base-1",
      timestamp: 1640995100000,
      event_type: TelemetryEventType::Metric,
      severity: SeverityLevel::Info,
      service: "database",
      message: "Query execution time",
      attributes: [("duration_ms", "80"), ("query_type", "SELECT")]
    },
    {
      id: "base-2",
      timestamp: 1640995101000,
      event_type: TelemetryEventType::Metric,
      severity: SeverityLevel::Info,
      service: "database",
      message: "Query execution time",
      attributes: [("duration_ms", "75"), ("query_type", "SELECT")]
    },
    {
      id: "base-3",
      timestamp: 1640995102000,
      event_type: TelemetryEventType::Metric,
      severity: SeverityLevel::Info,
      service: "database",
      message: "Query execution time",
      attributes: [("duration_ms", "85"), ("query_type", "SELECT")]
    }
  ]
  
  let anomaly_result = detect_anomaly(events[1], baseline_events)
  assert_eq(anomaly_result, "Performance anomaly: duration 150ms is 2x higher than baseline 80ms")
}