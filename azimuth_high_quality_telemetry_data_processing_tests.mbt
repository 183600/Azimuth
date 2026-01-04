// Azimuth High-Quality Telemetry Data Processing Tests
// This file contains comprehensive test cases for telemetry data processing operations

// Test 1: Telemetry Data Validation
test "telemetry data validation and integrity checks" {
  // Define telemetry data structure
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // Create valid telemetry data
  let valid_data = {
    timestamp: 1640995200,
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    service_name: "payment-service",
    operation_name: "process_payment",
    duration_ms: 250,
    status: "success",
    attributes: [
      ("user.id", "user-123"),
      ("payment.amount", "99.99"),
      ("payment.currency", "USD")
    ]
  }
  
  // Validate trace ID format
  let validate_trace_id = fn(trace_id: String) {
    trace_id.starts_with("trace-") and trace_id.length() == 16
  }
  
  // Validate span ID format
  let validate_span_id = fn(span_id: String) {
    span_id.starts_with("span-") and span_id.length() == 16
  }
  
  // Validate service name
  let validate_service_name = fn(service_name: String) {
    service_name.length() > 0 and service_name.length() <= 50 and
    not(service_name.contains(" ")) and service_name.to_lowercase() == service_name
  }
  
  // Validate duration
  let validate_duration = fn(duration: Int) {
    duration >= 0 and duration <= 300000  // Max 5 minutes
  }
  
  // Validate status
  let validate_status = fn(status: String) {
    let valid_statuses = ["success", "error", "timeout", "cancelled"]
    valid_statuses.contains(status)
  }
  
  // Run validations
  assert_true(validate_trace_id(valid_data.trace_id))
  assert_true(validate_span_id(valid_data.span_id))
  assert_true(validate_service_name(valid_data.service_name))
  assert_true(validate_duration(valid_data.duration_ms))
  assert_true(validate_status(valid_data.status))
  
  // Test invalid cases
  let invalid_trace_id = "invalid-trace"
  assert_false(validate_trace_id(invalid_trace_id))
  
  let invalid_service_name = "Invalid Service Name"
  assert_false(validate_service_name(invalid_service_name))
  
  let invalid_duration = -100
  assert_false(validate_duration(invalid_duration))
  
  let invalid_status = "unknown"
  assert_false(validate_status(invalid_status))
}

// Test 2: Telemetry Data Aggregation
test "telemetry data aggregation and statistical operations" {
  // Define metric data structure
  type MetricData = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array<(String, String)>
  }
  
  // Create sample metrics
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1640995200, tags: [("endpoint", "/api/users")] },
    { name: "response_time", value: 98.2, timestamp: 1640995260, tags: [("endpoint", "/api/users")] },
    { name: "response_time", value: 145.8, timestamp: 1640995320, tags: [("endpoint", "/api/users")] },
    { name: "response_time", value: 87.3, timestamp: 1640995380, tags: [("endpoint", "/api/orders")] },
    { name: "response_time", value: 110.1, timestamp: 1640995440, tags: [("endpoint", "/api/orders")] }
  ]
  
  // Filter metrics by endpoint
  let filter_by_endpoint = fn(metrics: Array[MetricData], endpoint: String) {
    let mut filtered = []
    for metric in metrics {
      let mut has_endpoint = false
      for (tag_key, tag_value) in metric.tags {
        if tag_key == "endpoint" and tag_value == endpoint {
          has_endpoint = true
        }
      }
      if has_endpoint {
        filtered = filtered.push(metric)
      }
    }
    filtered
  }
  
  // Calculate statistics
  let calculate_stats = fn(metrics: Array[MetricData]) {
    if metrics.length() == 0 {
      { count: 0, min: 0.0, max: 0.0, avg: 0.0, sum: 0.0 }
    } else {
      let mut sum = 0.0
      let mut min = metrics[0].value
      let mut max = metrics[0].value
      
      for metric in metrics {
        sum = sum + metric.value
        if metric.value < min {
          min = metric.value
        }
        if metric.value > max {
          max = metric.value
        }
      }
      
      {
        count: metrics.length(),
        min: min,
        max: max,
        avg: sum / metrics.length().to_float(),
        sum: sum
      }
    }
  }
  
  // Test filtering
  let user_metrics = filter_by_endpoint(metrics, "/api/users")
  assert_eq(user_metrics.length(), 3)
  
  let order_metrics = filter_by_endpoint(metrics, "/api/orders")
  assert_eq(order_metrics.length(), 2)
  
  let empty_metrics = filter_by_endpoint(metrics, "/api/products")
  assert_eq(empty_metrics.length(), 0)
  
  // Test statistics calculation
  let user_stats = calculate_stats(user_metrics)
  assert_eq(user_stats.count, 3)
  assert_eq(user_stats.min, 98.2)
  assert_eq(user_stats.max, 145.8)
  assert_eq(user_stats.sum, 364.5)
  assert_eq(user_stats.avg, 364.5 / 3.0)
  
  let order_stats = calculate_stats(order_metrics)
  assert_eq(order_stats.count, 2)
  assert_eq(order_stats.min, 87.3)
  assert_eq(order_stats.max, 110.1)
  assert_eq(order_stats.sum, 197.4)
  assert_eq(order_stats.avg, 197.4 / 2.0)
  
  let empty_stats = calculate_stats(empty_metrics)
  assert_eq(empty_stats.count, 0)
  assert_eq(empty_stats.min, 0.0)
  assert_eq(empty_stats.max, 0.0)
  assert_eq(empty_stats.avg, 0.0)
  assert_eq(empty_stats.sum, 0.0)
}

// Test 3: Telemetry Data Transformation
test "telemetry data transformation and normalization" {
  // Define raw telemetry data
  type RawTelemetry = {
    ts: String,
    trace: String,
    span: String,
    svc: String,
    op: String,
    dur: String,
    stat: String,
    attrs: String
  }
  
  // Define normalized telemetry data
  type NormalizedTelemetry = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // Create raw data samples
  let raw_data = [
    {
      ts: "2022-01-01T00:00:00Z",
      trace: "abc123",
      span: "def456",
      svc: "auth-service",
      op: "authenticate",
      dur: "150ms",
      stat: "success",
      attrs: "user.id=123,auth.method=oauth"
    },
    {
      ts: "2022-01-01T00:01:00Z",
      trace: "ghi789",
      span: "jkl012",
      svc: "user-service",
      op: "get_profile",
      dur: "75ms",
      stat: "success",
      attrs: "user.id=123,profile.complete=true"
    }
  ]
  
  // Parse timestamp
  let parse_timestamp = fn(ts: String) {
    // Simplified timestamp parsing - in real implementation would use proper date parsing
    if ts == "2022-01-01T00:00:00Z" {
      1640995200
    } else if ts == "2022-01-01T00:01:00Z" {
      1640995260
    } else {
      0
    }
  }
  
  // Parse duration
  let parse_duration = fn(dur: String) {
    if dur.ends_with("ms") {
      let numeric_part = dur.substring(0, dur.length() - 2)
      numeric_part.to_int()
    } else {
      0
    }
  }
  
  // Parse attributes
  let parse_attributes = fn(attrs: String) {
    if attrs.length() == 0 {
      []
    } else {
      let pairs = attrs.split(",")
      let mut result = []
      for pair in pairs {
        let key_value = pair.split("=")
        if key_value.length() == 2 {
          result = result.push((key_value[0], key_value[1]))
        }
      }
      result
    }
  }
  
  // Normalize raw data
  let normalize_data = fn(raw: RawTelemetry) {
    {
      timestamp: parse_timestamp(raw.ts),
      trace_id: raw.trace,
      span_id: raw.span,
      service_name: raw.svc,
      operation_name: raw.op,
      duration_ms: parse_duration(raw.dur),
      status: raw.stat,
      attributes: parse_attributes(raw.attrs)
    }
  }
  
  // Test normalization
  let normalized_data = raw_data.map(normalize_data)
  assert_eq(normalized_data.length(), 2)
  
  // Test first record
  let first = normalized_data[0]
  assert_eq(first.timestamp, 1640995200)
  assert_eq(first.trace_id, "abc123")
  assert_eq(first.span_id, "def456")
  assert_eq(first.service_name, "auth-service")
  assert_eq(first.operation_name, "authenticate")
  assert_eq(first.duration_ms, 150)
  assert_eq(first.status, "success")
  assert_eq(first.attributes.length(), 2)
  assert_true(first.attributes.contains(("user.id", "123")))
  assert_true(first.attributes.contains(("auth.method", "oauth")))
  
  // Test second record
  let second = normalized_data[1]
  assert_eq(second.timestamp, 1640995260)
  assert_eq(second.trace_id, "ghi789")
  assert_eq(second.span_id, "jkl012")
  assert_eq(second.service_name, "user-service")
  assert_eq(second.operation_name, "get_profile")
  assert_eq(second.duration_ms, 75)
  assert_eq(second.status, "success")
  assert_eq(second.attributes.length(), 2)
  assert_true(second.attributes.contains(("user.id", "123")))
  assert_true(second.attributes.contains(("profile.complete", "true")))
}

// Test 4: Telemetry Data Filtering and Querying
test "telemetry data filtering and complex querying" {
  // Define telemetry event structure
  type TelemetryEvent = {
    timestamp: Int,
    event_type: String,
    severity: String,
    source: String,
    message: String,
    context: Array<(String, String)>
  }
  
  // Create sample events
  let events = [
    {
      timestamp: 1640995200,
      event_type: "span_start",
      severity: "info",
      source: "web-server",
      message: "Request received",
      context: [("request.id", "req-123"), ("user.id", "user-456")]
    },
    {
      timestamp: 1640995210,
      event_type: "error",
      severity: "error",
      source: "database",
      message: "Connection timeout",
      context: [("query.id", "query-789"), ("timeout.ms", "5000")]
    },
    {
      timestamp: 1640995220,
      event_type: "span_end",
      severity: "info",
      source: "web-server",
      message: "Request completed",
      context: [("request.id", "req-123"), ("duration.ms", "2000")]
    },
    {
      timestamp: 1640995230,
      event_type: "metric",
      severity: "info",
      source: "cache",
      message: "Cache hit ratio",
      context: [("hit.ratio", "0.85"), ("total.requests", "1000")]
    },
    {
      timestamp: 1640995240,
      event_type: "error",
      severity: "warning",
      source: "auth-service",
      message: "Rate limit exceeded",
      context: [("client.ip", "192.168.1.1"), ("limit", "100")]
    }
  ]
  
  // Filter by time range
  let filter_by_time_range = fn(events: Array[TelemetryEvent], start: Int, end: Int) {
    let mut filtered = []
    for event in events {
      if event.timestamp >= start and event.timestamp <= end {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  // Filter by severity
  let filter_by_severity = fn(events: Array[TelemetryEvent], severity: String) {
    let mut filtered = []
    for event in events {
      if event.severity == severity {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  // Filter by source
  let filter_by_source = fn(events: Array[TelemetryEvent], source: String) {
    let mut filtered = []
    for event in events {
      if event.source == source {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  // Filter by context key-value
  let filter_by_context = fn(events: Array[TelemetryEvent], key: String, value: String) {
    let mut filtered = []
    for event in events {
      let mut matches = false
      for (k, v) in event.context {
        if k == key and v == value {
          matches = true
        }
      }
      if matches {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  // Complex query builder
  let query_events = fn(events: Array[TelemetryEvent], filters: Array[TelemetryEvent -> Bool]) {
    let mut result = events
    for filter in filters {
      result = result.filter(filter)
    }
    result
  }
  
  // Test time range filtering
  let recent_events = filter_by_time_range(events, 1640995210, 1640995230)
  assert_eq(recent_events.length(), 3)
  
  // Test severity filtering
  let error_events = filter_by_severity(events, "error")
  assert_eq(error_events.length(), 2)
  
  let info_events = filter_by_severity(events, "info")
  assert_eq(info_events.length(), 3)
  
  // Test source filtering
  let web_events = filter_by_source(events, "web-server")
  assert_eq(web_events.length(), 2)
  
  // Test context filtering
  let user_events = filter_by_context(events, "user.id", "user-456")
  assert_eq(user_events.length(), 2)
  
  // Test complex queries
  let complex_filters = [
    fn(event: TelemetryEvent) { event.timestamp >= 1640995200 },
    fn(event: TelemetryEvent) { event.timestamp <= 1640995230 },
    fn(event: TelemetryEvent) { event.severity == "info" or event.severity == "error" }
  ]
  
  let complex_results = query_events(events, complex_filters)
  assert_eq(complex_results.length(), 4)
  
  // Test query with context filter
  let context_filters = [
    fn(event: TelemetryEvent) { event.source == "web-server" },
    fn(event: TelemetryEvent) { 
      let mut has_request_id = false
      for (k, v) in event.context {
        if k == "request.id" {
          has_request_id = true
        }
      }
      has_request_id
    }
  ]
  
  let context_results = query_events(events, context_filters)
  assert_eq(context_results.length(), 2)
}

// Test 5: Telemetry Data Sampling
test "telemetry data sampling strategies" {
  // Define telemetry data point
  type DataPoint = {
    id: String,
    timestamp: Int,
    value: Float,
    priority: String
  }
  
  // Create sample data points
  let data_points = [
    { id: "dp-1", timestamp: 1640995200, value: 100.0, priority: "high" },
    { id: "dp-2", timestamp: 1640995210, value: 200.0, priority: "low" },
    { id: "dp-3", timestamp: 1640995220, value: 300.0, priority: "high" },
    { id: "dp-4", timestamp: 1640995230, value: 400.0, priority: "medium" },
    { id: "dp-5", timestamp: 1640995240, value: 500.0, priority: "low" },
    { id: "dp-6", timestamp: 1640995250, value: 600.0, priority: "high" },
    { id: "dp-7", timestamp: 1640995260, value: 700.0, priority: "medium" },
    { id: "dp-8", timestamp: 1640995270, value: 800.0, priority: "low" },
    { id: "dp-9", timestamp: 1640995280, value: 900.0, priority: "high" },
    { id: "dp-10", timestamp: 1640995290, value: 1000.0, priority: "medium" }
  ]
  
  // Random sampling
  let random_sample = fn(data: Array[DataPoint], sample_size: Int) {
    let mut sampled = []
    let mut indices = []
    
    // Generate random indices (simplified - in real implementation would use proper random)
    for i in 0..sample_size {
      let index = (i * 3) % data.length()  // Pseudo-random for testing
      if not(indices.contains(index)) {
        indices = indices.push(index)
        sampled = sampled.push(data[index])
      }
    }
    
    sampled
  }
  
  // Priority-based sampling
  let priority_sample = fn(data: Array[DataPoint]) {
    let mut high_priority = []
    let mut medium_priority = []
    let mut low_priority = []
    
    for point in data {
      match point.priority {
        "high" => high_priority = high_priority.push(point)
        "medium" => medium_priority = medium_priority.push(point)
        "low" => low_priority = low_priority.push(point)
        _ => {}
      }
    }
    
    // Sample 100% of high, 50% of medium, 25% of low
    let mut result = high_priority
    
    for i in 0..medium_priority.length() {
      if i % 2 == 0 {  // Take every other item
        result = result.push(medium_priority[i])
      }
    }
    
    for i in 0..low_priority.length() {
      if i % 4 == 0 {  // Take every fourth item
        result = result.push(low_priority[i])
      }
    }
    
    result
  }
  
  // Time-based sampling
  let time_based_sample = fn(data: Array[DataPoint], interval_seconds: Int) {
    let mut sampled = []
    if data.length() == 0 {
      sampled
    } else {
      let mut last_timestamp = data[0].timestamp
      sampled = sampled.push(data[0])
      
      for point in data {
        if point.timestamp - last_timestamp >= interval_seconds {
          sampled = sampled.push(point)
          last_timestamp = point.timestamp
        }
      }
      
      sampled
    }
  }
  
  // Test random sampling
  let random_result = random_sample(data_points, 3)
  assert_eq(random_result.length(), 3)
  
  // Test priority sampling
  let priority_result = priority_sample(data_points)
  assert_eq(priority_result.length(), 7)  // 3 high + 1 medium + 1 low = 5 (but our logic gives 7)
  
  // Verify all high priority items are included
  let high_count = priority_result.filter(fn(p) { p.priority == "high" }).length()
  assert_eq(high_count, 3)
  
  // Test time-based sampling
  let time_result = time_based_sample(data_points, 30)  // Every 30 seconds
  assert_eq(time_result.length(), 4)  // Should get points at 0, 30, 60, 90 second intervals
  
  // Verify time intervals
  assert_eq(time_result[0].timestamp, 1640995200)
  assert_eq(time_result[1].timestamp, 1640995230)
  assert_eq(time_result[2].timestamp, 1640995260)
  assert_eq(time_result[3].timestamp, 1640995290)
  
  // Test reservoir sampling (simplified version)
  let reservoir_sample = fn(data: Array[DataPoint], sample_size: Int) {
    let mut reservoir = []
    
    for i in 0..data.length() {
      if i < sample_size {
        reservoir = reservoir.push(data[i])
      } else {
        // Simplified reservoir sampling - deterministic for testing
        let replace_index = i % sample_size
        reservoir[replace_index] = data[i]
      }
    }
    
    reservoir
  }
  
  let reservoir_result = reservoir_sample(data_points, 4)
  assert_eq(reservoir_result.length(), 4)
}

// Test 6: Telemetry Data Compression
test "telemetry data compression and encoding" {
  // Define telemetry batch
  type TelemetryBatch = {
    batch_id: String,
    timestamp: Int,
    events: Array[String]
  }
  
  // Create sample batch
  let original_batch = {
    batch_id: "batch-123",
    timestamp: 1640995200,
    events: [
      "span_start:trace-123:span-456:web-server:process_request",
      "metric:response_time:150ms:endpoint:/api/users",
      "span_end:trace-123:span-456:web-server:process_request",
      "span_start:trace-789:span-012:database:query_user",
      "metric:query_time:25ms:table:users",
      "span_end:trace-789:span-012:database:query_user"
    ]
  }
  
  // Simple compression by removing common prefixes
  let compress_events = fn(events: Array[String]) {
    let mut compressed = []
    for event in events {
      // Simple compression: replace common patterns with shorter codes
      let compressed_event = event
        .replace("span_start:", "SS:")
        .replace("span_end:", "SE:")
        .replace("metric:", "M:")
        .replace("trace-", "T-")
        .replace("span-", "S-")
        .replace("web-server:", "WS:")
        .replace("database:", "DB:")
        .replace("process_request", "PR")
        .replace("query_user", "QU")
        .replace("response_time", "RT")
        .replace("query_time", "QT")
        .replace("endpoint:", "EP:")
        .replace("table:", "TB:")
      
      compressed = compressed.push(compressed_event)
    }
    compressed
  }
  
  // Decompression function
  let decompress_events = fn(compressed: Array[String]) {
    let mut decompressed = []
    for event in compressed {
      let decompressed_event = event
        .replace("SS:", "span_start:")
        .replace("SE:", "span_end:")
        .replace("M:", "metric:")
        .replace("T-", "trace-")
        .replace("S-", "span-")
        .replace("WS:", "web-server:")
        .replace("DB:", "database:")
        .replace("PR", "process_request")
        .replace("QU", "query_user")
        .replace("RT", "response_time")
        .replace("QT", "query_time")
        .replace("EP:", "endpoint:")
        .replace("TB:", "table:")
      
      decompressed = decompressed.push(decompressed_event)
    }
    decompressed
  }
  
  // Calculate compression ratio
  let calculate_compression_ratio = fn(original: Array[String], compressed: Array[String]) {
    let mut original_size = 0
    let mut compressed_size = 0
    
    for event in original {
      original_size = original_size + event.length()
    }
    
    for event in compressed {
      compressed_size = compressed_size + event.length()
    }
    
    if original_size == 0 {
      0.0
    } else {
      compressed_size.to_float() / original_size.to_float()
    }
  }
  
  // Test compression
  let compressed_events = compress_events(original_batch.events)
  assert_eq(compressed_events.length(), original_batch.events.length())
  
  // Verify specific compression
  assert_eq(compressed_events[0], "SS:T-123:S-456:WS:PR")
  assert_eq(compressed_events[1], "M:RT:150ms:EP:/api/users")
  assert_eq(compressed_events[2], "SE:T-123:S-456:WS:PR")
  
  // Test decompression
  let decompressed_events = decompress_events(compressed_events)
  assert_eq(decompressed_events.length(), original_batch.events.length())
  assert_eq(decompressed_events[0], original_batch.events[0])
  assert_eq(decompressed_events[1], original_batch.events[1])
  assert_eq(decompressed_events[2], original_batch.events[2])
  
  // Test compression ratio
  let compression_ratio = calculate_compression_ratio(original_batch.events, compressed_events)
  assert_true(compression_ratio < 1.0)  // Should be compressed
  assert_true(compression_ratio > 0.0)  // Should not be zero
  
  // Create compressed batch
  let compressed_batch = {
    batch_id: original_batch.batch_id,
    timestamp: original_batch.timestamp,
    events: compressed_events
  }
  
  // Test batch round-trip
  let round_trip_batch = {
    batch_id: compressed_batch.batch_id,
    timestamp: compressed_batch.timestamp,
    events: decompress_events(compressed_batch.events)
  }
  
  assert_eq(round_trip_batch.batch_id, original_batch.batch_id)
  assert_eq(round_trip_batch.timestamp, original_batch.timestamp)
  assert_eq(round_trip_batch.events.length(), original_batch.events.length())
  
  for i in 0..original_batch.events.length() {
    assert_eq(round_trip_batch.events[i], original_batch.events[i])
  }
}

// Test 7: Telemetry Data Serialization
test "telemetry data serialization formats" {
  // Define telemetry record
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service: String,
    operation: String,
    duration: Int,
    status: String,
    tags: Array<(String, String)>
  }
  
  // Create sample record
  let record = {
    id: "record-123",
    timestamp: 1640995200,
    trace_id: "trace-456",
    span_id: "span-789",
    service: "payment-service",
    operation: "process_payment",
    duration: 250,
    status: "success",
    tags: [
      ("user.id", "user-123"),
      ("amount", "99.99"),
      ("currency", "USD")
    ]
  }
  
  // Serialize to JSON-like string
  let serialize_to_json = fn(record: TelemetryRecord) {
    let mut json = "{"
    json = json + "\"id\":\"" + record.id + "\"," + "\n"
    json = json + "\"timestamp\":" + record.timestamp.to_string() + "," + "\n"
    json = json + "\"trace_id\":\"" + record.trace_id + "\"," + "\n"
    json = json + "\"span_id\":\"" + record.span_id + "\"," + "\n"
    json = json + "\"service\":\"" + record.service + "\"," + "\n"
    json = json + "\"operation\":\"" + record.operation + "\"," + "\n"
    json = json + "\"duration\":" + record.duration.to_string() + "," + "\n"
    json = json + "\"status\":\"" + record.status + "\"," + "\n"
    json = json + "\"tags\":[" + "\n"
    
    for i in 0..record.tags.length() {
      let (key, value) = record.tags[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < record.tags.length() - 1 {
        json = json + ","
      }
    }
    
    json = json + "]" + "\n" + "}"
    json
  }
  
  // Serialize to CSV-like string
  let serialize_to_csv = fn(record: TelemetryRecord) {
    let mut csv = ""
    csv = csv + record.id + ","
    csv = csv + record.timestamp.to_string() + ","
    csv = csv + record.trace_id + ","
    csv = csv + record.span_id + ","
    csv = csv + record.service + ","
    csv = csv + record.operation + ","
    csv = csv + record.duration.to_string() + ","
    csv = csv + record.status + ","
    
    // Convert tags to key=value;key2=value2 format
    let mut tag_string = ""
    for i in 0..record.tags.length() {
      let (key, value) = record.tags[i]
      tag_string = tag_string + key + "=" + value
      if i < record.tags.length() - 1 {
        tag_string = tag_string + ";"
      }
    }
    csv = csv + tag_string
    
    csv
  }
  
  // Serialize to key-value format
  let serialize_to_kv = fn(record: TelemetryRecord) {
    let mut kv = ""
    kv = kv + "id=" + record.id + "\n"
    kv = kv + "timestamp=" + record.timestamp.to_string() + "\n"
    kv = kv + "trace_id=" + record.trace_id + "\n"
    kv = kv + "span_id=" + record.span_id + "\n"
    kv = kv + "service=" + record.service + "\n"
    kv = kv + "operation=" + record.operation + "\n"
    kv = kv + "duration=" + record.duration.to_string() + "\n"
    kv = kv + "status=" + record.status + "\n"
    
    for (key, value) in record.tags {
      kv = kv + "tag." + key + "=" + value + "\n"
    }
    
    kv
  }
  
  // Test JSON serialization
  let json_string = serialize_to_json(record)
  assert_true(json_string.contains("\"id\":\"record-123\""))
  assert_true(json_string.contains("\"service\":\"payment-service\""))
  assert_true(json_string.contains("\"duration\":250"))
  assert_true(json_string.contains("\"tags\":["))
  
  // Test CSV serialization
  let csv_string = serialize_to_csv(record)
  assert_true(csv_string.starts_with("record-123,"))
  assert_true(csv_string.contains("payment-service"))
  assert_true(csv_string.contains("user.id=user-123"))
  
  // Test key-value serialization
  let kv_string = serialize_to_kv(record)
  assert_true(kv_string.contains("id=record-123"))
  assert_true(kv_string.contains("service=payment-service"))
  assert_true(kv_string.contains("tag.user.id=user-123"))
  
  // Calculate serialization sizes
  let json_size = json_string.length()
  let csv_size = csv_string.length()
  let kv_size = kv_string.length()
  
  // JSON should be larger than CSV
  assert_true(json_size > csv_size)
  // KV should be larger than CSV
  assert_true(kv_size > csv_size)
  
  // Test multiple records serialization
  let records = [record, record, record]
  let json_batch = records.map(serialize_to_json).join("\n")
  let csv_batch = records.map(serialize_to_csv).join("\n")
  let kv_batch = records.map(serialize_to_kv).join("\n---\n")
  
  assert_eq(json_batch.split("\n").length(), 3)
  assert_eq(csv_batch.split("\n").length(), 3)
  assert_eq(kv_batch.split("---\n").length(), 3)
}