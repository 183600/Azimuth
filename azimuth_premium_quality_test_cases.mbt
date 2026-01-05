// Azimuth Premium Quality Test Cases
// This file contains premium quality test cases focusing on advanced telemetry functionality

// Test 1: Telemetry Data Collection and Processing
test "telemetry data collection and processing pipeline" {
  // Define telemetry data structure
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample telemetry data
  let create_sample_data = fn() {
    [
      {
        timestamp: 1640995200000,
        trace_id: "trace-001",
        span_id: "span-001",
        operation_name: "database_query",
        duration_ms: 150,
        status: "success",
        attributes: [("db.type", "postgresql"), ("db.statement", "SELECT * FROM users")]
      },
      {
        timestamp: 1640995201000,
        trace_id: "trace-001",
        span_id: "span-002",
        operation_name: "cache_lookup",
        duration_ms: 5,
        status: "success",
        attributes: [("cache.key", "user:123"), ("cache.hit", "true")]
      },
      {
        timestamp: 1640995202000,
        trace_id: "trace-002",
        span_id: "span-003",
        operation_name: "http_request",
        duration_ms: 200,
        status: "error",
        attributes: [("http.method", "GET"), ("http.url", "/api/users"), ("http.status_code", "500")]
      }
    ]
  }
  
  let telemetry_data = create_sample_data()
  assert_eq(telemetry_data.length(), 3)
  
  // Test data filtering by status
  let filter_by_status = fn(data: Array[TelemetryData], status: String) {
    data.filter(fn(item) { item.status == status })
  }
  
  let successful_operations = filter_by_status(telemetry_data, "success")
  assert_eq(successful_operations.length(), 2)
  assert_eq(successful_operations[0].operation_name, "database_query")
  assert_eq(successful_operations[1].operation_name, "cache_lookup")
  
  let failed_operations = filter_by_status(telemetry_data, "error")
  assert_eq(failed_operations.length(), 1)
  assert_eq(failed_operations[0].operation_name, "http_request")
  
  // Test data filtering by operation
  let filter_by_operation = fn(data: Array[TelemetryData], operation: String) {
    data.filter(fn(item) { item.operation_name == operation })
  }
  
  let db_operations = filter_by_operation(telemetry_data, "database_query")
  assert_eq(db_operations.length(), 1)
  assert_eq(db_operations[0].attributes[0], ("db.type", "postgresql"))
  
  // Test average duration calculation
  let calculate_average_duration = fn(data: Array[TelemetryData]) {
    if data.length() == 0 {
      0
    } else {
      let total = data.reduce(fn(acc, item) { acc + item.duration_ms }, 0)
      total / data.length()
    }
  }
  
  let avg_duration = calculate_average_duration(telemetry_data)
  assert_eq(avg_duration, (150 + 5 + 200) / 3)
  
  // Test trace grouping
  let group_by_trace = fn(data: Array[TelemetryData]) {
    let mut result = []
    let mut processed_traces = []
    
    for item in data {
      if not(processed_traces.contains(item.trace_id)) {
        let trace_spans = data.filter(fn(span) { span.trace_id == item.trace_id })
        result = result.push((item.trace_id, trace_spans))
        processed_traces = processed_traces.push(item.trace_id)
      }
    }
    
    result
  }
  
  let grouped_traces = group_by_trace(telemetry_data)
  assert_eq(grouped_traces.length(), 2)
  
  // Find trace-001
  let trace_001 = grouped_traces.filter(fn(t) { t.0 == "trace-001" })[0]
  assert_eq(trace_001.1.length(), 2)
  
  // Find trace-002
  let trace_002 = grouped_traces.filter(fn(t) { t.0 == "trace-002" })[0]
  assert_eq(trace_002.1.length(), 1)
}

// Test 2: Telemetry Metrics Aggregation and Analysis
test "telemetry metrics aggregation and analysis" {
  // Define metric types
  type MetricValue = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Create sample metrics
  let create_sample_metrics = fn() {
    [
      { name: "http_requests_total", value: 100.0, unit: "count", timestamp: 1640995200, tags: [("method", "GET"), ("status", "200")] },
      { name: "http_requests_total", value: 5.0, unit: "count", timestamp: 1640995200, tags: [("method", "GET"), ("status", "500")] },
      { name: "http_requests_total", value: 50.0, unit: "count", timestamp: 1640995200, tags: [("method", "POST"), ("status", "200")] },
      { name: "response_time_ms", value: 120.5, unit: "ms", timestamp: 1640995200, tags: [("endpoint", "/api/users")] },
      { name: "response_time_ms", value: 85.3, unit: "ms", timestamp: 1640995200, tags: [("endpoint", "/api/products")] },
      { name: "error_rate", value: 0.05, unit: "ratio", timestamp: 1640995200, tags: [("service", "auth")] }
    ]
  }
  
  let metrics = create_sample_metrics()
  assert_eq(metrics.length(), 6)
  
  // Test metric aggregation by name
  let aggregate_by_name = fn(metrics: Array[MetricValue]) {
    let mut result = []
    let mut processed_names = []
    
    for metric in metrics {
      if not(processed_names.contains(metric.name)) {
        let same_metrics = metrics.filter(fn(m) { m.name == metric.name })
        let total = same_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
        let count = same_metrics.length().to_float()
        result = result.push((metric.name, total, total / count))
        processed_names = processed_names.push(metric.name)
      }
    }
    
    result
  }
  
  let aggregated = aggregate_by_name(metrics)
  assert_eq(aggregated.length(), 3)
  
  // Find http_requests_total aggregation
  let http_requests = aggregated.filter(fn(a) { a.0 == "http_requests_total" })[0]
  assert_eq(http_requests.1, 100.0 + 5.0 + 50.0)
  assert_eq(http_requests.2, (100.0 + 5.0 + 50.0) / 3.0)
  
  // Test metric filtering by tags
  let filter_by_tag = fn(metrics: Array[MetricValue], tag_key: String, tag_value: String) {
    metrics.filter(fn(metric) {
      metric.tags.contains((tag_key, tag_value))
    })
  }
  
  let get_requests = filter_by_tag(metrics, "method", "GET")
  assert_eq(get_requests.length(), 2)
  assert_eq(get_requests[0].value, 100.0)
  assert_eq(get_requests[1].value, 5.0)
  
  let success_requests = filter_by_tag(metrics, "status", "200")
  assert_eq(success_requests.length(), 2)
  assert_eq(success_requests[0].value, 100.0)
  assert_eq(success_requests[1].value, 50.0)
  
  // Test percentile calculation
  let calculate_percentile = fn(values: Array[Float], percentile: Float) {
    if values.length() == 0 {
      0.0
    } else {
      let sorted = values.sort(fn(a, b) { a <= b })
      let index = ((sorted.length().to_float() - 1.0) * percentile / 100.0).to_int()
      sorted[index]
    }
  }
  
  let response_times = metrics
    .filter(fn(m) { m.name == "response_time_ms" })
    .map(fn(m) { m.value })
  
  let p50 = calculate_percentile(response_times, 50.0)
  let p95 = calculate_percentile(response_times, 95.0)
  
  assert_eq(p50, 85.3)  // Median of [85.3, 120.5]
  assert_eq(p95, 120.5) // 95th percentile of [85.3, 120.5]
  
  // Test rate calculation
  let calculate_rate = fn(current_value: Float, previous_value: Float, time_diff: Int) {
    if time_diff <= 0 {
      0.0
    } else {
      (current_value - previous_value) / time_diff.to_float()
    }
  }
  
  let rate = calculate_rate(150.0, 100.0, 60)  // 50 requests in 60 seconds
  assert_eq(rate, 50.0 / 60.0)
}

// Test 3: Telemetry Trace and Span Management
test "telemetry trace and span management" {
  // Define span structure with parent-child relationships
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    service_name: String
  }
  
  // Create sample spans with hierarchy
  let create_sample_spans = fn() {
    [
      // Root span
      {
        trace_id: "trace-001",
        span_id: "span-001",
        parent_span_id: None,
        operation_name: "web_request",
        start_time: 1640995200000,
        end_time: 1640995200500,
        status: "success",
        service_name: "api-gateway"
      },
      // Child spans
      {
        trace_id: "trace-001",
        span_id: "span-002",
        parent_span_id: Some("span-001"),
        operation_name: "auth_check",
        start_time: 1640995200100,
        end_time: 1640995200150,
        status: "success",
        service_name: "auth-service"
      },
      {
        trace_id: "trace-001",
        span_id: "span-003",
        parent_span_id: Some("span-001"),
        operation_name: "database_query",
        start_time: 1640995200200,
        end_time: 1640995200350,
        status: "success",
        service_name: "user-service"
      },
      // Nested child span
      {
        trace_id: "trace-001",
        span_id: "span-004",
        parent_span_id: Some("span-003"),
        operation_name: "cache_lookup",
        start_time: 1640995200250,
        end_time: 1640995200280,
        status: "success",
        service_name: "user-service"
      }
    ]
  }
  
  let spans = create_sample_spans()
  assert_eq(spans.length(), 4)
  
  // Test finding root spans
  let find_root_spans = fn(spans: Array[Span]) {
    spans.filter(fn(span) { span.parent_span_id == None })
  }
  
  let root_spans = find_root_spans(spans)
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].span_id, "span-001")
  assert_eq(root_spans[0].operation_name, "web_request")
  
  // Test finding child spans
  let find_child_spans = fn(spans: Array[Span], parent_span_id: String) {
    spans.filter(fn(span) { 
      match span.parent_span_id {
        Some(id) => id == parent_span_id
        None => false
      }
    })
  }
  
  let children_of_root = find_child_spans(spans, "span-001")
  assert_eq(children_of_root.length(), 2)
  assert_true(children_of_root.map(fn(s) { s.span_id }).contains("span-002"))
  assert_true(children_of_root.map(fn(s) { s.span_id }).contains("span-003"))
  
  let children_of_span3 = find_child_spans(spans, "span-003")
  assert_eq(children_of_span3.length(), 1)
  assert_eq(children_of_span3[0].span_id, "span-004")
  
  // Test building span tree
  type SpanNode = {
    span: Span,
    children: Array[SpanNode]
  }
  
  let build_span_tree = fn(spans: Array[Span], root_span_id: String) {
    let root_span = spans.filter(fn(s) { s.span_id == root_span_id })[0]
    let children_spans = find_child_spans(spans, root_span_id)
    
    let children_nodes = children_spans.map(fn(child) {
      build_span_tree(spans, child.span_id)
    })
    
    {
      span: root_span,
      children: children_nodes
    }
  }
  
  let span_tree = build_span_tree(spans, "span-001")
  assert_eq(span_tree.span.span_id, "span-001")
  assert_eq(span_tree.children.length(), 2)
  
  // Test trace duration calculation
  let calculate_trace_duration = fn(spans: Array[Span], trace_id: String) {
    let trace_spans = spans.filter(fn(s) { s.trace_id == trace_id })
    
    if trace_spans.length() == 0 {
      0
    } else {
      let start_times = trace_spans.map(fn(s) { s.start_time })
      let end_times = trace_spans.map(fn(s) { s.end_time })
      
      let earliest_start = start_times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, start_times[0])
      let latest_end = end_times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, end_times[0])
      
      latest_end - earliest_start
    }
  }
  
  let trace_duration = calculate_trace_duration(spans, "trace-001")
  assert_eq(trace_duration, 500)  // From span-001 start to span-001 end
  
  // Test service involvement in trace
  let get_involved_services = fn(spans: Array[Span], trace_id: String) {
    let trace_spans = spans.filter(fn(s) { s.trace_id == trace_id })
    let service_names = trace_spans.map(fn(s) { s.service_name })
    
    // Remove duplicates
    let mut unique_services = []
    for service in service_names {
      if not(unique_services.contains(service)) {
        unique_services = unique_services.push(service)
      }
    }
    
    unique_services
  }
  
  let involved_services = get_involved_services(spans, "trace-001")
  assert_eq(involved_services.length(), 3)
  assert_true(involved_services.contains("api-gateway"))
  assert_true(involved_services.contains("auth-service"))
  assert_true(involved_services.contains("user-service"))
  
  // Test critical path analysis
  let find_critical_path = fn(spans: Array[Span], trace_id: String) {
    let trace_spans = spans.filter(fn(s) { s.trace_id == trace_id })
    trace_spans.sort(fn(a, b) { (b.end_time - b.start_time) >= (a.end_time - a.start_time) })
  }
  
  let critical_path = find_critical_path(spans, "trace-001")
  // The longest span should be first
  assert_eq(critical_path[0].span_id, "span-001")  // 500ms duration
  assert_eq(critical_path[1].span_id, "span-003")  // 150ms duration
}

// Test 4: Telemetry Configuration and Settings Management
test "telemetry configuration and settings management" {
  // Define configuration structure
  type TelemetryConfig = {
    service_name: String,
    service_version: String,
    environment: String,
    sampling_rate: Float,
    batch_size: Int,
    flush_interval_ms: Int,
    enabled_features: Array[String],
    exporters: Array[ExporterConfig]
  }
  
  type ExporterConfig = {
    name: String,
    type: String,
    endpoint: String,
    enabled: Bool,
    settings: Array[(String, String)]
  }
  
  // Create default configuration
  let create_default_config = fn() {
    {
      service_name: "azimuth-service",
      service_version: "1.0.0",
      environment: "production",
      sampling_rate: 1.0,
      batch_size: 100,
      flush_interval_ms: 5000,
      enabled_features: ["tracing", "metrics", "logging"],
      exporters: [
        {
          name: "jaeger",
          type: "otlp",
          endpoint: "http://jaeger:14268/api/traces",
          enabled: true,
          settings: [("timeout", "5000"), ("retry_count", "3")]
        },
        {
          name: "prometheus",
          type: "prometheus",
          endpoint: "http://prometheus:9090",
          enabled: true,
          settings: [("port", "9090"), ("path", "/metrics")]
        },
        {
          name: "elasticsearch",
          type: "elasticsearch",
          endpoint: "http://elasticsearch:9200",
          enabled: false,
          settings: [("index", "telemetry"), ("shards", "3")]
        }
      ]
    }
  }
  
  let config = create_default_config()
  assert_eq(config.service_name, "azimuth-service")
  assert_eq(config.sampling_rate, 1.0)
  assert_eq(config.enabled_features.length(), 3)
  assert_eq(config.exporters.length(), 3)
  
  // Test configuration validation
  let validate_config = fn(config: TelemetryConfig) {
    let mut errors = []
    
    // Validate service name
    if config.service_name.length() == 0 {
      errors = errors.push("Service name cannot be empty")
    }
    
    // Validate sampling rate
    if config.sampling_rate < 0.0 or config.sampling_rate > 1.0 {
      errors = errors.push("Sampling rate must be between 0.0 and 1.0")
    }
    
    // Validate batch size
    if config.batch_size <= 0 {
      errors = errors.push("Batch size must be positive")
    }
    
    // Validate flush interval
    if config.flush_interval_ms <= 0 {
      errors = errors.push("Flush interval must be positive")
    }
    
    // Validate at least one exporter is enabled
    let enabled_exporters = config.exporters.filter(fn(e) { e.enabled })
    if enabled_exporters.length() == 0 {
      errors = errors.push("At least one exporter must be enabled")
    }
    
    errors
  }
  
  let validation_errors = validate_config(config)
  assert_eq(validation_errors.length(), 0)  // Default config should be valid
  
  // Test invalid configuration
  let invalid_config = { config | 
    sampling_rate: 1.5,  // Invalid: > 1.0
    batch_size: 0,       // Invalid: <= 0
    exporters: config.exporters.map(fn(e) { { e | enabled: false } })  // All disabled
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 3)
  assert_true(invalid_errors.contains("Sampling rate must be between 0.0 and 1.0"))
  assert_true(invalid_errors.contains("Batch size must be positive"))
  assert_true(invalid_errors.contains("At least one exporter must be enabled"))
  
  // Test configuration merging
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      service_name: if override.service_name != "" { override.service_name } else { base.service_name },
      service_version: if override.service_version != "" { override.service_version } else { base.service_version },
      environment: if override.environment != "" { override.environment } else { base.environment },
      sampling_rate: if override.sampling_rate >= 0.0 { override.sampling_rate } else { base.sampling_rate },
      batch_size: if override.batch_size > 0 { override.batch_size } else { base.batch_size },
      flush_interval_ms: if override.flush_interval_ms > 0 { override.flush_interval_ms } else { base.flush_interval_ms },
      enabled_features: if override.enabled_features.length() > 0 { override.enabled_features } else { base.enabled_features },
      exporters: override.exporters
    }
  }
  
  let override_config = {
    service_name: "",
    service_version: "2.0.0",
    environment: "staging",
    sampling_rate: 0.5,
    batch_size: 0,
    flush_interval_ms: 10000,
    enabled_features: [],
    exporters: [
      {
        name: "tempo",
        type: "otlp",
        endpoint: "http://tempo:3100",
        enabled: true,
        settings: []
      }
    ]
  }
  
  let merged_config = merge_configs(config, override_config)
  assert_eq(merged_config.service_name, "azimuth-service")  // From base
  assert_eq(merged_config.service_version, "2.0.0")        // From override
  assert_eq(merged_config.environment, "staging")           // From override
  assert_eq(merged_config.sampling_rate, 0.5)               // From override
  assert_eq(merged_config.batch_size, 100)                  // From base (override is invalid)
  assert_eq(merged_config.flush_interval_ms, 10000)         // From override
  assert_eq(merged_config.enabled_features.length(), 0)     // From override
  assert_eq(merged_config.exporters.length(), 1)            // From override
  
  // Test environment-specific configuration
  let get_env_config = fn(base: TelemetryConfig, environment: String) {
    match environment {
      "development" => { base | 
        sampling_rate: 1.0,
        batch_size: 10,
        flush_interval_ms: 1000
      }
      "staging" => { base | 
        sampling_rate: 0.5,
        batch_size: 50,
        flush_interval_ms: 2500
      }
      "production" => { base | 
        sampling_rate: 0.1,
        batch_size: 200,
        flush_interval_ms: 10000
      }
      _ => base
    }
  }
  
  let dev_config = get_env_config(config, "development")
  assert_eq(dev_config.sampling_rate, 1.0)
  assert_eq(dev_config.batch_size, 10)
  assert_eq(dev_config.flush_interval_ms, 1000)
  
  let prod_config = get_env_config(config, "production")
  assert_eq(prod_config.sampling_rate, 0.1)
  assert_eq(prod_config.batch_size, 200)
  assert_eq(prod_config.flush_interval_ms, 10000)
}

// Test 5: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // Define simplified telemetry data structure
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: String,
    data: Array[(String, String)]
  }
  
  // Create sample event
  let sample_event = {
    id: "event-001",
    timestamp: 1640995200,
    event_type: "span",
    data: [
      ("trace_id", "trace-001"),
      ("span_id", "span-001"),
      ("operation_name", "database_query"),
      ("duration_ms", "150"),
      ("status", "success")
    ]
  }
  
  // Test serialization to JSON-like string
  let serialize_to_json = fn(event: TelemetryEvent) {
    let data_pairs = event.data.map(fn(pair) {
      "\"" + pair.0 + "\":\"" + pair.1 + "\""
    })
    
    "{" +
      "\"id\":\"" + event.id + "\"," +
      "\"timestamp\":" + event.timestamp.to_string() + "," +
      "\"event_type\":\"" + event.event_type + "\"," +
      "\"data\":{" + data_pairs.reduce(fn(acc, pair) { 
        if acc == "" { pair } else { acc + "," + pair } 
      }, "") + "}" +
    "}"
  }
  
  let json_string = serialize_to_json(sample_event)
  assert_true(json_string.contains("\"id\":\"event-001\""))
  assert_true(json_string.contains("\"timestamp\":1640995200"))
  assert_true(json_string.contains("\"event_type\":\"span\""))
  assert_true(json_string.contains("\"trace_id\":\"trace-001\""))
  
  // Test deserialization from JSON-like string
  let parse_json_value = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":" 
    let start_index = json.index_of(key_pattern)
    
    if start_index == -1 {
      ""
    } else {
      let value_start = start_index + key_pattern.length()
      let remaining = json.substring(value_start, json.length() - value_start)
      
      if remaining[0] == '"' {
        // String value
        let end_index = remaining.index_of("\"", 1)
        if end_index == -1 {
          ""
        } else {
          remaining.substring(1, end_index - 1)
        }
      } else {
        // Numeric value
        let end_index = remaining.index_of(",")
        if end_index == -1 {
          remaining.index_of("}")
        } else {
          end_index
        }
        
        if end_index == -1 {
          remaining
        } else {
          remaining.substring(0, end_index)
        }
      }
    }
  }
  
  let extracted_id = parse_json_value(json_string, "id")
  assert_eq(extracted_id, "event-001")
  
  let extracted_timestamp = parse_json_value(json_string, "timestamp")
  assert_eq(extracted_timestamp, "1640995200")
  
  let extracted_event_type = parse_json_value(json_string, "event_type")
  assert_eq(extracted_event_type, "span")
  
  // Test batch serialization
  let serialize_batch = fn(events: Array[TelemetryEvent]) {
    let event_strings = events.map(fn(e) { serialize_to_json(e) })
    "[" + event_strings.reduce(fn(acc, event) { 
      if acc == "" { event } else { acc + "," + event } 
    }, "") + "]"
  }
  
  let events = [
    sample_event,
    {
      id: "event-002",
      timestamp: 1640995201,
      event_type: "metric",
      data: [
        ("metric_name", "http_requests_total"),
        ("value", "100"),
        ("unit", "count")
      ]
    }
  ]
  
  let batch_json = serialize_batch(events)
  assert_true(batch_json.starts_with("["))
  assert_true(batch_json.ends_with("]"))
  assert_true(batch_json.contains("\"id\":\"event-001\""))
  assert_true(batch_json.contains("\"id\":\"event-002\""))
  
  // Test compression simulation
  let compress_string = fn(s: String) {
    // Simple compression simulation: replace repeated patterns
    let compressed = s
      .replace("\"trace_id\":", "\"t\":")
      .replace("\"span_id\":", "\"s\":")
      .replace("\"duration_ms\":", "\"d\":")
      .replace("\"operation_name\":", "\"o\":")
      .replace("\"status\":", "\"st\":")
    
    compressed
  }
  
  let compressed_json = compress_string(json_string)
  assert_true(compressed_json.length() < json_string.length())
  assert_true(compressed_json.contains("\"t\":\"trace-001\""))
  
  // Test decompression
  let decompress_string = fn(s: String) {
    let decompressed = s
      .replace("\"t\":", "\"trace_id\":")
      .replace("\"s\":", "\"span_id\":")
      .replace("\"d\":", "\"duration_ms\":")
      .replace("\"o\":", "\"operation_name\":")
      .replace("\"st\":", "\"status\":")
    
    decompressed
  }
  
  let decompressed_json = decompress_string(compressed_json)
  assert_eq(decompressed_json, json_string)
  
  // Test binary serialization simulation
  let serialize_to_binary = fn(event: TelemetryEvent) {
    // Simple binary format simulation
    let mut binary_data = []
    
    // Add event type as single byte
    let event_type_byte = match event.event_type {
      "span" => 1
      "metric" => 2
      "log" => 3
      _ => 0
    }
    binary_data = binary_data.push(event_type_byte)
    
    // Add timestamp as 4 bytes (simplified)
    let timestamp = event.timestamp
    binary_data = binary_data.push((timestamp / 16777216) % 256)  // Byte 3
    binary_data = binary_data.push((timestamp / 65536) % 256)     // Byte 2
    binary_data = binary_data.push((timestamp / 256) % 256)       // Byte 1
    binary_data = binary_data.push(timestamp % 256)               // Byte 0
    
    // Add data length and key-value pairs
    let data_length = event.data.length()
    binary_data = binary_data.push(data_length)
    
    for (key, value) in event.data {
      // Add key length and key
      binary_data = binary_data.push(key.length())
      for i in 0..key.length() {
        binary_data = binary_data.push(key[i].to_int())
      }
      
      // Add value length and value
      binary_data = binary_data.push(value.length())
      for i in 0..value.length() {
        binary_data = binary_data.push(value[i].to_int())
      }
    }
    
    binary_data
  }
  
  let binary_data = serialize_to_binary(sample_event)
  assert_eq(binary_data[0], 1)  // Event type: span
  assert_eq(binary_data[1], 0)  // Timestamp byte 3
  assert_eq(binary_data[2], 0)  // Timestamp byte 2
  assert_eq(binary_data[3], 152)  // Timestamp byte 1
  assert_eq(binary_data[4], 0)    // Timestamp byte 0
  assert_eq(binary_data[5], 5)    // Data length: 5 pairs
}

// Test 6: Telemetry Data Filtering and Querying
test "telemetry data filtering and querying" {
  // Define telemetry record structure
  type TelemetryRecord = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // Create sample records
  let create_sample_records = fn() {
    [
      {
        timestamp: 1640995200000,
        trace_id: "trace-001",
        span_id: "span-001",
        service_name: "api-gateway",
        operation_name: "http_request",
        duration_ms: 100,
        status: "success",
        attributes: [("http.method", "GET"), ("http.path", "/api/users"), ("http.status_code", "200")]
      },
      {
        timestamp: 1640995201000,
        trace_id: "trace-001",
        span_id: "span-002",
        service_name: "auth-service",
        operation_name: "authenticate",
        duration_ms: 50,
        status: "success",
        attributes: [("user.id", "123"), ("auth.method", "jwt")]
      },
      {
        timestamp: 1640995202000,
        trace_id: "trace-002",
        span_id: "span-003",
        service_name: "user-service",
        operation_name: "database_query",
        duration_ms: 200,
        status: "error",
        attributes: [("db.type", "postgresql"), ("db.table", "users"), ("error.type", "timeout")]
      },
      {
        timestamp: 1640995203000,
        trace_id: "trace-002",
        span_id: "span-004",
        service_name: "api-gateway",
        operation_name: "http_request",
        duration_ms: 300,
        status: "error",
        attributes: [("http.method", "POST"), ("http.path", "/api/orders"), ("http.status_code", "500")]
      },
      {
        timestamp: 1640995204000,
        trace_id: "trace-003",
        span_id: "span-005",
        service_name: "order-service",
        operation_name: "process_order",
        duration_ms: 150,
        status: "success",
        attributes: [("order.id", "456"), ("order.amount", "99.99")]
      }
    ]
  }
  
  let records = create_sample_records()
  assert_eq(records.length(), 5)
  
  // Test time range filtering
  let filter_by_time_range = fn(records: Array[TelemetryRecord], start_time: Int, end_time: Int) {
    records.filter(fn(record) { 
      record.timestamp >= start_time and record.timestamp <= end_time 
    })
  }
  
  let filtered_by_time = filter_by_time_range(records, 1640995201000, 1640995203000)
  assert_eq(filtered_by_time.length(), 3)
  assert_eq(filtered_by_time[0].span_id, "span-002")
  assert_eq(filtered_by_time[1].span_id, "span-003")
  assert_eq(filtered_by_time[2].span_id, "span-004")
  
  // Test service filtering
  let filter_by_service = fn(records: Array[TelemetryRecord], service_name: String) {
    records.filter(fn(record) { record.service_name == service_name })
  }
  
  let api_gateway_records = filter_by_service(records, "api-gateway")
  assert_eq(api_gateway_records.length(), 2)
  assert_eq(api_gateway_records[0].span_id, "span-001")
  assert_eq(api_gateway_records[1].span_id, "span-004")
  
  // Test status filtering
  let filter_by_status = fn(records: Array[TelemetryRecord], status: String) {
    records.filter(fn(record) { record.status == status })
  }
  
  let error_records = filter_by_status(records, "error")
  assert_eq(error_records.length(), 2)
  assert_eq(error_records[0].span_id, "span-003")
  assert_eq(error_records[1].span_id, "span-004")
  
  // Test attribute filtering
  let filter_by_attribute = fn(records: Array[TelemetryRecord], key: String, value: String) {
    records.filter(fn(record) { 
      record.attributes.contains((key, value))
    })
  }
  
  let get_requests = filter_by_attribute(records, "http.method", "GET")
  assert_eq(get_requests.length(), 1)
  assert_eq(get_requests[0].span_id, "span-001")
  
  let timeout_errors = filter_by_attribute(records, "error.type", "timeout")
  assert_eq(timeout_errors.length(), 1)
  assert_eq(timeout_errors[0].span_id, "span-003")
  
  // Test complex query builder
  type Query = {
    time_range: Option[(Int, Int)],
    services: Array[String],
    operations: Array[String],
    statuses: Array[String],
    attributes: Array[(String, String)],
    limit: Option[Int]
  }
  
  let execute_query = fn(records: Array[TelemetryRecord], query: Query) {
    let mut result = records
    
    // Apply time range filter
    match query.time_range {
      Some((start, end)) => {
        result = result.filter(fn(r) { r.timestamp >= start and r.timestamp <= end })
      }
      None => {}
    }
    
    // Apply service filter
    if query.services.length() > 0 {
      result = result.filter(fn(r) { query.services.contains(r.service_name) })
    }
    
    // Apply operation filter
    if query.operations.length() > 0 {
      result = result.filter(fn(r) { query.operations.contains(r.operation_name) })
    }
    
    // Apply status filter
    if query.statuses.length() > 0 {
      result = result.filter(fn(r) { query.statuses.contains(r.status) })
    }
    
    // Apply attribute filter
    for (key, value) in query.attributes {
      result = result.filter(fn(r) { r.attributes.contains((key, value)) })
    }
    
    // Apply limit
    match query.limit {
      Some(limit) => {
        if result.length() > limit {
          result = result.slice(0, limit)
        }
      }
      None => {}
    }
    
    result
  }
  
  // Test complex query
  let complex_query = {
    time_range: Some((1640995201000, 1640995204000)),
    services: ["api-gateway", "user-service"],
    operations: [],
    statuses: ["success", "error"],
    attributes: [],
    limit: Some(10)
  }
  
  let query_results = execute_query(records, complex_query)
  assert_eq(query_results.length(), 3)
  assert_eq(query_results[0].span_id, "span-002")  // auth-service (excluded)
  assert_eq(query_results[1].span_id, "span-003")  // user-service, error
  assert_eq(query_results[2].span_id, "span-004")  // api-gateway, error
  
  // Test aggregation query
  type AggregationQuery = {
    group_by: Array[String],
    aggregations: Array[(String, String)]  // (function, field)
  }
  
  let execute_aggregation = fn(records: Array[TelemetryRecord], query: AggregationQuery) {
    let mut groups = []
    let mut processed_keys = []
    
    for record in records {
      // Create group key
      let group_key_values = query.group_by.map(fn(field) {
        match field {
          "service_name" => record.service_name
          "operation_name" => record.operation_name
          "status" => record.status
          _ => "unknown"
        }
      })
      
      let group_key = group_key_values.reduce(fn(acc, value) { 
        if acc == "" { value } else { acc + "|" + value } 
      }, "")
      
      // Find or create group
      if not(processed_keys.contains(group_key)) {
        let group_records = records.filter(fn(r) {
          let key_values = query.group_by.map(fn(field) {
            match field {
              "service_name" => r.service_name
              "operation_name" => r.operation_name
              "status" => r.status
              _ => "unknown"
            }
          })
          
          let key = key_values.reduce(fn(acc, value) { 
            if acc == "" { value } else { acc + "|" + value } 
          }, "")
          
          key == group_key
        })
        
        // Calculate aggregations
        let mut aggregation_results = []
        
        for (func, field) in query.aggregations {
          let result = match func {
            "count" => group_records.length().to_string()
            "avg_duration" => {
              if group_records.length() == 0 {
                "0"
              } else {
                let total = group_records.reduce(fn(acc, r) { acc + r.duration_ms }, 0)
                (total / group_records.length()).to_string()
              }
            }
            "max_duration" => {
              if group_records.length() == 0 {
                "0"
              } else {
                let max = group_records.reduce(fn(acc, r) { 
                  if r.duration_ms > acc { r.duration_ms } else { acc } 
                }, group_records[0].duration_ms)
                max.to_string()
              }
            }
            _ => "unknown"
          }
          
          aggregation_results = aggregation_results.push((func + "_" + field, result))
        }
        
        groups = groups.push((group_key, aggregation_results))
        processed_keys = processed_keys.push(group_key)
      }
    }
    
    groups
  }
  
  // Test aggregation
  let aggregation_query = {
    group_by: ["service_name", "status"],
    aggregations: [("count", "records"), ("avg_duration", "duration_ms")]
  }
  
  let aggregation_results = execute_aggregation(records, aggregation_query)
  assert_eq(aggregation_results.length(), 5)  // 5 unique combinations
  
  // Find api-gateway|success group
  let api_success = aggregation_results.filter(fn(g) { g.0 == "api-gateway|success" })[0]
  assert_eq(api_success.1[0], ("count_records", "1"))
  assert_eq(api_success.1[1], ("avg_duration_duration_ms", "100"))
}

// Test 7: Telemetry Error Handling and Recovery
test "telemetry error handling and recovery" {
  // Define error types
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ConfigurationError(String)
    ResourceExhaustedError(String)
    AuthenticationError(String)
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
  
  // Test network error handling
  let send_telemetry_data = fn(data: String, endpoint: String) {
    // Simulate network failure for certain endpoints
    if endpoint.contains("unreachable") {
      create_error(TelemetryError::NetworkError("Connection timeout"))
    } else if endpoint.contains("auth-failed") {
      create_error(TelemetryError::AuthenticationError("Invalid credentials"))
    } else {
      create_success("Data sent successfully to " + endpoint)
    }
  }
  
  let valid_endpoint = "http://telemetry-collector:4317"
  let unreachable_endpoint = "http://unreachable-service:4317"
  let auth_failed_endpoint = "http://auth-failed-service:4317"
  
  let valid_result = send_telemetry_data("test data", valid_endpoint)
  assert_true(valid_result.success)
  assert_eq(valid_result.data, Some("Data sent successfully to http://telemetry-collector:4317"))
  
  let network_error_result = send_telemetry_data("test data", unreachable_endpoint)
  assert_false(network_error_result.success)
  match network_error_result.error {
    Some(TelemetryError::NetworkError(msg)) => assert_eq(msg, "Connection timeout")
    _ => assert_true(false)
  }
  
  let auth_error_result = send_telemetry_data("test data", auth_failed_endpoint)
  assert_false(auth_error_result.success)
  match auth_error_result.error {
    Some(TelemetryError::AuthenticationError(msg)) => assert_eq(msg, "Invalid credentials")
    _ => assert_true(false)
  }
  
  // Test retry mechanism
  let retry_with_backoff = fn(operation: () -> TelemetryResult[String], max_retries: Int) {
    let mut attempts = 0
    let mut result = operation()
    
    while not(result.success) and attempts < max_retries {
      attempts = attempts + 1
      
      // Simulate exponential backoff
      let backoff_ms = 100 * (2 ^ (attempts - 1))
      // In a real implementation, we would wait here
      
      result = operation()
    }
    
    (result, attempts)
  }
  
  // Simulate a flaky operation that succeeds after 2 attempts
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      create_error(TelemetryError::NetworkError("Temporary failure"))
    } else {
      create_success("Operation succeeded after retries")
    }
  }
  
  let (retry_result, total_attempts) = retry_with_backoff(flaky_operation, 5)
  assert_true(retry_result.success)
  assert_eq(total_attempts, 3)
  assert_eq(retry_result.data, Some("Operation succeeded after retries"))
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    failure_count: Int,
    failure_threshold: Int,
    state: String,  // "closed", "open", "half-open"
    last_failure_time: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int) {
    {
      failure_count: 0,
      failure_threshold: threshold,
      state: "closed",
      last_failure_time: 0
    }
  }
  
  let call_with_circuit_breaker = fn(
    breaker: CircuitBreaker, 
    operation: () -> TelemetryResult[String],
    current_time: Int
  ) {
    match breaker.state {
      "open" => {
        // Check if we should transition to half-open
        if current_time - breaker.last_failure_time > 30000 {  // 30 seconds
          // Try the operation
          let result = operation()
          if result.success {
            // Reset the circuit breaker
            ({
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              state: "closed",
              last_failure_time: 0
            }, result)
          } else {
            // Stay in half-open
            ({
              failure_count: breaker.failure_count + 1,
              failure_threshold: breaker.failure_threshold,
              state: "half-open",
              last_failure_time: current_time
            }, result)
          }
        } else {
          // Circuit is still open
          (breaker, create_error(TelemetryError::NetworkError("Circuit breaker is open")))
        }
      }
      _ => {
        // Circuit is closed or half-open, try the operation
        let result = operation()
        
        if result.success {
          // Reset failure count
          ({
            failure_count: 0,
            failure_threshold: breaker.failure_threshold,
            state: "closed",
            last_failure_time: 0
          }, result)
        } else {
          // Increment failure count
          let new_failure_count = breaker.failure_count + 1
          let new_state = if new_failure_count >= breaker.failure_threshold {
            "open"
          } else {
            breaker.state
          }
          
          ({
            failure_count: new_failure_count,
            failure_threshold: breaker.failure_threshold,
            state: new_state,
            last_failure_time: current_time
          }, result)
        }
      }
    }
  }
  
  // Test circuit breaker
  let circuit_breaker = create_circuit_breaker(3)
  let failing_operation = fn() {
    create_error(TelemetryError::NetworkError("Service unavailable"))
  }
  
  // First failure
  let (breaker1, result1) = call_with_circuit_breaker(circuit_breaker, failing_operation, 1640995200)
  assert_false(result1.success)
  assert_eq(breaker1.failure_count, 1)
  assert_eq(breaker1.state, "closed")
  
  // Second failure
  let (breaker2, result2) = call_with_circuit_breaker(breaker1, failing_operation, 1640995201)
  assert_false(result2.success)
  assert_eq(breaker2.failure_count, 2)
  assert_eq(breaker2.state, "closed")
  
  // Third failure - should open the circuit
  let (breaker3, result3) = call_with_circuit_breaker(breaker2, failing_operation, 1640995202)
  assert_false(result3.success)
  assert_eq(breaker3.failure_count, 3)
  assert_eq(breaker3.state, "open")
  
  // Fourth call - should fail immediately due to open circuit
  let (breaker4, result4) = call_with_circuit_breaker(breaker3, failing_operation, 1640995203)
  assert_false(result4.success)
  match result4.error {
    Some(TelemetryError::NetworkError(msg)) => assert_eq(msg, "Circuit breaker is open")
    _ => assert_true(false)
  }
  assert_eq(breaker4.failure_count, 3)
  assert_eq(breaker4.state, "open")
  
  // Test fallback mechanism
  let with_fallback = fn(
    primary: () -> TelemetryResult[String],
    fallback: () -> TelemetryResult[String]
  ) {
    let primary_result = primary()
    if primary_result.success {
      primary_result
    } else {
      fallback()
    }
  }
  
  let primary_operation = fn() {
    create_error(TelemetryError::NetworkError("Primary endpoint unavailable"))
  }
  
  let fallback_operation = fn() {
    create_success("Fallback operation succeeded")
  }
  
  let fallback_result = with_fallback(primary_operation, fallback_operation)
  assert_true(fallback_result.success)
  assert_eq(fallback_result.data, Some("Fallback operation succeeded"))
}

// Test 8: Telemetry Performance and Resource Management
test "telemetry performance and resource management" {
  // Define resource usage metrics
  type ResourceMetrics = {
    memory_usage_mb: Float,
    cpu_usage_percent: Float,
    disk_io_mb: Float,
    network_io_mb: Float,
    open_file_descriptors: Int
  }
  
  // Define telemetry performance metrics
  type TelemetryPerformanceMetrics = {
    events_processed_per_second: Float,
    average_processing_latency_ms: Float,
    batch_size: Int,
    queue_depth: Int,
    error_rate_percent: Float
  }
  
  // Create initial resource metrics
  let initial_resources = {
    memory_usage_mb: 100.0,
    cpu_usage_percent: 10.0,
    disk_io_mb: 5.0,
    network_io_mb: 2.0,
    open_file_descriptors: 50
  }
  
  // Create initial performance metrics
  let initial_performance = {
    events_processed_per_second: 1000.0,
    average_processing_latency_ms: 5.0,
    batch_size: 100,
    queue_depth: 10,
    error_rate_percent: 0.1
  }
  
  // Test resource usage calculation
  let calculate_resource_delta = fn(initial: ResourceMetrics, current: ResourceMetrics) {
    {
      memory_delta_mb: current.memory_usage_mb - initial.memory_usage_mb,
      cpu_delta_percent: current.cpu_usage_percent - initial.cpu_usage_percent,
      disk_io_delta_mb: current.disk_io_mb - initial.disk_io_mb,
      network_io_delta_mb: current.network_io_mb - initial.network_io_mb,
      fd_delta: current.open_file_descriptors - initial.open_file_descriptors
    }
  }
  
  let current_resources = {
    memory_usage_mb: 150.0,
    cpu_usage_percent: 25.0,
    disk_io_mb: 15.0,
    network_io_mb: 8.0,
    open_file_descriptors: 75
  }
  
  let resource_delta = calculate_resource_delta(initial_resources, current_resources)
  assert_eq(resource_delta.memory_delta_mb, 50.0)
  assert_eq(resource_delta.cpu_delta_percent, 15.0)
  assert_eq(resource_delta.disk_io_delta_mb, 10.0)
  assert_eq(resource_delta.network_io_delta_mb, 6.0)
  assert_eq(resource_delta.fd_delta, 25)
  
  // Test performance threshold checking
  let check_performance_thresholds = fn(metrics: TelemetryPerformanceMetrics) {
    let mut violations = []
    
    if metrics.events_processed_per_second < 500.0 {
      violations = violations.push("Low throughput: " + metrics.events_processed_per_second.to_string())
    }
    
    if metrics.average_processing_latency_ms > 10.0 {
      violations = violations.push("High latency: " + metrics.average_processing_latency_ms.to_string())
    }
    
    if metrics.queue_depth > 100 {
      violations = violations.push("High queue depth: " + metrics.queue_depth.to_string())
    }
    
    if metrics.error_rate_percent > 1.0 {
      violations = violations.push("High error rate: " + metrics.error_rate_percent.to_string())
    }
    
    violations
  }
  
  let good_performance = {
    events_processed_per_second: 1000.0,
    average_processing_latency_ms: 5.0,
    batch_size: 100,
    queue_depth: 10,
    error_rate_percent: 0.1
  }
  
  let good_violations = check_performance_thresholds(good_performance)
  assert_eq(good_violations.length(), 0)
  
  let poor_performance = {
    events_processed_per_second: 300.0,
    average_processing_latency_ms: 15.0,
    batch_size: 100,
    queue_depth: 150,
    error_rate_percent: 2.0
  }
  
  let poor_violations = check_performance_thresholds(poor_performance)
  assert_eq(poor_violations.length(), 4)
  assert_true(poor_violations.contains("Low throughput: 300"))
  assert_true(poor_violations.contains("High latency: 15"))
  assert_true(poor_violations.contains("High queue depth: 150"))
  assert_true(poor_violations.contains("High error rate: 2"))
  
  // Test adaptive batch sizing
  let calculate_optimal_batch_size = fn(
    current_batch_size: Int,
    current_latency: Float,
    target_latency: Float,
    min_batch_size: Int,
    max_batch_size: Int
  ) {
    if current_latency > target_latency * 1.2 {
      // Latency is too high, decrease batch size
      let new_size = current_batch_size * 3 / 4
      if new_size < min_batch_size { min_batch_size } else { new_size }
    } else if current_latency < target_latency * 0.8 {
      // Latency is good, can increase batch size
      let new_size = current_batch_size * 5 / 4
      if new_size > max_batch_size { max_batch_size } else { new_size }
    } else {
      // Latency is acceptable, keep current batch size
      current_batch_size
    }
  }
  
  let optimal_batch1 = calculate_optimal_batch_size(100, 8.0, 10.0, 10, 1000)
  assert_eq(optimal_batch1, 125)  // Latency is good, increase batch size
  
  let optimal_batch2 = calculate_optimal_batch_size(100, 15.0, 10.0, 10, 1000)
  assert_eq(optimal_batch2, 75)   // Latency is too high, decrease batch size
  
  let optimal_batch3 = calculate_optimal_batch_size(100, 10.0, 10.0, 10, 1000)
  assert_eq(optimal_batch3, 100)  // Latency is acceptable, keep batch size
  
  // Test memory pool simulation
  type MemoryPool = {
    total_size_mb: Float,
    used_size_mb: Float,
    allocated_blocks: Int,
    free_blocks: Int
  }
  
  let create_memory_pool = fn(size_mb: Float) {
    {
      total_size_mb: size_mb,
      used_size_mb: 0.0,
      allocated_blocks: 0,
      free_blocks: 100
    }
  }
  
  let allocate_from_pool = fn(pool: MemoryPool, size_mb: Float) {
    if pool.used_size_mb + size_mb <= pool.total_size_mb and pool.free_blocks > 0 {
      ({
        total_size_mb: pool.total_size_mb,
        used_size_mb: pool.used_size_mb + size_mb,
        allocated_blocks: pool.allocated_blocks + 1,
        free_blocks: pool.free_blocks - 1
      }, true)
    } else {
      (pool, false)
    }
  }
  
  let deallocate_to_pool = fn(pool: MemoryPool, size_mb: Float) {
    if pool.allocated_blocks > 0 {
      ({
        total_size_mb: pool.total_size_mb,
        used_size_mb: if pool.used_size_mb >= size_mb { pool.used_size_mb - size_mb } else { 0.0 },
        allocated_blocks: pool.allocated_blocks - 1,
        free_blocks: pool.free_blocks + 1
      }, true)
    } else {
      (pool, false)
    }
  }
  
  let memory_pool = create_memory_pool(1000.0)
  let (pool1, success1) = allocate_from_pool(memory_pool, 100.0)
  assert_true(success1)
  assert_eq(pool1.used_size_mb, 100.0)
  assert_eq(pool1.allocated_blocks, 1)
  assert_eq(pool1.free_blocks, 99)
  
  let (pool2, success2) = allocate_from_pool(pool1, 200.0)
  assert_true(success2)
  assert_eq(pool2.used_size_mb, 300.0)
  assert_eq(pool2.allocated_blocks, 2)
  assert_eq(pool2.free_blocks, 98)
  
  let (pool3, success3) = deallocate_to_pool(pool2, 100.0)
  assert_true(success3)
  assert_eq(pool3.used_size_mb, 200.0)
  assert_eq(pool3.allocated_blocks, 1)
  assert_eq(pool3.free_blocks, 99)
  
  // Test queue management
  type TelemetryQueue = {
    max_size: Int,
    current_size: Int,
    items: Array[String],
    dropped_items: Int
  }
  
  let create_queue = fn(max_size: Int) {
    {
      max_size: max_size,
      current_size: 0,
      items: [],
      dropped_items: 0
    }
  }
  
  let enqueue = fn(queue: TelemetryQueue, item: String) {
    if queue.current_size < queue.max_size {
      ({
        max_size: queue.max_size,
        current_size: queue.current_size + 1,
        items: queue.items.push(item),
        dropped_items: queue.dropped_items
      }, true)
    } else {
      ({
        max_size: queue.max_size,
        current_size: queue.current_size,
        items: queue.items,
        dropped_items: queue.dropped_items + 1
      }, false)
    }
  }
  
  let dequeue = fn(queue: TelemetryQueue) {
    if queue.current_size > 0 {
      let item = queue.items[0]
      let remaining_items = queue.items.slice(1, queue.items.length() - 1)
      ({
        max_size: queue.max_size,
        current_size: queue.current_size - 1,
        items: remaining_items,
        dropped_items: queue.dropped_items
      }, Some(item))
    } else {
      (queue, None)
    }
  }
  
  let telemetry_queue = create_queue(3)
  let (queue1, enqueued1) = enqueue(telemetry_queue, "item1")
  assert_true(enqueued1)
  assert_eq(queue1.current_size, 1)
  
  let (queue2, enqueued2) = enqueue(queue1, "item2")
  assert_true(enqueued2)
  assert_eq(queue2.current_size, 2)
  
  let (queue3, enqueued3) = enqueue(queue2, "item3")
  assert_true(enqueued3)
  assert_eq(queue3.current_size, 3)
  
  let (queue4, enqueued4) = enqueue(queue3, "item4")
  assert_false(enqueued4)  // Queue is full
  assert_eq(queue4.dropped_items, 1)
  
  let (queue5, dequeued1) = dequeue(queue4)
  assert_eq(dequeued1, Some("item1"))
  assert_eq(queue5.current_size, 2)
}

// Test 9: Telemetry Data Export and Reporting
test "telemetry data export and reporting" {
  // Define export format types
  enum ExportFormat {
    JSON
    CSV
    Prometheus
    OTLP
  }
  
  // Define export destination
  type ExportDestination = {
    type: String,
    endpoint: String,
    credentials: Option[(String, String)]
  }
  
  // Define telemetry data point
  type DataPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // Create sample data points
  let create_sample_data_points = fn() {
    [
      {
        name: "http_requests_total",
        value: 100.0,
        timestamp: 1640995200,
        labels: [("method", "GET"), ("status", "200"), ("service", "api-gateway")]
      },
      {
        name: "http_requests_total",
        value: 5.0,
        timestamp: 1640995200,
        labels: [("method", "GET"), ("status", "500"), ("service", "api-gateway")]
      },
      {
        name: "response_time_ms",
        value: 120.5,
        timestamp: 1640995200,
        labels: [("endpoint", "/api/users"), ("service", "user-service")]
      },
      {
        name: "error_rate",
        value: 0.05,
        timestamp: 1640995200,
        labels: [("service", "auth-service")]
      }
    ]
  }
  
  let data_points = create_sample_data_points()
  assert_eq(data_points.length(), 4)
  
  // Test JSON export
  let export_to_json = fn(data_points: Array[DataPoint]) {
    let point_strings = data_points.map(fn(point) {
      let label_strings = point.labels.map(fn(label) {
        "\"" + label.0 + "\":\"" + label.1 + "\""
      })
      
      "{" +
        "\"name\":\"" + point.name + "\"," +
        "\"value\":" + point.value.to_string() + "," +
        "\"timestamp\":" + point.timestamp.to_string() + "," +
        "\"labels\":{" + label_strings.reduce(fn(acc, label) { 
          if acc == "" { label } else { acc + "," + label } 
        }, "") + "}" +
      "}"
    })
    
    "[" + point_strings.reduce(fn(acc, point) { 
      if acc == "" { point } else { acc + "," + point } 
    }, "") + "]"
  }
  
  let json_export = export_to_json(data_points)
  assert_true(json_export.starts_with("["))
  assert_true(json_export.ends_with("]"))
  assert_true(json_export.contains("\"name\":\"http_requests_total\""))
  assert_true(json_export.contains("\"value\":100.0"))
  
  // Test CSV export
  let export_to_csv = fn(data_points: Array[DataPoint]) {
    let header = "name,value,timestamp,labels"
    
    let rows = data_points.map(fn(point) {
      let label_string = point.labels.map(fn(label) {
        label.0 + "=" + label.1
      }).reduce(fn(acc, label) { 
        if acc == "" { label } else { acc + ";" + label } 
      }, "")
      
      point.name + "," + point.value.to_string() + "," + 
      point.timestamp.to_string() + ",\"" + label_string + "\""
    })
    
    header + "\n" + rows.reduce(fn(acc, row) { acc + "\n" + row }, "")
  }
  
  let csv_export = export_to_csv(data_points)
  assert_true(csv_export.starts_with("name,value,timestamp,labels"))
  assert_true(csv_export.contains("http_requests_total,100.0,1640995200"))
  
  // Test Prometheus export
  let export_to_prometheus = fn(data_points: Array[DataPoint]) {
    let metric_lines = data_points.map(fn(point) {
      let label_string = point.labels.map(fn(label) {
        label.0 + "=\"" + label.1 + "\""
      }).reduce(fn(acc, label) { 
        if acc == "" { label } else { acc + "," + label } 
      }, "")
      
      "# TYPE " + point.name + " gauge\n" +
      point.name + "{" + label_string + "} " + point.value.to_string() + " " + 
      point.timestamp.to_string()
    })
    
    metric_lines.reduce(fn(acc, line) { acc + "\n" + line }, "")
  }
  
  let prometheus_export = export_to_prometheus(data_points)
  assert_true(prometheus_export.contains("# TYPE http_requests_total gauge"))
  assert_true(prometheus_export.contains("http_requests_total{method=\"GET\",status=\"200\",service=\"api-gateway\"} 100.0"))
  
  // Test OTLP export simulation
  let export_to_otlp = fn(data_points: Array[DataPoint]) {
    let resource_attributes = [
      ("service.name", "azimuth-telemetry"),
      ("service.version", "1.0.0"),
      ("telemetry.sdk.name", "azimuth"),
      ("telemetry.sdk.version", "1.0.0")
    ]
    
    let resource_attrs_string = resource_attributes.map(fn(attr) {
      attr.0 + ": " + attr.1
    }).reduce(fn(acc, attr) { acc + ", " + attr }, "")
    
    let metrics = data_points.map(fn(point) {
      let labels_string = point.labels.map(fn(label) {
        label.0 + "=" + label.1
      }).reduce(fn(acc, label) { 
        if acc == "" { label } else { acc + "," + label } 
      }, "")
      
      "Metric{" +
        "name: " + point.name + ", " +
        "value: " + point.value.to_string() + ", " +
        "labels: [" + labels_string + "]" +
      "}"
    })
    
    "OTLPExport{" +
      "resource: {" + resource_attrs_string + "}, " +
      "metrics: [" + metrics.reduce(fn(acc, metric) { 
        if acc == "" { metric } else { acc + ", " + metric } 
      }, "") + "]" +
    "}"
  }
  
  let otlp_export = export_to_otlp(data_points)
  assert_true(otlp_export.contains("OTLPExport{"))
  assert_true(otlp_export.contains("service.name: azimuth-telemetry"))
  assert_true(otlp_export.contains("Metric{name: http_requests_total"))
  
  // Test export destination validation
  let validate_destination = fn(destination: ExportDestination) {
    let mut errors = []
    
    if destination.endpoint.length() == 0 {
      errors = errors.push("Endpoint cannot be empty")
    }
    
    if destination.type == "http" and not(destination.endpoint.starts_with("http://")) and not(destination.endpoint.starts_with("https://")) {
      errors = errors.push("HTTP endpoint must start with http:// or https://")
    }
    
    if destination.type == "file" and not(destination.endpoint.starts_with("/")) {
      errors = errors.push("File endpoint must be an absolute path")
    }
    
    match destination.credentials {
      Some((username, password)) => {
        if username.length() == 0 {
          errors = errors.push("Username cannot be empty")
        }
        
        if password.length() == 0 {
          errors = errors.push("Password cannot be empty")
        }
      }
      None => {}
    }
    
    errors
  }
  
  let valid_http_destination = {
    type: "http",
    endpoint: "https://telemetry-collector:4317",
    credentials: Some(("admin", "password"))
  }
  
  let http_validation_errors = validate_destination(valid_http_destination)
  assert_eq(http_validation_errors.length(), 0)
  
  let invalid_http_destination = {
    type: "http",
    endpoint: "ftp://invalid-protocol",
    credentials: Some((", "password"))
  }
  
  let invalid_http_errors = validate_destination(invalid_http_destination)
  assert_eq(invalid_http_errors.length(), 2)
  
  // Test batch export
  let batch_export = fn(
    data_points: Array[DataPoint], 
    destination: ExportDestination, 
    format: ExportFormat,
    batch_size: Int
  ) {
    let mut batches = []
    
    for i in 0..data_points.length() {
      if i % batch_size == 0 {
        let end_index = if i + batch_size > data_points.length() {
          data_points.length()
        } else {
          i + batch_size
        }
        
        let batch = data_points.slice(i, end_index - i)
        batches = batches.push(batch)
      }
    }
    
    batches.map(fn(batch) {
      let formatted_data = match format {
        ExportFormat::JSON => export_to_json(batch)
        ExportFormat::CSV => export_to_csv(batch)
        ExportFormat::Prometheus => export_to_prometheus(batch)
        ExportFormat::OTLP => export_to_otlp(batch)
      }
      
      {
        destination: destination.endpoint,
        format: match format {
          ExportFormat::JSON => "json"
          ExportFormat::CSV => "csv"
          ExportFormat::Prometheus => "prometheus"
          ExportFormat::OTLP => "otlp"
        },
        data: formatted_data,
        size: batch.length()
      }
    })
  }
  
  let export_batches = batch_export(data_points, valid_http_destination, ExportFormat::JSON, 2)
  assert_eq(export_batches.length(), 2)
  assert_eq(export_batches[0].size, 2)
  assert_eq(export_batches[1].size, 2)
  assert_eq(export_batches[0].format, "json")
  assert_eq(export_batches[0].destination, "https://telemetry-collector:4317")
}

// Test 10: Telemetry Security and Privacy
test "telemetry security and privacy" {
  // Define data sensitivity levels
  enum SensitivityLevel {
    Public
    Internal
    Confidential
    Restricted
  }
  
  // Define PII types
  enum PIIType {
    EmailAddress
    PhoneNumber
    SocialSecurityNumber
    CreditCardNumber
    IPAddress
    Custom(String)
  }
  
  // Define telemetry field with sensitivity
  type TelemetryField = {
    name: String,
    value: String,
    sensitivity: SensitivityLevel,
    pii_types: Array[PIIType]
  }
  
  // Define security policy
  type SecurityPolicy = {
    allowed_sensitivity_levels: Array[SensitivityLevel],
    pii_redaction_enabled: Bool,
    encryption_required: Bool,
    retention_days: Int,
    export_allowed: Bool
  }
  
  // Create sample telemetry fields
  let create_sample_fields = fn() {
    [
      {
        name: "operation_name",
        value: "database_query",
        sensitivity: SensitivityLevel::Public,
        pii_types: []
      },
      {
        name: "user_email",
        value: "user@example.com",
        sensitivity: SensitivityLevel::Confidential,
        pii_types: [PIIType::EmailAddress]
      },
      {
        name: "user_ip",
        value: "192.168.1.100",
        sensitivity: SensitivityLevel::Internal,
        pii_types: [PIIType::IPAddress]
      },
      {
        name: "credit_card",
        value: "4111-1111-1111-1111",
        sensitivity: SensitivityLevel::Restricted,
        pii_types: [PIIType::CreditCardNumber]
      },
      {
        name: "response_time_ms",
        value: "150",
        sensitivity: SensitivityLevel::Public,
        pii_types: []
      }
    ]
  }
  
  let fields = create_sample_fields()
  assert_eq(fields.length(), 5)
  
  // Test PII detection
  let detect_pii = fn(value: String) {
    let mut detected_pii = []
    
    // Email detection
    if value.contains("@") and value.contains(".") {
      detected_pii = detected_pii.push(PIIType::EmailAddress)
    }
    
    // Phone number detection (simplified)
    let digits_only = value.replace("-", "").replace(" ", "").replace("(", "").replace(")", "")
    if digits_only.length() >= 10 and digits_only.length() <= 15 {
      let mut all_digits = true
      for i in 0..digits_only.length() {
        let c = digits_only[i]
        if c < '0' or c > '9' {
          all_digits = false
        }
      }
      
      if all_digits {
        detected_pii = detected_pii.push(PIIType::PhoneNumber)
      }
    }
    
    // IP address detection (simplified)
    let ip_parts = value.split(".")
    if ip_parts.length() == 4 {
      let mut valid_ip = true
      for part in ip_parts {
        let num = part.to_int()
        if num < 0 or num > 255 {
          valid_ip = false
        }
      }
      
      if valid_ip {
        detected_pii = detected_pii.push(PIIType::IPAddress)
      }
    }
    
    // Credit card detection (simplified)
    let cc_digits = value.replace("-", "").replace(" ", "")
    if cc_digits.length() == 16 and cc_digits.starts_with("4") {
      let mut all_digits = true
      for i in 0..cc_digits.length() {
        let c = cc_digits[i]
        if c < '0' or c > '9' {
          all_digits = false
        }
      }
      
      if all_digits {
        detected_pii = detected_pii.push(PIIType::CreditCardNumber)
      }
    }
    
    detected_pii
  }
  
  let email_pii = detect_pii("user@example.com")
  assert_eq(email_pii.length(), 1)
  assert_eq(email_pii[0], PIIType::EmailAddress)
  
  let ip_pii = detect_pii("192.168.1.100")
  assert_eq(ip_pii.length(), 1)
  assert_eq(ip_pii[0], PIIType::IPAddress)
  
  let cc_pii = detect_pii("4111-1111-1111-1111")
  assert_eq(cc_pii.length(), 1)
  assert_eq(cc_pii[0], PIIType::CreditCardNumber)
  
  // Test PII redaction
  let redact_pii = fn(value: String, pii_types: Array[PIIType]) {
    let mut redacted_value = value
    
    for pii_type in pii_types {
      redacted_value = match pii_type {
        PIIType::EmailAddress => {
          let at_index = redacted_value.index_of("@")
          if at_index > 0 {
            let domain = redacted_value.substring(at_index, redacted_value.length() - at_index)
            "*".repeat(at_index) + domain
          } else {
            redacted_value
          }
        }
        PIIType::IPAddress => {
          let parts = redacted_value.split(".")
          if parts.length() == 4 {
            parts[0] + "." + parts[1] + ".*."
          } else {
            redacted_value
          }
        }
        PIIType::CreditCardNumber => {
          let digits = redacted_value.replace("-", "").replace(" ", "")
          if digits.length() == 16 {
            "****-****-****-" + digits.substring(12, 4)
          } else {
            redacted_value
          }
        }
        PIIType::PhoneNumber => {
          let digits = redacted_value.replace("-", "").replace(" ", "").replace("(", "").replace(")", "")
          if digits.length() >= 4 {
            "*".repeat(digits.length() - 4) + digits.substring(digits.length() - 4, 4)
          } else {
            redacted_value
          }
        }
        _ => redacted_value
      }
    }
    
    redacted_value
  }
  
  let redacted_email = redact_pii("user@example.com", [PIIType::EmailAddress])
  assert_eq(redacted_email, "****@example.com")
  
  let redacted_ip = redact_pii("192.168.1.100", [PIIType::IPAddress])
  assert_eq(redacted_ip, "192.168.*.*")
  
  let redacted_cc = redact_pii("4111-1111-1111-1111", [PIIType::CreditCardNumber])
  assert_eq(redacted_cc, "****-****-****-1111")
  
  // Test field filtering based on security policy
  let filter_fields_by_policy = fn(fields: Array[TelemetryField], policy: SecurityPolicy) {
    fields.filter(fn(field) {
      // Check if sensitivity level is allowed
      let sensitivity_allowed = policy.allowed_sensitivity_levels.contains(field.sensitivity)
      
      // Check PII redaction
      let pii_allowed = if policy.pii_redaction_enabled and field.pii_types.length() > 0 {
        false  // PII fields are not allowed when redaction is enabled
      } else {
        true
      }
      
      sensitivity_allowed and pii_allowed
    })
  }
  
  let strict_policy = {
    allowed_sensitivity_levels: [SensitivityLevel::Public],
    pii_redaction_enabled: true,
    encryption_required: true,
    retention_days: 30,
    export_allowed: false
  }
  
  let filtered_fields = filter_fields_by_policy(fields, strict_policy)
  assert_eq(filtered_fields.length(), 2)
  assert_eq(filtered_fields[0].name, "operation_name")
  assert_eq(filtered_fields[1].name, "response_time_ms")
  
  // Test field transformation based on security policy
  let transform_fields_by_policy = fn(fields: Array[TelemetryField], policy: SecurityPolicy) {
    fields.map(fn(field) {
      // Check if sensitivity level is allowed
      let sensitivity_allowed = policy.allowed_sensitivity_levels.contains(field.sensitivity)
      
      if not(sensitivity_allowed) {
        // Field is not allowed, remove it
        None
      } else if policy.pii_redaction_enabled and field.pii_types.length() > 0 {
        // Field has PII and redaction is enabled
        Some({
          name: field.name,
          value: redact_pii(field.value, field.pii_types),
          sensitivity: field.sensitivity,
          pii_types: []  // PII has been redacted
        })
      } else {
        // Field is allowed as-is
        Some(field)
      }
    }).filter(fn(field) { field != None })
     .map(fn(field) { match field { Some(f) => f } })
  }
  
  let lenient_policy = {
    allowed_sensitivity_levels: [SensitivityLevel::Public, SensitivityLevel::Internal, SensitivityLevel::Confidential],
    pii_redaction_enabled: true,
    encryption_required: false,
    retention_days: 90,
    export_allowed: true
  }
  
  let transformed_fields = transform_fields_by_policy(fields, lenient_policy)
  assert_eq(transformed_fields.length(), 4)
  
  // Check that email was redacted
  let email_field = transformed_fields.filter(fn(f) { f.name == "user_email" })[0]
  assert_eq(email_field.value, "****@example.com")
  assert_eq(email_field.pii_types.length(), 0)
  
  // Check that IP was redacted
  let ip_field = transformed_fields.filter(fn(f) { f.name == "user_ip" })[0]
  assert_eq(ip_field.value, "192.168.*.*")
  assert_eq(ip_field.pii_types.length(), 0)
  
  // Check that restricted field was removed
  let restricted_fields = transformed_fields.filter(fn(f) { f.name == "credit_card" })
  assert_eq(restricted_fields.length(), 0)
  
  // Test data encryption simulation
  let encrypt_data = fn(data: String, key: String) {
    // Simple XOR encryption simulation
    let key_bytes = key.to_char_array()
    let data_bytes = data.to_char_array()
    
    let mut encrypted_bytes = []
    for i in 0..data_bytes.length() {
      let key_byte = key_bytes[i % key_bytes.length()]
      let encrypted_byte = data_bytes[i] ^ key_byte
      encrypted_bytes = encrypted_bytes.push(encrypted_byte)
    }
    
    "encrypted:" + encrypted_bytes.map(fn(b) { b.to_string() }).reduce(fn(acc, b) { acc + "," + b }, "")
  }
  
  let decrypt_data = fn(encrypted_data: String, key: String) {
    if not(encrypted_data.starts_with("encrypted:")) {
      encrypted_data
    } else {
      let key_bytes = key.to_char_array()
      let encrypted_part = encrypted_data.substring(10, encrypted_data.length() - 10)
      let encrypted_values = encrypted_part.split(",")
      
      let mut decrypted_bytes = []
      for i in 0..encrypted_values.length() {
        let encrypted_byte = encrypted_values[i].to_int()
        let key_byte = key_bytes[i % key_bytes.length()]
        let decrypted_byte = encrypted_byte ^ key_byte
        decrypted_bytes = decrypted_bytes.push(decrypted_byte)
      }
      
      decrypted_bytes.map(fn(b) { b.to_char() }).reduce(fn(acc, c) { acc + c }, "")
    }
  }
  
  let original_data = "sensitive telemetry data"
  let encryption_key = "secret-key"
  
  let encrypted = encrypt_data(original_data, encryption_key)
  assert_true(encrypted.starts_with("encrypted:"))
  assert_false(encrypted.contains("sensitive"))
  
  let decrypted = decrypt_data(encrypted, encryption_key)
  assert_eq(decrypted, original_data)
  
  // Test access control
  type AccessLevel = {
    read: Bool,
    write: Bool,
    export: Bool,
    delete: Bool
  }
  
  type User = {
    id: String,
    role: String,
    access_level: AccessLevel
  }
  
  let check_access = fn(user: User, action: String, data_sensitivity: SensitivityLevel) {
    let can_access = match action {
      "read" => user.access_level.read
      "write" => user.access_level.write
      "export" => user.access_level.export
      "delete" => user.access_level.delete
      _ => false
    }
    
    if not(can_access) {
      false
    } else {
      // Check role-based sensitivity access
      match user.role {
        "admin" => true
        "analyst" => data_sensitivity != SensitivityLevel::Restricted
        "viewer" => data_sensitivity == SensitivityLevel::Public
        _ => false
      }
    }
  }
  
  let admin_user = {
    id: "user-001",
    role: "admin",
    access_level: { read: true, write: true, export: true, delete: true }
  }
  
  let analyst_user = {
    id: "user-002",
    role: "analyst",
    access_level: { read: true, write: false, export: true, delete: false }
  }
  
  let viewer_user = {
    id: "user-003",
    role: "viewer",
    access_level: { read: true, write: false, export: false, delete: false }
  }
  
  assert_true(check_access(admin_user, "read", SensitivityLevel::Restricted))
  assert_true(check_access(admin_user, "delete", SensitivityLevel::Restricted))
  
  assert_true(check_access(analyst_user, "read", SensitivityLevel::Confidential))
  assert_false(check_access(analyst_user, "read", SensitivityLevel::Restricted))
  assert_false(check_access(analyst_user, "write", SensitivityLevel::Public))
  
  assert_true(check_access(viewer_user, "read", SensitivityLevel::Public))
  assert_false(check_access(viewer_user, "read", SensitivityLevel::Internal))
  assert_false(check_access(viewer_user, "export", SensitivityLevel::Public))
}