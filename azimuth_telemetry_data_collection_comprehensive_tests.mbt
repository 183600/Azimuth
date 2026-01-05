// Azimuth Telemetry Data Collection Comprehensive Test Suite
// This file contains comprehensive test cases for telemetry data collection functionality

// Test 1: Span Creation and Management
test "span creation and management" {
  // Define span types
  type SpanStatus = {
    code: Int,
    message: String
  }
  
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: SpanStatus,
    attributes: Array[(String, String)],
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // Span operations
  let create_span = fn(trace_id: String, span_id: String, operation_name: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      operation_name,
      start_time: 1640995200, // Mock timestamp
      end_time: None,
      status: { code: 1, message: "OK" },
      attributes: [],
      events: []
    }
  }
  
  let set_parent = fn(span: Span, parent_span_id: String) {
    { span | parent_span_id: Some(parent_span_id) }
  }
  
  let add_attribute = fn(span: Span, key: String, value: String) {
    { span | attributes: span.attributes.push((key, value)) }
  }
  
  let add_event = fn(span: Span, event_name: String, event_attributes: Array[(String, String)]) {
    let event = {
      name: event_name,
      timestamp: 1640995250, // Mock timestamp
      attributes: event_attributes
    }
    { span | events: span.events.push(event) }
  }
  
  let end_span = fn(span: Span, end_time: Int, status: SpanStatus) {
    { span | end_time: Some(end_time), status }
  }
  
  // Test span creation
  let span = create_span("trace-123", "span-456", "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.operation_name, "database_query")
  assert_eq(span.parent_span_id, None)
  assert_eq(span.end_time, None)
  assert_eq(span.status.code, 1)
  assert_eq(span.attributes.length(), 0)
  assert_eq(span.events.length(), 0)
  
  // Test setting parent
  let child_span = set_parent(span, "parent-span-789")
  assert_eq(child_span.parent_span_id, Some("parent-span-789"))
  
  // Test adding attributes
  let span_with_attrs = add_attribute(child_span, "db.statement", "SELECT * FROM users")
  let span_with_more_attrs = add_attribute(span_with_attrs, "db.type", "postgresql")
  assert_eq(span_with_more_attrs.attributes.length(), 2)
  assert_true(span_with_more_attrs.attributes.contains(("db.statement", "SELECT * FROM users")))
  assert_true(span_with_more_attrs.attributes.contains(("db.type", "postgresql")))
  
  // Test adding events
  let span_with_event = add_event(span_with_more_attrs, "query.error", [("error.code", "500"), ("error.message", "Connection timeout")])
  assert_eq(span_with_event.events.length(), 1)
  assert_eq(span_with_event.events[0].name, "query.error")
  assert_eq(span_with_event.events[0].attributes.length(), 2)
  
  // Test ending span
  let ended_span = end_span(span_with_event, 1640995300, { code: 2, message: "Error" })
  assert_eq(ended_span.end_time, Some(1640995300))
  assert_eq(ended_span.status.code, 2)
  assert_eq(ended_span.status.message, "Error")
}

// Test 2: Metric Collection
test "metric collection and aggregation" {
  // Define metric types
  type MetricValue = 
    | IntValue(Int)
    | FloatValue(Float)
    | BoolValue(Bool)
  
  type Metric = {
    name: String,
    value: MetricValue,
    unit: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  type MetricAggregation = {
    name: String,
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float
  }
  
  // Metric operations
  let create_int_metric = fn(name: String, value: Int, unit: String) {
    {
      name,
      value: IntValue(value),
      unit,
      timestamp: 1640995200,
      attributes: []
    }
  }
  
  let create_float_metric = fn(name: String, value: Float, unit: String) {
    {
      name,
      value: FloatValue(value),
      unit,
      timestamp: 1640995200,
      attributes: []
    }
  }
  
  let add_metric_attribute = fn(metric: Metric, key: String, value: String) {
    { metric | attributes: metric.attributes.push((key, value)) }
  }
  
  let extract_float_value = fn(metric: Metric) {
    match metric.value {
      IntValue(i) => i.to_float()
      FloatValue(f) => f
      BoolValue(b) => if b { 1.0 } else { 0.0 }
    }
  }
  
  let aggregate_metrics = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      None
    } else {
      let name = metrics[0].name
      let mut count = 0
      let mut sum = 0.0
      let mut min = 999999.0
      let mut max = -999999.0
      
      for metric in metrics {
        let value = extract_float_value(metric)
        count = count + 1
        sum = sum + value
        if value < min { min = value }
        if value > max { max = value }
      }
      
      Some({
        name,
        count,
        sum,
        min,
        max,
        avg: sum / count.to_float()
      })
    }
  }
  
  // Test metric creation
  let int_metric = create_int_metric("request_count", 100, "requests")
  assert_eq(int_metric.name, "request_count")
  match int_metric.value {
    IntValue(v) => assert_eq(v, 100)
    _ => assert_true(false)
  }
  assert_eq(int_metric.unit, "requests")
  
  let float_metric = create_float_metric("response_time", 125.5, "ms")
  assert_eq(float_metric.name, "response_time")
  match float_metric.value {
    FloatValue(v) => assert_eq(v, 125.5)
    _ => assert_true(false)
  }
  assert_eq(float_metric.unit, "ms")
  
  // Test adding attributes
  let metric_with_attrs = add_metric_attribute(int_metric, "service", "api-gateway")
  let metric_with_more_attrs = add_metric_attribute(metric_with_attrs, "endpoint", "/users")
  assert_eq(metric_with_more_attrs.attributes.length(), 2)
  assert_true(metric_with_more_attrs.attributes.contains(("service", "api-gateway")))
  assert_true(metric_with_more_attrs.attributes.contains(("endpoint", "/users")))
  
  // Test metric aggregation
  let metrics = [
    create_float_metric("response_time", 100.0, "ms"),
    create_float_metric("response_time", 150.0, "ms"),
    create_float_metric("response_time", 200.0, "ms"),
    create_float_metric("response_time", 125.0, "ms"),
    create_float_metric("response_time", 175.0, "ms")
  ]
  
  let aggregation = aggregate_metrics(metrics)
  assert_true(aggregation.is_some())
  
  match aggregation {
    Some(agg) => {
      assert_eq(agg.name, "response_time")
      assert_eq(agg.count, 5)
      assert_eq(agg.sum, 750.0)
      assert_eq(agg.min, 100.0)
      assert_eq(agg.max, 200.0)
      assert_eq(agg.avg, 150.0)
    }
    None => assert_true(false)
  }
  
  // Test empty metrics aggregation
  let empty_aggregation = aggregate_metrics([])
  assert_eq(empty_aggregation, None)
}

// Test 3: Log Collection
test "log collection and filtering" {
  // Define log types
  type LogLevel = 
    | Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
  
  type LogRecord = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    attributes: Array[(String, String)],
    trace_id: Option[String],
    span_id: Option[String]
  }
  
  // Log operations
  let level_to_int = fn(level: LogLevel) {
    match level {
      Trace => 0
      Debug => 1
      Info => 2
      Warn => 3
      Error => 4
      Fatal => 5
    }
  }
  
  let create_log = fn(level: LogLevel, message: String) {
    {
      timestamp: 1640995200,
      level,
      message,
      attributes: [],
      trace_id: None,
      span_id: None
    }
  }
  
  let add_log_attribute = fn(log: LogRecord, key: String, value: String) {
    { log | attributes: log.attributes.push((key, value)) }
  }
  
  let set_trace_context = fn(log: LogRecord, trace_id: String, span_id: String) {
    { log | trace_id: Some(trace_id), span_id: Some(span_id) }
  }
  
  let filter_by_level = fn(logs: Array[LogRecord], min_level: LogLevel) {
    let min_level_int = level_to_int(min_level)
    let mut filtered = []
    
    for log in logs {
      let log_level_int = level_to_int(log.level)
      if log_level_int >= min_level_int {
        filtered = filtered.push(log)
      }
    }
    
    filtered
  }
  
  let filter_by_trace = fn(logs: Array[LogRecord], trace_id: String) {
    let mut filtered = []
    
    for log in logs {
      match log.trace_id {
        Some(id) => if id == trace_id { filtered = filtered.push(log) }
        None => ()
      }
    }
    
    filtered
  }
  
  // Test log creation
  let info_log = create_log(Info, "User login successful")
  assert_eq(info_log.level, Info)
  assert_eq(info_log.message, "User login successful")
  assert_eq(info_log.attributes.length(), 0)
  
  let error_log = create_log(Error, "Database connection failed")
  assert_eq(error_log.level, Error)
  assert_eq(error_log.message, "Database connection failed")
  
  // Test adding attributes
  let log_with_attrs = add_log_attribute(info_log, "user_id", "12345")
  let log_with_more_attrs = add_log_attribute(log_with_attrs, "ip_address", "192.168.1.100")
  assert_eq(log_with_more_attrs.attributes.length(), 2)
  assert_true(log_with_more_attrs.attributes.contains(("user_id", "12345")))
  assert_true(log_with_more_attrs.attributes.contains(("ip_address", "192.168.1.100")))
  
  // Test setting trace context
  let log_with_trace = set_trace_context(log_with_more_attrs, "trace-123", "span-456")
  assert_eq(log_with_trace.trace_id, Some("trace-123"))
  assert_eq(log_with_trace.span_id, Some("span-456"))
  
  // Test log filtering by level
  let logs = [
    create_log(Trace, "Debugging info"),
    create_log(Debug, "Processing request"),
    create_log(Info, "Request completed"),
    create_log(Warn, "High memory usage"),
    create_log(Error, "Service unavailable"),
    create_log(Fatal, "System crash")
  ]
  
  let error_and_above = filter_by_level(logs, Error)
  assert_eq(error_and_above.length(), 2)
  assert_eq(error_and_above[0].level, Error)
  assert_eq(error_and_above[1].level, Fatal)
  
  let warn_and_above = filter_by_level(logs, Warn)
  assert_eq(warn_and_above.length(), 3)
  assert_eq(warn_and_above[0].level, Warn)
  assert_eq(warn_and_above[1].level, Error)
  assert_eq(warn_and_above[2].level, Fatal)
  
  // Test log filtering by trace
  let log1 = set_trace_context(create_log(Info, "Operation A"), "trace-1", "span-1")
  let log2 = set_trace_context(create_log(Info, "Operation B"), "trace-2", "span-2")
  let log3 = set_trace_context(create_log(Error, "Operation failed"), "trace-1", "span-3")
  let log4 = create_log(Info, "Operation C") // No trace context
  
  let logs_with_trace = [log1, log2, log3, log4]
  let trace1_logs = filter_by_trace(logs_with_trace, "trace-1")
  assert_eq(trace1_logs.length(), 2)
  assert_eq(trace1_logs[0].message, "Operation A")
  assert_eq(trace1_logs[1].message, "Operation failed")
}

// Test 4: Trace Context Propagation
test "trace context propagation" {
  // Define trace context types
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)]
  }
  
  type TraceParent = {
    version: Int,
    trace_id: String,
    span_id: String,
    trace_flags: Int
  }
  
  // Trace context operations
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      trace_flags: 1, // Sampled
      trace_state: []
    }
  }
  
  let add_trace_state = fn(context: TraceContext, key: String, value: String) {
    { context | trace_state: context.trace_state.push((key, value)) }
  }
  
  let extract_trace_parent = fn(context: TraceContext) {
    {
      version: 0,
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags
    }
  }
  
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id,
      trace_flags: parent.trace_flags,
      trace_state: parent.trace_state
    }
  }
  
  let serialize_trace_parent = fn(tp: TraceParent) {
    "00-" + tp.trace_id + "-" + tp.span_id + "-" + tp.trace_flags.to_string()
  }
  
  let parse_trace_parent = fn(trace_parent_str: String) {
    let parts = trace_parent_str.split("-")
    if parts.length() == 4 {
      Some({
        version: 0,
        trace_id: parts[1],
        span_id: parts[2],
        trace_flags: parts[3].to_int()
      })
    } else {
      None
    }
  }
  
  // Test trace context creation
  let context = create_trace_context("trace-123456789", "span-987654321")
  assert_eq(context.trace_id, "trace-123456789")
  assert_eq(context.span_id, "span-987654321")
  assert_eq(context.trace_flags, 1)
  assert_eq(context.trace_state.length(), 0)
  
  // Test adding trace state
  let context_with_state = add_trace_state(context, "vendor", "acme")
  let context_with_more_state = add_trace_state(context_with_state, "region", "us-west")
  assert_eq(context_with_more_state.trace_state.length(), 2)
  assert_true(context_with_more_state.trace_state.contains(("vendor", "acme")))
  assert_true(context_with_more_state.trace_state.contains(("region", "us-west")))
  
  // Test extracting trace parent
  let trace_parent = extract_trace_parent(context_with_more_state)
  assert_eq(trace_parent.version, 0)
  assert_eq(trace_parent.trace_id, "trace-123456789")
  assert_eq(trace_parent.span_id, "span-987654321")
  assert_eq(trace_parent.trace_flags, 1)
  
  // Test creating child context
  let child_context = create_child_context(context_with_more_state, "child-span-111")
  assert_eq(child_context.trace_id, "trace-123456789")
  assert_eq(child_context.span_id, "child-span-111")
  assert_eq(child_context.trace_flags, 1)
  assert_eq(child_context.trace_state.length(), 2)
  
  // Test trace parent serialization
  let serialized = serialize_trace_parent(trace_parent)
  assert_true(serialized.starts_with("00-"))
  assert_true(serialized.contains("trace-123456789"))
  assert_true(serialized.contains("span-987654321"))
  assert_true(serialized.ends_with("1"))
  
  // Test trace parent parsing
  let parsed = parse_trace_parent(serialized)
  assert_true(parsed.is_some())
  
  match parsed {
    Some(tp) => {
      assert_eq(tp.version, 0)
      assert_eq(tp.trace_id, "trace-123456789")
      assert_eq(tp.span_id, "span-987654321")
      assert_eq(tp.trace_flags, 1)
    }
    None => assert_true(false)
  }
  
  // Test invalid trace parent parsing
  let invalid_parsed = parse_trace_parent("invalid-format")
  assert_eq(invalid_parsed, None)
}

// Test 5: Telemetry Baggage
test "telemetry baggage management" {
  // Define baggage types
  type BaggageEntry = {
    key: String,
    value: String,
    properties: Array[(String, String)]
  }
  
  type Baggage = {
    entries: Array[BaggageEntry]
  }
  
  // Baggage operations
  let create_baggage = fn() {
    { entries: [] }
  }
  
  let add_baggage_entry = fn(baggage: Baggage, key: String, value: String, properties: Array[(String, String)]) {
    let entry = { key, value, properties }
    let mut new_entries = []
    let mut found = false
    
    // Replace existing entry with same key
    for e in baggage.entries {
      if e.key == key {
        new_entries = new_entries.push(entry)
        found = true
      } else {
        new_entries = new_entries.push(e)
      }
    }
    
    // Add new entry if key didn't exist
    if not(found) {
      new_entries = new_entries.push(entry)
    }
    
    { baggage | entries: new_entries }
  }
  
  let get_baggage_value = fn(baggage: Baggage, key: String) {
    let mut result = None
    
    for entry in baggage.entries {
      if entry.key == key {
        result = Some(entry.value)
      }
    }
    
    result
  }
  
  let remove_baggage_entry = fn(baggage: Baggage, key: String) {
    let mut new_entries = []
    
    for entry in baggage.entries {
      if entry.key != key {
        new_entries = new_entries.push(entry)
      }
    }
    
    { baggage | entries: new_entries }
  }
  
  let serialize_baggage = fn(baggage: Baggage) {
    let mut parts = []
    
    for entry in baggage.entries {
      let mut part = entry.key + "=" + entry.value
      
      for (prop_key, prop_value) in entry.properties {
        part = part + ";" + prop_key + "=" + prop_value
      }
      
      parts = parts.push(part)
    }
    
    parts.join(",")
  }
  
  // Test baggage creation
  let baggage = create_baggage()
  assert_eq(baggage.entries.length(), 0)
  
  // Test adding baggage entries
  let baggage1 = add_baggage_entry(baggage, "user.id", "12345", [])
  let baggage2 = add_baggage_entry(baggage1, "service.version", "1.2.3", [("host", "server1")])
  let baggage3 = add_baggage_entry(baggage2, "request.id", "req-67890", [("source", "mobile"), ("priority", "high")])
  
  assert_eq(baggage3.entries.length(), 3)
  
  // Test getting baggage values
  assert_eq(get_baggage_value(baggage3, "user.id"), Some("12345"))
  assert_eq(get_baggage_value(baggage3, "service.version"), Some("1.2.3"))
  assert_eq(get_baggage_value(baggage3, "request.id"), Some("req-67890"))
  assert_eq(get_baggage_value(baggage3, "nonexistent"), None)
  
  // Test updating baggage entries
  let baggage4 = add_baggage_entry(baggage3, "user.id", "54321", [("updated", "true")])
  assert_eq(baggage4.entries.length(), 3) // Still 3 entries, not 4
  assert_eq(get_baggage_value(baggage4, "user.id"), Some("54321"))
  
  // Test removing baggage entries
  let baggage5 = remove_baggage_entry(baggage4, "service.version")
  assert_eq(baggage5.entries.length(), 2)
  assert_eq(get_baggage_value(baggage5, "service.version"), None)
  assert_eq(get_baggage_value(baggage5, "user.id"), Some("54321"))
  assert_eq(get_baggage_value(baggage5, "request.id"), Some("req-67890"))
  
  // Test baggage serialization
  let serialized = serialize_baggage(baggage5)
  assert_true(serialized.contains("user.id=54321"))
  assert_true(serialized.contains("request.id=req-67890"))
  assert_true(serialized.contains("source=mobile"))
  assert_true(serialized.contains("priority=high"))
  assert_true(serialized.contains("updated=true"))
}

// Test 6: Resource Detection
test "resource detection and attributes" {
  // Define resource types
  type Resource = {
    attributes: Array[(String, String)],
    schema_url: Option[String]
  }
  
  // Resource operations
  let create_resource = fn() {
    {
      attributes: [],
      schema_url: None
    }
  }
  
  let add_resource_attribute = fn(resource: Resource, key: String, value: String) {
    let mut new_attributes = []
    let mut found = false
    
    // Replace existing attribute with same key
    for (k, v) in resource.attributes {
      if k == key {
        new_attributes = new_attributes.push((k, value))
        found = true
      } else {
        new_attributes = new_attributes.push((k, v))
      }
    }
    
    // Add new attribute if key didn't exist
    if not(found) {
      new_attributes = new_attributes.push((key, value))
    }
    
    { resource | attributes: new_attributes }
  }
  
  let set_schema_url = fn(resource: Resource, schema_url: String) {
    { resource | schema_url: Some(schema_url) }
  }
  
  let get_resource_attribute = fn(resource: Resource, key: String) {
    let mut result = None
    
    for (k, v) in resource.attributes {
      if k == key {
        result = Some(v)
      }
    }
    
    result
  }
  
  let merge_resources = fn(resource1: Resource, resource2: Resource) {
    let mut merged_attributes = resource1.attributes
    
    for (k, v) in resource2.attributes {
      let mut found = false
      
      // Check if key already exists in merged attributes
      for (existing_k, _) in merged_attributes {
        if existing_k == k {
          found = true
        }
      }
      
      if not(found) {
        merged_attributes = merged_attributes.push((k, v))
      }
    }
    
    {
      attributes: merged_attributes,
      schema_url: resource1.schema_url
    }
  }
  
  // Test resource creation
  let resource = create_resource()
  assert_eq(resource.attributes.length(), 0)
  assert_eq(resource.schema_url, None)
  
  // Test adding resource attributes
  let resource1 = add_resource_attribute(resource, "service.name", "payment-service")
  let resource2 = add_resource_attribute(resource1, "service.version", "1.2.3")
  let resource3 = add_resource_attribute(resource2, "host.name", "server-01")
  let resource4 = add_resource_attribute(resource3, "host.id", "host-12345")
  
  assert_eq(resource4.attributes.length(), 4)
  
  // Test getting resource attributes
  assert_eq(get_resource_attribute(resource4, "service.name"), Some("payment-service"))
  assert_eq(get_resource_attribute(resource4, "service.version"), Some("1.2.3"))
  assert_eq(get_resource_attribute(resource4, "host.name"), Some("server-01"))
  assert_eq(get_resource_attribute(resource4, "host.id"), Some("host-12345"))
  assert_eq(get_resource_attribute(resource4, "nonexistent"), None)
  
  // Test updating resource attributes
  let resource5 = add_resource_attribute(resource4, "service.version", "2.0.0")
  assert_eq(resource5.attributes.length(), 4) // Still 4 attributes, not 5
  assert_eq(get_resource_attribute(resource5, "service.version"), Some("2.0.0"))
  
  // Test setting schema URL
  let resource6 = set_schema_url(resource5, "https://opentelemetry.io/schemas/1.20.0")
  assert_eq(resource6.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // Test resource merging
  let resource_a = add_resource_attribute(create_resource(), "service.name", "auth-service")
  let resource_a2 = add_resource_attribute(resource_a, "service.instance.id", "instance-1")
  
  let resource_b = add_resource_attribute(create_resource(), "service.name", "payment-service") // This should be ignored
  let resource_b2 = add_resource_attribute(resource_b, "host.name", "server-02")
  
  let merged = merge_resources(resource_a2, resource_b2)
  assert_eq(merged.attributes.length(), 3) // service.name from resource_a, service.instance.id, and host.name
  assert_eq(get_resource_attribute(merged, "service.name"), Some("auth-service")) // From resource_a
  assert_eq(get_resource_attribute(merged, "service.instance.id"), Some("instance-1"))
  assert_eq(get_resource_attribute(merged, "host.name"), Some("server-02")) // From resource_b
}

// Test 7: Sampler Operations
test "sampler operations for telemetry data" {
  // Define sampler types
  type SamplingDecision = 
    | Drop
    | RecordOnly
    | RecordAndSample
  
  type SamplingResult = {
    decision: SamplingDecision,
    attributes: Array[(String, String)]
  }
  
  type Sampler = {
    name: String,
    sample: (String, String) -> SamplingResult
  }
  
  // Sampler operations
  let create_always_on_sampler = fn() {
    {
      name: "AlwaysOnSampler",
      sample: fn(trace_id: String, span_name: String) {
        {
          decision: RecordAndSample,
          attributes: []
        }
      }
    }
  }
  
  let create_always_off_sampler = fn() {
    {
      name: "AlwaysOffSampler",
      sample: fn(trace_id: String, span_name: String) {
        {
          decision: Drop,
          attributes: []
        }
      }
    }
  }
  
  let create_trace_id_ratio_sampler = fn(ratio: Float) {
    {
      name: "TraceIdRatioSampler(" + ratio.to_string() + ")",
      sample: fn(trace_id: String, span_name: String) {
        // Simple hash-based sampling using trace ID
        let mut hash = 0
        for i in 0..trace_id.length() {
          hash = (hash + trace_id[i].to_int()) % 1000
        }
        
        let threshold = (ratio * 1000.0).to_int()
        
        if hash < threshold {
          {
            decision: RecordAndSample,
            attributes: [("sampler.type", "traceidratio"), ("sampler.param", ratio.to_string())]
          }
        } else {
          {
            decision: Drop,
            attributes: []
          }
        }
      }
    }
  }
  
  let create_parent_based_sampler = fn(root_sampler: Sampler) {
    {
      name: "ParentBased(" + root_sampler.name + ")",
      sample: fn(trace_id: String, span_name: String) {
        // In a real implementation, this would check parent span's sampling decision
        // For this test, we'll just use the root sampler
        root_sampler.sample(trace_id, span_name)
      }
    }
  }
  
  // Test always on sampler
  let always_on = create_always_on_sampler()
  assert_eq(always_on.name, "AlwaysOnSampler")
  
  let result1 = always_on.sample("trace-123", "operation")
  assert_eq(result1.decision, RecordAndSample)
  assert_eq(result1.attributes.length(), 0)
  
  // Test always off sampler
  let always_off = create_always_off_sampler()
  assert_eq(always_off.name, "AlwaysOffSampler")
  
  let result2 = always_off.sample("trace-456", "operation")
  assert_eq(result2.decision, Drop)
  assert_eq(result2.attributes.length(), 0)
  
  // Test trace ID ratio sampler
  let ratio_50 = create_trace_id_ratio_sampler(0.5)
  assert_true(ratio_50.name.contains("traceidratio"))
  assert_true(ratio_50.name.contains("0.5"))
  
  // Test with different trace IDs
  let result3 = ratio_50.sample("trace-abc", "operation")
  // We can't predict the exact result without knowing the hash implementation
  // But we can verify the structure
  match result3.decision {
    Drop => assert_eq(result3.attributes.length(), 0)
    RecordAndSample => {
      assert_eq(result3.attributes.length(), 2)
      assert_true(result3.attributes.contains(("sampler.type", "traceidratio")))
      assert_true(result3.attributes.contains(("sampler.param", "0.5")))
    }
    RecordOnly => assert_true(false) // Shouldn't happen with this sampler
  }
  
  // Test parent-based sampler
  let parent_based = create_parent_based_sampler(always_on)
  assert_true(parent_based.name.contains("ParentBased"))
  assert_true(parent_based.name.contains("AlwaysOnSampler"))
  
  let result4 = parent_based.sample("trace-789", "operation")
  assert_eq(result4.decision, RecordAndSample)
}

// Test 8: Telemetry Batch Processing
test "telemetry batch processing" {
  // Define batch types
  type ExportableItem = 
    | SpanItem(Span)
    | MetricItem(Metric)
    | LogItem(LogRecord)
  
  type Batch = {
    items: Array[ExportableItem],
    max_size: Int,
    created_at: Int
  }
  
  // Batch operations
  let create_batch = fn(max_size: Int) {
    {
      items: [],
      max_size,
      created_at: 1640995200
    }
  }
  
  let add_to_batch = fn(batch: Batch, item: ExportableItem) {
    if batch.items.length() < batch.max_size {
      { batch | items: batch.items.push(item) }
    } else {
      batch // Batch is full
    }
  }
  
  let is_batch_full = fn(batch: Batch) {
    batch.items.length() >= batch.max_size
  }
  
  let batch_size = fn(batch: Batch) {
    batch.items.length()
  }
  
  let get_batch_items_by_type = fn(batch: Batch, item_type: String) {
    let mut filtered = []
    
    for item in batch.items {
      match (item_type, item) {
        ("span", SpanItem(_)) => filtered = filtered.push(item)
        ("metric", MetricItem(_)) => filtered = filtered.push(item)
        ("log", LogItem(_)) => filtered = filtered.push(item)
        _ => ()
      }
    }
    
    filtered
  }
  
  // Test batch creation
  let batch = create_batch(5)
  assert_eq(batch_size(batch), 0)
  assert_false(is_batch_full(batch))
  assert_eq(batch.max_size, 5)
  
  // Test adding items to batch
  let span = create_span("trace-123", "span-456", "database_query")
  let metric = create_float_metric("response_time", 125.5, "ms")
  let log = create_log(Info, "Request processed")
  
  let batch1 = add_to_batch(batch, SpanItem(span))
  let batch2 = add_to_batch(batch1, MetricItem(metric))
  let batch3 = add_to_batch(batch2, LogItem(log))
  
  assert_eq(batch_size(batch3), 3)
  assert_false(is_batch_full(batch3))
  
  // Fill the batch
  let batch4 = add_to_batch(batch3, SpanItem(span))
  let batch5 = add_to_batch(batch4, MetricItem(metric))
  
  assert_eq(batch_size(batch5), 5)
  assert_true(is_batch_full(batch5))
  
  // Try to add to full batch
  let batch6 = add_to_batch(batch5, LogItem(log))
  assert_eq(batch_size(batch6), 5) // Still 5, not 6
  assert_true(is_batch_full(batch6))
  
  // Test filtering by type
  let span_items = get_batch_items_by_type(batch6, "span")
  assert_eq(span_items.length(), 2)
  
  let metric_items = get_batch_items_by_type(batch6, "metric")
  assert_eq(metric_items.length(), 2)
  
  let log_items = get_batch_items_by_type(batch6, "log")
  assert_eq(log_items.length(), 1)
  
  let empty_items = get_batch_items_by_type(batch6, "nonexistent")
  assert_eq(empty_items.length(), 0)
}