// Azimuth Telemetry Data Structures Test Suite
// This file contains test cases for telemetry data structures and operations

// Test 1: Span Data Structure
test "span data structure operations" {
  type Span = {
    name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  let create_span = fn(name: String, trace_id: String, span_id: String) {
    {
      name,
      trace_id,
      span_id,
      parent_span_id: None,
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok",
      attributes: []
    }
  }
  
  let span = create_span("database_query", "trace-123", "span-456")
  assert_eq(span.name, "database_query")
  assert_eq(span.trace_id, "trace-123")
  assert_eq(span.span_id, "span-456")
  assert_eq(span.parent_span_id, None)
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.end_time, 1640995250)
  assert_eq(span.status, "ok")
  assert_eq(span.attributes.length(), 0)
  
  let duration = span.end_time - span.start_time
  assert_eq(duration, 50)
  
  let child_span = { span | 
    name: "sub_query", 
    span_id: "span-789", 
    parent_span_id: Some("span-456"),
    start_time: 1640995210,
    end_time: 1640995230
  }
  assert_eq(child_span.name, "sub_query")
  assert_eq(child_span.parent_span_id, Some("span-456"))
  assert_eq(child_span.end_time - child_span.start_time, 20)
}

// Test 2: Metric Data Structure
test "metric data structure operations" {
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)],
    metric_type: String
  }
  
  let create_counter = fn(name: String, value: Int) {
    {
      name,
      value: value.to_float(),
      unit: "count",
      timestamp: 1640995200,
      tags: [],
      metric_type: "counter"
    }
  }
  
  let create_gauge = fn(name: String, value: Float) {
    {
      name,
      value,
      unit: "value",
      timestamp: 1640995200,
      tags: [],
      metric_type: "gauge"
    }
  }
  
  let create_histogram = fn(name: String, value: Float, buckets: Array[String]) {
    {
      name,
      value,
      unit: "ms",
      timestamp: 1640995200,
      tags: buckets.map(fn(b) { ("le", b) }),
      metric_type: "histogram"
    }
  }
  
  let counter = create_counter("requests_total", 1000)
  assert_eq(counter.name, "requests_total")
  assert_eq(counter.value, 1000.0)
  assert_eq(counter.unit, "count")
  assert_eq(counter.metric_type, "counter")
  
  let gauge = create_gauge("memory_usage", 512.5)
  assert_eq(gauge.name, "memory_usage")
  assert_eq(gauge.value, 512.5)
  assert_eq(gauge.unit, "value")
  assert_eq(gauge.metric_type, "gauge")
  
  let histogram = create_histogram("request_duration", 150.0, ["100", "200", "500"])
  assert_eq(histogram.name, "request_duration")
  assert_eq(histogram.value, 150.0)
  assert_eq(histogram.unit, "ms")
  assert_eq(histogram.metric_type, "histogram")
  assert_eq(histogram.tags.length(), 3)
  assert_true(histogram.tags.contains(("le", "100")))
  assert_true(histogram.tags.contains(("le", "200")))
  assert_true(histogram.tags.contains(("le", "500")))
}

// Test 3: Log Entry Data Structure
test "log entry data structure operations" {
  type LogLevel = {
    ERROR: Int,
    WARN: Int,
    INFO: Int,
    DEBUG: Int
  }
  
  let log_levels = {
    ERROR: 0,
    WARN: 1,
    INFO: 2,
    DEBUG: 3
  }
  
  type LogEntry = {
    timestamp: Int,
    level: Int,
    message: String,
    trace_id: Option[String],
    span_id: Option[String],
    attributes: Array[(String, String)]
  }
  
  let create_log_entry = fn(level: Int, message: String) {
    {
      timestamp: 1640995200,
      level,
      message,
      trace_id: None,
      span_id: None,
      attributes: []
    }
  }
  
  let error_log = create_log_entry(log_levels.ERROR, "Database connection failed")
  assert_eq(error_log.level, log_levels.ERROR)
  assert_eq(error_log.message, "Database connection failed")
  assert_eq(error_log.trace_id, None)
  assert_eq(error_log.span_id, None)
  
  let info_log = { error_log | 
    level: log_levels.INFO, 
    message: "Request processed successfully",
    trace_id: Some("trace-123"),
    span_id: Some("span-456")
  }
  assert_eq(info_log.level, log_levels.INFO)
  assert_eq(info_log.message, "Request processed successfully")
  assert_eq(info_log.trace_id, Some("trace-123"))
  assert_eq(info_log.span_id, Some("span-456"))
  
  let log_with_attributes = { info_log | 
    attributes: [
      ("user_id", "user-789"),
      ("request_id", "req-101"),
      ("response_time", "250")
    ]
  }
  assert_eq(log_with_attributes.attributes.length(), 3)
  assert_true(log_with_attributes.attributes.contains(("user_id", "user-789")))
  assert_true(log_with_attributes.attributes.contains(("request_id", "req-101")))
  assert_true(log_with_attributes.attributes.contains(("response_time", "250")))
}

// Test 4: Trace Context Data Structure
test "trace context data structure operations" {
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)]
  }
  
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      trace_flags: 1,
      trace_state: []
    }
  }
  
  let context = create_trace_context("trace-123", "span-456")
  assert_eq(context.trace_id, "trace-123")
  assert_eq(context.span_id, "span-456")
  assert_eq(context.trace_flags, 1)
  assert_eq(context.trace_state.length(), 0)
  
  let context_with_state = { context | 
    trace_state: [
      ("vendor1", "value1"),
      ("vendor2", "value2")
    ]
  }
  assert_eq(context_with_state.trace_state.length(), 2)
  assert_true(context_with_state.trace_state.contains(("vendor1", "value1")))
  assert_true(context_with_state.trace_state.contains(("vendor2", "value2")))
  
  let extract_trace_id = fn(context: TraceContext) {
    context.trace_id
  }
  
  let extract_span_id = fn(context: TraceContext) {
    context.span_id
  }
  
  assert_eq(extract_trace_id(context), "trace-123")
  assert_eq(extract_span_id(context), "span-456")
  
  let is_sampled = fn(context: TraceContext) {
    context.trace_flags & 1 == 1
  }
  
  assert_true(is_sampled(context))
  
  let unsampled_context = { context | trace_flags: 0 }
  assert_false(is_sampled(unsampled_context))
}

// Test 5: Resource Data Structure
test "resource data structure operations" {
  type Resource = {
    service_name: String,
    service_version: String,
    service_instance_id: String,
    attributes: Array[(String, String)]
  }
  
  let create_resource = fn(service_name: String, service_version: String) {
    {
      service_name,
      service_version,
      service_instance_id: "instance-" + service_name + "-" + service_version,
      attributes: [
        ("service.name", service_name),
        ("service.version", service_version),
        ("service.instance.id", "instance-" + service_name + "-" + service_version)
      ]
    }
  }
  
  let resource = create_resource("payment-service", "1.2.3")
  assert_eq(resource.service_name, "payment-service")
  assert_eq(resource.service_version, "1.2.3")
  assert_eq(resource.service_instance_id, "instance-payment-service-1.2.3")
  assert_eq(resource.attributes.length(), 3)
  
  let resource_with_env = { resource | 
    attributes: resource.attributes + [
      ("environment", "production"),
      ("region", "us-west-2"),
      ("zone", "us-west-2a")
    ]
  }
  assert_eq(resource_with_env.attributes.length(), 6)
  assert_true(resource_with_env.attributes.contains(("environment", "production")))
  assert_true(resource_with_env.attributes.contains(("region", "us-west-2")))
  assert_true(resource_with_env.attributes.contains(("zone", "us-west-2a")))
  
  let get_attribute = fn(resource: Resource, key: String) {
    let mut found = None
    for (k, v) in resource.attributes {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let service_name_attr = get_attribute(resource_with_env, "service.name")
  assert_eq(service_name_attr, Some("payment-service"))
  
  let env_attr = get_attribute(resource_with_env, "environment")
  assert_eq(env_attr, Some("production"))
  
  let missing_attr = get_attribute(resource_with_env, "missing.key")
  assert_eq(missing_attr, None)
}

// Test 6: Baggage Data Structure
test "baggage data structure operations" {
  type Baggage = {
    entries: Array[(String, String)]
  }
  
  let create_baggage = fn() {
    { entries: [] }
  }
  
  let add_entry = fn(baggage: Baggage, key: String, value: String) {
    let mut found = false
    let mut updated = []
    
    for (k, v) in baggage.entries {
      if k == key {
        updated = updated.push((k, value))
        found = true
      } else {
        updated = updated.push((k, v))
      }
    }
    
    if not(found) {
      updated = updated.push((key, value))
    }
    
    { baggage | entries: updated }
  }
  
  let get_entry = fn(baggage: Baggage, key: String) {
    let mut found = None
    for (k, v) in baggage.entries {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let remove_entry = fn(baggage: Baggage, key: String) {
    let mut filtered = []
    for (k, v) in baggage.entries {
      if k != key {
        filtered = filtered.push((k, v))
      }
    }
    { baggage | entries: filtered }
  }
  
  let baggage = create_baggage()
  assert_eq(baggage.entries.length(), 0)
  
  let baggage1 = add_entry(baggage, "user_id", "user-123")
  assert_eq(baggage1.entries.length(), 1)
  assert_eq(get_entry(baggage1, "user_id"), Some("user-123"))
  
  let baggage2 = add_entry(baggage1, "request_id", "req-456")
  assert_eq(baggage2.entries.length(), 2)
  assert_eq(get_entry(baggage2, "user_id"), Some("user-123"))
  assert_eq(get_entry(baggage2, "request_id"), Some("req-456"))
  
  let baggage3 = add_entry(baggage2, "user_id", "user-789")
  assert_eq(baggage3.entries.length(), 2)
  assert_eq(get_entry(baggage3, "user_id"), Some("user-789"))
  assert_eq(get_entry(baggage3, "request_id"), Some("req-456"))
  
  let baggage4 = remove_entry(baggage3, "request_id")
  assert_eq(baggage4.entries.length(), 1)
  assert_eq(get_entry(baggage4, "user_id"), Some("user-789"))
  assert_eq(get_entry(baggage4, "request_id"), None)
}

// Test 7: Event Data Structure
test "event data structure operations" {
  type Event = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  let create_event = fn(name: String) {
    {
      name,
      timestamp: 1640995200,
      attributes: []
    }
  }
  
  let add_attribute = fn(event: Event, key: String, value: String) {
    { event | 
      attributes: event.attributes + [(key, value)]
    }
  }
  
  let event = create_event("exception")
  assert_eq(event.name, "exception")
  assert_eq(event.timestamp, 1640995200)
  assert_eq(event.attributes.length(), 0)
  
  let event_with_type = add_attribute(event, "exception.type", "NullPointerException")
  assert_eq(event_with_type.attributes.length(), 1)
  assert_true(event_with_type.attributes.contains(("exception.type", "NullPointerException")))
  
  let event_with_message = add_attribute(event_with_type, "exception.message", "Null pointer access")
  assert_eq(event_with_message.attributes.length(), 2)
  assert_true(event_with_message.attributes.contains(("exception.type", "NullPointerException")))
  assert_true(event_with_message.attributes.contains(("exception.message", "Null pointer access")))
  
  let event_with_stack = add_attribute(event_with_message, "exception.stacktrace", "at com.example.Service.process(Service.java:42)")
  assert_eq(event_with_stack.attributes.length(), 3)
  assert_true(event_with_stack.attributes.contains(("exception.type", "NullPointerException")))
  assert_true(event_with_stack.attributes.contains(("exception.message", "Null pointer access")))
  assert_true(event_with_stack.attributes.contains(("exception.stacktrace", "at com.example.Service.process(Service.java:42)")))
}

// Test 8: Link Data Structure
test "link data structure operations" {
  type Link = {
    trace_id: String,
    span_id: String,
    attributes: Array[(String, String)]
  }
  
  let create_link = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      attributes: []
    }
  }
  
  let link = create_link("trace-123", "span-456")
  assert_eq(link.trace_id, "trace-123")
  assert_eq(link.span_id, "span-456")
  assert_eq(link.attributes.length(), 0)
  
  let link_with_attrs = { link | 
    attributes: [
      ("link.type", "parent"),
      ("service", "auth-service")
    ]
  }
  assert_eq(link_with_attrs.attributes.length(), 2)
  assert_true(link_with_attrs.attributes.contains(("link.type", "parent")))
  assert_true(link_with_attrs.attributes.contains(("service", "auth-service")))
  
  let create_follows_from_link = fn(trace_id: String, span_id: String) {
    create_link(trace_id, span_id)
  }
  
  let follows_from_link = create_follows_from_link("trace-789", "span-101")
  assert_eq(follows_from_link.trace_id, "trace-789")
  assert_eq(follows_from_link.span_id, "span-101")
}