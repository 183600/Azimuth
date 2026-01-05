// Azimuth Distributed Tracing Test Suite
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation across service boundaries" {
  // Create trace context
  let trace_id = "trace-1234567890"
  let span_id = "span-0987654321"
  let parent_span_id = "parent-1122334455"
  
  // Validate trace ID format
  assert_eq(trace_id.length(), 16)
  assert_true(trace_id.starts_with("trace-"))
  
  // Validate span ID format
  assert_eq(span_id.length(), 15)
  assert_true(span_id.starts_with("span-"))
  
  // Validate parent span ID format
  assert_eq(parent_span_id.length(), 18)
  assert_true(parent_span_id.starts_with("parent-"))
  
  // Test context serialization
  let trace_context = trace_id + ":" + span_id + ":" + parent_span_id
  assert_eq(trace_context.length(), 51)
  
  // Test context deserialization
  let parts = trace_context.split(":")
  assert_eq(parts.length(), 3)
  assert_eq(parts[0], trace_id)
  assert_eq(parts[1], span_id)
  assert_eq(parts[2], parent_span_id)
}

// Test 2: Span Creation and Lifecycle
test "span creation and lifecycle management" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    tags: Array[(String, String)]
  }
  
  // Create root span
  let root_span = {
    trace_id: "trace-abcdef123456",
    span_id: "span-1234567890",
    parent_span_id: None,
    operation_name: "http_request",
    start_time: 1640995200,
    end_time: None,
    status: "running",
    tags: [("http.method", "GET"), ("http.url", "/api/users")]
  }
  
  // Validate root span
  assert_eq(root_span.trace_id.length(), 16)
  assert_eq(root_span.span_id.length(), 14)
  assert_eq(root_span.operation_name, "http_request")
  assert_eq(root_span.status, "running")
  assert_eq(root_span.tags.length(), 2)
  assert_eq(root_span.end_time, None)
  
  // Create child span
  let child_span = {
    trace_id: root_span.trace_id,
    span_id: "span-0987654321",
    parent_span_id: Some(root_span.span_id),
    operation_name: "database_query",
    start_time: 1640995205,
    end_time: None,
    status: "running",
    tags: [("db.type", "postgresql"), ("db.statement", "SELECT * FROM users")]
  }
  
  // Validate child span
  assert_eq(child_span.trace_id, root_span.trace_id)
  assert_eq(child_span.parent_span_id, Some(root_span.span_id))
  assert_eq(child_span.operation_name, "database_query")
  assert_eq(child_span.tags.length(), 2)
  
  // Complete child span
  let completed_child = { child_span | 
    end_time: Some(1640995210),
    status: "completed"
  }
  
  assert_eq(completed_child.end_time, Some(1640995210))
  assert_eq(completed_child.status, "completed")
  
  // Calculate duration
  let duration = match completed_child.end_time {
    Some(end) => end - completed_child.start_time
    None => 0
  }
  assert_eq(duration, 5)
}

// Test 3: Trace Sampling Strategies
test "trace sampling strategies and decision making" {
  // Define sampling decision types
  enum SamplingDecision {
    RecordAndSample
    Record
    Drop
  }
  
  // Define sampler types
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)
    ParentBased
  }
  
  // Always-on sampler
  let always_on_sampler = fn(trace_id: String) {
    SamplingDecision::RecordAndSample
  }
  
  // Always-off sampler
  let always_off_sampler = fn(trace_id: String) {
    SamplingDecision::Drop
  }
  
  // Ratio-based sampler
  let ratio_sampler = fn(trace_id: String, ratio: Float) {
    // Simple hash-based sampling
    let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
    let normalized = (hash % 100) as Float / 100.0
    if normalized <= ratio {
      SamplingDecision::RecordAndSample
    } else {
      SamplingDecision::Drop
    }
  }
  
  // Parent-based sampler
  let parent_based_sampler = fn(trace_id: String, parent_decision: Option[SamplingDecision]) {
    match parent_decision {
      Some(SamplingDecision::RecordAndSample) => SamplingDecision::RecordAndSample,
      Some(SamplingDecision::Record) => SamplingDecision::Record,
      Some(SamplingDecision::Drop) => SamplingDecision::Drop,
      None => SamplingDecision::RecordAndSample  // Default for root spans
    }
  }
  
  // Test always-on sampler
  let decision1 = always_on_sampler("trace-123")
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  
  // Test always-off sampler
  let decision2 = always_off_sampler("trace-456")
  assert_eq(decision2, SamplingDecision::Drop)
  
  // Test ratio sampler with 50% sampling
  let decision3 = ratio_sampler("trace-789", 0.5)
  // Result depends on hash, but should be either RecordAndSample or Drop
  match decision3 {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)
    SamplingDecision::Record => assert_true(false)  // Should not happen
  }
  
  // Test parent-based sampler
  let decision4 = parent_based_sampler("trace-abc", Some(SamplingDecision::RecordAndSample))
  assert_eq(decision4, SamplingDecision::RecordAndSample)
  
  let decision5 = parent_based_sampler("trace-def", Some(SamplingDecision::Drop))
  assert_eq(decision5, SamplingDecision::Drop)
  
  let decision6 = parent_based_sampler("trace-ghi", None)  // Root span
  assert_eq(decision6, SamplingDecision::RecordAndSample)
}

// Test 4: Baggage Items Propagation
test "baggage items propagation across trace context" {
  // Define baggage item structure
  type BaggageItem = {
    key: String,
    value: String,
    metadata: Option[String>
  }
  
  // Create initial baggage
  let initial_baggage = [
    { key: "user.id", value: "12345", metadata: None },
    { key: "request.id", value: "req-67890", metadata: None },
    { key: "service.version", value: "1.2.3", metadata: Some("propagated") }
  ]
  
  // Validate baggage items
  assert_eq(initial_baggage.length(), 3)
  assert_eq(initial_baggage[0].key, "user.id")
  assert_eq(initial_baggage[0].value, "12345")
  assert_eq(initial_baggage[2].metadata, Some("propagated"))
  
  // Serialize baggage for propagation
  let serialize_baggage = fn(baggage: Array[BaggageItem]) {
    baggage.map(fn(item) {
      match item.metadata {
        Some(meta) => item.key + "=" + item.value + ";" + meta
        None => item.key + "=" + item.value
      }
    }).join(",")
  }
  
  let serialized = serialize_baggage(initial_baggage)
  assert_true(serialized.contains("user.id=12345"))
  assert_true(serialized.contains("request.id=req-67890"))
  assert_true(serialized.contains("service.version=1.2.3;propagated"))
  
  // Deserialize baggage
  let deserialize_baggage = fn(serialized: String) {
    let items_str = serialized.split(",")
    items_str.map(fn(item_str) {
      let parts = item_str.split(";")
      let key_value = parts[0].split("=")
      let key = key_value[0]
      let value = if key_value.length() > 1 { key_value[1] } else { "" }
      let metadata = if parts.length() > 1 { Some(parts[1]) } else { None }
      
      { key, value, metadata }
    })
  }
  
  let deserialized = deserialize_baggage(serialized)
  assert_eq(deserialized.length(), 3)
  assert_eq(deserialized[0].key, "user.id")
  assert_eq(deserialized[0].value, "12345")
  assert_eq(deserialized[2].metadata, Some("propagated"))
  
  // Test baggage item addition
  let add_baggage_item = fn(baggage: Array[BaggageItem], new_item: BaggageItem) {
    let filtered = baggage.filter(fn(item) { item.key != new_item.key })
    filtered.push(new_item)
  }
  
  let new_item = { key: "tenant.id", value: "tenant-999", metadata: None }
  let updated_baggage = add_baggage_item(initial_baggage, new_item)
  assert_eq(updated_baggage.length(), 4)
  
  // Test baggage item update
  let updated_item = { key: "user.id", value: "54321", metadata: None }
  let final_baggage = add_baggage_item(updated_baggage, updated_item)
  assert_eq(final_baggage.length(), 4)
  
  // Find updated user.id
  let updated_user = final_baggage.find(fn(item) { item.key == "user.id" })
  match updated_user {
    Some(user_item) => assert_eq(user_item.value, "54321")
    None => assert_true(false)
  }
}

// Test 5: Trace Linking and Joins
test "trace linking and joining operations" {
  // Define trace link structure
  type TraceLink = {
    trace_id: String,
    span_id: String,
    trace_state: String,
    attributes: Array[(String, String)]
  }
  
  // Create trace links
  let links = [
    {
      trace_id: "trace-1111111111",
      span_id: "span-1111111111",
      trace_state: "ro=1",
      attributes: [("link.type", "causality"), ("service", "auth-service")]
    },
    {
      trace_id: "trace-2222222222",
      span_id: "span-2222222222",
      trace_state: "ro=0",
      attributes: [("link.type", "follows-from"), ("service", "user-service")]
    }
  ]
  
  // Validate links
  assert_eq(links.length(), 2)
  assert_eq(links[0].trace_id, "trace-1111111111")
  assert_eq(links[1].attributes.length(), 2)
  
  // Test link filtering
  let filter_links_by_attribute = fn(links: Array[TraceLink], key: String, value: String) {
    links.filter(fn(link) {
      link.attributes.any(fn(attr) { attr.0 == key && attr.1 == value })
    })
  }
  
  let causality_links = filter_links_by_attribute(links, "link.type", "causality")
  assert_eq(causality_links.length(), 1)
  assert_eq(causality_links[0].trace_id, "trace-1111111111")
  
  let service_links = filter_links_by_attribute(links, "service", "user-service")
  assert_eq(service_links.length(), 1)
  assert_eq(service_links[0].trace_id, "trace-2222222222")
  
  // Test trace state merging
  let merge_trace_states = fn(links: Array[TraceLink]) {
    let mut states = []
    for link in links {
      if not(states.contains(link.trace_state)) {
        states = states.push(link.trace_state)
      }
    }
    states.join(",")
  }
  
  let merged_state = merge_trace_states(links)
  assert_true(merged_state.contains("ro=1"))
  assert_true(merged_state.contains("ro=0"))
  
  // Test link attribute aggregation
  let aggregate_attributes = fn(links: Array[TraceLink]) {
    let mut result = []
    for link in links {
      for attr in link.attributes {
        if not(result.any(fn(existing) { existing.0 == attr.0 && existing.1 == attr.1 })) {
          result = result.push(attr)
        }
      }
    }
    result
  }
  
  let aggregated = aggregate_attributes(links)
  assert_eq(aggregated.length(), 3)  // link.type appears twice with different values
  assert_true(aggregated.contains(("link.type", "causality")))
  assert_true(aggregated.contains(("link.type", "follows-from")))
  assert_true(aggregated.contains(("service", "auth-service")))
}

// Test 6: Span Event Management
test "span event management and annotation" {
  // Define span event structure
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // Create span with events
  type SpanWithEvents = {
    trace_id: String,
    span_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    events: Array[SpanEvent]
  }
  
  // Create initial span
  let span = {
    trace_id: "trace-events-123",
    span_id: "span-events-456",
    operation_name: "user_registration",
    start_time: 1640995200,
    end_time: None,
    events: []
  }
  
  // Add events to span
  let add_event = fn(current_span: SpanWithEvents, event_name: String, attributes: Array[(String, String)>) {
    let new_event = {
      timestamp: 1640995200 + current_span.events.length() * 100,
      name: event_name,
      attributes
    }
    
    { current_span | 
      events: current_span.events.push(new_event)
    }
  }
  
  // Add validation event
  let span_with_validation = add_event(span, "validation.started", [
    ("validation.type", "email"),
    ("validation.input", "user@example.com")
  ])
  
  assert_eq(span_with_validation.events.length(), 1)
  assert_eq(span_with_validation.events[0].name, "validation.started")
  assert_eq(span_with_validation.events[0].attributes.length(), 2)
  
  // Add database event
  let span_with_db = add_event(span_with_validation, "db.query.started", [
    ("db.type", "postgresql"),
    ("db.table", "users"),
    ("db.operation", "INSERT")
  ])
  
  assert_eq(span_with_db.events.length(), 2)
  assert_eq(span_with_db.events[1].name, "db.query.started")
  
  // Add completion event
  let completed_span = add_event(span_with_db, "registration.completed", [
    ("user.id", "12345"),
    ("registration.status", "success")
  ])
  
  assert_eq(completed_span.events.length(), 3)
  
  // Test event filtering
  let filter_events_by_name = fn(events: Array[SpanEvent], name_pattern: String) {
    events.filter(fn(event) { event.name.contains(name_pattern) })
  }
  
  let db_events = filter_events_by_name(completed_span.events, "db")
  assert_eq(db_events.length(), 1)
  assert_eq(db_events[0].name, "db.query.started")
  
  let validation_events = filter_events_by_name(completed_span.events, "validation")
  assert_eq(validation_events.length(), 1)
  assert_eq(validation_events[0].attributes[0], ("validation.type", "email"))
  
  // Test event timeline analysis
  let analyze_event_timeline = fn(events: Array[SpanEvent]) {
    if events.length() == 0 {
      { duration: 0, event_count: 0 }
    } else {
      let first_event = events[0]
      let last_event = events[events.length() - 1]
      let duration = last_event.timestamp - first_event.timestamp
      
      { duration, event_count: events.length() }
    }
  }
  
  let timeline = analyze_event_timeline(completed_span.events)
  assert_eq(timeline.event_count, 3)
  assert_eq(timeline.duration, 200)  // 3 events * 100ms apart
}

// Test 7: Distributed Trace Analysis
test "distributed trace analysis and insights" {
  // Define trace analysis metrics
  type TraceMetrics = {
    total_spans: Int,
    total_duration: Int,
    service_count: Int,
    error_count: Int,
    critical_path_duration: Int
  }
  
  // Define span data structure for analysis
  type SpanData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String
  }
  
  // Create sample trace data
  let spans = [
    {
      trace_id: "trace-analysis-001",
      span_id: "span-001",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "http.request",
      start_time: 1640995200,
      end_time: Some(1640995250),
      status: "ok"
    },
    {
      trace_id: "trace-analysis-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      service_name: "auth-service",
      operation_name: "authenticate",
      start_time: 1640995205,
      end_time: Some(1640995220),
      status: "ok"
    },
    {
      trace_id: "trace-analysis-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      service_name: "user-service",
      operation_name: "get_user",
      start_time: 1640995225,
      end_time: Some(1640995240),
      status: "ok"
    },
    {
      trace_id: "trace-analysis-001",
      span_id: "span-004",
      parent_span_id: Some("span-003"),
      service_name: "database",
      operation_name: "query",
      start_time: 1640995230,
      end_time: Some(1640995235),
      status: "ok"
    },
    {
      trace_id: "trace-analysis-001",
      span_id: "span-005",
      parent_span_id: Some("span-001"),
      service_name: "notification-service",
      operation_name: "send_email",
      start_time: 1640995245,
      end_time: Some(1640995260),
      status: "error"
    }
  ]
  
  // Analyze trace metrics
  let analyze_trace = fn(span_data: Array[SpanData]) {
    let trace_id = if span_data.length() > 0 { span_data[0].trace_id } else { "" }
    
    // Calculate total spans
    let total_spans = span_data.length()
    
    // Calculate total duration (root span duration)
    let root_spans = span_data.filter(fn(s) { s.parent_span_id == None })
    let total_duration = match root_spans.length() {
      1 => match root_spans[0].end_time {
        Some(end) => end - root_spans[0].start_time
        None => 0
      }
      _ => 0
    }
    
    // Count unique services
    let service_names = span_data.map(fn(s) { s.service_name })
    let unique_services = service_names.filter(fn(service, index) {
      not(service_names.slice(0, index).contains(service))
    })
    let service_count = unique_services.length()
    
    // Count errors
    let error_count = span_data.filter(fn(s) { s.status == "error" }).length()
    
    // Calculate critical path (longest path through the trace)
    let calculate_span_duration = fn(span: SpanData) {
      match span.end_time {
        Some(end) => end - span.start_time
        None => 0
      }
    }
    
    let span_durations = span_data.map(calculate_span_duration)
    let critical_path_duration = span_durations.reduce(fn(max, duration) {
      if duration > max { duration } else { max }
    }, 0)
    
    {
      total_spans,
      total_duration,
      service_count,
      error_count,
      critical_path_duration
    }
  }
  
  // Perform trace analysis
  let metrics = analyze_trace(spans)
  assert_eq(metrics.total_spans, 5)
  assert_eq(metrics.total_duration, 50)  // Root span: 1640995250 - 1640995200
  assert_eq(metrics.service_count, 4)  // api-gateway, auth-service, user-service, database, notification-service
  assert_eq(metrics.error_count, 1)    // notification-service error
  assert_eq(metrics.critical_path_duration, 15)  // Longest span duration
  
  // Test service dependency analysis
  let analyze_dependencies = fn(span_data: Array[SpanData]) {
    let dependencies = []
    
    for span in span_data {
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_span = span_data.find(fn(s) { s.span_id == parent_id })
          match parent_span {
            Some(parent) => {
              let dependency = (parent.service_name, span.service_name)
              if not(dependencies.contains(dependency)) {
                dependencies = dependencies.push(dependency)
              }
            }
            None => {}
          }
        }
        None => {}
      }
    }
    
    dependencies
  }
  
  let dependencies = analyze_dependencies(spans)
  assert_eq(dependencies.length(), 3)
  assert_true(dependencies.contains(("api-gateway", "auth-service")))
  assert_true(dependencies.contains(("api-gateway", "user-service")))
  assert_true(dependencies.contains(("user-service", "database")))
  
  // Test bottleneck identification
  let identify_bottlenecks = fn(span_data: Array[SpanData]) {
    let span_durations = span_data.map(fn(span) {
      let duration = match span.end_time {
        Some(end) => end - span.start_time
        None => 0
      }
      (span.span_id, span.operation_name, span.service_name, duration)
    })
    
    // Sort by duration (descending)
    let sorted = span_durations.sort(fn(a, b) {
      if a.3 > b.3 { -1 } else if a.3 < b.3 { 1 } else { 0 }
    })
    
    // Return top 3 longest spans
    sorted.slice(0, if sorted.length() > 3 { 3 } else { sorted.length() })
  }
  
  let bottlenecks = identify_bottlenecks(spans)
  assert_eq(bottlenecks.length(), 3)
  assert_eq(bottlenecks[0].3, 15)  // Longest span
}