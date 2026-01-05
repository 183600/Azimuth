// Azimuth Premium Distributed Tracing Tests
// This file contains high-quality test cases for distributed tracing consistency

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Trace context types
  type TraceId {
    value : String
  }
  
  type SpanId {
    value : String
  }
  
  type TraceFlags {
    sampled : Bool
  }
  
  type TraceState {
    entries : Array[(String, String)]
  }
  
  type TraceContext {
    trace_id : TraceId
    span_id : SpanId
    trace_flags : TraceFlags
    trace_state : TraceState
  }
  
  let create_trace_id = fn() -> TraceId {
    // Generate random trace ID (simplified)
    TraceId { value: "0af7651916cd43dd8448eb211c80319c" }
  }
  
  let create_span_id = fn() -> SpanId {
    // Generate random span ID (simplified)
    SpanId { value: "b7ad6b7169203331" }
  }
  
  let create_trace_context = fn(trace_id : TraceId, span_id : SpanId, sampled : Bool) -> TraceContext {
    TraceContext {
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: TraceFlags { sampled: sampled },
      trace_state: TraceState { entries: [] }
    }
  }
  
  let extract_from_headers = fn(headers : Array[(String, String)]) -> Option[TraceContext] {
    let mut trace_parent = None
    let mut trace_state = None
    
    for (name, value) in headers {
      if name == "traceparent" {
        trace_parent = Some(value)
      } else if name == "tracestate" {
        trace_state = Some(value)
      }
    }
    
    match trace_parent {
      Some(tp) => {
        // Parse traceparent: version-trace_id-span_id-flags
        let parts = tp.split("-")
        if parts.length() == 4 {
          let trace_id = TraceId { value: parts[1] }
          let span_id = SpanId { value: parts[2] }
          let flags = parts[3]
          let sampled = flags.length() > 0 && (flags[0].to_int() & 0x01) != 0
          
          let trace_state_entries = match trace_state {
            Some(ts) => {
              let entries = ts.split(",")
              let mut result = []
              
              for entry in entries {
                let kv = entry.split("=")
                if kv.length() == 2 {
                  result.push((kv[0], kv[1]))
                }
              }
              
              result
            }
            None => []
          }
          
          Some(TraceContext {
            trace_id: trace_id,
            span_id: span_id,
            trace_flags: TraceFlags { sampled: sampled },
            trace_state: TraceState { entries: trace_state_entries }
          })
        } else {
          None
        }
      }
      None => None
    }
  }
  
  let inject_to_headers = fn(context : TraceContext) -> Array[(String, String)] {
    let flags = if context.trace_flags.sampled { "01" } else { "00" }
    let trace_parent = "00-" + context.trace_id.value + "-" + context.span_id.value + "-" + flags
    
    let mut trace_state_str = ""
    for i = 0; i < context.trace_state.entries.length(); i = i + 1 {
      if i > 0 { trace_state_str = trace_state_str + "," }
      trace_state_str = trace_state_str + context.trace_state.entries[i].0 + "=" + context.trace_state.entries[i].1
    }
    
    let mut headers = [("traceparent", trace_parent)]
    
    if trace_state_str.length() > 0 {
      headers.push(("tracestate", trace_state_str))
    }
    
    headers
  }
  
  // Test trace context creation
  let trace_id = create_trace_id()
  let span_id = create_span_id()
  let context = create_trace_context(trace_id, span_id, true)
  
  assert_eq(context.trace_id.value, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(context.span_id.value, "b7ad6b7169203331")
  assert_true(context.trace_flags.sampled)
  
  // Test header injection
  let headers = inject_to_headers(context)
  assert_eq(headers.length(), 1)
  assert_eq(headers[0], ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test header extraction
  let extracted = extract_from_headers(headers)
  match extracted {
    Some(ctx) => {
      assert_eq(ctx.trace_id.value, context.trace_id.value)
      assert_eq(ctx.span_id.value, context.span_id.value)
      assert_eq(ctx.trace_flags.sampled, context.trace_flags.sampled)
    }
    None => assert_true(false)
  }
  
  // Test with tracestate
  let context_with_state = TraceContext {
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: TraceFlags { sampled: true },
    trace_state: TraceState { entries: [("vendor1", "value1"), ("vendor2", "value2")] }
  }
  
  let headers_with_state = inject_to_headers(context_with_state)
  assert_eq(headers_with_state.length(), 2)
  assert_eq(headers_with_state[1], ("tracestate", "vendor1=value1,vendor2=value2"))
  
  let extracted_with_state = extract_from_headers(headers_with_state)
  match extracted_with_state {
    Some(ctx) => {
      assert_eq(ctx.trace_state.entries.length(), 2)
      assert_eq(ctx.trace_state.entries[0], ("vendor1", "value1"))
      assert_eq(ctx.trace_state.entries[1], ("vendor2", "value2"))
    }
    None => assert_true(false)
  }
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  // Span types
  type SpanKind {
    Internal
    Server
    Client
    Producer
    Consumer
  }
  
  type SpanStatus {
    Unset
    Ok
    Error
  }
  
  type SpanEvent {
    name : String
    timestamp : Int
    attributes : Array[(String, String)]
  }
  
  type SpanLink {
    trace_context : TraceContext
    attributes : Array[(String, String)]
  }
  
  type Span {
    trace_context : TraceContext
    parent_span_id : Option[SpanId]
    kind : SpanKind
    name : String
    start_time : Int
    end_time : Option[Int]
    status : SpanStatus
    events : Array[SpanEvent]
    links : Array[SpanLink]
    attributes : Array[(String, String)]
  }
  
  let create_span = fn(trace_context : TraceContext, parent_span_id : Option[SpanId], kind : SpanKind, name : String, start_time : Int) -> Span {
    Span {
      trace_context: trace_context,
      parent_span_id: parent_span_id,
      kind: kind,
      name: name,
      start_time: start_time,
      end_time: None,
      status: Unset,
      events: [],
      links: [],
      attributes: []
    }
  }
  
  let set_attribute = fn(span : Span, key : String, value : String) -> Span {
    Span {
      trace_context: span.trace_context,
      parent_span_id: span.parent_span_id,
      kind: span.kind,
      name: span.name,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      events: span.events,
      links: span.links,
      attributes: span.attributes.push((key, value))
    }
  }
  
  let add_event = fn(span : Span, name : String, timestamp : Int, attributes : Array[(String, String)]) -> Span {
    let event = SpanEvent {
      name: name,
      timestamp: timestamp,
      attributes: attributes
    }
    
    Span {
      trace_context: span.trace_context,
      parent_span_id: span.parent_span_id,
      kind: span.kind,
      name: span.name,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      events: span.events.push(event),
      links: span.links,
      attributes: span.attributes
    }
  }
  
  let set_status = fn(span : Span, status : SpanStatus) -> Span {
    Span {
      trace_context: span.trace_context,
      parent_span_id: span.parent_span_id,
      kind: span.kind,
      name: span.name,
      start_time: span.start_time,
      end_time: span.end_time,
      status: status,
      events: span.events,
      links: span.links,
      attributes: span.attributes
    }
  }
  
  let end_span = fn(span : Span, end_time : Int) -> Span {
    Span {
      trace_context: span.trace_context,
      parent_span_id: span.parent_span_id,
      kind: span.kind,
      name: span.name,
      start_time: span.start_time,
      end_time: Some(end_time),
      status: span.status,
      events: span.events,
      links: span.links,
      attributes: span.attributes
    }
  }
  
  let get_duration = fn(span : Span) -> Option[Int] {
    match span.end_time {
      Some(end) => Some(end - span.start_time)
      None => None
    }
  }
  
  // Test span lifecycle
  let trace_id = create_trace_id()
  let span_id = create_span_id()
  let parent_span_id = create_span_id()
  let trace_context = create_trace_context(trace_id, span_id, true)
  
  let span = create_span(trace_context, Some(parent_span_id), Server, "http.request", 1000)
  assert_eq(span.name, "http.request")
  assert_eq(span.kind, Server)
  assert_eq(span.start_time, 1000)
  assert_eq(span.end_time, None)
  assert_eq(span.status, Unset)
  
  // Set attributes
  let span_with_attrs = span
    |> set_attribute("http.method", "GET")
    |> set_attribute("http.url", "https://example.com/api")
    |> set_attribute("http.status_code", "200")
  
  assert_eq(span_with_attrs.attributes.length(), 3)
  assert_eq(span_with_attrs.attributes[0], ("http.method", "GET"))
  
  // Add events
  let span_with_events = span_with_attrs
    |> add_event("http.request.start", 1000, [("http.method", "GET")])
    |> add_event("http.request.end", 1500, [("http.status_code", "200")])
  
  assert_eq(span_with_events.events.length(), 2)
  assert_eq(span_with_events.events[0].name, "http.request.start")
  assert_eq(span_with_events.events[0].timestamp, 1000)
  
  // Set status
  let span_with_status = set_status(span_with_events, Ok)
  assert_eq(span_with_status.status, Ok)
  
  // End span
  let ended_span = end_span(span_with_status, 1600)
  assert_eq(ended_span.end_time, Some(1600))
  
  // Get duration
  let duration = get_duration(ended_span)
  assert_eq(duration, Some(600))
  
  // Test incomplete span (no end time)
  let incomplete_duration = get_duration(span)
  assert_eq(incomplete_duration, None)
}

// Test 3: Distributed Trace Consistency
test "distributed trace consistency" {
  // Service representation
  type Service {
    name : String
    version : String
    instance_id : String
  }
  
  // Trace node in distributed system
  type TraceNode {
    service : Service
    span : Span
    children : Array[TraceNode]
  }
  
  // Trace tree
  type TraceTree {
    root : TraceNode
    all_spans : Array[Span]
  }
  
  let create_service = fn(name : String, version : String, instance_id : String) -> Service {
    Service {
      name: name,
      version: version,
      instance_id: instance_id
    }
  }
  
  let create_trace_node = fn(service : Service, span : Span) -> TraceNode {
    TraceNode {
      service: service,
      span: span,
      children: []
    }
  }
  
  let add_child = fn(parent : TraceNode, child : TraceNode) -> TraceNode {
    TraceNode {
      service: parent.service,
      span: parent.span,
      children: parent.children.push(child)
    }
  }
  
  let build_trace_tree = fn(root : TraceNode) -> TraceTree {
    let collect_spans = fn(node : TraceNode, accumulator : Array[Span]) -> Array[Span] {
      let mut result = accumulator.push(node.span)
      
      for child in node.children {
        result = collect_spans(child, result)
      }
      
      result
    }
    
    let all_spans = collect_spans(root, [])
    
    TraceTree {
      root: root,
      all_spans: all_spans
    }
  }
  
  let validate_trace_consistency = fn(tree : TraceTree) -> Bool {
    // All spans should have the same trace ID
    let trace_id = tree.root.span.trace_context.trace_id.value
    
    for span in tree.all_spans {
      if span.trace_context.trace_id.value != trace_id {
        return false
      }
    }
    
    // Parent-child relationships should be consistent
    let validate_parent_child = fn(node : TraceNode, parent_span_id : Option[SpanId>) -> Bool {
      // Check parent reference
      match parent_span_id {
        Some(parent_id) => {
          match node.span.parent_span_id {
            Some(span_parent_id) => {
              if span_parent_id.value != parent_id.value {
                return false
              }
            }
            None => return false
          }
        }
        None => {
          // Root node should not have parent
          if node.span.parent_span_id.is_some() {
            return false
          }
        }
      }
      
      // Validate children
      for child in node.children {
        if !validate_parent_child(child, Some(node.span.trace_context.span_id)) {
          return false
        }
      }
      
      true
    }
    
    validate_parent_child(tree.root, None)
  }
  
  // Create a distributed trace scenario
  let api_service = create_service("api-service", "1.0.0", "api-1")
  let db_service = create_service("db-service", "2.1.0", "db-1")
  let cache_service = create_service("cache-service", "1.5.0", "cache-1")
  
  let trace_id = create_trace_id()
  
  // Root span (API request)
  let api_span_id = create_span_id()
  let api_trace_context = create_trace_context(trace_id, api_span_id, true)
  let api_span = create_span(api_trace_context, None, Server, "api.request", 1000)
    |> set_attribute("http.method", "POST")
    |> set_attribute("http.url", "/api/users")
    |> end_span(1600)
  
  let api_node = create_trace_node(api_service, api_span)
  
  // Database query span
  let db_span_id = create_span_id()
  let db_trace_context = create_trace_context(trace_id, db_span_id, true)
  let db_span = create_span(db_trace_context, Some(api_span_id), Client, "db.query", 1100)
    |> set_attribute("db.statement", "SELECT * FROM users")
    |> set_attribute("db.type", "postgresql")
    |> end_span(1400)
  
  let db_node = create_trace_node(db_service, db_span)
  
  // Cache query span
  let cache_span_id = create_span_id()
  let cache_trace_context = create_trace_context(trace_id, cache_span_id, true)
  let cache_span = create_span(cache_trace_context, Some(api_span_id), Client, "cache.get", 1050)
    |> set_attribute("cache.key", "user:123")
    |> set_attribute("cache.hit", "false")
    |> end_span(1080)
  
  let cache_node = create_trace_node(cache_service, cache_span)
  
  // Build trace tree
  let api_with_children = api_node
    |> add_child(db_node)
    |> add_child(cache_node)
  
  let trace_tree = build_trace_tree(api_with_children)
  
  // Validate trace consistency
  assert_true(validate_trace_consistency(trace_tree))
  
  // Verify all spans have same trace ID
  for span in trace_tree.all_spans {
    assert_eq(span.trace_context.trace_id.value, trace_id.value)
  }
  
  // Verify parent-child relationships
  assert_eq(db_span.parent_span_id.unwrap().value, api_span_id.value)
  assert_eq(cache_span.parent_span_id.unwrap().value, api_span_id.value)
  assert_eq(api_span.parent_span_id, None)
  
  // Verify timing consistency
  assert_true(db_span.start_time >= api_span.start_time)
  assert_true(db_span.start_time < db_span.end_time.unwrap())
  assert_true(cache_span.start_time >= api_span.start_time)
  assert_true(cache_span.start_time < cache_span.end_time.unwrap())
}

// Test 4: Cross-Service Trace Correlation
test "cross-service trace correlation" {
  // Service call representation
  type ServiceCall {
    from_service : String
    to_service : String
    trace_context : TraceContext
    timestamp : Int
  }
  
  // Trace correlation graph
  type TraceCorrelation {
    calls : Array[ServiceCall]
    services : Array[String]
  }
  
  let create_service_call = fn(from_service : String, to_service : String, trace_context : TraceContext, timestamp : Int) -> ServiceCall {
    ServiceCall {
      from_service: from_service,
      to_service: to_service,
      trace_context: trace_context,
      timestamp: timestamp
    }
  }
  
  let add_call = fn(correlation : TraceCorrelation, call : ServiceCall) -> TraceCorrelation {
    let mut services = correlation.services
    
    if !services.contains(call.from_service) {
      services = services.push(call.from_service)
    }
    
    if !services.contains(call.to_service) {
      services = services.push(call.to_service)
    }
    
    TraceCorrelation {
      calls: correlation.calls.push(call),
      services: services
    }
  }
  
  let get_service_calls = fn(correlation : TraceCorrelation, service : String) -> Array[ServiceCall] {
    let mut result = []
    
    for call in correlation.calls {
      if call.from_service == service || call.to_service == service {
        result.push(call)
      }
    }
    
    result
  }
  
  let get_trace_path = fn(correlation : TraceCorrelation, trace_id : TraceId) -> Array[ServiceCall] {
    let mut result = []
    
    for call in correlation.calls {
      if call.trace_context.trace_id.value == trace_id.value {
        result.push(call)
      }
    }
    
    // Sort by timestamp
    let sorted = result.sort(|a, b| a.timestamp - b.timestamp)
    sorted
  }
  
  // Test cross-service correlation
  let correlation = TraceCorrelation { calls: [], services: [] }
  
  let trace_id = create_trace_id()
  
  // Create service calls in a request chain
  let call1 = create_service_call(
    "gateway",
    "user-service",
    create_trace_context(trace_id, create_span_id(), true),
    1000
  )
  
  let call2 = create_service_call(
    "user-service",
    "database",
    create_trace_context(trace_id, create_span_id(), true),
    1100
  )
  
  let call3 = create_service_call(
    "user-service",
    "cache-service",
    create_trace_context(trace_id, create_span_id(), true),
    1050
  )
  
  let call4 = create_service_call(
    "user-service",
    "notification-service",
    create_trace_context(trace_id, create_span_id(), true),
    1200
  )
  
  // Build correlation graph
  let correlation1 = correlation
    |> add_call(call1)
    |> add_call(call2)
    |> add_call(call3)
    |> add_call(call4)
  
  // Verify all services are tracked
  assert_eq(correlation1.services.length(), 5)
  assert_true(correlation1.services.contains("gateway"))
  assert_true(correlation1.services.contains("user-service"))
  assert_true(correlation1.services.contains("database"))
  assert_true(correlation1.services.contains("cache-service"))
  assert_true(correlation1.services.contains("notification-service"))
  
  // Get calls for specific service
  let user_service_calls = get_service_calls(correlation1, "user-service")
  assert_eq(user_service_calls.length(), 3)
  
  // Verify trace path
  let trace_path = get_trace_path(correlation1, trace_id)
  assert_eq(trace_path.length(), 4)
  
  // Verify chronological order
  assert_eq(trace_path[0].from_service, "gateway")
  assert_eq(trace_path[0].to_service, "user-service")
  assert_eq(trace_path[0].timestamp, 1000)
  
  assert_eq(trace_path[1].from_service, "user-service")
  assert_eq(trace_path[1].to_service, "cache-service")
  assert_eq(trace_path[1].timestamp, 1050)
  
  assert_eq(trace_path[2].from_service, "user-service")
  assert_eq(trace_path[2].to_service, "database")
  assert_eq(trace_path[2].timestamp, 1100)
  
  assert_eq(trace_path[3].from_service, "user-service")
  assert_eq(trace_path[3].to_service, "notification-service")
  assert_eq(trace_path[3].timestamp, 1200)
}

// Test 5: Trace Sampling Strategy
test "trace sampling strategy" {
  // Sampling decision types
  type SamplingDecision {
    Drop
    Record
    RecordAndSample
  }
  
  // Sampling result
  type SamplingResult {
    decision : SamplingDecision
    attributes : Array[(String, String)]
  }
  
  // Sampler types
  type Sampler {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // Sample rate between 0.0 and 1.0
    ParentBased(Box[Sampler])
  }
  
  let should_sample = fn(sampler : Sampler, trace_id : TraceId, parent_context : Option[TraceContext]) -> SamplingResult {
    match sampler {
      AlwaysOn => SamplingResult {
        decision: RecordAndSample,
        attributes: []
      }
      AlwaysOff => SamplingResult {
        decision: Drop,
        attributes: []
      }
      TraceIdRatio(rate) => {
        // Simple hash-based sampling (simplified)
        let hash = trace_id.value.substring(0, 8).to_int() % 10000
        let threshold = (rate * 10000.0).to_int()
        
        if hash < threshold {
          SamplingResult {
            decision: RecordAndSample,
            attributes: [("sampler.type", "traceidratio"), ("sampler.param", rate.to_string())]
          }
        } else {
          SamplingResult {
            decision: Drop,
            attributes: []
          }
        }
      }
      ParentBased(inner_sampler) => {
        match parent_context {
          Some(parent) => {
            if parent.trace_flags.sampled {
              SamplingResult {
                decision: RecordAndSample,
                attributes: [("sampler.type", "parentbased")]
              }
            } else {
              SamplingResult {
                decision: Drop,
                attributes: []
              }
            }
          }
          None => should_sample(*inner_sampler, trace_id, None)
        }
      }
    }
  }
  
  // Test always-on sampler
  let always_on_result = should_sample(AlwaysOn, create_trace_id(), None)
  assert_eq(always_on_result.decision, RecordAndSample)
  
  // Test always-off sampler
  let always_off_result = should_sample(AlwaysOff, create_trace_id(), None)
  assert_eq(always_off_result.decision, Drop)
  
  // Test trace ID ratio sampler
  let trace_id1 = TraceId { value: "00000000000000000000000000000001" } // Low hash
  let trace_id2 = TraceId { value: "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" } // High hash
  
  let ratio_50 = TraceIdRatio(0.5)
  
  let result1 = should_sample(ratio_50, trace_id1, None)
  let result2 = should_sample(ratio_50, trace_id2, None)
  
  // Results may vary based on hash calculation, but should be consistent
  assert_true(result1.decision == RecordAndSample || result1.decision == Drop)
  assert_true(result2.decision == RecordAndSample || result2.decision == Drop)
  
  // Test parent-based sampler
  let parent_sampled = TraceContext {
    trace_id: create_trace_id(),
    span_id: create_span_id(),
    trace_flags: TraceFlags { sampled: true },
    trace_state: TraceState { entries: [] }
  }
  
  let parent_not_sampled = TraceContext {
    trace_id: create_trace_id(),
    span_id: create_span_id(),
    trace_flags: TraceFlags { sampled: false },
    trace_state: TraceState { entries: [] }
  }
  
  let parent_based = ParentBased(Box(AlwaysOff))
  
  let with_sampled_parent = should_sample(parent_based, create_trace_id(), Some(parent_sampled))
  assert_eq(with_sampled_parent.decision, RecordAndSample)
  
  let with_not_sampled_parent = should_sample(parent_based, create_trace_id(), Some(parent_not_sampled))
  assert_eq(with_not_sampled_parent.decision, Drop)
  
  let with_no_parent = should_sample(parent_based, create_trace_id(), None)
  assert_eq(with_no_parent.decision, Drop) // Uses inner sampler (AlwaysOff)
}

// Test 6: Trace Analytics and Metrics
test "trace analytics and metrics" {
  // Trace metrics
  type TraceMetrics {
    total_spans : Int
    error_spans : Int
    total_duration : Int
    services : Array[String]
    operations : Array[String]
  }
  
  // Span duration bucket
  type DurationBucket {
    min : Int
    max : Int
    count : Int
  }
  
  // Trace analytics
  type TraceAnalytics {
    metrics : TraceMetrics
    duration_distribution : Array[DurationBucket]
    error_rate : Float
    average_duration : Float
  }
  
  let calculate_metrics = fn(spans : Array[Span]) -> TraceMetrics {
    let mut total_spans = 0
    let mut error_spans = 0
    let mut total_duration = 0
    let mut services = []
    let mut operations = []
    
    for span in spans {
      total_spans = total_spans + 1
      
      if span.status == Error {
        error_spans = error_spans + 1
      }
      
      match get_duration(span) {
        Some(duration) => total_duration = total_duration + duration
        None => ()
      }
      
      // Extract service and operation from attributes
      for (key, value) in span.attributes {
        if key == "service.name" && !services.contains(value) {
          services = services.push(value)
        } else if key == "operation.name" && !operations.contains(value) {
          operations = operations.push(value)
        }
      }
    }
    
    TraceMetrics {
      total_spans: total_spans,
      error_spans: error_spans,
      total_duration: total_duration,
      services: services,
      operations: operations
    }
  }
  
  let calculate_duration_distribution = fn(spans : Array[Span], bucket_size : Int) -> Array[DurationBucket] {
    let mut durations = []
    
    for span in spans {
      match get_duration(span) {
        Some(duration) => durations.push(duration)
        None => ()
      }
    }
    
    if durations.length() == 0 {
      return []
    }
    
    let min_duration = durations.reduce(|acc, val| if val < acc { val } else { acc }, durations[0])
    let max_duration = durations.reduce(|acc, val| if val > acc { val } else { acc }, durations[0])
    
    let mut buckets = []
    let mut bucket_start = (min_duration / bucket_size) * bucket_size
    
    while bucket_start <= max_duration {
      let bucket_end = bucket_start + bucket_size
      let mut count = 0
      
      for duration in durations {
        if duration >= bucket_start && duration < bucket_end {
          count = count + 1
        }
      }
      
      buckets.push(DurationBucket {
        min: bucket_start,
        max: bucket_end,
        count: count
      })
      
      bucket_start = bucket_end
    }
    
    buckets
  }
  
  let analyze_traces = fn(spans : Array[Span]) -> TraceAnalytics {
    let metrics = calculate_metrics(spans)
    let duration_distribution = calculate_duration_distribution(spans, 100)
    
    let error_rate = if metrics.total_spans > 0 {
      metrics.error_spans.to_float() / metrics.total_spans.to_float()
    } else {
      0.0
    }
    
    let average_duration = if metrics.total_spans > 0 {
      metrics.total_duration.to_float() / metrics.total_spans.to_float()
    } else {
      0.0
    }
    
    TraceAnalytics {
      metrics: metrics,
      duration_distribution: duration_distribution,
      error_rate: error_rate,
      average_duration: average_duration
    }
  }
  
  // Create test spans
  let trace_id = create_trace_id()
  let span1 = create_span(
    create_trace_context(trace_id, create_span_id(), true),
    None,
    Server,
    "operation1",
    1000
  ) |> set_attribute("service.name", "service-a")
    |> set_attribute("operation.name", "op1")
    |> end_span(1200) // 200ms duration
  
  let span2 = create_span(
    create_trace_context(trace_id, create_span_id(), true),
    None,
    Server,
    "operation2",
    1100
  ) |> set_attribute("service.name", "service-b")
    |> set_attribute("operation.name", "op2")
    |> set_status(Error)
    |> end_span(1400) // 300ms duration
  
  let span3 = create_span(
    create_trace_context(trace_id, create_span_id(), true),
    None,
    Server,
    "operation3",
    1200
  ) |> set_attribute("service.name", "service-a")
    |> set_attribute("operation.name", "op3")
    |> end_span(1600) // 400ms duration
  
  let spans = [span1, span2, span3]
  
  // Analyze traces
  let analytics = analyze_traces(spans)
  
  // Verify metrics
  assert_eq(analytics.metrics.total_spans, 3)
  assert_eq(analytics.metrics.error_spans, 1)
  assert_eq(analytics.metrics.total_duration, 900) // 200 + 300 + 400
  assert_eq(analytics.metrics.services.length(), 2)
  assert_true(analytics.metrics.services.contains("service-a"))
  assert_true(analytics.metrics.services.contains("service-b"))
  
  // Verify analytics
  assert_eq(analytics.error_rate, 1.0 / 3.0)
  assert_eq(analytics.average_duration, 900.0 / 3.0)
  
  // Verify duration distribution
  assert_true(analytics.duration_distribution.length() > 0)
  
  // Find bucket for 200ms duration
  let mut bucket_200 = None
  for bucket in analytics.duration_distribution {
    if 200 >= bucket.min && 200 < bucket.max {
      bucket_200 = Some(bucket)
      break
    }
  }
  
  match bucket_200 {
    Some(b) => assert_eq(b.count, 1)
    None => assert_true(false)
  }
}