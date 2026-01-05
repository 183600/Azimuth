// Azimuth Distributed Tracing Tests
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Creation and Propagation
test "trace context creation and propagation" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    sampled: Bool,
    baggage: Array[(String, String)]
  }
  
  // Create a root trace context
  let create_root_context = fn(trace_id: String) {
    {
      trace_id,
      span_id: "span-" + trace_id,
      parent_span_id: None,
      sampled: true,
      baggage: []
    }
  }
  
  // Create a child context from parent
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id,
      parent_span_id: Some(parent.span_id),
      sampled: parent.sampled,
      baggage: parent.baggage
    }
  }
  
  // Test root context creation
  let root_context = create_root_context("trace-12345")
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-trace-12345")
  assert_eq(root_context.parent_span_id, None)
  assert_true(root_context.sampled)
  assert_eq(root_context.baggage.length(), 0)
  
  // Test child context creation
  let child_context = create_child_context(root_context, "child-span-1")
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "child-span-1")
  assert_eq(child_context.parent_span_id, Some("span-trace-12345"))
  assert_true(child_context.sampled)
  assert_eq(child_context.baggage.length(), 0)
  
  // Test grandchild context creation
  let grandchild_context = create_child_context(child_context, "grandchild-span-1")
  assert_eq(grandchild_context.trace_id, "trace-12345")
  assert_eq(grandchild_context.span_id, "grandchild-span-1")
  assert_eq(grandchild_context.parent_span_id, Some("child-span-1"))
  assert_true(grandchild_context.sampled)
  assert_eq(grandchild_context.baggage.length(), 0)
  
  // Test trace ID consistency across the hierarchy
  assert_eq(root_context.trace_id, child_context.trace_id)
  assert_eq(child_context.trace_id, grandchild_context.trace_id)
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  // Define span structure
  type Span = {
    context: {
      trace_id: String,
      span_id: String,
      parent_span_id: Option[String]
    },
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    events: Array[SpanEvent],
    attributes: Array[(String, String)]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // Create a new span
  let create_span = fn(trace_id: String, span_id: String, operation_name: String, start_time: Int) {
    {
      context: {
        trace_id,
        span_id,
        parent_span_id: None
      },
      operation_name,
      start_time,
      end_time: None,
      status: "running",
      events: [],
      attributes: []
    }
  }
  
  // Start a span
  let start_span = fn(span: Span) {
    { span | status: "running" }
  }
  
  // Finish a span
  let finish_span = fn(span: Span, end_time: Int, status: String) {
    { span | end_time: Some(end_time), status }
  }
  
  // Add event to span
  let add_event = fn(span: Span, event_name: String, timestamp: Int, attributes: Array[(String, String)]) {
    let event = {
      timestamp,
      name: event_name,
      attributes
    }
    { span | events: span.events.push(event) }
  }
  
  // Add attribute to span
  let add_attribute = fn(span: Span, key: String, value: String) {
    { span | attributes: span.attributes.push((key, value)) }
  }
  
  // Test span creation
  let span = create_span("trace-001", "span-001", "http.request", 1640995200)
  assert_eq(span.context.trace_id, "trace-001")
  assert_eq(span.context.span_id, "span-001")
  assert_eq(span.operation_name, "http.request")
  assert_eq(span.start_time, 1640995200)
  assert_eq(span.end_time, None)
  assert_eq(span.status, "running")
  assert_eq(span.events.length(), 0)
  assert_eq(span.attributes.length(), 0)
  
  // Test adding attributes
  let span_with_attrs = span
    |> add_attribute("http.method", "GET")
    |> add_attribute("http.url", "/api/users")
    |> add_attribute("user.id", "12345")
  
  assert_eq(span_with_attrs.attributes.length(), 3)
  assert_true(span_with_attrs.attributes.contains(("http.method", "GET")))
  assert_true(span_with_attrs.attributes.contains(("http.url", "/api/users")))
  assert_true(span_with_attrs.attributes.contains(("user.id", "12345")))
  
  // Test adding events
  let span_with_events = span_with_attrs
    |> add_event("cache.miss", 1640995205, [("cache.key", "user:12345")])
    |> add_event("db.query", 1640995210, [("db.statement", "SELECT * FROM users WHERE id = 12345")])
  
  assert_eq(span_with_events.events.length(), 2)
  assert_eq(span_with_events.events[0].name, "cache.miss")
  assert_eq(span_with_events.events[1].name, "db.query")
  
  // Test finishing span
  let finished_span = finish_span(span_with_events, 1640995220, "success")
  assert_eq(finished_span.end_time, Some(1640995220))
  assert_eq(finished_span.status, "success")
  
  // Calculate span duration
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end_time) => Some(end_time - span.start_time)
      None => None
    }
  }
  
  let duration = calculate_duration(finished_span)
  assert_eq(duration, Some(20))
}

// Test 3: Trace Tree Construction
test "trace tree construction and traversal" {
  // Define span node structure for trace tree
  type SpanNode = {
    span: {
      span_id: String,
      parent_span_id: Option[String],
      operation_name: String,
      service_name: String
    },
    children: Array[SpanNode]
  }
  
  // Create sample spans
  let spans = [
    { span_id: "span-1", parent_span_id: None, operation_name: "http.request", service_name: "api-gateway" },
    { span_id: "span-2", parent_span_id: Some("span-1"), operation_name: "auth.validate", service_name: "auth-service" },
    { span_id: "span-3", parent_span_id: Some("span-1"), operation_name: "user.get", service_name: "user-service" },
    { span_id: "span-4", parent_span_id: Some("span-3"), operation_name: "db.query", service_name: "user-service" },
    { span_id: "span-5", parent_span_id: Some("span-3"), operation_name: "cache.get", service_name: "user-service" },
    { span_id: "span-6", parent_span_id: Some("span-2"), operation_name: "token.validate", service_name: "auth-service" }
  ]
  
  // Build trace tree from spans
  let build_trace_tree = fn(spans: Array[SpanNode]) {
    // Find root spans (those with no parent)
    let roots = spans.filter(fn(s) { s.span.parent_span_id == None })
    
    // Build children recursively
    let build_children = fn(parent_span_id: String) {
      spans.filter(fn(s) { s.span.parent_span_id == Some(parent_span_id) })
        .map(fn(child) { 
          { child | children: build_children(child.span.span_id) } 
        })
    }
    
    roots.map(fn(root) { { root | children: build_children(root.span.span_id) } })
  }
  
  // Convert spans to span nodes
  let span_nodes = spans.map(fn(s) { 
    { span: s, children: [] } 
  })
  
  // Build trace tree
  let trace_trees = build_trace_tree(span_nodes)
  assert_eq(trace_trees.length(), 1)
  
  let root_tree = trace_trees[0]
  assert_eq(root_tree.span.span_id, "span-1")
  assert_eq(root_tree.span.operation_name, "http.request")
  assert_eq(root_tree.children.length(), 2)
  
  // Verify first level children
  let auth_child = root_tree.children.filter(fn(c) { c.span.span_id == "span-2" })[0]
  let user_child = root_tree.children.filter(fn(c) { c.span.span_id == "span-3" })[0]
  
  assert_eq(auth_child.span.operation_name, "auth.validate")
  assert_eq(user_child.span.operation_name, "user.get")
  
  // Verify second level children
  assert_eq(auth_child.children.length(), 1)
  assert_eq(auth_child.children[0].span.span_id, "span-6")
  assert_eq(auth_child.children[0].span.operation_name, "token.validate")
  
  assert_eq(user_child.children.length(), 2)
  let db_child = user_child.children.filter(fn(c) { c.span.span_id == "span-4" })[0]
  let cache_child = user_child.children.filter(fn(c) { c.span.span_id == "span-5" })[0]
  
  assert_eq(db_child.span.operation_name, "db.query")
  assert_eq(cache_child.span.operation_name, "cache.get")
  
  // Count total spans in tree
  let count_spans = fn(node: SpanNode) {
    let mut total = 1
    for child in node.children {
      total = total + count_spans(child)
    }
    total
  }
  
  let total_spans = count_spans(root_tree)
  assert_eq(total_spans, 6)
  
  // Find spans by operation name
  let find_by_operation = fn(node: SpanNode, operation: String) {
    let mut result = []
    
    if node.span.operation_name == operation {
      result = result.push(node.span.span_id)
    }
    
    for child in node.children {
      result = result + find_by_operation(child, operation)
    }
    result
  }
  
  let db_query_spans = find_by_operation(root_tree, "db.query")
  assert_eq(db_query_spans.length(), 1)
  assert_eq(db_query_spans[0], "span-4")
  
  let get_spans = find_by_operation(root_tree, "cache.get")
  assert_eq(get_spans.length(), 1)
  assert_eq(get_spans[0], "span-5")
}

// Test 4: Cross-Service Trace Propagation
test "cross-service trace propagation" {
  // Define trace header structure
  type TraceHeaders = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    sampled: String,
    baggage: Array[(String, String)]
  }
  
  // Define service call structure
  type ServiceCall = {
    from_service: String,
    to_service: String,
    operation: String,
    headers: TraceHeaders
  }
  
  // Inject trace context into headers
  let inject_context = fn(context: {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    sampled: Bool,
    baggage: Array[(String, String)]
  }) {
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      sampled: if context.sampled { "1" } else { "0" },
      baggage: context.baggage
    }
  }
  
  // Extract trace context from headers
  let extract_context = fn(headers: TraceHeaders) {
    {
      trace_id: headers.trace_id,
      span_id: headers.span_id,
      parent_span_id: headers.parent_span_id,
      sampled: headers.sampled == "1",
      baggage: headers.baggage
    }
  }
  
  // Create initial trace context
  let initial_context = {
    trace_id: "trace-789",
    span_id: "span-001",
    parent_span_id: None,
    sampled: true,
    baggage: [("user.id", "12345"), ("request.id", "req-001")]
  }
  
  // Service 1: API Gateway
  let api_gateway_context = initial_context
  let api_gateway_headers = inject_context(api_gateway_context)
  
  let api_gateway_call = {
    from_service: "client",
    to_service: "api-gateway",
    operation: "process_request",
    headers: api_gateway_headers
  }
  
  // Service 2: Auth Service
  let auth_extracted = extract_context(api_gateway_call.headers)
  let auth_context = {
    trace_id: auth_extracted.trace_id,
    span_id: "span-002",
    parent_span_id: Some(auth_extracted.span_id),
    sampled: auth_extracted.sampled,
    baggage: auth_extracted.baggage.push(("auth.method", "jwt"))
  }
  let auth_headers = inject_context(auth_context)
  
  let auth_call = {
    from_service: "api-gateway",
    to_service: "auth-service",
    operation: "validate_token",
    headers: auth_headers
  }
  
  // Service 3: User Service
  let user_extracted = extract_context(api_gateway_call.headers)
  let user_context = {
    trace_id: user_extracted.trace_id,
    span_id: "span-003",
    parent_span_id: Some(user_extracted.span_id),
    sampled: user_extracted.sampled,
    baggage: user_extracted.baggage.push(("user.action", "get_profile"))
  }
  let user_headers = inject_context(user_context)
  
  let user_call = {
    from_service: "api-gateway",
    to_service: "user-service",
    operation: "get_user_profile",
    headers: user_headers
  }
  
  // Verify trace ID consistency across services
  assert_eq(api_gateway_call.headers.trace_id, "trace-789")
  assert_eq(auth_call.headers.trace_id, "trace-789")
  assert_eq(user_call.headers.trace_id, "trace-789")
  
  // Verify parent-child relationships
  assert_eq(auth_call.headers.parent_span_id, Some("span-001"))
  assert_eq(user_call.headers.parent_span_id, Some("span-001"))
  
  // Verify sampling decision propagation
  assert_eq(api_gateway_call.headers.sampled, "1")
  assert_eq(auth_call.headers.sampled, "1")
  assert_eq(user_call.headers.sampled, "1")
  
  // Verify baggage propagation
  assert_true(api_gateway_call.headers.baggage.contains(("user.id", "12345")))
  assert_true(auth_call.headers.baggage.contains(("user.id", "12345")))
  assert_true(user_call.headers.baggage.contains(("user.id", "12345")))
  
  // Verify service-specific baggage additions
  assert_true(auth_call.headers.baggage.contains(("auth.method", "jwt")))
  assert_true(user_call.headers.baggage.contains(("user.action", "get_profile")))
}

// Test 5: Trace Sampling Strategies
test "trace sampling strategies" {
  // Define trace context for sampling
  type TraceContext = {
    trace_id: String,
    operation_name: String,
    service_name: String,
    attributes: Array[(String, String)]
  }
  
  // Constant sampling (always sample)
  let constant_sampling = fn(context: TraceContext, sample: Bool) {
    sample
  }
  
  // Probabilistic sampling
  let probabilistic_sampling = fn(context: TraceContext, sample_rate: Float) {
    // Simple hash-based sampling simulation
    let hash = context.trace_id.length() % 100
    hash.to_float() < (sample_rate * 100.0)
  }
  
  // Operation-based sampling
  let operation_based_sampling = fn(context: TraceContext, operations: Array[String]) {
    operations.contains(context.operation_name)
  }
  
  // Service-based sampling
  let service_based_sampling = fn(context: TraceContext, services: Array[String]) {
    services.contains(context.service_name)
  }
  
  // Attribute-based sampling
  let attribute_based_sampling = fn(context: TraceContext, key: String, value: String) {
    context.attributes.contains((key, value))
  }
  
  // Create test contexts
  let high_priority_context = {
    trace_id: "trace-001",
    operation_name: "payment.process",
    service_name: "payment-service",
    attributes: [("priority", "high"), ("amount", "1000.00")]
  }
  
  let low_priority_context = {
    trace_id: "trace-002",
    operation_name: "health.check",
    service_name: "monitoring-service",
    attributes: [("priority", "low")]
  }
  
  let medium_priority_context = {
    trace_id: "trace-003",
    operation_name: "user.profile",
    service_name: "user-service",
    attributes: [("priority", "medium")]
  }
  
  // Test constant sampling
  assert_true(constant_sampling(high_priority_context, true))
  assert_false(constant_sampling(low_priority_context, false))
  
  // Test probabilistic sampling
  // With 50% sample rate, approximately half of traces should be sampled
  let sampled_50 = probabilistic_sampling(high_priority_context, 0.5)
  let sampled_100 = probabilistic_sampling(high_priority_context, 1.0)
  let sampled_0 = probabilistic_sampling(high_priority_context, 0.0)
  
  assert_true(sampled_100)
  assert_false(sampled_0)
  // sampled_50 could be true or false depending on hash
  
  // Test operation-based sampling
  let critical_operations = ["payment.process", "auth.login", "user.register"]
  assert_true(operation_based_sampling(high_priority_context, critical_operations))
  assert_false(operation_based_sampling(low_priority_context, critical_operations))
  assert_false(operation_based_sampling(medium_priority_context, critical_operations))
  
  // Test service-based sampling
  let critical_services = ["payment-service", "auth-service"]
  assert_true(service_based_sampling(high_priority_context, critical_services))
  assert_false(service_based_sampling(low_priority_context, critical_services))
  assert_false(service_based_sampling(medium_priority_context, critical_services))
  
  // Test attribute-based sampling
  assert_true(attribute_based_sampling(high_priority_context, "priority", "high"))
  assert_false(attribute_based_sampling(high_priority_context, "priority", "low"))
  assert_true(attribute_based_sampling(low_priority_context, "priority", "low"))
  assert_false(attribute_based_sampling(medium_priority_context, "priority", "high"))
  
  // Test composite sampling strategy
  let composite_sampling = fn(context: TraceContext) {
    // Always sample high priority operations
    if attribute_based_sampling(context, "priority", "high") {
      true
    } else if operation_based_sampling(context, ["payment.process"]) {
      true
    } else if service_based_sampling(context, ["auth-service"]) {
      true
    } else {
      // Otherwise use probabilistic sampling with 10% rate
      probabilistic_sampling(context, 0.1)
    }
  }
  
  assert_true(composite_sampling(high_priority_context))
  assert_false(composite_sampling(low_priority_context))
  assert_false(composite_sampling(medium_priority_context))
}

// Test 6: Trace Analysis and Metrics
test "trace analysis and metrics extraction" {
  // Define span structure
  type Span = {
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    service_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create sample trace spans
  let spans = [
    { span_id: "span-1", parent_span_id: None, operation_name: "http.request", service_name: "api-gateway", start_time: 1000, end_time: 1050, status: "success" },
    { span_id: "span-2", parent_span_id: Some("span-1"), operation_name: "auth.validate", service_name: "auth-service", start_time: 1005, end_time: 1025, status: "success" },
    { span_id: "span-3", parent_span_id: Some("span-1"), operation_name: "user.get", service_name: "user-service", start_time: 1010, end_time: 1040, status: "success" },
    { span_id: "span-4", parent_span_id: Some("span-3"), operation_name: "db.query", service_name: "user-service", start_time: 1015, end_time: 1035, status: "success" },
    { span_id: "span-5", parent_span_id: Some("span-3"), operation_name: "cache.get", service_name: "user-service", start_time: 1020, end_time: 1025, status: "success" },
    { span_id: "span-6", parent_span_id: Some("span-2"), operation_name: "token.validate", service_name: "auth-service", start_time: 1010, end_time: 1020, status: "error" }
  ]
  
  // Calculate span duration
  let span_duration = fn(span: Span) {
    span.end_time - span.start_time
  }
  
  // Calculate total trace duration
  let trace_duration = fn(spans: Array[Span]) {
    let start_times = spans.map(fn(s) { s.start_time })
    let end_times = spans.map(fn(s) { s.end_time })
    
    let min_start = start_times.reduce(fn(min, t) { if t < min { t } else { min } }, start_times[0])
    let max_end = end_times.reduce(fn(max, t) { if t > max { t } else { max } }, end_times[0])
    
    max_end - min_start
  }
  
  // Group spans by service
  let group_by_service = fn(spans: Array[Span]) {
    let mut result = []
    let mut processed = []
    
    for span in spans {
      if not(processed.contains(span.service_name)) {
        let service_spans = spans.filter(fn(s) { s.service_name == span.service_name })
        result = result.push((span.service_name, service_spans))
        processed = processed.push(span.service_name)
      }
    }
    result
  }
  
  // Calculate service metrics
  let calculate_service_metrics = fn(service_spans: Array[Span]) {
    let total_spans = service_spans.length()
    let successful_spans = service_spans.filter(fn(s) { s.status == "success" }).length()
    let error_spans = total_spans - successful_spans
    
    let durations = service_spans.map(span_duration)
    let total_duration = durations.reduce(fn(sum, d) { sum + d }, 0)
    let avg_duration = total_duration / total_spans
    
    {
      total_spans,
      successful_spans,
      error_spans,
      success_rate: successful_spans.to_float() / total_spans.to_float(),
      avg_duration,
      total_duration
    }
  }
  
  // Test trace duration calculation
  let duration = trace_duration(spans)
  assert_eq(duration, 50) // 1050 - 1000
  
  // Test individual span durations
  let span_1_duration = span_duration(spans[0])
  let span_2_duration = span_duration(spans[1])
  let span_3_duration = span_duration(spans[2])
  
  assert_eq(span_1_duration, 50)
  assert_eq(span_2_duration, 20)
  assert_eq(span_3_duration, 30)
  
  // Test grouping by service
  let service_groups = group_by_service(spans)
  assert_eq(service_groups.length(), 3)
  
  // Find metrics for each service
  let api_gateway_spans = service_groups.filter(fn(g) { g.0 == "api-gateway" })[0].1
  let auth_service_spans = service_groups.filter(fn(g) { g.0 == "auth-service" })[0].1
  let user_service_spans = service_groups.filter(fn(g) { g.0 == "user-service" })[0].1
  
  assert_eq(api_gateway_spans.length(), 1)
  assert_eq(auth_service_spans.length(), 2)
  assert_eq(user_service_spans.length(), 3)
  
  // Test service metrics calculation
  let api_gateway_metrics = calculate_service_metrics(api_gateway_spans)
  let auth_service_metrics = calculate_service_metrics(auth_service_spans)
  let user_service_metrics = calculate_service_metrics(user_service_spans)
  
  assert_eq(api_gateway_metrics.total_spans, 1)
  assert_eq(api_gateway_metrics.successful_spans, 1)
  assert_eq(api_gateway_metrics.error_spans, 0)
  assert_eq(api_gateway_metrics.success_rate, 1.0)
  assert_eq(api_gateway_metrics.avg_duration, 50)
  
  assert_eq(auth_service_metrics.total_spans, 2)
  assert_eq(auth_service_metrics.successful_spans, 1)
  assert_eq(auth_service_metrics.error_spans, 1)
  assert_eq(auth_service_metrics.success_rate, 0.5)
  assert_eq(auth_service_metrics.avg_duration, 15) // (20 + 10) / 2
  
  assert_eq(user_service_metrics.total_spans, 3)
  assert_eq(user_service_metrics.successful_spans, 3)
  assert_eq(user_service_metrics.error_spans, 0)
  assert_eq(user_service_metrics.success_rate, 1.0)
  assert_eq(user_service_metrics.avg_duration, 10) // (20 + 30 + 5) / 3
  
  // Find slowest spans
  let slowest_spans = spans
    .map(fn(s) { (s, span_duration(s)) })
    .sort(fn(a, b) { a.1 > b.1 })
    .slice(0, 3)
    .map(fn(pair) { pair.0 })
  
  assert_eq(slowest_spans.length(), 3)
  assert_eq(slowest_spans[0].span_id, "span-1") // 50ms
  assert_eq(slowest_spans[1].span_id, "span-3") // 30ms
  assert_eq(slowest_spans[2].span_id, "span-2") // 20ms
  
  // Find error spans
  let error_spans = spans.filter(fn(s) { s.status == "error" })
  assert_eq(error_spans.length(), 1)
  assert_eq(error_spans[0].span_id, "span-6")
  assert_eq(error_spans[0].operation_name, "token.validate")
}