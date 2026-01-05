// Azimuth Distributed Tracing Test Suite
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Array[String]
  }
  
  type TraceHeader = {
    key: String,
    value: String
  }
  
  type ServiceCall = {
    call_id: String,
    service_name: String,
    operation: String,
    incoming_context: Option[TraceContext],
    outgoing_contexts: Array[TraceContext],
    timestamp: Int,
    duration_ms: Int
  }
  
  // Create trace contexts
  let trace_contexts = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      baggage: [
        ("user.id", "12345"),
        ("request.id", "req-001"),
        ("session.id", "sess-abc123")
      ],
      flags: ["sampled"]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      baggage: [
        ("user.id", "12345"),
        ("request.id", "req-001"),
        ("session.id", "sess-abc123"),
        ("service.version", "1.2.3")
      ],
      flags: ["sampled"]
    },
    {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-002"),
      baggage: [
        ("user.id", "12345"),
        ("request.id", "req-001"),
        ("session.id", "sess-abc123"),
        ("service.version", "1.2.3"),
        ("cache.hit", "false")
      ],
      flags: ["sampled"]
    },
    {
      trace_id: "trace-002",
      span_id: "span-004",
      parent_span_id: None,
      baggage: [
        ("user.id", "67890"),
        ("request.id", "req-002")
      ],
      flags: ["sampled"]
    }
  ]
  
  // Create service calls
  let service_calls = [
    {
      call_id: "call-001",
      service_name: "frontend",
      operation: "HTTP GET /api/orders",
      incoming_context: None,
      outgoing_contexts: [trace_contexts[1]],
      timestamp: 1640995200,
      duration_ms: 150
    },
    {
      call_id: "call-002",
      service_name: "order-service",
      operation: "Process Order Request",
      incoming_context: Some(trace_contexts[1]),
      outgoing_contexts: [trace_contexts[2]],
      timestamp: 1640995205,
      duration_ms: 200
    },
    {
      call_id: "call-003",
      service_name: "user-service",
      operation: "Get User Details",
      incoming_context: Some(trace_contexts[2]),
      outgoing_contexts: [],
      timestamp: 1640995210,
      duration_ms: 50
    },
    {
      call_id: "call-004",
      service_name: "frontend",
      operation: "HTTP GET /api/products",
      incoming_context: None,
      outgoing_contexts: [trace_contexts[3]],
      timestamp: 1640995300,
      duration_ms: 100
    }
  ]
  
  // Extract trace context from headers
  let extract_context_from_headers = fn(headers: Array[TraceHeader]) {
    let trace_id_header = headers.find(fn(h) { h.key == "trace-id" })
    let span_id_header = headers.find(fn(h) { h.key == "span-id" })
    let parent_span_id_header = headers.find(fn(h) { h.key == "parent-span-id" })
    let baggage_header = headers.find(fn(h) { h.key == "baggage" })
    let flags_header = headers.find(fn(h) { h.key == "trace-flags" })
    
    match trace_id_header {
      Some(trace_id_h) => {
        match span_id_header {
          Some(span_id_h) => {
            let trace_id = trace_id_h.value
            let span_id = span_id_h.value
            let parent_span_id = match parent_span_id_header {
              Some(p) => Some(p.value)
              None => None
            }
            
            let baggage = match baggage_header {
              Some(b) => {
                let baggage_items = b.value.split(",")
                baggage_items.map(fn(item) {
                  let parts = item.split("=")
                  if parts.length() == 2 {
                    (parts[0], parts[1])
                  } else {
                    ("", "")
                  }
                }).filter(fn(pair) { pair.0 != "" })
              }
              None => []
            }
            
            let flags = match flags_header {
              Some(f) => f.value.split(",")
              None => []
            }
            
            Some({
              trace_id,
              span_id,
              parent_span_id,
              baggage,
              flags
            })
          }
          None => None
        }
      }
      None => None
    }
  }
  
  // Inject trace context into headers
  let inject_context_into_headers = fn(context: TraceContext) {
    let mut headers = []
    
    headers = headers.push({ key: "trace-id", value: context.trace_id })
    headers = headers.push({ key: "span-id", value: context.span_id })
    
    match context.parent_span_id {
      Some(parent_id) => {
        headers = headers.push({ key: "parent-span-id", value: parent_id })
      }
      None => {}
    }
    
    if context.baggage.length() > 0 {
      let baggage_string = context.baggage.map(fn(item) {
        item.0 + "=" + item.1
      }).join(",")
      headers = headers.push({ key: "baggage", value: baggage_string })
    }
    
    if context.flags.length() > 0 {
      let flags_string = context.flags.join(",")
      headers = headers.push({ key: "trace-flags", value: flags_string })
    }
    
    headers
  }
  
  // Create child context
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // Add baggage item to context
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    let existing_item = context.baggage.find(fn(item) { item.0 == key })
    
    match existing_item {
      Some(_) => {
        // Update existing item
        {
          context |
          baggage: context.baggage.map(fn(item) {
            if item.0 == key {
              (key, value)
            } else {
              item
            }
          })
        }
      }
      None => {
        // Add new item
        {
          context |
          baggage: context.baggage.push((key, value))
        }
      }
    }
  }
  
  // Test context extraction from headers
  let headers = [
    { key: "trace-id", value: "trace-001" },
    { key: "span-id", value: "span-001" },
    { key: "baggage", value: "user.id=12345,request.id=req-001" },
    { key: "trace-flags", value: "sampled" }
  ]
  
  let extracted_context = extract_context_from_headers(headers)
  assert_true(extracted_context.is_some())
  
  let context = extracted_context.unwrap()
  assert_eq(context.trace_id, "trace-001")
  assert_eq(context.span_id, "span-001")
  assert_eq(context.parent_span_id, None)
  assert_eq(context.baggage.length(), 2)
  assert_true(context.baggage.contains(("user.id", "12345")))
  assert_true(context.baggage.contains(("request.id", "req-001")))
  assert_eq(context.flags.length(), 1)
  assert_eq(context.flags[0], "sampled")
  
  // Test context injection into headers
  let injected_headers = inject_context_into_headers(trace_contexts[0])
  assert_eq(injected_headers.length(), 4)  // trace-id, span-id, baggage, trace-flags
  
  assert_true(injected_headers.contains({ key: "trace-id", value: "trace-001" }))
  assert_true(injected_headers.contains({ key: "span-id", value: "span-001" }))
  assert_true(injected_headers.contains({ key: "baggage", value: "user.id=12345,request.id=req-001,session.id=sess-abc123" }))
  assert_true(injected_headers.contains({ key: "trace-flags", value: "sampled" }))
  
  // Test child context creation
  let child_context = create_child_context(trace_contexts[0], "span-001-child")
  assert_eq(child_context.trace_id, "trace-001")
  assert_eq(child_context.span_id, "span-001-child")
  assert_eq(child_context.parent_span_id, Some("span-001"))
  assert_eq(child_context.baggage.length(), 3)  // Same as parent
  assert_eq(child_context.flags.length(), 1)   // Same as parent
  
  // Test baggage item addition
  let updated_context = add_baggage_item(trace_contexts[0], "new.key", "new.value")
  assert_eq(updated_context.baggage.length(), 4)  // Original 3 + 1 new
  assert_true(updated_context.baggage.contains(("new.key", "new.value")))
  
  // Test baggage item update
  let updated_context2 = add_baggage_item(trace_contexts[0], "user.id", "99999")
  assert_eq(updated_context2.baggage.length(), 3)  // Same count, just updated
  assert_true(updated_context2.baggage.contains(("user.id", "99999")))
  assert_false(updated_context2.baggage.contains(("user.id", "12345")))
  
  // Find trace by ID
  let find_trace_by_id = fn(contexts: Array[TraceContext], trace_id: String) {
    contexts.filter(fn(c) { c.trace_id == trace_id })
  }
  
  let trace001_contexts = find_trace_by_id(trace_contexts, "trace-001")
  assert_eq(trace001_contexts.length(), 3)
  
  let trace002_contexts = find_trace_by_id(trace_contexts, "trace-002")
  assert_eq(trace002_contexts.length(), 1)
  
  // Build trace tree
  let build_trace_tree = fn(contexts: Array[TraceContext]) {
    let root_contexts = contexts.filter(fn(c) { c.parent_span_id.is_none() })
    
    let find_children = fn(parent_id: String) {
      contexts.filter(fn(c) { 
        match c.parent_span_id {
          Some(id) => id == parent_id
          None => false
        }
      })
    }
    
    let build_tree = fn(context: TraceContext) {
      let children = find_children(context.span_id)
      let child_trees = children.map(build_tree)
      (context, child_trees)
    }
    
    root_contexts.map(build_tree)
  }
  
  let trace_trees = build_trace_tree(trace_contexts)
  assert_eq(trace_trees.length(), 2)  // Two trace trees
  
  let trace001_tree = trace_trees.filter(fn(t) { t.0.trace_id == "trace-001" })[0]
  assert_eq(trace001_tree.0.span_id, "span-001")
  assert_eq(trace001_tree.1.length(), 1)  // One child
  
  let trace001_child = trace001_tree.1[0]
  assert_eq(trace001_child.0.span_id, "span-002")
  assert_eq(trace001_child.1.length(), 1)  // One grandchild
  
  let trace001_grandchild = trace001_child.1[0]
  assert_eq(trace001_grandchild.0.span_id, "span-003")
  assert_eq(trace001_grandchild.1.length(), 0)  // No children
  
  // Calculate trace propagation metrics
  let calculate_propagation_metrics = fn(calls: Array[ServiceCall]) {
    let total_calls = calls.length()
    
    let calls_with_incoming_context = calls.filter(fn(c) { c.incoming_context.is_some() }).length()
    let incoming_context_rate = if total_calls > 0 {
      (calls_with_incoming_context.to_float() / total_calls.to_float()) * 100.0
    } else {
      0.0
    }
    
    let calls_with_outgoing_context = calls.filter(fn(c) { c.outgoing_contexts.length() > 0 }).length()
    let outgoing_context_rate = if total_calls > 0 {
      (calls_with_outgoing_context.to_float() / total_calls.to_float()) * 100.0
    } else {
      0.0
    }
    
    let unique_traces = {
      let mut trace_ids = []
      for call in calls {
        let trace_id = match call.incoming_context {
          Some(context) => context.trace_id
          None => {
            if call.outgoing_contexts.length() > 0 {
              call.outgoing_contexts[0].trace_id
            } else {
              ""
            }
          }
        }
        
        if trace_id != "" and not(trace_ids.contains(trace_id)) {
          trace_ids = trace_ids.push(trace_id)
        }
      }
      trace_ids.length()
    }
    
    let total_duration = calls.reduce(fn(acc, c) { acc + c.duration_ms }, 0)
    let avg_duration = if total_calls > 0 {
      total_duration / total_calls
    } else {
      0
    }
    
    {
      total_calls,
      incoming_context_rate,
      outgoing_context_rate,
      unique_traces,
      avg_duration_ms: avg_duration
    }
  }
  
  let propagation_metrics = calculate_propagation_metrics(service_calls)
  assert_eq(propagation_metrics.total_calls, 4)
  assert_eq(propagation_metrics.incoming_context_rate, 50.0)  // 2/4 * 100
  assert_eq(propagation_metrics.outgoing_context_rate, 75.0)  // 3/4 * 100
  assert_eq(propagation_metrics.unique_traces, 2)
  assert_eq(propagation_metrics.avg_duration_ms, 125)  // (150 + 200 + 50 + 100) / 4
}

// Test 2: Span Lifecycle Management
test "span lifecycle management" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    duration_ms: Option[Int],
    status: String,
    tags: Array[(String, String)],
    logs: Array[SpanLog],
    service_name: String
  }
  
  type SpanLog = {
    timestamp: Int,
    level: String,
    message: String,
    fields: Array[(String, String)]
  }
  
  type SpanEvent = {
    event_type: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // Create spans
  let spans = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "HTTP GET /api/orders",
      start_time: 1640995200000,  // Microseconds
      end_time: Some(1640995200150),
      duration_ms: Some(150),
      status: "ok",
      tags: [
        ("http.method", "GET"),
        ("http.url", "/api/orders"),
        ("http.status_code", "200"),
        ("component", "http")
      ],
      logs: [
        {
          timestamp: 1640995200050,
          level: "info",
          message: "Request received",
          fields: [
            ("user.id", "12345"),
            ("request.id", "req-001")
          ]
        },
        {
          timestamp: 1640995200140,
          level: "info",
          message: "Response sent",
          fields: [
            ("response.size", "1024"),
            ("response.time", "150")
          ]
        }
      ],
      service_name: "frontend"
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "Database Query",
      start_time: 1640995200020,
      end_time: Some(1640995200080),
      duration_ms: Some(60),
      status: "ok",
      tags: [
        ("db.type", "postgresql"),
        ("db.statement", "SELECT * FROM orders WHERE user_id = ?"),
        ("db.rows", "5")
      ],
      logs: [
        {
          timestamp: 1640995200025,
          level: "debug",
          message: "Query executed",
          fields: [
            ("query.duration", "55"),
            ("rows.returned", "5")
          ]
        }
      ],
      service_name: "order-service"
    },
    {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "Cache Lookup",
      start_time: 1640995200100,
      end_time: Some(1640995200120),
      duration_ms: Some(20),
      status: "ok",
      tags: [
        ("cache.hit", "false"),
        ("cache.key", "user:12345")
      ],
      logs: [],
      service_name: "user-service"
    },
    {
      trace_id: "trace-002",
      span_id: "span-004",
      parent_span_id: None,
      operation_name "HTTP POST /api/payments",
      start_time: 1640995300000,
      end_time: None,  // Still running
      duration_ms: None,
      status: "running",
      tags: [
        ("http.method", "POST"),
        ("http.url", "/api/payments"),
        ("component", "http")
      ],
      logs: [
        {
          timestamp: 1640995300010,
          level: "info",
          message: "Payment processing started",
          fields: [
            ("payment.id", "pay-001"),
            ("amount", "99.99")
          ]
        }
      ],
      service_name: "payment-service"
    }
  ]
  
  // Start a new span
  let start_span = fn(trace_id: String, span_id: String, parent_span_id: Option[String], 
                     operation_name: String, service_name: String, tags: Array[(String, String)], 
                     start_time: Int) {
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      start_time,
      end_time: None,
      duration_ms: None,
      status: "running",
      tags,
      logs: [],
      service_name
    }
  }
  
  // Finish a span
  let finish_span = fn(span: Span, end_time: Int, status: String) {
    let duration = end_time - span.start_time
    
    {
      span |
      end_time: Some(end_time),
      duration_ms: Some(duration),
      status
    }
  }
  
  // Add a log to a span
  let add_log = fn(span: Span, level: String, message: String, fields: Array[(String, String)], timestamp: Int) {
    let new_log = {
      timestamp,
      level,
      message,
      fields
    }
    
    {
      span |
      logs: span.logs.push(new_log)
    }
  }
  
  // Add a tag to a span
  let add_tag = fn(span: Span, key: String, value: String) {
    let existing_tag = span.tags.find(fn(t) { t.0 == key })
    
    match existing_tag {
      Some(_) => {
        // Update existing tag
        {
          span |
          tags: span.tags.map(fn(t) {
            if t.0 == key {
              (key, value)
            } else {
              t
            }
          })
        }
      }
      None => {
        // Add new tag
        {
          span |
          tags: span.tags.push((key, value))
        }
      }
    }
  }
  
  // Test span creation
  let new_span = start_span(
    "trace-003", 
    "span-005", 
    Some("span-001"), 
    "HTTP POST /api/products", 
    "product-service", 
    [("http.method", "POST")], 
    1640995400000
  )
  
  assert_eq(new_span.trace_id, "trace-003")
  assert_eq(new_span.span_id, "span-005")
  assert_eq(new_span.parent_span_id, Some("span-001"))
  assert_eq(new_span.operation_name, "HTTP POST /api/products")
  assert_eq(new_span.service_name, "product-service")
  assert_eq(new_span.status, "running")
  assert_eq(new_span.end_time, None)
  assert_eq(new_span.duration_ms, None)
  assert_eq(new_span.tags.length(), 1)
  assert_eq(new_span.logs.length(), 0)
  
  // Test span finishing
  let finished_span = finish_span(new_span, 1640995400200, "ok")
  assert_eq(finished_span.end_time, Some(1640995400200))
  assert_eq(finished_span.duration_ms, Some(200))
  assert_eq(finished_span.status, "ok")
  
  // Test adding logs
  let span_with_log = add_log(
    new_span, 
    "info", 
    "Processing request", 
    [("request.id", "req-002")], 
    1640995400010
  )
  
  assert_eq(span_with_log.logs.length(), 1)
  assert_eq(span_with_log.logs[0].level, "info")
  assert_eq(span_with_log.logs[0].message, "Processing request")
  assert_eq(span_with_log.logs[0].fields.length(), 1)
  assert_eq(span_with_log.logs[0].fields[0], ("request.id", "req-002"))
  
  // Test adding tags
  let span_with_tag = add_tag(new_span, "http.status_code", "201")
  assert_eq(span_with_tag.tags.length(), 2)
  assert_true(span_with_tag.tags.contains(("http.method", "POST")))
  assert_true(span_with_tag.tags.contains(("http.status_code", "201")))
  
  // Test updating existing tags
  let span_with_updated_tag = add_tag(span_with_tag, "http.method", "PUT")
  assert_eq(span_with_updated_tag.tags.length(), 2)
  assert_true(span_with_updated_tag.tags.contains(("http.method", "PUT")))
  assert_true(span_with_updated_tag.tags.contains(("http.status_code", "201")))
  assert_false(span_with_updated_tag.tags.contains(("http.method", "POST")))
  
  // Filter spans by status
  let filter_spans_by_status = fn(spans: Array[Span], status: String) {
    spans.filter(fn(s) { s.status == status })
  }
  
  let running_spans = filter_spans_by_status(spans, "running")
  assert_eq(running_spans.length(), 1)
  assert_eq(running_spans[0].span_id, "span-004")
  
  let completed_spans = filter_spans_by_status(spans, "ok")
  assert_eq(completed_spans.length(), 3)
  
  // Filter spans by service
  let filter_spans_by_service = fn(spans: Array[Span], service_name: String) {
    spans.filter(fn(s) { s.service_name == service_name })
  }
  
  let frontend_spans = filter_spans_by_service(spans, "frontend")
  assert_eq(frontend_spans.length(), 1)
  assert_eq(frontend_spans[0].span_id, "span-001")
  
  let order_service_spans = filter_spans_by_service(spans, "order-service")
  assert_eq(order_service_spans.length(), 1)
  assert_eq(order_service_spans[0].span_id, "span-002")
  
  // Calculate span duration statistics
  let calculate_duration_stats = fn(spans: Array[Span]) {
    let completed_spans = spans.filter(fn(s) { s.duration_ms.is_some() })
    let durations = completed_spans.map(fn(s) { s.duration_ms.unwrap_or(0) })
    
    if durations.length() > 0 {
      let total = durations.reduce(fn(acc, d) { acc + d }, 0)
      let avg = total / durations.length()
      
      let sorted = durations.sort(fn(a, b) { a - b })
      let min = sorted[0]
      let max = sorted[durations.length() - 1]
      
      let median = if durations.length() % 2 == 0 {
        (sorted[durations.length() / 2 - 1] + sorted[durations.length() / 2]) / 2
      } else {
        sorted[durations.length() / 2]
      }
      
      {
        count: durations.length(),
        min_duration_ms: min,
        max_duration_ms: max,
        avg_duration_ms: avg,
        median_duration_ms: median
      }
    } else {
      {
        count: 0,
        min_duration_ms: 0,
        max_duration_ms: 0,
        avg_duration_ms: 0,
        median_duration_ms: 0
      }
    }
  }
  
  let duration_stats = calculate_duration_stats(spans)
  assert_eq(duration_stats.count, 3)
  assert_eq(duration_stats.min_duration_ms, 20)
  assert_eq(duration_stats.max_duration_ms, 150)
  assert_eq(duration_stats.avg_duration_ms, 76)  // (150 + 60 + 20) / 3
  assert_eq(duration_stats.median_duration_ms, 60)
  
  // Find spans with specific tags
  let find_spans_with_tag = fn(spans: Array[Span], key: String, value: String) {
    spans.filter(fn(s) { s.tags.contains((key, value)) })
  }
  
  let get_spans = find_spans_with_tag(spans, "http.method", "GET")
  assert_eq(get_spans.length(), 1)
  assert_eq(get_spans[0].span_id, "span-001")
  
  let cache_spans = find_spans_with_tag(spans, "cache.hit", "false")
  assert_eq(cache_spans.length(), 1)
  assert_eq(cache_spans[0].span_id, "span-003")
  
  // Calculate span lifecycle metrics
  let calculate_lifecycle_metrics = fn(spans: Array[Span]) {
    let total_spans = spans.length()
    
    let spans_by_status = [
      ("running", spans.filter(fn(s) { s.status == "running" }).length()),
      ("ok", spans.filter(fn(s) { s.status == "ok" }).length()),
      ("error", spans.filter(fn(s) { s.status == "error" }).length())
    ]
    
    let spans_by_service = {
      let mut services = []
      let mut service_counts = []
      
      for span in spans {
        if not(services.contains(span.service_name)) {
          services = services.push(span.service_name)
          let count = spans.filter(fn(s) { s.service_name == span.service_name }).length()
          service_counts = service_counts.push((span.service_name, count))
        }
      }
      
      service_counts
    }
    
    let total_logs = spans.reduce(fn(acc, s) { acc + s.logs.length() }, 0)
    let total_tags = spans.reduce(fn(acc, s) { acc + s.tags.length() }, 0)
    
    let completed_spans = spans.filter(fn(s) { s.duration_ms.is_some() })
    let completion_rate = if total_spans > 0 {
      (completed_spans.length().to_float() / total_spans.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      total_spans,
      spans_by_status,
      spans_by_service,
      total_logs,
      total_tags,
      completion_rate
    }
  }
  
  let lifecycle_metrics = calculate_lifecycle_metrics(spans)
  assert_eq(lifecycle_metrics.total_spans, 4)
  assert_eq(lifecycle_metrics.spans_by_status.filter(fn(s) { s.0 == "running" })[0].1, 1)
  assert_eq(lifecycle_metrics.spans_by_status.filter(fn(s) { s.0 == "ok" })[0].1, 3)
  assert_eq(lifecycle_metrics.spans_by_service.length(), 4)
  assert_eq(lifecycle_metrics.total_logs, 4)
  assert_eq(lifecycle_metrics.total_tags, 9)
  assert_eq(lifecycle_metrics.completion_rate, 75.0)  // 3/4 * 100
}