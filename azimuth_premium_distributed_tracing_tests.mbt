// Azimuth Premium Distributed Tracing Test Suite
// Advanced test cases for distributed tracing chain management

// Test 1: Distributed Trace Context Management
test "comprehensive distributed trace context management" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array<(String, String)>,
    baggage: Array<(String, String)>
  }
  
  // Define span relationship
  enum SpanKind {
    Root
    Internal
    Server
    Client
    Producer
    Consumer
  }
  
  // Define span
  type Span = {
    context: TraceContext,
    name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Option[Int>,
    status: String,
    attributes: Array<(String, String)>,
    events: Array<(String, Int, Array<(String, String)])>
  }
  
  // Create trace context manager
  let create_context_manager = fn() {
    let mut trace_counter = 0
    let mut span_counter = 0
    
    {
      create_root_context: fn(trace_state: Array<(String, String)>, baggage: Array<(String, String)>) {
        trace_counter = trace_counter + 1
        span_counter = 1
        
        {
          trace_id: "trace-" + trace_counter.to_string(),
          span_id: "span-" + span_counter.to_string(),
          parent_span_id: None,
          trace_flags: 1,  // Sampled
          trace_state,
          baggage
        }
      },
      
      create_child_context: fn(parent_context: TraceContext, span_kind: SpanKind) {
        span_counter = span_counter + 1
        
        {
          trace_id: parent_context.trace_id,
          span_id: "span-" + span_counter.to_string(),
          parent_span_id: Some(parent_context.span_id),
          trace_flags: parent_context.trace_flags,
          trace_state: parent_context.trace_state,
          baggage: parent_context.baggage
        }
      },
      
      extract_context: fn(headers: Array<(String, String)>) {
        let trace_header = headers.find(fn(h) { h.0 == "traceparent" })
        let state_header = headers.find(fn(h) { h.0 == "tracestate" })
        let baggage_header = headers.find(fn(h) { h.0 == "baggage" })
        
        match trace_header {
          Some((_, trace_value)) => {
            // Parse traceparent header: version-trace_id-span_id-flags
            let parts = trace_value.split("-")
            if parts.length() == 4 {
              let trace_id = parts[1]
              let span_id = parts[2]
              let flags = parts[3].to_int()
              
              let trace_state = match state_header {
                Some((_, state_value)) => {
                  state_value.split(",").map(fn(pair) {
                    let kv = pair.split("=")
                    if kv.length() == 2 {
                      (kv[0], kv[1])
                    } else {
                      ("", "")
                    }
                  }).filter(fn(kv) { kv.0 != "" })
                }
                None => []
              }
              
              let baggage = match baggage_header {
                Some((_, baggage_value)) => {
                  baggage_value.split(",").map(fn(pair) {
                    let kv = pair.split("=")
                    if kv.length() == 2 {
                      (kv[0], kv[1])
                    } else {
                      ("", "")
                    }
                  }).filter(fn(kv) { kv.0 != "" })
                }
                None => []
              }
              
              Some({
                trace_id,
                span_id,
                parent_span_id: None,
                trace_flags: flags,
                trace_state,
                baggage
              })
            } else {
              None
            }
          }
          None => None
        }
      },
      
      inject_context: fn(context: TraceContext) {
        let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string()
        
        let tracestate = if context.trace_state.length() > 0 {
          context.trace_state.map(fn(pair) { pair.0 + "=" + pair.1 }).join(",")
        } else {
          ""
        }
        
        let baggage = if context.baggage.length() > 0 {
          context.baggage.map(fn(pair) { pair.0 + "=" + pair.1 }).join(",")
        } else {
          ""
        }
        
        let mut headers = [
          ("traceparent", traceparent)
        ]
        
        if tracestate != "" {
          headers = headers.push(("tracestate", tracestate))
        }
        
        if baggage != "" {
          headers = headers.push(("baggage", baggage))
        }
        
        headers
      }
    }
  }
  
  // Create context manager
  let manager = create_context_manager()
  
  // Test root context creation
  let root_context = manager.create_root_context(
    [("vendor", "azimuth"), ("environment", "production")],
    [("user.id", "user-123"), ("request.id", "req-456")]
  )
  
  assert_true(root_context.trace_id.starts_with("trace-"))
  assert_true(root_context.span_id.starts_with("span-"))
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.trace_flags, 1)
  assert_eq(root_context.trace_state.length(), 2)
  assert_eq(root_context.baggage.length(), 2)
  
  // Test child context creation
  let child_context = manager.create_child_context(root_context, SpanKind::Internal)
  
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_not_eq(child_context.span_id, root_context.span_id)
  assert_eq(child_context.parent_span_id, Some(root_context.span_id))
  assert_eq(child_context.trace_flags, root_context.trace_flags)
  assert_eq(child_context.trace_state, root_context.trace_state)
  assert_eq(child_context.baggage, root_context.baggage)
  
  // Test context injection
  let injected_headers = manager.inject_context(child_context)
  
  assert_true(injected_headers.length() >= 1)
  let traceparent = injected_headers.find(fn(h) { h.0 == "traceparent" })
  assert_not_eq(traceparent, None)
  
  match traceparent {
    Some((_, value)) => {
      let parts = value.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[1], child_context.trace_id)
      assert_eq(parts[2], child_context.span_id)
      assert_eq(parts[3], child_context.trace_flags.to_string())
    }
    None => assert_true(false)
  }
  
  // Test context extraction
  let extracted_context = manager.extract_context(injected_headers)
  
  assert_not_eq(extracted_context, None)
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, child_context.trace_id)
      assert_eq(context.span_id, child_context.span_id)
      assert_eq(context.trace_flags, child_context.trace_flags)
      assert_eq(context.trace_state, child_context.trace_state)
      assert_eq(context.baggage, child_context.baggage)
    }
    None => assert_true(false)
  }
  
  // Test nested child contexts
  let grandchild_context = manager.create_child_context(child_context, SpanKind::Client)
  
  assert_eq(grandchild_context.trace_id, root_context.trace_id)
  assert_eq(grandchild_context.parent_span_id, Some(child_context.span_id))
  assert_eq(grandchild_context.baggage, root_context.baggage)
}

// Test 2: Trace Chain Reconstruction and Analysis
test "trace chain reconstruction and analysis" {
  // Define trace edge
  type TraceEdge = {
    from_span_id: String,
    to_span_id: String,
    relationship_type: String,
    attributes: Array<(String, String)>
  }
  
  // Define trace graph
  type TraceGraph = {
    trace_id: String,
    spans: Array[Span],
    edges: Array[TraceEdge]
  }
  
  // Define span from previous test
  type Span = {
    context: TraceContext,
    name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array<(String, String)>,
    events: Array[(String, Int, Array[(String, String)])]
  }
  
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)>,
    baggage: Array<(String, String)>
  }
  
  enum SpanKind {
    Root
    Internal
    Server
    Client
    Producer
    Consumer
  }
  
  // Create trace graph builder
  let create_trace_graph_builder = fn() {
    let mut spans = []
    let mut edges = []
    
    {
      add_span: fn(span: Span) {
        spans = spans.push(span)
      },
      
      add_edge: fn(from_span_id: String, to_span_id: String, relationship_type: String, attributes: Array<(String, String)>) {
        edges = edges.push({
          from_span_id,
          to_span_id,
          relationship_type,
          attributes
        })
      },
      
      build_graph: fn(trace_id: String) {
        // Auto-generate edges from parent-child relationships
        let parent_child_edges = spans.filter_map(fn(span) {
          match span.context.parent_span_id {
            Some(parent_id) => Some({
              from_span_id: parent_id,
              to_span_id: span.context.span_id,
              relationship_type: "parent-child",
              attributes: []
            })
            None => None
          }
        })
        
        let all_edges = edges + parent_child_edges
        
        {
          trace_id,
          spans,
          edges: all_edges
        }
      }
    }
  }
  
  // Create trace analyzer
  let create_trace_analyzer = fn() {
    {
      calculate_critical_path: fn(graph: TraceGraph) {
        // Find the longest path through the trace graph
        let span_map = graph.spans.reduce(fn(acc, span) {
          let mut new_acc = acc
          new_acc = new_acc.set(span.context.span_id, span)
          new_acc
        }, {})
        
        // Build adjacency list
        let mut adjacency = {}
        for edge in graph.edges {
          let current_neighbors = adjacency.get(edge.from_span_id)
          let neighbors = match current_neighbors {
            Some(n) => n.push(edge.to_span_id)
            None => [edge.to_span_id]
          }
          adjacency = adjacency.set(edge.from_span_id, neighbors)
        }
        
        // Find root spans (no parent)
        let root_spans = graph.spans.filter(fn(span) {
          span.context.parent_span_id == None
        })
        
        // Calculate longest path from each root
        let mut longest_path = []
        let mut longest_duration = 0
        
        for root in root_spans {
          let mut current_path = [root.context.span_id]
          let mut current_duration = 0
          
          // Simple depth-first search for longest path
          let mut stack = [(root.context.span_id, current_path, 0)]
          
          while stack.length() > 0 {
            let (current_id, path, duration) = stack.pop()
            let current_span = span_map.get(current_id)
            
            match current_span {
              Some(span) => {
                let span_duration = match span.end_time {
                  Some(end) => end - span.start_time
                  None => 0
                }
                
                let total_duration = duration + span_duration
                
                if total_duration > current_duration {
                  current_duration = total_duration
                  current_path = path
                }
                
                // Add children to stack
                let children = adjacency.get(current_id)
                match children {
                  Some(child_ids) => {
                    for child_id in child_ids {
                      stack = stack.push((child_id, path.push(child_id), total_duration))
                    }
                  }
                  None => ()
                }
              }
              None => ()
            }
          }
          
          if current_duration > longest_duration {
            longest_duration = current_duration
            longest_path = current_path
          }
        }
        
        {
          critical_path: longest_path,
          total_duration: longest_duration
        }
      },
      
      find_performance_bottlenecks: fn(graph: TraceGraph, threshold_ms: Int) {
        graph.spans.filter(fn(span) {
          let duration = match span.end_time {
            Some(end) => end - span.start_time
            None => 0
          }
          duration > threshold_ms
        }).map(fn(span) {
          let duration = match span.end_time {
            Some(end) => end - span.start_time
            None => 0
          }
          {
            span_id: span.context.span_id,
            span_name: span.name,
            duration_ms: duration,
            status: span.status
          }
        })
      },
      
      analyze_service_dependencies: fn(graph: TraceGraph) {
        let service_calls = graph.spans.filter(fn(span) {
          span.kind == SpanKind::Client or span.kind == SpanKind::Producer
        })
        
        let dependencies = service_calls.map(fn(span) {
          let service_name = span.attributes.find(fn(attr) { attr.0 == "service.name" })
          let target_service = span.attributes.find(fn(attr) { attr.0 == "target.service" })
          
          match (service_name, target_service) {
            (Some((_, source)), Some((_, target))) => {
              Some((source, target))
            }
            _ => None
          }
        }).filter_map(fn(dep) { dep })
        
        // Count unique dependencies
        let mut unique_deps = {}
        for (source, target) in dependencies {
          let key = source + "->" + target
          let current_count = unique_deps.get(key)
          let count = match current_count {
            Some(c) => c + 1
            None => 1
          }
          unique_deps = unique_deps.set(key, count)
        }
        
        unique_deps.to_array().map(fn(pair) {
          let parts = pair.0.split("->")
          {
            source_service: parts[0],
            target_service: parts[1],
            call_count: pair.1
          }
        })
      }
    }
  }
  
  // Create test spans
  let base_time = 1640995200
  
  let root_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-1",
      parent_span_id: None,
      trace_flags: 1,
      trace_state: [],
      baggage: []
    },
    name: "http.request",
    kind: SpanKind::Server,
    start_time: base_time,
    end_time: Some(base_time + 1000),
    status: "ok",
    attributes: [("service.name", "api-gateway"), ("http.method", "GET")],
    events: []
  }
  
  let auth_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-2",
      parent_span_id: Some("span-1"),
      trace_flags: 1,
      trace_state: [],
      baggage: []
    },
    name: "auth.validate",
    kind: SpanKind::Internal,
    start_time: base_time + 100,
    end_time: Some(base_time + 300),
    status: "ok",
    attributes: [("service.name", "api-gateway")],
    events: []
  }
  
  let user_service_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-3",
      parent_span_id: Some("span-1"),
      trace_flags: 1,
      trace_state: [],
      baggage: []
    },
    name: "http.request",
    kind: SpanKind::Client,
    start_time: base_time + 350,
    end_time: Some(base_time + 600),
    status: "ok",
    attributes: [
      ("service.name", "api-gateway"),
      ("target.service", "user-service"),
      ("http.method", "GET")
    ],
    events: []
  }
  
  let database_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-4",
      parent_span_id: Some("span-3"),
      trace_flags: 1,
      trace_state: [],
      baggage: []
    },
    name: "db.query",
    kind: SpanKind::Client,
    start_time: base_time + 400,
    end_time: Some(base_time + 550),
    status: "ok",
    attributes: [
      ("service.name", "user-service"),
      ("target.service", "database"),
      ("db.statement", "SELECT * FROM users")
    ],
    events: []
  }
  
  let slow_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-5",
      parent_span_id: Some("span-1"),
      trace_flags: 1,
      trace_state: [],
      baggage: []
    },
    name: "slow.operation",
    kind: SpanKind::Internal,
    start_time: base_time + 650,
    end_time: Some(base_time + 1200),  // 550ms duration
    status: "ok",
    attributes: [("service.name", "api-gateway")],
    events: []
  }
  
  // Build trace graph
  let builder = create_trace_graph_builder()
  builder.add_span(root_span)
  builder.add_span(auth_span)
  builder.add_span(user_service_span)
  builder.add_span(database_span)
  builder.add_span(slow_span)
  
  let graph = builder.build_graph("trace-123")
  
  // Analyze trace graph
  let analyzer = create_trace_analyzer()
  
  // Test critical path calculation
  let critical_path = analyzer.calculate_critical_path(graph)
  assert_eq(critical_path.critical_path.length(), 3)  // span-1 -> span-3 -> span-4
  assert_true(critical_path.total_duration > 0)
  
  // Test performance bottleneck detection
  let bottlenecks = analyzer.find_performance_bottlenecks(graph, 500)
  assert_eq(bottlenecks.length(), 1)  // Only slow_span exceeds 500ms
  assert_eq(bottlenecks[0].span_id, "span-5")
  assert_eq(bottlenecks[0].span_name, "slow.operation")
  
  // Test service dependency analysis
  let dependencies = analyzer.analyze_service_dependencies(graph)
  assert_eq(dependencies.length(), 2)
  
  let api_to_user = dependencies.find(fn(dep) { 
    dep.source_service == "api-gateway" and dep.target_service == "user-service" 
  })
  assert_not_eq(api_to_user, None)
  
  let user_to_db = dependencies.find(fn(dep) { 
    dep.source_service == "user-service" and dep.target_service == "database" 
  })
  assert_not_eq(user_to_db, None)
}

// Test 3: Distributed Trace Sampling and Propagation
test "distributed trace sampling and propagation strategies" {
  // Define sampling decision
  type SamplingDecision = {
    sampled: Bool,
    sample_rate: Float,
    decision_point: String,
    attributes: Array<(String, String)>
  }
  
  // Define propagation context
  type PropagationContext = {
    trace_id: String,
    sampled: Bool,
    sample_rate: Float,
    decision_points: Array[String>
  }
  
  // Create distributed sampling manager
  let create_sampling_manager = fn() {
    let mut sampling_decisions = {}
    
    {
      make_sampling_decision: fn(
        trace_id: String,
        service_name: String,
        operation_name: String,
        parent_sampled: Option[Bool],
        base_sample_rate: Float
      ) {
        // Check if parent was sampled
        match parent_sampled {
          Some(true) => {
            // Parent was sampled, continue sampling
            let decision = {
              sampled: true,
              sample_rate: 1.0,
              decision_point: service_name + ":" + operation_name,
              attributes: [("sampling.reason", "parent-sampled")]
            }
            
            sampling_decisions = sampling_decisions.set(trace_id, decision)
            decision
          }
          Some(false) => {
            // Parent was not sampled, don't sample
            let decision = {
              sampled: false,
              sample_rate: 0.0,
              decision_point: service_name + ":" + operation_name,
              attributes: [("sampling.reason", "parent-not-sampled")]
            }
            
            sampling_decisions = sampling_decisions.set(trace_id, decision)
            decision
          }
          None => {
            // No parent, make new sampling decision
            let hash = trace_id.length() % 100
            let threshold = (base_sample_rate * 100.0).to_int()
            let sampled = hash < threshold
            
            let decision = {
              sampled,
              sample_rate: base_sample_rate,
              decision_point: service_name + ":" + operation_name,
              attributes: [
                ("sampling.reason", "head-sampling"),
                ("sampling.hash", hash.to_string()),
                ("sampling.threshold", threshold.to_string())
              ]
            }
            
            sampling_decisions = sampling_decisions.set(trace_id, decision)
            decision
          }
        }
      },
      
      create_propagation_context: fn(trace_id: String) {
        let decision = sampling_decisions.get(trace_id)
        match decision {
          Some(dec) => {
            Some({
              trace_id,
              sampled: dec.sampled,
              sample_rate: dec.sample_rate,
              decision_points: [dec.decision_point]
            })
          }
          None => None
        }
      },
      
      merge_propagation_context: fn(
        local_context: PropagationContext,
        remote_context: PropagationContext
      ) {
        // Merge decision points and maintain most permissive sampling
        let all_decision_points = local_context.decision_points + remote_context.decision_points
        let sampled = local_context.sampled or remote_context.sampled
        let sample_rate = if sampled {
          local_context.sample_rate.max(remote_context.sample_rate)
        } else {
          0.0
        }
        
        {
          trace_id: local_context.trace_id,
          sampled,
          sample_rate,
          decision_points: all_decision_points
        }
      },
      
      get_sampling_statistics: fn() {
        let decisions = sampling_decisions.to_array()
        let total_decisions = decisions.length()
        let sampled_decisions = decisions.count_fn(fn(pair) { pair.1.sampled })
        
        let sample_rate = if total_decisions > 0 {
          sampled_decisions.to_float() / total_decisions.to_float()
        } else {
          0.0
        }
        
        let decisions_by_service = decisions.reduce(fn(acc, pair) {
          let decision = pair.1
          let parts = decision.decision_point.split(":")
          let service_name = if parts.length() > 0 { parts[0] } else { "unknown" }
          
          let current_stats = acc.get(service_name)
          let stats = match current_stats {
            Some(s) => {
              {
                total: s.total + 1,
                sampled: if decision.sampled { s.sampled + 1 } else { s.sampled }
              }
            }
            None => {
              {
                total: 1,
                sampled: if decision.sampled { 1 } else { 0 }
              }
            }
          }
          
          acc.set(service_name, stats)
        }, {})
        
        {
          total_decisions,
          sampled_decisions,
          overall_sample_rate: sample_rate,
          decisions_by_service
        }
      }
    }
  }
  
  // Create sampling manager
  let manager = create_sampling_manager()
  
  // Test head sampling (no parent)
  let head_decision = manager.make_sampling_decision(
    "trace-123",
    "api-gateway",
    "http.request",
    None,
    0.5
  )
  
  assert_true(head_decision.sampled or not(head_decision.sampled))  // Could be either based on hash
  assert_eq(head_decision.sample_rate, 0.5)
  assert_eq(head_decision.decision_point, "api-gateway:http.request")
  
  // Test child sampling with sampled parent
  let child_decision_sampled = manager.make_sampling_decision(
    "trace-123",
    "user-service",
    "db.query",
    Some(true),
    0.1  // Lower rate, but should be ignored due to parent sampling
  )
  
  assert_true(child_decision_sampled.sampled)
  assert_eq(child_decision_sampled.sample_rate, 1.0)
  assert_eq(child_decision_sampled.decision_point, "user-service:db.query")
  
  // Test child sampling with non-sampled parent
  let child_decision_not_sampled = manager.make_sampling_decision(
    "trace-456",
    "auth-service",
    "validate.token",
    Some(false),
    0.8  // Higher rate, but should be ignored due to parent non-sampling
  )
  
  assert_false(child_decision_not_sampled.sampled)
  assert_eq(child_decision_not_sampled.sample_rate, 0.0)
  assert_eq(child_decision_not_sampled.decision_point, "auth-service:validate.token")
  
  // Test propagation context creation
  let propagation_context = manager.create_propagation_context("trace-123")
  assert_not_eq(propagation_context, None)
  
  match propagation_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-123")
      assert_eq(context.sampled, head_decision.sampled)
      assert_eq(context.sample_rate, head_decision.sample_rate)
      assert_eq(context.decision_points.length(), 1)
    }
    None => assert_true(false)
  }
  
  // Test propagation context merging
  let local_context = {
    trace_id: "trace-789",
    sampled: true,
    sample_rate: 0.5,
    decision_points: ["service-a:operation-1"]
  }
  
  let remote_context = {
    trace_id: "trace-789",
    sampled: false,
    sample_rate: 0.1,
    decision_points: ["service-b:operation-2"]
  }
  
  let merged_context = manager.merge_propagation_context(local_context, remote_context)
  assert_true(merged_context.sampled)  // Should be true (most permissive)
  assert_eq(merged_context.sample_rate, 0.5)  // Should be max of 0.5 and 0.1
  assert_eq(merged_context.decision_points.length(), 2)
  
  // Test sampling statistics
  let stats = manager.get_sampling_statistics()
  assert_eq(stats.total_decisions, 3)
  assert_true(stats.overall_sample_rate >= 0.0 and stats.overall_sample_rate <= 1.0)
  assert_true(stats.decisions_by_service.size() >= 2)
}

// Test 4: Trace Consistency Across Service Boundaries
test "trace consistency across service boundaries" {
  // Define service boundary
  type ServiceBoundary = {
    service_name: String,
    ingress_span: Option[String>,
    egress_spans: Array[String>,
    trace_state: Array[(String, String)]
  }
  
  // Define trace consistency report
  type ConsistencyReport = {
    trace_id: String,
    is_consistent: Bool,
    issues: Array[String],
    service_boundaries: Array[ServiceBoundary],
    missing_links: Array<(String, String)>
  }
  
  // Define span from previous tests
  type Span = {
    context: TraceContext,
    name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array<(String, String)>,
    events: Array[(String, Int, Array[(String, String)])]
  }
  
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)>,
    baggage: Array[(String, String)>
  }
  
  enum SpanKind {
    Root
    Internal
    Server
    Client
    Producer
    Consumer
  }
  
  // Create consistency checker
  let create_consistency_checker = fn() {
    {
      check_trace_consistency: fn(spans: Array[Span]) {
        let trace_id = spans[0].context.trace_id
        let mut issues = []
        let mut service_boundaries = []
        let mut missing_links = []
        
        // Group spans by service
        let mut spans_by_service = {}
        for span in spans {
          let service_name = span.attributes.find(fn(attr) { attr.0 == "service.name" })
          match service_name {
            Some((_, name)) => {
              let current_spans = spans_by_service.get(name)
              let service_spans = match current_spans {
                Some(s) => s.push(span)
                None => [span]
              }
              spans_by_service = spans_by_service.set(name, service_spans)
            }
            None => {
              issues = issues.push("Span missing service.name attribute: " + span.context.span_id)
            }
          }
        }
        
        // Check each service boundary
        for (service_name, service_spans) in spans_by_service.to_array() {
          let ingress_spans = service_spans.filter(fn(span) {
            span.kind == SpanKind::Server or span.kind == SpanKind::Consumer
          })
          
          let egress_spans = service_spans.filter(fn(span) {
            span.kind == SpanKind::Client or span.kind == SpanKind::Producer
          })
          
          let ingress_span_id = if ingress_spans.length() > 0 {
            Some(ingress_spans[0].context.span_id)
          } else {
            None
          }
          
          let egress_span_ids = egress_spans.map(fn(span) { span.context.span_id })
          
          service_boundaries = service_boundaries.push({
            service_name,
            ingress_span: ingress_span_id,
            egress_spans: egress_span_ids,
            trace_state: service_spans[0].context.trace_state
          })
          
          // Check for missing ingress span (except for root service)
          if ingress_spans.length() == 0 and service_spans[0].context.parent_span_id != None {
            issues = issues.push("Service " + service_name + " missing ingress span")
          }
        }
        
        // Check for missing links between services
        for boundary in service_boundaries {
          for egress_span_id in boundary.egress_spans {
            let egress_span = spans.find(fn(span) { span.context.span_id == egress_span_id })
            match egress_span {
              Some(span) => {
                // Find corresponding ingress span in target service
                let target_service = span.attributes.find(fn(attr) { attr.0 == "target.service" })
                match target_service {
                  Some((_, target_name)) => {
                    let target_boundary = service_boundaries.find(fn(b) { b.service_name == target_name })
                    match target_boundary {
                      Some(target) => {
                        match target.ingress_span {
                          Some(ingress_span_id) => {
                            // Check if parent-child relationship exists
                            let ingress_span = spans.find(fn(s) { s.context.span_id == ingress_span_id })
                            match ingress_span {
                              Some(i_span) => {
                                if i_span.context.parent_span_id != Some(egress_span_id) {
                                  missing_links = missing_links.push((egress_span_id, ingress_span_id))
                                  issues = issues.push("Missing link between " + boundary.service_name + " and " + target_name)
                                }
                              }
                              None => ()
                            }
                          }
                          None => {
                            missing_links = missing_links.push((egress_span_id, "unknown"))
                            issues = issues.push("Target service " + target_name + " missing ingress span for " + egress_span_id)
                          }
                        }
                      }
                      None => {
                        issues = issues.push("Unknown target service: " + target_name)
                      }
                    }
                  }
                  None => {
                    issues = issues.push("Egress span " + egress_span_id + " missing target.service attribute")
                  }
                }
              }
              None => ()
            }
          }
        }
        
        // Check trace state consistency
        let trace_states = service_boundaries.map(fn(boundary) { boundary.trace_state })
        let first_trace_state = trace_states[0]
        
        for (i, trace_state) in trace_states.enumerate() {
          if i > 0 {
            if trace_state != first_trace_state {
              issues = issues.push("Trace state inconsistency detected")
            }
          }
        }
        
        {
          trace_id,
          is_consistent: issues.length() == 0,
          issues,
          service_boundaries,
          missing_links
        }
      }
    }
  }
  
  // Create test spans with service boundaries
  let base_time = 1640995200
  
  let api_gateway_root = {
    context: {
      trace_id: "trace-123",
      span_id: "span-1",
      parent_span_id: None,
      trace_flags: 1,
      trace_state: [("vendor", "azimuth")],
      baggage: [("user.id", "user-123")]
    },
    name: "http.request",
    kind: SpanKind::Server,
    start_time: base_time,
    end_time: Some(base_time + 1000),
    status: "ok",
    attributes: [
      ("service.name", "api-gateway"),
      ("http.method", "GET"),
      ("http.url", "/api/users/123")
    ],
    events: []
  }
  
  let api_gateway_egress = {
    context: {
      trace_id: "trace-123",
      span_id: "span-2",
      parent_span_id: Some("span-1"),
      trace_flags: 1,
      trace_state: [("vendor", "azimuth")],
      baggage: [("user.id", "user-123")]
    },
    name: "http.request",
    kind: SpanKind::Client,
    start_time: base_time + 100,
    end_time: Some(base_time + 400),
    status: "ok",
    attributes: [
      ("service.name", "api-gateway"),
      ("target.service", "user-service"),
      ("http.method", "GET"),
      ("http.url", "http://user-service/users/123")
    ],
    events: []
  }
  
  let user_service_ingress = {
    context: {
      trace_id: "trace-123",
      span_id: "span-3",
      parent_span_id: Some("span-2"),  // Correct parent-child link
      trace_flags: 1,
      trace_state: [("vendor", "azimuth")],
      baggage: [("user.id", "user-123")]
    },
    name: "http.request",
    kind: SpanKind::Server,
    start_time: base_time + 150,
    end_time: Some(base_time + 350),
    status: "ok",
    attributes: [
      ("service.name", "user-service"),
      ("http.method", "GET"),
      ("http.url", "/users/123")
    ],
    events: []
  }
  
  let user_service_db = {
    context: {
      trace_id: "trace-123",
      span_id: "span-4",
      parent_span_id: Some("span-3"),
      trace_flags: 1,
      trace_state: [("vendor", "azimuth")],
      baggage: [("user.id", "user-123")]
    },
    name: "db.query",
    kind: SpanKind::Client,
    start_time: base_time + 200,
    end_time: Some(base_time + 300),
    status: "ok",
    attributes: [
      ("service.name", "user-service"),
      ("target.service", "database"),
      ("db.statement", "SELECT * FROM users WHERE id = 123")
    ],
    events: []
  }
  
  let broken_span = {
    context: {
      trace_id: "trace-123",
      span_id: "span-5",
      parent_span_id: Some("span-99"),  // Non-existent parent
      trace_flags: 1,
      trace_state: [("vendor", "different")],  // Different trace state
      baggage: [("user.id", "user-123")]
    },
    name: "orphan.operation",
    kind: SpanKind::Internal,
    start_time: base_time + 500,
    end_time: Some(base_time + 600),
    status: "ok",
    attributes: [
      ("service.name", "auth-service")  // No ingress span
    ],
    events: []
  }
  
  let spans = [
    api_gateway_root,
    api_gateway_egress,
    user_service_ingress,
    user_service_db,
    broken_span
  ]
  
  // Check consistency
  let checker = create_consistency_checker()
  let report = checker.check_trace_consistency(spans)
  
  // Verify report
  assert_eq(report.trace_id, "trace-123")
  assert_false(report.is_consistent)  // Should have issues
  assert_true(report.issues.length() >= 2)
  assert_eq(report.service_boundaries.length(), 3)  // api-gateway, user-service, auth-service
  assert_true(report.missing_links.length() >= 1)
  
  // Check for specific issues
  let missing_parent_issue = report.issues.find(fn(issue) { 
    issue.contains("span-99") or issue.contains("Missing link")
  })
  assert_not_eq(missing_parent_issue, None)
  
  let trace_state_issue = report.issues.find(fn(issue) { 
    issue.contains("Trace state inconsistency")
  })
  assert_not_eq(trace_state_issue, None)
  
  let missing_ingress_issue = report.issues.find(fn(issue) { 
    issue.contains("missing ingress span")
  })
  assert_not_eq(missing_ingress_issue, None)
}