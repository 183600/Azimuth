// Azimuth Distributed Tracing Consistency Test Suite
// This file contains high-quality test cases for distributed tracing consistency

// Test 1: Trace Context Propagation Across Services
test "trace context propagation across services" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Map[String, String]
  }
  
  // Define service call
  type ServiceCall = {
    service_name: String,
    operation: String,
    trace_context: TraceContext,
    timestamp: Int
  }
  
  // Create trace context
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String>) {
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_span_id,
      trace_flags: 1,
      trace_state: []
    }
  }
  
  // Extract trace context from headers
  let extract_from_headers = fn(headers: Map[String, String]) {
    let trace_id = match headers.find(fn(pair) { pair.0 == "traceparent" }) {
      Some((_, value)) => {
        let parts = value.split("-")
        if parts.length() >= 2 {
          parts[1]
        } else {
          ""
        }
      }
      None => ""
    }
    
    let span_id = match headers.find(fn(pair) { pair.0 == "traceparent" }) {
      Some((_, value)) => {
        let parts = value.split("-")
        if parts.length() >= 3 {
          parts[2]
        } else {
          ""
        }
      }
      None => ""
    }
    
    if trace_id != "" and span_id != "" {
      Some(create_trace_context(trace_id, span_id, None))
    } else {
      None
    }
  }
  
  // Inject trace context into headers
  let inject_to_headers = fn(context: TraceContext, headers: Map[String, String]) {
    let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string()
    
    let updated_headers = headers
      .filter(fn(pair) { pair.0 != "traceparent" })
      .push(("traceparent", traceparent))
    
    updated_headers
  }
  
  // Create child span context
  let create_child_context = fn(parent: TraceContext, span_id: String) {
    create_trace_context(parent.trace_id, span_id, Some(parent.span_id))
  }
  
  // Test trace context propagation
  let root_context = create_trace_context("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", None)
  
  // Service A receives request (no existing context)
  let service_a_headers = []
  let extracted_a = extract_from_headers(service_a_headers)
  assert_eq(extracted_a, None)
  
  // Service A creates root span
  let service_a_context = root_context
  
  // Service A calls Service B
  let headers_a_to_b = inject_to_headers(service_a_context, [])
  let extracted_b = extract_from_headers(headers_a_to_b)
  
  match extracted_b {
    Some(context) => {
      assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(context.span_id, "b7ad6b7169203331")
    }
    None => assert_true(false)
  }
  
  // Service B creates child span
  let service_b_context = create_child_context(service_a_context, "c7ad6b7169203332")
  assert_eq(service_b_context.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(service_b_context.span_id, "c7ad6b7169203332")
  assert_eq(service_b_context.parent_span_id, Some("b7ad6b7169203331"))
  
  // Service B calls Service C
  let headers_b_to_c = inject_to_headers(service_b_context, [])
  let extracted_c = extract_from_headers(headers_b_to_c)
  
  match extracted_c {
    Some(context) => {
      assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(context.span_id, "c7ad6b7169203332")
    }
    None => assert_true(false)
  }
  
  // Service C creates child span
  let service_c_context = create_child_context(service_b_context, "d7ad6b7169203333")
  assert_eq(service_c_context.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(service_c_context.span_id, "d7ad6b7169203333")
  assert_eq(service_c_context.parent_span_id, Some("c7ad6b7169203332"))
  
  // Verify trace ID consistency across all services
  assert_eq(service_a_context.trace_id, service_b_context.trace_id)
  assert_eq(service_b_context.trace_id, service_c_context.trace_id)
  
  // Verify parent-child relationships
  assert_eq(service_b_context.parent_span_id, Some(service_a_context.span_id))
  assert_eq(service_c_context.parent_span_id, Some(service_b_context.span_id))
}

// Test 2: Trace Consistency in Microservices Architecture
test "trace consistency in microservices architecture" {
  // Define service instance
  type ServiceInstance = {
    service_name: String,
    instance_id: String,
    traces_processed: Array[String]
  }
  
  // Define distributed trace
  type DistributedTrace = {
    trace_id: String,
    spans: Array[ServiceSpan]
  }
  
  // Define service span
  type ServiceSpan = {
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create service instance
  let create_service_instance = fn(service_name: String, instance_id: String) {
    {
      service_name: service_name,
      instance_id: instance_id,
      traces_processed: []
    }
  }
  
  // Create distributed trace
  let create_distributed_trace = fn(trace_id: String) {
    {
      trace_id: trace_id,
      spans: []
    }
  }
  
  // Add span to trace
  let add_span = fn(trace: DistributedTrace, span: ServiceSpan) {
    {
      trace_id: trace.trace_id,
      spans: trace.spans.push(span)
    }
  }
  
  // Process trace in service
  let process_trace = fn(service: ServiceInstance, trace: DistributedTrace) {
    let updated_traces = service.traces_processed.push(trace.trace_id)
    
    {
      service_name: service.service_name,
      instance_id: service.instance_id,
      traces_processed: updated_traces
    }
  }
  
  // Validate trace consistency
  let validate_trace_consistency = fn(trace: DistributedTrace) {
    let mut errors = []
    
    // Check all spans have the same trace ID
    for i in 0..trace.spans.length() {
      if trace.spans[i].span_id != trace.trace_id {
        errors = errors.push("Span " + trace.spans[i].span_id + " has different trace ID")
      }
    }
    
    // Check parent-child relationships
    for i in 0..trace.spans.length() {
      let span = trace.spans[i]
      match span.parent_span_id {
        Some(parent_id) => {
          let parent_exists = trace.spans.some(fn(s) { s.span_id == parent_id })
          if not(parent_exists) {
            errors = errors.push("Parent span " + parent_id + " not found for span " + span.span_id)
          }
        }
        None => {}  // Root span
      }
    }
    
    // Check for cycles
    let mut visited = []
    let mut current = None
    
    // Find root span (no parent)
    let root_spans = trace.spans.filter(fn(span) { span.parent_span_id.is_none() })
    
    if root_spans.length() == 0 {
      errors = errors.push("No root span found")
    }
    
    errors
  }
  
  // Test trace consistency in microservices
  let trace = create_distributed_trace("trace-12345")
  
  // Create service instances
  let api_gateway = create_service_instance("api-gateway", "gateway-1")
  let auth_service = create_service_instance("auth-service", "auth-1")
  let user_service = create_service_instance("user-service", "user-1")
  let order_service = create_service_instance("order-service", "order-1")
  
  // Create spans for each service
  let gateway_span = {
    span_id: "span-1",
    parent_span_id: None,
    service_name: "api-gateway",
    operation_name: "handle-request",
    start_time: 1000,
    end_time: 1050,
    status: "ok"
  }
  
  let auth_span = {
    span_id: "span-2",
    parent_span_id: Some("span-1"),
    service_name: "auth-service",
    operation_name: "authenticate",
    start_time: 1010,
    end_time: 1030,
    status: "ok"
  }
  
  let user_span = {
    span_id: "span-3",
    parent_span_id: Some("span-1"),
    service_name: "user-service",
    operation_name: "get-user",
    start_time: 1035,
    end_time: 1045,
    status: "ok"
  }
  
  let order_span = {
    span_id: "span-4",
    parent_span_id: Some("span-3"),
    service_name: "order-service",
    operation_name: "get-orders",
    start_time: 1040,
    end_time: 1048,
    status: "ok"
  }
  
  // Add spans to trace
  let trace1 = add_span(trace, gateway_span)
  let trace2 = add_span(trace1, auth_span)
  let trace3 = add_span(trace2, user_span)
  let trace4 = add_span(trace3, order_span)
  
  // Process trace in services
  let api_gateway_processed = process_trace(api_gateway, trace4)
  let auth_service_processed = process_trace(auth_service, trace4)
  let user_service_processed = process_trace(user_service, trace4)
  let order_service_processed = process_trace(order_service, trace4)
  
  // Verify all services processed the trace
  assert_true(api_gateway_processed.traces_processed.contains("trace-12345"))
  assert_true(auth_service_processed.traces_processed.contains("trace-12345"))
  assert_true(user_service_processed.traces_processed.contains("trace-12345"))
  assert_true(order_service_processed.traces_processed.contains("trace-12345"))
  
  // Validate trace consistency
  let errors = validate_trace_consistency(trace4)
  assert_eq(errors.length(), 0)
  
  // Test inconsistent trace
  let inconsistent_span = {
    span_id: "span-5",
    parent_span_id: Some("non-existent-span"),
    service_name: "payment-service",
    operation_name: "process-payment",
    start_time: 1050,
    end_time: 1070,
    status: "ok"
  }
  
  let inconsistent_trace = add_span(trace4, inconsistent_span)
  let inconsistent_errors = validate_trace_consistency(inconsistent_trace)
  assert_true(inconsistent_errors.length() > 0)
}

// Test 3: Cross-Service Trace Correlation
test "cross-service trace correlation" {
  // Define trace correlation
  type TraceCorrelation = {
    trace_id: String,
    correlation_id: String,
    user_id: Option[String],
    session_id: Option[String],
    request_id: String
  }
  
  // Define service event
  type ServiceEvent = {
    service_name: String,
    event_type: String,
    correlation: TraceCorrelation,
    timestamp: Int,
    data: Map[String, String]
  }
  
  // Create trace correlation
  let create_trace_correlation = fn(trace_id: String, correlation_id: String, request_id: String) {
    {
      trace_id: trace_id,
      correlation_id: correlation_id,
      user_id: None,
      session_id: None,
      request_id: request_id
    }
  }
  
  // Add user context to correlation
  let add_user_context = fn(correlation: TraceCorrelation, user_id: String, session_id: String) {
    {
      trace_id: correlation.trace_id,
      correlation_id: correlation.correlation_id,
      user_id: Some(user_id),
      session_id: Some(session_id),
      request_id: correlation.request_id
    }
  }
  
  // Create service event
  let create_service_event = fn(service_name: String, event_type: String, correlation: TraceCorrelation, timestamp: Int) {
    {
      service_name: service_name,
      event_type: event_type,
      correlation: correlation,
      timestamp: timestamp,
      data: []
    }
  }
  
  // Correlate events across services
  let correlate_events = fn(events: Array[ServiceEvent]) {
    let mut correlations = []
    
    for i in 0..events.length() {
      let event = events[i]
      let trace_id = event.correlation.trace_id
      let correlation_id = event.correlation.correlation_id
      
      // Find all events with the same trace_id or correlation_id
      let related_events = events.filter(fn(e) { 
        e.correlation.trace_id == trace_id or e.correlation.correlation_id == correlation_id 
      })
      
      if related_events.length() > 1 {
        correlations = correlations.push({
          trace_id: trace_id,
          correlation_id: correlation_id,
          events: related_events,
          services: related_events.map(fn(e) { e.service_name }).unique()
        })
      }
    }
    
    correlations
  }
  
  // Test cross-service trace correlation
  let correlation = create_trace_correlation("trace-67890", "corr-12345", "req-98765")
  let correlation_with_user = add_user_context(correlation, "user-111", "session-222")
  
  // Create events across services
  let event1 = create_service_event("frontend", "page-load", correlation_with_user, 1000)
  let event2 = create_service_event("api-gateway", "request-start", correlation_with_user, 1005)
  let event3 = create_service_event("auth-service", "login", correlation_with_user, 1010)
  let event4 = create_service_event("user-service", "profile-fetch", correlation_with_user, 1020)
  let event5 = create_service_event("order-service", "order-history", correlation_with_user, 1030)
  
  let events = [event1, event2, event3, event4, event5]
  
  // Correlate events
  let correlations = correlate_events(events)
  
  // Should have one correlation group for all events
  assert_eq(correlations.length(), 1)
  assert_eq(correlations[0].trace_id, "trace-67890")
  assert_eq(correlations[0].correlation_id, "corr-12345")
  assert_eq(correlations[0].events.length(), 5)
  
  // Check all services are included
  let services = correlations[0].services
  assert_true(services.contains("frontend"))
  assert_true(services.contains("api-gateway"))
  assert_true(services.contains("auth-service"))
  assert_true(services.contains("user-service"))
  assert_true(services.contains("order-service"))
  
  // Test with multiple traces
  let correlation2 = create_trace_correlation("trace-67891", "corr-12346", "req-98766")
  let event6 = create_service_event("frontend", "page-load", correlation2, 2000)
  let event7 = create_service_event("api-gateway", "request-start", correlation2, 2005)
  
  let mixed_events = events + [event6, event7]
  let mixed_correlations = correlate_events(mixed_events)
  
  // Should have two correlation groups
  assert_eq(mixed_correlations.length(), 2)
  
  // Find correlations for each trace
  let trace1_correlation = mixed_correlations.find(fn(c) { c.trace_id == "trace-67890" })
  let trace2_correlation = mixed_correlations.find(fn(c) { c.trace_id == "trace-67891" })
  
  match trace1_correlation {
    Some(c) => assert_eq(c.events.length(), 5)
    None => assert_true(false)
  }
  
  match trace2_correlation {
    Some(c) => assert_eq(c.events.length(), 2)
    None => assert_true(false)
  }
}

// Test 4: Distributed Sampling Consistency
test "distributed sampling consistency" {
  // Define sampling decision
  enum SamplingDecision {
    Sampled
    NotSampled
  }
  
  // Define sampling context
  type SamplingContext = {
    trace_id: String,
    parent_sampled: Option[SamplingDecision],
    sample_rate: Float,
    decision: Option[SamplingDecision]
  }
  
  // Define sampler
  type Sampler = {
    name: String,
    sample_rate: Float,
    always_sample_parent: Bool
  }
  
  // Create sampler
  let create_sampler = fn(name: String, sample_rate: Float, always_sample_parent: Bool) {
    {
      name: name,
      sample_rate: sample_rate,
      always_sample_parent: always_sample_parent
    }
  }
  
  // Create sampling context
  let create_sampling_context = fn(trace_id: String, parent_sampled: Option[SamplingDecision], sample_rate: Float) {
    {
      trace_id: trace_id,
      parent_sampled: parent_sampled,
      sample_rate: sample_rate,
      decision: None
    }
  }
  
  // Simple hash function for consistent sampling
  let hash_trace_id = fn(trace_id: String) {
    let mut hash = 0
    let chars = trace_id.to_char_array()
    for i in 0..chars.length() {
      hash = (hash * 31 + chars[i].to_int()) % 10000
    }
    if hash < 0 { hash = hash + 10000 }
    hash
  }
  
  // Make sampling decision
  let make_sampling_decision = fn(sampler: Sampler, context: SamplingContext) {
    match context.parent_sampled {
      Some(Sampled) => {
        // Parent is sampled, always sample child
        SamplingDecision::Sampled
      }
      Some(NotSampled) => {
        // Parent is not sampled, don't sample child
        SamplingDecision::NotSampled
      }
      None => {
        // No parent decision, make new decision
        if sampler.always_sample_parent {
          SamplingDecision::Sampled
        } else {
          let hash_value = hash_trace_id(context.trace_id)
          let threshold = (sampler.sample_rate * 10000.0) as Int
          
          if hash_value < threshold {
            SamplingDecision::Sampled
          } else {
            SamplingDecision::NotSampled
          }
        }
      }
    }
  }
  
  // Test distributed sampling consistency
  let sampler = create_sampler("trace-id-ratio", 0.1, false)  // 10% sample rate
  
  // Test root span sampling
  let root_context1 = create_sampling_context("trace-11111", None, 0.1)
  let decision1 = make_sampling_decision(sampler, root_context1)
  
  let root_context2 = create_sampling_context("trace-22222", None, 0.1)
  let decision2 = make_sampling_decision(sampler, root_context2)
  
  let root_context3 = create_sampling_context("trace-33333", None, 0.1)
  let decision3 = make_sampling_decision(sampler, root_context3)
  
  // Test child span sampling with sampled parent
  let child_context1 = create_sampling_context("trace-11111", Some(decision1), 0.1)
  let child_decision1 = make_sampling_decision(sampler, child_context1)
  assert_eq(child_decision1, SamplingDecision::Sampled)  // Should match parent
  
  // Test child span sampling with not sampled parent
  let child_context2 = create_sampling_context("trace-22222", Some(decision2), 0.1)
  let child_decision2 = make_sampling_decision(sampler, child_context2)
  assert_eq(child_decision2, decision2)  // Should match parent
  
  // Test consistent sampling for same trace ID
  let root_context1_again = create_sampling_context("trace-11111", None, 0.1)
  let decision1_again = make_sampling_decision(sampler, root_context1_again)
  assert_eq(decision1, decision1_again)  // Should be consistent
  
  // Test with always_sample_parent sampler
  let always_parent_sampler = create_sampler("always-parent", 0.1, true)
  
  let root_context4 = create_sampling_context("trace-44444", None, 0.1)
  let decision4 = make_sampling_decision(always_parent_sampler, root_context4)
  assert_eq(decision4, SamplingDecision::Sampled)  // Always samples
  
  let child_context4 = create_sampling_context("trace-44444", Some(decision4), 0.1)
  let child_decision4 = make_sampling_decision(always_parent_sampler, child_context4)
  assert_eq(child_decision4, SamplingDecision::Sampled)  // Should match parent
  
  // Test distributed sampling across services
  let services = ["frontend", "api-gateway", "user-service", "order-service"]
  let mut service_decisions = []
  
  // All services make sampling decision for the same trace
  for i in 0..services.length() {
    let service_context = create_sampling_context("trace-55555", None, 0.1)
    let service_decision = make_sampling_decision(sampler, service_context)
    service_decisions = service_decisions.push(service_decision)
  }
  
  // All decisions should be the same
  let first_decision = service_decisions[0]
  for i in 0..service_decisions.length() {
    assert_eq(service_decisions[i], first_decision)
  }
}

// Test 5: Trace State Propagation
test "trace state propagation" {
  // Define trace state entry
  type TraceStateEntry = {
    key: String,
    value: String
  }
  
  // Define trace context with state
  type TraceContextWithState = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_state: Array[TraceStateEntry]
  }
  
  // Create trace context with state
  let create_trace_context_with_state = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_span_id,
      trace_state: []
    }
  }
  
  // Add trace state entry
  let add_trace_state = fn(context: TraceContextWithState, key: String, value: String) {
    let new_entry = { key: key, value: value }
    
    // Remove existing entry with same key
    let filtered_state = context.trace_state.filter(fn(entry) { entry.key != key })
    
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      parent_span_id: context.parent_span_id,
      trace_state: filtered_state.push(new_entry)
    }
  }
  
  // Get trace state value
  let get_trace_state = fn(context: TraceContextWithState, key: String) {
    let entry_opt = context.trace_state.find(fn(entry) { entry.key == key })
    match entry_opt {
      Some(entry) => Some(entry.value)
      None => None
    }
  }
  
  // Serialize trace state to header format
  let serialize_trace_state = fn(context: TraceContextWithState) {
    let mut pairs = []
    for i in 0..context.trace_state.length() {
      let entry = context.trace_state[i]
      pairs = pairs.push(entry.key + "=" + entry.value)
    }
    pairs.join(",")
  }
  
  // Parse trace state from header format
  let parse_trace_state = fn(trace_state_header: String) {
    if trace_state_header == "" {
      []
    } else {
      let pairs = trace_state_header.split(",")
      let mut entries = []
      
      for i in 0..pairs.length() {
        let pair = pairs[i]
        let parts = pair.split("=")
        if parts.length() == 2 {
          entries = entries.push({
            key: parts[0],
            value: parts[1]
          })
        }
      }
      
      entries
    }
  }
  
  // Test trace state propagation
  let context = create_trace_context_with_state("trace-99999", "span-11111", None)
  
  // Add trace state entries
  let context1 = add_trace_state(context, "vendor", "acme")
  let context2 = add_trace_state(context1, "environment", "production")
  let context3 = add_trace_state(context2, "user.id", "12345")
  
  // Test trace state retrieval
  assert_eq(get_trace_state(context3, "vendor"), Some("acme"))
  assert_eq(get_trace_state(context3, "environment"), Some("production"))
  assert_eq(get_trace_state(context3, "user.id"), Some("12345"))
  assert_eq(get_trace_state(context3, "nonexistent"), None)
  
  // Test trace state update
  let context4 = add_trace_state(context3, "environment", "staging")
  assert_eq(get_trace_state(context4, "environment"), Some("staging"))
  assert_eq(context4.trace_state.length(), 3)  // Still 3 entries, not 4
  
  // Test trace state serialization
  let serialized = serialize_trace_state(context4)
  assert_true(serialized.contains("vendor=acme"))
  assert_true(serialized.contains("environment=staging"))
  assert_true(serialized.contains("user.id=12345"))
  
  // Test trace state parsing
  let parsed_entries = parse_trace_state(serialized)
  assert_eq(parsed_entries.length(), 3)
  
  // Create child context and propagate trace state
  let child_context = create_trace_context_with_state("trace-99999", "span-22222", Some("span-11111"))
  
  // Copy trace state from parent
  let mut child_with_state = child_context
  for i in 0..context4.trace_state.length() {
    let entry = context4.trace_state[i]
    child_with_state = add_trace_state(child_with_state, entry.key, entry.value)
  }
  
  // Verify trace state propagation
  assert_eq(get_trace_state(child_with_state, "vendor"), Some("acme"))
  assert_eq(get_trace_state(child_with_state, "environment"), Some("staging"))
  assert_eq(get_trace_state(child_with_state, "user.id"), Some("12345"))
  
  // Child can add its own trace state
  let child_with_additional = add_trace_state(child_with_state, "service", "payment")
  assert_eq(get_trace_state(child_with_additional, "service"), Some("payment"))
  
  // Parent context is unchanged
  assert_eq(get_trace_state(context4, "service"), None)
}

// Test 6: Trace Consistency Validation
test "trace consistency validation" {
  // Define trace validation rule
  type ValidationRule = {
    name: String,
    description: String,
    validate: fn(DistributedTrace) -> Array[String]
  }
  
  // Define distributed trace (reused from earlier test)
  type DistributedTrace = {
    trace_id: String,
    spans: Array[ServiceSpan]
  }
  
  // Define service span (reused from earlier test)
  type ServiceSpan = {
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create validation rules
  let create_validation_rules = fn() {
    [
      {
        name: "single-root-span",
        description: "Trace should have exactly one root span",
        validate: fn(trace: DistributedTrace) {
          let root_spans = trace.spans.filter(fn(span) { span.parent_span_id.is_none() })
          
          if root_spans.length() == 1 {
            []
          } else if root_spans.length() == 0 {
            ["No root span found"]
          } else {
            ["Multiple root spans found: " + root_spans.length().to_string()]
          }
        }
      },
      {
        name: "valid-parent-relationships",
        description: "All parent span references should be valid",
        validate: fn(trace: DistributedTrace) {
          let mut errors = []
          let span_ids = trace.spans.map(fn(span) { span.span_id })
          
          for i in 0..trace.spans.length() {
            let span = trace.spans[i]
            match span.parent_span_id {
              Some(parent_id) => {
                if not(span_ids.contains(parent_id)) {
                  errors = errors.push("Parent span " + parent_id + " not found for span " + span.span_id)
                }
              }
              None => {}
            }
          }
          
          errors
        }
      },
      {
        name: "no-cycles",
        description: "Trace should not contain cycles",
        validate: fn(trace: DistributedTrace) {
          // Simplified cycle detection
          let mut errors = []
          let mut visited = []
          
          // Find root span
          let root_spans = trace.spans.filter(fn(span) { span.parent_span_id.is_none() })
          
          if root_spans.length() == 1 {
            let root = root_spans[0]
            
            // Depth-first traversal to detect cycles
            let mut stack = [root.span_id]
            
            while stack.length() > 0 {
              let current = stack[stack.length() - 1]
              stack = stack.slice(0, stack.length() - 1)
              
              if visited.contains(current) {
                errors = errors.push("Cycle detected at span: " + current)
                break
              }
              
              visited = visited.push(current)
              
              // Find children of current span
              let children = trace.spans.filter(fn(span) { 
                match span.parent_span_id {
                  Some(parent_id) => parent_id == current
                  None => false
                }
              })
              
              for i in 0..children.length() {
                stack = stack.push(children[i].span_id)
              }
            }
          }
          
          errors
        }
      },
      {
        name: "chronological-order",
        description: "Child spans should start after their parent",
        validate: fn(trace: DistributedTrace) {
          let mut errors = []
          
          for i in 0..trace.spans.length() {
            let span = trace.spans[i]
            match span.parent_span_id {
              Some(parent_id) => {
                let parent_opt = trace.spans.find(fn(s) { s.span_id == parent_id })
                match parent_opt {
                  Some(parent) => {
                    if span.start_time < parent.start_time {
                      errors = errors.push("Child span " + span.span_id + " starts before parent " + parent_id)
                    }
                  }
                  None => {}  // Parent not found (handled by another rule)
                }
              }
              None => {}
            }
          }
          
          errors
        }
      }
    ]
  }
  
  // Validate trace against rules
  let validate_trace = fn(trace: DistributedTrace, rules: Array[ValidationRule]) {
    let mut all_errors = []
    
    for i in 0..rules.length() {
      let rule = rules[i]
      let errors = rule.validate(trace)
      
      for j in 0..errors.length() {
        all_errors = all_errors.push({
          rule: rule.name,
          error: errors[j]
        })
      }
    }
    
    all_errors
  }
  
  // Test trace consistency validation
  let rules = create_validation_rules()
  
  // Create a valid trace
  let valid_trace = {
    trace_id: "trace-valid",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: None,
        service_name: "frontend",
        operation_name: "handle-request",
        start_time: 1000,
        end_time: 1050,
        status: "ok"
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        service_name: "api-gateway",
        operation_name: "route-request",
        start_time: 1010,
        end_time: 1030,
        status: "ok"
      },
      {
        span_id: "span-3",
        parent_span_id: Some("span-2"),
        service_name: "user-service",
        operation_name: "get-user",
        start_time: 1020,
        end_time: 1025,
        status: "ok"
      }
    ]
  }
  
  let valid_errors = validate_trace(valid_trace, rules)
  assert_eq(valid_errors.length(), 0)  // No errors for valid trace
  
  // Create an invalid trace (no root span)
  let invalid_trace1 = {
    trace_id: "trace-invalid-1",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: Some("span-0"),  // Non-existent parent
        service_name: "service-1",
        operation_name: "operation-1",
        start_time: 1000,
        end_time: 1050,
        status: "ok"
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        service_name: "service-2",
        operation_name: "operation-2",
        start_time: 1010,
        end_time: 1030,
        status: "ok"
      }
    ]
  }
  
  let invalid_errors1 = validate_trace(invalid_trace1, rules)
  assert_true(invalid_errors1.length() > 0)
  
  // Check for specific errors
  let no_root_error = invalid_errors1.find(fn(error) { error.rule == "single-root-span" })
  match no_root_error {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  let invalid_parent_error = invalid_errors1.find(fn(error) { error.rule == "valid-parent-relationships" })
  match invalid_parent_error {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Create trace with chronological order issue
  let invalid_trace2 = {
    trace_id: "trace-invalid-2",
    spans: [
      {
        span_id: "span-1",
        parent_span_id: None,
        service_name: "frontend",
        operation_name: "handle-request",
        start_time: 1000,
        end_time: 1050,
        status: "ok"
      },
      {
        span_id: "span-2",
        parent_span_id: Some("span-1"),
        service_name: "api-gateway",
        operation_name: "route-request",
        start_time: 900,  // Before parent!
        end_time: 950,
        status: "ok"
      }
    ]
  }
  
  let invalid_errors2 = validate_trace(invalid_trace2, rules)
  assert_true(invalid_errors2.length() > 0)
  
  let chronological_error = invalid_errors2.find(fn(error) { error.rule == "chronological-order" })
  match chronological_error {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
}