// Azimuth Distributed Tracing Consistency Comprehensive Tests
// This file contains comprehensive test cases for distributed tracing consistency across services

// Test 1: Trace Context Propagation
test "trace context propagation across service boundaries" {
  // Define trace context structure
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array<(String, String)>
  }
  
  // Define service span structure
  type ServiceSpan = {
    service_name: String,
    operation_name: String,
    context: TraceContext,
    start_time: Int,
    end_time: Int,
    status: String
  }
  
  // Create initial trace context
  let initial_context = {
    trace_id: "trace-abc123def456",
    span_id: "span-root789012",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: [("sampling.decision", "true"), ("service.priority", "high")]
  }
  
  // Simulate trace propagation through services
  let propagate_trace = fn(context: TraceContext, service_name: String, operation_name: String, parent_span: String) {
    let new_span_id = "span-" + service_name + "-" + operation_name + "-" + "123"
    {
      trace_id: context.trace_id,
      span_id: new_span_id,
      parent_span_id: Some(parent_span),
      trace_flags: context.trace_flags,
      trace_state: context.trace_state
    }
  }
  
  // Create service spans
  let gateway_span = {
    service_name: "api-gateway",
    operation_name: "authenticate_request",
    context: initial_context,
    start_time: 1640995200,
    end_time: 1640995210,
    status: "success"
  }
  
  let auth_context = propagate_trace(gateway_span.context, "auth-service", "validate_token", gateway_span.context.span_id)
  let auth_span = {
    service_name: "auth-service",
    operation_name: "validate_token",
    context: auth_context,
    start_time: 1640995211,
    end_time: 1640995215,
    status: "success"
  }
  
  let user_context = propagate_trace(auth_span.context, "user-service", "get_profile", auth_span.context.span_id)
  let user_span = {
    service_name: "user-service",
    operation_name: "get_profile",
    context: user_context,
    start_time: 1640995216,
    end_time: 1640995220,
    status: "success"
  }
  
  // Verify trace consistency
  assert_eq(gateway_span.context.trace_id, auth_span.context.trace_id)
  assert_eq(auth_span.context.trace_id, user_span.context.trace_id)
  
  // Verify parent-child relationships
  assert_eq(auth_span.context.parent_span_id, Some(gateway_span.context.span_id))
  assert_eq(user_span.context.parent_span_id, Some(auth_span.context.span_id))
  
  // Verify trace state propagation
  assert_eq(gateway_span.context.trace_state, auth_span.context.trace_state)
  assert_eq(auth_span.context.trace_state, user_span.context.trace_state)
  
  // Verify trace flags consistency
  assert_eq(gateway_span.context.trace_flags, auth_span.context.trace_flags)
  assert_eq(auth_span.context.trace_flags, user_span.context.trace_flags)
  
  // Verify span uniqueness
  assert_not_eq(gateway_span.context.span_id, auth_span.context.span_id)
  assert_not_eq(auth_span.context.span_id, user_span.context.span_id)
  assert_not_eq(gateway_span.context.span_id, user_span.context.span_id)
}

// Test 2: Cross-Service Trace Consistency
test "cross-service trace consistency validation" {
  // Define trace tree structure
  type TraceNode = {
    span_id: String,
    service_name: String,
    operation: String,
    children: Array[TraceNode],
    start_time: Int,
    end_time: Int
  }
  
  // Build a trace tree for a complex request flow
  let trace_tree = {
    span_id: "root-span-001",
    service_name: "api-gateway",
    operation: "process_request",
    children: [
      {
        span_id: "auth-span-001",
        service_name: "auth-service",
        operation: "authenticate",
        children: [
          {
            span_id: "db-span-001",
            service_name: "database",
            operation: "query_user",
            children: [],
            start_time: 1640995211,
            end_time: 1640995213
          }
        ],
        start_time: 1640995210,
        end_time: 1640995215
      },
      {
        span_id: "business-span-001",
        service_name: "business-service",
        operation: "process_order",
        children: [
          {
            span_id: "inventory-span-001",
            service_name: "inventory-service",
            operation: "check_stock",
            children: [],
            start_time: 1640995216,
            end_time: 1640995218
          },
          {
            span_id: "payment-span-001",
            service_name: "payment-service",
            operation: "process_payment",
            children: [
              {
                span_id: "payment-db-span-001",
                service_name: "database",
                operation: "update_payment",
                children: [],
                start_time: 1640995219,
                end_time: 1640995221
              }
            ],
            start_time: 1640995218,
            end_time: 1640995222
          }
        ],
        start_time: 1640995215,
        end_time: 1640995225
      }
    ],
    start_time: 1640995200,
    end_time: 1640995230
  }
  
  // Validate trace tree consistency
  let validate_trace_tree = fn(node: TraceNode, trace_id: String) -> Bool {
    // Check span ID format
    if not(node.span_id.starts_with("span-") and node.span_id.length() > 5) {
      false
    } else if node.start_time > node.end_time {
      false
    } else {
      // Recursively validate children
      let mut all_children_valid = true
      for child in node.children {
        if not(validate_trace_tree(child, trace_id)) {
          all_children_valid = false
        }
      }
      all_children_valid
    }
  }
  
  // Check temporal consistency
  let check_temporal_consistency = fn(node: TraceNode) -> Bool {
    // Parent should start before and end after all children
    for child in node.children {
      if node.start_time > child.start_time {
        return false
      }
      if node.end_time < child.end_time {
        return false
      }
      if not(check_temporal_consistency(child)) {
        return false
      }
    }
    true
  }
  
  // Count total spans in tree
  let count_spans = fn(node: TraceNode) {
    let mut count = 1  // Count current node
    for child in node.children {
      count = count + count_spans(child)
    }
    count
  }
  
  // Find spans by service
  let find_spans_by_service = fn(node: TraceNode, service_name: String) {
    let mut result = []
    if node.service_name == service_name {
      result = result.push(node)
    }
    for child in node.children {
      result = result + find_spans_by_service(child, service_name)
    }
    result
  }
  
  // Test validation
  assert_true(validate_trace_tree(trace_tree, "trace-123"))
  assert_true(check_temporal_consistency(trace_tree))
  
  // Test span counting
  let total_spans = count_spans(trace_tree)
  assert_eq(total_spans, 7)  // 1 root + 1 auth + 1 auth-db + 1 business + 1 inventory + 1 payment + 1 payment-db
  
  // Test service-specific span retrieval
  let db_spans = find_spans_by_service(trace_tree, "database")
  assert_eq(db_spans.length(), 2)
  
  let payment_spans = find_spans_by_service(trace_tree, "payment-service")
  assert_eq(payment_spans.length(), 1)
  
  // Test trace depth calculation
  let calculate_depth = fn(node: TraceNode) {
    if node.children.length() == 0 {
      1
    } else {
      let mut max_child_depth = 0
      for child in node.children {
        let child_depth = calculate_depth(child)
        if child_depth > max_child_depth {
          max_child_depth = child_depth
        }
      }
      1 + max_child_depth
    }
  }
  
  let trace_depth = calculate_depth(trace_tree)
  assert_eq(trace_depth, 3)  // Maximum depth is 3 levels
}

// Test 3: Distributed Trace Sampling Consistency
test "distributed trace sampling consistency" {
  // Define sampling decision structure
  type SamplingDecision = {
    trace_id: String,
    sampled: Bool,
    sampling_rate: Float,
    decision_reason: String,
    attributes: Array<(String, String)>
  }
  
  // Define sampling strategy
  type SamplingStrategy = {
    name: String,
    sample_rate: Float,
    priority_services: Array[String],
    error_sampling: Bool,
    slow_request_threshold: Int
  }
  
  // Create sampling strategies
  let default_strategy = {
    name: "default",
    sample_rate: 0.1,  // 10%
    priority_services: [],
    error_sampling: true,
    slow_request_threshold: 1000
  }
  
  let high_priority_strategy = {
    name: "high_priority",
    sample_rate: 1.0,  // 100%
    priority_services: ["payment-service", "auth-service"],
    error_sampling: true,
    slow_request_threshold: 500
  }
  
  // Make sampling decision
  let make_sampling_decision = fn(trace_id: String, service_name: String, strategy: SamplingStrategy, is_error: Bool, duration: Int) {
    let sampled = if strategy.priority_services.contains(service_name) {
      true  // Always sample priority services
    } else if is_error and strategy.error_sampling {
      true  // Always sample errors
    } else if duration > strategy.slow_request_threshold {
      true  // Always sample slow requests
    } else {
      // Use deterministic sampling based on trace ID hash
      let hash = trace_id.length() % 100  // Simple hash for testing
      (hash.to_float() / 100.0) < strategy.sample_rate
    }
    
    let reason = if strategy.priority_services.contains(service_name) {
      "priority_service"
    } else if is_error and strategy.error_sampling {
      "error_sampling"
    } else if duration > strategy.slow_request_threshold {
      "slow_request"
    } else {
      "probabilistic"
    }
    
    {
      trace_id,
      sampled,
      sampling_rate: strategy.sample_rate,
      decision_reason: reason,
      attributes: [
        ("service", service_name),
        ("strategy", strategy.name),
        ("duration_ms", duration.to_string())
      ]
    }
  }
  
  // Test sampling consistency across services
  let trace_id = "trace-consistency-test-123"
  
  let gateway_decision = make_sampling_decision(trace_id, "api-gateway", default_strategy, false, 200)
  let auth_decision = make_sampling_decision(trace_id, "auth-service", high_priority_strategy, false, 150)
  let payment_decision = make_sampling_decision(trace_id, "payment-service", high_priority_strategy, false, 300)
  let user_decision = make_sampling_decision(trace_id, "user-service", default_strategy, false, 100)
  let error_decision = make_sampling_decision(trace_id, "order-service", default_strategy, true, 50)
  let slow_decision = make_sampling_decision(trace_id, "catalog-service", default_strategy, false, 1500)
  
  // Verify sampling decisions
  assert_eq(gateway_decision.trace_id, trace_id)
  assert_eq(auth_decision.trace_id, trace_id)
  assert_eq(payment_decision.trace_id, trace_id)
  assert_eq(user_decision.trace_id, trace_id)
  assert_eq(error_decision.trace_id, trace_id)
  assert_eq(slow_decision.trace_id, trace_id)
  
  // Priority services should always be sampled
  assert_true(auth_decision.sampled)
  assert_true(payment_decision.sampled)
  assert_eq(auth_decision.decision_reason, "priority_service")
  assert_eq(payment_decision.decision_reason, "priority_service")
  
  // Errors should always be sampled
  assert_true(error_decision.sampled)
  assert_eq(error_decision.decision_reason, "error_sampling")
  
  // Slow requests should always be sampled
  assert_true(slow_decision.sampled)
  assert_eq(slow_decision.decision_reason, "slow_request")
  
  // Regular services follow probabilistic sampling
  // Note: In real implementation, this would be based on trace ID hash
  // For testing, we check the decision reason
  assert_eq(gateway_decision.decision_reason, "probabilistic")
  assert_eq(user_decision.decision_reason, "probabilistic")
  
  // Verify sampling rate consistency
  assert_eq(gateway_decision.sampling_rate, 0.1)
  assert_eq(auth_decision.sampling_rate, 1.0)
  assert_eq(payment_decision.sampling_rate, 1.0)
  assert_eq(user_decision.sampling_rate, 0.1)
  assert_eq(error_decision.sampling_rate, 0.1)
  assert_eq(slow_decision.sampling_rate, 0.1)
}

// Test 4: Trace Context Injection and Extraction
test "trace context injection and extraction mechanisms" {
  // Define context carrier types
  type HttpCarrier = {
    headers: Array<(String, String)>
  }
  
  type MessageCarrier = {
    properties: Array<(String, String)>
  }
  
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array<(String, String)>
  }
  
  // Inject context into HTTP headers
  let inject_http_headers = fn(context: TraceContext) {
    let mut headers = []
    headers = headers.push(("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"))
    headers = headers.push(("x-trace-id", context.trace_id))
    headers = headers.push(("x-span-id", context.span_id))
    
    match context.parent_span_id {
      Some(parent_id) => headers = headers.push(("x-parent-span-id", parent_id))
      None => {}
    }
    
    // Inject baggage items
    for (key, value) in context.baggage {
      headers = headers.push(("baggage-" + key, value))
    }
    
    { headers }
  }
  
  // Extract context from HTTP headers
  let extract_http_headers = fn(carrier: HttpCarrier) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut baggage = []
    
    for (key, value) in carrier.headers {
      if key == "traceparent" {
        // Parse traceparent format: 00-traceId-spanId-flags
        let parts = value.split("-")
        if parts.length() >= 3 {
          trace_id = parts[1]
          span_id = parts[2]
        }
      } else if key == "x-trace-id" {
        trace_id = value
      } else if key == "x-span-id" {
        span_id = value
      } else if key == "x-parent-span-id" {
        parent_span_id = Some(value)
      } else if key.starts_with("baggage-") {
        let baggage_key = key.substring(8, key.length() - 8)  // Remove "baggage-" prefix
        baggage = baggage.push((baggage_key, value))
      }
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage
    }
  }
  
  // Inject context into message properties
  let inject_message_properties = fn(context: TraceContext) {
    let mut properties = []
    properties = properties.push(("trace_id", context.trace_id))
    properties = properties.push(("span_id", context.span_id))
    
    match context.parent_span_id {
      Some(parent_id) => properties = properties.push(("parent_span_id", parent_id))
      None => {}
    }
    
    // Inject baggage items as JSON-like string
    let mut baggage_string = ""
    for i in 0..context.baggage.length() {
      let (key, value) = context.baggage[i]
      baggage_string = baggage_string + key + "=" + value
      if i < context.baggage.length() - 1 {
        baggage_string = baggage_string + ","
      }
    }
    
    if baggage_string.length() > 0 {
      properties = properties.push(("baggage", baggage_string))
    }
    
    { properties }
  }
  
  // Extract context from message properties
  let extract_message_properties = fn(carrier: MessageCarrier) {
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut baggage = []
    
    for (key, value) in carrier.properties {
      if key == "trace_id" {
        trace_id = value
      } else if key == "span_id" {
        span_id = value
      } else if key == "parent_span_id" {
        parent_span_id = Some(value)
      } else if key == "baggage" {
        // Parse baggage string: key1=value1,key2=value2
        let pairs = value.split(",")
        for pair in pairs {
          let key_value = pair.split("=")
          if key_value.length() == 2 {
            baggage = baggage.push((key_value[0], key_value[1]))
          }
        }
      }
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage
    }
  }
  
  // Create original context
  let original_context = {
    trace_id: "trace-inject-extract-123",
    span_id: "span-inject-extract-456",
    parent_span_id: Some("span-parent-789"),
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("service.version", "1.2.3")
    ]
  }
  
  // Test HTTP injection and extraction
  let http_carrier = inject_http_headers(original_context)
  let extracted_http_context = extract_http_headers(http_carrier)
  
  assert_eq(extracted_http_context.trace_id, original_context.trace_id)
  assert_eq(extracted_http_context.span_id, original_context.span_id)
  assert_eq(extracted_http_context.parent_span_id, original_context.parent_span_id)
  assert_eq(extracted_http_context.baggage.length(), original_context.baggage.length())
  
  // Verify specific baggage items
  for (key, value) in original_context.baggage {
    assert_true(extracted_http_context.baggage.contains((key, value)))
  }
  
  // Test message injection and extraction
  let message_carrier = inject_message_properties(original_context)
  let extracted_message_context = extract_message_properties(message_carrier)
  
  assert_eq(extracted_message_context.trace_id, original_context.trace_id)
  assert_eq(extracted_message_context.span_id, original_context.span_id)
  assert_eq(extracted_message_context.parent_span_id, original_context.parent_span_id)
  assert_eq(extracted_message_context.baggage.length(), original_context.baggage.length())
  
  // Verify specific baggage items
  for (key, value) in original_context.baggage {
    assert_true(extracted_message_context.baggage.contains((key, value)))
  }
  
  // Test cross-carrier consistency
  assert_eq(extracted_http_context.trace_id, extracted_message_context.trace_id)
  assert_eq(extracted_http_context.span_id, extracted_message_context.span_id)
  assert_eq(extracted_http_context.parent_span_id, extracted_message_context.parent_span_id)
  
  // Verify baggage consistency across carriers
  assert_eq(extracted_http_context.baggage.length(), extracted_message_context.baggage.length())
  for item in extracted_http_context.baggage {
    assert_true(extracted_message_context.baggage.contains(item))
  }
}

// Test 5: Distributed Trace State Management
test "distributed trace state management and synchronization" {
  // Define trace state entry
  type TraceStateEntry = {
    key: String,
    value: String,
    ttl: Int  // Time to live in seconds
  }
  
  // Define distributed trace state
  type DistributedTraceState = {
    trace_id: String,
    state_entries: Array[TraceStateEntry],
    last_updated: Int,
    version: Int
  }
  
  // Define state operation
  type StateOperation = {
    operation_type: String,
    key: String,
    value: Option[String>,
    timestamp: Int,
    service_name: String
  }
  
  // Create initial trace state
  let initial_state = {
    trace_id: "trace-state-test-123",
    state_entries: [
      { key: "sampling.decision", value: "true", ttl: 3600 },
      { key: "service.priority", value: "high", ttl: 1800 },
      { key: "user.tier", value: "premium", ttl: 7200 }
    ],
    last_updated: 1640995200,
    version: 1
  }
  
  // Apply state operation
  let apply_state_operation = fn(state: DistributedTraceState, operation: StateOperation) {
    let mut new_entries = state.state_entries
    let mut updated = false
    let current_time = operation.timestamp
    
    match operation.operation_type {
      "set" => {
        // Update existing entry or add new one
        let mut found = false
        let mut updated_entries = []
        
        for entry in new_entries {
          if entry.key == operation.key {
            match operation.value {
              Some(new_value) => {
                updated_entries = updated_entries.push({
                  key: entry.key,
                  value: new_value,
                  ttl: entry.ttl
                })
                found = true
                updated = true
              }
              None => {
                // Remove entry (skip adding to updated_entries)
                found = true
                updated = true
              }
            }
          } else {
            updated_entries = updated_entries.push(entry)
          }
        }
        
        if not(found) and operation.value.length > 0 {
          updated_entries = updated_entries.push({
            key: operation.key,
            value: operation.value.get_or(""),
            ttl: 3600  // Default TTL
          })
          updated = true
        }
        
        new_entries = updated_entries
      }
      "increment" => {
        // Increment numeric values
        let mut updated_entries = []
        for entry in new_entries {
          if entry.key == operation.key {
            match operation.value {
              Some(increment_value) => {
                let current_val = entry.value.to_int()
                let increment = increment_value.to_int()
                updated_entries = updated_entries.push({
                  key: entry.key,
                  value: (current_val + increment).to_string(),
                  ttl: entry.ttl
                })
                updated = true
              }
              None => {
                updated_entries = updated_entries.push(entry)
              }
            }
          } else {
            updated_entries = updated_entries.push(entry)
          }
        }
        new_entries = updated_entries
      }
      _ => {
        // Unknown operation, no change
      }
    }
    
    if updated {
      {
        trace_id: state.trace_id,
        state_entries: new_entries,
        last_updated: current_time,
        version: state.version + 1
      }
    } else {
      state
    }
  }
  
  // Get state value
  let get_state_value = fn(state: DistributedTraceState, key: String) {
    let mut found = None
    for entry in state.state_entries {
      if entry.key == key {
        found = Some(entry.value)
      }
    }
    found
  }
  
  // Check if state entry is expired
  let is_entry_expired = fn(entry: TraceStateEntry, current_time: Int) {
    let entry_age = current_time - entry.last_updated
    entry_age > entry.ttl
  }
  
  // Clean up expired entries
  let cleanup_expired_entries = fn(state: DistributedTraceState, current_time: Int) {
    let mut valid_entries = []
    for entry in state.state_entries {
      if not(is_entry_expired(entry, current_time)) {
        valid_entries = valid_entries.push(entry)
      }
    }
    
    {
      trace_id: state.trace_id,
      state_entries: valid_entries,
      last_updated: state.last_updated,
      version: state.version
    }
  }
  
  // Test state operations
  let operation1 = {
    operation_type: "set",
    key: "request.count",
    value: Some("1"),
    timestamp: 1640995205,
    service_name: "api-gateway"
  }
  
  let operation2 = {
    operation_type: "increment",
    key: "request.count",
    value: Some("1"),
    timestamp: 1640995210,
    service_name: "auth-service"
  }
  
  let operation3 = {
    operation_type: "set",
    key: "user.tier",
    value: Some("enterprise"),
    timestamp: 1640995215,
    service_name: "user-service"
  }
  
  let operation4 = {
    operation_type: "set",
    key: "service.priority",
    value: None,  // Remove this entry
    timestamp: 1640995220,
    service_name: "business-service"
  }
  
  // Apply operations
  let state1 = apply_state_operation(initial_state, operation1)
  let state2 = apply_state_operation(state1, operation2)
  let state3 = apply_state_operation(state2, operation3)
  let state4 = apply_state_operation(state3, operation4)
  
  // Verify state changes
  assert_eq(state4.version, 4)  // Each successful operation increments version
  
  // Check final state values
  assert_eq(get_state_value(state4, "sampling.decision"), Some("true"))
  assert_eq(get_state_value(state4, "request.count"), Some("2"))  // 1 + 1
  assert_eq(get_state_value(state4, "user.tier"), Some("enterprise"))
  assert_eq(get_state_value(state4, "service.priority"), None)  // Should be removed
  
  // Test state synchronization between services
  let synchronize_states = fn(local_state: DistributedTraceState, remote_state: DistributedTraceState) {
    if remote_state.version > local_state.version {
      remote_state
    } else {
      local_state
    }
  }
  
  // Create conflicting state
  let conflicting_state = {
    trace_id: state4.trace_id,
    state_entries: state4.state_entries,
    last_updated: 1640995230,
    version: 5  // Higher version
  }
  
  // Test synchronization
  let synchronized_state = synchronize_states(state4, conflicting_state)
  assert_eq(synchronized_state.version, 5)
  
  // Test state cleanup
  let expired_state = {
    trace_id: "trace-expired-test-456",
    state_entries: [
      { key: "temp.value", value: "123", ttl: 10 },  // Will expire
      { key: "permanent.value", value: "456", ttl: 3600 }  // Won't expire
    ],
    last_updated: 1640995200,
    version: 1
  }
  
  let current_time = 1640995250  // 50 seconds later
  let cleaned_state = cleanup_expired_entries(expired_state, current_time)
  
  assert_eq(cleaned_state.state_entries.length(), 1)
  assert_eq(cleaned_state.state_entries[0].key, "permanent.value")
  assert_eq(get_state_value(cleaned_state, "temp.value"), None)
  assert_eq(get_state_value(cleaned_state, "permanent.value"), Some("456"))
}

// Test 6: Trace Consistency Validation
test "trace consistency validation and anomaly detection" {
  // Define trace validation rule
  type ValidationRule = {
    name: String,
    description: String,
    validate: Array[String] -> Bool
  }
  
  // Define trace anomaly
  type TraceAnomaly = {
    anomaly_type: String,
    severity: String,
    description: String,
    affected_spans: Array[String]
  }
  
  // Define trace validation result
  type ValidationResult = {
    is_valid: Bool,
    anomalies: Array[TraceAnomaly],
    validated_at: Int
  }
  
  // Create validation rules
  let rules = [
    {
      name: "trace_id_consistency",
      description: "All spans in a trace must have the same trace ID",
      validate: fn(span_trace_ids: Array[String]) {
        if span_trace_ids.length() <= 1 {
          true
        } else {
          let first_id = span_trace_ids[0]
          let mut all_consistent = true
          for id in span_trace_ids {
            if id != first_id {
              all_consistent = false
            }
          }
          all_consistent
        }
      }
    },
    {
      name: "parent_child_temporal_consistency",
      description: "Parent spans must start before and end after child spans",
      validate: fn(temporal_data: Array[String]) {
        // Simplified validation - in real implementation would parse temporal relationships
        temporal_data.length() % 3 == 0  // Dummy validation for testing
      }
    },
    {
      name: "span_id_uniqueness",
      description: "All span IDs within a trace must be unique",
      validate: fn(span_ids: Array[String]) {
        let mut unique_ids = []
        for id in span_ids {
          if unique_ids.contains(id) {
            return false
          }
          unique_ids = unique_ids.push(id)
        }
        true
      }
    }
  ]
  
  // Create sample trace data for validation
  let trace_spans = [
    { span_id: "span-001", trace_id: "trace-validation-123", parent_span_id: None, start_time: 1640995200, end_time: 1640995230 },
    { span_id: "span-002", trace_id: "trace-validation-123", parent_span_id: Some("span-001"), start_time: 1640995205, end_time: 1640995210 },
    { span_id: "span-003", trace_id: "trace-validation-123", parent_span_id: Some("span-001"), start_time: 1640995215, end_time: 1640995220 },
    { span_id: "span-004", trace_id: "trace-validation-123", parent_span_id: Some("span-002"), start_time: 1640995207, end_time: 1640995209 }
  ]
  
  // Create inconsistent trace data
  let inconsistent_spans = [
    { span_id: "span-001", trace_id: "trace-inconsistent-123", parent_span_id: None, start_time: 1640995200, end_time: 1640995230 },
    { span_id: "span-002", trace_id: "trace-inconsistent-456", parent_span_id: Some("span-001"), start_time: 1640995205, end_time: 1640995210 },  // Different trace ID
    { span_id: "span-001", trace_id: "trace-inconsistent-123", parent_span_id: Some("span-001"), start_time: 1640995215, end_time: 1640995220 },  // Duplicate span ID
    { span_id: "span-004", trace_id: "trace-inconsistent-123", parent_span_id: Some("span-002"), start_time: 1640995250, end_time: 1640995240 }  // Invalid temporal relationship
  ]
  
  // Validate trace consistency
  let validate_trace_consistency = fn(spans: Array[String]) {
    let mut anomalies = []
    let mut is_valid = true
    
    // Extract trace IDs
    let trace_ids = spans.map(fn(span) { span.split(",")[1] })  // Simplified parsing
    
    // Check trace ID consistency
    let trace_id_rule = rules[0]
    if not(trace_id_rule.validate(trace_ids)) {
      anomalies = anomalies.push({
        anomaly_type: "trace_id_inconsistency",
        severity: "error",
        description: "Spans have inconsistent trace IDs",
        affected_spans: spans.map(fn(span) { span.split(",")[0] })
      })
      is_valid = false
    }
    
    // Extract span IDs
    let span_ids = spans.map(fn(span) { span.split(",")[0] })
    
    // Check span ID uniqueness
    let span_id_rule = rules[2]
    if not(span_id_rule.validate(span_ids)) {
      anomalies = anomalies.push({
        anomaly_type: "duplicate_span_id",
        severity: "error",
        description: "Duplicate span IDs found in trace",
        affected_spans: span_ids
      })
      is_valid = false
    }
    
    {
      is_valid,
      anomalies,
      validated_at: 1640995300
    }
  }
  
  // Convert spans to string format for validation
  let spans_to_strings = fn(spans) {
    spans.map(fn(span) {
      span.span_id + "," + span.trace_id + "," + 
      match span.parent_span_id {
        Some(parent) => parent
        None => "none"
      } + "," + span.start_time.to_string() + "," + span.end_time.to_string()
    })
  }
  
  // Test consistent trace validation
  let consistent_span_strings = spans_to_strings(trace_spans)
  let consistent_result = validate_trace_consistency(consistent_span_strings)
  
  assert_true(consistent_result.is_valid)
  assert_eq(consistent_result.anomalies.length(), 0)
  
  // Test inconsistent trace validation
  let inconsistent_span_strings = spans_to_strings(inconsistent_spans)
  let inconsistent_result = validate_trace_consistency(inconsistent_span_strings)
  
  assert_false(inconsistent_result.is_valid)
  assert_true(inconsistent_result.anomalies.length() > 0)
  
  // Verify specific anomalies
  let mut found_trace_id_anomaly = false
  let mut found_duplicate_span_anomaly = false
  
  for anomaly in inconsistent_result.anomalies {
    if anomaly.anomaly_type == "trace_id_inconsistency" {
      found_trace_id_anomaly = true
      assert_eq(anomaly.severity, "error")
    } else if anomaly.anomaly_type == "duplicate_span_id" {
      found_duplicate_span_anomaly = true
      assert_eq(anomaly.severity, "error")
    }
  }
  
  assert_true(found_trace_id_anomaly)
  assert_true(found_duplicate_span_anomaly)
  
  // Test anomaly severity classification
  let classify_anomaly_severity = fn(anomaly_type: String) {
    match anomaly_type {
      "trace_id_inconsistency" => "critical"
      "duplicate_span_id" => "error"
      "temporal_inconsistency" => "warning"
      "missing_parent" => "warning"
      _ => "info"
    }
  }
  
  assert_eq(classify_anomaly_severity("trace_id_inconsistency"), "critical")
  assert_eq(classify_anomaly_severity("duplicate_span_id"), "error")
  assert_eq(classify_anomaly_severity("temporal_inconsistency"), "warning")
  assert_eq(classify_anomaly_severity("unknown_anomaly"), "info")
}