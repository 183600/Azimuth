// Azimuth Telemetry System - New Context Propagation Boundary Tests
// This file contains test cases for context propagation across boundaries

// Test 1: Cross-Service Context Propagation
test "cross-service context propagation operations" {
  // Create initial context in service A
  let service_a_context = {
    trace_id: "trace-boundary-001",
    span_id: "span-a-001",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("session.id", "session-789")
    ],
    trace_flags: 1
  }
  
  // Simulate context extraction for service B
  let extract_context_for_service = fn(context: Dynamic, target_service: String) {
    let relevant_baggage = context.baggage.filter_fn(item) {
      // Only propagate certain baggage items based on target service
      match target_service {
        "auth-service" => {
          item.0 == "user.id" or item.0 == "session.id"
        }
        "payment-service" => {
          item.0 == "user.id" or item.0 == "request.id"
        }
        "notification-service" => {
          item.0 == "user.id" or item.0 == "request.id"
        }
        _ => {
          true // Default: propagate all
        }
      }
    }
    
    {
      trace_id: context.trace_id,
      span_id: "span-" + target_service + "-" + "001",
      baggage: relevant_baggage,
      trace_flags: context.trace_flags,
      source_service: "service-a"
    }
  }
  
  // Test context propagation to auth-service
  let auth_context = extract_context_for_service(service_a_context, "auth-service")
  assert_eq(auth_context.trace_id, "trace-boundary-001")
  assert_eq(auth_context.span_id, "span-auth-service-001")
  assert_eq(auth_context.baggage.length(), 2) // Only user.id and session.id
  assert_true(auth_context.baggage.contains(("user.id", "user-123")))
  assert_true(auth_context.baggage.contains(("session.id", "session-789")))
  assert_false(auth_context.baggage.contains(("request.id", "req-456")))
  
  // Test context propagation to payment-service
  let payment_context = extract_context_for_service(service_a_context, "payment-service")
  assert_eq(payment_context.trace_id, "trace-boundary-001")
  assert_eq(payment_context.span_id, "span-payment-service-001")
  assert_eq(payment_context.baggage.length(), 2) // Only user.id and request.id
  assert_true(payment_context.baggage.contains(("user.id", "user-123")))
  assert_true(payment_context.baggage.contains(("request.id", "req-456")))
  assert_false(payment_context.baggage.contains(("session.id", "session-789")))
  
  // Test context propagation to unknown service (should propagate all)
  let unknown_context = extract_context_for_service(service_a_context, "unknown-service")
  assert_eq(unknown_context.baggage.length(), 3) // All baggage items
  assert_true(unknown_context.baggage.contains(("user.id", "user-123")))
  assert_true(unknown_context.baggage.contains(("request.id", "req-456")))
  assert_true(unknown_context.baggage.contains(("session.id", "session-789")))
}

// Test 2: Context Propagation with Baggage Mutations
test "context propagation with baggage mutations" {
  // Create initial context
  let initial_context = {
    trace_id: "trace-mutation-001",
    span_id: "span-001",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("correlation.id", "corr-789")
    ],
    trace_flags: 1
  }
  
  // Add baggage item function
  let add_baggage_item = fn(context: Dynamic, key: String, value: String) {
    let existing_item = context.baggage.find_fn(item) { item.0 == key }
    
    match existing_item {
      Some(_) => {
        // Update existing item
        { context | 
          baggage: context.baggage.map_fn(item) {
            if item.0 == key {
              (key, value)
            } else {
              item
            }
          }
        }
      }
      None => {
        // Add new item
        { context | 
          baggage: context.baggage.push((key, value))
        }
      }
    }
  }
  
  // Remove baggage item function
  let remove_baggage_item = fn(context: Dynamic, key: String) {
    { context | 
      baggage: context.baggage.filter_fn(item) { item.0 != key }
    }
  }
  
  // Create child context with new span ID
  let create_child_context = fn(parent_context: Dynamic, span_id: String) {
    { parent_context | span_id: span_id }
  }
  
  // Test baggage mutations
  let context_with_added = add_baggage_item(initial_context, "auth.token", "token-abc")
  assert_eq(context_with_added.baggage.length(), 4)
  assert_true(context_with_added.baggage.contains(("auth.token", "token-abc")))
  
  let context_with_updated = add_baggage_item(context_with_added, "user.id", "user-456")
  assert_eq(context_with_updated.baggage.length(), 4) // Still 4, not 5
  assert_true(context_with_updated.baggage.contains(("user.id", "user-456")))
  
  let context_with_removed = remove_baggage_item(context_with_updated, "request.id")
  assert_eq(context_with_removed.baggage.length(), 3)
  assert_false(context_with_removed.baggage.contains(("request.id", "req-456")))
  
  // Test child context creation
  let child_context = create_child_context(context_with_removed, "span-002")
  assert_eq(child_context.span_id, "span-002")
  assert_eq(child_context.trace_id, "trace-mutation-001")
  assert_eq(child_context.baggage.length(), 3) // Inherits baggage from parent
  
  // Test that mutations to child don't affect parent
  let mutated_child = add_baggage_item(child_context, "child.only", "value")
  assert_eq(mutated_child.baggage.length(), 4)
  assert_eq(context_with_removed.baggage.length(), 3) // Parent unchanged
}

// Test 3: Context Propagation Across Process Boundaries
test "context propagation across process boundaries" {
  // Serialize context to string (simulating cross-process transmission)
  let serialize_context = fn(context: Dynamic) {
    let baggage_str = context.baggage.map_fn(item) {
      item.0 + "=" + item.1
    }.join(",")
    
    "trace-id=" + context.trace_id + 
    ";span-id=" + context.span_id + 
    ";trace-flags=" + context.trace_flags.to_string() + 
    ";baggage=" + baggage_str
  }
  
  // Deserialize context from string
  let deserialize_context = fn(context_str: String) {
    let parts = context_str.split(";")
    
    let get_part_value = fn(parts: Array[String], prefix: String) {
      let part = parts.find_fn(p) { p.starts_with(prefix) }
      match part {
        Some(p) => p.substring(prefix.length(), p.length())
        None => ""
      }
    }
    
    let trace_id = get_part_value(parts, "trace-id=")
    let span_id = get_part_value(parts, "span-id=")
    let trace_flags = get_part_value(parts, "trace-flags=").to_int()
    
    let baggage_str = get_part_value(parts, "baggage=")
    let baggage_items = if baggage_str == "" {
      []
    } else {
      baggage_str.split(",").map_fn(item_str) {
        let item_parts = item_str.split("=")
        (item_parts[0], item_parts[1])
      }
    }
    
    {
      trace_id: trace_id,
      span_id: span_id,
      baggage: baggage_items,
      trace_flags: trace_flags
    }
  }
  
  // Create original context
  let original_context = {
    trace_id: "trace-process-001",
    span_id: "span-process-001",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("tenant.id", "tenant-789")
    ],
    trace_flags: 1
  }
  
  // Test serialization
  let serialized = serialize_context(original_context)
  assert_true(serialized.contains("trace-id=trace-process-001"))
  assert_true(serialized.contains("span-id=span-process-001"))
  assert_true(serialized.contains("trace-flags=1"))
  assert_true(serialized.contains("baggage=user.id=user-123,request.id=req-456,tenant.id=tenant-789"))
  
  // Test deserialization
  let deserialized = deserialize_context(serialized)
  assert_eq(deserialized.trace_id, "trace-process-001")
  assert_eq(deserialized.span_id, "span-process-001")
  assert_eq(deserialized.trace_flags, 1)
  assert_eq(deserialized.baggage.length(), 3)
  assert_true(deserialized.baggage.contains(("user.id", "user-123")))
  assert_true(deserialized.baggage.contains(("request.id", "req-456")))
  assert_true(deserialized.baggage.contains(("tenant.id", "tenant-789")))
  
  // Test round-trip consistency
  let reserialized = serialize_context(deserialized)
  let redeserialized = deserialize_context(reserialized)
  
  assert_eq(redeserialized.trace_id, original_context.trace_id)
  assert_eq(redeserialized.span_id, original_context.span_id)
  assert_eq(redeserialized.trace_flags, original_context.trace_flags)
  assert_eq(redeserialized.baggage.length(), original_context.baggage.length())
  
  for item in original_context.baggage {
    assert_true(redeserialized.baggage.contains(item))
  }
}

// Test 4: Context Propagation with Sampling Decisions
test "context propagation with sampling decisions" {
  // Define sampling decision types
  enum SamplingDecision {
    Drop
    RecordOnly
    RecordAndSample
  }
  
  // Create context with sampling decision
  let create_context_with_sampling = fn(trace_id: String, span_id: String, sampling_decision: SamplingDecision) {
    let trace_flags = match sampling_decision {
      SamplingDecision::Drop => 0
      SamplingDecision::RecordOnly => 1
      SamplingDecision::RecordAndSample => 2
    }
    
    {
      trace_id: trace_id,
      span_id: span_id,
      baggage: [],
      trace_flags: trace_flags,
      sampling_decision: sampling_decision
    }
  }
  
  // Check if context is sampled
  let is_sampled = fn(context: Dynamic) {
    context.trace_flags > 0
  }
  
  // Check if context is recorded and sampled
  let is_recorded_and_sampled = fn(context: Dynamic) {
    context.trace_flags == 2
  }
  
  // Propagate sampling decision to child
  let propagate_sampling_to_child = fn(parent_context: Dynamic, child_span_id: String) {
    // Child inherits sampling decision from parent
    {
      trace_id: parent_context.trace_id,
      span_id: child_span_id,
      baggage: parent_context.baggage,
      trace_flags: parent_context.trace_flags,
      sampling_decision: parent_context.sampling_decision
    }
  }
  
  // Test different sampling decisions
  let drop_context = create_context_with_sampling("trace-drop-001", "span-drop-001", SamplingDecision::Drop)
  let record_only_context = create_context_with_sampling("trace-record-001", "span-record-001", SamplingDecision::RecordOnly)
  let record_and_sample_context = create_context_with_sampling("trace-sample-001", "span-sample-001", SamplingDecision::RecordAndSample)
  
  // Test sampling checks
  assert_false(is_sampled(drop_context))
  assert_true(is_sampled(record_only_context))
  assert_true(is_sampled(record_and_sample_context))
  
  assert_false(is_recorded_and_sampled(drop_context))
  assert_false(is_recorded_and_sampled(record_only_context))
  assert_true(is_recorded_and_sampled(record_and_sample_context))
  
  // Test sampling propagation to children
  let drop_child = propagate_sampling_to_child(drop_context, "span-drop-002")
  let record_child = propagate_sampling_to_child(record_only_context, "span-record-002")
  let sample_child = propagate_sampling_to_child(record_and_sample_context, "span-sample-002")
  
  assert_false(is_sampled(drop_child))
  assert_true(is_sampled(record_child))
  assert_true(is_sampled(sample_child))
  
  assert_false(is_recorded_and_sampled(drop_child))
  assert_false(is_recorded_and_sampled(record_child))
  assert_true(is_recorded_and_sampled(sample_child))
  
  // Test baggage propagation with different sampling decisions
  let context_with_baggage = { record_only_context | 
    baggage: [("user.id", "user-123"), ("request.id", "req-456")]
  }
  
  let child_with_baggage = propagate_sampling_to_child(context_with_baggage, "span-child-001")
  assert_eq(child_with_baggage.baggage.length(), 2)
  assert_eq(child_with_baggage.trace_flags, 1) // Inherits RecordOnly sampling
}

// Test 5: Context Propagation Security Boundaries
test "context propagation security boundaries" {
  // Define security levels
  enum SecurityLevel {
    Public
    Internal
    Confidential
    Secret
  }
  
  // Create context with security-sensitive baggage
  let create_secure_context = fn(security_level: SecurityLevel) {
    let baggage = match security_level {
      SecurityLevel::Public => [
        ("request.id", "req-public"),
        ("correlation.id", "corr-public")
      ]
      SecurityLevel::Internal => [
        ("request.id", "req-internal"),
        ("correlation.id", "corr-internal"),
        ("service.name", "internal-service")
      ]
      SecurityLevel::Confidential => [
        ("request.id", "req-confidential"),
        ("correlation.id", "corr-confidential"),
        ("service.name", "confidential-service"),
        ("user.id", "user-confidential")
      ]
      SecurityLevel::Secret => [
        ("request.id", "req-secret"),
        ("correlation.id", "corr-secret"),
        ("service.name", "secret-service"),
        ("user.id", "user-secret"),
        ("auth.token", "token-secret")
      ]
    }
    
    {
      trace_id: "trace-secure-001",
      span_id: "span-secure-001",
      baggage: baggage,
      trace_flags: 1,
      security_level: security_level
    }
  }
  
  // Filter baggage for security boundary
  let filter_baggage_for_boundary = fn(context: Dynamic, target_security_level: SecurityLevel) {
    let source_level = context.security_level
    let target_level = target_security_level
    
    // Define which baggage items can cross security boundaries
    let allowed_items = match (source_level, target_level) {
      // Public can receive any baggage (least restrictive)
      (_, SecurityLevel::Public) => [
        "request.id", "correlation.id"
      ]
      
      // Internal can receive from Public or Internal
      (SecurityLevel::Public, SecurityLevel::Internal) => [
        "request.id", "correlation.id"
      ]
      (SecurityLevel::Internal, SecurityLevel::Internal) => [
        "request.id", "correlation.id", "service.name"
      ]
      (SecurityLevel::Confidential, SecurityLevel::Internal) => [
        "request.id", "correlation.id", "service.name"
      ]
      (SecurityLevel::Secret, SecurityLevel::Internal) => [
        "request.id", "correlation.id", "service.name"
      ]
      
      // Confidential can receive from Public, Internal, or Confidential
      (SecurityLevel::Public, SecurityLevel::Confidential) => [
        "request.id", "correlation.id"
      ]
      (SecurityLevel::Internal, SecurityLevel::Confidential) => [
        "request.id", "correlation.id", "service.name"
      ]
      (SecurityLevel::Confidential, SecurityLevel::Confidential) => [
        "request.id", "correlation.id", "service.name", "user.id"
      ]
      (SecurityLevel::Secret, SecurityLevel::Confidential) => [
        "request.id", "correlation.id", "service.name", "user.id"
      ]
      
      // Secret can receive from any level (most restrictive)
      (SecurityLevel::Public, SecurityLevel::Secret) => [
        "request.id", "correlation.id"
      ]
      (SecurityLevel::Internal, SecurityLevel::Secret) => [
        "request.id", "correlation.id", "service.name"
      ]
      (SecurityLevel::Confidential, SecurityLevel::Secret) => [
        "request.id", "correlation.id", "service.name", "user.id"
      ]
      (SecurityLevel::Secret, SecurityLevel::Secret) => [
        "request.id", "correlation.id", "service.name", "user.id", "auth.token"
      ]
    }
    
    let filtered_baggage = context.baggage.filter_fn(item) {
      allowed_items.contains(item.0)
    }
    
    {
      trace_id: context.trace_id,
      span_id: "span-" + target_level.to_string() + "-001",
      baggage: filtered_baggage,
      trace_flags: context.trace_flags,
      security_level: target_level
    }
  }
  
  // Test security level contexts
  let public_context = create_secure_context(SecurityLevel::Public)
  let internal_context = create_secure_context(SecurityLevel::Internal)
  let confidential_context = create_secure_context(SecurityLevel::Confidential)
  let secret_context = create_secure_context(SecurityLevel::Secret)
  
  // Test baggage lengths
  assert_eq(public_context.baggage.length(), 2)
  assert_eq(internal_context.baggage.length(), 3)
  assert_eq(confidential_context.baggage.length(), 4)
  assert_eq(secret_context.baggage.length(), 5)
  
  // Test filtering from Secret to Public (most restrictive)
  let secret_to_public = filter_baggage_for_boundary(secret_context, SecurityLevel::Public)
  assert_eq(secret_to_public.baggage.length(), 2) // Only request.id and correlation.id
  assert_true(secret_to_public.baggage.contains(("request.id", "req-secret")))
  assert_true(secret_to_public.baggage.contains(("correlation.id", "corr-secret")))
  assert_false(secret_to_public.baggage.contains(("user.id", "user-secret")))
  assert_false(secret_to_public.baggage.contains(("auth.token", "token-secret")))
  
  // Test filtering from Secret to Internal
  let secret_to_internal = filter_baggage_for_boundary(secret_context, SecurityLevel::Internal)
  assert_eq(secret_to_internal.baggage.length(), 3) // request.id, correlation.id, service.name
  assert_true(secret_to_internal.baggage.contains(("request.id", "req-secret")))
  assert_true(secret_to_internal.baggage.contains(("correlation.id", "corr-secret")))
  assert_true(secret_to_internal.baggage.contains(("service.name", "secret-service")))
  assert_false(secret_to_internal.baggage.contains(("user.id", "user-secret")))
  assert_false(secret_to_internal.baggage.contains(("auth.token", "token-secret")))
  
  // Test filtering from Secret to Confidential
  let secret_to_confidential = filter_baggage_for_boundary(secret_context, SecurityLevel::Confidential)
  assert_eq(secret_to_confidential.baggage.length(), 4) // request.id, correlation.id, service.name, user.id
  assert_true(secret_to_confidential.baggage.contains(("request.id", "req-secret")))
  assert_true(secret_to_confidential.baggage.contains(("correlation.id", "corr-secret")))
  assert_true(secret_to_confidential.baggage.contains(("service.name", "secret-service")))
  assert_true(secret_to_confidential.baggage.contains(("user.id", "user-secret")))
  assert_false(secret_to_confidential.baggage.contains(("auth.token", "token-secret")))
  
  // Test filtering from Secret to Secret (no filtering)
  let secret_to_secret = filter_baggage_for_boundary(secret_context, SecurityLevel::Secret)
  assert_eq(secret_to_secret.baggage.length(), 5) // All baggage items
  assert_true(secret_to_secret.baggage.contains(("request.id", "req-secret")))
  assert_true(secret_to_secret.baggage.contains(("correlation.id", "corr-secret")))
  assert_true(secret_to_secret.baggage.contains(("service.name", "secret-service")))
  assert_true(secret_to_secret.baggage.contains(("user.id", "user-secret")))
  assert_true(secret_to_secret.baggage.contains(("auth.token", "token-secret")))
}