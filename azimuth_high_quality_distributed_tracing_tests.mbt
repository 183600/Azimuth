// Azimuth Telemetry System - High Quality Distributed Tracing Tests
// This file contains comprehensive test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Test trace context creation and serialization
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = 1 // Sampled
  
  let trace_context = TraceContext::new(trace_id, span_id, trace_flags)
  
  // Verify trace context properties
  assert_eq(TraceContext::trace_id(trace_context), trace_id)
  assert_eq(TraceContext::span_id(trace_context), span_id)
  assert_eq(TraceContext::trace_flags(trace_context), trace_flags)
  assert_true(TraceContext::is_sampled(trace_context))
  
  // Test trace context injection into headers
  let headers = TextMapCarrier::new()
  let injector = TraceContextPropagator::new()
  
  TraceContextPropagator::inject(injector, trace_context, headers)
  
  // Verify injected headers
  let traceparent = TextMapCarrier::get(headers, "traceparent")
  match traceparent {
    Some(value) => {
      // Expected format: 00-trace_id-span_id-trace_flags
      assert_eq(value, "00-" + trace_id + "-" + span_id + "-01")
    }
    None => assert_true(false) // Should have traceparent header
  }
  
  // Test trace context extraction from headers
  let extraction_headers = TextMapCarrier::new()
  TextMapCarrier::set(extraction_headers, "traceparent", "00-" + trace_id + "-" + span_id + "-01")
  
  let extractor = TraceContextPropagator::new()
  let extracted_context = TraceContextPropagator::extract(extractor, extraction_headers)
  
  match extracted_context {
    Some(context) => {
      assert_eq(TraceContext::trace_id(context), trace_id)
      assert_eq(TraceContext::span_id(context), span_id)
      assert_eq(TraceContext::trace_flags(context), trace_flags)
    }
    None => assert_true(false) // Should extract context
  }
  
  // Test trace context with baggage
  let baggage_context = TraceContext::with_baggage(trace_context, [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("service.version", "1.2.3")
  ])
  
  // Inject baggage
  let baggage_headers = TextMapCarrier::new()
  TraceContextPropagator::inject(injector, baggage_context, baggage_headers)
  
  // Verify baggage header
  let baggage_header = TextMapCarrier::get(baggage_headers, "baggage")
  match baggage_header {
    Some(value) => {
      assert_true(value.contains("user.id=12345"))
      assert_true(value.contains("request.id=req-67890"))
      assert_true(value.contains("service.version=1.2.3"))
    }
    None => assert_true(false) // Should have baggage header
  }
  
  // Extract baggage
  let extracted_baggage = TraceContextPropagator::extract(extractor, baggage_headers)
  match extracted_baggage {
    Some(context) => {
      let baggage_entries = TraceContext::baggage(context)
      assert_eq(baggage_entries.length(), 3)
      
      let user_id = TraceContext::get_baggage_item(context, "user.id")
      match user_id {
        Some(value) => assert_eq(value, "12345"),
        None => assert_true(false) // Should have user.id
      }
      
      let request_id = TraceContext::get_baggage_item(context, "request.id")
      match request_id {
        Some(value) => assert_eq(value, "req-67890"),
        None => assert_true(false) // Should have request.id
      }
    }
    None => assert_true(false) // Should extract context
  }
}

// Test 2: Span Creation and Lifecycle Management
test "span creation and lifecycle management" {
  // Test span creation
  let tracer = Tracer::new("test-service")
  
  let span = Tracer::start_span(tracer, "operation-name")
  
  // Verify span properties
  assert_eq(Span::name(span), "operation-name")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::start_time(span) > 0)
  assert_eq(Span::end_time(span), 0) // Not ended yet
  assert_true(Span::is_recording(span))
  
  // Test span with parent
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let child_span = Tracer::start_span_with_parent(tracer, "child-operation", parent_span)
  
  // Verify parent-child relationship
  assert_eq(Span::trace_id(child_span), Span::trace_id(parent_span))
  assert_not_eq(Span::span_id(child_span), Span::span_id(parent_span))
  assert_eq(Span::parent_span_id(child_span), Some(Span::span_id(parent_span)))
  
  // Test span attributes
  Span::set_attribute(span, "http.method", "GET")
  Span::set_attribute(span, "http.url", "https://example.com/api/data")
  Span::set_attribute(span, "http.status_code", 200)
  
  let attributes = Span::attributes(span)
  assert_true(attributes.contains("http.method"))
  assert_true(attributes.contains("http.url"))
  assert_true(attributes.contains("http.status_code"))
  
  // Test span events
  Span::add_event(span, "database.query.start", [
    ("db.statement", "SELECT * FROM users"),
    ("db.type", "postgresql")
  ])
  
  Span::add_event(span, "database.query.complete", [
    ("db.rows_affected", "42"),
    ("db.duration_ms", "150")
  ])
  
  let events = Span::events(span)
  assert_eq(events.length(), 2)
  
  // Verify first event
  let first_event = events[0]
  assert_eq(Event::name(first_event), "database.query.start")
  assert_true(Event::attributes(first_event).contains("db.statement"))
  assert_true(Event::attributes(first_event).contains("db.type"))
  
  // Test span status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  let status = Span::status(span)
  assert_eq(status.status_code, Ok)
  match status.message {
    Some(msg) => assert_eq(msg, "Operation completed successfully"),
    None => assert_true(false) // Should have message
  }
  
  // Test span ending
  Span::end(span)
  assert_true(Span::end_time(span) > 0)
  assert_false(Span::is_recording(span))
  
  // Test span duration calculation
  let duration = Span::duration(span)
  assert_true(duration > 0)
  
  // Test span with specific timestamps
  let start_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  let end_time = 1640995200150L // 150ms later
  
  let timed_span = Tracer::start_span_with_timestamp(tracer, "timed-operation", start_time)
  Span::end_with_timestamp(timed_span, end_time)
  
  let timed_duration = Span::duration(timed_span)
  assert_eq(timed_duration, 150)
}

// Test 3: Distributed Trace Across Multiple Services
test "distributed trace across multiple services" {
  // Simulate distributed trace across multiple services
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // Service A: Entry point
  let tracer_a = Tracer::new("service-a")
  let root_span = Tracer::start_span_with_trace_id(tracer_a, "HTTP GET /api/data", trace_id)
  
  Span::set_attribute(root_span, "http.method", "GET")
  Span::set_attribute(root_span, "http.url", "https://api.example.com/data")
  Span::set_attribute(root_span, "user.id", "12345")
  
  // Extract context for downstream service
  let headers = TextMapCarrier::new()
  let injector = TraceContextPropagator::new()
  TraceContextPropagator::inject(injector, Span::context(root_span), headers)
  
  // Service B: Processes request
  let tracer_b = Tracer::new("service-b")
  let extractor = TraceContextPropagator::new()
  let extracted_context = TraceContextPropagator::extract(extractor, headers)
  
  match extracted_context {
    Some(context) => {
      let service_b_span = Tracer::start_span_with_context(tracer_b, "process-data", context)
      
      Span::set_attribute(service_b_span, "service", "service-b")
      Span::add_event(service_b_span, "validation.start")
      
      // Service B calls Service C
      let service_b_headers = TextMapCarrier::new()
      TraceContextPropagator::inject(injector, Span::context(service_b_span), service_b_headers)
      
      // Service C: Database operation
      let tracer_c = Tracer::new("service-c")
      let service_c_context = TraceContextPropagator::extract(extractor, service_b_headers)
      
      match service_c_context {
        Some(c_context) => {
          let service_c_span = Tracer::start_span_with_context(tracer_c, "database.query", c_context)
          
          Span::set_attribute(service_c_span, "db.system", "postgresql")
          Span::set_attribute(service_c_span, "db.statement", "SELECT * FROM data WHERE id = $1")
          
          Span::add_event(service_c_span, "query.start")
          Span::add_event(service_c_span, "query.complete", [
            ("db.rows_affected", "1"),
            ("db.duration_ms", "25")
          ])
          
          Span::set_status(service_c_span, Ok, Some("Query successful"))
          Span::end(service_c_span)
          
          // Service C completes
        }
        None => assert_true(false) // Should extract context
      }
      
      Span::add_event(service_b_span, "validation.complete")
      Span::set_status(service_b_span, Ok, Some("Processing successful"))
      Span::end(service_b_span)
      
      // Service B completes
    }
    None => assert_true(false) // Should extract context
  }
  
  // Service A completes
  Span::set_status(root_span, Ok, Some("Request processed"))
  Span::end(root_span)
  
  // Verify trace relationships
  assert_eq(Span::trace_id(root_span), trace_id)
  
  // In a real implementation, we would collect all spans and verify relationships
  // For this test, we just verify the spans were created correctly
  assert_true(Span::duration(root_span) > 0)
}

// Test 4: Span Sampling Strategies
test "span sampling strategies" {
  // Test always-on sampler
  let always_on_sampler = AlwaysOnSampler::new()
  
  for i = 0; i < 10; i = i + 1 {
    let sampling_decision = Sampler::should_sample(always_on_sampler, "trace_" + i.to_string(), "operation")
    assert_eq(sampling_decision.decision, RecordAndSample)
    assert_true(sampling_decision.attributes.is_empty())
  }
  
  // Test always-off sampler
  let always_off_sampler = AlwaysOffSampler::new()
  
  for i = 0; i < 10; i = i + 1 {
    let sampling_decision = Sampler::should_sample(always_off_sampler, "trace_" + i.to_string(), "operation")
    assert_eq(sampling_decision.decision, Drop)
    assert_true(sampling_decision.attributes.is_empty())
  }
  
  // Test trace ID ratio sampler
  let trace_id_ratio_sampler = TraceIdRatioSampler::new(0.5) // 50% sampling
  
  let sampled_count = 0
  let total_count = 1000
  
  for i = 0; i < total_count; i = i + 1 {
    let trace_id = generate_trace_id(i)
    let sampling_decision = Sampler::should_sample(trace_id_ratio_sampler, trace_id, "operation")
    
    if sampling_decision.decision == RecordAndSample {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 50% sampled
  let sampling_ratio = sampled_count.to_float() / total_count.to_float()
  assert_true(sampling_ratio > 0.4 && sampling_ratio < 0.6)
  
  // Test parent-based sampler
  let parent_based_sampler = ParentBasedSampler::new(AlwaysOnSampler::new())
  
  // With no parent, should use delegate sampler (AlwaysOn)
  let no_parent_decision = Sampler::should_sample(parent_based_sampler, "trace", "operation")
  assert_eq(no_parent_decision.decision, RecordAndSample)
  
  // With sampled parent, should sample
  let parent_context = TraceContext::new("trace", "span", 1) // Sampled
  let sampled_parent_decision = Sampler::should_sample_with_parent(parent_based_sampler, "trace", "operation", parent_context)
  assert_eq(sampled_parent_decision.decision, RecordAndSample)
  
  // With unsampled parent, should not sample
  let unsampled_parent_context = TraceContext::new("trace", "span", 0) // Not sampled
  let unsampled_parent_decision = Sampler::should_sample_with_parent(parent_based_sampler, "trace", "operation", unsampled_parent_context)
  assert_eq(unsampled_parent_decision.decision, Drop)
  
  // Test custom sampler
  let custom_sampler = CustomSampler::new(|trace_id, operation| {
    if operation.contains("critical") {
      SamplingDecision::new(RecordAndSample, [])
    } else if operation.contains("debug") {
      SamplingDecision::new(RecordAndSample, [("debug", "true")])
    } else {
      SamplingDecision::new(Drop, [])
    }
  })
  
  let critical_decision = Sampler::should_sample(custom_sampler, "trace", "critical-operation")
  assert_eq(critical_decision.decision, RecordAndSample)
  assert_true(critical_decision.attributes.is_empty())
  
  let debug_decision = Sampler::should_sample(custom_sampler, "trace", "debug-operation")
  assert_eq(debug_decision.decision, RecordAndSample)
  assert_true(debug_decision.attributes.contains("debug"))
  assert_eq(debug_decision.attributes.get("debug"), Some("true"))
  
  let normal_decision = Sampler::should_sample(custom_sampler, "trace", "normal-operation")
  assert_eq(normal_decision.decision, Drop)
}

// Test 5: Span Linking and Causal Relationships
test "span linking and causal relationships" {
  // Test span linking
  let tracer = Tracer::new("link-test-service")
  
  // Create first trace
  let span1 = Tracer::start_span(tracer, "operation-1")
  
  // Create second trace with link to first
  let span2 = Tracer::start_span(tracer, "operation-2")
  
  let link = SpanLink::new(
    Span::context(span1),
    [
      ("link.type", "follows-from"),
      ("link.reason", "causal-relationship")
    ]
  )
  
  Span::add_link(span2, link)
  
  // Verify link
  let links = Span::links(span2)
  assert_eq(links.length(), 1)
  
  let first_link = links[0]
  assert_eq(SpanLink::trace_context(first_link), Span::context(span1))
  assert_true(SpanLink::attributes(first_link).contains("link.type"))
  assert_eq(SpanLink::attributes(first_link).get("link.type"), Some("follows-from"))
  
  // Test multiple links
  let span3 = Tracer::start_span(tracer, "operation-3")
  
  let link1 = SpanLink::new(Span::context(span1), [("link.type", "caused-by")])
  let link2 = SpanLink::new(Span::context(span2), [("link.type", "related-to")])
  
  Span::add_link(span3, link1)
  Span::add_link(span3, link2)
  
  let multiple_links = Span::links(span3)
  assert_eq(multiple_links.length(), 2)
  
  // Test bidirectional linking
  let span4 = Tracer::start_span(tracer, "operation-4")
  let span5 = Tracer::start_span(tracer, "operation-5")
  
  let forward_link = SpanLink::new(Span::context(span4), [("link.type", "precedes")])
  let backward_link = SpanLink::new(Span::context(span5), [("link.type", "follows")])
  
  Span::add_link(span4, forward_link)
  Span::add_link(span5, backward_link)
  
  // Verify bidirectional relationship
  let span4_links = Span::links(span4)
  let span5_links = Span::links(span5)
  
  assert_eq(span4_links.length(), 1)
  assert_eq(span5_links.length(), 1)
  
  assert_eq(SpanLink::trace_context(span4_links[0]), Span::context(span4))
  assert_eq(SpanLink::trace_context(span5_links[0]), Span::context(span5))
  
  // Test linking with trace state
  let trace_state = "rojo=00f067aa0ba902b7"
  let span_with_state = Tracer::start_span_with_trace_state(
    tracer, 
    "operation-with-state", 
    "trace-with-state",
    trace_state
  )
  
  let state_link = SpanLink::new(Span::context(span_with_state), [("link.type", "stateful")])
  Span::add_link(span4, state_link)
  
  let updated_links = Span::links(span4)
  assert_eq(updated_links.length(), 2)
  
  // Verify trace state is preserved in link
  let stateful_link = updated_links.find(|link| {
    match SpanLink::attributes(link).get("link.type") {
      Some(link_type) => link_type == "stateful",
      None => false
    }
  })
  match stateful_link {
    Some(link) => assert_eq(SpanLink::trace_context(link).trace_state(), trace_state),
    None => assert_true(false) // Should find stateful link
  }
}

// Test 6: Span Event and Annotation Handling
test "span event and annotation handling" {
  // Test basic span events
  let tracer = Tracer::new("event-test-service")
  let span = Tracer::start_span(tracer, "operation-with-events")
  
  // Add events with different timestamps
  let base_time = System::current_time_millis()
  
  Span::add_event_with_timestamp(span, "event1", base_time, [
    ("key1", "value1"),
    ("key2", "value2")
  ])
  
  Span::add_event_with_timestamp(span, "event2", base_time + 100, [
    ("key3", "value3")
  ])
  
  Span::add_event_with_timestamp(span, "event3", base_time + 200, [])
  
  // Verify events
  let events = Span::events(span)
  assert_eq(events.length(), 3)
  
  // Verify event timestamps
  assert_eq(Event::timestamp(events[0]), base_time)
  assert_eq(Event::timestamp(events[1]), base_time + 100)
  assert_eq(Event::timestamp(events[2]), base_time + 200)
  
  // Verify event attributes
  let event1_attributes = Event::attributes(events[0])
  assert_eq(event1_attributes.length(), 2)
  assert_eq(event1_attributes.get("key1"), Some("value1"))
  assert_eq(event1_attributes.get("key2"), Some("value2"))
  
  let event2_attributes = Event::attributes(events[1])
  assert_eq(event2_attributes.length(), 1)
  assert_eq(event2_attributes.get("key3"), Some("value3"))
  
  let event3_attributes = Event::attributes(events[2])
  assert_eq(event3_attributes.length(), 0)
  
  // Test event ordering
  let ordered_events = Span::events_ordered_by_time(span)
  assert_eq(ordered_events.length(), 3)
  
  for i = 0; i < ordered_events.length() - 1; i = i + 1 {
    assert_true(Event::timestamp(ordered_events[i]) <= Event::timestamp(ordered_events[i + 1]))
  }
  
  // Test events with special characters
  Span::add_event(span, "special-chars-event", [
    ("unicode", "🚀 test"),
    ("quotes", "\"quoted text\""),
    ("spaces", "value with spaces"),
    ("equals", "key=value in value")
  ])
  
  let special_events = Span::events(span)
  let special_event = special_events.find(|event| Event::name(event) == "special-chars-event")
  match special_event {
    Some(event) => {
      let attributes = Event::attributes(event)
      assert_eq(attributes.get("unicode"), Some("🚀 test"))
      assert_eq(attributes.get("quotes"), Some("\"quoted text\""))
      assert_eq(attributes.get("spaces"), Some("value with spaces"))
      assert_eq(attributes.get("equals"), Some("key=value in value"))
    }
    None => assert_true(false) // Should find special event
  }
  
  // Test large number of events
  for i = 0; i < 100; i = i + 1 {
    Span::add_event(span, "bulk-event-" + i.to_string(), [("index", i.to_string())])
  }
  
  let all_events = Span::events(span)
  assert_eq(all_events.length(), 103) // 3 initial + 100 bulk
  
  // Test event filtering
  let filtered_events = Span::filter_events(span, |event| {
    Event::name(event).contains("bulk-event")
  })
  assert_eq(filtered_events.length(), 100)
}

// Test 7: Span Kind and Semantic Conventions
test "span kind and semantic conventions" {
  // Test different span kinds
  let tracer = Tracer::new("kind-test-service")
  
  // Test internal span
  let internal_span = Tracer::start_span_with_kind(tracer, "internal-operation", Internal)
  assert_eq(Span::kind(internal_span), Internal)
  
  // Test server span
  let server_span = Tracer::start_span_with_kind(tracer, "server-operation", Server)
  assert_eq(Span::kind(server_span), Server)
  
  // Test client span
  let client_span = Tracer::start_span_with_kind(tracer, "client-operation", Client)
  assert_eq(Span::kind(client_span), Client)
  
  // Test producer span
  let producer_span = Tracer::start_span_with_kind(tracer, "producer-operation", Producer)
  assert_eq(Span::kind(producer_span), Producer)
  
  // Test consumer span
  let consumer_span = Tracer::start_span_with_kind(tracer, "consumer-operation", Consumer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Test HTTP semantic conventions
  let http_server_span = Tracer::start_span_with_kind(tracer, "HTTP GET /api/users", Server)
  
  // Set HTTP server attributes
  Span::set_attribute(http_server_span, "http.method", "GET")
  Span::set_attribute(http_server_span, "http.target", "/api/users")
  Span::set_attribute(http_server_span, "http.scheme", "https")
  Span::set_attribute(http_server_span, "http.host", "api.example.com")
  Span::set_attribute(http_server_span, "http.status_code", 200)
  Span::set_attribute(http_server_span, "http.flavor", "1.1")
  Span::set_attribute(http_server_span, "net.host.name", "api.example.com")
  Span::set_attribute(http_server_span, "net.host.port", 443)
  
  // Verify HTTP server attributes
  let http_attributes = Span::attributes(http_server_span)
  assert_eq(http_attributes.get("http.method"), Some("GET"))
  assert_eq(http_attributes.get("http.target"), Some("/api/users"))
  assert_eq(http_attributes.get("http.status_code"), Some(200))
  
  // Test database semantic conventions
  let db_span = Tracer::start_span_with_kind(tracer, "database.query", Client)
  
  // Set database attributes
  Span::set_attribute(db_span, "db.system", "postgresql")
  Span::set_attribute(db_span, "db.name", "mydb")
  Span::set_attribute(db_span, "db.statement", "SELECT * FROM users WHERE id = $1")
  Span::set_attribute(db_span, "db.user", "appuser")
  Span::set_attribute(db_span, "net.peer.name", "db.example.com")
  Span::set_attribute(db_span, "net.peer.port", 5432)
  
  // Verify database attributes
  let db_attributes = Span::attributes(db_span)
  assert_eq(db_attributes.get("db.system"), Some("postgresql"))
  assert_eq(db_attributes.get("db.name"), Some("mydb"))
  assert_eq(db_attributes.get("db.statement"), Some("SELECT * FROM users WHERE id = $1"))
  
  // Test messaging semantic conventions
  let messaging_span = Tracer::start_span_with_kind(tracer, "messaging.publish", Producer)
  
  // Set messaging attributes
  Span::set_attribute(messaging_span, "messaging.system", "kafka")
  Span::set_attribute(messaging_span, "messaging.destination", "user-events")
  Span::set_attribute(messaging_span, "messaging.destination_kind", "topic")
  Span::set_attribute(messaging_span, "messaging.message_id", "msg-12345")
  Span::set_attribute(messaging_span, "messaging.operation", "publish")
  
  // Verify messaging attributes
  let messaging_attributes = Span::attributes(messaging_span)
  assert_eq(messaging_attributes.get("messaging.system"), Some("kafka"))
  assert_eq(messaging_attributes.get("messaging.destination"), Some("user-events"))
  assert_eq(messaging_attributes.get("messaging.operation"), Some("publish"))
  
  // Test RPC semantic conventions
  let rpc_span = Tracer::start_span_with_kind(tracer, "grpc.example.com/Echo/Echo", Client)
  
  // Set RPC attributes
  Span::set_attribute(rpc_span, "rpc.system", "grpc")
  Span::set_attribute(rpc_span, "rpc.service", "example.com.Echo")
  Span::set_attribute(rpc_span, "rpc.method", "Echo")
  Span::set_attribute(rpc_span, "net.peer.name", "grpc.example.com")
  Span::set_attribute(rpc_span, "net.peer.port", 443)
  
  // Verify RPC attributes
  let rpc_attributes = Span::attributes(rpc_span)
  assert_eq(rpc_attributes.get("rpc.system"), Some("grpc"))
  assert_eq(rpc_attributes.get("rpc.service"), Some("example.com.Echo"))
  assert_eq(rpc_attributes.get("rpc.method"), Some("Echo"))
}

// Test 8: Trace Exporting and Collection
test "trace exporting and collection" {
  // Test in-memory span exporter
  let in_memory_exporter = InMemorySpanExporter::new()
  let tracer_provider = TracerProvider::new(in_memory_exporter)
  let tracer = TracerProvider::get_tracer(tracer_provider, "export-test-service")
  
  // Create and end spans
  let span1 = Tracer::start_span(tracer, "operation-1")
  Span::set_attribute(span1, "key1", "value1")
  Span::end(span1)
  
  let span2 = Tracer::start_span(tracer, "operation-2")
  Span::set_attribute(span2, "key2", "value2")
  Span::end(span2)
  
  // Get exported spans
  let exported_spans = InMemorySpanExporter::get_exported_spans(in_memory_exporter)
  assert_eq(exported_spans.length(), 2)
  
  // Verify exported span data
  let exported_span1 = exported_spans[0]
  assert_eq(SpanData::name(exported_span1), "operation-1")
  assert_eq(SpanData::attributes(exported_span1).get("key1"), Some("value1"))
  
  let exported_span2 = exported_spans[1]
  assert_eq(SpanData::name(exported_span2), "operation-2")
  assert_eq(SpanData::attributes(exported_span2).get("key2"), Some("value2"))
  
  // Test batch span exporter
  let batch_exporter = BatchSpanExporter::new(5, 1000) // Batch size 5, flush timeout 1s
  let batch_tracer_provider = TracerProvider::new(batch_exporter)
  let batch_tracer = TracerProvider::get_tracer(batch_tracer_provider, "batch-test-service")
  
  // Create spans
  for i = 0; i < 3; i = i + 1 {
    let span = Tracer::start_span(batch_tracer, "batch-operation-" + i.to_string())
    Span::set_attribute(span, "index", i.to_string())
    Span::end(span)
  }
  
  // Force flush
  BatchSpanExporter::flush(batch_exporter)
  
  // Verify batch export
  let batch_exported_spans = BatchSpanExporter::get_exported_spans(batch_exporter)
  assert_eq(batch_exported_spans.length(), 3)
  
  // Test exporting with sampling
  let sampler = TraceIdRatioSampler::new(0.5)
  let sampled_tracer_provider = TracerProvider::new_with_sampler(in_memory_exporter, sampler)
  let sampled_tracer = TracerProvider::get_tracer(sampled_tracer_provider, "sampling-test-service")
  
  // Create many spans
  let mut sampled_count = 0
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(sampled_tracer, "sampled-operation-" + i.to_string())
    if Span::is_recording(span) {
      sampled_count = sampled_count + 1
    }
    Span::end(span)
  }
  
  // Check exported spans
  let sampled_exported_spans = InMemorySpanExporter::get_exported_spans(in_memory_exporter)
  
  // Should have approximately 50% of spans exported
  let export_ratio = sampled_exported_spans.length().to_float() / 100.0
  assert_true(export_ratio > 0.3 && export_ratio < 0.7)
  
  // Test exporting with transformation
  let transforming_exporter = TransformingSpanExporter::new(|span| {
    // Add export timestamp
    let transformed = SpanData::clone(span)
    SpanData::set_attribute(transformed, "export.timestamp", System::current_time_millis().to_string())
    transformed
  })
  
  let transforming_tracer_provider = TracerProvider::new(transforming_exporter)
  let transforming_tracer = TracerProvider::get_tracer(transforming_tracer_provider, "transform-test-service")
  
  let transform_span = Tracer::start_span(transforming_tracer, "transform-operation")
  Span::end(transform_span)
  
  let transformed_spans = TransformingSpanExporter::get_exported_spans(transforming_exporter)
  assert_eq(transformed_spans.length(), 1)
  
  let transformed_span = transformed_spans[0]
  assert_true(SpanData::attributes(transformed_span).contains("export.timestamp"))
}

// Test 9: Trace Context Propagation Edge Cases
test "trace context propagation edge cases" {
  // Test malformed traceparent header
  let malformed_headers = TextMapCarrier::new()
  TextMapCarrier::set(malformed_headers, "traceparent", "malformed-header")
  
  let extractor = TraceContextPropagator::new()
  let malformed_result = TraceContextPropagator::extract(extractor, malformed_headers)
  
  match malformed_result {
    Some(_) => assert_true(false), // Should not extract from malformed header
    None => assert_true(true) // Expected case
  }
  
  // Test missing traceparent header
  let missing_headers = TextMapCarrier::new()
  let missing_result = TraceContextPropagator::extract(extractor, missing_headers)
  
  match missing_result {
    Some(_) => assert_true(false), // Should not extract from missing header
    None => assert_true(true) // Expected case
  }
  
  // Test invalid trace ID in header
  let invalid_trace_id_headers = TextMapCarrier::new()
  TextMapCarrier::set(invalid_trace_id_headers, "traceparent", "00-invalid-trace-id-1234567890123456-1234567890123456-01")
  
  let invalid_trace_id_result = TraceContextPropagator::extract(extractor, invalid_trace_id_headers)
  
  match invalid_trace_id_result {
    Some(_) => assert_true(false), // Should not extract with invalid trace ID
    None => assert_true(true) // Expected case
  }
  
  // Test baggage with special characters
  let special_baggage_headers = TextMapCarrier::new()
  TextMapCarrier::set(special_baggage_headers, "baggage", "key1=value1,key2=value with spaces,key3=value=with=equals,key4=value,with,commas")
  
  let special_baggage_result = TraceContextPropagator::extract(extractor, special_baggage_headers)
  
  match special_baggage_result {
    Some(context) => {
      let baggage = TraceContext::baggage(context)
      assert_eq(baggage.length(), 4)
      
      let value1 = TraceContext::get_baggage_item(context, "key1")
      assert_eq(value1, Some("value1"))
      
      let value2 = TraceContext::get_baggage_item(context, "key2")
      assert_eq(value2, Some("value with spaces"))
      
      let value3 = TraceContext::get_baggage_item(context, "key3")
      assert_eq(value3, Some("value=with=equals"))
      
      let value4 = TraceContext::get_baggage_item(context, "key4")
      assert_eq(value4, Some("value,with,commas"))
    }
    None => assert_true(false) // Should extract baggage
  }
  
  // Test very long baggage header
  let long_baggage_headers = TextMapCarrier::new()
  let long_baggage_value = "key1=" + "a".repeat(1000) + ",key2=" + "b".repeat(1000)
  TextMapCarrier::set(long_baggage_headers, "baggage", long_baggage_value)
  
  let long_baggage_result = TraceContextPropagator::extract(extractor, long_baggage_headers)
  
  match long_baggage_result {
    Some(context) => {
      let baggage = TraceContext::baggage(context)
      assert_eq(baggage.length(), 2)
      
      let value1 = TraceContext::get_baggage_item(context, "key1")
      match value1 {
        Some(v) => assert_eq(v.length(), 1000),
        None => assert_true(false) // Should find key1
      }
      
      let value2 = TraceContext::get_baggage_item(context, "key2")
      match value2 {
        Some(v) => assert_eq(v.length(), 1000),
        None => assert_true(false) // Should find key2
      }
    }
    None => assert_true(false) // Should extract baggage
  }
  
  // Test context propagation with multiple propagators
  let composite_propagator = CompositePropagator::new([
    TraceContextPropagator::new(),
    BaggagePropagator::new()
  ])
  
  let multi_headers = TextMapCarrier::new()
  let trace_context = TraceContext::new("trace123", "span456", 1)
  TraceContext::set_baggage_item(trace_context, "user.id", "12345")
  
  CompositePropagator::inject(composite_propagator, trace_context, multi_headers)
  
  // Verify both headers are injected
  let traceparent = TextMapCarrier::get(multi_headers, "traceparent")
  let baggage = TextMapCarrier::get(multi_headers, "baggage")
  
  match (traceparent, baggage) {
    (Some(tp), Some(bg)) => {
      assert_true(tp.contains("trace123"))
      assert_true(bg.contains("user.id=12345"))
    }
    _ => assert_true(false) // Should have both headers
  }
  
  // Extract with composite propagator
  let extracted_multi = CompositePropagator::extract(composite_propagator, multi_headers)
  
  match extracted_multi {
    Some(context) => {
      assert_eq(TraceContext::trace_id(context), "trace123")
      assert_eq(TraceContext::span_id(context), "span456")
      assert_eq(TraceContext::get_baggage_item(context, "user.id"), Some("12345"))
    }
    None => assert_true(false) // Should extract context
  }
}

// Test 10: Performance and Scalability of Distributed Tracing
test "performance and scalability of distributed tracing" {
  // Test span creation performance
  let tracer = Tracer::new("performance-test-service")
  
  let start_time = PerformanceCounter::now()
  
  // Create many spans
  for i = 0; i < 10000; i = i + 1 {
    let span = Tracer::start_span(tracer, "operation-" + i.to_string())
    Span::set_attribute(span, "index", i.to_string())
    Span::add_event(span, "event-" + i.to_string())
    Span::end(span)
  }
  
  let end_time = PerformanceCounter::now()
  let duration = PerformanceCounter::duration_ms(start_time, end_time)
  
  // Should complete within reasonable time
  assert_true(duration < 5000.0) // 5 seconds
  
  // Test trace context propagation performance
  let propagator = TraceContextPropagator::new()
  let trace_context = TraceContext::new("trace123", "span456", 1)
  
  let propagation_start = PerformanceCounter::now()
  
  for i = 0; i < 10000; i = i + 1 {
    let headers = TextMapCarrier::new()
    TraceContextPropagator::inject(propagator, trace_context, headers)
    
    let extracted = TraceContextPropagator::extract(propagator, headers)
    match extracted {
      Some(_) => assert_true(true), // Should extract successfully
      None => assert_true(false) // Should not fail
    }
  }
  
  let propagation_end = PerformanceCounter::now()
  let propagation_duration = PerformanceCounter::duration_ms(propagation_start, propagation_end)
  
  // Should complete within reasonable time
  assert_true(propagation_duration < 3000.0) // 3 seconds
  
  // Test memory usage with many spans
  let memory_tracer = Tracer::new("memory-test-service")
  let initial_memory = System::memory_usage()
  
  // Create many spans without ending them
  let active_spans = []
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(memory_tracer, "active-operation-" + i.to_string())
    Span::set_attribute(span, "data", "x".repeat(100)) // 100 bytes per span
    active_spans.push(span)
  }
  
  let peak_memory = System::memory_usage()
  let memory_increase = peak_memory - initial_memory
  
  // Memory usage should be reasonable
  assert_true(memory_increase < 10000000) // Less than 10MB for 1000 spans
  
  // End all spans
  for span in active_spans {
    Span::end(span)
  }
  
  // Force garbage collection
  System::gc()
  Thread::sleep(100)
  
  let final_memory = System::memory_usage()
  let memory_after_gc = final_memory - initial_memory
  
  // Memory should be reclaimed after GC
  assert_true(memory_after_gc < memory_increase / 2)
  
  // Test concurrent span creation
  let thread_pool = ThreadPool::new(4)
  
  let concurrent_start = PerformanceCounter::now()
  
  let futures = []
  for i = 0; i < 4; i = i + 1 {
    let thread_id = i
    let task = || {
      let local_tracer = Tracer::new("concurrent-test-service-" + thread_id.to_string())
      
      for j = 0; j < 2500; j = j + 1 {
        let span = Tracer::start_span(local_tracer, "concurrent-operation-" + thread_id.to_string() + "-" + j.to_string())
        Span::set_attribute(span, "thread", thread_id.to_string())
        Span::set_attribute(span, "index", j.to_string())
        Span::end(span)
      }
      
      thread_id
    }
    
    let future = ThreadPool::submit(thread_pool, task)
    futures.push(future)
  }
  
  // Wait for all threads to complete
  for future in futures {
    Future::get(future)
  }
  
  let concurrent_end = PerformanceCounter::now()
  let concurrent_duration = PerformanceCounter::duration_ms(concurrent_start, concurrent_end)
  
  // Concurrent execution should be faster
  assert_true(concurrent_duration < duration * 1.5)
  
  // Test large trace with many spans
  let large_tracer = Tracer::new("large-trace-test-service")
  let large_trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  let large_start = PerformanceCounter::now()
  
  // Create a trace with many spans
  let root_span = Tracer::start_span_with_trace_id(large_tracer, "root-operation", large_trace_id)
  
  for i = 0; i < 1000; i = i + 1 {
    let child_span = Tracer::start_span_with_parent(large_tracer, "child-operation-" + i.to_string(), root_span)
    
    // Add grandchild spans
    for j = 0; j < 5; j = j + 1 {
      let grandchild_span = Tracer::start_span_with_parent(large_tracer, "grandchild-operation-" + i.to_string() + "-" + j.to_string(), child_span)
      Span::set_attribute(grandchild_span, "depth", "2")
      Span::end(grandchild_span)
    }
    
    Span::set_attribute(child_span, "depth", "1")
    Span::end(child_span)
  }
  
  Span::end(root_span)
  
  let large_end = PerformanceCounter::now()
  let large_duration = PerformanceCounter::duration_ms(large_start, large_end)
  
  // Should complete within reasonable time
  assert_true(large_duration < 10000.0) // 10 seconds
}

// Helper functions
func generate_trace_id(seed : Int) -> String {
  // Generate deterministic trace ID based on seed
  let hex_chars = "0123456789abcdef"
  let mut result = ""
  
  for i = 0; i < 32; i = i + 1 {
    let index = (seed + i) % 16
    result = result + hex_chars[index].to_string()
  }
  
  result
}

// Mock implementations for testing
type TraceContext
type TextMapCarrier
type TraceContextPropagator
type BaggagePropagator
type CompositePropagator
type Tracer
type Span
type SpanLink
type Event
type Sampler
type SamplingDecision
type AlwaysOnSampler
type AlwaysOffSampler
type TraceIdRatioSampler
type ParentBasedSampler
type CustomSampler
type TracerProvider
type InMemorySpanExporter
type BatchSpanExporter
type TransformingSpanExporter
type SpanData
type ThreadPool
type Future
type PerformanceCounter
type System

// Trace context
func TraceContext::new(trace_id : String, span_id : String, trace_flags : Int) -> TraceContext { /* implementation */ }
func TraceContext::with_baggage(context : TraceContext, items : Array[(String, String)]) -> TraceContext { /* implementation */ }
func TraceContext::trace_id(context : TraceContext) -> String { "" }
func TraceContext::span_id(context : TraceContext) -> String { "" }
func TraceContext::trace_flags(context : TraceContext) -> Int { 0 }
func TraceContext::is_sampled(context : TraceContext) -> Bool { false }
func TraceContext::baggage(context : TraceContext) -> Array[(String, String)] { [] }
func TraceContext::get_baggage_item(context : TraceContext, key : String) -> String { "" }
func TraceContext::set_baggage_item(context : TraceContext, key : String, value : String) -> Unit { /* implementation */ }
func TraceContext::trace_state(context : TraceContext) -> String { "" }

// Text map carrier
func TextMapCarrier::new() -> TextMapCarrier { /* implementation */ }
func TextMapCarrier::set(carrier : TextMapCarrier, key : String, value : String) -> Unit { /* implementation */ }
func TextMapCarrier::get(carrier : TextMapCarrier, key : String) -> Option[String] { Some("") }

// Trace context propagator
func TraceContextPropagator::new() -> TraceContextPropagator { /* implementation */ }
func TraceContextPropagator::inject(propagator : TraceContextPropagator, context : TraceContext, carrier : TextMapCarrier) -> Unit { /* implementation */ }
func TraceContextPropagator::extract(propagator : TraceContextPropagator, carrier : TextMapCarrier) -> Option[TraceContext] { Some(/* context */) }

// Baggage propagator
func BaggagePropagator::new() -> BaggagePropagator { /* implementation */ }
func BaggagePropagator::inject(propagator : BaggagePropagator, context : TraceContext, carrier : TextMapCarrier) -> Unit { /* implementation */ }
func BaggagePropagator::extract(propagator : BaggagePropagator, carrier : TextMapCarrier) -> Option[TraceContext] { Some(/* context */) }

// Composite propagator
func CompositePropagator::new(propagators : Array[Any]) -> CompositePropagator { /* implementation */ }
func CompositePropagator::inject(propagator : CompositePropagator, context : TraceContext, carrier : TextMapCarrier) -> Unit { /* implementation */ }
func CompositePropagator::extract(propagator : CompositePropagator, carrier : TextMapCarrier) -> Option[TraceContext] { Some(/* context */) }

// Tracer
func Tracer::new(service_name : String) -> Tracer { /* implementation */ }
func Tracer::start_span(tracer : Tracer, name : String) -> Span { /* implementation */ }
func Tracer::start_span_with_trace_id(tracer : Tracer, name : String, trace_id : String) -> Span { /* implementation */ }
func Tracer::start_span_with_parent(tracer : Tracer, name : String, parent : Span) -> Span { /* implementation */ }
func Tracer::start_span_with_context(tracer : Tracer, name : String, context : TraceContext) -> Span { /* implementation */ }
func Tracer::start_span_with_kind(tracer : Tracer, name : String, kind : SpanKind) -> Span { /* implementation */ }
func Tracer::start_span_with_timestamp(tracer : Tracer, name : String, timestamp : Int64) -> Span { /* implementation */ }
func Tracer::start_span_with_trace_state(tracer : Tracer, name : String, trace_id : String, trace_state : String) -> Span { /* implementation */ }

// Span
func Span::name(span : Span) -> String { "" }
func Span::kind(span : Span) -> SpanKind { Internal }
func Span::trace_id(span : Span) -> String { "" }
func Span::span_id(span : Span) -> String { "" }
func Span::parent_span_id(span : Span) -> Option[String] { Some("") }
func Span::context(span : Span) -> TraceContext { /* implementation */ }
func Span::start_time(span : Span) -> Int64 { 0L }
func Span::end_time(span : Span) -> Int64 { 0L }
func Span::is_recording(span : Span) -> Bool { false }
func Span::set_attribute(span : Span, key : String, value : String) -> Unit { /* implementation */ }
func Span::set_attribute(span : Span, key : String, value : Int) -> Unit { /* implementation */ }
func Span::attributes(span : Span) -> Attributes { /* implementation */ }
func Span::add_event(span : Span, name : String) -> Unit { /* implementation */ }
func Span::add_event(span : Span, name : String, attributes : Array[(String, String)]) -> Unit { /* implementation */ }
func Span::add_event_with_timestamp(span : Span, name : String, timestamp : Int64, attributes : Array[(String, String)]) -> Unit { /* implementation */ }
func Span::events(span : Span) -> Array[Event] { [] }
func Span::events_ordered_by_time(span : Span) -> Array[Event] { [] }
func Span::filter_events(span : Span, filter_fn : Event -> Bool) -> Array[Event] { [] }
func Span::add_link(span : Span, link : SpanLink) -> Unit { /* implementation */ }
func Span::links(span : Span) -> Array[SpanLink] { [] }
func Span::set_status(span : Span, code : StatusCode, message : Option[String]) -> Unit { /* implementation */ }
func Span::status(span : Span) -> SpanStatus { /* implementation */ }
func Span::end(span : Span) -> Unit { /* implementation */ }
func Span::end_with_timestamp(span : Span, timestamp : Int64) -> Unit { /* implementation */ }
func Span::duration(span : Span) -> Int64 { 0L }

// Span link
func SpanLink::new(context : TraceContext, attributes : Array[(String, String)]) -> SpanLink { /* implementation */ }
func SpanLink::trace_context(link : SpanLink) -> TraceContext { /* implementation */ }
func SpanLink::attributes(link : SpanLink) -> Attributes { /* implementation */ }

// Event
func Event::name(event : Event) -> String { "" }
func Event::timestamp(event : Event) -> Int64 { 0L }
func Event::attributes(event : Event) -> Attributes { /* implementation */ }

// Sampler
func Sampler::should_sample(sampler : Sampler, trace_id : String, name : String) -> SamplingDecision { /* implementation */ }
func Sampler::should_sample_with_parent(sampler : Sampler, trace_id : String, name : String, parent_context : TraceContext) -> SamplingDecision { /* implementation */ }

// Sampling decision
func SamplingDecision::new(decision : SamplingResult, attributes : Array[(String, String)]) -> SamplingDecision { /* implementation */ }

// Samplers
func AlwaysOnSampler::new() -> AlwaysOnSampler { /* implementation */ }
func AlwaysOffSampler::new() -> AlwaysOffSampler { /* implementation */ }
func TraceIdRatioSampler::new(ratio : Float) -> TraceIdRatioSampler { /* implementation */ }
func ParentBasedSampler::new(delegate : Sampler) -> ParentBasedSampler { /* implementation */ }
func CustomSampler::new(sampling_fn : (String, String) -> SamplingDecision) -> CustomSampler { /* implementation */ }

// Tracer provider
func TracerProvider::new(exporter : SpanExporter) -> TracerProvider { /* implementation */ }
func TracerProvider::new_with_sampler(exporter : SpanExporter, sampler : Sampler) -> TracerProvider { /* implementation */ }
func TracerProvider::get_tracer(provider : TracerProvider, service_name : String) -> Tracer { /* implementation */ }

// Span exporters
func InMemorySpanExporter::new() -> InMemorySpanExporter { /* implementation */ }
func InMemorySpanExporter::get_exported_spans(exporter : InMemorySpanExporter) -> Array[SpanData] { [] }

func BatchSpanExporter::new(batch_size : Int, timeout_ms : Int) -> BatchSpanExporter { /* implementation */ }
func BatchSpanExporter::flush(exporter : BatchSpanExporter) -> Unit { /* implementation */ }
func BatchSpanExporter::get_exported_spans(exporter : BatchSpanExporter) -> Array[SpanData] { [] }

func TransformingSpanExporter::new(transform_fn : SpanData -> SpanData) -> TransformingSpanExporter { /* implementation */ }
func TransformingSpanExporter::get_exported_spans(exporter : TransformingSpanExporter) -> Array[SpanData] { [] }

// Span data
func SpanData::name(data : SpanData) -> String { "" }
func SpanData::attributes(data : SpanData) -> Attributes { /* implementation */ }
func SpanData::clone(data : SpanData) -> SpanData { /* implementation */ }
func SpanData::set_attribute(data : SpanData, key : String, value : String) -> Unit { /* implementation */ }

// Thread pool
func ThreadPool::new(size : Int) -> ThreadPool { /* implementation */ }
func ThreadPool::submit[T](pool : ThreadPool, task : () -> T) -> Future[T] { /* implementation */ }

// Future
func Future::get[T](future : Future[T]) -> T { /* implementation */ }

// Performance counter
func PerformanceCounter::now() -> PerformanceCounter { /* implementation */ }
func PerformanceCounter::duration_ms(start : PerformanceCounter, end : PerformanceCounter) -> Float { 0.0 }

// System utilities
func System::current_time_millis() -> Int64 { 0L }
func System::memory_usage() -> Int { 0 }
func System::gc() -> Unit { /* implementation */ }

// Thread utilities
func Thread::sleep(ms : Int) -> Unit { /* implementation */ }

// Types
type SpanKind
type StatusCode
type SpanStatus
type SpanExporter
type Attributes
type SamplingResult

// Enums
enum SpanKind { Internal, Server, Client, Producer, Consumer }
enum StatusCode { Unset, Ok, Error }
enum SamplingResult { Drop, RecordOnly, RecordAndSample }

// Attributes implementation
type Attributes {
  // Implementation details would go here
}

func Attributes::new() -> Attributes { /* implementation */ }
func Attributes::contains(attributes : Attributes, key : String) -> Bool { false }
func Attributes::get(attributes : Attributes, key : String) -> Option[String] { Some("") }
func Attributes::length(attributes : Attributes) -> Int { 0 }

// Span status implementation
type SpanStatus {
  status_code : StatusCode,
  message : Option[String]
}