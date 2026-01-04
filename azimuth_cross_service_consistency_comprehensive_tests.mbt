// Azimuth Telemetry System - Cross-Service Consistency Tests
// This file contains comprehensive cross-service consistency test cases for the telemetry system

// Test 1: Trace Context Propagation Across Services
test "trace context propagation across services" {
  // Simulate Service A - Entry point
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  
  let root_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  match root_span {
    Some(span) => {
      // Add service A specific attributes
      Span::set_attribute(span, "service.name", StringValue("service_a"))
      Span::set_attribute(span, "service.version", StringValue("1.0.0"))
      Span::set_attribute(span, "service.instance.id", StringValue("instance-a-123"))
      
      // Get context for propagation
      let ctx = Span::span_context(span)
      let trace_id = SpanContext::trace_id(ctx)
      let span_id = SpanContext::span_id(ctx)
      
      // Simulate Service B - Called by Service A
      let service_b_provider = TracerProvider::default()
      let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
      
      let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", Some(ctx))
      match service_b_span {
        Some(b_span) => {
          // Add service B specific attributes
          Span::set_attribute(b_span, "service.name", StringValue("service_b"))
          Span::set_attribute(b_span, "service.version", StringValue("2.1.0"))
          Span::set_attribute(b_span, "service.instance.id", StringValue("instance-b-456"))
          
          // Verify trace context is preserved
          let b_ctx = Span::span_context(b_span)
          assert_eq(SpanContext::trace_id(b_ctx), trace_id, "Trace ID should be preserved across services")
          assert_ne(SpanContext::span_id(b_ctx), span_id, "Span ID should be different for each service")
          assert_eq(SpanContext::is_sampled(b_ctx), SpanContext::is_sampled(ctx), "Sampling decision should be preserved")
          
          // Simulate Service C - Called by Service B
          let service_c_provider = TracerProvider::default()
          let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
          
          let service_c_span = Tracer::start_span_with_context(service_c_tracer, "service_c_operation", Some(b_ctx))
          match service_c_span {
            Some(c_span) => {
              // Add service C specific attributes
              Span::set_attribute(c_span, "service.name", StringValue("service_c"))
              Span::set_attribute(c_span, "service.version", StringValue("3.2.1"))
              Span::set_attribute(c_span, "service.instance.id", StringValue("instance-c-789"))
              
              // Verify trace context is preserved
              let c_ctx = Span::span_context(c_span)
              assert_eq(SpanContext::trace_id(c_ctx), trace_id, "Trace ID should be preserved across all services")
              assert_ne(SpanContext::span_id(c_ctx), span_id, "Span ID should be unique for each service")
              assert_ne(SpanContext::span_id(c_ctx), SpanContext::span_id(b_ctx), "Span ID should be unique for each service")
              assert_eq(SpanContext::is_sampled(c_ctx), SpanContext::is_sampled(ctx), "Sampling decision should be preserved")
              
              Span::end(c_span)
            }
            None => assert_true(false, "Service C span should be created")
          }
          
          Span::end(b_span)
        }
        None => assert_true(false, "Service B span should be created")
      }
      
      Span::end(span)
    }
    None => assert_true(false, "Service A span should be created")
  }
}

// Test 2: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Create initial baggage in Service A
  let baggage = Baggage::new()
  let baggage_a = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_ab = Baggage::set_entry(baggage_a, "request.id", "req-456")
  
  // Service A adds its own baggage entry
  let baggage_ab_service_a = Baggage::set_entry(baggage_ab, "service.a.entry", "service-a-value")
  
  // Propagate to Service B
  let baggage_b = Baggage::set_entry(baggage_ab_service_a, "service.b.entry", "service-b-value")
  
  // Verify Service A baggage entries are preserved
  match Baggage::get_entry(baggage_b, "user.id") {
    Some(value) => assert_eq(value, "user-123"),
    None => assert_true(false, "user.id should be preserved in Service B")
  }
  
  match Baggage::get_entry(baggage_b, "request.id") {
    Some(value) => assert_eq(value, "req-456"),
    None => assert_true(false, "request.id should be preserved in Service B")
  }
  
  match Baggage::get_entry(baggage_b, "service.a.entry") {
    Some(value) => assert_eq(value, "service-a-value"),
    None => assert_true(false, "service.a.entry should be preserved in Service B")
  }
  
  // Verify Service B baggage entry is added
  match Baggage::get_entry(baggage_b, "service.b.entry") {
    Some(value) => assert_eq(value, "service-b-value"),
    None => assert_true(false, "service.b.entry should be available in Service B")
  }
  
  // Propagate to Service C
  let baggage_c = Baggage::set_entry(baggage_b, "service.c.entry", "service-c-value")
  
  // Verify all previous baggage entries are preserved
  match Baggage::get_entry(baggage_c, "user.id") {
    Some(value) => assert_eq(value, "user-123"),
    None => assert_true(false, "user.id should be preserved in Service C")
  }
  
  match Baggage::get_entry(baggage_c, "request.id") {
    Some(value) => assert_eq(value, "req-456"),
    None => assert_true(false, "request.id should be preserved in Service C")
  }
  
  match Baggage::get_entry(baggage_c, "service.a.entry") {
    Some(value) => assert_eq(value, "service-a-value"),
    None => assert_true(false, "service.a.entry should be preserved in Service C")
  }
  
  match Baggage::get_entry(baggage_c, "service.b.entry") {
    Some(value) => assert_eq(value, "service-b-value"),
    None => assert_true(false, "service.b.entry should be preserved in Service C")
  }
  
  // Verify Service C baggage entry is added
  match Baggage::get_entry(baggage_c, "service.c.entry") {
    Some(value) => assert_eq(value, "service-c-value"),
    None => assert_true(false, "service.c.entry should be available in Service C")
  }
}

// Test 3: Cross-Service Metrics Consistency
test "cross-service metrics consistency" {
  // Create metrics providers for different services
  let service_a_meter_provider = MeterProvider::default()
  let service_a_meter = MeterProvider::get_meter(service_a_meter_provider, "service_a")
  
  let service_b_meter_provider = MeterProvider::default()
  let service_b_meter = MeterProvider::get_meter(service_b_meter_provider, "service_b")
  
  let service_c_meter_provider = MeterProvider::default()
  let service_c_meter = MeterProvider::get_meter(service_c_meter_provider, "service_c")
  
  // Create consistent metrics across services
  let service_a_counter = Meter::create_counter(service_a_meter, "cross_service_requests", Some("Total cross-service requests"), Some("count"))
  let service_b_counter = Meter::create_counter(service_b_meter, "cross_service_requests", Some("Total cross-service requests"), Some("count"))
  let service_c_counter = Meter::create_counter(service_c_meter, "cross_service_requests", Some("Total cross-service requests"), Some("count"))
  
  // Create consistent histograms across services
  let service_a_histogram = Meter::create_histogram(service_a_meter, "request_duration", Some("Request duration"), Some("ms"))
  let service_b_histogram = Meter::create_histogram(service_b_meter, "request_duration", Some("Request duration"), Some("ms"))
  let service_c_histogram = Meter::create_histogram(service_c_meter, "request_duration", Some("Request duration"), Some("ms"))
  
  // Verify metrics have consistent names and descriptions
  match service_a_counter {
    Some(counter) => {
      let data = Counter::get_data(counter)
      assert_eq(data.name, "cross_service_requests")
      assert_eq(data.description, Some("Total cross-service requests"))
      assert_eq(data.unit, Some("count"))
    }
    None => assert_true(false, "Service A counter should be created")
  }
  
  match service_b_counter {
    Some(counter) => {
      let data = Counter::get_data(counter)
      assert_eq(data.name, "cross_service_requests")
      assert_eq(data.description, Some("Total cross-service requests"))
      assert_eq(data.unit, Some("count"))
    }
    None => assert_true(false, "Service B counter should be created")
  }
  
  match service_c_counter {
    Some(counter) => {
      let data = Counter::get_data(counter)
      assert_eq(data.name, "cross_service_requests")
      assert_eq(data.description, Some("Total cross-service requests"))
      assert_eq(data.unit, Some("count"))
    }
    None => assert_true(false, "Service C counter should be created")
  }
  
  // Verify histograms have consistent names and descriptions
  match service_a_histogram {
    Some(histogram) => {
      let data = Histogram::get_data(histogram)
      assert_eq(data.name, "request_duration")
      assert_eq(data.description, Some("Request duration"))
      assert_eq(data.unit, Some("ms"))
    }
    None => assert_true(false, "Service A histogram should be created")
  }
  
  match service_b_histogram {
    Some(histogram) => {
      let data = Histogram::get_data(histogram)
      assert_eq(data.name, "request_duration")
      assert_eq(data.description, Some("Request duration"))
      assert_eq(data.unit, Some("ms"))
    }
    None => assert_true(false, "Service B histogram should be created")
  }
  
  match service_c_histogram {
    Some(histogram) => {
      let data = Histogram::get_data(histogram)
      assert_eq(data.name, "request_duration")
      assert_eq(data.description, Some("Request duration"))
      assert_eq(data.unit, Some("ms"))
    }
    None => assert_true(false, "Service C histogram should be created")
  }
}

// Test 4: Cross-Service Logging Consistency
test "cross-service logging consistency" {
  // Create logger providers for different services
  let service_a_logger_provider = LoggerProvider::default()
  let service_a_logger = LoggerProvider::get_logger(service_a_logger_provider, "service_a")
  
  let service_b_logger_provider = LoggerProvider::default()
  let service_b_logger = LoggerProvider::get_logger(service_b_logger_provider, "service_b")
  
  let service_c_logger_provider = LoggerProvider::default()
  let service_c_logger = LoggerProvider::get_logger(service_c_logger_provider, "service_c")
  
  // Create consistent log records across services
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    Some([
      ("service.name", StringValue("service_a")),
      ("service.version", StringValue("1.0.0")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue(span_id))
    ]),
    Some(get_current_timestamp()),
    None,
    Some(trace_id),
    Some(span_id),
    None
  )
  
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B processing request"),
    Some([
      ("service.name", StringValue("service_b")),
      ("service.version", StringValue("2.1.0")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue("b7ad6b7169203332"))
    ]),
    Some(get_current_timestamp()),
    None,
    Some(trace_id),
    Some("b7ad6b7169203332"),
    None
  )
  
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Service C processing request"),
    Some([
      ("service.name", StringValue("service_c")),
      ("service.version", StringValue("3.2.1")),
      ("trace.id", StringValue(trace_id)),
      ("span.id", StringValue("b7ad6b7169203333"))
    ]),
    Some(get_current_timestamp()),
    None,
    Some(trace_id),
    Some("b7ad6b7169203333"),
    None
  )
  
  // Verify trace ID is consistent across services
  assert_eq(LogRecord::trace_id(service_a_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service_b_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service_c_log), Some(trace_id))
  
  // Verify span IDs are different for each service
  assert_eq(LogRecord::span_id(service_a_log), Some(span_id))
  assert_eq(LogRecord::span_id(service_b_log), Some("b7ad6b7169203332"))
  assert_eq(LogRecord::span_id(service_c_log), Some("b7ad6b7169203333"))
  
  // Emit log records
  Logger::emit(service_a_logger, service_a_log)
  Logger::emit(service_b_logger, service_b_log)
  Logger::emit(service_c_logger, service_c_log)
}

// Test 5: Resource Consistency Across Services
test "resource consistency across services" {
  // Create resources for different services
  let service_a_resource = Resource::new()
  let service_b_resource = Resource::new()
  let service_c_resource = Resource::new()
  
  // Add service-specific attributes
  let service_a_attrs = [
    ("service.name", StringValue("service_a")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-a-123")),
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod"))
  ]
  
  let service_b_attrs = [
    ("service.name", StringValue("service_b")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-b-456")),
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod"))
  ]
  
  let service_c_attrs = [
    ("service.name", StringValue("service_c")),
    ("service.version", StringValue("3.2.1")),
    ("service.instance.id", StringValue("instance-c-789")),
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod"))
  ]
  
  let service_a_with_attrs = Resource::with_attributes(service_a_resource, service_a_attrs)
  let service_b_with_attrs = Resource::with_attributes(service_b_resource, service_b_attrs)
  let service_c_with_attrs = Resource::with_attributes(service_c_resource, service_c_attrs)
  
  // Verify consistent attributes across services
  let service_a_namespace = Resource::get_attribute(service_a_with_attrs, "service.namespace")
  let service_b_namespace = Resource::get_attribute(service_b_with_attrs, "service.namespace")
  let service_c_namespace = Resource::get_attribute(service_c_with_attrs, "service.namespace")
  
  match service_a_namespace {
    Some(StringValue(ns)) => assert_eq(ns, "production"),
    _ => assert_true(false, "Service A namespace should be production")
  }
  
  match service_b_namespace {
    Some(StringValue(ns)) => assert_eq(ns, "production"),
    _ => assert_true(false, "Service B namespace should be production")
  }
  
  match service_c_namespace {
    Some(StringValue(ns)) => assert_eq(ns, "production"),
    _ => assert_true(false, "Service C namespace should be production")
  }
  
  let service_a_env = Resource::get_attribute(service_a_with_attrs, "deployment.environment")
  let service_b_env = Resource::get_attribute(service_b_with_attrs, "deployment.environment")
  let service_c_env = Resource::get_attribute(service_c_with_attrs, "deployment.environment")
  
  match service_a_env {
    Some(StringValue(env)) => assert_eq(env, "prod"),
    _ => assert_true(false, "Service A environment should be prod")
  }
  
  match service_b_env {
    Some(StringValue(env)) => assert_eq(env, "prod"),
    _ => assert_true(false, "Service B environment should be prod")
  }
  
  match service_c_env {
    Some(StringValue(env)) => assert_eq(env, "prod"),
    _ => assert_true(false, "Service C environment should be prod")
  }
}

// Test 6: Cross-Service Error Propagation
test "cross-service error propagation" {
  // Service A initiates operation
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  
  let root_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  match root_span {
    Some(span) => {
      // Service A calls Service B
      let ctx = Span::span_context(span)
      let service_b_provider = TracerProvider::default()
      let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
      
      let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", Some(ctx))
      match service_b_span {
        Some(b_span) => {
          // Service B encounters error
          Span::set_status(b_span, Error, Some("Service B encountered an error"))
          Span::add_event(b_span, "exception", Some([
            ("exception.type", StringValue("ServiceException")),
            ("exception.message", StringValue("Service B failed to process request"))
          ]))
          
          // Service B calls Service C
          let b_ctx = Span::span_context(b_span)
          let service_c_provider = TracerProvider::default()
          let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
          
          let service_c_span = Tracer::start_span_with_context(service_c_tracer, "service_c_operation", Some(b_ctx))
          match service_c_span {
            Some(c_span) => {
              // Service C also encounters error
              Span::set_status(c_span, Error, Some("Service C encountered an error"))
              Span::add_event(c_span, "exception", Some([
                ("exception.type", StringValue("ServiceException")),
                ("exception.message", StringValue("Service C failed to process request"))
              ]))
              
              Span::end(c_span)
              
              // Service C propagates error back to Service B
              Span::add_event(b_span, "error_propagation", Some([
                ("error.source", StringValue("service_c")),
                ("error.message", StringValue("Error propagated from Service C"))
              ]))
            }
            None => assert_true(false, "Service C span should be created")
          }
          
          Span::end(b_span)
          
          // Service B propagates error back to Service A
          Span::add_event(span, "error_propagation", Some([
            ("error.source", StringValue("service_b")),
            ("error.message", StringValue("Error propagated from Service B"))
          ]))
          
          // Service A sets error status
          Span::set_status(span, Error, Some("Operation failed due to downstream errors"))
        }
        None => assert_true(false, "Service B span should be created")
      }
      
      Span::end(span)
    }
    None => assert_true(false, "Service A span should be created")
  }
}

// Test 7: Cross-Service Sampling Consistency
test "cross-service sampling consistency" {
  // Test with sampled trace
  let sampled_ctx = SpanContext::new("trace_id_1", "span_id_1", true, "")
  assert_true(SpanContext::is_sampled(sampled_ctx), "Context should be sampled")
  
  // Propagate to Service B
  let service_b_provider = TracerProvider::default()
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
  
  let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", Some(sampled_ctx))
  match service_b_span {
    Some(b_span) => {
      let b_ctx = Span::span_context(b_span)
      assert_true(SpanContext::is_sampled(b_ctx), "Service B should inherit sampling decision")
      
      // Propagate to Service C
      let service_c_provider = TracerProvider::default()
      let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
      
      let service_c_span = Tracer::start_span_with_context(service_c_tracer, "service_c_operation", Some(b_ctx))
      match service_c_span {
        Some(c_span) => {
          let c_ctx = Span::span_context(c_span)
          assert_true(SpanContext::is_sampled(c_ctx), "Service C should inherit sampling decision")
          
          Span::end(c_span)
        }
        None => assert_true(false, "Service C span should be created")
      }
      
      Span::end(b_span)
    }
    None => assert_true(false, "Service B span should be created")
  }
  
  // Test with non-sampled trace
  let non_sampled_ctx = SpanContext::new("trace_id_2", "span_id_2", false, "")
  assert_false(SpanContext::is_sampled(non_sampled_ctx), "Context should not be sampled")
  
  // Propagate to Service B
  let service_b_span2 = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", Some(non_sampled_ctx))
  match service_b_span2 {
    Some(b_span) => {
      let b_ctx = Span::span_context(b_span)
      assert_false(SpanContext::is_sampled(b_ctx), "Service B should inherit sampling decision")
      
      // Propagate to Service C
      let service_c_span2 = Tracer::start_span_with_context(service_c_tracer, "service_c_operation", Some(b_ctx))
      match service_c_span2 {
        Some(c_span) => {
          let c_ctx = Span::span_context(c_span)
          assert_false(SpanContext::is_sampled(c_ctx), "Service C should inherit sampling decision")
          
          Span::end(c_span)
        }
        None => assert_true(false, "Service C span should be created")
      }
      
      Span::end(b_span)
    }
    None => assert_true(false, "Service B span should be created")
  }
}

// Test 8: Cross-Service Correlation
test "cross-service correlation" {
  // Create correlation context in Service A
  let correlation_id = "corr-123456789"
  let request_id = "req-987654321"
  let user_id = "user-456789123"
  
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  
  let service_a_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  match service_a_span {
    Some(span) => {
      // Add correlation attributes
      Span::set_attribute(span, "correlation.id", StringValue(correlation_id))
      Span::set_attribute(span, "request.id", StringValue(request_id))
      Span::set_attribute(span, "user.id", StringValue(user_id))
      
      // Get context for propagation
      let ctx = Span::span_context(span)
      
      // Service B receives and propagates correlation
      let service_b_provider = TracerProvider::default()
      let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
      
      let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service_b_operation", Some(ctx))
      match service_b_span {
        Some(b_span) => {
          // Service B adds correlation attributes
          Span::set_attribute(b_span, "correlation.id", StringValue(correlation_id))
          Span::set_attribute(b_span, "request.id", StringValue(request_id))
          Span::set_attribute(b_span, "user.id", StringValue(user_id))
          
          // Add service B specific correlation
          Span::set_attribute(b_span, "service.b.operation.id", StringValue("op-b-123"))
          
          // Get context for propagation
          let b_ctx = Span::span_context(b_span)
          
          // Service C receives and propagates correlation
          let service_c_provider = TracerProvider::default()
          let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
          
          let service_c_span = Tracer::start_span_with_context(service_c_tracer, "service_c_operation", Some(b_ctx))
          match service_c_span {
            Some(c_span) => {
              // Service C adds correlation attributes
              Span::set_attribute(c_span, "correlation.id", StringValue(correlation_id))
              Span::set_attribute(c_span, "request.id", StringValue(request_id))
              Span::set_attribute(c_span, "user.id", StringValue(user_id))
              
              // Add service C specific correlation
              Span::set_attribute(c_span, "service.c.operation.id", StringValue("op-c-456"))
              
              // Verify correlation consistency
              let c_corr_id = Span::get_attribute(c_span, "correlation.id")
              match c_corr_id {
                Some(StringValue(id)) => assert_eq(id, correlation_id),
                _ => assert_true(false, "Correlation ID should be preserved in Service C")
              }
              
              let c_req_id = Span::get_attribute(c_span, "request.id")
              match c_req_id {
                Some(StringValue(id)) => assert_eq(id, request_id),
                _ => assert_true(false, "Request ID should be preserved in Service C")
              }
              
              let c_user_id = Span::get_attribute(c_span, "user.id")
              match c_user_id {
                Some(StringValue(id)) => assert_eq(id, user_id),
                _ => assert_true(false, "User ID should be preserved in Service C")
              }
              
              Span::end(c_span)
            }
            None => assert_true(false, "Service C span should be created")
          }
          
          Span::end(b_span)
        }
        None => assert_true(false, "Service B span should be created")
      }
      
      Span::end(span)
    }
    None => assert_true(false, "Service A span should be created")
  }
}

// Test 9: Cross-Service Format Compatibility
test "cross-service format compatibility" {
  // Create span in Service A
  let service_a_provider = TracerProvider::default()
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  
  let service_a_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  match service_a_span {
    Some(span) => {
      // Add attributes with different types
      Span::set_attribute(span, "string.attr", StringValue("string_value"))
      Span::set_attribute(span, "int.attr", IntValue(42))
      Span::set_attribute(span, "float.attr", FloatValue(3.14))
      Span::set_attribute(span, "bool.attr", BoolValue(true))
      Span::set_attribute(span, "array.string.attr", ArrayStringValue(["value1", "value2"]))
      Span::set_attribute(span, "array.int.attr", ArrayIntValue([1, 2, 3]))
      
      // Serialize span for cross-service transmission
      let serialized = Span::serialize(span)
      
      // Service B deserializes and processes
      let service_b_provider = TracerProvider::default()
      let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
      
      let deserialized_span = Span::deserialize(serialized)
      match deserialized_span {
        Some(b_span) => {
          // Verify all attribute types are preserved
          let string_attr = Span::get_attribute(b_span, "string.attr")
          match string_attr {
            Some(StringValue(value)) => assert_eq(value, "string_value"),
            _ => assert_true(false, "String attribute should be preserved")
          }
          
          let int_attr = Span::get_attribute(b_span, "int.attr")
          match int_attr {
            Some(IntValue(value)) => assert_eq(value, 42),
            _ => assert_true(false, "Int attribute should be preserved")
          }
          
          let float_attr = Span::get_attribute(b_span, "float.attr")
          match float_attr {
            Some(FloatValue(value)) => assert_true(abs(value - 3.14) < 0.0001),
            _ => assert_true(false, "Float attribute should be preserved")
          }
          
          let bool_attr = Span::get_attribute(b_span, "bool.attr")
          match bool_attr {
            Some(BoolValue(value)) => assert_true(value),
            _ => assert_true(false, "Bool attribute should be preserved")
          }
          
          let array_string_attr = Span::get_attribute(b_span, "array.string.attr")
          match array_string_attr {
            Some(ArrayStringValue(value)) => {
              assert_eq(value.length(), 2)
              assert_eq(value[0], "value1")
              assert_eq(value[1], "value2")
            }
            _ => assert_true(false, "Array string attribute should be preserved")
          }
          
          let array_int_attr = Span::get_attribute(b_span, "array.int.attr")
          match array_int_attr {
            Some(ArrayIntValue(value)) => {
              assert_eq(value.length(), 3)
              assert_eq(value[0], 1)
              assert_eq(value[1], 2)
              assert_eq(value[2], 3)
            }
            _ => assert_true(false, "Array int attribute should be preserved")
          }
          
          Span::end(b_span)
        }
        None => assert_true(false, "Deserialized span should be available")
      }
      
      Span::end(span)
    }
    None => assert_true(false, "Service A span should be created")
  }
}

// Test 10: Cross-Service Version Compatibility
test "cross-service version compatibility" {
  // Simulate different service versions
  let service_v1_provider = TracerProvider::default()
  let service_v1_tracer = TracerProvider::get_tracer(service_v1_provider, "service_v1")
  
  let service_v2_provider = TracerProvider::default()
  let service_v2_tracer = TracerProvider::get_tracer(service_v2_provider, "service_v2")
  
  let service_v3_provider = TracerProvider::default()
  let service_v3_tracer = TracerProvider::get_tracer(service_v3_provider, "service_v3")
  
  // Service v1 creates span with v1-specific attributes
  let service_v1_span = Tracer::start_span(service_v1_tracer, "service_v1_operation")
  match service_v1_span {
    Some(v1_span) => {
      Span::set_attribute(v1_span, "service.version", StringValue("1.0.0"))
      Span::set_attribute(v1_span, "service.v1.feature", StringValue("enabled"))
      Span::set_attribute(v1_span, "common.attr", StringValue("common_value"))
      
      let ctx = Span::span_context(v1_span)
      
      // Service v2 receives and adds v2-specific attributes
      let service_v2_span = Tracer::start_span_with_context(service_v2_tracer, "service_v2_operation", Some(ctx))
      match service_v2_span {
        Some(v2_span) => {
          Span::set_attribute(v2_span, "service.version", StringValue("2.0.0"))
          Span::set_attribute(v2_span, "service.v2.feature", StringValue("enabled"))
          Span::set_attribute(v2_span, "common.attr", StringValue("common_value"))
          
          // Preserve v1 attributes
          Span::set_attribute(v2_span, "service.v1.feature", StringValue("enabled"))
          
          let v2_ctx = Span::span_context(v2_span)
          
          // Service v3 receives and adds v3-specific attributes
          let service_v3_span = Tracer::start_span_with_context(service_v3_tracer, "service_v3_operation", Some(v2_ctx))
          match service_v3_span {
            Some(v3_span) => {
              Span::set_attribute(v3_span, "service.version", StringValue("3.0.0"))
              Span::set_attribute(v3_span, "service.v3.feature", StringValue("enabled"))
              Span::set_attribute(v3_span, "common.attr", StringValue("common_value"))
              
              // Preserve v1 and v2 attributes
              Span::set_attribute(v3_span, "service.v1.feature", StringValue("enabled"))
              Span::set_attribute(v3_span, "service.v2.feature", StringValue("enabled"))
              
              // Verify version compatibility
              let v1_feature = Span::get_attribute(v3_span, "service.v1.feature")
              match v1_feature {
                Some(StringValue(value)) => assert_eq(value, "enabled"),
                _ => assert_true(false, "v1 feature should be preserved in v3")
              }
              
              let v2_feature = Span::get_attribute(v3_span, "service.v2.feature")
              match v2_feature {
                Some(StringValue(value)) => assert_eq(value, "enabled"),
                _ => assert_true(false, "v2 feature should be preserved in v3")
              }
              
              let v3_feature = Span::get_attribute(v3_span, "service.v3.feature")
              match v3_feature {
                Some(StringValue(value)) => assert_eq(value, "enabled"),
                _ => assert_true(false, "v3 feature should be available")
              }
              
              let common_attr = Span::get_attribute(v3_span, "common.attr")
              match common_attr {
                Some(StringValue(value)) => assert_eq(value, "common_value"),
                _ => assert_true(false, "Common attribute should be preserved")
              }
              
              Span::end(v3_span)
            }
            None => assert_true(false, "Service v3 span should be created")
          }
          
          Span::end(v2_span)
        }
        None => assert_true(false, "Service v2 span should be created")
      }
      
      Span::end(v1_span)
    }
    None => assert_true(false, "Service v1 span should be created")
  }
}

// Helper function to get current timestamp
fn get_current_timestamp() -> Int {
  // Implementation would depend on the available time functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}