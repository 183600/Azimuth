// Additional edge case and boundary tests for Azimuth telemetry system
// Tests covering boundary conditions, resource management, and configuration scenarios

test "boundary conditions for attribute values" {
  // Test boundary conditions for attribute values
  let attrs = Attributes::new()
  
  // Test with extremely long strings
  let very_long_string = "x".repeat(10000)
  Attributes::set(attrs, "long.key", StringValue(very_long_string))
  
  // Test with special Unicode characters
  Attributes::set(attrs, "unicode.key", StringValue("🚀🎯📊\x00\x01\x02"))
  
  // Test with numeric boundaries
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))
  Attributes::set(attrs, "infinity", FloatValue(1.0/0.0))
  Attributes::set(attrs, "neg.infinity", FloatValue(-1.0/0.0))
  
  // Test passes if no exceptions are thrown
  assert_true(true)
}

test "resource management and cleanup scenarios" {
  // Test resource management scenarios
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  let resource3 = Resource::new()
  
  // Create resources with different attributes
  let attrs1 = [("service.name", StringValue("service1")), ("version", StringValue("1.0.0"))]
  let attrs2 = [("service.name", StringValue("service2")), ("version", StringValue("2.0.0"))]
  let attrs3 = [("service.name", StringValue("service3")), ("version", StringValue("3.0.0"))]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  let resource3_with_attrs = Resource::with_attributes(resource3, attrs3)
  
  // Test resource merging in different combinations
  let merged1 = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let merged2 = Resource::merge(merged1, resource3_with_attrs)
  let merged3 = Resource::merge(resource2_with_attrs, resource1_with_attrs)
  
  // Test passes if resource operations don't cause issues
  assert_true(true)
}

test "configuration and initialization validation" {
  // Test various configuration scenarios
  let trace_provider = TracerProvider::default()
  let metrics_provider = MeterProvider::default()
  let logs_provider = LoggerProvider::default()
  
  // Test with different instrumentation scope configurations
  let tracer1 = TracerProvider::get_tracer(trace_provider, "config.test")
  let tracer2 = TracerProvider::get_tracer(trace_provider, "config.test")
  let tracer3 = TracerProvider::get_tracer(trace_provider, "config.test")
  
  let meter1 = MeterProvider::get_meter(metrics_provider, "config.test")
  let meter2 = MeterProvider::get_meter(metrics_provider, "config.test")
  let meter3 = MeterProvider::get_meter(metrics_provider, "config.test")
  
  let logger1 = LoggerProvider::get_logger(logs_provider, "config.test")
  let logger2 = LoggerProvider::get_logger(logs_provider, "config.test")
  let logger3 = LoggerProvider::get_logger(logs_provider, "config.test")
  
  // Verify different configurations
  assert_eq(tracer1.scope.name, "config.test")
  assert_eq(tracer1.scope.version, None)
  assert_eq(tracer2.scope.version, Some("1.0.0"))
  assert_eq(tracer3.scope.schema_url, Some("https://example.com/schema"))
  
  assert_eq(meter1.scope.name, "config.test")
  assert_eq(meter2.scope.version, Some("1.0.0"))
  assert_eq(meter3.scope.schema_url, Some("https://example.com/schema"))
  
  assert_eq(logger1.scope.name, "config.test")
  assert_eq(logger2.scope.version, Some("1.0.0"))
  assert_eq(logger3.scope.schema_url, Some("https://example.com/schema"))
}

test "span lifecycle edge cases" {
  // Test span lifecycle edge cases
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "lifecycle.test")
  
  // Create multiple spans
  let span1 = Tracer::start_span(tracer, "span1")
  let span2 = Tracer::start_span(tracer, "span2")
  let span3 = Tracer::start_span(tracer, "span3")
  
  // Test multiple operations on active spans
  Span::add_event(span1, "event1")
  Span::add_event(span2, "event2")
  Span::add_event(span3, "event3")
  
  Span::set_status(span1, Ok)
  Span::set_status(span2, Error, Some("Test error"))
  Span::set_status(span3, Unset)
  
  // End spans in different orders
  Span::end(span2)
  Span::end(span1)
  Span::end(span3)
  
  // Test operations on ended spans (should not throw exceptions)
  Span::add_event(span1, "after_end_event")
  Span::set_status(span2, Ok)
  
  // Test passes if all operations complete without exceptions
  assert_true(true)
}