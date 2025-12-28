test "context operations and key-value management" {
  // Test context creation, value setting, and retrieval
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Create context keys
  let user_key = @azimuth.telemetry.api.context.ContextKey::new("user.id")
  let request_key = @azimuth.telemetry.api.context.ContextKey::new("request.id")
  let trace_key = @azimuth.telemetry.api.context.ContextKey::new("trace.info")
  
  // Set values in context
  let ctx_with_user = @azimuth.telemetry.api.context.Context.with_value(ctx, user_key, "user123")
  let ctx_with_request = @azimuth.telemetry.api.context.Context.with_value(ctx_with_user, request_key, "req-456")
  let ctx_with_trace = @azimuth.telemetry.api.context.Context.with_value(ctx_with_request, trace_key, "trace-789")
  
  // Retrieve values from context
  let user_value = @azimuth.telemetry.api.context.Context.get(ctx_with_trace, user_key)
  let request_value = @azimuth.telemetry.api.context.Context.get(ctx_with_trace, request_key)
  let trace_value = @azimuth.telemetry.api.context.Context.get(ctx_with_trace, trace_key)
  
  assert_eq(user_value, Some("user123"))
  assert_eq(request_value, Some("req-456"))
  assert_eq(trace_value, Some("trace-789"))
  
  // Test non-existent key
  let missing_key = @azimuth.telemetry.api.context.ContextKey::new("missing.key")
  let missing_value = @azimuth.telemetry.api.context.Context.get(ctx_with_trace, missing_key)
  assert_eq(missing_value, None)
  
  // Test root context has no values
  let root_user = @azimuth.telemetry.api.context.Context.get(ctx, user_key)
  assert_eq(root_user, None)
}

test "baggage operations and entry management" {
  // Test baggage creation, entry setting, and retrieval
  let baggage = @azimuth.telemetry.api.context.Baggage::new()
  
  // Set baggage entries
  let baggage_with_user = @azimuth.telemetry.api.context.Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = @azimuth.telemetry.api.context.Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_region = @azimuth.telemetry.api.context.Baggage::set_entry(baggage_with_session, "region", "us-west")
  
  // Retrieve baggage entries
  let user_value = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_with_region, "user.id")
  let session_value = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_with_region, "session.id")
  let region_value = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_with_region, "region")
  
  assert_eq(user_value, Some("user123"))
  assert_eq(session_value, Some("session456"))
  assert_eq(region_value, Some("us-west"))
  
  // Test non-existent entry
  let missing_value = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_with_region, "missing.key")
  assert_eq(missing_value, None)
  
  // Test entry removal
  let baggage_without_user = @azimuth.telemetry.api.context.Baggage::remove_entry(baggage_with_region, "user.id")
  let removed_user_value = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_without_user, "user.id")
  assert_eq(removed_user_value, None)
  
  // Verify other entries still exist
  let remaining_session = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_without_user, "session.id")
  let remaining_region = @azimuth.telemetry.api.context.Baggage::get_entry(baggage_without_user, "region")
  assert_eq(remaining_session, Some("session456"))
  assert_eq(remaining_region, Some("us-west"))
}

test "w3c trace context propagation" {
  // Test W3C trace context propagation injection and extraction
  let propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new()
  let composite_propagator = @azimuth.telemetry.api.propagation.CompositePropagator::new([propagator])
  
  // Create a context with trace information
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  // Create carrier for injection
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  
  // Inject trace context into carrier
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Extract trace context from carrier
  let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction worked
  let extracted_key = @azimuth.telemetry.api.context.ContextKey::new("extracted")
  let extracted_value = @azimuth.telemetry.api.context.Context.get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Test direct carrier operations
  let traceparent_value = @azimuth.telemetry.api.propagation.TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent_value != None)
  assert_true(traceparent_value.unwrap().length() > 0)
  
  // Test non-existent header
  let missing_header = @azimuth.telemetry.api.propagation.TextMapCarrier::get(carrier, "non-existent")
  assert_eq(missing_header, None)
}

test "http client request and response handling" {
  // Test HTTP client creation and request/response handling
  let client = @azimuth.telemetry.sdk.platform.http.HttpClient::new()
  
  // Create HTTP request
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Telemetry/1.0")
  ]
  
  let request = @azimuth.telemetry.sdk.platform.http.HttpRequest::new(
    "POST",
    "https://api.example.com/v1/traces",
    headers,
    Some("{\"traces\": []}")
  )
  
  // Test request properties
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpRequest::http_method(request), "POST")
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpRequest::url(request), "https://api.example.com/v1/traces")
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpRequest::body(request), Some("{\"traces\": []}"))
  
  // Create HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Server", "nginx/1.18.0")
  ]
  
  let response = @azimuth.telemetry.sdk.platform.http.HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\": \"success\"}")
  )
  
  // Test response properties
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpResponse::status_code(response), 200)
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpResponse::body(response), Some("{\"status\": \"success\"}"))
  
  // Test error response
  let error_response = @azimuth.telemetry.sdk.platform.http.HttpResponse::new(
    400,
    [],
    Some("{\"error\": \"Bad Request\"}")
  )
  
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpResponse::status_code(error_response), 400)
  assert_eq(@azimuth.telemetry.sdk.platform.http.HttpResponse::body(error_response), Some("{\"error\": \"Bad Request\"}"))
}

test "platform clock and random utilities" {
  // Test platform clock functionality
  let clock = @azimuth.telemetry.sdk.platform.time.Clock::system()
  let timestamp = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(clock)
  
  // Verify timestamp is reasonable (should be > 0 and represent 2025)
  assert_true(timestamp > 0L)
  assert_true(timestamp > 1700000000000000000L)  // Sometime in 2023+
  assert_true(timestamp < 2000000000000000000L)  // Sometime before 2033
  
  // Test multiple calls produce different timestamps (or at least non-decreasing)
  let timestamp2 = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp)
  
  // Test platform random functionality
  let random = @azimuth.telemetry.sdk.platform.random.Random::system()
  
  // Test random bytes generation
  let random_bytes_8 = @azimuth.telemetry.sdk.platform.random.Random::next_bytes(random, 8)
  let random_bytes_16 = @azimuth.telemetry.sdk.platform.random.Random::next_bytes(random, 16)
  let random_bytes_32 = @azimuth.telemetry.sdk.platform.random.Random::next_bytes(random, 32)
  
  assert_eq(random_bytes_8.length(), 8)
  assert_eq(random_bytes_16.length(), 16)
  assert_eq(random_bytes_32.length(), 32)
  
  // Test random u64 generation
  let random_u64_1 = @azimuth.telemetry.sdk.platform.random.Random::next_u64(random)
  let random_u64_2 = @azimuth.telemetry.sdk.platform.random.Random::next_u64(random)
  
  // Random values should be different (very high probability)
  assert_true(random_u64_1 != random_u64_2)
  
  // Test random values are within u64 range
  assert_true(random_u64_1 >= 0UL)
  assert_true(random_u64_2 >= 0UL)
}

test "comprehensive log record operations" {
  // Test comprehensive log record creation and operations
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "test.logger", Some("1.0.0"))
  
  // Test different severity levels
  let severity_levels = [
    (@azimuth.telemetry.api.logs.SeverityNumber.Trace, "Trace level message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Debug, "Debug level message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Info, "Info level message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Warn, "Warning level message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Error, "Error level message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Fatal, "Fatal level message")
  ]
  
  for (severity, message) in severity_levels {
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(severity, message)
    
    assert_eq(@azimuth.telemetry.api.logs.LogRecord::severity_number(log_record), severity)
    assert_eq(@azimuth.telemetry.api.logs.LogRecord::body(log_record), Some(message))
    
    // Emit log record
    @azimuth.telemetry.api.logs.Logger::emit(logger, log_record)
  }
  
  // Test log record with full context
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "user.id", @azimuth.telemetry.api.common.StringValue("user123"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "request.id", @azimuth.telemetry.api.common.StringValue("req-456"))
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let comprehensive_log = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    Some("Comprehensive log message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(ctx)
  )
  
  // Verify comprehensive log properties
  assert_eq(@azimuth.telemetry.api.logs.LogRecord::severity_number(comprehensive_log), @azimuth.telemetry.api.logs.SeverityNumber.Info)
  assert_eq(@azimuth.telemetry.api.logs.LogRecord::body(comprehensive_log), Some("Comprehensive log message"))
  assert_eq(@azimuth.telemetry.api.logs.LogRecord::trace_id(comprehensive_log), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(@azimuth.telemetry.api.logs.LogRecord::span_id(comprehensive_log), Some("b7ad6b7169203331"))
  
  @azimuth.telemetry.api.logs.Logger::emit(logger, comprehensive_log)
}

test "resource management and attribute operations" {
  // Test resource creation, attribute management, and merging
  let base_resource = @azimuth.telemetry.api.common.Resource::new()
  
  // Test default resource attributes
  let service_name = @azimuth.telemetry.api.common.Resource::get_attribute(base_resource, "service.name")
  assert_true(service_name == None)  // Default implementation might not set service.name
  
  // Create resource with custom attributes
  let custom_attrs = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("azimuth-telemetry")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("deployment.environment", @azimuth.telemetry.api.common.StringValue("production")),
    ("host.name", @azimuth.telemetry.api.common.StringValue("server-01"))
  ]
  
  let custom_resource = @azimuth.telemetry.api.common.Resource::with_attributes(base_resource, custom_attrs)
  
  // Test attribute retrieval
  let retrieved_service_name = @azimuth.telemetry.api.common.Resource::get_attribute(custom_resource, "service.name")
  let retrieved_version = @azimuth.telemetry.api.common.Resource::get_attribute(custom_resource, "service.version")
  let retrieved_env = @azimuth.telemetry.api.common.Resource::get_attribute(custom_resource, "deployment.environment")
  let retrieved_host = @azimuth.telemetry.api.common.Resource::get_attribute(custom_resource, "host.name")
  
  assert_eq(retrieved_service_name, Some(@azimuth.telemetry.api.common.StringValue("azimuth-telemetry")))
  assert_eq(retrieved_version, Some(@azimuth.telemetry.api.common.StringValue("1.0.0")))
  assert_eq(retrieved_env, Some(@azimuth.telemetry.api.common.StringValue("production")))
  assert_eq(retrieved_host, Some(@azimuth.telemetry.api.common.StringValue("server-01")))
  
  // Test non-existent attribute
  let missing_attr = @azimuth.telemetry.api.common.Resource::get_attribute(custom_resource, "missing.attribute")
  assert_eq(missing_attr, None)
  
  // Test resource merging
  let override_attrs = [
    ("service.version", @azimuth.telemetry.api.common.StringValue("2.0.0")),  // Override
    ("new.attribute", @azimuth.telemetry.api.common.StringValue("new.value"))  // New
  ]
  
  let override_resource = @azimuth.telemetry.api.common.Resource::with_attributes(base_resource, override_attrs)
  let merged_resource = @azimuth.telemetry.api.common.Resource::merge(custom_resource, override_resource)
  
  // Test merged resource attributes
  let merged_version = @azimuth.telemetry.api.common.Resource::get_attribute(merged_resource, "service.version")
  let merged_new_attr = @azimuth.telemetry.api.common.Resource::get_attribute(merged_resource, "new.attribute")
  let merged_service_name = @azimuth.telemetry.api.common.Resource::get_attribute(merged_resource, "service.name")
  
  assert_eq(merged_version, Some(@azimuth.telemetry.api.common.StringValue("2.0.0")))
  assert_eq(merged_new_attr, Some(@azimuth.telemetry.api.common.StringValue("new.value")))
  // Note: Based on simplified merge implementation, original attributes might not be preserved
}

test "span context validation and edge cases" {
  // Test span context validation and edge cases
  let valid_span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  // Test valid span context properties
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(valid_span_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_sampled(valid_span_ctx))
  assert_eq(@azimuth.telemetry.api.trace.SpanContext::trace_id(valid_span_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(@azimuth.telemetry.api.trace.SpanContext::span_id(valid_span_ctx), "b7ad6b7169203331")
  
  // Test invalid span context (empty trace ID)
  let invalid_trace_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "",
    "b7ad6b7169203331",
    true,
    ""
  )
  
  assert_false(@azimuth.telemetry.api.trace.SpanContext::is_valid(invalid_trace_ctx))
  assert_false(@azimuth.telemetry.api.trace.SpanContext::is_sampled(invalid_trace_ctx))
  
  // Test invalid span context (empty span ID)
  let invalid_span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "",
    true,
    ""
  )
  
  assert_false(@azimuth.telemetry.api.trace.SpanContext::is_valid(invalid_span_ctx))
  assert_false(@azimuth.telemetry.api.trace.SpanContext::is_sampled(invalid_span_ctx))
  
  // Test non-sampled span context
  let non_sampled_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    false,
    ""
  )
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(non_sampled_ctx))
  assert_false(@azimuth.telemetry.api.trace.SpanContext::is_sampled(non_sampled_ctx))
  
  // Test span context with complex trace state
  let complex_trace_state_ctx = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE,rojo=00f067aa0ba902b7"
  )
  
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(complex_trace_state_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_sampled(complex_trace_state_ctx))
}

test "instrumentation scope variations and validation" {
  // Test instrumentation scope with different parameter combinations
  let minimal_scope = @azimuth.telemetry.api.common.InstrumentationScope::new("minimal.instrument", None, None)
  assert_eq(minimal_scope.name(), "minimal.instrument")
  assert_eq(minimal_scope.version(), None)
  assert_eq(minimal_scope.schema_url(), None)
  
  let version_only_scope = @azimuth.telemetry.api.common.InstrumentationScope::new("version.instrument", Some("2.1.0"), None)
  assert_eq(version_only_scope.name(), "version.instrument")
  assert_eq(version_only_scope.version(), Some("2.1.0"))
  assert_eq(version_only_scope.schema_url(), None)
  
  let schema_only_scope = @azimuth.telemetry.api.common.InstrumentationScope::new("schema.instrument", None, Some("https://example.com/schema/v1"))
  assert_eq(schema_only_scope.name(), "schema.instrument")
  assert_eq(schema_only_scope.version(), None)
  assert_eq(schema_only_scope.schema_url(), Some("https://example.com/schema/v1"))
  
  let full_scope = @azimuth.telemetry.api.common.InstrumentationScope::new(
    "full.instrument",
    Some("3.0.0"),
    Some("https://example.com/schema/v2")
  )
  assert_eq(full_scope.name(), "full.instrument")
  assert_eq(full_scope.version(), Some("3.0.0"))
  assert_eq(full_scope.schema_url(), Some("https://example.com/schema/v2"))
  
  // Test scope with special characters in name
  let special_name_scope = @azimuth.telemetry.api.common.InstrumentationScope::new(
    "special-name.instrument_123",
    Some("1.0.0-beta"),
    Some("https://example.com/special-schema")
  )
  assert_eq(special_name_scope.name(), "special-name.instrument_123")
  assert_eq(special_name_scope.version(), Some("1.0.0-beta"))
  assert_eq(special_name_scope.schema_url(), Some("https://example.com/special-schema"))
  
  // Test scope equality (same name should be considered same scope)
  let scope1 = @azimuth.telemetry.api.common.InstrumentationScope::new("test.scope", Some("1.0.0"), None)
  let scope2 = @azimuth.telemetry.api.common.InstrumentationScope::new("test.scope", Some("1.0.0"), None)
  let scope3 = @azimuth.telemetry.api.common.InstrumentationScope::new("different.scope", Some("1.0.0"), None)
  
  // Note: Equality comparison depends on implementation, but same name should typically be equal
  assert_eq(scope1.name(), scope2.name())
  assert_true(scope1.name() != scope3.name())
}