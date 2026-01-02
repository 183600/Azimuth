// Specialized Test Cases for Azimuth Telemetry System
// This file contains specialized test cases focusing on specific telemetry scenarios

// Test 1: Metrics aggregation with time windows
test "metrics aggregation with time windows" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time-window-test")
  
  // Create metrics for time-based aggregation
  let request_counter = Meter::create_counter(meter, "time.window.requests", Some("Requests in time windows"), Some("count"))
  let response_time_histogram = Meter::create_histogram(meter, "time.window.response_time", Some("Response time distribution"), Some("ms"))
  
  // Simulate requests in different time windows
  for i = 0; i < 100; i = i + 1 {
    Counter::add(request_counter, 1.0)
    // Simulate varying response times
    let response_time = 10.0 + (i % 50) * 2.0
    Histogram::record(response_time_histogram, response_time)
  }
  
  // Verify metrics were recorded
  assert_eq(request_counter.name, "time.window.requests")
  assert_eq(response_time_histogram.name, "time.window.response_time")
}

// Test 2: Span correlation with distributed systems
test "span correlation with distributed systems" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed-test")
  
  // Create root span for the distributed operation
  let root_span = Tracer::start_span(tracer, "distributed.operation", Some([
    ("operation.type", StringValue("user.request")),
    ("user.id", StringValue("user-12345"))
  ]))
  
  // Extract context for propagation
  let span_context = Span::span_context(root_span)
  let trace_id = SpanContext::trace_id(span_context)
  let span_id = SpanContext::span_id(span_context)
  
  // Simulate service A span
  let service_a_context = SpanContext::new(trace_id, "service-a-span-123", true, "key1=value1")
  let service_a_span = Span::new("service.a.operation", Server, service_a_context)
  Span::add_event(service_a_span, "service.a.started", Some([
    ("service.name", StringValue("service-a")),
    ("service.version", StringValue("1.0.0"))
  ]))
  Span::end(service_a_span)
  
  // Simulate service B span
  let service_b_context = SpanContext::new(trace_id, "service-b-span-456", true, "key2=value2")
  let service_b_span = Span::new("service.b.operation", Server, service_b_context)
  Span::add_event(service_b_span, "service.b.started", Some([
    ("service.name", StringValue("service-b")),
    ("service.version", StringValue("2.1.0"))
  ]))
  Span::end(service_b_span)
  
  // Verify correlation
  assert_eq(SpanContext::trace_id(service_a_context), trace_id)
  assert_eq(SpanContext::trace_id(service_b_context), trace_id)
  
  // End root span
  Span::end(root_span)
}

// Test 3: Logger with correlation and context propagation
test "logger with correlation and context propagation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation-test")
  
  // Create correlation attributes
  let correlation_attrs = Attributes::new()
  Attributes::set(correlation_attrs, "correlation.id", StringValue("corr-12345-abcde"))
  Attributes::set(correlation_attrs, "request.id", StringValue("req-67890-fghij"))
  Attributes::set(correlation_attrs, "session.id", StringValue("sess-11111-22222"))
  Attributes::set(correlation_attrs, "user.id", StringValue("user-33333-44444"))
  Attributes::set(correlation_attrs, "tenant.id", StringValue("tenant-55555-66666"))
  
  // Create log with correlation context
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Processing user request"),
    Some(correlation_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  Logger::emit(logger, info_log)
  
  // Create warning log with additional context
  let warning_attrs = Attributes::new()
  Attributes::set(warning_attrs, "correlation.id", StringValue("corr-12345-abcde"))
  Attributes::set(warning_attrs, "warning.type", StringValue("performance.degradation"))
  Attributes::set(warning_attrs, "response.time", IntValue(5000))
  Attributes::set(warning_attrs, "threshold", IntValue(3000))
  
  let warning_log = LogRecord::new_with_context(
    Warn,
    Some("Response time exceeds threshold"),
    Some(warning_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  Logger::emit(logger, warning_log)
  
  // Verify log properties
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warning_log), Warn)
  assert_eq(LogRecord::body(info_log), Some("Processing user request"))
  assert_eq(LogRecord::body(warning_log), Some("Response time exceeds threshold"))
}

// Test 4: Resource with dynamic attributes
test "resource with dynamic attributes" {
  let base_resource = Resource::new()
  
  // Test resource with runtime attributes
  let runtime_attrs = [
    ("runtime.name", StringValue("moonbit")),
    ("runtime.version", StringValue("0.1.20240101")),
    ("runtime.os", StringValue("linux")),
    ("runtime.arch", StringValue("x86_64")),
    ("runtime.description", StringValue("MoonBit Runtime Environment"))
  ]
  
  let runtime_resource = Resource::with_attributes(base_resource, runtime_attrs)
  
  // Test resource with cloud provider attributes
  let cloud_attrs = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.platform", StringValue("aws_ec2"))
  ]
  
  let cloud_resource = Resource::with_attributes(base_resource, cloud_attrs)
  
  // Test resource with deployment attributes
  let deployment_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.user", StringValue("azimuth-deployer")),
    ("deployment.timestamp", StringValue("2024-01-01T00:00:00Z")),
    ("deployment.version", StringValue("v2.1.0")),
    ("deployment.commit", StringValue("abc123def456"))
  ]
  
  let deployment_resource = Resource::with_attributes(base_resource, deployment_attrs)
  
  // Merge all resources
  let merged_resource = Resource::merge(runtime_resource, cloud_resource)
  let fully_merged_resource = Resource::merge(merged_resource, deployment_resource)
  
  // Verify merged resource contains all attributes
  assert_eq(Resource::get_attribute(fully_merged_resource, "runtime.name"), Some(StringValue("moonbit")))
  assert_eq(Resource::get_attribute(fully_merged_resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(fully_merged_resource, "deployment.environment"), Some(StringValue("production")))
}

// Test 5: Baggage with metadata properties
test "baggage with metadata properties" {
  let baggage = Baggage::new()
  
  // Test baggage with simple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-abcdef")
  
  // Test baggage with complex entries
  let baggage3 = Baggage::set_entry(baggage2, "request.metadata", "{\"type\":\"api\",\"version\":\"v1\",\"source\":\"web\"}")
  let baggage4 = Baggage::set_entry(baggage3, "trace.flags", "sampled=1,debug=0")
  let baggage5 = Baggage::set_entry(baggage4, "custom.properties", "prop1=value1;prop2=value2;prop3=value3")
  
  // Test baggage with URL-encoded values
  let baggage6 = Baggage::set_entry(baggage5, "url.data", "https%3A%2F%2Fexample.com%2Fpath%3Fparam%3Dvalue%26other%3Dtest")
  
  // Test baggage with JSON array
  let baggage7 = Baggage::set_entry(baggage6, "array.data", "[\"item1\",\"item2\",\"item3\"]")
  
  // Verify all baggage entries
  assert_eq(Baggage::get_entry(baggage7, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage7, "session.id"), Some("session-abcdef"))
  assert_eq(Baggage::get_entry(baggage7, "request.metadata"), Some("{\"type\":\"api\",\"version\":\"v1\",\"source\":\"web\"}"))
  assert_eq(Baggage::get_entry(baggage7, "trace.flags"), Some("sampled=1,debug=0"))
  assert_eq(Baggage::get_entry(baggage7, "custom.properties"), Some("prop1=value1;prop2=value2;prop3=value3"))
  assert_eq(Baggage::get_entry(baggage7, "url.data"), Some("https%3A%2F%2Fexample.com%2Fpath%3Fparam%3Dvalue%26other%3Dtest"))
  assert_eq(Baggage::get_entry(baggage7, "array.data"), Some("[\"item1\",\"item2\",\"item3\"]"))
  
  // Test baggage iteration
  let entries = Baggage::get_all_entries(baggage7)
  assert_true(entries.length() >= 7)
}

// Test 6: Context with hierarchical data
test "context with hierarchical data" {
  // Test context with nested values
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("app.name"), "azimuth-service")
  
  // Test context with configuration values
  let ctx2 = Context::with_value(ctx1, ContextKey::new("config.database.host"), "db.example.com")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("config.database.port"), "5432")
  let ctx4 = Context::with_value(ctx3, ContextKey::new("config.database.name"), "azimuth_prod")
  
  // Test context with feature flags
  let ctx5 = Context::with_value(ctx4, ContextKey::new("features.enable_metrics"), "true")
  let ctx6 = Context::with_value(ctx5, ContextKey::new("features.enable_tracing"), "true")
  let ctx7 = Context::with_value(ctx6, ContextKey::new("features.enable_logging"), "true")
  
  // Test context with user preferences
  let ctx8 = Context::with_value(ctx7, ContextKey::new("user.preferences.timezone"), "UTC")
  let ctx9 = Context::with_value(ctx8, ContextKey::new("user.preferences.language"), "en")
  let ctx10 = Context::with_value(ctx9, ContextKey::new("user.preferences.theme"), "dark")
  
  // Verify context values
  assert_eq(Context::get(ctx10, ContextKey::new("app.name")), Some("azimuth-service"))
  assert_eq(Context::get(ctx10, ContextKey::new("config.database.host")), Some("db.example.com"))
  assert_eq(Context::get(ctx10, ContextKey::new("config.database.port")), Some("5432"))
  assert_eq(Context::get(ctx10, ContextKey::new("config.database.name")), Some("azimuth_prod"))
  assert_eq(Context::get(ctx10, ContextKey::new("features.enable_metrics")), Some("true"))
  assert_eq(Context::get(ctx10, ContextKey::new("features.enable_tracing")), Some("true"))
  assert_eq(Context::get(ctx10, ContextKey::new("features.enable_logging")), Some("true"))
  assert_eq(Context::get(ctx10, ContextKey::new("user.preferences.timezone")), Some("UTC"))
  assert_eq(Context::get(ctx10, ContextKey::new("user.preferences.language")), Some("en"))
  assert_eq(Context::get(ctx10, ContextKey::new("user.preferences.theme")), Some("dark"))
}

// Test 7: Span with custom attributes and events
test "span with custom attributes and events" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "custom-attributes-test")
  
  // Create span with custom attributes
  let custom_attrs = Attributes::new()
  Attributes::set(custom_attrs, "business.domain", StringValue("ecommerce"))
  Attributes::set(custom_attrs, "business.process", StringValue("order.fulfillment"))
  Attributes::set(custom_attrs, "business.step", StringValue("inventory.check"))
  Attributes::set(custom_attrs, "business.criticality", StringValue("high"))
  
  let span = Tracer::start_span(tracer, "business.process.execution", Some(custom_attrs))
  
  // Add custom events with business context
  let inventory_attrs = Attributes::new()
  Attributes::set(inventory_attrs, "product.id", StringValue("prod-12345"))
  Attributes::set(inventory_attrs, "product.quantity", IntValue(10))
  Attributes::set(inventory_attrs, "warehouse.id", StringValue("wh-001"))
  Attributes::set(inventory_attrs, "inventory.status", StringValue("available"))
  
  Span::add_event(span, "inventory.check.started", Some(inventory_attrs))
  
  // Add another event with different attributes
  let reservation_attrs = Attributes::new()
  Attributes::set(reservation_attrs, "reservation.id", StringValue("res-abcdef"))
  Attributes::set(reservation_attrs, "reservation.quantity", IntValue(5))
  Attributes::set(reservation_attrs, "reservation.expiry", StringValue("2024-01-02T00:00:00Z"))
  
  Span::add_event(span, "inventory.reservation.created", Some(reservation_attrs))
  
  // Set custom status
  Span::set_status(span, Ok, Some("Inventory check completed successfully"))
  
  // Verify span properties
  assert_eq(Span::name(span), "business.process.execution")
  
  // End span
  Span::end(span)
}

// Test 8: Metrics with dynamic dimensions
test "metrics with dynamic dimensions" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dynamic-dimensions-test")
  
  // Create metrics with dynamic dimensions
  let api_counter = Meter::create_counter(meter, "api.requests", Some("API requests with dimensions"), Some("count"))
  let db_histogram = Meter::create_histogram(meter, "db.query.time", Some("Database query time"), Some("ms"))
  
  // Record metrics with different dimensions
  let api_attrs1 = Attributes::new()
  Attributes::set(api_attrs1, "api.method", StringValue("GET"))
  Attributes::set(api_attrs1, "api.endpoint", StringValue("/api/users"))
  Attributes::set(api_attrs1, "api.version", StringValue("v1"))
  Attributes::set(api_attrs1, "api.status", StringValue("200"))
  
  Counter::add(api_counter, 1.0, Some(api_attrs1))
  
  let api_attrs2 = Attributes::new()
  Attributes::set(api_attrs2, "api.method", StringValue("POST"))
  Attributes::set(api_attrs2, "api.endpoint", StringValue("/api/orders"))
  Attributes::set(api_attrs2, "api.version", StringValue("v1"))
  Attributes::set(api_attrs2, "api.status", StringValue("201"))
  
  Counter::add(api_counter, 1.0, Some(api_attrs2))
  
  let api_attrs3 = Attributes::new()
  Attributes::set(api_attrs3, "api.method", StringValue("GET"))
  Attributes::set(api_attrs3, "api.endpoint", StringValue("/api/products"))
  Attributes::set(api_attrs3, "api.version", StringValue("v2"))
  Attributes::set(api_attrs3, "api.status", StringValue("404"))
  
  Counter::add(api_counter, 1.0, Some(api_attrs3))
  
  // Record database metrics with dimensions
  let db_attrs1 = Attributes::new()
  Attributes::set(db_attrs1, "db.type", StringValue("postgresql"))
  Attributes::set(db_attrs1, "db.operation", StringValue("SELECT"))
  Attributes::set(db_attrs1, "db.table", StringValue("users"))
  Attributes::set(db_attrs1, "db.index.used", StringValue("idx_users_email"))
  
  Histogram::record(db_histogram, 25.5, Some(db_attrs1))
  
  let db_attrs2 = Attributes::new()
  Attributes::set(db_attrs2, "db.type", StringValue("postgresql"))
  Attributes::set(db_attrs2, "db.operation", StringValue("INSERT"))
  Attributes::set(db_attrs2, "db.table", StringValue("orders"))
  Attributes::set(db_attrs2, "db.index.used", StringValue("none"))
  
  Histogram::record(db_histogram, 150.2, Some(db_attrs2))
  
  // Verify metrics were recorded
  assert_eq(api_counter.name, "api.requests")
  assert_eq(db_histogram.name, "db.query.time")
}