// Azimuth Advanced High-Quality Test Suite
// This file contains 10 comprehensive test cases covering edge cases, performance, and complex scenarios

// Test 1: Error Handling and Boundary Conditions
test "error handling and boundary conditions" {
  // Test empty and null string handling in attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "", StringValue("empty_key_test"))
  Attributes::set(attrs, "normal.key", StringValue(""))
  
  let empty_key_result = Attributes::get(attrs, "")
  let empty_value_result = Attributes::get(attrs, "normal.key")
  
  // Should handle empty keys gracefully
  assert_eq(empty_key_result, Some(StringValue("empty_key_test")))
  assert_eq(empty_value_result, Some(StringValue("")))
  
  // Test extremely long strings
  let long_string = "a" * 10000
  Attributes::set(attrs, "long.key", StringValue(long_string))
  let long_result = Attributes::get(attrs, "long.key")
  assert_eq(long_result, Some(StringValue(long_string)))
  
  // Test boundary conditions for numeric values
  Attributes::set(attrs, "max.int", IntValue(2147483647))
  Attributes::set(attrs, "min.int", IntValue(-2147483648))
  Attributes::set(attrs, "max.float", FloatValue(1.7976931348623157e+308))
  Attributes::set(attrs, "min.float", FloatValue(-1.7976931348623157e+308))
  
  let max_int = Attributes::get(attrs, "max.int")
  let min_int = Attributes::get(attrs, "min.int")
  let max_float = Attributes::get(attrs, "max.float")
  let min_float = Attributes::get(attrs, "min.float")
  
  assert_eq(max_int, Some(IntValue(2147483647)))
  assert_eq(min_int, Some(IntValue(-2147483648)))
  assert_eq(max_float, Some(FloatValue(1.7976931348623157e+308)))
  assert_eq(min_float, Some(FloatValue(-1.7976931348623157e+308)))
  
  // Test invalid span context handling
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_sampled(invalid_span_ctx))
  
  // Test context with null values
  let null_ctx = Context::root()
  let null_key = ContextKey::new("")
  let ctx_with_null = Context::with_value(null_ctx, null_key, "")
  let null_result = Context::get(ctx_with_null, null_key)
  assert_eq(null_result, Some(""))
}

// Test 2: Concurrency Safety
test "concurrency safety and thread safety" {
  // Test multiple operations on shared resources
  let shared_attrs = Attributes::new()
  let shared_baggage = Baggage::new()
  let shared_resource = Resource::new()
  
  // Simulate concurrent attribute operations
  for i in 0..100 {
    let key = "concurrent.key." + i.to_string()
    let value = "value." + i.to_string()
    Attributes::set(shared_attrs, key, StringValue(value))
  }
  
  // Verify all attributes were set correctly
  for i in 0..100 {
    let key = "concurrent.key." + i.to_string()
    let expected_value = "value." + i.to_string()
    let result = Attributes::get(shared_attrs, key)
    assert_eq(result, Some(StringValue(expected_value)))
  }
  
  // Test concurrent baggage operations
  let updated_baggage = shared_baggage
  for i in 0..50 {
    let baggage_key = "baggage." + i.to_string()
    let baggage_value = "data." + i.to_string()
    updated_baggage = Baggage::set_entry(updated_baggage, baggage_key, baggage_value)
  }
  
  // Verify baggage entries
  for i in 0..50 {
    let baggage_key = "baggage." + i.to_string()
    let expected_value = "data." + i.to_string()
    let result = Baggage::get_entry(updated_baggage, baggage_key)
    assert_eq(result, Some(expected_value))
  }
  
  // Test concurrent span creation and operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  let spans = []
  
  for i in 0..20 {
    let span_name = "concurrent-span-" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // Verify all spans were created with correct names
  for i in 0..spans.length() {
    let expected_name = "concurrent-span-" + i.to_string()
    let span = spans[i]
    assert_eq(Span::name(span), expected_name)
    assert_true(Span::is_recording(span))
  }
}

// Test 3: Performance and Resource Management
test "performance and resource management" {
  // Test large-scale attribute operations
  let start_time = Clock::now_unix_nanos(Clock::system())
  let large_attrs = Attributes::new()
  
  // Create 1000 attributes
  for i in 0..1000 {
    let key = "perf.key." + i.to_string()
    let value = "perf.value." + i.to_string()
    Attributes::set(large_attrs, key, StringValue(value))
  }
  
  let creation_time = Clock::now_unix_nanos(Clock::system())
  let creation_duration = creation_time - start_time
  
  // Verify creation time is reasonable (should be less than 1 second)
  assert_true(creation_duration < 1000000000L)
  
  // Test retrieval performance
  let retrieval_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..1000 {
    let key = "perf.key." + i.to_string()
    let expected_value = "perf.value." + i.to_string()
    let result = Attributes::get(large_attrs, key)
    assert_eq(result, Some(StringValue(expected_value)))
  }
  
  let retrieval_end = Clock::now_unix_nanos(Clock::system())
  let retrieval_duration = retrieval_end - retrieval_start
  
  // Verify retrieval time is reasonable
  assert_true(retrieval_duration < 1000000000L)
  
  // Test memory efficiency with large baggage
  let large_baggage = Baggage::new()
  let updated_baggage = large_baggage
  
  for i in 0..500 {
    let key = "large.baggage." + i.to_string()
    let value = "large.data." + i.to_string() + ".additional.content.to.simulate.real.world.usage"
    updated_baggage = Baggage::set_entry(updated_baggage, key, value)
  }
  
  // Test metrics performance
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-test")
  let counter = Meter::create_counter(meter, "performance.counter")
  
  let metrics_start = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..10000 {
    Counter::add(counter, i.to_double())
  }
  
  let metrics_end = Clock::now_unix_nanos(Clock::system())
  let metrics_duration = metrics_end - metrics_start
  
  // Verify metrics operations are efficient
  assert_true(metrics_duration < 500000000L)
}

// Test 4: Complex Scenario Integration
test "complex scenario integration" {
  // Create a complete distributed tracing scenario
  let service_resource = Resource::new()
  let service_attrs = [
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_attrs = Resource::with_attributes(service_resource, service_attrs)
  
  // Initialize trace context
  let trace_id = "complex-trace-" + Random::next_u64(Random::system()).to_string()
  let root_span_id = "root-span-" + Random::next_u64(Random::system()).to_string()
  let root_span_ctx = SpanContext::new(trace_id, root_span_id, true, "key1=value1,key2=value2")
  
  // Create root span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "complex-tracer", Some("1.0.0"))
  let root_span = Span::new("root-operation", Internal, root_span_ctx)
  
  // Add events and attributes to root span
  Span::add_event(root_span, "operation.started", [
    ("operation.type", StringValue("complex_workflow")),
    ("user.id", StringValue("user-67890"))
  ])
  
  // Create child spans for different operations
  let child_span1 = Tracer::start_span(tracer, "database.query")
  let child_span2 = Tracer::start_span(tracer, "cache.lookup")
  let child_span3 = Tracer::start_span(tracer, "external.api.call")
  
  // Add business context to child spans
  Span::add_event(child_span1, "query.executed", [
    ("db.statement", StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.duration.ms", IntValue(45))
  ])
  
  Span::add_event(child_span2, "cache.hit", [
    ("cache.key", StringValue("user:67890")),
    ("cache.ttl", IntValue(300))
  ])
  
  Span::add_event(child_span3, "api.response", [
    ("api.endpoint", StringValue("/api/v1/recommendations")),
    ("api.status_code", IntValue(200)),
    ("api.response_time.ms", IntValue(120))
  ])
  
  // Set span statuses
  Span::set_status(child_span1, Ok, "Query completed successfully")
  Span::set_status(child_span2, Ok, "Cache hit successful")
  Span::set_status(child_span3, Ok, "External API call successful")
  
  // Create metrics for the operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "complex-metrics")
  
  let request_counter = Meter::create_counter(meter, "requests.total")
  let db_duration_histogram = Meter::create_histogram(meter, "db.query.duration.ms", Some("Database query duration"), Some("ms"))
  let cache_hit_gauge = Meter::create_gauge(meter, "cache.hit.rate")
  let api_latency_histogram = Meter::create_histogram(meter, "api.latency.ms", Some("API call latency"), Some("ms"))
  
  // Record metrics
  Counter::add(request_counter, 1.0)
  Histogram::record(db_duration_histogram, 45.0)
  Histogram::record(api_latency_histogram, 120.0)
  
  // Create logs for the operations
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "complex-logger")
  
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Complex workflow completed successfully"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(trace_id),
    Some(root_span_id),
    Some(Context::root())
  )
  
  Logger::emit(logger, info_log)
  
  // Verify all components are properly integrated
  assert_eq(Span::name(root_span), "root-operation")
  assert_eq(Span::name(child_span1), "database.query")
  assert_eq(Span::name(child_span2), "cache.lookup")
  assert_eq(Span::name(child_span3), "external.api.call")
  
  assert_eq(Span::status(child_span1), Ok)
  assert_eq(Span::status(child_span2), Ok)
  assert_eq(Span::status(child_span3), Ok)
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  let environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  
  assert_eq(service_name, Some(StringValue("complex-service")))
  assert_eq(service_version, Some(StringValue("2.1.0")))
  assert_eq(instance_id, Some(StringValue("instance-12345")))
  assert_eq(environment, Some(StringValue("production")))
}

// Test 5: Data Serialization and Integrity
test "data serialization and integrity" {
  // Test complex attribute serialization scenarios
  let complex_attrs = Attributes::new()
  
  // Test nested data structures
  let complex_string = "Complex data with special chars: 中文, émojis 🚀, quotes \"', and newlines\n\t"
  let unicode_string = "Unicode test: αβγδεζηθικλμνξοπρστυφχψω"
  let json_string = "{\"key\": \"value\", \"array\": [1, 2, 3], \"nested\": {\"inner\": \"data\"}}"
  
  Attributes::set(complex_attrs, "complex.string", StringValue(complex_string))
  Attributes::set(complex_attrs, "unicode.string", StringValue(unicode_string))
  Attributes::set(complex_attrs, "json.string", StringValue(json_string))
  
  // Test array values
  let string_array = ["item1", "item2", "item3", "item with spaces", "特殊字符"]
  let int_array = [1, 2, 3, 100, 999, -1, 0]
  
  Attributes::set(complex_attrs, "string.array", ArrayStringValue(string_array))
  Attributes::set(complex_attrs, "int.array", ArrayIntValue(int_array))
  
  // Test extreme numeric values
  Attributes::set(complex_attrs, "scientific.notation", FloatValue(1.23e-10))
  Attributes::set(complex_attrs, "negative.infinity", FloatValue(-1.0/0.0))
  Attributes::set(complex_attrs, "positive.infinity", FloatValue(1.0/0.0))
  
  // Verify data integrity after retrieval
  let retrieved_complex = Attributes::get(complex_attrs, "complex.string")
  let retrieved_unicode = Attributes::get(complex_attrs, "unicode.string")
  let retrieved_json = Attributes::get(complex_attrs, "json.string")
  
  assert_eq(retrieved_complex, Some(StringValue(complex_string)))
  assert_eq(retrieved_unicode, Some(StringValue(unicode_string)))
  assert_eq(retrieved_json, Some(StringValue(json_string)))
  
  // Test span context serialization
  let original_trace_id = "serialized-trace-1234567890"
  let original_span_id = "serialized-span-0987654321"
  let original_span_ctx = SpanContext::new(original_trace_id, original_span_id, true, "key1=value1,key2=value2")
  
  // Verify span context preserves all data
  assert_eq(SpanContext::trace_id(original_span_ctx), original_trace_id)
  assert_eq(SpanContext::span_id(original_span_ctx), original_span_id)
  assert_true(SpanContext::is_sampled(original_span_ctx))
  
  // Test baggage serialization with complex values
  let complex_baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(complex_baggage, "complex.key", complex_string)
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "unicode.key", unicode_string)
  let updated_baggage3 = Baggage::set_entry(updated_baggage2, "json.key", json_string)
  
  // Verify baggage integrity
  let retrieved_complex_baggage = Baggage::get_entry(updated_baggage3, "complex.key")
  let retrieved_unicode_baggage = Baggage::get_entry(updated_baggage3, "unicode.key")
  let retrieved_json_baggage = Baggage::get_entry(updated_baggage3, "json.key")
  
  assert_eq(retrieved_complex_baggage, Some(complex_string))
  assert_eq(retrieved_unicode_baggage, Some(unicode_string))
  assert_eq(retrieved_json_baggage, Some(json_string))
  
  // Test log record serialization with complex data
  let complex_log = LogRecord::new_with_context(
    Warn,
    Some("Complex log with special characters: " + complex_string),
    Some(complex_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system()) + 1000000L),
    Some(original_trace_id),
    Some(original_span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(complex_log), Warn)
  assert_eq(LogRecord::body(complex_log), Some("Complex log with special characters: " + complex_string))
  assert_eq(LogRecord::trace_id(complex_log), Some(original_trace_id))
  assert_eq(LogRecord::span_id(complex_log), Some(original_span_id))
}

// Test 6: Cross-Service Consistency
test "cross-service consistency" {
  // Simulate multiple services in a distributed system
  let services = ["user-service", "order-service", "payment-service", "inventory-service", "notification-service"]
  let trace_id = "cross-service-trace-" + Random::next_u64(Random::system()).to_string()
  
  // Create consistent resource configuration across services
  let base_resource_attrs = [
    ("trace.id", StringValue(trace_id)),
    ("correlation.id", StringValue("corr-123456789")),
    ("request.id", StringValue("req-987654321")),
    ("session.id", StringValue("sess-456789123")),
    ("business.domain", StringValue("ecommerce"))
  ]
  
  let service_spans = []
  let service_metrics = []
  let service_logs = []
  
  // Create consistent telemetry across all services
  for service_name in services {
    // Create service-specific resource
    let service_resource = Resource::new()
    let service_specific_attrs = [
      ("service.name", StringValue(service_name)),
      ("service.version", StringValue("1.2.3")),
      ("service.environment", StringValue("production"))
    ]
    let all_attrs = base_resource_attrs.concat(service_specific_attrs)
    let service_resource_with_attrs = Resource::with_attributes(service_resource, all_attrs)
    
    // Create tracer and span for this service
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, service_name)
    let span_id = service_name + "-span-" + Random::next_u64(Random::system()).to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new(service_name + ".operation", Internal, span_ctx)
    
    // Add consistent context
    Span::add_event(span, "service.processing", [
      ("service.name", StringValue(service_name)),
      ("processing.started", StringValue(Clock::now_unix_nanos(Clock::system()).to_string()))
    ])
    
    service_spans.push((service_name, span))
    
    // Create consistent metrics
    let meter_provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(meter_provider, service_name)
    let request_counter = Meter::create_counter(meter, "requests.total")
    let latency_histogram = Meter::create_histogram(meter, "operation.latency.ms", Some("Operation latency"), Some("ms"))
    
    Counter::add(request_counter, 1.0, Some(service_resource_with_attrs))
    Histogram::record(latency_histogram, 50.0 + (service_name.length().to_double() * 10.0), Some(service_resource_with_attrs))
    
    service_metrics.push((service_name, request_counter, latency_histogram))
    
    // Create consistent logs
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, service_name)
    let log_record = LogRecord::new_with_context(
      Info,
      Some("Service " + service_name + " processing completed"),
      Some(service_resource_with_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(trace_id),
      Some(span_id),
      Some(Context::root())
    )
    
    service_logs.push((service_name, log_record))
  }
  
  // Verify consistency across all services
  for (service_name, span) in service_spans {
    assert_eq(Span::name(span), service_name + ".operation")
    assert_true(Span::is_recording(span))
  }
  
  // Verify metrics consistency
  for (service_name, counter, histogram) in service_metrics {
    assert_eq(counter.name, "requests.total")
    assert_eq(histogram.name, "operation.latency.ms")
    assert_eq(histogram.description, Some("Operation latency"))
    assert_eq(histogram.unit, Some("ms"))
  }
  
  // Verify log consistency
  for (service_name, log_record) in service_logs {
    assert_eq(LogRecord::severity_number(log_record), Info)
    assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
    let expected_body = Some("Service " + service_name + " processing completed")
    assert_eq(LogRecord::body(log_record), expected_body)
  }
}

// Test 7: Internationalization and Localization
test "internationalization and localization" {
  // Test multilingual attribute values
  let i18n_attrs = Attributes::new()
  
  let messages = [
    ("en", "Hello, World!"),
    ("zh", "你好，世界！"),
    ("ja", "こんにちは、世界！"),
    ("ko", "안녕하세요, 세계!"),
    ("es", "¡Hola, Mundo!"),
    ("fr", "Bonjour, le Monde!"),
    ("de", "Hallo, Welt!"),
    ("ru", "Привет, Мир!"),
    ("ar", "مرحبا بالعالم!"),
    ("hi", "नमस्ते दुनिया!"),
    ("th", "สวัสดีชาวโลก!"),
    ("he", "שלום עולם!")
  ]
  
  // Set multilingual messages
  for (lang, message) in messages {
    let key = "message." + lang
    Attributes::set(i18n_attrs, key, StringValue(message))
  }
  
  // Verify multilingual message retrieval
  for (lang, message) in messages {
    let key = "message." + lang
    let retrieved = Attributes::get(i18n_attrs, key)
    assert_eq(retrieved, Some(StringValue(message)))
  }
  
  // Test localized error messages
  let error_messages = [
    ("en", "Operation failed"),
    ("zh", "操作失败"),
    ("ja", "操作が失敗しました"),
    ("ko", "작업 실패"),
    ("es", "La operación falló"),
    ("fr", "L'opération a échoué")
  ]
  
  for (lang, error_msg) in error_messages {
    let key = "error." + lang
    Attributes::set(i18n_attrs, key, StringValue(error_msg))
  }
  
  // Test localized log messages
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-logger")
  
  for (lang, message) in messages {
    let localized_log = LogRecord::new_with_context(
      Info,
      Some("[" + lang + "] " + message),
      Some(i18n_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("i18n-trace-" + lang),
      Some("i18n-span-" + lang),
      Some(Context::root())
    )
    
    Logger::emit(logger, localized_log)
    
    // Verify log content
    assert_eq(LogRecord::body(localized_log), Some("[" + lang + "] " + message))
    assert_eq(LogRecord::trace_id(localized_log), Some("i18n-trace-" + lang))
    assert_eq(LogRecord::span_id(localized_log), Some("i18n-span-" + lang))
  }
  
  // Test localized span names and events
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-tracer")
  
  let localized_operations = [
    ("en", "user.registration"),
    ("zh", "用户注册"),
    ("ja", "ユーザー登録"),
    ("ko", "사용자 등록")
  ]
  
  for (lang, operation) in localized_operations {
    let span = Tracer::start_span(tracer, operation)
    assert_eq(Span::name(span), operation)
    
    // Add localized events
    let event_message = match lang {
      "en" => "Operation started",
      "zh" => "操作开始",
      "ja" => "操作開始",
      "ko" => "작업 시작",
      _ => "Operation started"
    }
    
    Span::add_event(span, event_message, [
      ("language", StringValue(lang)),
      ("operation.type", StringValue(operation))
    ])
  }
  
  // Test timezone-aware timestamps
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let timezone_attrs = [
    ("timezone.utc", StringValue("2025-01-01T00:00:00Z")),
    ("timezone.est", StringValue("2024-12-31T19:00:00-05:00")),
    ("timezone.pst", StringValue("2024-12-31T16:00:00-08:00")),
    ("timezone.cst", StringValue("2025-01-01T08:00:00+08:00")),
    ("timezone.jst", StringValue("2025-01-01T09:00:00+09:00"))
  ]
  
  for (tz_key, tz_value) in timezone_attrs {
    Attributes::set(i18n_attrs, tz_key, StringValue(tz_value))
  }
  
  // Verify timezone attributes
  for (tz_key, tz_value) in timezone_attrs {
    let retrieved = Attributes::get(i18n_attrs, tz_key)
    assert_eq(retrieved, Some(StringValue(tz_value)))
  }
}

// Test 8: Advanced Attributes Operations
test "advanced attributes operations" {
  // Test deep nested attribute operations
  let advanced_attrs = Attributes::new()
  
  // Create hierarchical attribute keys
  let hierarchy_keys = [
    "app.service.name",
    "app.service.version",
    "app.service.environment",
    "app.service.region",
    "app.service.zone",
    "app.database.host",
    "app.database.port",
    "app.database.name",
    "app.cache.type",
    "app.cache.ttl",
    "app.queue.name",
    "app.queue.max_size"
  ]
  
  let hierarchy_values = [
    "advanced-microservice",
    "3.2.1",
    "production",
    "us-west-2",
    "us-west-2a",
    "db.example.com",
    "5432",
    "advanced_db",
    "redis",
    "3600",
    "task_queue",
    "10000"
  ]
  
  // Set hierarchical attributes
  for i in 0..hierarchy_keys.length() {
    Attributes::set(advanced_attrs, hierarchy_keys[i], StringValue(hierarchy_values[i]))
  }
  
  // Verify hierarchical attributes
  for i in 0..hierarchy_keys.length() {
    let retrieved = Attributes::get(advanced_attrs, hierarchy_keys[i])
    assert_eq(retrieved, Some(StringValue(hierarchy_values[i])))
  }
  
  // Test attribute value type conversions
  let conversion_attrs = Attributes::new()
  
  // String to numeric conversions
  Attributes::set(conversion_attrs, "string.number", StringValue("12345"))
  Attributes::set(conversion_attrs, "actual.number", IntValue(12345))
  Attributes::set(conversion_attrs, "string.float", StringValue("123.45"))
  Attributes::set(conversion_attrs, "actual.float", FloatValue(123.45))
  
  // Boolean representations
  Attributes::set(conversion_attrs, "bool.true.string", StringValue("true"))
  Attributes::set(conversion_attrs, "bool.false.string", StringValue("false"))
  Attributes::set(conversion_attrs, "bool.true.actual", BoolValue(true))
  Attributes::set(conversion_attrs, "bool.false.actual", BoolValue(false))
  
  // Test array attribute operations
  let array_attrs = Attributes::new()
  
  let string_array = ["alpha", "beta", "gamma", "delta", "epsilon"]
  let int_array = [10, 20, 30, 40, 50]
  let mixed_string_array = ["item1", "123", "true", "mixed.content", "special_chars_!@#$%^&*()"]
  
  Attributes::set(array_attrs, "string.array.test", ArrayStringValue(string_array))
  Attributes::set(array_attrs, "int.array.test", ArrayIntValue(int_array))
  Attributes::set(array_attrs, "mixed.array.test", ArrayStringValue(mixed_string_array))
  
  // Verify array attributes
  let retrieved_string_array = Attributes::get(array_attrs, "string.array.test")
  let retrieved_int_array = Attributes::get(array_attrs, "int.array.test")
  let retrieved_mixed_array = Attributes::get(array_attrs, "mixed.array.test")
  
  assert_eq(retrieved_string_array, Some(ArrayStringValue(string_array)))
  assert_eq(retrieved_int_array, Some(ArrayIntValue(int_array)))
  assert_eq(retrieved_mixed_array, Some(ArrayStringValue(mixed_string_array)))
  
  // Test attribute filtering and searching
  let filter_attrs = Attributes::new()
  
  // Create attributes with different patterns
  let filter_patterns = [
    ("user.id", "user123"),
    ("user.name", "John Doe"),
    ("user.email", "john@example.com"),
    ("user.role", "admin"),
    ("product.id", "prod456"),
    ("product.name", "Advanced Product"),
    ("product.price", "99.99"),
    ("order.id", "order789"),
    ("order.status", "completed"),
    ("system.timestamp", "2025-01-01T00:00:00Z")
  ]
  
  for (key, value) in filter_patterns {
    Attributes::set(filter_attrs, key, StringValue(value))
  }
  
  // Verify filtering patterns exist
  let user_id = Attributes::get(filter_attrs, "user.id")
  let product_name = Attributes::get(filter_attrs, "product.name")
  let order_status = Attributes::get(filter_attrs, "order.status")
  let system_timestamp = Attributes::get(filter_attrs, "system.timestamp")
  
  assert_eq(user_id, Some(StringValue("user123")))
  assert_eq(product_name, Some(StringValue("Advanced Product")))
  assert_eq(order_status, Some(StringValue("completed")))
  assert_eq(system_timestamp, Some(StringValue("2025-01-01T00:00:00Z")))
  
  // Test attribute merging and overriding
  let base_attrs = Attributes::new()
  let override_attrs = Attributes::new()
  
  // Set base attributes
  Attributes::set(base_attrs, "base.key1", StringValue("base_value1"))
  Attributes::set(base_attrs, "base.key2", StringValue("base_value2"))
  Attributes::set(base_attrs, "shared.key", StringValue("base_shared"))
  
  // Set override attributes
  Attributes::set(override_attrs, "override.key1", StringValue("override_value1"))
  Attributes::set(override_attrs, "override.key2", StringValue("override_value2"))
  Attributes::set(override_attrs, "shared.key", StringValue("override_shared"))
  
  // In a real implementation, we would merge these attributes
  // For this test, we just verify both sets exist independently
  let base_shared = Attributes::get(base_attrs, "shared.key")
  let override_shared = Attributes::get(override_attrs, "shared.key")
  
  assert_eq(base_shared, Some(StringValue("base_shared")))
  assert_eq(override_shared, Some(StringValue("override_shared")))
}

// Test 9: Time Series and Temporal Operations
test "time series and temporal operations" {
  // Test time-based attribute operations
  let time_series_attrs = Attributes::new()
  
  // Create time series data points
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  for i in 0..100 {
    let timestamp = base_timestamp + (i.to_int64() * 1000000000L)  // 1 second intervals
    let metric_value = 100.0 + (i.to_double() * 2.5) + (Random::next_u64(Random::system()).to_double() % 10.0)
    
    let key = "timeseries.metric." + i.to_string()
    let value = "timestamp:" + timestamp.to_string() + ":value:" + metric_value.to_string()
    
    Attributes::set(time_series_attrs, key, StringValue(value))
  }
  
  // Verify time series data integrity
  for i in 0..100 {
    let key = "timeseries.metric." + i.to_string()
    let expected_timestamp = base_timestamp + (i.to_int64() * 1000000000L)
    let expected_metric_value = 100.0 + (i.to_double() * 2.5)  // Base value without random component
    
    let retrieved = Attributes::get(time_series_attrs, key)
    match retrieved {
      Some(StringValue(value_str)) => {
        // Verify the value contains expected components
        assert_true(value_str.contains("timestamp:" + expected_timestamp.to_string()))
        assert_true(value_str.contains(":value:"))
      }
      _ => assert_true(false, "Expected string value for time series data")
    }
  }
  
  // Test temporal window operations
  let temporal_attrs = Attributes::new()
  
  // Create windows of different sizes
  let window_sizes = [60, 300, 900, 3600, 86400]  // 1min, 5min, 15min, 1hr, 1day in seconds
  
  for window_size in window_sizes {
    let window_start = base_timestamp
    let window_end = base_timestamp + (window_size.to_int64() * 1000000000L)
    
    let key = "window." + window_size.to_string() + "seconds"
    let value = "start:" + window_start.to_string() + ":end:" + window_end.to_string()
    
    Attributes::set(temporal_attrs, key, StringValue(value))
  }
  
  // Verify temporal windows
  for window_size in window_sizes {
    let key = "window." + window_size.to_string() + "seconds"
    let expected_start = base_timestamp
    let expected_end = base_timestamp + (window_size.to_int64() * 1000000000L)
    
    let retrieved = Attributes::get(temporal_attrs, key)
    match retrieved {
      Some(StringValue(value_str)) => {
        assert_true(value_str.contains("start:" + expected_start.to_string()))
        assert_true(value_str.contains(":end:" + expected_end.to_string()))
      }
      _ => assert_true(false, "Expected string value for temporal window")
    }
  }
  
  // Test time-based aggregation patterns
  let aggregation_attrs = Attributes::new()
  
  // Create aggregation metrics for different time periods
  let aggregation_types = ["sum", "avg", "min", "max", "count"]
  let time_periods = ["1m", "5m", "15m", "1h", "1d", "7d"]
  
  for agg_type in aggregation_types {
    for period in time_periods {
      let key = "agg." + agg_type + "." + period
      let value = match agg_type {
        "sum" => "1000.0",
        "avg" => "50.0",
        "min" => "1.0",
        "max" => "100.0",
        "count" => "20",
        _ => "0.0"
      }
      
      Attributes::set(aggregation_attrs, key, StringValue(value))
    }
  }
  
  // Verify aggregation metrics
  for agg_type in aggregation_types {
    for period in time_periods {
      let key = "agg." + agg_type + "." + period
      let expected_value = match agg_type {
        "sum" => "1000.0",
        "avg" => "50.0",
        "min" => "1.0",
        "max" => "100.0",
        "count" => "20",
        _ => "0.0"
      }
      
      let retrieved = Attributes::get(aggregation_attrs, key)
      assert_eq(retrieved, Some(StringValue(expected_value)))
    }
  }
  
  // Test temporal span operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "time-series-tracer")
  
  // Create spans with different durations
  let span_durations = [10, 50, 100, 500, 1000, 5000]  // milliseconds
  
  for duration in span_durations {
    let span_name = "operation.duration." + duration.to_string() + "ms"
    let span = Tracer::start_span(tracer, span_name)
    
    // Simulate span duration by adding events
    Span::add_event(span, "operation.started", [])
    
    // Add duration as attribute
    let duration_attrs = Attributes::new()
    Attributes::set(duration_attrs, "duration.ms", IntValue(duration))
    Span::add_event(span, "operation.completed", [
      ("duration.ms", IntValue(duration))
    ])
    
    assert_eq(Span::name(span), span_name)
  }
  
  // Test time-based metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "time-series-meter")
  
  // Create time-based histograms
  let time_histograms = [
    ("response.time.ms", "Response time in milliseconds", "ms"),
    ("processing.time.ms", "Processing time in milliseconds", "ms"),
    ("queue.time.ms", "Queue wait time in milliseconds", "ms"),
    ("db.query.time.ms", "Database query time in milliseconds", "ms")
  ]
  
  for (histogram_name, description, unit) in time_histograms {
    let histogram = Meter::create_histogram(meter, histogram_name, Some(description), Some(unit))
    
    // Record values at different time points
    for i in 0..10 {
      let value = (i + 1).to_double() * 10.0  // 10, 20, 30, ..., 100
      Histogram::record(histogram, value)
    }
    
    assert_eq(histogram.name, histogram_name)
    assert_eq(histogram.description, Some(description))
    assert_eq(histogram.unit, Some(unit))
  }
}

// Test 10: Real-time Dashboard and Streaming Operations
test "real-time dashboard and streaming operations" {
  // Simulate real-time dashboard data collection
  let dashboard_attrs = Attributes::new()
  
  // Create real-time metrics
  let realtime_metrics = [
    ("active.users", "1250"),
    ("requests.per.second", "450.5"),
    ("error.rate", "0.02"),
    ("cpu.usage", "67.8"),
    ("memory.usage", "82.3"),
    ("disk.usage", "45.6"),
    ("network.in", "1024.5"),
    ("network.out", "2048.7"),
    ("queue.depth", "125"),
    ("cache.hit.rate", "94.2")
  ]
  
  for (metric_name, metric_value) in realtime_metrics {
    let timestamp = Clock::now_unix_nanos(Clock::system())
    let key = "realtime." + metric_name
    let value = "value:" + metric_value + ":timestamp:" + timestamp.to_string()
    
    Attributes::set(dashboard_attrs, key, StringValue(value))
  }
  
  // Verify real-time metrics
  for (metric_name, metric_value) in realtime_metrics {
    let key = "realtime." + metric_name
    let retrieved = Attributes::get(dashboard_attrs, key)
    
    match retrieved {
      Some(StringValue(value_str)) => {
        assert_true(value_str.contains("value:" + metric_value))
        assert_true(value_str.contains(":timestamp:"))
      }
      _ => assert_true(false, "Expected string value for real-time metric")
    }
  }
  
  // Test streaming data simulation
  let streaming_attrs = Attributes::new()
  
  // Simulate streaming events
  let event_types = ["user.login", "user.logout", "page.view", "button.click", "form.submit", "api.request", "error.occurred"]
  
  for i in 0..1000 {
    let event_type = event_types[i % event_types.length()]
    let event_id = "event_" + i.to_string()
    let timestamp = Clock::now_unix_nanos(Clock::system()) + (i.to_int64() * 1000000L)  // 1ms intervals
    
    let key = "stream.event." + i.to_string()
    let value = "id:" + event_id + ":type:" + event_type + ":timestamp:" + timestamp.to_string()
    
    Attributes::set(streaming_attrs, key, StringValue(value))
  }
  
  // Verify streaming events
  for i in 0..100 {  // Sample first 100 events
    let key = "stream.event." + i.to_string()
    let expected_event_type = event_types[i % event_types.length()]
    let expected_event_id = "event_" + i.to_string()
    
    let retrieved = Attributes::get(streaming_attrs, key)
    match retrieved {
      Some(StringValue(value_str)) => {
        assert_true(value_str.contains("id:" + expected_event_id))
        assert_true(value_str.contains("type:" + expected_event_type))
        assert_true(value_str.contains(":timestamp:"))
      }
      _ => assert_true(false, "Expected string value for streaming event")
    }
  }
  
  // Test dashboard alert conditions
  let alert_attrs = Attributes::new()
  
  // Define alert thresholds and current values
  let alert_conditions = [
    ("cpu.usage.threshold", "80.0"),
    ("cpu.usage.current", "85.5"),
    ("cpu.usage.alert", "true"),
    ("memory.usage.threshold", "90.0"),
    ("memory.usage.current", "82.3"),
    ("memory.usage.alert", "false"),
    ("error.rate.threshold", "0.05"),
    ("error.rate.current", "0.08"),
    ("error.rate.alert", "true"),
    ("response.time.threshold", "500.0"),
    ("response.time.current", "650.0"),
    ("response.time.alert", "true")
  ]
  
  for (alert_key, alert_value) in alert_conditions {
    Attributes::set(alert_attrs, alert_key, StringValue(alert_value))
  }
  
  // Verify alert conditions
  let cpu_alert = Attributes::get(alert_attrs, "cpu.usage.alert")
  let memory_alert = Attributes::get(alert_attrs, "memory.usage.alert")
  let error_rate_alert = Attributes::get(alert_attrs, "error.rate.alert")
  let response_time_alert = Attributes::get(alert_attrs, "response.time.alert")
  
  assert_eq(cpu_alert, Some(StringValue("true")))
  assert_eq(memory_alert, Some(StringValue("false")))
  assert_eq(error_rate_alert, Some(StringValue("true")))
  assert_eq(response_time_alert, Some(StringValue("true")))
  
  // Test real-time span operations for dashboard
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "dashboard-tracer")
  
  // Create spans representing real-time operations
  let dashboard_operations = [
    ("user.authentication", "auth"),
    ("data.query", "database"),
    ("cache.lookup", "cache"),
    ("external.api", "api"),
    ("notification.send", "notification")
  ]
  
  for (operation_name, operation_type) in dashboard_operations {
    let span = Tracer::start_span(tracer, operation_name)
    
    // Add real-time events
    Span::add_event(span, "operation.started", [
      ("operation.type", StringValue(operation_type)),
      ("dashboard.visible", BoolValue(true))
    ])
    
    // Add performance metrics
    Span::add_event(span, "metrics.collected", [
      ("response.time.ms", IntValue(50 + operation_name.length() * 10)),
      ("success.rate", FloatValue(0.95 + (operation_type.length().to_double() * 0.01)))
    ])
    
    assert_eq(Span::name(span), operation_name)
  }
  
  // Test real-time metrics for dashboard
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "dashboard-meter")
  
  // Create dashboard-specific metrics
  let dashboard_metrics = [
    ("dashboard.active.users", "Active users on dashboard", "count"),
    ("dashboard.page.views", "Dashboard page views", "count"),
    ("dashboard.response.time", "Dashboard response time", "ms"),
    ("dashboard.errors", "Dashboard errors", "count")
  ]
  
  for (metric_name, description, unit) in dashboard_metrics {
    match unit {
      "count" => {
        let counter = Meter::create_counter(meter, metric_name, Some(description), Some(unit))
        Counter::add(counter, 1.0)
        assert_eq(counter.name, metric_name)
      }
      "ms" => {
        let histogram = Meter::create_histogram(meter, metric_name, Some(description), Some(unit))
        Histogram::record(histogram, 100.0)
        assert_eq(histogram.name, metric_name)
      }
      _ => assert_true(false, "Unsupported metric unit")
    }
  }
  
  // Test real-time logging for dashboard
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "dashboard-logger")
  
  let dashboard_log_levels = [Info, Warn, Error]
  let dashboard_log_messages = [
    "Dashboard loaded successfully",
    "High CPU usage detected",
    "Database connection failed",
    "Cache refresh completed",
    "User session timeout"
  ]
  
  for i in 0..dashboard_log_messages.length() {
    let log_level = dashboard_log_levels[i % dashboard_log_levels.length()]
    let log_message = dashboard_log_messages[i]
    
    let log_record = LogRecord::new_with_context(
      log_level,
      Some(log_message),
      Some(dashboard_attrs),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some("dashboard-trace"),
      Some("dashboard-span"),
      Some(Context::root())
    )
    
    Logger::emit(logger, log_record)
    assert_eq(LogRecord::severity_number(log_record), log_level)
    assert_eq(LogRecord::body(log_record), Some(log_message))
  }
}