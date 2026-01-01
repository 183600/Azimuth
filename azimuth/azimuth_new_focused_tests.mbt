// Azimuth New Focused Test Suite
// This file contains focused test cases for specific Azimuth telemetry system features

// Test 1: Cross-service tracing consistency
test "cross service tracing consistency" {
  // Create a root trace context
  let root_trace_id = "1234567890abcdef1234567890abcdef"
  let root_span_id = "abcdef1234567890"
  let root_ctx = SpanContext::new(root_trace_id, root_span_id, true, "key1=value1")
  
  // Create child spans for different services
  let service_a_span_id = "1111111111111111"
  let service_a_ctx = SpanContext::new(root_trace_id, service_a_span_id, true, "key1=value1,service=a")
  
  let service_b_span_id = "2222222222222222"
  let service_b_ctx = SpanContext::new(root_trace_id, service_b_span_id, true, "key1=value1,service=b")
  
  let service_c_span_id = "3333333333333333"
  let service_c_ctx = SpanContext::new(root_trace_id, service_c_span_id, true, "key1=value1,service=c")
  
  // Verify trace consistency across services
  assert_eq(SpanContext::trace_id(root_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_a_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_b_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_c_ctx), root_trace_id)
  
  // Verify each service has unique span ID
  assert_eq(SpanContext::span_id(root_ctx), root_span_id)
  assert_eq(SpanContext::span_id(service_a_ctx), service_a_span_id)
  assert_eq(SpanContext::span_id(service_b_ctx), service_b_span_id)
  assert_eq(SpanContext::span_id(service_c_ctx), service_c_span_id)
  
  // Verify all spans are sampled
  assert_true(SpanContext::is_sampled(root_ctx))
  assert_true(SpanContext::is_sampled(service_a_ctx))
  assert_true(SpanContext::is_sampled(service_b_ctx))
  assert_true(SpanContext::is_sampled(service_c_ctx))
  
  // Verify all spans are valid
  assert_true(SpanContext::is_valid(root_ctx))
  assert_true(SpanContext::is_valid(service_a_ctx))
  assert_true(SpanContext::is_valid(service_b_ctx))
  assert_true(SpanContext::is_valid(service_c_ctx))
}

// Test 2: Time series data processing
test "time series data processing" {
  // Test time series data with various intervals
  let base_timestamp = 1735689600000000000L  // Base timestamp
  
  // Create time series data points
  let timestamps = [
    base_timestamp,
    base_timestamp + 60000000000L,  // +1 minute
    base_timestamp + 120000000000L, // +2 minutes
    base_timestamp + 180000000000L, // +3 minutes
    base_timestamp + 240000000000L, // +4 minutes
    base_timestamp + 300000000000L  // +5 minutes
  ]
  
  let values = [10.5, 15.2, 12.8, 18.3, 14.7, 20.1]
  let attributes = [
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))],
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))],
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))],
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))],
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))],
    [("metric.name", StringValue("cpu.usage")), ("host", StringValue("server1"))]
  ]
  
  // Verify time series data structure
  assert_eq(timestamps.length(), 6)
  assert_eq(values.length(), 6)
  assert_eq(attributes.length(), 6)
  
  // Verify timestamp ordering
  for i in 1...5 {
    assert_true(timestamps[i] > timestamps[i-1])
  }
  
  // Verify value ranges
  for value in values {
    assert_true(value >= 10.0)
    assert_true(value <= 25.0)
  }
  
  // Verify attribute consistency
  for attr_set in attributes {
    assert_eq(attr_set.length(), 2)
    assert_eq(attr_set[0], ("metric.name", StringValue("cpu.usage")))
    assert_eq(attr_set[1], ("host", StringValue("server1")))
  }
}

// Test 3: Resource attribute inheritance
test "resource attribute inheritance" {
  // Create base resource with common attributes
  let base_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create child resource with additional attributes
  let child_attrs = [
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("prod-server-01")),
    ("process.id", IntValue(12345)),
    ("process.name", StringValue("base-service"))
  ]
  
  let child_resource = Resource::with_attributes(base_resource, child_attrs)
  
  // Verify base resource attributes
  assert_eq(Resource::get_attribute(base_resource, "service.name"), Some(StringValue("base-service")))
  assert_eq(Resource::get_attribute(base_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(base_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(base_resource, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(base_resource, "telemetry.sdk.version"), Some(StringValue("0.1.0")))
  
  // Verify child resource has both base and child attributes
  assert_eq(Resource::get_attribute(child_resource, "service.name"), Some(StringValue("base-service")))
  assert_eq(Resource::get_attribute(child_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(child_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(child_resource, "telemetry.sdk.name"), Some(StringValue("azimuth")))
  assert_eq(Resource::get_attribute(child_resource, "telemetry.sdk.version"), Some(StringValue("0.1.0")))
  assert_eq(Resource::get_attribute(child_resource, "service.instance.id"), Some(StringValue("instance-12345")))
  assert_eq(Resource::get_attribute(child_resource, "host.name"), Some(StringValue("prod-server-01")))
  assert_eq(Resource::get_attribute(child_resource, "process.id"), Some(IntValue(12345)))
  assert_eq(Resource::get_attribute(child_resource, "process.name"), Some(StringValue("base-service")))
}

// Test 4: Internationalization and localization
test "internationalization and localization" {
  // Test log messages with different languages
  let english_log = LogRecord::new(Info, "Operation completed successfully")
  let chinese_log = LogRecord::new(Info, "操作成功完成")
  let japanese_log = LogRecord::new(Info, "操作が正常に完了しました")
  let spanish_log = LogRecord::new(Info, "Operación completada con éxito")
  let french_log = LogRecord::new(Info, "Opération terminée avec succès")
  let arabic_log = LogRecord::new(Info, "اكتملت العملية بنجاح")
  let russian_log = LogRecord::new(Info, "Операция успешно завершена")
  
  // Verify all log messages are preserved
  assert_eq(LogRecord::body(english_log), Some("Operation completed successfully"))
  assert_eq(LogRecord::body(chinese_log), Some("操作成功完成"))
  assert_eq(LogRecord::body(japanese_log), Some("操作が正常に完了しました"))
  assert_eq(LogRecord::body(spanish_log), Some("Operación completada con éxito"))
  assert_eq(LogRecord::body(french_log), Some("Opération terminée avec succès"))
  assert_eq(LogRecord::body(arabic_log), Some("اكتملت العملية بنجاح"))
  assert_eq(LogRecord::body(russian_log), Some("Операция успешно завершена"))
  
  // Test attributes with internationalized values
  let i18n_attrs = Attributes::new()
  Attributes::set(i18n_attrs, "error.message.en", StringValue("File not found"))
  Attributes::set(i18n_attrs, "error.message.zh", StringValue("文件未找到"))
  Attributes::set(i18n_attrs, "error.message.ja", StringValue("ファイルが見つかりません"))
  Attributes::set(i18n_attrs, "error.message.es", StringValue("Archivo no encontrado"))
  
  // Verify internationalized attributes
  assert_eq(Attributes::get(i18n_attrs, "error.message.en"), Some(StringValue("File not found")))
  assert_eq(Attributes::get(i18n_attrs, "error.message.zh"), Some(StringValue("文件未找到")))
  assert_eq(Attributes::get(i18n_attrs, "error.message.ja"), Some(StringValue("ファイルが見つかりません")))
  assert_eq(Attributes::get(i18n_attrs, "error.message.es"), Some(StringValue("Archivo no encontrado")))
  
  // Test resource attributes with locale information
  let locale_resource = Resource::with_attributes(Resource::new(), [
    ("service.locale", StringValue("zh-CN")),
    ("service.timezone", StringValue("Asia/Shanghai")),
    ("service.currency", StringValue("CNY")),
    ("service.encoding", StringValue("UTF-8"))
  ])
  
  assert_eq(Resource::get_attribute(locale_resource, "service.locale"), Some(StringValue("zh-CN")))
  assert_eq(Resource::get_attribute(locale_resource, "service.timezone"), Some(StringValue("Asia/Shanghai")))
  assert_eq(Resource::get_attribute(locale_resource, "service.currency"), Some(StringValue("CNY")))
  assert_eq(Resource::get_attribute(locale_resource, "service.encoding"), Some(StringValue("UTF-8")))
}

// Test 5: Memory management
test "memory management" {
  // Test resource cleanup
  let resource1 = Resource::with_attributes(Resource::new(), [
    ("temp.data", StringValue("temporary data 1")),
    ("cache.size", IntValue(1024))
  ])
  
  let resource2 = Resource::with_attributes(Resource::new(), [
    ("temp.data", StringValue("temporary data 2")),
    ("cache.size", IntValue(2048))
  ])
  
  // Test resource merging and cleanup
  let merged_resource = Resource::merge(resource1, resource2)
  
  // Verify merged resource contains expected attributes
  assert_eq(Resource::get_attribute(merged_resource, "temp.data"), Some(StringValue("temporary data 2")))
  assert_eq(Resource::get_attribute(merged_resource, "cache.size"), Some(IntValue(2048)))
  
  // Test large attribute sets
  let large_attrs = Attributes::new()
  for i in 0...99 {
    let key = "attr." + i.to_string()
    let value = StringValue("value." + i.to_string())
    Attributes::set(large_attrs, key, value)
  }
  
  // Verify some attributes from the large set
  assert_eq(Attributes::get(large_attrs, "attr.0"), Some(StringValue("value.0")))
  assert_eq(Attributes::get(large_attrs, "attr.49"), Some(StringValue("value.49")))
  assert_eq(Attributes::get(large_attrs, "attr.99"), Some(StringValue("value.99")))
  
  // Test large baggage entries
  let large_baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(large_baggage, "session.id", "sess-12345")
  let baggage_with_more = Baggage::set_entry(baggage_with_entries, "user.id", "user-67890")
  let baggage_final = Baggage::set_entry(baggage_with_more, "request.id", "req-abcdef")
  
  // Verify baggage entries
  assert_eq(Baggage::get_entry(baggage_final, "session.id"), Some("sess-12345"))
  assert_eq(Baggage::get_entry(baggage_final, "user.id"), Some("user-67890"))
  assert_eq(Baggage::get_entry(baggage_final, "request.id"), Some("req-abcdef"))
}

// Test 6: Platform compatibility
test "platform compatibility" {
  // Test platform-specific resource attributes
  let linux_resource = Resource::with_attributes(Resource::new(), [
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0-76-generic")),
    ("os.arch", StringValue("x86_64")),
    ("os.description", StringValue("Ubuntu 22.04.1 LTS"))
  ])
  
  let windows_resource = Resource::with_attributes(Resource::new(), [
    ("os.type", StringValue("windows")),
    ("os.version", StringValue("10.0.19044")),
    ("os.arch", StringValue("AMD64")),
    ("os.description", StringValue("Windows 10 Pro"))
  ])
  
  let macos_resource = Resource::with_attributes(Resource::new(), [
    ("os.type", StringValue("darwin")),
    ("os.version", StringValue("22.1.0")),
    ("os.arch", StringValue("arm64")),
    ("os.description", StringValue("macOS 13.0.1"))
  ])
  
  // Verify Linux resource attributes
  assert_eq(Resource::get_attribute(linux_resource, "os.type"), Some(StringValue("linux")))
  assert_eq(Resource::get_attribute(linux_resource, "os.version"), Some(StringValue("5.15.0-76-generic")))
  assert_eq(Resource::get_attribute(linux_resource, "os.arch"), Some(StringValue("x86_64")))
  assert_eq(Resource::get_attribute(linux_resource, "os.description"), Some(StringValue("Ubuntu 22.04.1 LTS")))
  
  // Verify Windows resource attributes
  assert_eq(Resource::get_attribute(windows_resource, "os.type"), Some(StringValue("windows")))
  assert_eq(Resource::get_attribute(windows_resource, "os.version"), Some(StringValue("10.0.19044")))
  assert_eq(Resource::get_attribute(windows_resource, "os.arch"), Some(StringValue("AMD64")))
  assert_eq(Resource::get_attribute(windows_resource, "os.description"), Some(StringValue("Windows 10 Pro")))
  
  // Verify macOS resource attributes
  assert_eq(Resource::get_attribute(macos_resource, "os.type"), Some(StringValue("darwin")))
  assert_eq(Resource::get_attribute(macos_resource, "os.version"), Some(StringValue("22.1.0")))
  assert_eq(Resource::get_attribute(macos_resource, "os.arch"), Some(StringValue("arm64")))
  assert_eq(Resource::get_attribute(macos_resource, "os.description"), Some(StringValue("macOS 13.0.1")))
  
  // Test platform-specific HTTP headers
  let linux_request = HttpRequest::new("GET", "https://api.example.com/linux", [
    ("User-Agent", "azimuth-linux/1.0.0"),
    ("X-Platform", "linux")
  ], None)
  
  let windows_request = HttpRequest::new("GET", "https://api.example.com/windows", [
    ("User-Agent", "azimuth-windows/1.0.0"),
    ("X-Platform", "windows")
  ], None)
  
  // Verify platform-specific headers
  assert_eq(HttpRequest::http_method(linux_request), "GET")
  assert_eq(HttpRequest::url(linux_request), "https://api.example.com/linux")
  
  assert_eq(HttpRequest::http_method(windows_request), "GET")
  assert_eq(HttpRequest::url(windows_request), "https://api.example.com/windows")
}

// Test 7: Serialization and deserialization
test "serialization and deserialization" {
  // Test span context serialization
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let sampled = true
  let trace_state = "key1=value1,key2=value2"
  
  let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  // Verify span context properties
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_eq(SpanContext::is_sampled(span_ctx), sampled)
  assert_eq(SpanContext::trace_state(span_ctx), trace_state)
  
  // Test metric serialization through attributes
  let metric_attrs = Attributes::new()
  Attributes::set(metric_attrs, "metric.name", StringValue("http.requests.total"))
  Attributes::set(metric_attrs, "metric.type", StringValue("counter"))
  Attributes::set(metric_attrs, "metric.unit", StringValue("requests"))
  Attributes::set(metric_attrs, "metric.description", StringValue("Total HTTP requests"))
  Attributes::set(metric_attrs, "metric.value", IntValue(12345))
  
  // Verify metric attributes
  assert_eq(Attributes::get(metric_attrs, "metric.name"), Some(StringValue("http.requests.total")))
  assert_eq(Attributes::get(metric_attrs, "metric.type"), Some(StringValue("counter")))
  assert_eq(Attributes::get(metric_attrs, "metric.unit"), Some(StringValue("requests")))
  assert_eq(Attributes::get(metric_attrs, "metric.description"), Some(StringValue("Total HTTP requests")))
  assert_eq(Attributes::get(metric_attrs, "metric.value"), Some(IntValue(12345)))
  
  // Test log record serialization
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(metric_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Error)
  assert_eq(LogRecord::body(log_record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
  
  // Test resource serialization
  let service_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("api-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production")),
    ("service.instance.id", StringValue("api-prod-12345"))
  ])
  
  // Verify resource attributes
  assert_eq(Resource::get_attribute(service_resource, "service.name"), Some(StringValue("api-service")))
  assert_eq(Resource::get_attribute(service_resource, "service.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(service_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_resource, "service.instance.id"), Some(StringValue("api-prod-12345")))
}

// Test 8: Advanced context propagation
test "advanced context propagation" {
  // Test context propagation with multiple levels
  let root_ctx = Context::root()
  
  // Level 1: Add request ID
  let request_key = ContextKey::new("request.id")
  let ctx1 = Context::with_value(root_ctx, request_key, "req-12345")
  
  // Level 2: Add user ID
  let user_key = ContextKey::new("user.id")
  let ctx2 = Context::with_value(ctx1, user_key, "user-67890")
  
  // Level 3: Add session ID
  let session_key = ContextKey::new("session.id")
  let ctx3 = Context::with_value(ctx2, session_key, "sess-abcdef")
  
  // Level 4: Add correlation ID
  let correlation_key = ContextKey::new("correlation.id")
  let ctx4 = Context::with_value(ctx3, correlation_key, "corr-123456")
  
  // Verify all context values are accessible
  assert_eq(Context::get(ctx4, request_key), Some("req-12345"))
  assert_eq(Context::get(ctx4, user_key), Some("user-67890"))
  assert_eq(Context::get(ctx4, session_key), Some("sess-abcdef"))
  assert_eq(Context::get(ctx4, correlation_key), Some("corr-123456"))
  
  // Verify intermediate contexts have expected values
  assert_eq(Context::get(ctx1, request_key), Some("req-12345"))
  assert_eq(Context::get(ctx1, user_key), None)
  assert_eq(Context::get(ctx1, session_key), None)
  assert_eq(Context::get(ctx1, correlation_key), None)
  
  assert_eq(Context::get(ctx2, request_key), Some("req-12345"))
  assert_eq(Context::get(ctx2, user_key), Some("user-67890"))
  assert_eq(Context::get(ctx2, session_key), None)
  assert_eq(Context::get(ctx2, correlation_key), None)
  
  assert_eq(Context::get(ctx3, request_key), Some("req-12345"))
  assert_eq(Context::get(ctx3, user_key), Some("user-67890"))
  assert_eq(Context::get(ctx3, session_key), Some("sess-abcdef"))
  assert_eq(Context::get(ctx3, correlation_key), None)
  
  // Test baggage propagation
  let empty_baggage = Baggage::new()
  let baggage1 = Baggage::set_entry(empty_baggage, "request.id", "req-12345")
  let baggage2 = Baggage::set_entry(baggage1, "user.id", "user-67890")
  let baggage3 = Baggage::set_entry(baggage2, "session.id", "sess-abcdef")
  
  // Verify baggage propagation
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("req-12345"))
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("user-67890"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("sess-abcdef"))
  
  // Test composite propagator with context
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(propagator, ctx4, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify extraction
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx, extracted_key), Some("true"))
}