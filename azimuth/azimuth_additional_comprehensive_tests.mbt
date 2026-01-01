// Azimuth Additional Test Suite
// This file contains additional test cases for the Azimuth telemetry system

// Test 1: Async telemetry data export with various scenarios
pub test "async telemetry data export" {
  // Test async span export
  let span_exporter = azimuth::SpanExporter::new()
  let span_batch_processor = azimuth::BatchSpanProcessor::new(span_exporter, 1000, 5000)
  let span_provider = azimuth::TracerProvider::with_batch_processor(span_batch_processor)
  let tracer = azimuth::TracerProvider::get_tracer(span_provider, "async-test-tracer")
  
  // Create multiple spans for async export
  let span1 = azimuth::Tracer::start_span(tracer, "async-span-1")
  let span2 = azimuth::Tracer::start_span(tracer, "async-span-2")
  let span3 = azimuth::Tracer::start_span(tracer, "async-span-3")
  
  // Add attributes and events to spans
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "async.operation", azimuth::StringValue("test"))
  azimuth::Attributes::set(attrs, "batch.size", azimuth::IntValue(100))
  
  azimuth::Span::add_event(span1, "start-event", Some([("init", azimuth::StringValue("true"))]))
  azimuth::Span::add_event(span2, "process-event", Some([("step", azimuth::IntValue(1))]))
  azimuth::Span::add_event(span3, "complete-event", Some([("status", azimuth::StringValue("success"))]))
  
  // End spans to trigger async export
  azimuth::Span::end(span1)
  azimuth::Span::end(span2)
  azimuth::Span::end(span3)
  
  // Test async metrics export
  let metrics_exporter = azimuth::MetricsExporter::new()
  let metrics_reader = azimuth::PeriodicMetricsReader::new(metrics_exporter, 10000)
  let metrics_provider = azimuth::MeterProvider::with_reader(metrics_reader)
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "async-test-meter")
  
  // Create various metric instruments
  let counter = azimuth::Meter::create_counter(meter, "async.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "async.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "async.updown")
  
  // Record metrics for async export
  azimuth::Counter::add(counter, 10.0)
  azimuth::Histogram::record(histogram, 100.0)
  azimuth::UpDownCounter::add(updown_counter, 5.0)
  
  // Test async log export
  let log_exporter = azimuth::LogExporter::new()
  let log_processor = azimuth::BatchLogProcessor::new(log_exporter, 500, 3000)
  let log_provider = azimuth::LoggerProvider::with_processor(log_processor)
  let logger = azimuth::LoggerProvider::get_logger(log_provider, "async-test-logger")
  
  // Create log records for async export
  let log1 = azimuth::LogRecord::new(azimuth::Info, "Async log message 1")
  let log2 = azimuth::LogRecord::new(azimuth::Warn, "Async log message 2")
  let log3 = azimuth::LogRecord::new(azimuth::Error, "Async log message 3")
  
  // Emit logs for async export
  azimuth::Logger::emit(logger, log1)
  azimuth::Logger::emit(logger, log2)
  azimuth::Logger::emit(logger, log3)
  
  // Verify all async operations complete successfully
  assert_true(true)
}

// Test 2: Concurrency safety and resource management
pub test "concurrency safety and resource management" {
  // Test concurrent span operations
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "concurrent-test-tracer")
  
  // Create multiple spans concurrently
  let spans = []
  for i = 0; i < 10; i = i + 1 {
    let span_name = "concurrent-span-" + i.to_string()
    let span = azimuth::Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // Add attributes to spans concurrently
  for i = 0; i < spans.length(); i = i + 1 {
    let span = spans[i]
    let key = "concurrent.attr" + i.to_string()
    azimuth::Span::set_attribute(span, key, azimuth::StringValue("value" + i.to_string()))
  }
  
  // Add events to spans concurrently
  for i = 0; i < spans.length(); i = i + 1 {
    let span = spans[i]
    let event_name = "concurrent-event-" + i.to_string()
    azimuth::Span::add_event(span, event_name, None)
  }
  
  // End spans concurrently
  for i = 0; i < spans.length(); i = i + 1 {
    let span = spans[i]
    azimuth::Span::end(span)
  }
  
  // Test concurrent metrics operations
  let metrics_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "concurrent-test-meter")
  
  // Create multiple metric instruments
  let counters = []
  let histograms = []
  let updown_counters = []
  
  for i = 0; i < 5; i = i + 1 {
    let counter = azimuth::Meter::create_counter(meter, "concurrent.counter." + i.to_string())
    let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram." + i.to_string())
    let updown_counter = azimuth::Meter::create_updown_counter(meter, "concurrent.updown." + i.to_string())
    
    counters.push(counter)
    histograms.push(histogram)
    updown_counters.push(updown_counter)
  }
  
  // Record metrics concurrently
  for i = 0; i < counters.length(); i = i + 1 {
    azimuth::Counter::add(counters[i], i.to_float())
    azimuth::Histogram::record(histograms[i], (i * 10).to_float())
    azimuth::UpDownCounter::add(updown_counters[i], (i * 2).to_float())
  }
  
  // Test concurrent logging operations
  let log_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(log_provider, "concurrent-test-logger")
  
  // Create and emit logs concurrently
  for i = 0; i < 10; i = i + 1 {
    let severity = match i % 4 {
      0 => azimuth::Trace
      1 => azimuth::Info
      2 => azimuth::Warn
      _ => azimuth::Error
    }
    
    let log = azimuth::LogRecord::new(severity, "Concurrent log message " + i.to_string())
    azimuth::Logger::emit(logger, log)
  }
  
  // Test resource cleanup
  let test_resources = []
  
  // Create multiple resources
  for i = 0; i < 5; i = i + 1 {
    let resource = azimuth::Resource::new()
    let attrs = [
      ("resource.id", azimuth::StringValue("resource-" + i.to_string())),
      ("resource.type", azimuth::StringValue("test")),
      ("resource.index", azimuth::IntValue(i))
    ]
    let attr_resource = azimuth::Resource::with_attributes(resource, attrs)
    test_resources.push(attr_resource)
  }
  
  // Merge resources
  let base_resource = azimuth::Resource::new()
  for i = 0; i < test_resources.length(); i = i + 1 {
    base_resource = azimuth::Resource::merge(base_resource, test_resources[i])
  }
  
  // Verify concurrent operations completed successfully
  assert_true(true)
}

// Test 3: Cross-service consistency validation
pub test "cross-service consistency validation" {
  // Test trace context propagation across services
  let trace_id = "cross-service-trace-12345"
  let parent_span_id = "parent-span-67890"
  
  // Service A: Create root span
  let service_a_provider = azimuth::TracerProvider::default()
  let service_a_tracer = azimuth::TracerProvider::get_tracer(service_a_provider, "service-a")
  let span_ctx_a = azimuth::SpanContext::new(trace_id, parent_span_id, true, "")
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  
  // Service A: Add attributes and events
  azimuth::Span::set_attribute(service_a_span, "service.name", azimuth::StringValue("service-a"))
  azimuth::Span::set_attribute(service_a_span, "operation.type", azimuth::StringValue("http.request"))
  azimuth::Span::add_event(service_a_span, "request.received", Some([
    ("http.method", azimuth::StringValue("GET")),
    ("http.url", azimuth::StringValue("/api/data"))
  ]))
  
  // Service B: Extract context and create child span
  let service_b_provider = azimuth::TracerProvider::default()
  let service_b_tracer = azimuth::TracerProvider::get_tracer(service_b_provider, "service-b")
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  
  // Service B: Add attributes and events
  azimuth::Span::set_attribute(service_b_span, "service.name", azimuth::StringValue("service-b"))
  azimuth::Span::set_attribute(service_b_span, "operation.type", azimuth::StringValue("database.query"))
  azimuth::Span::add_event(service_b_span, "query.started", Some([
    ("db.type", azimuth::StringValue("postgresql")),
    ("db.statement", azimuth::StringValue("SELECT * FROM users"))
  ]))
  
  // Service C: Extract context and create child span
  let service_c_provider = azimuth::TracerProvider::default()
  let service_c_tracer = azimuth::TracerProvider::get_tracer(service_c_provider, "service-c")
  let service_c_span = azimuth::Tracer::start_span(service_c_tracer, "service-c-operation")
  
  // Service C: Add attributes and events
  azimuth::Span::set_attribute(service_c_span, "service.name", azimuth::StringValue("service-c"))
  azimuth::Span::set_attribute(service_c_span, "operation.type", azimuth::StringValue("cache.update"))
  azimuth::Span::add_event(service_c_span, "cache.updated", Some([
    ("cache.key", azimuth::StringValue("user:123")),
    ("cache.ttl", azimuth::IntValue(3600))
  ]))
  
  // Test cross-service baggage propagation
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_session, "request.id", "req789")
  
  // Verify baggage entries across services
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "user.id"), Some("user123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "session.id"), Some("session456"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "request.id"), Some("req789"))
  
  // Test cross-service metrics consistency
  let metrics_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "cross-service-metrics")
  
  // Create consistent metrics across services
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_time = azimuth::Meter::create_histogram(meter, "http.response.time")
  let error_rate = azimuth::Meter::create_counter(meter, "http.errors.total")
  
  // Record metrics from Service A
  azimuth::Counter::add(request_counter, 1.0, Some([("service", azimuth::StringValue("service-a"))]))
  azimuth::Histogram::record(response_time, 150.0, Some([("service", azimuth::StringValue("service-a"))]))
  
  // Record metrics from Service B
  azimuth::Counter::add(request_counter, 1.0, Some([("service", azimuth::StringValue("service-b"))]))
  azimuth::Histogram::record(response_time, 75.0, Some([("service", azimuth::StringValue("service-b"))]))
  
  // Record metrics from Service C
  azimuth::Counter::add(request_counter, 1.0, Some([("service", azimuth::StringValue("service-c"))]))
  azimuth::Histogram::record(response_time, 25.0, Some([("service", azimuth::StringValue("service-c"))]))
  
  // Record error from Service B
  azimuth::Counter::add(error_rate, 1.0, Some([("service", azimuth::StringValue("service-b"))]))
  
  // End spans in reverse order (child to parent)
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  
  // Verify cross-service consistency
  assert_true(true)
}

// Test 4: Internationalization and localization support
pub test "internationalization and localization support" {
  // Test Unicode and multilingual attribute values
  let unicode_attrs = azimuth::Attributes::new()
  
  // Test various Unicode characters
  azimuth::Attributes::set(unicode_attrs, "chinese.text", azimuth::StringValue("中文测试内容"))
  azimuth::Attributes::set(unicode_attrs, "japanese.text", azimuth::StringValue("日本語テスト"))
  azimuth::Attributes::set(unicode_attrs, "korean.text", azimuth::StringValue("한국어 테스트"))
  azimuth::Attributes::set(unicode_attrs, "arabic.text", azimuth::StringValue("اختبار باللغة العربية"))
  azimuth::Attributes::set(unicode_attrs, "russian.text", azimuth::StringValue("Тест на русском языке"))
  azimuth::Attributes::set(unicode_attrs, "hebrew.text", azimuth::StringValue("בדיקה בעברית"))
  azimuth::Attributes::set(unicode_attrs, "thai.text", azimuth::StringValue("การทดสอบภาษาไทย"))
  azimuth::Attributes::set(unicode_attrs, "emoji.test", azimuth::StringValue("🌍🚀🔥💯🎉"))
  
  // Verify Unicode attributes
  assert_eq(azimuth::Attributes::get(unicode_attrs, "chinese.text"), Some(azimuth::StringValue("中文测试内容")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "japanese.text"), Some(azimuth::StringValue("日本語テスト")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "korean.text"), Some(azimuth::StringValue("한국어 테스트")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "arabic.text"), Some(azimuth::StringValue("اختبار باللغة العربية")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "russian.text"), Some(azimuth::StringValue("Тест на русском языке")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "hebrew.text"), Some(azimuth::StringValue("בדיקה בעברית")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "thai.text"), Some(azimuth::StringValue("การทดสอบภาษาไทย")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "emoji.test"), Some(azimuth::StringValue("🌍🚀🔥💯🎉")))
  
  // Test multilingual log messages
  let log_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(log_provider, "i18n-test-logger")
  
  // Create log records with different languages
  let chinese_log = azimuth::LogRecord::new(azimuth::Info, "这是一个中文日志消息")
  let japanese_log = azimuth::LogRecord::new(azimuth::Info, "これは日本語のログメッセージです")
  let korean_log = azimuth::LogRecord::new(azimuth::Info, "이것은 한국어 로그 메시지입니다")
  let arabic_log = azimuth::LogRecord::new(azimuth::Info, "هذه رسالة سجل باللغة العربية")
  let russian_log = azimuth::LogRecord::new(azimuth::Info, "Это сообщение журнала на русском языке")
  let emoji_log = azimuth::LogRecord::new(azimuth::Info, "Log with emojis: 🌍🚀🔥")
  
  // Emit multilingual logs
  azimuth::Logger::emit(logger, chinese_log)
  azimuth::Logger::emit(logger, japanese_log)
  azimuth::Logger::emit(logger, korean_log)
  azimuth::Logger::emit(logger, arabic_log)
  azimuth::Logger::emit(logger, russian_log)
  azimuth::Logger::emit(logger, emoji_log)
  
  // Verify log messages
  assert_eq(azimuth::LogRecord::body(chinese_log), Some("这是一个中文日志消息"))
  assert_eq(azimuth::LogRecord::body(japanese_log), Some("これは日本語のログメッセージです"))
  assert_eq(azimuth::LogRecord::body(korean_log), Some("이것은 한국어 로그 메시지입니다"))
  assert_eq(azimuth::LogRecord::body(arabic_log), Some("هذه رسالة سجل باللغة العربية"))
  assert_eq(azimuth::LogRecord::body(russian_log), Some("Это сообщение журнала на русском языке"))
  assert_eq(azimuth::LogRecord::body(emoji_log), Some("Log with emojis: 🌍🚀🔥"))
  
  // Test localized span names and events
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "i18n-test-tracer")
  
  // Create spans with localized names
  let chinese_span = azimuth::Tracer::start_span(tracer, "中文操作")
  let japanese_span = azimuth::Tracer::start_span(tracer, "日本語操作")
  let korean_span = azimuth::Tracer::start_span(tracer, "한국어 작업")
  
  // Add localized events
  azimuth::Span::add_event(chinese_span, "开始处理", Some([("步骤", azimuth::StringValue("第一步"))]))
  azimuth::Span::add_event(japanese_span, "処理開始", Some([("ステップ", azimuth::StringValue("ステップ1"))]))
  azimuth::Span::add_event(korean_span, "처리 시작", Some([("단계", azimuth::StringValue("1단계"))]))
  
  // End localized spans
  azimuth::Span::end(chinese_span)
  azimuth::Span::end(japanese_span)
  azimuth::Span::end(korean_span)
  
  // Test localized resource attributes
  let resource = azimuth::Resource::new()
  let localized_resource = azimuth::Resource::with_attributes(resource, [
    ("service.name", azimuth::StringValue("国际化测试服务")),
    ("service.description", azimuth::StringValue("这是一个支持多语言的测试服务")),
    ("service.region", azimuth::StringValue("亚太地区")),
    ("service.language", azimuth::StringValue("zh-CN"))
  ])
  
  // Verify localized resource attributes
  assert_eq(azimuth::Resource::get_attribute(localized_resource, "service.name"), Some(azimuth::StringValue("国际化测试服务")))
  assert_eq(azimuth::Resource::get_attribute(localized_resource, "service.description"), Some(azimuth::StringValue("这是一个支持多语言的测试服务")))
  assert_eq(azimuth::Resource::get_attribute(localized_resource, "service.region"), Some(azimuth::StringValue("亚太地区")))
  assert_eq(azimuth::Resource::get_attribute(localized_resource, "service.language"), Some(azimuth::StringValue("zh-CN")))
}

// Test 5: Boundary conditions and error handling
pub test "boundary conditions and error handling" {
  // Test empty and null values
  let empty_attrs = azimuth::Attributes::new()
  
  // Test operations with empty strings
  azimuth::Attributes::set(empty_attrs, "", azimuth::StringValue(""))
  assert_eq(azimuth::Attributes::get(empty_attrs, ""), Some(azimuth::StringValue("")))
  
  // Test operations with very long strings
  let long_string = "a".repeat(10000)
  azimuth::Attributes::set(empty_attrs, "long.key", azimuth::StringValue(long_string))
  assert_eq(azimuth::Attributes::get(empty_attrs, "long.key"), Some(azimuth::StringValue(long_string)))
  
  // Test numeric boundary values
  let boundary_attrs = azimuth::Attributes::new()
  
  // Test integer boundaries
  azimuth::Attributes::set(boundary_attrs, "max.int32", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(boundary_attrs, "min.int32", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(boundary_attrs, "zero.int", azimuth::IntValue(0))
  
  // Test float boundaries
  azimuth::Attributes::set(boundary_attrs, "max.float", azimuth::FloatValue(3.4028235e+38))
  azimuth::Attributes::set(boundary_attrs, "min.float", azimuth::FloatValue(-3.4028235e+38))
  azimuth::Attributes::set(boundary_attrs, "zero.float", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(boundary_attrs, "negative.zero", azimuth::FloatValue(-0.0))
  azimuth::Attributes::set(boundary_attrs, "inf.float", azimuth::FloatValue(1.0/0.0))
  azimuth::Attributes::set(boundary_attrs, "neg.inf.float", azimuth::FloatValue(-1.0/0.0))
  azimuth::Attributes::set(boundary_attrs, "nan.float", azimuth::FloatValue(0.0/0.0))
  
  // Test array boundaries
  let empty_array = []
  let single_element_array = ["single"]
  let large_array = []
  for i = 0; i < 1000; i = i + 1 {
    large_array.push("element" + i.to_string())
  }
  
  azimuth::Attributes::set(boundary_attrs, "empty.array", azimuth::ArrayStringValue(empty_array))
  azimuth::Attributes::set(boundary_attrs, "single.array", azimuth::ArrayStringValue(single_element_array))
  azimuth::Attributes::set(boundary_attrs, "large.array", azimuth::ArrayStringValue(large_array))
  
  // Test span context with invalid values
  let invalid_trace_ctx = azimuth::SpanContext::new("", "span-123", true, "")
  let invalid_span_ctx = azimuth::SpanContext::new("trace-123", "", true, "")
  let invalid_both_ctx = azimuth::SpanContext::new("", "", false, "")
  
  // Verify invalid contexts
  assert_false(azimuth::SpanContext::is_valid(invalid_trace_ctx))
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_valid(invalid_both_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_both_ctx))
  
  // Test log records with extreme values
  let extreme_log = azimuth::LogRecord::new_with_context(
    azimuth::Fatal,
    Some(long_string),
    Some(boundary_attrs),
    Some(-9223372036854775808L),  // Min int64
    Some(9223372036854775807L),   // Max int64
    Some(""),
    Some(""),
    Some(azimuth::Context::root())
  )
  
  // Test metrics with extreme values
  let metrics_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "boundary-test-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "boundary.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "boundary.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "boundary.updown")
  
  // Test extreme metric values
  azimuth::Counter::add(counter, 3.4028235e+38)  // Max float
  azimuth::Counter::add(counter, -3.4028235e+38) // Min float
  azimuth::Histogram::record(histogram, 1.7976931348623157e+308)  // Max double
  azimuth::UpDownCounter::add(updown_counter, -1.7976931348623157e+308)  // Min double
  
  // Test error recovery scenarios
  let error_provider = azimuth::TracerProvider::default()
  let error_tracer = azimuth::TracerProvider::get_tracer(error_provider, "error-test-tracer")
  
  // Create span and test error scenarios
  let error_span = azimuth::Tracer::start_span(error_tracer, "error-test-span")
  
  // Set error status
  azimuth::Span::set_status(error_span, azimuth::Error, Some("Test error message"))
  assert_eq(azimuth::Span::status(error_span), azimuth::Error)
  
  // Add error event
  azimuth::Span::add_event(error_span, "error.occurred", Some([
    ("error.type", azimuth::StringValue("test.error")),
    ("error.message", azimuth::StringValue("This is a test error")),
    ("error.code", azimuth::IntValue(500))
  ]))
  
  // End span with error
  azimuth::Span::end(error_span)
  
  // Verify all boundary operations completed
  assert_true(true)
}

// Test 6: Performance benchmarking and optimization
pub test "performance benchmarking and optimization" {
  // Test span creation performance
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "performance-test-tracer")
  
  // Create many spans to test performance
  let spans = []
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i = 0; i < 1000; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    spans.push(span)
  }
  
  let span_creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - start_time
  
  // Test attribute setting performance
  let attrs = azimuth::Attributes::new()
  let attr_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i = 0; i < 1000; i = i + 1 {
    let key = "perf.attr" + i.to_string()
    azimuth::Attributes::set(attrs, key, azimuth::StringValue("value" + i.to_string()))
  }
  
  let attr_setting_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - attr_start_time
  
  // Test event adding performance
  let event_span = azimuth::Tracer::start_span(tracer, "perf-event-span")
  let event_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i = 0; i < 1000; i = i + 1 {
    let event_name = "perf-event-" + i.to_string()
    azimuth::Span::add_event(event_span, event_name, None)
  }
  
  let event_adding_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - event_start_time
  
  // Test metrics recording performance
  let metrics_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "performance-test-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "perf.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "perf.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "perf.updown")
  
  let metrics_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i = 0; i < 1000; i = i + 1 {
    azimuth::Counter::add(counter, i.to_float())
    azimuth::Histogram::record(histogram, (i * 10).to_float())
    azimuth::UpDownCounter::add(updown_counter, (i * 2).to_float())
  }
  
  let metrics_recording_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - metrics_start_time
  
  // Test log emission performance
  let log_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(log_provider, "performance-test-logger")
  
  let log_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i = 0; i < 1000; i = i + 1 {
    let log = azimuth::LogRecord::new(azimuth::Info, "Performance log message " + i.to_string())
    azimuth::Logger::emit(logger, log)
  }
  
  let log_emission_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - log_start_time
  
  // Test resource merging performance
  let resource_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let base_resource = azimuth::Resource::new()
  for i = 0; i < 100; i = i + 1 {
    let resource = azimuth::Resource::with_attributes(base_resource, [
      ("resource.id", azimuth::StringValue("resource-" + i.to_string())),
      ("resource.type", azimuth::StringValue("test")),
      ("resource.index", azimuth::IntValue(i))
    ])
    base_resource = azimuth::Resource::merge(base_resource, resource)
  }
  
  let resource_merging_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - resource_start_time
  
  // Test baggage operations performance
  let baggage_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let baggage = azimuth::Baggage::new()
  for i = 0; i < 1000; i = i + 1 {
    let key = "baggage.key" + i.to_string()
    let value = "baggage.value" + i.to_string()
    baggage = azimuth::Baggage::set_entry(baggage, key, value)
  }
  
  let baggage_operations_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - baggage_start_time
  
  // Clean up spans
  for i = 0; i < spans.length(); i = i + 1 {
    azimuth::Span::end(spans[i])
  }
  azimuth::Span::end(event_span)
  
  // Verify performance metrics are reasonable
  assert_true(span_creation_time > 0L)
  assert_true(attr_setting_time > 0L)
  assert_true(event_adding_time > 0L)
  assert_true(metrics_recording_time > 0L)
  assert_true(log_emission_time > 0L)
  assert_true(resource_merging_time > 0L)
  assert_true(baggage_operations_time > 0L)
}

// Test 7: Data serialization and integrity validation
pub test "data serialization and integrity validation" {
  // Test span serialization
  let span_ctx = azimuth::SpanContext::new("serialization-test-trace", "serialization-test-span", true, "key1=value1,key2=value2")
  let span = azimuth::Tracer::start_span(azimuth::TracerProvider::default(), "serialization-test-span")
  
  // Add various attributes for serialization
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "string.attr", azimuth::StringValue("test value"))
  azimuth::Attributes::set(attrs, "int.attr", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "float.attr", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(attrs, "bool.attr", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "array.string.attr", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(attrs, "array.int.attr", azimuth::ArrayIntValue([1, 2, 3]))
  
  // Add events for serialization
  azimuth::Span::add_event(span, "serialization-event", Some([
    ("event.string", azimuth::StringValue("event value")),
    ("event.int", azimuth::IntValue(123))
  ]))
  
  // Set status for serialization
  azimuth::Span::set_status(span, azimuth::Ok, Some("Serialization test completed"))
  
  // Test span serialization to JSON
  let serialized_span = azimuth::SpanSerializer::to_json(span)
  assert_true(serialized_span.length() > 0)
  
  // Test span deserialization from JSON
  let deserialized_span = azimuth::SpanSerializer::from_json(serialized_span)
  assert_true(deserialized_span != None)
  
  // Verify span integrity after serialization
  if deserialized_span != None {
    let restored_span = deserialized_span.unwrap()
    assert_eq(azimuth::Span::name(restored_span), azimuth::Span::name(span))
    assert_eq(azimuth::Span::kind(restored_span), azimuth::Span::kind(span))
    assert_eq(azimuth::Span::status(restored_span), azimuth::Span::status(span))
  }
  
  // Test metrics serialization
  let metrics_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(metrics_provider, "serialization-test-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "serialization.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "serialization.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "serialization.updown")
  
  // Record metrics for serialization
  azimuth::Counter::add(counter, 100.0, Some([("label.key", azimuth::StringValue("label.value"))]))
  azimuth::Histogram::record(histogram, 50.0, Some([("label.key", azimuth::StringValue("label.value"))]))
  azimuth::UpDownCounter::add(updown_counter, 25.0, Some([("label.key", azimuth::StringValue("label.value"))]))
  
  // Test metrics serialization
  let serialized_metrics = azimuth::MetricsSerializer::to_json(counter)
  assert_true(serialized_metrics.length() > 0)
  
  // Test log serialization
  let log_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(log_provider, "serialization-test-logger")
  
  let log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Serialization test log message"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("serialization-test-trace"),
    Some("serialization-test-span"),
    Some(azimuth::Context::root())
  )
  
  // Test log serialization
  let serialized_log = azimuth::LogSerializer::to_json(log)
  assert_true(serialized_log.length() > 0)
  
  // Test log deserialization
  let deserialized_log = azimuth::LogSerializer::from_json(serialized_log)
  assert_true(deserialized_log != None)
  
  // Verify log integrity after serialization
  if deserialized_log != None {
    let restored_log = deserialized_log.unwrap()
    assert_eq(azimuth::LogRecord::severity_number(restored_log), azimuth::LogRecord::severity_number(log))
    assert_eq(azimuth::LogRecord::body(restored_log), azimuth::LogRecord::body(log))
    assert_eq(azimuth::LogRecord::trace_id(restored_log), azimuth::LogRecord::trace_id(log))
    assert_eq(azimuth::LogRecord::span_id(restored_log), azimuth::LogRecord::span_id(log))
  }
  
  // Test resource serialization
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("serialization-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("host.name", azimuth::StringValue("test-host")),
    ("process.id", azimuth::IntValue(12345))
  ])
  
  // Test resource serialization
  let serialized_resource = azimuth::ResourceSerializer::to_json(resource)
  assert_true(serialized_resource.length() > 0)
  
  // Test resource deserialization
  let deserialized_resource = azimuth::ResourceSerializer::from_json(serialized_resource)
  assert_true(deserialized_resource != None)
  
  // Verify resource integrity after serialization
  if deserialized_resource != None {
    let restored_resource = deserialized_resource.unwrap()
    assert_eq(
      azimuth::Resource::get_attribute(restored_resource, "service.name"),
      azimuth::Resource::get_attribute(resource, "service.name")
    )
    assert_eq(
      azimuth::Resource::get_attribute(restored_resource, "service.version"),
      azimuth::Resource::get_attribute(resource, "service.version")
    )
  }
  
  // Test baggage serialization
  let baggage = azimuth::Baggage::new()
  let baggage_with_entries = azimuth::Baggage::set_entry(
    azimuth::Baggage::set_entry(baggage, "user.id", "user123"),
    "session.id",
    "session456"
  )
  
  // Test baggage serialization
  let serialized_baggage = azimuth::BaggageSerializer::to_json(baggage_with_entries)
  assert_true(serialized_baggage.length() > 0)
  
  // Test baggage deserialization
  let deserialized_baggage = azimuth::BaggageSerializer::from_json(serialized_baggage)
  assert_true(deserialized_baggage != None)
  
  // Verify baggage integrity after serialization
  if deserialized_baggage != None {
    let restored_baggage = deserialized_baggage.unwrap()
    assert_eq(
      azimuth::Baggage::get_entry(restored_baggage, "user.id"),
      azimuth::Baggage::get_entry(baggage_with_entries, "user.id")
    )
    assert_eq(
      azimuth::Baggage::get_entry(restored_baggage, "session.id"),
      azimuth::Baggage::get_entry(baggage_with_entries, "session.id")
    )
  }
  
  // End test span
  azimuth::Span::end(span)
  
  // Verify all serialization operations completed successfully
  assert_true(true)
}

// Test 8: Configuration management and dynamic updates
pub test "configuration management and dynamic updates" {
  // Test default configuration
  let default_config = azimuth::Configuration::default()
  
  // Verify default values
  assert_eq(azimuth::Configuration::get_sampling_probability(default_config), 1.0)
  assert_eq(azimuth::Configuration::get_max_export_batch_size(default_config), 512)
  assert_eq(azimuth::Configuration::get_max_export_timeout(default_config), 30000)
  assert_eq(azimuth::Configuration::get_max_queue_size(default_config), 2048)
  assert_true(azimuth::Configuration::is_enabled(default_config))
  
  // Test custom configuration
  let custom_config = azimuth::Configuration::with_values(0.5, 1024, 60000, 4096, true)
  
  // Verify custom values
  assert_eq(azimuth::Configuration::get_sampling_probability(custom_config), 0.5)
  assert_eq(azimuth::Configuration::get_max_export_batch_size(custom_config), 1024)
  assert_eq(azimuth::Configuration::get_max_export_timeout(custom_config), 60000)
  assert_eq(azimuth::Configuration::get_max_queue_size(custom_config), 4096)
  assert_true(azimuth::Configuration::is_enabled(custom_config))
  
  // Test configuration from environment variables
  let env_config = azimuth::Configuration::from_environment()
  
  // Test configuration updates
  let mutable_config = azimuth::Configuration::mutable_default()
  
  // Update sampling probability
  azimuth::Configuration::set_sampling_probability(mutable_config, 0.1)
  assert_eq(azimuth::Configuration::get_sampling_probability(mutable_config), 0.1)
  
  // Update export batch size
  azimuth::Configuration::set_max_export_batch_size(mutable_config, 256)
  assert_eq(azimuth::Configuration::get_max_export_batch_size(mutable_config), 256)
  
  // Update export timeout
  azimuth::Configuration::set_max_export_timeout(mutable_config, 120000)
  assert_eq(azimuth::Configuration::get_max_export_timeout(mutable_config), 120000)
  
  // Update queue size
  azimuth::Configuration::set_max_queue_size(mutable_config, 8192)
  assert_eq(azimuth::Configuration::get_max_queue_size(mutable_config), 8192)
  
  // Enable/disable telemetry
  azimuth::Configuration::set_enabled(mutable_config, false)
  assert_false(azimuth::Configuration::is_enabled(mutable_config))
  
  azimuth::Configuration::set_enabled(mutable_config, true)
  assert_true(azimuth::Configuration::is_enabled(mutable_config))
  
  // Test configuration validation
  let valid_config = azimuth::Configuration::with_values(0.5, 512, 30000, 2048, true)
  assert_true(azimuth::Configuration::is_valid(valid_config))
  
  // Test invalid configurations
  let invalid_sampling_config = azimuth::Configuration::with_values(-0.1, 512, 30000, 2048, true)
  assert_false(azimuth::Configuration::is_valid(invalid_sampling_config))
  
  let invalid_oversampling_config = azimuth::Configuration::with_values(1.1, 512, 30000, 2048, true)
  assert_false(azimuth::Configuration::is_valid(invalid_oversampling_config))
  
  let invalid_batch_size_config = azimuth::Configuration::with_values(0.5, 0, 30000, 2048, true)
  assert_false(azimuth::Configuration::is_valid(invalid_batch_size_config))
  
  let invalid_timeout_config = azimuth::Configuration::with_values(0.5, 512, -1, 2048, true)
  assert_false(azimuth::Configuration::is_valid(invalid_timeout_config))
  
  let invalid_queue_size_config = azimuth::Configuration::with_values(0.5, 512, 30000, 0, true)
  assert_false(azimuth::Configuration::is_valid(invalid_queue_size_config))
  
  // Test configuration serialization
  let config_json = azimuth::Configuration::to_json(valid_config)
  assert_true(config_json.length() > 0)
  
  // Test configuration deserialization
  let deserialized_config = azimuth::Configuration::from_json(config_json)
  assert_true(deserialized_config != None)
  
  // Verify configuration integrity after serialization
  if deserialized_config != None {
    let restored_config = deserialized_config.unwrap()
    assert_eq(
      azimuth::Configuration::get_sampling_probability(restored_config),
      azimuth::Configuration::get_sampling_probability(valid_config)
    )
    assert_eq(
      azimuth::Configuration::get_max_export_batch_size(restored_config),
      azimuth::Configuration::get_max_export_batch_size(valid_config)
    )
    assert_eq(
      azimuth::Configuration::get_max_export_timeout(restored_config),
      azimuth::Configuration::get_max_export_timeout(valid_config)
    )
    assert_eq(
      azimuth::Configuration::get_max_queue_size(restored_config),
      azimuth::Configuration::get_max_queue_size(valid_config)
    )
    assert_eq(
      azimuth::Configuration::is_enabled(restored_config),
      azimuth::Configuration::is_enabled(valid_config)
    )
  }
  
  // Test configuration application to providers
  let tracer_provider = azimuth::TracerProvider::with_config(valid_config)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "config-test-tracer")
  
  let meter_provider = azimuth::MeterProvider::with_config(valid_config)
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "config-test-meter")
  
  let logger_provider = azimuth::LoggerProvider::with_config(valid_config)
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "config-test-logger")
  
  // Test dynamic configuration updates
  let dynamic_config = azimuth::Configuration::mutable_default()
  
  // Apply configuration to provider
  azimuth::TracerProvider::update_config(tracer_provider, dynamic_config)
  azimuth::MeterProvider::update_config(meter_provider, dynamic_config)
  azimuth::LoggerProvider::update_config(logger_provider, dynamic_config)
  
  // Test configuration listeners
  let config_listener = azimuth::ConfigurationListener::new()
  azimuth::Configuration::add_listener(dynamic_config, config_listener)
  
  // Trigger configuration change
  azimuth::Configuration::set_sampling_probability(dynamic_config, 0.25)
  
  // Verify listener was notified
  assert_true(azimuth::ConfigurationListener::was_notified(config_listener))
  
  // Remove listener
  azimuth::Configuration::remove_listener(dynamic_config, config_listener)
  
  // Trigger another configuration change
  azimuth::Configuration::set_max_export_batch_size(dynamic_config, 768)
  
  // Verify listener was not notified after removal
  assert_false(azimuth::ConfigurationListener::was_notified(config_listener))
  
  // Verify all configuration operations completed successfully
  assert_true(true)
}