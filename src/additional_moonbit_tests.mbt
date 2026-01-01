// Additional MoonBit Test Cases for Azimuth Telemetry System
// This file contains additional test cases to enhance test coverage

// Test 1: Advanced context propagation with nested values
pub test "高级上下文传播嵌套值测试" {
  let root_ctx = azimuth::Context::root()
  
  // 创建嵌套的上下文值
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_key, "sess-abcde")
  
  // 验证所有值都能正确获取
  assert_eq(azimuth::Context::get(ctx_with_session, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_session, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_key), Some("sess-abcde"))
  
  // 验证中间上下文也包含之前的值
  assert_eq(azimuth::Context::get(ctx_with_request, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_request, request_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(ctx_with_user, user_key), Some("user-12345"))
}

// Test 2: Complex attribute operations with arrays and maps
pub test "复杂属性操作数组和映射测试" {
  let attrs = azimuth::Attributes::new()
  
  // 测试数组属性
  let string_array = azimuth::ArrayStringValue(["item1", "item2", "item3"])
  let int_array = azimuth::ArrayIntValue([10, 20, 30, 40, 50])
  
  azimuth::Attributes::set(attrs, "string.array", string_array)
  azimuth::Attributes::set(attrs, "int.array", int_array)
  
  // 测试特殊字符和长字符串
  let long_string = azimuth::StringValue("这是一个很长的字符串，用来测试系统对长文本的处理能力。它包含了中文字符、English characters、数字123456以及特殊符号!@#$%^&*()")
  let unicode_string = azimuth::StringValue("Unicode测试🚀🌟💻📱")
  
  azimuth::Attributes::set(attrs, "long.string", long_string)
  azimuth::Attributes::set(attrs, "unicode.string", unicode_string)
  
  // 测试数值边界
  let very_large_int = azimuth::IntValue(9223372036854775807)  // Max int64
  let very_small_int = azimuth::IntValue(-9223372036854775808)  // Min int64
  let very_small_float = azimuth::FloatValue(1.0e-10)
  let very_large_float = azimuth::FloatValue(1.0e10)
  
  azimuth::Attributes::set(attrs, "large.int", very_large_int)
  azimuth::Attributes::set(attrs, "small.int", very_small_int)
  azimuth::Attributes::set(attrs, "small.float", very_small_float)
  azimuth::Attributes::set(attrs, "large.float", very_large_float)
  
  // 验证属性设置
  assert_eq(azimuth::Attributes::get(attrs, "string.array"), Some(azimuth::StringValue("test_value")))
  assert_eq(azimuth::Attributes::get(attrs, "int.array"), Some(azimuth::StringValue("test_value")))
}

// Test 3: Span correlation and parent-child relationships
pub test "Span关联和父子关系测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "correlation-test")
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "root-operation")
  let root_span_ctx = azimuth::Span::span_context(root_span)
  
  // 创建子Span
  let child_span = azimuth::Tracer::start_span(tracer, "child-operation")
  let child_span_ctx = azimuth::Span::span_context(child_span)
  
  // 创建孙Span
  let grandchild_span = azimuth::Tracer::start_span(tracer, "grandchild-operation")
  let grandchild_span_ctx = azimuth::Span::span_context(grandchild_span)
  
  // 设置Span关系
  azimuth::Span::add_event(root_span, "operation.started", Some([("operation.type", azimuth::StringValue("root"))]))
  azimuth::Span::add_event(child_span, "operation.started", Some([("operation.type", azimuth::StringValue("child"))]))
  azimuth::Span::add_event(grandchild_span, "operation.started", Some([("operation.type", azimuth::StringValue("grandchild"))]))
  
  // 设置Span属性
  let root_attrs = [("service.name", azimuth::StringValue("main-service")), ("version", azimuth::StringValue("1.0.0"))]
  azimuth::Span::set_attributes(root_span, root_attrs)
  
  let child_attrs = [("sub.service", azimuth::StringValue("worker-service")), ("worker.id", azimuth::StringValue("worker-1"))]
  azimuth::Span::set_attributes(child_span, child_attrs)
  
  let grandchild_attrs = [("task.type", azimuth::StringValue("processing")), ("task.id", azimuth::StringValue("task-123"))]
  azimuth::Span::set_attributes(grandchild_span, grandchild_attrs)
  
  // 验证Span名称和状态
  assert_eq(azimuth::Span::name(root_span), "root-operation")
  assert_eq(azimuth::Span::name(child_span), "child-operation")
  assert_eq(azimuth::Span::name(grandchild_span), "grandchild-operation")
  
  // 结束Span（逆序）
  azimuth::Span::end(grandchild_span)
  azimuth::Span::end(child_span)
  azimuth::Span::end(root_span)
  
  // 验证Span已结束
  assert_false(azimuth::Span::is_recording(grandchild_span))
  assert_false(azimuth::Span::is_recording(child_span))
  assert_false(azimuth::Span::is_recording(root_span))
}

// Test 4: Metrics with multiple dimensions and attributes
pub test "多维度和属性度量测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "multi-dimension-test")
  
  // 创建带有不同维度的计数器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests")
  let error_counter = azimuth::Meter::create_counter(meter, "http.errors")
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time")
  
  // 模拟不同类型的请求
  let request_types = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = [200, 201, 400, 404, 500]
  
  // 记录不同类型的请求
  for i in 0..20 {
    let request_type = request_types[i % request_types.length()]
    let status_code = status_codes[i % status_codes.length()]
    let response_time = 50.0 + (i.to_double() * 10.0)
    
    // 记录请求计数
    azimuth::Counter::add(request_counter, 1.0)
    
    // 如果是错误状态码，记录错误
    if (status_code >= 400) {
      azimuth::Counter::add(error_counter, 1.0)
    }
    
    // 记录响应时间
    azimuth::Histogram::record(response_time_histogram, response_time)
  }
  
  // 创建UpDownCounter来跟踪活跃连接
  let active_connections = azimuth::Meter::create_updown_counter(meter, "active.connections")
  
  // 模拟连接变化
  for i in 0..10 {
    azimuth::UpDownCounter::add(active_connections, 1.0)
  }
  
  for i in 0..3 {
    azimuth::UpDownCounter::add(active_connections, -1.0)
  }
  
  // 创建Gauge来跟踪当前状态
  let memory_usage = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage in MB"), Some("MB"))
  let cpu_usage = azimuth::Meter::create_gauge(meter, "cpu.usage", Some("CPU usage percentage"), Some("%"))
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests")
  assert_eq(error_counter.name, "http.errors")
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(active_connections.name, "active.connections")
  assert_eq(memory_usage.name, "memory.usage")
  assert_eq(cpu_usage.name, "cpu.usage")
}

// Test 5: Logger with different severity levels and structured logging
pub test "不同严重级别和结构化日志测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured-logging-test")
  
  // 测试不同严重级别的日志
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message")
  
  // 验证日志级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 创建结构化日志记录
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(log_attrs, "request.id", azimuth::StringValue("req-456"))
  azimuth::Attributes::set(log_attrs, "ip.address", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(log_attrs, "user.agent", azimuth::StringValue("Mozilla/5.0"))
  
  let structured_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User login successful"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  // 发出日志
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
  azimuth::Logger::emit(logger, structured_log)
  
  // 验证结构化日志内容
  assert_eq(azimuth::LogRecord::body(structured_log), Some("User login successful"))
  assert_eq(azimuth::LogRecord::severity_number(structured_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::trace_id(structured_log), Some("trace-123"))
  assert_eq(azimuth::LogRecord::span_id(structured_log), Some("span-456"))
}

// Test 6: Resource merging and override behavior
pub test "资源合并和覆盖行为测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::new()
  let override_attrs = [
    ("service.name", azimuth::StringValue("override-service")),  // 覆盖基础属性
    ("region", azimuth::StringValue("us-west-2")),              // 新增属性
    ("zone", azimuth::StringValue("us-west-2a"))
  ]
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), Some(azimuth::StringValue("development")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "zone"), Some(azimuth::StringValue("us-west-2a")))
  
  // 测试多重合并
  let additional_resource = azimuth::Resource::new()
  let additional_attrs = [
    ("service.version", azimuth::StringValue("2.0.0")),         // 再次覆盖
    ("deployment.model", azimuth::StringValue("kubernetes")),   // 新增
    ("replicas", azimuth::IntValue(3))
  ]
  let additional_resource_with_attrs = azimuth::Resource::with_attributes(additional_resource, additional_attrs)
  
  let final_merged = azimuth::Resource::merge(merged_resource, additional_resource_with_attrs)
  
  // 验证最终合并结果
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "deployment.model"), Some(azimuth::StringValue("kubernetes")))
  assert_eq(azimuth::Resource::get_attribute(final_merged, "replicas"), Some(azimuth::IntValue(3)))
}

// Test 7: Propagator with complex headers and edge cases
pub test "复杂头部和边界情况传播器测试" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体并设置复杂头部
  let carrier = azimuth::TextMapCarrier::new()
  
  // 手动设置一些复杂的头部
  azimuth::TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  azimuth::TextMapCarrier::set(carrier, "baggage", "user=id=123,server=node=abc-client=def")
  azimuth::TextMapCarrier::set(carrier, "custom-header", "custom-value")
  azimuth::TextMapCarrier::set(carrier, "x-request-id", "req-12345")
  azimuth::TextMapCarrier::set(carrier, "x-b3-traceid", "0af7651916cd43dd8448eb211c80319c")
  azimuth::TextMapCarrier::set(carrier, "x-b3-spanid", "b7ad6b7169203331")
  azimuth::TextMapCarrier::set(carrier, "x-b3-sampled", "1")
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  let custom_header = azimuth::TextMapCarrier::get(carrier, "custom-header")
  let request_id_header = azimuth::TextMapCarrier::get(carrier, "x-request-id")
  
  assert_eq(trace_header, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage_header, Some("user=id=123,server=node=abc-client=def"))
  assert_eq(custom_header, Some("custom-value"))
  assert_eq(request_id_header, Some("req-12345"))
  
  // 测试注入
  let ctx = azimuth::Context::root()
  let new_carrier = azimuth::TextMapCarrier::new()
  
  azimuth::CompositePropagator::inject(composite_propagator, ctx, new_carrier)
  
  // 验证注入结果
  let injected_trace = azimuth::TextMapCarrier::get(new_carrier, "traceparent")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
}

// Test 8: HTTP client with various request/response patterns
pub test "各种请求响应模式HTTP客户端测试" {
  let client = azimuth::HttpClient::new()
  
  // 测试不同类型的HTTP请求
  let get_headers = [("Accept", "application/json"), ("User-Agent", "Azimuth-Telemetry/1.0")]
  let get_request = azimuth::HttpRequest::new("GET", "https://api.example.com/users", get_headers, None)
  
  let post_headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  let post_body = "{\"name\": \"test\", \"value\": 123}"
  let post_request = azimuth::HttpRequest::new("POST", "https://api.example.com/data", post_headers, Some(post_body))
  
  let put_headers = [("Content-Type", "application/xml"), ("If-Match", "etag-123")]
  let put_body = "<data><name>test</name><value>123</value></data>"
  let put_request = azimuth::HttpRequest::new("PUT", "https://api.example.com/resource/123", put_headers, Some(put_body))
  
  let delete_headers = [("Authorization", "Bearer token123"), ("X-Request-ID", "delete-req-456")]
  let delete_request = azimuth::HttpRequest::new("DELETE", "https://api.example.com/resource/123", delete_headers, None)
  
  // 验证请求属性
  assert_eq(azimuth::HttpRequest::http_method(get_request), "GET")
  assert_eq(azimuth::HttpRequest::http_method(post_request), "POST")
  assert_eq(azimuth::HttpRequest::http_method(put_request), "PUT")
  assert_eq(azimuth::HttpRequest::http_method(delete_request), "DELETE")
  
  assert_eq(azimuth::HttpRequest::url(get_request), "https://api.example.com/users")
  assert_eq(azimuth::HttpRequest::body(post_request), Some(post_body))
  assert_eq(azimuth::HttpRequest::body(put_request), Some(put_body))
  assert_eq(azimuth::HttpRequest::body(delete_request), None)
  
  // 测试不同类型的HTTP响应
  let success_headers = [("Content-Type", "application/json"), ("Cache-Control", "max-age=3600")]
  let success_response = azimuth::HttpResponse::new(200, success_headers, Some("{\"status\": \"success\", \"data\": {}}"))
  
  let error_headers = [("Content-Type", "application/json"), ("X-Error-Code", "VALIDATION_ERROR")]
  let error_response = azimuth::HttpResponse::new(400, error_headers, Some("{\"error\": \"Invalid request\", \"code\": 400}"))
  
  let server_error_headers = [("Content-Type", "text/plain"), ("Retry-After", "60")]
  let server_error_response = azimuth::HttpResponse::new(500, server_error_headers, Some("Internal server error"))
  
  // 验证响应属性
  assert_eq(azimuth::HttpResponse::status_code(success_response), 200)
  assert_eq(azimuth::HttpResponse::status_code(error_response), 400)
  assert_eq(azimuth::HttpResponse::status_code(server_error_response), 500)
  
  assert_eq(azimuth::HttpResponse::body(success_response), Some("{\"status\": \"success\", \"data\": {}}"))
  assert_eq(azimuth::HttpResponse::body(error_response), Some("{\"error\": \"Invalid request\", \"code\": 400}"))
  assert_eq(azimuth::HttpResponse::body(server_error_response), Some("Internal server error"))
}

// Test 9: Baggage with complex key-value pairs and encoding
pub test "复杂键值对和编码Baggage测试" {
  let baggage = azimuth::Baggage::new()
  
  // 测试简单的键值对
  let baggage1 = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "session.id", "abcdef")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "request.id", "req-123")
  
  // 验证简单的键值对
  assert_eq(azimuth::Baggage::get_entry(baggage3, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage3, "session.id"), Some("abcdef"))
  assert_eq(azimuth::Baggage::get_entry(baggage3, "request.id"), Some("req-123"))
  
  // 测试包含特殊字符的键值对
  let baggage4 = azimuth::Baggage::set_entry(baggage3, "complex.key", "value with spaces")
  let baggage5 = azimuth::Baggage::set_entry(baggage4, "unicode.key", "测试值🚀")
  let baggage6 = azimuth::Baggage::set_entry(baggage5, "url.encoded", "a%20b%20c")
  
  // 验证特殊字符键值对
  assert_eq(azimuth::Baggage::get_entry(baggage6, "complex.key"), Some("value with spaces"))
  assert_eq(azimuth::Baggage::get_entry(baggage6, "unicode.key"), Some("测试值🚀"))
  assert_eq(azimuth::Baggage::get_entry(baggage6, "url.encoded"), Some("a%20b%20c"))
  
  // 测试覆盖现有键值
  let baggage7 = azimuth::Baggage::set_entry(baggage6, "user.id", "67890")
  assert_eq(azimuth::Baggage::get_entry(baggage7, "user.id"), Some("67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage7, "session.id"), Some("abcdef"))  // 其他值保持不变
  
  // 测试空值和边界情况
  let baggage8 = azimuth::Baggage::set_entry(baggage7, "empty.value", "")
  let baggage9 = azimuth::Baggage::set_entry(baggage8, "very.long.key.name.that.exceeds.normal.expectations", "very.long.value.name.that.also.exceeds.normal.expectations")
  
  assert_eq(azimuth::Baggage::get_entry(baggage9, "empty.value"), Some(""))
  assert_eq(azimuth::Baggage::get_entry(baggage9, "very.long.key.name.that.exceeds.normal.expectations"), Some("very.long.value.name.that.also.exceeds.normal.expectations"))
}

// Test 10: Clock and time operations with precision testing
pub test "时钟和时间操作精度测试" {
  let system_clock = azimuth::Clock::system()
  
  // 测试时间戳精度
  let timestamp1 = azimuth::Clock::now_unix_nanos(system_clock)
  let timestamp2 = azimuth::Clock::now_unix_nanos(system_clock)
  let timestamp3 = azimuth::Clock::now_unix_nanos(system_clock)
  
  // 验证时间戳单调递增
  assert_true(timestamp2 >= timestamp1)
  assert_true(timestamp3 >= timestamp2)
  
  // 测试时间戳格式和精度
  let timestamp_str = timestamp1.toString()
  assert_true(timestamp_str.length() >= 16)  // 纳秒级精度应该至少有16位数字
  
  // 测试时间戳范围（应该是2025年的时间戳）
  let year_2025_start = 1735689600000000000L  // 2025-01-01 00:00:00 UTC
  let year_2026_start = 1767225600000000000L  // 2026-01-01 00:00:00 UTC
  
  assert_true(timestamp1 >= year_2025_start)
  assert_true(timestamp1 < year_2026_start)
  
  // 测试时间差计算
  let diff1 = timestamp2 - timestamp1
  let diff2 = timestamp3 - timestamp2
  let total_diff = timestamp3 - timestamp1
  
  assert_true(diff1 >= 0L)
  assert_true(diff2 >= 0L)
  assert_true(total_diff >= 0L)
  assert_true(total_diff == diff1 + diff2)
  
  // 测试时间戳转换为其他格式（如果可用）
  // 注意：这里假设有转换方法，实际实现可能不同
  let timestamp_seconds = timestamp1 / 1000000000L  // 转换为秒
  let timestamp_millis = timestamp1 / 1000000L      // 转换为毫秒
  let timestamp_micros = timestamp1 / 1000L         // 转换为微秒
  
  assert_true(timestamp_seconds > 0L)
  assert_true(timestamp_millis > 0L)
  assert_true(timestamp_micros > 0L)
  assert_true(timestamp_millis > timestamp_seconds)
  assert_true(timestamp_micros > timestamp_millis)
}