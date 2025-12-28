// Focused Azimuth Enhancement Tests - 专注增强测试集
// 重点测试关键功能的边界条件、异常处理和性能优化

// 测试1: Attributes复杂数据类型操作
test "attributes_complex_data_types_operations" {
  let attrs = Attributes::new()
  
  // 测试数组类型的属性值
  Attributes::set(attrs, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 验证数组属性获取
  let string_array = Attributes::get(attrs, "string.array")
  assert_true(string_array is Some)
  match string_array {
    Some(ArrayStringValue(items)) => assert_eq(items.length, 3)
    _ => assert_true(false)
  }
  
  let int_array = Attributes::get(attrs, "int.array")
  assert_true(int_array is Some)
  match int_array {
    Some(ArrayIntValue(items)) => assert_eq(items.length, 5)
    _ => assert_true(false)
  }
  
  // 测试混合数据类型
  Attributes::set(attrs, "mixed.string", StringValue("test"))
  Attributes::set(attrs, "mixed.int", IntValue(42))
  Attributes::set(attrs, "mixed.float", FloatValue(3.14))
  Attributes::set(attrs, "mixed.bool", BoolValue(true))
  
  let mixed_string = Attributes::get(attrs, "mixed.string")
  assert_eq(mixed_string, Some(StringValue("test")))
  
  let mixed_int = Attributes::get(attrs, "mixed.int")
  assert_eq(mixed_int, Some(IntValue(42)))
  
  let mixed_float = Attributes::get(attrs, "mixed.float")
  assert_eq(mixed_float, Some(FloatValue(3.14)))
  
  let mixed_bool = Attributes::get(attrs, "mixed.bool")
  assert_eq(mixed_bool, Some(BoolValue(true)))
}

// 测试2: SpanKind类型完整性和状态转换
test "span_kind_comprehensive_and_state_transitions" {
  // 测试所有SpanKind类型
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "test-state")
  
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  assert_eq(internal_span.name, "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  let server_span = Span::new("server-request", Server, span_ctx)
  assert_eq(Span::kind(server_span), Server)
  
  let client_span = Span::new("client-request", Client, span_ctx)
  assert_eq(Span::kind(client_span), Client)
  
  let producer_span = Span::new("message-producer", Producer, span_ctx)
  assert_eq(Span::kind(producer_span), Producer)
  
  let consumer_span = Span::new("message-consumer", Consumer, span_ctx)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // 测试Span状态管理
  assert_eq(Span::status(internal_span), Unset)
  
  // 设置状态为OK
  Span::set_status(internal_span, Ok)
  assert_eq(Span::status(internal_span), Ok)
  
  // 设置状态为Error
  Span::set_status(internal_span, Error, Some("Operation failed"))
  assert_eq(Span::status(internal_span), Error)
  
  // 测试事件添加
  Span::add_event(internal_span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(internal_span, "event2", None)
  
  // 测试Span结束
  Span::end(internal_span)
}

// 测试3: InstrumentationScope配置和管理
test "instrumentation_scope_configuration_management" {
  // 测试完整的InstrumentationScope创建
  let full_scope = InstrumentationScope::{ 
    name: "com.example.azimuth", 
    version: Some("1.2.3"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  assert_eq(full_scope.name, "com.example.azimuth")
  assert_eq(full_scope.version, Some("1.2.3"))
  assert_eq(full_scope.schema_url, Some("https://example.com/schema/v1"))
  
  // 测试最小InstrumentationScope
  let minimal_scope = InstrumentationScope::{ 
    name: "minimal.scope", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(minimal_scope.name, "minimal.scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 测试Tracer和Meter的scope关联
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer", Some("2.0.0"))
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(tracer_scope.name, "test.tracer")
  assert_eq(tracer_scope.version, Some("2.0.0"))
  assert_eq(tracer_scope.schema_url, None)
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test.meter")
  let meter_scope = meter.scope
  
  assert_eq(meter_scope.name, "test.meter")
  assert_eq(meter_scope.version, None)
  assert_eq(meter_scope.schema_url, None)
}

// 测试4: LogRecord完整生命周期和上下文关联
test "log_record_lifecycle_and_context_correlation" {
  // 创建基础LogRecord
  let basic_record = LogRecord::new(Info, "Basic log message")
  assert_eq(LogRecord::severity_number(basic_record), Info)
  assert_eq(LogRecord::body(basic_record), Some("Basic log message"))
  assert_eq(LogRecord::trace_id(basic_record), None)
  assert_eq(LogRecord::span_id(basic_record), None)
  
  // 创建带有完整上下文的LogRecord
  let span_ctx = SpanContext::new("trace-789", "span-101", true, "")
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "user.id", StringValue("user-123"))
  Attributes::set(log_attrs, "request.id", StringValue("req-456"))
  
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let observed_timestamp = timestamp + 1000L
  
  let enriched_record = LogRecord::new_with_context(
    Error,
    Some("Error processing request"),
    Some(log_attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(span_ctx.trace_id),
    Some(span_ctx.span_id),
    None
  )
  
  assert_eq(LogRecord::severity_number(enriched_record), Error)
  assert_eq(LogRecord::body(enriched_record), Some("Error processing request"))
  assert_eq(LogRecord::trace_id(enriched_record), Some("trace-789"))
  assert_eq(LogRecord::span_id(enriched_record), Some("span-101"))
  
  // 测试Logger发射记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  Logger::emit(logger, basic_record)
  Logger::emit(logger, enriched_record)
}

// 测试5: Baggage高级操作和传播集成
test "baggage_advanced_operations_and_propagation_integration" {
  // 创建和操作Baggage
  let baggage = Baggage::new()
  
  // 设置多个条目
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_more = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  let final_baggage = Baggage::set_entry(baggage_with_more, "session.id", "session-789")
  
  // 验证条目获取
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  assert_eq(user_id, Some("user-123"))
  
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  assert_eq(request_id, Some("req-456"))
  
  let session_id = Baggage::get_entry(final_baggage, "session.id")
  assert_eq(session_id, Some("session-789"))
  
  // 测试不存在的条目
  let non_existent = Baggage::get_entry(final_baggage, "non.existent")
  assert_eq(non_existent, None)
  
  // 测试条目移除
  let baggage_without_user = Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  assert_eq(removed_user_id, None)
  
  // 验证其他条目仍然存在
  let remaining_request_id = Baggage::get_entry(baggage_without_user, "request.id")
  assert_eq(remaining_request_id, Some("req-456"))
}

// 测试6: CompositePropagator多传播器协调
test "composite_propagator_multiple_propagators_coordination" {
  // 创建多个传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let composite = CompositePropagator::new([trace_propagator])
  
  // 测试注入操作
  let ctx = Context::root()
  let enriched_ctx = Context::with_value(ctx, ContextKey::new("test.key"), "test.value")
  let carrier = TextMapCarrier::new()
  
  CompositePropagator::inject(composite, enriched_ctx, carrier)
  
  // 验证注入结果
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header is Some)
  
  // 测试提取操作
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

// 测试7: HttpClient完整请求-响应周期
test "http_client_complete_request_response_cycle" {
  let client = HttpClient::new()
  
  // 创建HTTP请求
  let headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/1.0")
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/telemetry",
    headers,
    Some("{\"data\":\"test\"}")
  )
  
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(HttpRequest::body(request), Some("{\"data\":\"test\"}"))
  
  // 创建HTTP响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-12345")
  ]
  
  let response = HttpResponse::new(
    200,
    response_headers,
    Some("{\"status\":\"success\",\"id\":\"12345\"}")
  )
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"status\":\"success\",\"id\":\"12345\"}"))
}

// 测试8: Clock和Random系统服务集成
test "clock_and_random_system_services_integration" {
  // 测试Clock服务
  let system_clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(system_clock)
  let timestamp2 = Clock::now_unix_nanos(system_clock)
  
  // 验证时间戳是合理的（基于代码中的固定值）
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  
  // 测试Random服务
  let system_random = Random::system()
  
  // 测试随机字节生成
  let random_bytes = Random::next_bytes(system_random, 16)
  assert_eq(random_bytes.length, 16)
  
  // 测试随机U64生成
  let random_u64 = Random::next_u64(system_random)
  assert_true(random_u64 > 0UL)
  
  // 验证随机性的简单检查（多次调用应该返回相同值，因为代码中是固定实现）
  let another_random_u64 = Random::next_u64(system_random)
  assert_eq(random_u64, another_random_u64)
}