// 错误处理和恢复测试
// 测试遥测系统在错误情况下的处理和恢复能力

test "Span错误处理测试" {
  // 测试Span在错误情况下的状态管理
  let span_ctx = SpanContext::new("error-trace", "error-span", true, "")
  let error_span = Span::new("error-operation", Internal, span_ctx)
  
  // 测试正常操作后发生错误
  Span::add_event(error_span, "operation.started", None)
  Span::add_event(error_span, "operation.progress", Some([
    ("progress.percent", IntValue(50)),
    ("step", StringValue("processing"))
  ]))
  
  // 添加错误事件
  Span::add_event(error_span, "operation.error", Some([
    ("error.type", StringValue("BusinessException")),
    ("error.message", StringValue("Invalid business rule violation")),
    ("error.code", IntValue(4001)),
    ("error.severity", StringValue("high"))
  ]))
  
  // 设置错误状态
  Span::set_status(error_span, Error, Some("Operation failed due to business rule violation"))
  
  // 验证Span状态
  assert_eq(Span::status(error_span), Error)
  
  // 结束Span
  Span::end(error_span)
  
  // 测试已结束Span的错误处理
  Span::add_event(error_span, "post.end.event", None) // 应该被忽略或处理
  Span::set_status(error_span, Ok, Some("Attempt to reset status")) // 应该被忽略或处理
}

test "度量系统错误处理测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error-test-meter")
  
  // 测试Counter的错误处理
  let error_counter = Meter::create_counter(meter, "errors.total",
    Some("Total number of errors"), Some("errors"))
  
  // 测试正常记录
  Counter::add(error_counter, 1.0)
  
  // 测试负值记录（应该被处理或忽略）
  Counter::add(error_counter, -1.0)
  
  // 测试极大值记录
  Counter::add(error_counter, 999999999.0)
  
  // 测试NaN值处理（如果支持）
  // Counter::add(error_counter, Double::nan) // 如果MoonBit支持NaN
  
  // 测试Histogram的错误处理
  let latency_histogram = Meter::create_histogram(meter, "latency.errors",
    Some("Error latency distribution"), Some("ms"))
  
  // 测试负值记录
  Histogram::record(latency_histogram, -100.0)
  
  // 测试极大值记录
  Histogram::record(latency_histogram, 999999999.0)
  
  // 测试零值记录
  Histogram::record(latency_histogram, 0.0)
  
  // 测试带错误属性的记录
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error.type", StringValue("TimeoutException"))
  Attributes::set(error_attrs, "error.retry_count", IntValue(3))
  Attributes::set(error_attrs, "error.timeout_ms", IntValue(30000))
  
  Counter::add(error_counter, 5.0, Some(error_attrs))
  Histogram::record(latency_histogram, 30000.0, Some(error_attrs))
}

test "日志系统错误处理测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error-test-logger")
  
  // 测试错误日志记录
  let error_log = LogRecord::new(Error, "Database connection failed")
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(error_log), Some("Database connection failed"))
  
  // 测试致命错误日志
  let fatal_log = LogRecord::new(Fatal, "System out of memory")
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  assert_eq(LogRecord::body(fatal_log), Some("System out of memory"))
  
  // 测试带异常信息的日志
  let exception_attrs = Attributes::new()
  Attributes::set(exception_attrs, "exception.type", StringValue("SQLException"))
  Attributes::set(exception_attrs, "exception.message", StringValue("Connection timeout"))
  Attributes::set(exception_attrs, "exception.code", StringValue("SQL0805"))
  Attributes::set(exception_attrs, "exception.stack_trace", StringValue("at com.example.Database.connect(Database.java:123)"))
  
  let exception_log = LogRecord::new_with_context(
    Error,
    Some("Database operation failed"),
    Some(exception_attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("error-trace-123"),
    Some("error-span-456"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(exception_log), Error)
  assert_eq(LogRecord::body(exception_log), Some("Database operation failed"))
  assert_eq(LogRecord::trace_id(exception_log), Some("error-trace-123"))
  assert_eq(LogRecord::span_id(exception_log), Some("error-span-456"))
  
  // 测试日志发射
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  Logger::emit(logger, exception_log)
}

test "传播系统错误处理测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 测试损坏的traceparent格式
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-format-data")
  TextMapCarrier::set(corrupted_carrier, "baggage", "key=value")
  
  // 尝试提取损坏的上下文
  let ctx_from_corrupted = CompositePropagator::extract(composite_propagator, corrupted_carrier)
  let extracted_value = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // 测试空traceparent
  let empty_trace_carrier = TextMapCarrier::new()
  TextMapCarrier::set(empty_trace_carrier, "traceparent", "")
  TextMapCarrier::set(empty_trace_carrier, "baggage", "user.id=123")
  
  let ctx_from_empty = CompositePropagator::extract(composite_propagator, empty_trace_carrier)
  let extracted_from_empty = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  assert_eq(extracted_from_empty, Some("true"))
  
  // 测试损坏的baggage格式
  let corrupted_baggage_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_baggage_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(corrupted_baggage_carrier, "baggage", "invalid-baggage-format=")
  
  let ctx_from_corrupted_baggage = CompositePropagator::extract(composite_propagator, corrupted_baggage_carrier)
  let extracted_from_corrupted_baggage = Context::get(ctx_from_corrupted_baggage, ContextKey::new("extracted"))
  assert_eq(extracted_from_corrupted_baggage, Some("true"))
  
  // 测试注入到无效载体
  let ctx = Context::root()
  let invalid_carrier = TextMapCarrier::new()
  
  // 模拟载体设置失败
  CompositePropagator::inject(composite_propagator, ctx, invalid_carrier)
  // 验证注入是否仍然工作
  let injected_traceparent = TextMapCarrier::get(invalid_carrier, "traceparent")
  assert!(injected_traceparent.is_some())
}

test "HTTP客户端错误处理测试" {
  let client = HttpClient::new()
  
  // 测试错误响应处理
  let error_responses = [
    HttpResponse::new(400, [], Some("Bad Request")),
    HttpResponse::new(401, [], Some("Unauthorized")),
    HttpResponse::new(403, [], Some("Forbidden")),
    HttpResponse::new(404, [], Some("Not Found")),
    HttpResponse::new(500, [], Some("Internal Server Error")),
    HttpResponse::new(502, [], Some("Bad Gateway")),
    HttpResponse::new(503, [], Some("Service Unavailable")),
    HttpResponse::new(504, [], Some("Gateway Timeout"))
  ]
  
  for response in error_responses {
    assert_true(HttpResponse::status_code(response) >= 400)
    assert!(HttpResponse::body(response).is_some())
  }
  
  // 测试空响应体
  let empty_response = HttpResponse::new(204, [], None)
  assert_eq(HttpResponse::status_code(empty_response), 204)
  assert_eq(HttpResponse::body(empty_response), None)
  
  // 测试带有错误头部的响应
  let error_headers = [
    ("X-Error-Code", "DATABASE_ERROR"),
    ("X-Error-Message", "Connection pool exhausted"),
    ("X-Retry-After", "30"),
    ("X-Error-Severity", "high")
  ]
  let headers_error_response = HttpResponse::new(503, error_headers, Some("Service temporarily unavailable"))
  
  assert_eq(HttpResponse::status_code(headers_error_response), 503)
  assert!(HttpResponse::body(headers_error_response).is_some())
  
  // 测试超时场景
  let timeout_request = HttpRequest::new("GET", "https://slow-api.example.com/timeout", [], None)
  assert_eq(HttpRequest::http_method(timeout_request), "GET")
  assert_eq(HttpRequest::url(timeout_request), "https://slow-api.example.com/timeout")
}

test "资源耗尽和恢复测试" {
  // 测试内存资源管理
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "resource-test-meter")
  
  // 创建大量度量对象
  let counters = []
  for i in range(0, 100) {
    let counter_name = "counter." + i.to_string()
    let counter = Meter::create_counter(meter, counter_name, Some("Test counter"), Some("count"))
    counters.push(counter)
  }
  
  // 测试大量度量记录
  for counter in counters {
    Counter::add(counter, 1.0)
    Counter::add(counter, 2.0)
    Counter::add(counter, 3.0)
  }
  
  // 测试大量属性创建
  let large_attrs = Attributes::new()
  for i in range(0, 50) {
    let key = "attr." + i.to_string()
    let value = StringValue("value." + i.to_string())
    Attributes::set(large_attrs, key, value)
  }
  
  // 测试大量Span创建
  let spans = []
  for i in range(0, 50) {
    let span_ctx = SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, "")
    let span = Span::new("span-" + i.to_string(), Internal, span_ctx)
    spans.push(span)
  }
  
  // 清理资源
  for span in spans {
    Span::end(span)
  }
}

test "网络分区和恢复测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 模拟网络分区前的正常传播
  let normal_ctx = Context::root()
  let normal_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, normal_ctx, normal_carrier)
  
  // 模拟网络分区（传播失败）
  let partition_carrier = TextMapCarrier::new()
  // 故意不设置必要的头部，模拟网络分区
  TextMapCarrier::set(partition_carrier, "X-Network-Status", "partitioned")
  
  let ctx_from_partition = CompositePropagator::extract(composite_propagator, partition_carrier)
  let extracted_from_partition = Context::get(ctx_from_partition, ContextKey::new("extracted"))
  assert_eq(extracted_from_partition, Some("true"))
  
  // 模拟网络恢复后的传播
  let recovery_carrier = TextMapCarrier::new()
  TextMapCarrier::set(recovery_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(recovery_carrier, "baggage", "user.id=123,session.id=456")
  TextMapCarrier::set(recovery_carrier, "X-Network-Status", "recovered")
  
  let ctx_from_recovery = CompositePropagator::extract(composite_propagator, recovery_carrier)
  let extracted_from_recovery = Context::get(ctx_from_recovery, ContextKey::new("extracted"))
  assert_eq(extracted_from_recovery, Some("true"))
  
  // 验证网络状态头部
  let network_status = TextMapCarrier::get(recovery_carrier, "X-Network-Status")
  assert_eq(network_status, Some("recovered"))
}

test "数据损坏和修复测试" {
  // 测试损坏的SpanContext
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  let valid_span_ctx = SpanContext::new("valid-trace", "valid-span", true, "key=value")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  
  // 测试损坏的属性值
  let corrupted_attrs = Attributes::new()
  Attributes::set(corrupted_attrs, "valid.key", StringValue("valid-value"))
  // 模拟设置损坏的属性
  Attributes::set(corrupted_attrs, "", StringValue("empty-key"))
  Attributes::set(corrupted_attrs, "null.key", StringValue(""))
  
  // 测试损坏的日志记录
  let corrupted_log = LogRecord::new_with_context(
    Error,
    Some(""), // 空错误消息
    Some(corrupted_attrs),
    Some(-1L), // 无效时间戳
    Some(-1L), // 无效观察时间戳
    Some(""),  // 空trace_id
    Some(""),  // 空span_id
    None       // 无上下文
  )
  
  assert_eq(LogRecord::severity_number(corrupted_log), Error)
  assert_eq(LogRecord::body(corrupted_log), Some(""))
  
  // 测试修复机制
  let fixed_log = LogRecord::new_with_context(
    Error,
    Some("Fixed error message"),
    Some(Attributes::new()),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some("fixed-trace-id"),
    Some("fixed-span-id"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::body(fixed_log), Some("Fixed error message"))
  assert_eq(LogRecord::trace_id(fixed_log), Some("fixed-trace-id"))
  assert_eq(LogRecord::span_id(fixed_log), Some("fixed-span-id"))
}