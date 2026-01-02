// Azimuth Concise Telemetry Test Suite
// 简洁的遥测系统测试用例，专注于核心功能

// 测试1: 基础Span创建与属性设置
test "基础Span创建与属性设置" {
  let tracer = Tracer::new("test-tracer")
  let span = Tracer::start_span(tracer, "operation-span")
  
  Span::set_attribute(span, "user.id", "12345")
  Span::set_attribute(span, "operation.type", "query")
  Span::set_status(span, Ok, "操作成功")
  
  assert_eq(Span::name(span), "operation-span")
  assert_eq(Span::get_attribute(span, "user.id"), Some("12345"))
  assert_eq(Span::get_attribute(span, "operation.type"), Some("query"))
  assert_eq(Span::status(span), Ok)
  
  Span::end(span)
}

// 测试2: Counter度量记录
test "Counter度量记录" {
  let meter = Meter::new("test-meter")
  let counter = Meter::create_counter(meter, "api.requests", "API请求总数", "count")
  
  Counter::add(counter, 1.0, [("endpoint", "/api/data")])
  Counter::add(counter, 3.0, [("endpoint", "/api/users")])
  Counter::add(counter, 2.0, [("endpoint", "/api/data")])
  
  assert_eq(Instrument::name(counter), "api.requests")
  assert_eq(Instrument::description(counter), "API请求总数")
  assert_eq(Instrument::unit(counter), "count")
}

// 测试3: 日志记录器基础功能
test "日志记录器基础功能" {
  let logger = Logger::new("test-logger")
  let info_log = LogRecord::info("用户登录成功", [("user.id", "12345")])
  let error_log = LogRecord::error("数据库连接失败", [("error.code", "500")])
  
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::body(info_log), Some("用户登录成功"))
  assert_eq(LogRecord::body(error_log), Some("数据库连接失败"))
  
  Logger::emit(logger, info_log)
  Logger::emit(logger, error_log)
}

// 测试4: 上下文传播
test "上下文传播" {
  let ctx = Context::root()
  let trace_id = "trace-1234567890"
  let span_id = "span-9876543210"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  let ctx_with_span = Context::with_span(ctx, span_ctx)
  let extracted_span = Context::span_context(ctx_with_span)
  
  assert_eq(SpanContext::trace_id(extracted_span), trace_id)
  assert_eq(SpanContext::span_id(extracted_span), span_id)
  assert_true(SpanContext::is_sampled(extracted_span))
}

// 测试5: Histogram度量记录
test "Histogram度量记录" {
  let meter = Meter::new("test-meter")
  let histogram = Meter::create_histogram(meter, "response.time", "响应时间", "ms")
  
  Histogram::record(histogram, 120.5, [("endpoint", "/api/data")])
  Histogram::record(histogram, 85.3, [("endpoint", "/api/users")])
  Histogram::record(histogram, 200.1, [("endpoint", "/api/data")])
  Histogram::record(histogram, 95.7, [("endpoint", "/api/users")])
  
  assert_eq(Instrument::name(histogram), "response.time")
  assert_eq(Instrument::description(histogram), "响应时间")
  assert_eq(Instrument::unit(histogram), "ms")
}

// 测试6: 资源属性管理
test "资源属性管理" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", "azimuth-service"),
    ("service.version", "1.2.3"),
    ("environment", "production")
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.name"), Some("azimuth-service"))
  assert_eq(Resource::get_attribute(resource_with_attrs, "service.version"), Some("1.2.3"))
  assert_eq(Resource::get_attribute(resource_with_attrs, "environment"), Some("production"))
  assert_eq(Resource::get_attribute(resource_with_attrs, "nonexistent"), None)
}

// 测试7: Span事件记录
test "Span事件记录" {
  let tracer = Tracer::new("test-tracer")
  let span = Tracer::start_span(tracer, "operation-with-events")
  
  Span::add_event(span, "开始处理", [("step", "1")])
  Span::add_event(span, "验证完成", [("step", "2")])
  Span::add_event(span, "处理完成", [("step", "3")])
  
  Span::set_status(span, Ok, "操作成功")
  Span::end(span)
  
  assert_eq(Span::name(span), "operation-with-events")
  assert_eq(Span::status(span), Ok)
}

// 测试8: Gauge度量记录
test "Gauge度量记录" {
  let meter = Meter::new("test-meter")
  let gauge = Meter::create_gauge(meter, "memory.usage", "内存使用量", "bytes")
  
  Gauge::record(gauge, 1024.0 * 1024.0 * 512.0, [("type", "heap")])
  Gauge::record(gauge, 1024.0 * 1024.0 * 128.0, [("type", "non-heap")])
  
  assert_eq(Instrument::name(gauge), "memory.usage")
  assert_eq(Instrument::description(gauge), "内存使用量")
  assert_eq(Instrument::unit(gauge), "bytes")
}

// 测试9: 错误处理与异常记录
test "错误处理与异常记录" {
  let tracer = Tracer::new("test-tracer")
  let span = Tracer::start_span(tracer, "error-prone-operation")
  
  // 模拟错误情况
  Span::set_status(span, Error, "连接超时")
  Span::record_exception(span, "ConnectionTimeout", "无法连接到数据库")
  
  assert_eq(Span::status(span), Error)
  assert_eq(Span::status_message(span), "连接超时")
  
  Span::end(span)
}

// 测试10: 跨服务传播
test "跨服务传播" {
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  let ctx = Context::root()
  let trace_id = "trace-abcdef123456"
  let span_id = "span-789012345678"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let ctx_with_span = Context::with_span(ctx, span_ctx)
  
  // 注入上下文
  W3CTraceContextPropagator::inject(propagator, ctx_with_span, carrier)
  
  // 提取上下文
  let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
  let extracted_span = Context::span_context(extracted_ctx)
  
  assert_eq(SpanContext::trace_id(extracted_span), trace_id)
  assert_eq(SpanContext::span_id(extracted_span), span_id)
  assert_true(SpanContext::is_sampled(extracted_span))
}