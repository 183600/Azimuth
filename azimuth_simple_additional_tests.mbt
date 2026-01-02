// Azimuth Additional Simple Tests
// 简单补充测试用例

// 测试1: 基础度量操作
test "基础度量操作" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "basic.test")
  
  // 创建计数器
  let counter = Meter::create_counter(meter, "requests.total")
  
  // 记录计数
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  // 验证计数器属性
  assert_eq(counter.name, "requests.total")
}

// 测试2: 基础Span操作
test "基础Span操作" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "span.test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "test.operation")
  
  // 测试span基本属性
  let span_name = Span::name(span)
  assert_eq(span_name, "test.operation")
  
  // 测试span上下文
  let span_ctx = Span::span_context(span)
  assert_true(SpanContext::is_valid(span_ctx))
  
  // 添加事件
  let event_attrs = [
    ("operation.type", StringValue("test")),
    ("operation.result", StringValue("success"))
  ]
  Span::add_event(span, "operation.completed", Some(event_attrs))
  
  // 结束span
  Span::end(span)
}

// 测试3: 基础日志操作
test "基础日志操作" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log.test")
  
  // 创建日志属性
  let log_attrs = Attributes::{ values: [
    ("component", StringValue("test")),
    ("version", StringValue("1.0.0"))
  ]}
  
  // 创建日志记录
  let log_record = LogRecord::new_with_context(
    Info, 
    Some("Test log message"), 
    Some(log_attrs), 
    None, None, None, None, None
  )
  
  // 验证日志记录
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  
  // 发射日志
  Logger::emit(logger, log_record)
}

// 测试4: 基础上下文操作
test "基础上下文操作" {
  let ctx = Context::root()
  
  // 添加上下文值
  let correlation_key = ContextKey::new("correlation.id")
  let user_key = ContextKey::new("user.id")
  
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "user-67890")
  
  // 验证上下文值
  let retrieved_correlation = Context::get(ctx_with_user, correlation_key)
  let retrieved_user = Context::get(ctx_with_user, user_key)
  
  assert_eq(retrieved_correlation, Some("corr-12345"))
  assert_eq(retrieved_user, Some("user-67890"))
}

// 测试5: 基础直方图操作
test "基础直方图操作" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram.test")
  
  // 创建直方图
  let histogram = Meter::create_histogram(
    meter, 
    "response.time", 
    Some("Response time"), 
    Some("ms")
  )
  
  // 记录不同的值
  Histogram::record(histogram, 10.5)
  Histogram::record(histogram, 25.8)
  Histogram::record(histogram, 15.2)
  
  // 验证直方图属性
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time"))
  assert_eq(histogram.unit, Some("ms"))
}