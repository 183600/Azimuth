// Telemetry Data Consistency Tests
// 验证遥测数据在不同操作和传输过程中的一致性

test "telemetry data consistency across operations" {
  // 创建测试资源
  let resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource, "service.name", azimuth::StringValue("test-service"))
  azimuth::Resource::set_attribute(resource, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(resource, "service.instance.id", azimuth::StringValue("instance-123"))
  
  // 创建测试span
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let span = azimuth::Span::new("test-operation", azimuth::Internal, span_ctx)
  
  // 设置span属性
  azimuth::Span::set_attribute(span, "user.id", azimuth::IntValue(42))
  azimuth::Span::set_attribute(span, "request.size", azimuth::IntValue(1024))
  azimuth::Span::set_attribute(span, "response.time", azimuth::FloatValue(123.45))
  azimuth::Span::set_attribute(span, "cache.hit", azimuth::BoolValue(true))
  
  // 验证属性一致性
  let user_id = azimuth::Span::get_attribute(span, "user.id")
  let request_size = azimuth::Span::get_attribute(span, "request.size")
  let response_time = azimuth::Span::get_attribute(span, "response.time")
  let cache_hit = azimuth::Span::get_attribute(span, "cache.hit")
  
  assert_eq(user_id, Some(azimuth::IntValue(42)))
  assert_eq(request_size, Some(azimuth::IntValue(1024)))
  assert_eq(response_time, Some(azimuth::FloatValue(123.45)))
  assert_eq(cache_hit, Some(azimuth::BoolValue(true)))
  
  // 验证span上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(span)), "trace-12345")
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::context(span)), "span-67890")
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::context(span)))
}

test "metrics data consistency across aggregations" {
  // 创建测试指标
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "test-meter")
  let counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let histogram = azimuth::Meter::create_histogram(meter, "http.request.duration")
  
  // 记录指标数据
  azimuth::Counter::add(counter, 1, [{"method", azimuth::StringValue("GET")}])
  azimuth::Counter::add(counter, 1, [{"method", azimuth::StringValue("GET")}])
  azimuth::Counter::add(counter, 1, [{"method", azimuth::StringValue("POST")}])
  
  azimuth::Histogram::record(histogram, 100.0, [{"method", azimuth::StringValue("GET")}])
  azimuth::Histogram::record(histogram, 200.0, [{"method", azimuth::StringValue("GET")}])
  azimuth::Histogram::record(histogram, 150.0, [{"method", azimuth::StringValue("POST")}])
  
  // 验证指标数据一致性
  let counter_metrics = azimuth::Counter::collect(counter)
  assert_true(counter_metrics.length > 0)
  
  let histogram_metrics = azimuth::Histogram::collect(histogram)
  assert_true(histogram_metrics.length > 0)
}

test "log record consistency with trace correlation" {
  // 创建日志记录器
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "test-logger")
  
  // 创建span上下文
  let span_ctx = azimuth::SpanContext::new("trace-98765", "span-43210", true, "key1=value1")
  
  // 创建关联的日志记录
  let log_record1 = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(log_record1, "Processing request started")
  azimuth::LogRecord::set_severity(log_record1, azimuth::INFO)
  azimuth::LogRecord::set_trace_id(log_record1, azimuth::SpanContext::trace_id(span_ctx))
  azimuth::LogRecord::set_span_id(log_record1, azimuth::SpanContext::span_id(span_ctx))
  
  let log_record2 = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(log_record2, "Processing request completed")
  azimuth::LogRecord::set_severity(log_record2, azimuth::INFO)
  azimuth::LogRecord::set_trace_id(log_record2, azimuth::SpanContext::trace_id(span_ctx))
  azimuth::LogRecord::set_span_id(log_record2, azimuth::SpanContext::span_id(span_ctx))
  
  // 验证日志记录与追踪的关联一致性
  assert_eq(azimuth::LogRecord::trace_id(log_record1), "trace-98765")
  assert_eq(azimuth::LogRecord::span_id(log_record1), "span-43210")
  assert_eq(azimuth::LogRecord::trace_id(log_record2), "trace-98765")
  assert_eq(azimuth::LogRecord::span_id(log_record2), "span-43210")
  
  // 验证时间戳顺序一致性
  assert_true(azimuth::LogRecord::timestamp(log_record2) >= azimuth::LogRecord::timestamp(log_record1))
}