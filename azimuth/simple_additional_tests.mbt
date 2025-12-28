// Azimuth Telemetry System - Simple Additional Test Cases
// 简单的补充测试用例，确保能正常编译和运行

test "Span基本操作测试" {
  // 测试Span的基本操作
  let span = Span::new("test-span", Internal, SpanContext::new("trace123", "span456", true, ""))
  
  // 验证Span名称
  assert_eq(Span::name(span), "test-span")
  
  // 验证Span是否在记录状态
  assert_true(Span::is_recording(span))
  
  // 验证Span上下文
  let ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(ctx), "trace123")
  assert_eq(SpanContext::span_id(ctx), "span456")
  assert_true(SpanContext::is_sampled(ctx))
  
  // 结束Span
  Span::end(span)
}

test "Counter基本操作测试" {
  // 测试Counter的基本操作
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  let counter = Meter::create_counter(meter, "test-counter")
  
  // 验证Counter名称
  assert_eq(counter.name, "test-counter")
  
  // 添加值（不验证返回值，只确保不崩溃）
  Counter::add(counter, 10.5)
  
  // 测试通过表示没有崩溃
  assert_true(true)
}

test "Context基本操作测试" {
  // 测试Context的基本操作
  let root_ctx = Context::root()
  let key = ContextKey::new("test-key")
  
  // 设置值
  let ctx_with_value = Context::with_value(root_ctx, key, "test-value")
  
  // 获取值
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test-value"))
  
  // 测试不存在的键
  let missing_key = ContextKey::new("missing-key")
  let missing_value = Context::get(ctx_with_value, missing_key)
  assert_eq(missing_value, None)
}

test "LogRecord基本操作测试" {
  // 测试LogRecord的基本操作
  let log_record = LogRecord::new(Info, "Test log message")
  
  // 验证日志级别
  match LogRecord::severity_number(log_record) {
    Info => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证日志内容
  assert_eq(LogRecord::body(log_record), Some("Test log message"))
  
  // 验证时间戳为空
  assert_eq(log_record.timestamp, None)
  assert_eq(log_record.observed_timestamp, None)
}

test "Resource基本操作测试" {
  // 测试Resource的基本操作
  let resource = Resource::new()
  
  // 验证初始资源为空
  let service_name = Resource::get_attribute(resource, "service.name")
  assert_eq(service_name, None)
  
  // 添加属性
  let attrs = [("service.name", StringValue("test-service"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // 验证属性存在
  let retrieved_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match retrieved_name {
    Some(StringValue(value)) => assert_eq(value, "test-service")
    _ => assert_true(false)
  }
}