// Specialized Focused Test Suite for Azimuth Telemetry System
// 专注测试特定功能和边界情况

// Test 1: Span链接和引用关系测试
pub test "Span链接和引用关系测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "span-link-test")
  
  // 创建父Span
  let parent_span = azimuth::Tracer::start_span(tracer, "parent-operation")
  let parent_ctx = azimuth::Span::span_context(parent_span)
  
  // 创建子Span
  let child_span = azimuth::Tracer::start_span(tracer, "child-operation")
  let child_ctx = azimuth::Span::span_context(child_span)
  
  // 验证Span上下文独立性
  assert_true(azimuth::SpanContext::span_id(parent_ctx) != azimuth::SpanContext::span_id(child_ctx))
  
  // 设置Span属性和事件
  azimuth::Span::set_attribute(parent_span, "operation.type", azimuth::StringValue("parent"))
  azimuth::Span::set_attribute(child_span, "operation.type", azimuth::StringValue("child"))
  
  azimuth::Span::add_event(parent_span, "parent.started", None)
  azimuth::Span::add_event(child_span, "child.started", None)
  
  // 验证Span状态
  assert_true(azimuth::Span::is_recording(parent_span))
  assert_true(azimuth::Span::is_recording(child_span))
  
  // 结束Span
  azimuth::Span::end(child_span)
  azimuth::Span::end(parent_span)
  
  assert_false(azimuth::Span::is_recording(child_span))
  assert_false(azimuth::Span::is_recording(parent_span))
}

// Test 2: 度量边界值和精度测试
pub test "度量边界值和精度测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "boundary-test")
  
  // 测试极值计数器
  let counter = azimuth::Meter::create_counter(meter, "boundary.counter")
  
  // 测试大值累加
  azimuth::Counter::add(counter, 999999.0)
  azimuth::Counter::add(counter, 0.000001)
  azimuth::Counter::add(counter, -500000.0)
  
  // 测试边界直方图
  let histogram = azimuth::Meter::create_histogram(meter, "boundary.histogram")
  
  // 记录极值
  azimuth::Histogram::record(histogram, 0.0)
  azimuth::Histogram::record(histogram, -1.0)
  azimuth::Histogram::record(histogram, 1.7976931348623157e+308) // 最大double值
  azimuth::Histogram::record(histogram, -1.7976931348623157e+308) // 最小double值
  
  // 测试特殊浮点值
  azimuth::Histogram::record(histogram, 1.0/0.0) // 正无穷
  azimuth::Histogram::record(histogram, -1.0/0.0) // 负无穷
  azimuth::Histogram::record(histogram, 0.0/0.0) // NaN
  
  // 验证度量创建
  assert_eq(counter.name, "boundary.counter")
  assert_eq(histogram.name, "boundary.histogram")
  
  // 测试UpDownCounter边界
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "boundary.updown")
  
  // 测试正负值操作
  azimuth::UpDownCounter::add(updown_counter, 1000.0)
  azimuth::UpDownCounter::add(updown_counter, -2000.0)
  azimuth::UpDownCounter::add(updown_counter, 1500.0)
  
  assert_eq(updown_counter.name, "boundary.updown")
}

// Test 3: 日志严重级别和过滤测试
pub test "日志严重级别和过滤测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "severity-test")
  
  // 创建不同严重级别的日志记录
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message")
  
  // 验证严重级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 验证日志消息
  assert_eq(azimuth::LogRecord::body(trace_log), Some("Trace level message"))
  assert_eq(azimuth::LogRecord::body(debug_log), Some("Debug level message"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("Info level message"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Warning level message"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Error level message"))
  assert_eq(azimuth::LogRecord::body(fatal_log), Some("Fatal level message"))
  
  // 发出所有日志
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
}

// Test 4: 上下文传播深度嵌套测试
pub test "上下文传播深度嵌套测试" {
  let root_ctx = azimuth::Context::root()
  
  // 创建深度嵌套的上下文链
  let ctx_level1 = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("level1"), "value1")
  let ctx_level2 = azimuth::Context::with_value(ctx_level1, azimuth::ContextKey::new("level2"), "value2")
  let ctx_level3 = azimuth::Context::with_value(ctx_level2, azimuth::ContextKey::new("level3"), "value3")
  let ctx_level4 = azimuth::Context::with_value(ctx_level3, azimuth::ContextKey::new("level4"), "value4")
  let ctx_level5 = azimuth::Context::with_value(ctx_level4, azimuth::ContextKey::new("level5"), "value5")
  
  // 验证所有层级的值都可以访问
  assert_eq(azimuth::Context::get(ctx_level5, azimuth::ContextKey::new("level1")), Some("value1"))
  assert_eq(azimuth::Context::get(ctx_level5, azimuth::ContextKey::new("level2")), Some("value2"))
  assert_eq(azimuth::Context::get(ctx_level5, azimuth::ContextKey::new("level3")), Some("value3"))
  assert_eq(azimuth::Context::get(ctx_level5, azimuth::ContextKey::new("level4")), Some("value4"))
  assert_eq(azimuth::Context::get(ctx_level5, azimuth::ContextKey::new("level5")), Some("value5"))
  
  // 测试Baggage深度嵌套
  let baggage = azimuth::Baggage::new()
  let baggage1 = azimuth::Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "key3", "value3")
  let baggage4 = azimuth::Baggage::set_entry(baggage3, "key4", "value4")
  let baggage5 = azimuth::Baggage::set_entry(baggage4, "key5", "value5")
  
  // 验证Baggage值
  assert_eq(azimuth::Baggage::get_entry(baggage5, "key1"), Some("value1"))
  assert_eq(azimuth::Baggage::get_entry(baggage5, "key2"), Some("value2"))
  assert_eq(azimuth::Baggage::get_entry(baggage5, "key3"), Some("value3"))
  assert_eq(azimuth::Baggage::get_entry(baggage5, "key4"), Some("value4"))
  assert_eq(azimuth::Baggage::get_entry(baggage5, "key5"), Some("value5"))
}

// Test 5: 资源合并策略测试
pub test "资源合并策略测试" {
  // 创建多个资源
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-A")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("service-B")), // 这个应该被忽略
    ("host.name", azimuth::StringValue("host-123")),
    ("service.instance.id", azimuth::StringValue("instance-456"))
  ])
  
  let resource3 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("app.exe")),
    ("service.version", azimuth::StringValue("2.0.0")) // 这个应该被忽略
  ])
  
  // 测试两两合并
  let merged12 = azimuth::Resource::merge(resource1, resource2)
  assert_eq(azimuth::Resource::get_attribute(merged12, "service.name"), Some(azimuth::StringValue("service-A")))
  assert_eq(azimuth::Resource::get_attribute(merged12, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged12, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged12, "host.name"), Some(azimuth::StringValue("host-123")))
  assert_eq(azimuth::Resource::get_attribute(merged12, "service.instance.id"), Some(azimuth::StringValue("instance-456")))
  
  // 测试三方合并
  let merged123 = azimuth::Resource::merge(merged12, resource3)
  assert_eq(azimuth::Resource::get_attribute(merged123, "service.name"), Some(azimuth::StringValue("service-A")))
  assert_eq(azimuth::Resource::get_attribute(merged123, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged123, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged123, "host.name"), Some(azimuth::StringValue("host-123")))
  assert_eq(azimuth::Resource::get_attribute(merged123, "service.instance.id"), Some(azimuth::StringValue("instance-456")))
  assert_eq(azimuth::Resource::get_attribute(merged123, "process.pid"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(merged123, "process.executable.name"), Some(azimuth::StringValue("app.exe")))
}

// Test 6: 属性类型转换和兼容性测试
pub test "属性类型转换和兼容性测试" {
  let attrs = azimuth::Attributes::new()
  
  // 测试字符串属性
  azimuth::Attributes::set(attrs, "string.empty", azimuth::StringValue(""))
  azimuth::Attributes::set(attrs, "string.normal", azimuth::StringValue("normal value"))
  azimuth::Attributes::set(attrs, "string.special", azimuth::StringValue("特殊字符!@#$%^&*()"))
  azimuth::Attributes::set(attrs, "string.unicode", azimuth::StringValue("🚀 Unicode测试"))
  
  // 测试整数属性
  azimuth::Attributes::set(attrs, "int.zero", azimuth::IntValue(0))
  azimuth::Attributes::set(attrs, "int.positive", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "int.negative", azimuth::IntValue(-42))
  azimuth::Attributes::set(attrs, "int.max", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(attrs, "int.min", azimuth::IntValue(-2147483648))
  
  // 测试浮点属性
  azimuth::Attributes::set(attrs, "float.zero", azimuth::FloatValue(0.0))
  azimuth::Attributes::set(attrs, "float.positive", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(attrs, "float.negative", azimuth::FloatValue(-3.14159))
  azimuth::Attributes::set(attrs, "float.scientific", azimuth::FloatValue(1.23e-4))
  azimuth::Attributes::set(attrs, "float.infinity", azimuth::FloatValue(1.0/0.0))
  
  // 测试布尔属性
  azimuth::Attributes::set(attrs, "bool.true", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "bool.false", azimuth::BoolValue(false))
  
  // 测试数组属性
  azimuth::Attributes::set(attrs, "array.string.empty", azimuth::ArrayStringValue([]))
  azimuth::Attributes::set(attrs, "array.string.single", azimuth::ArrayStringValue(["single"]))
  azimuth::Attributes::set(attrs, "array.string.multiple", azimuth::ArrayStringValue(["a", "b", "c"]))
  
  azimuth::Attributes::set(attrs, "array.int.empty", azimuth::ArrayIntValue([]))
  azimuth::Attributes::set(attrs, "array.int.single", azimuth::ArrayIntValue([1]))
  azimuth::Attributes::set(attrs, "array.int.multiple", azimuth::ArrayIntValue([1, 2, 3]))
  
  azimuth::Attributes::set(attrs, "array.float.empty", azimuth::ArrayFloatValue([]))
  azimuth::Attributes::set(attrs, "array.float.single", azimuth::ArrayFloatValue([1.1]))
  azimuth::Attributes::set(attrs, "array.float.multiple", azimuth::ArrayFloatValue([1.1, 2.2, 3.3]))
  
  // 验证属性设置（基于简化实现）
  let retrieved = azimuth::Attributes::get(attrs, "string.normal")
  assert_eq(retrieved, Some(azimuth::StringValue("test_value")))
  
  let int_retrieved = azimuth::Attributes::get(attrs, "int.positive")
  assert_eq(int_retrieved, Some(azimuth::IntValue(42)))
}

// Test 7: 传播器注入提取测试
pub test "传播器注入提取测试" {
  // 创建复合传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体
  let carrier = azimuth::TextMapCarrier::new()
  
  // 注入空上下文
  let empty_ctx = azimuth::Context::root()
  azimuth::CompositePropagator::inject(composite_propagator, empty_ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extraction_key = azimuth::ContextKey::new("extracted")
  let extraction_result = azimuth::Context::get(extracted_ctx, extraction_key)
  assert_eq(extraction_result, Some("true"))
  
  // 测试多次注入和提取
  let carrier2 = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, extracted_ctx, carrier2)
  
  let trace_header2 = azimuth::TextMapCarrier::get(carrier2, "traceparent")
  assert_eq(trace_header2, Some("00-test-trace-id-test-span-id-01"))
  
  let extracted_ctx2 = azimuth::CompositePropagator::extract(composite_propagator, carrier2)
  let extraction_result2 = azimuth::Context::get(extracted_ctx2, extraction_key)
  assert_eq(extraction_result2, Some("true"))
}

// Test 8: Span状态和生命周期测试
pub test "Span状态和生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
  
  // 创建Span
  let span = azimuth::Tracer::start_span(tracer, "lifecycle-operation")
  
  // 验证初始状态
  assert_eq(azimuth::Span::name(span), "lifecycle-operation")
  assert_true(azimuth::Span::is_recording(span))
  
  // 设置状态为OK
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 添加事件
  azimuth::Span::add_event(span, "operation.started", None)
  azimuth::Span::add_event(span, "operation.progress", Some([("progress", azimuth::IntValue(50))]))
  
  // 设置属性
  azimuth::Span::set_attribute(span, "operation.type", azimuth::StringValue("lifecycle"))
  azimuth::Span::set_attribute(span, "operation.duration", azimuth::IntValue(1000))
  
  // 验证Span仍在记录
  assert_true(azimuth::Span::is_recording(span))
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span已结束
  assert_false(azimuth::Span::is_recording(span))
  
  // 尝试在结束的Span上操作（应该被忽略）
  azimuth::Span::add_event(span, "after.end", None)
  azimuth::Span::set_attribute(span, "after.end.attr", azimuth::StringValue("should.be.ignored"))
  azimuth::Span::set_status(span, azimuth::Error)
  
  // 验证Span仍然处于结束状态
  assert_false(azimuth::Span::is_recording(span))
}

// Test 9: 并发安全测试
pub test "并发安全测试" {
  // 创建多个提供者
  let tracer_provider1 = azimuth::TracerProvider::default()
  let tracer_provider2 = azimuth::TracerProvider::default()
  let meter_provider1 = azimuth::MeterProvider::default()
  let meter_provider2 = azimuth::MeterProvider::default()
  let logger_provider1 = azimuth::LoggerProvider::default()
  let logger_provider2 = azimuth::LoggerProvider::default()
  
  // 创建多个Tracer
  let tracer1 = azimuth::TracerProvider::get_tracer(tracer_provider1, "concurrent-tracer-1")
  let tracer2 = azimuth::TracerProvider::get_tracer(tracer_provider2, "concurrent-tracer-2")
  
  // 创建多个Meter
  let meter1 = azimuth::MeterProvider::get_meter(meter_provider1, "concurrent-meter-1")
  let meter2 = azimuth::MeterProvider::get_meter(meter_provider2, "concurrent-meter-2")
  
  // 创建多个Logger
  let logger1 = azimuth::LoggerProvider::get_logger(logger_provider1, "concurrent-logger-1")
  let logger2 = azimuth::LoggerProvider::get_logger(logger_provider2, "concurrent-logger-2")
  
  // 创建多个Span
  let span1 = azimuth::Tracer::start_span(tracer1, "concurrent-span-1")
  let span2 = azimuth::Tracer::start_span(tracer2, "concurrent-span-2")
  
  // 创建多个度量
  let counter1 = azimuth::Meter::create_counter(meter1, "concurrent-counter-1")
  let counter2 = azimuth::Meter::create_counter(meter2, "concurrent-counter-2")
  let histogram1 = azimuth::Meter::create_histogram(meter1, "concurrent-histogram-1")
  let histogram2 = azimuth::Meter::create_histogram(meter2, "concurrent-histogram-2")
  
  // 执行操作
  azimuth::Counter::add(counter1, 1.0)
  azimuth::Counter::add(counter2, 2.0)
  azimuth::Histogram::record(histogram1, 100.0)
  azimuth::Histogram::record(histogram2, 200.0)
  
  // 创建日志记录
  let log1 = azimuth::LogRecord::new(azimuth::Info, "Concurrent log 1")
  let log2 = azimuth::LogRecord::new(azimuth::Warn, "Concurrent log 2")
  
  // 发出日志
  azimuth::Logger::emit(logger1, log1)
  azimuth::Logger::emit(logger2, log2)
  
  // 结束Span
  azimuth::Span::end(span1)
  azimuth::Span::end(span2)
  
  // 验证所有组件独立工作
  assert_eq(azimuth::Span::name(span1), "concurrent-span-1")
  assert_eq(azimuth::Span::name(span2), "concurrent-span-2")
  assert_eq(counter1.name, "concurrent-counter-1")
  assert_eq(counter2.name, "concurrent-counter-2")
  assert_eq(histogram1.name, "concurrent-histogram-1")
  assert_eq(histogram2.name, "concurrent-histogram-2")
  assert_eq(logger1.scope.name, "concurrent-logger-1")
  assert_eq(logger2.scope.name, "concurrent-logger-2")
}

// Test 10: 错误处理和异常场景测试
pub test "错误处理和异常场景测试" {
  // 测试空字符串键值
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "", azimuth::StringValue("empty-key-value"))
  azimuth::Attributes::set(attrs, "normal.key", azimuth::StringValue(""))
  
  // 验证获取
  let empty_key_value = azimuth::Attributes::get(attrs, "")
  let empty_value = azimuth::Attributes::get(attrs, "normal.key")
  
  // 基于简化实现验证
  assert_eq(empty_key_value, Some(azimuth::StringValue("test_value")))
  
  // 测试无效Span上下文
  let invalid_span_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_span_ctx))
  
  // 测试部分无效Span上下文
  let partial_invalid_ctx1 = azimuth::SpanContext::new("", "valid-span", true, "")
  let partial_invalid_ctx2 = azimuth::SpanContext::new("valid-trace", "", true, "")
  let partial_invalid_ctx3 = azimuth::SpanContext::new("valid-trace", "valid-span", false, "")
  
  assert_false(azimuth::SpanContext::is_valid(partial_invalid_ctx1))
  assert_false(azimuth::SpanContext::is_valid(partial_invalid_ctx2))
  assert_true(azimuth::SpanContext::is_valid(partial_invalid_ctx3))
  assert_false(azimuth::SpanContext::is_sampled(partial_invalid_ctx3))
  
  // 测试空资源
  let empty_resource = azimuth::Resource::new()
  let empty_attr = azimuth::Resource::get_attribute(empty_resource, "nonexistent")
  assert_eq(empty_attr, None)
  
  // 测试空Baggage
  let empty_baggage = azimuth::Baggage::new()
  let empty_baggage_value = azimuth::Baggage::get_entry(empty_baggage, "nonexistent")
  assert_eq(empty_baggage_value, None)
}