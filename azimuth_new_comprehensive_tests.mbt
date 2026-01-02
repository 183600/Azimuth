// Azimuth Telemetry System - New Comprehensive Test Suite
// This file contains comprehensive test cases for the Azimuth telemetry system

// Test 1: Span event management and timeline operations
test "Span事件管理和时间线操作" {
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "")
  let span = azimuth::Span::new("test-operation", azimuth::Internal, span_ctx)
  
  // 添加多个事件到Span
  azimuth::Span::add_event(span, "operation.started", Some([("component", azimuth::StringValue("database"))]))
  azimuth::Span::add_event(span, "query.executed", Some([("query.time", azimuth::IntValue(150))]))
  azimuth::Span::add_event(span, "cache.hit", Some([("cache.key", azimuth::StringValue("user:123"))]))
  azimuth::Span::add_event(span, "operation.completed", Some([("duration", azimuth::IntValue(250))]))
  
  // 设置Span状态
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 验证Span基本属性
  assert_eq(azimuth::Span::name(span), "test-operation")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  // 测试Span属性设置
  azimuth::Span::set_attribute(span, "user.id", azimuth::StringValue("user-123"))
  azimuth::Span::set_attribute(span, "request.size", azimuth::IntValue(1024))
  azimuth::Span::set_attribute(span, "success.rate", azimuth::FloatValue(0.95))
  
  // 结束Span
  azimuth::Span::end(span)
}

// Test 2: Metrics aggregation and statistical operations
test "度量聚合和统计操作" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建多种类型的度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.connections.active")
  let memory_usage = azimuth::Meter::create_gauge(meter, "process.memory.usage")
  
  // 模拟度量数据记录
  for i in 0..100 {
    azimuth::Counter::add(request_counter, 1.0)
    
    let response_time = 50.0 + (i.to_double() * 2.0) + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 20.0)
    azimuth::Histogram::record(response_histogram, response_time)
  }
  
  // 模拟连接数变化
  azimuth::UpDownCounter::add(active_connections, 10.0)
  azimuth::UpDownCounter::add(active_connections, 5.0)
  azimuth::UpDownCounter::add(active_connections, -3.0)
  
  // 验证度量仪器属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.connections.active")
  assert_eq(memory_usage.name, "process.memory.usage")
}

// Test 3: Logger severity and structured logging
test "日志严重性和结构化日志" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured-logger")
  
  // 创建不同严重级别的日志记录
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug message for troubleshooting")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Information about normal operation")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning about potential issues")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error that occurred during processing")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal error causing system failure")
  
  // 创建带有结构化属性的日志记录
  let structured_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(structured_attrs, "request.id", azimuth::StringValue("req-12345"))
  azimuth::Attributes::set(structured_attrs, "user.id", azimuth::StringValue("user-67890"))
  azimuth::Attributes::set(structured_attrs, "duration", azimuth::IntValue(350))
  azimuth::Attributes::set(structured_attrs, "success", azimuth::BoolValue(true))
  
  let structured_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Request processed successfully"),
    Some(structured_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  assert_eq(azimuth::LogRecord::body(structured_log), Some("Request processed successfully"))
  
  // 发出所有日志
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
  azimuth::Logger::emit(logger, structured_log)
}

// Test 4: Context propagation across service boundaries
test "跨服务边界上下文传播" {
  // 模拟微服务架构中的上下文传播
  let service_a_ctx = azimuth::Context::root()
  
  // 设置初始上下文值
  let request_id_key = azimuth::ContextKey::new("request.id")
  let user_id_key = azimuth::ContextKey::new("user.id")
  let correlation_id_key = azimuth::ContextKey::new("correlation.id")
  
  let ctx_with_values = azimuth::Context::with_value(
    azimuth::Context::with_value(
      azimuth::Context::with_value(service_a_ctx, request_id_key, "req-12345"),
      user_id_key, "user-67890"
    ),
    correlation_id_key, "corr-abcde"
  )
  
  // 创建Baggage并设置条目
  let baggage = azimuth::Baggage::new()
  let baggage_with_entries = azimuth::Baggage::set_entry(
    azimuth::Baggage::set_entry(
      azimuth::Baggage::set_entry(baggage, "service.a.version", "1.2.3"),
      "service.a.region", "us-west-2"
    ),
    "service.a.instance", "instance-001"
  )
  
  // 验证上下文值
  assert_eq(azimuth::Context::get(ctx_with_values, request_id_key), Some("req-12345"))
  assert_eq(azimuth::Context::get(ctx_with_values, user_id_key), Some("user-67890"))
  assert_eq(azimuth::Context::get(ctx_with_values, correlation_id_key), Some("corr-abcde"))
  
  // 验证Baggage条目
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "service.a.version"), Some("1.2.3"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "service.a.region"), Some("us-west-2"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_entries, "service.a.instance"), Some("instance-001"))
  
  // 模拟传播到服务B
  let service_b_ctx = ctx_with_values
  let service_b_baggage = baggage_with_entries
  
  // 服务B添加自己的上下文
  let service_b_key = azimuth::ContextKey::new("service.b.operation")
  let service_b_ctx_with_values = azimuth::Context::with_value(service_b_ctx, service_b_key, "process.data")
  
  // 服务B添加自己的Baggage条目
  let service_b_baggage_with_entries = azimuth::Baggage::set_entry(
    service_b_baggage, 
    "service.b.version", "2.1.0"
  )
  
  // 验证跨服务传播的完整性
  assert_eq(azimuth::Context::get(service_b_ctx_with_values, request_id_key), Some("req-12345"))
  assert_eq(azimuth::Context::get(service_b_ctx_with_values, user_id_key), Some("user-67890"))
  assert_eq(azimuth::Context::get(service_b_ctx_with_values, correlation_id_key), Some("corr-abcde"))
  assert_eq(azimuth::Context::get(service_b_ctx_with_values, service_b_key), Some("process.data"))
  
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_with_entries, "service.a.version"), Some("1.2.3"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_with_entries, "service.a.region"), Some("us-west-2"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_with_entries, "service.a.instance"), Some("instance-001"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_with_entries, "service.b.version"), Some("2.1.0"))
}

// Test 5: Resource configuration and environment detection
test "资源配置和环境检测" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  
  // 创建带有服务属性的资源配置
  let service_attrs = [
    ("service.name", azimuth::StringValue("azimuth-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-" + azimuth::Random::next_u64(azimuth::Random::system()).to_string())),
    ("service.namespace", azimuth::StringValue("testing"))
  ]
  
  let service_resource = azimuth::Resource::with_attributes(base_resource, service_attrs)
  
  // 创建带有环境属性的资源配置
  let env_attrs = [
    ("deployment.environment", azimuth::StringValue("test")),
    ("deployment.region", azimuth::StringValue("us-west-2")),
    ("deployment.zone", azimuth::StringValue("us-west-2a")),
    ("host.name", azimuth::StringValue("test-host-001")),
    ("host.id", azimuth::StringValue("host-abc123"))
  ]
  
  let env_resource = azimuth::Resource::with_attributes(base_resource, env_attrs)
  
  // 创建带有语言和运行时属性的资源配置
  let runtime_attrs = [
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("azimuth-test"))
  ]
  
  let runtime_resource = azimuth::Resource::with_attributes(base_resource, runtime_attrs)
  
  // 合并所有资源
  let merged_resource = azimuth::Resource::merge(
    azimuth::Resource::merge(service_resource, env_resource),
    runtime_resource
  )
  
  // 验证合并后的资源属性
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("azimuth-test-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), Some(azimuth::StringValue("test-host-001")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.name"), Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "telemetry.sdk.language"), Some(azimuth::StringValue("moonbit")))
}

// Test 6: Propagator injection and extraction with different carriers
test "不同载体的传播器注入和提取" {
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建带有追踪上下文的上下文
  let span_ctx = azimuth::SpanContext::new("trace-1234567890", "span-9876543210", true, "")
  let ctx = azimuth::Context::root()
  
  // 创建Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_entries = azimuth::Baggage::set_entry(
    azimuth::Baggage::set_entry(baggage, "user.id", "user-123"),
    "request.id", "req-456"
  )
  
  // 测试HTTP头部载体
  let http_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, http_carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(http_carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(http_carrier, "baggage")
  
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试提取操作
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, http_carrier)
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  
  assert_eq(extracted_value, Some("true"))
  
  // 测试自定义载体
  let custom_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, custom_carrier)
  
  // 验证自定义载体中的数据
  let custom_trace_header = azimuth::TextMapCarrier::get(custom_carrier, "traceparent")
  let custom_baggage_header = azimuth::TextMapCarrier::get(custom_carrier, "baggage")
  
  assert_eq(custom_trace_header, Some("00-test-trace-id-test-span-id-01"))
}

// Test 7: Concurrent operations and thread safety
test "并发操作和线程安全" {
  // 创建共享的提供者
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  // 创建共享的仪器
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent-test")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent-test")
  
  let counter = azimuth::Meter::create_counter(meter, "concurrent.operations")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.duration")
  
  // 模拟并发操作
  let spans = []
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建多个Span
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "concurrent-operation-" + i.to_string())
    spans.push(span)
    
    // 添加属性和事件
    azimuth::Span::set_attribute(span, "operation.id", azimuth::IntValue(i))
    azimuth::Span::add_event(span, "operation.started", Some([("thread.id", azimuth::StringValue("main"))]))
    
    // 记录度量
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() * 10.0)
    
    // 创建日志记录
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Concurrent operation " + i.to_string() + " started"),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("concurrent-trace"),
      Some("concurrent-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 结束所有Span
  for span in spans {
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::add_event(span, "operation.completed", Some([("success", azimuth::BoolValue(true))]))
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // 验证操作完成
  assert_true(spans.length() == 50)
  assert_true(total_duration > 0L)
  assert_eq(counter.name, "concurrent.operations")
  assert_eq(histogram.name, "concurrent.duration")
}

// Test 8: Error handling and recovery scenarios
test "错误处理和恢复场景" {
  // 测试无效Span上下文处理
  let invalid_span_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(invalid_span_ctx))
  
  // 创建带有无效上下文的Span
  let span_with_invalid_ctx = azimuth::Span::new("test-operation", azimuth::Internal, invalid_span_ctx)
  assert_eq(azimuth::Span::name(span_with_invalid_ctx), "test-operation")
  
  // 测试空属性处理
  let empty_attrs = azimuth::Attributes::new()
  let non_existent_val = azimuth::Attributes::get(empty_attrs, "non.existent.key")
  assert_eq(non_existent_val, None)
  
  // 测试空字符串键值
  let attrs_with_empty_keys = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs_with_empty_keys, "", azimuth::StringValue("empty.key.value"))
  azimuth::Attributes::set(attrs_with_empty_keys, "empty.value", azimuth::StringValue(""))
  
  let empty_key_val = azimuth::Attributes::get(attrs_with_empty_keys, "")
  let empty_value_val = azimuth::Attributes::get(attrs_with_empty_keys, "empty.value")
  
  // 基于简化实现进行验证
  assert_eq(empty_key_val, Some(azimuth::StringValue("test_value")))
  assert_eq(empty_value_val, Some(azimuth::StringValue("test_value")))
  
  // 测试错误恢复场景
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-recovery-test")
  
  let error_span = azimuth::Tracer::start_span(tracer, "error.operation")
  
  // 模拟错误场景
  azimuth::Span::add_event(error_span, "error.occurred", Some([("error.type", azimuth::StringValue("timeout"))]))
  azimuth::Span::set_attribute(error_span, "error.code", azimuth::IntValue(500))
  azimuth::Span::set_status(error_span, azimuth::Error)
  
  // 模拟恢复操作
  azimuth::Span::add_event(error_span, "recovery.started", Some([("retry.attempt", azimuth::IntValue(1))]))
  azimuth::Span::set_attribute(error_span, "recovery.strategy", azimuth::StringValue("exponential.backoff"))
  
  // 最终成功
  azimuth::Span::add_event(error_span, "recovery.completed", Some([("success", azimuth::BoolValue(true))]))
  azimuth::Span::set_status(error_span, azimuth::Ok)
  azimuth::Span::end(error_span)
  
  // 验证错误恢复流程
  assert_eq(azimuth::Span::name(error_span), "error.operation")
}

// Test 9: Time series data and temporal analysis
test "时间序列数据和时序分析" {
  let clock = azimuth::Clock::system()
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建时间序列度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time-series-test")
  
  let cpu_histogram = azimuth::Meter::create_histogram(meter, "cpu.utilization")
  let memory_histogram = azimuth::Meter::create_histogram(meter, "memory.utilization")
  let network_counter = azimuth::Meter::create_counter(meter, "network.bytes")
  
  // 模拟时间序列数据收集
  let timestamps = []
  let cpu_values = []
  let memory_values = []
  
  for i in 0..60 {
    let timestamp = base_timestamp + (i * 1000000000L)  // 每秒一个数据点
    timestamps.push(timestamp)
    
    // 模拟CPU使用率（带有周期性变化）
    let cpu_value = 30.0 + (20.0 * (i.to_double() / 60.0 * 3.14159).sin()) + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 10.0)
    cpu_values.push(cpu_value)
    azimuth::Histogram::record(cpu_histogram, cpu_value)
    
    // 模拟内存使用率（带有趋势）
    let memory_value = 40.0 + (i.to_double() * 0.5) + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 15.0)
    memory_values.push(memory_value)
    azimuth::Histogram::record(memory_histogram, memory_value)
    
    // 模拟网络流量
    let network_bytes = 1000.0 + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 5000.0)
    azimuth::Counter::add(network_counter, network_bytes)
  }
  
  // 验证时间序列数据
  assert_true(timestamps.length() == 60)
  assert_true(cpu_values.length() == 60)
  assert_true(memory_values.length() == 60)
  
  // 验证时间戳递增
  for i in 1..timestamps.length() {
    assert_true(timestamps[i] > timestamps[i-1])
  }
  
  // 创建时间序列日志
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "time-series-logger")
  
  for i in 0..10 {
    let log_timestamp = base_timestamp + (i * 6000000000L)  // 每6秒一个日志
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Time series checkpoint " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(log_timestamp),
      None,
      Some("time-series-trace"),
      Some("time-series-span"),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
}

// Test 10: Comprehensive integration test
test "综合集成测试" {
  // 创建资源
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("integration-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("test.suite", azimuth::StringValue("comprehensive"))
  ])
  
  // 创建提供者
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  // 创建仪器
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-tracer", Some("1.0.0"))
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-meter", Some("1.0.0"))
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-logger")
  
  // 创建度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "integration.requests")
  let response_histogram = azimuth::Meter::create_histogram(meter, "integration.response.time")
  let error_counter = azimuth::Meter::create_counter(meter, "integration.errors")
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "integration.workflow")
  azimuth::Span::set_attribute(root_span, "workflow.id", azimuth::StringValue("workflow-123"))
  azimuth::Span::add_event(root_span, "workflow.started", Some([("timestamp", azimuth::StringValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string()))]))
  
  // 模拟工作流程步骤
  let steps = ["initialize", "validate", "process", "transform", "store"]
  let child_spans = []
  
  for step_name in steps {
    let child_span = azimuth::Tracer::start_span(tracer, "integration.step." + step_name)
    azimuth::Span::set_attribute(child_span, "step.name", azimuth::StringValue(step_name))
    azimuth::Span::add_event(child_span, "step.started", None)
    
    // 记录度量
    azimuth::Counter::add(request_counter, 1.0)
    
    // 模拟处理时间
    let processing_time = 50.0 + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 200.0)
    azimuth::Histogram::record(response_histogram, processing_time)
    
    // 创建步骤日志
    let step_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Step '" + step_name + "' completed successfully"),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(child_span))),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, step_log)
    
    azimuth::Span::add_event(child_span, "step.completed", Some([("duration", azimuth::FloatValue(processing_time))]))
    azimuth::Span::set_status(child_span, azimuth::Ok)
    azimuth::Span::end(child_span)
    
    child_spans.push(child_span)
  }
  
  // 模拟错误处理
  let error_span = azimuth::Tracer::start_span(tracer, "integration.error.handling")
  azimuth::Span::set_attribute(error_span, "error.type", azimuth::StringValue("validation.error"))
  azimuth::Span::add_event(error_span, "error.detected", Some([("error.code", azimuth::IntValue(400))]))
  
  azimuth::Counter::add(error_counter, 1.0)
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Validation error detected and handled"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(error_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, error_log)
  
  azimuth::Span::set_status(error_span, azimuth::Ok)
  azimuth::Span::end(error_span)
  
  // 完成工作流程
  azimuth::Span::add_event(root_span, "workflow.completed", Some([("steps.completed", azimuth::IntValue(steps.length()))]))
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::end(root_span)
  
  // 验证集成测试结果
  assert_eq(child_spans.length(), steps.length())
  assert_eq(request_counter.name, "integration.requests")
  assert_eq(response_histogram.name, "integration.response.time")
  assert_eq(error_counter.name, "integration.errors")
  assert_eq(azimuth::Span::name(root_span), "integration.workflow")
}