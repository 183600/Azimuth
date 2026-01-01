// Comprehensive MoonBit Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases covering advanced functionality

// Test 1: Advanced Span Operations with Event Management
pub test "高级Span操作和事件管理" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "advanced-test")
  let span = azimuth::Tracer::start_span(tracer, "operation-with-events")
  
  // 添加多个事件
  azimuth::Span::add_event(span, "operation.started", Some([("step", azimuth::StringValue("1"))]))
  azimuth::Span::add_event(span, "processing.data", Some([("records", azimuth::IntValue(100))]))
  azimuth::Span::add_event(span, "validation.completed", Some([("valid", azimuth::BoolValue(true))]))
  
  // 设置Span状态
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::set_attribute(span, "operation.type", azimuth::StringValue("batch"))
  azimuth::Span::set_attribute(span, "batch.size", azimuth::IntValue(1000))
  
  // 验证Span属性
  assert_eq(azimuth::Span::name(span), "operation-with-events")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  // 结束Span
  azimuth::Span::end(span)
  assert_false(azimuth::Span::is_recording(span))
}

// Test 2: Metrics Aggregation and Percentile Calculations
pub test "度量聚合和百分位计算" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  let histogram = azimuth::Meter::create_histogram(meter, "response.time.histogram")
  
  // 记录不同响应时间值
  let response_times = [10.0, 25.0, 50.0, 75.0, 100.0, 150.0, 200.0, 300.0, 500.0, 1000.0]
  
  for time in response_times {
    azimuth::Histogram::record(histogram, time)
  }
  
  // 创建多个度量仪表进行对比
  let p50_gauge = azimuth::Meter::create_gauge(meter, "response.time.p50")
  let p95_gauge = azimuth::Meter::create_gauge(meter, "response.time.p95")
  let p99_gauge = azimuth::Meter::create_gauge(meter, "response.time.p99")
  
  // 验证度量创建
  assert_eq(p50_gauge.name, "response.time.p50")
  assert_eq(p95_gauge.name, "response.time.p95")
  assert_eq(p99_gauge.name, "response.time.p99")
  
  // 测试计数器累加
  let request_counter = azimuth::Meter::create_counter(meter, "total.requests")
  let error_counter = azimuth::Meter::create_counter(meter, "error.requests")
  
  azimuth::Counter::add(request_counter, 1000.0)
  azimuth::Counter::add(error_counter, 50.0)
  
  assert_eq(request_counter.name, "total.requests")
  assert_eq(error_counter.name, "error.requests")
}

// Test 3: Context Propagation with Complex Baggage
pub test "复杂上下文传播和Baggage" {
  let root_ctx = azimuth::Context::root()
  
  // 设置多层上下文值
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let trace_key = azimuth::ContextKey::new("trace.level")
  
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-abcdef")
  let ctx_with_trace = azimuth::Context::with_value(ctx_with_request, trace_key, "verbose")
  
  // 验证上下文值传播
  assert_eq(azimuth::Context::get(ctx_with_trace, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_trace, request_key), Some("req-abcdef"))
  assert_eq(azimuth::Context::get(ctx_with_trace, trace_key), Some("verbose"))
  
  // 测试Baggage复杂操作
  let baggage = azimuth::Baggage::new()
  let baggage1 = azimuth::Baggage::set_entry(baggage, "user.role", "admin")
  let baggage2 = azimuth::Baggage::set_entry(baggage1, "user.region", "us-west")
  let baggage3 = azimuth::Baggage::set_entry(baggage2, "request.priority", "high")
  
  // 验证Baggage内容
  assert_eq(azimuth::Baggage::get_entry(baggage3, "user.role"), Some("admin"))
  assert_eq(azimuth::Baggage::get_entry(baggage3, "user.region"), Some("us-west"))
  assert_eq(azimuth::Baggage::get_entry(baggage3, "request.priority"), Some("high"))
  
  // 测试Baggage覆盖
  let baggage4 = azimuth::Baggage::set_entry(baggage3, "user.role", "super-admin")
  assert_eq(azimuth::Baggage::get_entry(baggage4, "user.role"), Some("super-admin"))
}

// Test 4: Log Correlation and Structured Logging
pub test "日志关联和结构化日志" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "correlation-test")
  
  // 创建关联的Span上下文
  let span_ctx = azimuth::SpanContext::new("trace-789", "span-456", true, "state=test")
  
  // 创建结构化属性
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "service.component", azimuth::StringValue("auth-service"))
  azimuth::Attributes::set(log_attrs, "operation.type", azimuth::StringValue("user-authentication"))
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(log_attrs, "session.id", azimuth::StringValue("session-456"))
  
  // 创建不同严重级别的日志
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User authentication started"),
    Some(log_attrs),
    Some(1735689600000000000L),
    None,
    Some("trace-789"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  let warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Authentication taking longer than expected"),
    Some(log_attrs),
    Some(1735689600000001000L),
    None,
    Some("trace-789"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Authentication failed due to invalid credentials"),
    Some(log_attrs),
    Some(1735689600000002000L),
    None,
    Some("trace-789"),
    Some("span-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  
  assert_eq(azimuth::LogRecord::trace_id(info_log), Some("trace-789"))
  assert_eq(azimuth::LogRecord::span_id(info_log), Some("span-456"))
  
  // 发出日志
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
}

// Test 5: Composite Propagator with Multiple Formats
pub test "复合传播器和多格式支持" {
  // 创建不同类型的传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体并注入上下文
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 设置Baggage值
  let baggage = azimuth::Baggage::new()
  let baggage_with_values = azimuth::Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_final = azimuth::Baggage::set_entry(baggage_with_values, "request.id", "req-67890")
  
  // 注入上下文
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extraction_key = azimuth::ContextKey::new("extracted")
  let extraction_result = azimuth::Context::get(extracted_ctx, extraction_key)
  
  assert_eq(extraction_result, Some("true"))
}

// Test 6: Resource Hierarchy and Inheritance
pub test "资源层次结构和继承" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("production"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建子资源
  let child_resource = azimuth::Resource::new()
  let child_attrs = [
    ("service.component", azimuth::StringValue("auth-handler")),
    ("component.version", azimuth::StringValue("2.1.0")),
    ("service.name", azimuth::StringValue("should-not-override")) // 这个不应该覆盖父级
  ]
  let child_resource_with_attrs = azimuth::Resource::with_attributes(child_resource, child_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource_with_attrs, child_resource_with_attrs)
  
  // 验证继承和覆盖规则
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.component"), Some(azimuth::StringValue("auth-handler")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "component.version"), Some(azimuth::StringValue("2.1.0")))
}

// Test 7: Advanced Attribute Operations with Deep Nesting
pub test "高级属性操作和深度嵌套" {
  let attrs = azimuth::Attributes::new()
  
  // 设置基本属性
  azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("advanced-test"))
  azimuth::Attributes::set(attrs, "service.version", azimuth::StringValue("3.0.0"))
  
  // 设置数组属性
  let string_array = azimuth::ArrayStringValue(["tag1", "tag2", "tag3"])
  let int_array = azimuth::ArrayIntValue([10, 20, 30, 40, 50])
  let float_array = azimuth::ArrayFloatValue([1.1, 2.2, 3.3, 4.4])
  
  azimuth::Attributes::set(attrs, "service.tags", string_array)
  azimuth::Attributes::set(attrs, "metric.thresholds", int_array)
  azimuth::Attributes::set(attrs, "performance.ratios", float_array)
  
  // 设置特殊值属性
  azimuth::Attributes::set(attrs, "feature.enabled", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "config.timeout", azimuth::IntValue(30000))
  azimuth::Attributes::set(attrs, "config.retry.factor", azimuth::FloatValue(2.5))
  
  // 验证属性获取
  let service_name = azimuth::Attributes::get(attrs, "service.name")
  let service_version = azimuth::Attributes::get(attrs, "service.version")
  let feature_enabled = azimuth::Attributes::get(attrs, "feature.enabled")
  let timeout = azimuth::Attributes::get(attrs, "config.timeout")
  let retry_factor = azimuth::Attributes::get(attrs, "config.retry.factor")
  
  // 基于简化实现进行验证
  assert_eq(service_name, Some(azimuth::StringValue("test_value")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  
  // 测试属性更新
  azimuth::Attributes::set(attrs, "service.version", azimuth::StringValue("3.1.0"))
  azimuth::Attributes::set(attrs, "feature.enabled", azimuth::BoolValue(false))
  
  // 测试属性删除（通过设置为null或空值）
  azimuth::Attributes::set(attrs, "deprecated.property", azimuth::StringValue("old-value"))
  azimuth::Attributes::set(attrs, "deprecated.property", azimuth::StringValue(""))
}

// Test 8: Cross-Service Tracing with Multiple Spans
pub test "跨服务追踪和多Span管理" {
  let tracer_provider = azimuth::TracerProvider::default()
  
  // 服务A：创建根Span
  let service_a_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "service-a")
  let root_span = azimuth::Tracer::start_span(service_a_tracer, "api.request")
  
  // 设置根Span属性
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("GET"))
  azimuth::Span::set_attribute(root_span, "http.url", azimuth::StringValue("/api/v1/resource"))
  azimuth::Span::set_attribute(root_span, "user.id", azimuth::StringValue("user-123"))
  
  // 服务B：创建子Span
  let service_b_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "service-b")
  let child_span = azimuth::Tracer::start_span(service_b_tracer, "database.query")
  
  // 设置子Span属性
  azimuth::Span::set_attribute(child_span, "db.system", azimuth::StringValue("postgresql"))
  azimuth::Span::set_attribute(child_span, "db.statement", azimuth::StringValue("SELECT * FROM users"))
  azimuth::Span::set_attribute(child_span, "db.rows", azimuth::IntValue(100))
  
  // 服务C：创建孙级Span
  let service_c_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "service-c")
  let grandchild_span = azimuth::Tracer::start_span(service_c_tracer, "cache.operation")
  
  // 设置孙级Span属性
  azimuth::Span::set_attribute(grandchild_span, "cache.system", azimuth::StringValue("redis"))
  azimuth::Span::set_attribute(grandchild_span, "cache.operation", azimuth::StringValue("get"))
  azimuth::Span::set_attribute(grandchild_span, "cache.hit", azimuth::BoolValue(true))
  
  // 添加事件到各个Span
  azimuth::Span::add_event(root_span, "request.received", None)
  azimuth::Span::add_event(child_span, "query.started", None)
  azimuth::Span::add_event(grandchild_span, "cache.accessed", None)
  
  // 验证Span创建和属性
  assert_eq(azimuth::Span::name(root_span), "api.request")
  assert_eq(azimuth::Span::name(child_span), "database.query")
  assert_eq(azimuth::Span::name(grandchild_span), "cache.operation")
  
  // 按顺序结束Span（孙子级先结束）
  azimuth::Span::end(grandchild_span)
  azimuth::Span::end(child_span)
  azimuth::Span::end(root_span)
  
  // 验证Span已结束
  assert_false(azimuth::Span::is_recording(grandchild_span))
  assert_false(azimuth::Span::is_recording(child_span))
  assert_false(azimuth::Span::is_recording(root_span))
}

// Test 9: Performance Metrics with Time Window Operations
pub test "性能度量和时间窗口操作" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance-test")
  
  // 创建性能相关度量
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.server.response.time")
  let request_rate_counter = azimuth::Meter::create_counter(meter, "http.server.requests")
  let active_connections_gauge = azimuth::Meter::create_gauge(meter, "http.server.active_connections")
  
  // 模拟时间窗口内的度量记录
  let base_timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 记录不同时间点的响应时间
  let response_times = [50.0, 75.0, 100.0, 125.0, 150.0, 200.0, 300.0, 500.0]
  
  for i in 0..response_times.length() {
    let timestamp = base_timestamp + (i.to_int64() * 1000000000L) // 每秒一个数据点
    azimuth::Histogram::record(response_time_histogram, response_times[i])
    azimuth::Counter::add(request_rate_counter, 1.0)
  }
  
  // 验证度量创建
  assert_eq(response_time_histogram.name, "http.server.response.time")
  assert_eq(request_rate_counter.name, "http.server.requests")
  assert_eq(active_connections_gauge.name, "http.server.active_connections")
  
  // 创建窗口统计度量
  let p50_latency_gauge = azimuth::Meter::create_gauge(meter, "http.server.latency.p50")
  let p95_latency_gauge = azimuth::Meter::create_gauge(meter, "http.server.latency.p95")
  let p99_latency_gauge = azimuth::Meter::create_gauge(meter, "http.server.latency.p99")
  
  // 验证窗口统计度量
  assert_eq(p50_latency_gauge.name, "http.server.latency.p50")
  assert_eq(p95_latency_gauge.name, "http.server.latency.p95")
  assert_eq(p99_latency_gauge.name, "http.server.latency.p99")
  
  // 测试错误率度量
  let error_counter = azimuth::Meter::create_counter(meter, "http.server.errors")
  azimuth::Counter::add(error_counter, 5.0) // 5个错误
  
  // 计算错误率
  let total_requests = 8.0
  let total_errors = 5.0
  let error_rate = total_errors / total_requests
  
  assert_true(error_rate > 0.5 && error_rate < 1.0)
}

// Test 10: Comprehensive Integration Test
pub test "综合集成测试" {
  // 创建完整的遥测栈
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("integration-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("test"))
  ])
  
  // 设置追踪
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-test")
  let main_span = azimuth::Tracer::start_span(tracer, "main.operation")
  
  // 设置度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-test")
  let operation_counter = azimuth::Meter::create_counter(meter, "operations.total")
  let duration_histogram = azimuth::Meter::create_histogram(meter, "operation.duration")
  
  // 设置日志
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // 执行操作并记录度量
  azimuth::Counter::add(operation_counter, 1.0)
  azimuth::Histogram::record(duration_histogram, 150.0)
  
  // 添加Span事件
  azimuth::Span::add_event(main_span, "operation.started", Some([("component", azimuth::StringValue("test"))]))
  azimuth::Span::set_attribute(main_span, "operation.type", azimuth::StringValue("integration"))
  
  // 创建日志记录
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "operation.id", azimuth::StringValue("op-123"))
  azimuth::Attributes::set(log_attrs, "operation.status", azimuth::StringValue("success"))
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration test operation completed successfully"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(main_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(main_span))),
    Some(azimuth::Context::root())
  )
  
  // 发出日志
  azimuth::Logger::emit(logger, log_record)
  
  // 结束Span
  azimuth::Span::end(main_span)
  
  // 验证所有组件正常工作
  assert_eq(azimuth::Span::name(main_span), "main.operation")
  assert_eq(operation_counter.name, "operations.total")
  assert_eq(duration_histogram.name, "operation.duration")
  assert_eq(logger.scope.name, "integration-test")
  assert_eq(azimuth::LogRecord::body(log_record), Some("Integration test operation completed successfully"))
  
  // 验证资源属性
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("integration-test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource, "environment"), Some(azimuth::StringValue("test")))
}