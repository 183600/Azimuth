// 跨服务一致性测试
// 测试Azimuth遥测系统在跨多个服务场景下的数据一致性和传播准确性

// 测试1: 基本分布式追踪一致性
pub test "基本分布式追踪一致性测试" {
  // 模拟三个服务的分布式追踪场景
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  
  // 服务A创建根Span
  let root_span_ctx = azimuth::SpanContext::new("trace-1234567890abcdef", "span-a-root", true, "test-state=key1=value1")
  let root_span = azimuth::Span::new("service-a-operation", azimuth::Server, root_span_ctx)
  
  // 服务A调用服务B
  let span_b_ctx = azimuth::SpanContext::new("trace-1234567890abcdef", "span-b-child", true, "test-state=key1=value1")
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Server, span_b_ctx)
  
  // 服务B调用服务C
  let span_c_ctx = azimuth::SpanContext::new("trace-1234567890abcdef", "span-c-grandchild", true, "test-state=key1=value1")
  let span_c = azimuth::Span::new("service-c-operation", azimuth::Internal, span_c_ctx)
  
  // 验证Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(root_span_ctx), "trace-1234567890abcdef")
  assert_eq(azimuth::SpanContext::trace_id(span_b_ctx), "trace-1234567890abcdef")
  assert_eq(azimuth::SpanContext::trace_id(span_c_ctx), "trace-1234567890abcdef")
  
  // 验证Span ID唯一性
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_b_ctx))
  assert_true(azimuth::SpanContext::span_id(span_b_ctx) != azimuth::SpanContext::span_id(span_c_ctx))
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(span_c_ctx))
  
  // 验证采样状态一致性
  assert_true(azimuth::SpanContext::is_sampled(root_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_b_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_c_ctx))
  
  // 验证Trace状态一致性
  assert_eq(azimuth::SpanContext::trace_state(root_span_ctx), "test-state=key1=value1")
  assert_eq(azimuth::SpanContext::trace_state(span_b_ctx), "test-state=key1=value1")
  assert_eq(azimuth::SpanContext::trace_state(span_c_ctx), "test-state=key1=value1")
  
  // 验证Span种类
  assert_eq(azimuth::Span::kind(root_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_b), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_c), azimuth::Internal)
}

// 测试2: 跨服务Baggage传播一致性
pub test "跨服务Baggage传播一致性测试" {
  // 模拟跨服务的Baggage传播
  let baggage_a = azimuth::Baggage::new()
  let baggage_b = azimuth::Baggage::set_entry(baggage_a, "user.id", "12345")
  let baggage_c = azimuth::Baggage::set_entry(baggage_b, "request.id", "req-67890")
  let baggage_d = azimuth::Baggage::set_entry(baggage_c, "session.id", "session-abc123")
  
  // 验证Baggage传播一致性
  assert_eq(azimuth::Baggage::get_entry(baggage_a, "user.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "user.id"), Some("12345"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_b, "request.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "request.id"), Some("req-67890"))
  
  assert_eq(azimuth::Baggage::get_entry(baggage_c, "session.id"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_d, "session.id"), Some("session-abc123"))
  
  // 测试Baggage覆盖场景
  let baggage_e = azimuth::Baggage::set_entry(baggage_d, "user.id", "54321")  // 覆盖现有值
  assert_eq(azimuth::Baggage::get_entry(baggage_e, "user.id"), Some("54321"))
  assert_eq(azimuth::Baggage::get_entry(baggage_e, "request.id"), Some("req-67890"))  // 其他值保持不变
  assert_eq(azimuth::Baggage::get_entry(baggage_e, "session.id"), Some("session-abc123"))
  
  // 测试特殊字符和Unicode Baggage
  let baggage_f = azimuth::Baggage::set_entry(baggage_e, "special.key", "!@#$%^&*()")
  let baggage_g = azimuth::Baggage::set_entry(baggage_f, "unicode.key", "测试中文🚀")
  
  assert_eq(azimuth::Baggage::get_entry(baggage_g, "special.key"), Some("!@#$%^&*()"))
  assert_eq(azimuth::Baggage::get_entry(baggage_g, "unicode.key"), Some("测试中文🚀"))
  assert_eq(azimuth::Baggage::get_entry(baggage_g, "user.id"), Some("54321"))  // 确保其他值保持不变
}

// 测试3: 跨服务上下文传播一致性
pub test "跨服务上下文传播一致性测试" {
  // 模拟跨服务的上下文传播
  let root_ctx = azimuth::Context::root()
  
  // 服务A设置上下文值
  let key_a = azimuth::ContextKey::new("service.a.value")
  let ctx_a = azimuth::Context::with_value(root_ctx, key_a, "service-a-data")
  
  // 服务B添加上下文值
  let key_b = azimuth::ContextKey::new("service.b.value")
  let ctx_b = azimuth::Context::with_value(ctx_a, key_b, "service-b-data")
  
  // 服务C添加上下文值
  let key_c = azimuth::ContextKey::new("service.c.value")
  let ctx_c = azimuth::Context::with_value(ctx_b, key_c, "service-c-data")
  
  // 验证上下文传播一致性
  assert_eq(azimuth::Context::get(root_ctx, key_a), None)
  assert_eq(azimuth::Context::get(root_ctx, key_b), None)
  assert_eq(azimuth::Context::get(root_ctx, key_c), None)
  
  assert_eq(azimuth::Context::get(ctx_a, key_a), Some("service-a-data"))
  assert_eq(azimuth::Context::get(ctx_a, key_b), None)
  assert_eq(azimuth::Context::get(ctx_a, key_c), None)
  
  assert_eq(azimuth::Context::get(ctx_b, key_a), Some("service-a-data"))
  assert_eq(azimuth::Context::get(ctx_b, key_b), Some("service-b-data"))
  assert_eq(azimuth::Context::get(ctx_b, key_c), None)
  
  assert_eq(azimuth::Context::get(ctx_c, key_a), Some("service-a-data"))
  assert_eq(azimuth::Context::get(ctx_c, key_b), Some("service-b-data"))
  assert_eq(azimuth::Context::get(ctx_c, key_c), Some("service-c-data"))
  
  // 测试上下文值覆盖
  let ctx_d = azimuth::Context::with_value(ctx_c, key_a, "updated-service-a-data")
  assert_eq(azimuth::Context::get(ctx_d, key_a), Some("updated-service-a-data"))
  assert_eq(azimuth::Context::get(ctx_d, key_b), Some("service-b-data"))
  assert_eq(azimuth::Context::get(ctx_d, key_c), Some("service-c-data"))
  
  // 测试特殊字符和Unicode上下文键
  let special_key = azimuth::ContextKey::new("special.key.with-dashes_and_underscores")
  let unicode_key = azimuth::ContextKey::new("键名.中文")
  
  let ctx_e = azimuth::Context::with_value(ctx_d, special_key, "special-value")
  let ctx_f = azimuth::Context::with_value(ctx_e, unicode_key, "中文值")
  
  assert_eq(azimuth::Context::get(ctx_f, special_key), Some("special-value"))
  assert_eq(azimuth::Context::get(ctx_f, unicode_key), Some("中文值"))
  assert_eq(azimuth::Context::get(ctx_f, key_a), Some("updated-service-a-data"))
}

// 测试4: 跨服务度量一致性
pub test "跨服务度量一致性测试" {
  // 模拟跨服务的度量收集
  let meter_provider = azimuth::MeterProvider::default()
  
  // 各服务创建自己的Meter
  let meter_a = azimuth::MeterProvider::get_meter(meter_provider, "service-a-meter", Some("1.0.0"))
  let meter_b = azimuth::MeterProvider::get_meter(meter_provider, "service-b-meter", Some("1.0.0"))
  let meter_c = azimuth::MeterProvider::get_meter(meter_provider, "service-c-meter", Some("1.0.0"))
  
  // 创建相同名称的度量仪器
  let counter_a = azimuth::Meter::create_counter(meter_a, "request.count", Some("Total request count"), Some("requests"))
  let counter_b = azimuth::Meter::create_counter(meter_b, "request.count", Some("Total request count"), Some("requests"))
  let counter_c = azimuth::Meter::create_counter(meter_c, "request.count", Some("Total request count"), Some("requests"))
  
  // 验证度量仪器属性一致性
  assert_eq(counter_a.name, "request.count")
  assert_eq(counter_b.name, "request.count")
  assert_eq(counter_c.name, "request.count")
  
  assert_eq(counter_a.description, Some("Total request count"))
  assert_eq(counter_b.description, Some("Total request count"))
  assert_eq(counter_c.description, Some("Total request count"))
  
  assert_eq(counter_a.unit, Some("requests"))
  assert_eq(counter_b.unit, Some("requests"))
  assert_eq(counter_c.unit, Some("requests"))
  
  // 创建Histogram度量
  let histogram_a = azimuth::Meter::create_histogram(meter_a, "response.time", Some("Response time"), Some("ms"))
  let histogram_b = azimuth::Meter::create_histogram(meter_b, "response.time", Some("Response time"), Some("ms"))
  let histogram_c = azimuth::Meter::create_histogram(meter_c, "response.time", Some("Response time"), Some("ms"))
  
  // 验证Histogram属性一致性
  assert_eq(histogram_a.name, "response.time")
  assert_eq(histogram_b.name, "response.time")
  assert_eq(histogram_c.name, "response.time")
  
  // 执行度量操作
  azimuth::Counter::add(counter_a, 100.0)  // 服务A处理100个请求
  azimuth::Counter::add(counter_b, 50.0)   // 服务B处理50个请求
  azimuth::Counter::add(counter_c, 25.0)   // 服务C处理25个请求
  
  azimuth::Histogram::record(histogram_a, 150.5)  // 服务A平均响应时间
  azimuth::Histogram::record(histogram_b, 200.0)  // 服务B平均响应时间
  azimuth::Histogram::record(histogram_c, 100.0)  // 服务C平均响应时间
  
  // 验证仪器范围一致性
  let instrument_a = azimuth::Histogram::as_instrument(histogram_a)
  let instrument_b = azimuth::Histogram::as_instrument(histogram_b)
  let instrument_c = azimuth::Histogram::as_instrument(histogram_c)
  
  assert_eq(azimuth::Instrument::name(instrument_a), "response.time")
  assert_eq(azimuth::Instrument::name(instrument_b), "response.time")
  assert_eq(azimuth::Instrument::name(instrument_c), "response.time")
}

// 测试5: 跨服务日志一致性
pub test "跨服务日志一致性测试" {
  // 模拟跨服务的日志记录
  let logger_provider = azimuth::LoggerProvider::default()
  
  // 各服务创建自己的Logger
  let logger_a = azimuth::LoggerProvider::get_logger(logger_provider, "service-a-logger")
  let logger_b = azimuth::LoggerProvider::get_logger(logger_provider, "service-b-logger")
  let logger_c = azimuth::LoggerProvider::get_logger(logger_provider, "service-c-logger")
  
  // 验证Logger范围一致性
  assert_eq(logger_a.scope.name, "service-a-logger")
  assert_eq(logger_b.scope.name, "service-b-logger")
  assert_eq(logger_c.scope.name, "service-c-logger")
  
  // 创建具有相同Trace ID的日志记录
  let trace_id = "trace-9876543210fedcba"
  let span_a_id = "span-a-12345"
  let span_b_id = "span-b-67890"
  let span_c_id = "span-c-11111"
  
  let log_a = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A processing request"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(trace_id),
    Some(span_a_id),
    Some(azimuth::Context::root())
  )
  
  let log_b = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B processing request"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(trace_id),
    Some(span_b_id),
    Some(azimuth::Context::root())
  )
  
  let log_c = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service C processing request"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some(trace_id),
    Some(span_c_id),
    Some(azimuth::Context::root())
  )
  
  // 验证Trace ID一致性
  assert_eq(azimuth::LogRecord::trace_id(log_a), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_b), Some(trace_id))
  assert_eq(azimuth::LogRecord::trace_id(log_c), Some(trace_id))
  
  // 验证Span ID唯一性
  assert_eq(azimuth::LogRecord::span_id(log_a), Some(span_a_id))
  assert_eq(azimuth::LogRecord::span_id(log_b), Some(span_b_id))
  assert_eq(azimuth::LogRecord::span_id(log_c), Some(span_c_id))
  
  assert_true(span_a_id != span_b_id)
  assert_true(span_b_id != span_c_id)
  assert_true(span_a_id != span_c_id)
  
  // 验证时间戳顺序
  assert_true(azimuth::LogRecord::timestamp(log_a) < azimuth::LogRecord::timestamp(log_b))
  assert_true(azimuth::LogRecord::timestamp(log_b) < azimuth::LogRecord::timestamp(log_c))
  
  // 发送日志记录
  azimuth::Logger::emit(logger_a, log_a)
  azimuth::Logger::emit(logger_b, log_b)
  azimuth::Logger::emit(logger_c, log_c)
}

// 测试6: 跨服务资源一致性
pub test "跨服务资源一致性测试" {
  // 模拟跨服务的资源配置
  let base_attrs = [
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("prod")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0"))
  ]
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  
  // 各服务添加自己的特定属性
  let service_a_attrs = [
    ("service.name", azimuth::StringValue("service-a")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.instance.id", azimuth::StringValue("service-a-instance-1"))
  ]
  
  let service_b_attrs = [
    ("service.name", azimuth::StringValue("service-b")),
    ("service.version", azimuth::StringValue("2.3.4")),
    ("service.instance.id", azimuth::StringValue("service-b-instance-1"))
  ]
  
  let service_c_attrs = [
    ("service.name", azimuth::StringValue("service-c")),
    ("service.version", azimuth::StringValue("3.4.5")),
    ("service.instance.id", azimuth::StringValue("service-c-instance-1"))
  ]
  
  let resource_a = azimuth::Resource::with_attributes(base_resource, service_a_attrs)
  let resource_b = azimuth::Resource::with_attributes(base_resource, service_b_attrs)
  let resource_c = azimuth::Resource::with_attributes(base_resource, service_c_attrs)
  
  // 验证基础资源属性一致性
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.namespace"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "deployment.environment"), Some(azimuth::StringValue("prod")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "deployment.environment"), Some(azimuth::StringValue("prod")))
  
  // 验证服务特定属性
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.name"), Some(azimuth::StringValue("service-c")))
  
  assert_eq(azimuth::Resource::get_attribute(resource_a, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(resource_b, "service.version"), Some(azimuth::StringValue("2.3.4")))
  assert_eq(azimuth::Resource::get_attribute(resource_c, "service.version"), Some(azimuth::StringValue("3.4.5")))
  
  // 测试资源合并一致性
  let common_attrs = [
    ("region", azimuth::StringValue("us-west-1")),
    ("zone", azimuth::StringValue("us-west-1a"))
  ]
  
  let common_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), common_attrs)
  
  let merged_a = azimuth::Resource::merge(resource_a, common_resource)
  let merged_b = azimuth::Resource::merge(resource_b, common_resource)
  let merged_c = azimuth::Resource::merge(resource_c, common_resource)
  
  // 验证合并后的共同属性
  assert_eq(azimuth::Resource::get_attribute(merged_a, "region"), Some(azimuth::StringValue("us-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_b, "region"), Some(azimuth::StringValue("us-west-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_c, "region"), Some(azimuth::StringValue("us-west-1")))
  
  // 验证合并后服务特定属性保持不变
  assert_eq(azimuth::Resource::get_attribute(merged_a, "service.name"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::Resource::get_attribute(merged_b, "service.name"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::Resource::get_attribute(merged_c, "service.name"), Some(azimuth::StringValue("service-c")))
}

// 测试7: 跨服务传播器一致性
pub test "跨服务传播器一致性测试" {
  // 模拟跨服务的传播器使用
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 服务A注入上下文
  let carrier_a = azimuth::TextMapCarrier::new()
  let ctx_a = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(composite_propagator, ctx_a, carrier_a)
  
  // 服务B提取上下文
  let extracted_ctx_b = azimuth::CompositePropagator::extract(composite_propagator, carrier_a)
  
  // 服务B注入新的上下文
  let carrier_b = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, extracted_ctx_b, carrier_b)
  
  // 服务C提取上下文
  let extracted_ctx_c = azimuth::CompositePropagator::extract(composite_propagator, carrier_b)
  
  // 验证提取键的一致性
  let key = azimuth::ContextKey::new("extracted")
  let value_a = azimuth::Context::get(ctx_a, key)
  let value_b = azimuth::Context::get(extracted_ctx_b, key)
  let value_c = azimuth::Context::get(extracted_ctx_c, key)
  
  // 基于简化实现进行验证
  assert_eq(value_a, None)
  assert_eq(value_b, Some("true"))
  assert_eq(value_c, Some("true"))
  
  // 测试传播器头的一致性
  let trace_header_a = azimuth::TextMapCarrier::get(carrier_a, "traceparent")
  let trace_header_b = azimuth::TextMapCarrier::get(carrier_b, "traceparent")
  
  // 在实际实现中，这些应该是相同的traceparent值
  // 基于简化实现进行验证
  assert_eq(trace_header_a, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(trace_header_b, Some("00-test-trace-id-test-span-id-01"))
}

// 测试8: 复杂跨服务场景一致性
pub test "复杂跨服务场景一致性测试" {
  // 模拟复杂的微服务调用链：API Gateway -> Service A -> Service B -> Service C -> Service D
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway")
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  let service_d_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-d")
  
  // 共享的Trace ID
  let shared_trace_id = "complex-trace-1234567890abcdef"
  
  // API Gateway创建根Span
  let gateway_span_ctx = azimuth::SpanContext::new(shared_trace_id, "span-gateway", true, "env=production")
  let gateway_span = azimuth::Span::new("gateway-request", azimuth::Server, gateway_span_ctx)
  
  // Service A创建子Span
  let span_a_ctx = azimuth::SpanContext::new(shared_trace_id, "span-a-child", true, "env=production")
  let span_a = azimuth::Span::new("service-a-operation", azimuth::Server, span_a_ctx)
  
  // Service B创建子Span
  let span_b_ctx = azimuth::SpanContext::new(shared_trace_id, "span-b-grandchild", true, "env=production")
  let span_b = azimuth::Span::new("service-b-operation", azimuth::Server, span_b_ctx)
  
  // Service C创建子Span
  let span_c_ctx = azimuth::SpanContext::new(shared_trace_id, "span-c-greatgrandchild", true, "env=production")
  let span_c = azimuth::Span::new("service-c-operation", azimuth::Internal, span_c_ctx)
  
  // Service D创建子Span
  let span_d_ctx = azimuth::SpanContext::new(shared_trace_id, "span-d-greatgreatgrandchild", true, "env=production")
  let span_d = azimuth::Span::new("service-d-operation", azimuth::Client, span_d_ctx)
  
  // 验证所有Span的Trace ID一致性
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), shared_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_a_ctx), shared_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_b_ctx), shared_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_c_ctx), shared_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(span_d_ctx), shared_trace_id)
  
  // 验证所有Span ID的唯一性
  let span_ids = [
    azimuth::SpanContext::span_id(gateway_span_ctx),
    azimuth::SpanContext::span_id(span_a_ctx),
    azimuth::SpanContext::span_id(span_b_ctx),
    azimuth::SpanContext::span_id(span_c_ctx),
    azimuth::SpanContext::span_id(span_d_ctx)
  ]
  
  // 检查所有Span ID都不相同
  for i in 0..span_ids.length() {
    for j in 0..span_ids.length() {
      if i != j {
        assert_true(span_ids[i] != span_ids[j])
      }
    }
  }
  
  // 验证所有Span的采样状态一致性
  assert_true(azimuth::SpanContext::is_sampled(gateway_span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_a_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_b_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_c_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_d_ctx))
  
  // 验证所有Span的Trace状态一致性
  assert_eq(azimuth::SpanContext::trace_state(gateway_span_ctx), "env=production")
  assert_eq(azimuth::SpanContext::trace_state(span_a_ctx), "env=production")
  assert_eq(azimuth::SpanContext::trace_state(span_b_ctx), "env=production")
  assert_eq(azimuth::SpanContext::trace_state(span_c_ctx), "env=production")
  assert_eq(azimuth::SpanContext::trace_state(span_d_ctx), "env=production")
  
  // 验证Span种类
  assert_eq(azimuth::Span::kind(gateway_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_a), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_b), azimuth::Server)
  assert_eq(azimuth::Span::kind(span_c), azimuth::Internal)
  assert_eq(azimuth::Span::kind(span_d), azimuth::Client)
  
  // 测试跨服务的Baggage传播
  let initial_baggage = azimuth::Baggage::new()
  let gateway_baggage = azimuth::Baggage::set_entry(initial_baggage, "user.id", "user-123")
  let service_a_baggage = azimuth::Baggage::set_entry(gateway_baggage, "request.id", "req-456")
  let service_b_baggage = azimuth::Baggage::set_entry(service_a_baggage, "session.id", "sess-789")
  let service_c_baggage = azimuth::Baggage::set_entry(service_b_baggage, "operation.id", "op-101112")
  let service_d_baggage = azimuth::Baggage::set_entry(service_c_baggage, "correlation.id", "corr-131415")
  
  // 验证Baggage传播一致性
  assert_eq(azimuth::Baggage::get_entry(initial_baggage, "user.id"), None)
  assert_eq(azimuth::Baggage::get_entry(gateway_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(service_a_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "user.id"), Some("user-123"))
  
  assert_eq(azimuth::Baggage::get_entry(service_d_baggage, "correlation.id"), Some("corr-131415"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage, "correlation.id"), None)
  
  // 测试跨服务的度量一致性
  let meter_provider = azimuth::MeterProvider::default()
  
  let gateway_meter = azimuth::MeterProvider::get_meter(meter_provider, "api-gateway")
  let service_a_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-a")
  let service_b_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-b")
  let service_c_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-c")
  let service_d_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-d")
  
  // 创建相同名称的度量仪器
  let gateway_counter = azimuth::Meter::create_counter(gateway_meter, "request.duration")
  let service_a_counter = azimuth::Meter::create_counter(service_a_meter, "request.duration")
  let service_b_counter = azimuth::Meter::create_counter(service_b_meter, "request.duration")
  let service_c_counter = azimuth::Meter::create_counter(service_c_meter, "request.duration")
  let service_d_counter = azimuth::Meter::create_counter(service_d_meter, "request.duration")
  
  // 验证度量仪器名称一致性
  assert_eq(gateway_counter.name, "request.duration")
  assert_eq(service_a_counter.name, "request.duration")
  assert_eq(service_b_counter.name, "request.duration")
  assert_eq(service_c_counter.name, "request.duration")
  assert_eq(service_d_counter.name, "request.duration")
}