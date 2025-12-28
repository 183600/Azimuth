// 跨服务一致性高级测试
// 测试分布式系统中跨服务的数据一致性、传播和同步

test "跨服务追踪一致性测试" {
  // 创建服务A的追踪上下文
  let service_a_trace_id = "trace-1234567890abcdef"
  let service_a_span_id = "span-1111111111111111"
  let service_a_context = SpanContext::new(service_a_trace_id, service_a_span_id, true, "key1=value1")
  
  // 验证服务A的追踪上下文
  assert_eq(SpanContext::trace_id(service_a_context), service_a_trace_id)
  assert_eq(SpanContext::span_id(service_a_context), service_a_span_id)
  assert_true(SpanContext::is_sampled(service_a_context))
  assert_true(SpanContext::is_valid(service_a_context))
  
  // 模拟服务A到服务B的传播
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  let ctx_a = Context::root()
  let ctx_a_with_span = Context::with_value(ctx_a, ContextKey::new("span.context"), "service-a-data")
  
  // 注入追踪信息
  let composite_propagator = CompositePropagator::new([propagator])
  CompositePropagator::inject(composite_propagator, ctx_a_with_span, carrier)
  
  // 服务B接收并提取追踪信息
  let ctx_b = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_b = Context::get(ctx_b, ContextKey::new("extracted"))
  
  assert_eq(extracted_b, Some("true"), "服务B应该能提取追踪信息")
  
  // 服务B创建子span
  let service_b_span_id = "span-2222222222222222"
  let service_b_context = SpanContext::new(service_a_trace_id, service_b_span_id, true, "key1=value1")
  
  // 验证跨服务的追踪一致性
  assert_eq(SpanContext::trace_id(service_b_context), service_a_trace_id, "跨服务追踪ID应该一致")
  assert_eq(SpanContext::span_id(service_b_context), service_b_span_id, "子服务span ID应该唯一")
  assert_eq(SpanContext::is_sampled(service_b_context), SpanContext::is_sampled(service_a_context), "采样决策应该一致")
}

test "跨服务度量一致性测试" {
  // 创建全局度量提供者
  let global_provider = MeterProvider::default()
  
  // 服务A创建度量
  let service_a_meter = MeterProvider::get_meter(global_provider, "service-a")
  let service_a_counter = Meter::create_counter(service_a_meter, "http.requests.total")
  let service_a_histogram = Meter::create_histogram(service_a_meter, "request.duration")
  let service_a_updown = Meter::create_updown_counter(service_a_meter, "active.connections")
  let service_a_gauge = Meter::create_gauge(service_a_meter, "memory.usage")
  
  // 服务A记录度量数据
  Counter::add(service_a_counter, 100.0)
  Histogram::record(service_a_histogram, 150.5)
  UpDownCounter::add(service_a_updown, 25.0)
  
  // 服务B创建同名度量
  let service_b_meter = MeterProvider::get_meter(global_provider, "service-b")
  let service_b_counter = Meter::create_counter(service_b_meter, "http.requests.total")
  let service_b_histogram = Meter::create_histogram(service_b_meter, "request.duration")
  let service_b_updown = Meter::create_updown_counter(service_b_meter, "active.connections")
  let service_b_gauge = Meter::create_gauge(service_b_meter, "memory.usage")
  
  // 服务B记录度量数据
  Counter::add(service_b_counter, 200.0)
  Histogram::record(service_b_histogram, 75.25)
  UpDownCounter::add(service_b_updown, -5.0)
  
  // 验证跨服务度量命名一致性
  assert_eq(service_a_counter.name, service_b_counter.name, "跨服务计数器名称应该一致")
  assert_eq(service_a_histogram.name, service_b_histogram.name, "跨服务直方图名称应该一致")
  assert_eq(service_a_updown.name, service_b_updown.name, "跨服务上下行计数器名称应该一致")
  assert_eq(service_a_gauge.name, service_b_gauge.name, "跨服务仪表名称应该一致")
  
  // 验证度量类型一致性
  match Instrument::name(Counter(service_a_counter.name, service_a_counter.description, service_a_counter.unit)) {
    name => assert_eq(name, "http.requests.total", "度量名称应该正确")
  }
  
  match Instrument::name(Histogram(service_a_histogram.name, service_a_histogram.description, service_a_histogram.unit)) {
    name => assert_eq(name, "request.duration", "直方图名称应该正确")
  }
}

test "跨服务日志一致性测试" {
  // 创建全局日志提供者
  let global_logger_provider = LoggerProvider::default()
  
  // 服务A创建日志器
  let service_a_logger = LoggerProvider::get_logger(global_logger_provider, "service-a")
  
  // 服务A创建日志记录
  let service_a_trace_id = "trace-abcdef1234567890"
  let service_a_span_id = "span-aaaaaaaaaaaaaaaa"
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(service_a_trace_id),
    Some(service_a_span_id),
    Some(Context::with_value(Context::root(), ContextKey::new("service"), "service-a"))
  )
  
  // 发送服务A的日志
  Logger::emit(service_a_logger, service_a_log)
  
  // 服务B创建日志器
  let service_b_logger = LoggerProvider::get_logger(global_logger_provider, "service-b")
  
  // 服务B创建相关日志记录（相同的追踪上下文）
  let service_b_span_id = "span-bbbbbbbbbbbbbbbb"
  let service_b_log = LogRecord::new_with_context(
    Warn,
    Some("Service B detected potential issue"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some(service_a_trace_id), // 相同的追踪ID
    Some(service_b_span_id),  // 不同的span ID
    Some(Context::with_value(Context::root(), ContextKey::new("service"), "service-b"))
  )
  
  // 发送服务B的日志
  Logger::emit(service_b_logger, service_b_log)
  
  // 验证跨服务日志追踪一致性
  assert_eq(LogRecord::trace_id(service_a_log), Some(service_a_trace_id), "服务A日志追踪ID应该正确")
  assert_eq(LogRecord::trace_id(service_b_log), Some(service_a_trace_id), "服务B日志追踪ID应该与服务A一致")
  assert_eq(LogRecord::span_id(service_a_log), Some(service_a_span_id), "服务A日志span ID应该正确")
  assert_eq(LogRecord::span_id(service_b_log), Some(service_b_span_id), "服务B日志span ID应该唯一")
  
  // 验证日志时间顺序
  match (LogRecord::timestamp(service_a_log), LogRecord::timestamp(service_b_log)) {
    (Some(ts_a), Some(ts_b)) => assert_true(ts_b > ts_a, "服务B日志时间应该晚于服务A")
    _ => assert_true(false, "期望两个日志都有时间戳")
  }
}

test "跨服务上下文传播一致性测试" {
  // 创建服务A的初始上下文
  let service_a_context = Context::root()
  let correlation_id_key = ContextKey::new("correlation.id")
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  
  // 服务A添加上下文信息
  let service_a_enriched = Context::with_value(
    Context::with_value(
      Context::with_value(service_a_context, correlation_id_key, "corr-12345"),
      user_id_key, "user-67890"
    ),
    request_id_key, "req-abcdef"
  )
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 服务A注入上下文
  let carrier_ab = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_enriched, carrier_ab)
  
  // 服务B提取上下文
  let service_b_context = CompositePropagator::extract(composite_propagator, carrier_ab)
  
  // 服务B添加自己的上下文
  let service_b_key = ContextKey::new("service.b.operation")
  let service_b_enriched = Context::with_value(service_b_context, service_b_key, "process.data")
  
  // 服务B传播到服务C
  let carrier_bc = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_b_enriched, carrier_bc)
  
  // 服务C提取最终上下文
  let service_c_context = CompositePropagator::extract(composite_propagator, carrier_bc)
  
  // 验证跨服务上下文传播一致性
  let extracted_correlation = Context::get(service_c_context, correlation_id_key)
  let extracted_user_id = Context::get(service_c_context, user_id_key)
  let extracted_request_id = Context::get(service_c_context, request_id_key)
  let extracted_service_b = Context::get(service_c_context, service_b_key)
  let extracted_trace = Context::get(service_c_context, ContextKey::new("extracted"))
  
  assert_eq(extracted_correlation, Some("corr-12345"), "关联ID应该在所有服务间传播")
  assert_eq(extracted_user_id, Some("user-67890"), "用户ID应该在所有服务间传播")
  assert_eq(extracted_request_id, Some("req-abcdef"), "请求ID应该在所有服务间传播")
  assert_eq(extracted_service_b, Some("process.data"), "服务B的上下文应该传播到服务C")
  assert_eq(extracted_trace, Some("true"), "追踪信息应该正确传播")
}

test "跨服务资源一致性测试" {
  // 创建全局资源定义
  let global_resource = Resource::new()
  let global_attributes = [
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod")),
    ("host.name", StringValue("prod-cluster-01")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2"))
  ]
  let enriched_global = Resource::with_attributes(global_resource, global_attributes)
  
  // 服务A特定资源
  let service_a_resource = Resource::new()
  let service_a_attributes = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("user-service-001")),
    ("port", IntValue(8080))
  ]
  let enriched_service_a = Resource::with_attributes(service_a_resource, service_a_attributes)
  
  // 服务B特定资源
  let service_b_resource = Resource::new()
  let service_b_attributes = [
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("1.5.2")),
    ("service.instance.id", StringValue("order-service-003")),
    ("port", IntValue(8081))
  ]
  let enriched_service_b = Resource::with_attributes(service_b_resource, service_b_attributes)
  
  // 合并全局和服务特定资源
  let merged_resource_a = Resource::merge(enriched_global, enriched_service_a)
  let merged_resource_b = Resource::merge(enriched_global, enriched_service_b)
  
  // 验证跨服务资源一致性
  let global_namespace_a = Resource::get_attribute(merged_resource_a, "service.namespace")
  let global_namespace_b = Resource::get_attribute(merged_resource_b, "service.namespace")
  let global_env_a = Resource::get_attribute(merged_resource_a, "deployment.environment")
  let global_env_b = Resource::get_attribute(merged_resource_b, "deployment.environment")
  let global_host_a = Resource::get_attribute(merged_resource_a, "host.name")
  let global_host_b = Resource::get_attribute(merged_resource_b, "host.name")
  
  // 验证全局属性在所有服务中一致
  match (global_namespace_a, global_namespace_b) {
    (Some(StringValue(ns_a)), Some(StringValue(ns_b))) => {
      assert_eq(ns_a, ns_b, "服务命名空间在跨服务间应该一致")
      assert_eq(ns_a, "production", "服务命名空间应该正确")
    }
    _ => assert_true(false, "期望服务命名空间为字符串值")
  }
  
  match (global_env_a, global_env_b) {
    (Some(StringValue(env_a)), Some(StringValue(env_b))) => {
      assert_eq(env_a, env_b, "部署环境在跨服务间应该一致")
      assert_eq(env_a, "prod", "部署环境应该正确")
    }
    _ => assert_true(false, "期望部署环境为字符串值")
  }
  
  match (global_host_a, global_host_b) {
    (Some(StringValue(host_a)), Some(StringValue(host_b))) => {
      assert_eq(host_a, host_b, "主机名在跨服务间应该一致")
      assert_eq(host_a, "prod-cluster-01", "主机名应该正确")
    }
    _ => assert_true(false, "期望主机名为字符串值")
  }
  
  // 验证服务特定属性的正确性
  let service_name_a = Resource::get_attribute(merged_resource_a, "service.name")
  let service_name_b = Resource::get_attribute(merged_resource_b, "service.name")
  
  match service_name_a {
    Some(StringValue(name)) => assert_eq(name, "user-service", "服务A名称应该正确")
    _ => assert_true(false, "期望服务A名称")
  }
  
  match service_name_b {
    Some(StringValue(name)) => assert_eq(name, "order-service", "服务B名称应该正确")
    _ => assert_true(false, "期望服务B名称")
  }
}

test "跨服务并发一致性测试" {
  // 模拟多个服务并发操作
  let base_trace_id = "concurrent-trace-123456"
  
  // 服务A、B、C同时处理相同的追踪
  let service_a_context = SpanContext::new(base_trace_id, "span-aaaaaaaaaaaaaaaa", true, "")
  let service_b_context = SpanContext::new(base_trace_id, "span-bbbbbbbbbbbbbbbb", true, "")
  let service_c_context = SpanContext::new(base_trace_id, "span-cccccccccccccccc", true, "")
  
  // 验证并发操作的追踪一致性
  assert_eq(SpanContext::trace_id(service_a_context), base_trace_id, "服务A追踪ID应该一致")
  assert_eq(SpanContext::trace_id(service_b_context), base_trace_id, "服务B追踪ID应该一致")
  assert_eq(SpanContext::trace_id(service_c_context), base_trace_id, "服务C追踪ID应该一致")
  
  // 验证span ID的唯一性
  assert_true(SpanContext::span_id(service_a_context) != SpanContext::span_id(service_b_context), "服务A和B的span ID应该不同")
  assert_true(SpanContext::span_id(service_b_context) != SpanContext::span_id(service_c_context), "服务B和C的span ID应该不同")
  assert_true(SpanContext::span_id(service_a_context) != SpanContext::span_id(service_c_context), "服务A和C的span ID应该不同")
  
  // 并发度量操作
  let provider = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider, "service-a")
  let meter_b = MeterProvider::get_meter(provider, "service-b")
  let meter_c = MeterProvider::get_meter(provider, "service-c")
  
  let counter_a = Meter::create_counter(meter_a, "concurrent.operations")
  let counter_b = Meter::create_counter(meter_b, "concurrent.operations")
  let counter_c = Meter::create_counter(meter_c, "concurrent.operations")
  
  // 并发记录度量
  Counter::add(counter_a, 10.0)
  Counter::add(counter_b, 15.0)
  Counter::add(counter_c, 20.0)
  
  // 验证并发度量命名一致性
  assert_eq(counter_a.name, counter_b.name, "并发计数器名称应该一致")
  assert_eq(counter_b.name, counter_c.name, "并发计数器名称应该一致")
  assert_eq(counter_a.name, "concurrent.operations", "计数器名称应该正确")
  
  // 并发日志操作
  let logger_provider = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(logger_provider, "service-a")
  let logger_b = LoggerProvider::get_logger(logger_provider, "service-b")
  let logger_c = LoggerProvider::get_logger(logger_provider, "service-c")
  
  let timestamp = 1735689600000000000L
  
  let log_a = LogRecord::new_with_context(
    Info, Some("Service A concurrent operation"), None, 
    Some(timestamp), Some(timestamp + 1000000L), 
    Some(base_trace_id), Some(SpanContext::span_id(service_a_context)), 
    Some(Context::root())
  )
  
  let log_b = LogRecord::new_with_context(
    Info, Some("Service B concurrent operation"), None, 
    Some(timestamp), Some(timestamp + 2000000L), 
    Some(base_trace_id), Some(SpanContext::span_id(service_b_context)), 
    Some(Context::root())
  )
  
  let log_c = LogRecord::new_with_context(
    Info, Some("Service C concurrent operation"), None, 
    Some(timestamp), Some(timestamp + 3000000L), 
    Some(base_trace_id), Some(SpanContext::span_id(service_c_context)), 
    Some(Context::root())
  )
  
  // 发送并发日志
  Logger::emit(logger_a, log_a)
  Logger::emit(logger_b, log_b)
  Logger::emit(logger_c, log_c)
  
  // 验证并发日志追踪一致性
  assert_eq(LogRecord::trace_id(log_a), Some(base_trace_id), "并发日志A追踪ID应该一致")
  assert_eq(LogRecord::trace_id(log_b), Some(base_trace_id), "并发日志B追踪ID应该一致")
  assert_eq(LogRecord::trace_id(log_c), Some(base_trace_id), "并发日志C追踪ID应该一致")
  
  // 验证并发日志span唯一性
  assert_eq(LogRecord::span_id(log_a), Some(SpanContext::span_id(service_a_context)), "日志A span ID应该匹配")
  assert_eq(LogRecord::span_id(log_b), Some(SpanContext::span_id(service_b_context)), "日志B span ID应该匹配")
  assert_eq(LogRecord::span_id(log_c), Some(SpanContext::span_id(service_c_context)), "日志C span ID应该匹配")
}