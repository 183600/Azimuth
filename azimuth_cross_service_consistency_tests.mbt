// Azimuth Cross-Service Consistency Tests
// 测试跨服务遥测数据一致性功能

test "trace context consistency across microservices" {
  // 服务A创建根span
  let service_a_tracer = @azimuth.Tracer::get_provider().get_tracer("service-a")
  let root_span = service_a_tracer.start_span("root-operation")
  
  // 获取追踪上下文
  let context = root_span.get_context()
  
  // 服务A创建子span
  let child_span_a = service_a_tracer.start_span("service-a-operation", @azimuth.SpanKind::CLIENT, context)
  
  // 服务A注入上下文到HTTP头部
  let injector = @azimuth.TextMapInjector::new()
  let headers = @azimuth.Headers::new()
  injector.inject(context, headers)
  
  // 服务B提取上下文
  let service_b_tracer = @azimuth.Tracer::get_provider().get_tracer("service-b")
  let extractor = @azimuth.TextMapExtractor::new()
  let extracted_context = extractor.extract(headers)
  
  assert_true(extracted_context.is_some())
  
  // 服务B创建子span
  match extracted_context {
    Some(ctx) => {
      let child_span_b = service_b_tracer.start_span("service-b-operation", @azimuth.SpanKind::SERVER, ctx)
      
      // 验证追踪ID一致性
      assert_eq(child_span_b.get_context().trace_id, context.trace_id)
      assert_eq(child_span_b.get_context().trace_id, child_span_a.get_context().trace_id)
      
      // 验证父子关系
      assert_eq(child_span_b.get_context().parent_span_id, Some(child_span_a.get_context().span_id))
      
      child_span_b.end()
    }
    None => assert_true(false)
  }
  
  child_span_a.end()
  root_span.end()
}

test "metric consistency across services" {
  // 创建统一的度量配置
  let metric_config = @azimuth.MetricConfig::builder()
    .with_name("request.duration")
    .with_unit("ms")
    .with_description("Request duration in milliseconds")
    .build()
  
  // 服务A创建度量工具
  let service_a_meter = @azimuth.Meter::get_provider().get_meter("service-a")
  let service_a_histogram = service_a_meter.create_histogram(metric_config)
  
  // 服务B创建度量工具
  let service_b_meter = @azimuth.Meter::get_provider().get_meter("service-b")
  let service_b_histogram = service_b_meter.create_histogram(metric_config)
  
  // 记录度量数据
  let common_attributes = @azimuth.Attributes::builder()
    .with("endpoint", "/api/data")
    .with("method", "GET")
    .build()
  
  service_a_histogram.record(150.0, common_attributes)
  service_b_histogram.record(200.0, common_attributes)
  
  // 验证度量数据一致性
  let service_a_data = service_a_histogram.get_data()
  let service_b_data = service_b_histogram.get_data()
  
  assert_eq(service_a_data.name, service_b_data.name)
  assert_eq(service_a_data.unit, service_b_data.unit)
  assert_eq(service_a_data.description, service_b_data.description)
}

test "log correlation across services" {
  // 创建统一的日志配置
  let log_config = @azimuth.LogConfig::builder()
    .with_include_trace_id(true)
    .with_include_span_id(true)
    .build()
  
  // 服务A创建日志记录器
  let service_a_logger = @azimuth.Logger::get_provider().get_logger("service-a", log_config)
  
  // 服务B创建日志记录器
  let service_b_logger = @azimuth.Logger::get_provider().get_logger("service-b", log_config)
  
  // 创建追踪上下文
  let trace_id = @azimuth.TraceId::generate()
  let span_id = @azimuth.SpanId::generate()
  let context = @azimuth.TraceContext::new(trace_id, span_id, @azimuth.TraceFlags::SAMPLED)
  
  // 设置当前上下文
  @azimuth.Context::set_current(context)
  
  // 记录日志
  service_a_logger.info("Service A processing request")
  service_b_logger.error("Service B encountered error")
  
  // 验证日志关联性
  let service_a_logs = service_a_logger.get_logs()
  let service_b_logs = service_b_logger.get_logs()
  
  assert_true(service_a_logs.length() > 0)
  assert_true(service_b_logs.length() > 0)
  
  // 验证日志包含相同的追踪ID
  let last_log_a = service_a_logs[service_a_logs.length() - 1]
  let last_log_b = service_b_logs[service_b_logs.length() - 1]
  
  assert_eq(last_log_a.trace_id, Some(trace_id))
  assert_eq(last_log_b.trace_id, Some(trace_id))
}

test "attribute consistency across services" {
  // 定义通用属性键
  let common_keys = [
    "service.name",
    "service.version",
    "deployment.environment",
    "host.name"
  ]
  
  // 服务A创建资源
  let service_a_resource = @azimuth.Resource::builder()
    .with_service_name("auth-service")
    .with_service_version("1.2.3")
    .with_attribute("deployment.environment", "production")
    .with_attribute("host.name", "auth-host-1")
    .build()
  
  // 服务B创建资源
  let service_b_resource = @azimuth.Resource::builder()
    .with_service_name("user-service")
    .with_service_version("1.2.3")
    .with_attribute("deployment.environment", "production")
    .with_attribute("host.name", "user-host-1")
    .build()
  
  // 验证通用属性键的存在
  for key in common_keys {
    assert_true(service_a_resource.get_attribute(key).is_some())
    assert_true(service_b_resource.get_attribute(key).is_some())
  }
  
  // 验证特定属性值
  assert_eq(service_a_resource.get_attribute("service.name"), Some("auth-service"))
  assert_eq(service_b_resource.get_attribute("service.name"), Some("user-service"))
  
  // 验证共享属性值
  assert_eq(service_a_resource.get_attribute("service.version"), Some("1.2.3"))
  assert_eq(service_b_resource.get_attribute("service.version"), Some("1.2.3"))
  assert_eq(service_a_resource.get_attribute("deployment.environment"), Some("production"))
  assert_eq(service_b_resource.get_attribute("deployment.environment"), Some("production"))
}

test "baggage propagation consistency" {
  // 创建初始baggage
  let initial_baggage = @azimuth.Baggage::builder()
    .with("user.id", "12345")
    .with("request.id", "req-abc-123")
    .with("client.version", "2.1.0")
    .build()
  
  // 服务A添加baggage项
  let service_a_baggage = initial_baggage
    .to_builder()
    .with("service.a.timestamp", "2023-01-01T12:00:00Z")
    .build()
  
  // 序列化和传输baggage
  let serialized = service_a_baggage.to_string()
  
  // 服务B反序列化baggage
  let service_b_baggage = @azimuth.Baggage::from_string(serialized)
  
  assert_true(service_b_baggage.is_some())
  
  // 验证baggage一致性
  match service_b_baggage {
    Some(baggage) => {
      assert_eq(baggage.get("user.id"), Some("12345"))
      assert_eq(baggage.get("request.id"), Some("req-abc-123"))
      assert_eq(baggage.get("client.version"), Some("2.1.0"))
      assert_eq(baggage.get("service.a.timestamp"), Some("2023-01-01T12:00:00Z"))
      
      // 服务B添加自己的baggage项
      let service_b_extended = baggage
        .to_builder()
        .with("service.b.processing.time", "150ms")
        .build()
      
      // 验证所有baggage项都存在
      assert_eq(service_b_extended.get("user.id"), Some("12345"))
      assert_eq(service_b_extended.get("request.id"), Some("req-abc-123"))
      assert_eq(service_b_extended.get("client.version"), Some("2.1.0"))
      assert_eq(service_b_extended.get("service.a.timestamp"), Some("2023-01-01T12:00:00Z"))
      assert_eq(service_b_extended.get("service.b.processing.time"), Some("150ms"))
    }
    None => assert_true(false)
  }
}

test "sampling decision consistency across services" {
  // 创建统一的采样配置
  let sampling_config = @azimuth.SamplingConfig::builder()
    .with_sample_rate(0.5)
    .build()
  
  // 创建采样器
  let sampler = @azimuth.TraceIdRatioBasedSampler::new(sampling_config)
  
  // 生成追踪ID
  let trace_id = @azimuth.TraceId::from_string("4bf92f3577b34da6a3ce929d0e0e4736")
  
  // 服务A做出采样决策
  let service_a_decision = sampler.should_sample(trace_id)
  
  // 服务B使用相同的追踪ID做出采样决策
  let service_b_decision = sampler.should_sample(trace_id)
  
  // 验证采样决策一致性
  assert_eq(service_a_decision, service_b_decision)
  
  // 验证采样决策在上下文中传播
  let context = @azimuth.TraceContext::new(
    trace_id,
    @azimuth.SpanId::generate(),
    if service_a_decision { @azimuth.TraceFlags::SAMPLED } else { @azimuth.TraceFlags::NOT_SAMPLED }
  )
  
  // 序列化和反序列化上下文
  let serialized = context.to_string()
  let deserialized = @azimuth.TraceContext::from_string(serialized)
  
  assert_true(deserialized.is_some())
  match deserialized {
    Some(parsed_context) => {
      assert_eq(parsed_context.is_sampled(), service_a_decision)
    }
    None => assert_true(false)
  }
}