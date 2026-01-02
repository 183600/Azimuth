// 跨服务一致性测试用例
// 测试Azimuth遥测系统在跨多个服务环境中的一致性行为

test "跨服务追踪ID一致性" {
  // 测试跨服务的追踪ID一致性
  let service1_tracer = TracerProvider::with_service_name("service-1")
  let service2_tracer = TracerProvider::with_service_name("service-2")
  let service3_tracer = TracerProvider::with_service_name("service-3")
  
  let tracer1 = TracerProvider::get_tracer(service1_tracer, "service1.tracer")
  let tracer2 = TracerProvider::get_tracer(service2_tracer, "service2.tracer")
  let tracer3 = TracerProvider::get_tracer(service3_tracer, "service3.tracer")
  
  // 在服务1中创建根span
  let root_span = Tracer::start_span(tracer1, "cross.service.request")
  Span::set_attribute(root_span, "service.name", "service-1")
  Span::set_attribute(root_span, "request.id", "req-12345")
  
  let root_ctx = Span::span_context(root_span)
  let root_trace_id = SpanContext::trace_id(root_ctx)
  
  // 模拟服务间调用，传播追踪上下文
  let service1_to_service2_headers = ContextPropagator::inject(root_ctx)
  
  // 在服务2中提取上下文并创建子span
  let service2_ctx = ContextPropagator::extract(service1_to_service2_headers)
  let service2_span = Tracer::start_span_with_context(tracer2, "service2.processing", service2_ctx)
  Span::set_attribute(service2_span, "service.name", "service-2")
  Span::set_attribute(service2_span, "parent.service", "service-1")
  
  let service2_ctx_obj = Span::span_context(service2_span)
  let service2_trace_id = SpanContext::trace_id(service2_ctx_obj)
  
  // 验证追踪ID一致性
  assert_eq(root_trace_id, service2_trace_id, "Trace ID should be consistent across services")
  
  // 从服务2到服务3的调用
  let service2_to_service3_headers = ContextPropagator::inject(service2_ctx_obj)
  
  // 在服务3中提取上下文并创建子span
  let service3_ctx = ContextPropagator::extract(service2_to_service3_headers)
  let service3_span = Tracer::start_span_with_context(tracer3, "service3.processing", service3_ctx)
  Span::set_attribute(service3_span, "service.name", "service-3")
  Span::set_attribute(service3_span, "parent.service", "service-2")
  
  let service3_ctx_obj = Span::span_context(service3_span)
  let service3_trace_id = SpanContext::trace_id(service3_ctx_obj)
  
  // 验证追踪ID一致性
  assert_eq(root_trace_id, service3_trace_id, "Trace ID should be consistent across all services")
  
  // 验证span父子关系
  assert_eq(SpanContext::span_id(root_ctx), SpanContext::parent_span_id(service2_ctx_obj), 
            "Service2 span should have Service1 span as parent")
  assert_eq(SpanContext::span_id(service2_ctx_obj), SpanContext::parent_span_id(service3_ctx_obj), 
            "Service3 span should have Service2 span as parent")
  
  Span::end(service3_span)
  Span::end(service2_span)
  Span::end(root_span)
  
  assert_true(true)
}

test "跨服务度量一致性" {
  // 测试跨服务的度量一致性
  let service1_meter = MeterProvider::with_service_name("service-1")
  let service2_meter = MeterProvider::with_service_name("service-2")
  let service3_meter = MeterProvider::with_service_name("service-3")
  
  let meter1 = MeterProvider::get_meter(service1_meter, "service1.meter")
  let meter2 = MeterProvider::get_meter(service2_meter, "service2.meter")
  let meter3 = MeterProvider::get_meter(service3_meter, "service3.meter")
  
  // 在所有服务中创建相同名称的度量
  let service1_counter = Meter::create_counter(meter1, "cross.service.requests", Some("Cross-service requests"), Some("count"))
  let service2_counter = Meter::create_counter(meter2, "cross.service.requests", Some("Cross-service requests"), Some("count"))
  let service3_counter = Meter::create_counter(meter3, "cross.service.requests", Some("Cross-service requests"), Some("count"))
  
  // 记录一致的度量值
  Counter::add_with_attributes(service1_counter, 1.0, [
    ("service.name", "service-1"),
    ("operation", "process_request"),
    ("status", "success")
  ])
  
  Counter::add_with_attributes(service2_counter, 1.0, [
    ("service.name", "service-2"),
    ("operation", "process_request"),
    ("status", "success")
  ])
  
  Counter::add_with_attributes(service3_counter, 1.0, [
    ("service.name", "service-3"),
    ("operation", "process_request"),
    ("status", "success")
  ])
  
  // 验证度量定义一致性
  assert_eq(service1_counter.name, service2_counter.name, "Counter names should be consistent")
  assert_eq(service2_counter.name, service3_counter.name, "Counter names should be consistent")
  
  assert_eq(service1_counter.description, service2_counter.description, "Counter descriptions should be consistent")
  assert_eq(service2_counter.description, service3_counter.description, "Counter descriptions should be consistent")
  
  assert_eq(service1_counter.unit, service2_counter.unit, "Counter units should be consistent")
  assert_eq(service2_counter.unit, service3_counter.unit, "Counter units should be consistent")
  
  // 创建直方图并测试一致性
  let service1_histogram = Meter::create_histogram(meter1, "cross.service.duration", Some("Cross-service duration"), Some("ms"))
  let service2_histogram = Meter::create_histogram(meter2, "cross.service.duration", Some("Cross-service duration"), Some("ms"))
  let service3_histogram = Meter::create_histogram(meter3, "cross.service.duration", Some("Cross-service duration"), Some("ms"))
  
  // 记录一致的度量值
  Histogram::record_with_attributes(service1_histogram, 100.5, [
    ("service.name", "service-1"),
    ("operation", "process_request")
  ])
  
  Histogram::record_with_attributes(service2_histogram, 150.7, [
    ("service.name", "service-2"),
    ("operation", "process_request")
  ])
  
  Histogram::record_with_attributes(service3_histogram, 125.3, [
    ("service.name", "service-3"),
    ("operation", "process_request")
  ])
  
  // 验证直方图定义一致性
  assert_eq(service1_histogram.name, service2_histogram.name, "Histogram names should be consistent")
  assert_eq(service2_histogram.name, service3_histogram.name, "Histogram names should be consistent")
  
  assert_eq(service1_histogram.description, service2_histogram.description, "Histogram descriptions should be consistent")
  assert_eq(service2_histogram.description, service3_histogram.description, "Histogram descriptions should be consistent")
  
  assert_eq(service1_histogram.unit, service2_histogram.unit, "Histogram units should be consistent")
  assert_eq(service2_histogram.unit, service3_histogram.unit, "Histogram units should be consistent")
  
  assert_true(true)
}

test "跨服务日志一致性" {
  // 测试跨服务的日志一致性
  let service1_logger = LoggerProvider::with_service_name("service-1")
  let service2_logger = LoggerProvider::with_service_name("service-2")
  let service3_logger = LoggerProvider::with_service_name("service-3")
  
  let logger1 = LoggerProvider::get_logger(service1_logger, "service1.logger")
  let logger2 = LoggerProvider::get_logger(service2_logger, "service2.logger")
  let logger3 = LoggerProvider::get_logger(service3_logger, "service3.logger")
  
  // 创建一致的日志记录
  let service1_log = LogRecord::new(Info, "Cross-service operation started")
  LogRecord::add_attribute(service1_log, "service.name", "service-1")
  LogRecord::add_attribute(service1_log, "operation.id", "op-12345")
  LogRecord::add_attribute(service1_log, "request.id", "req-67890")
  LogRecord::add_attribute(service1_log, "correlation.id", "corr-abcde")
  
  let service2_log = LogRecord::new(Info, "Cross-service operation started")
  LogRecord::add_attribute(service2_log, "service.name", "service-2")
  LogRecord::add_attribute(service2_log, "operation.id", "op-12345")
  LogRecord::add_attribute(service2_log, "request.id", "req-67890")
  LogRecord::add_attribute(service2_log, "correlation.id", "corr-abcde")
  
  let service3_log = LogRecord::new(Info, "Cross-service operation started")
  LogRecord::add_attribute(service3_log, "service.name", "service-3")
  LogRecord::add_attribute(service3_log, "operation.id", "op-12345")
  LogRecord::add_attribute(service3_log, "request.id", "req-67890")
  LogRecord::add_attribute(service3_log, "correlation.id", "corr-abcde")
  
  // 验证日志严重性一致性
  assert_eq(LogRecord::severity_number(service1_log), LogRecord::severity_number(service2_log), 
            "Log severity should be consistent")
  assert_eq(LogRecord::severity_number(service2_log), LogRecord::severity_number(service3_log), 
            "Log severity should be consistent")
  
  // 验证日志消息一致性
  assert_eq(LogRecord::body(service1_log), LogRecord::body(service2_log), "Log messages should be consistent")
  assert_eq(LogRecord::body(service2_log), LogRecord::body(service3_log), "Log messages should be consistent")
  
  // 验证关键属性一致性
  let service1_attrs = LogRecord::get_attributes(service1_log)
  let service2_attrs = LogRecord::get_attributes(service2_log)
  let service3_attrs = LogRecord::get_attributes(service3_log)
  
  // 验证操作ID一致性
  assert_eq(Map::get(service1_attrs, "operation.id"), Map::get(service2_attrs, "operation.id"))
  assert_eq(Map::get(service2_attrs, "operation.id"), Map::get(service3_attrs, "operation.id"))
  
  // 验证请求ID一致性
  assert_eq(Map::get(service1_attrs, "request.id"), Map::get(service2_attrs, "request.id"))
  assert_eq(Map::get(service2_attrs, "request.id"), Map::get(service3_attrs, "request.id"))
  
  // 验证关联ID一致性
  assert_eq(Map::get(service1_attrs, "correlation.id"), Map::get(service2_attrs, "correlation.id"))
  assert_eq(Map::get(service2_attrs, "correlation.id"), Map::get(service3_attrs, "correlation.id"))
  
  Logger::emit(logger1, service1_log)
  Logger::emit(logger2, service2_log)
  Logger::emit(logger3, service3_log)
  
  assert_true(true)
}

test "跨服务资源属性一致性" {
  // 测试跨服务的资源属性一致性
  let service1_resource = Resource::with_service_name("service-1")
  let service2_resource = Resource::with_service_name("service-2")
  let service3_resource = Resource::with_service_name("service-3")
  
  // 添加通用资源属性
  let common_attributes = [
    ("deployment.environment", StringValue("production")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("azimuth")),
    ("region", StringValue("us-west-2")),
    ("availability.zone", StringValue("us-west-2a")),
    ("cloud.provider", StringValue("aws")),
    ("kubernetes.cluster.name", StringValue("prod-cluster")),
    ("kubernetes.namespace", StringValue("azimuth-prod"))
  ]
  
  let service1_with_common = Resource::with_attributes(service1_resource, common_attributes)
  let service2_with_common = Resource::with_attributes(service2_resource, common_attributes)
  let service3_with_common = Resource::with_attributes(service3_resource, common_attributes)
  
  // 添加服务特定属性
  let service1_specific = [
    ("service.type", StringValue("frontend")),
    ("container.image", StringValue("azimuth/frontend:2.1.0")),
    ("port", StringValue("8080"))
  ]
  
  let service2_specific = [
    ("service.type", StringValue("backend")),
    ("container.image", StringValue("azimuth/backend:2.1.0")),
    ("port", StringValue("9090"))
  ]
  
  let service3_specific = [
    ("service.type", StringValue("database")),
    ("container.image", StringValue("azimuth/database:2.1.0")),
    ("port", StringValue("5432"))
  ]
  
  let service1_final = Resource::with_attributes(service1_with_common, service1_specific)
  let service2_final = Resource::with_attributes(service2_with_common, service2_specific)
  let service3_final = Resource::with_attributes(service3_with_common, service3_specific)
  
  // 验证通用属性一致性
  let service1_attrs = Resource::get_all_attributes(service1_final)
  let service2_attrs = Resource::get_all_attributes(service2_final)
  let service3_attrs = Resource::get_all_attributes(service3_final)
  
  // 验证部署环境一致性
  assert_eq(Map::get(service1_attrs, "deployment.environment"), 
            Map::get(service2_attrs, "deployment.environment"))
  assert_eq(Map::get(service2_attrs, "deployment.environment"), 
            Map::get(service3_attrs, "deployment.environment"))
  
  // 验证服务版本一致性
  assert_eq(Map::get(service1_attrs, "service.version"), 
            Map::get(service2_attrs, "service.version"))
  assert_eq(Map::get(service2_attrs, "service.version"), 
            Map::get(service3_attrs, "service.version"))
  
  // 验证命名空间一致性
  assert_eq(Map::get(service1_attrs, "service.namespace"), 
            Map::get(service2_attrs, "service.namespace"))
  assert_eq(Map::get(service2_attrs, "service.namespace"), 
            Map::get(service3_attrs, "service.namespace"))
  
  // 验证区域一致性
  assert_eq(Map::get(service1_attrs, "region"), 
            Map::get(service2_attrs, "region"))
  assert_eq(Map::get(service2_attrs, "region"), 
            Map::get(service3_attrs, "region"))
  
  // 验证服务特定属性不同但符合预期
  assert_eq(Map::get(service1_attrs, "service.type"), Some(StringValue("frontend")))
  assert_eq(Map::get(service2_attrs, "service.type"), Some(StringValue("backend")))
  assert_eq(Map::get(service3_attrs, "service.type"), Some(StringValue("database")))
  
  assert_true(true)
}

test "跨服务采样一致性" {
  // 测试跨服务的采样一致性
  let sampling_config = SamplingConfiguration::new()
  SamplingConfiguration::set_parent_based(sampling_config, true)
  SamplingConfiguration::set_rate(sampling_config, 0.1)  // 10%采样率
  
  let service1_tracer = TracerProvider::with_sampling(sampling_config)
  let service2_tracer = TracerProvider::with_sampling(sampling_config)
  let service3_tracer = TracerProvider::with_sampling(sampling_config)
  
  let tracer1 = TracerProvider::get_tracer(service1_tracer, "service1.tracer")
  let tracer2 = TracerProvider::get_tracer(service2_tracer, "service2.tracer")
  let tracer3 = TracerProvider::get_tracer(service3_tracer, "service3.tracer")
  
  // 测试采样决策一致性
  let sampled_spans = []
  let unsampled_spans = []
  
  for i = 0; i < 100; i = i + 1 {
    let root_span = Tracer::start_span(tracer1, "sampling.test." + Int::to_string(i))
    let root_ctx = Span::span_context(root_span)
    let is_sampled = SpanContext::is_sampled(root_ctx)
    
    if is_sampled {
      Array::push(sampled_spans, root_span)
      
      // 在服务2中创建子span，应该继承采样决策
      let headers = ContextPropagator::inject(root_ctx)
      let service2_ctx = ContextPropagator::extract(headers)
      let service2_span = Tracer::start_span_with_context(tracer2, "service2.child", service2_ctx)
      let service2_ctx_obj = Span::span_context(service2_span)
      
      // 验证子span继承采样决策
      assert_true(SpanContext::is_sampled(service2_ctx_obj), "Child span should inherit sampling decision")
      
      // 在服务3中创建子span，应该继承采样决策
      let headers2 = ContextPropagator::inject(service2_ctx_obj)
      let service3_ctx = ContextPropagator::extract(headers2)
      let service3_span = Tracer::start_span_with_context(tracer3, "service3.child", service3_ctx)
      let service3_ctx_obj = Span::span_context(service3_span)
      
      // 验证子span继承采样决策
      assert_true(SpanContext::is_sampled(service3_ctx_obj), "Grandchild span should inherit sampling decision")
      
      Span::end(service3_span)
      Span::end(service2_span)
    } else {
      Array::push(unsampled_spans, root_span)
      
      // 在服务2中创建子span，应该继承不采样决策
      let headers = ContextPropagator::inject(root_ctx)
      let service2_ctx = ContextPropagator::extract(headers)
      let service2_span = Tracer::start_span_with_context(tracer2, "service2.child", service2_ctx)
      let service2_ctx_obj = Span::span_context(service2_span)
      
      // 验证子span继承不采样决策
      assert_false(SpanContext::is_sampled(service2_ctx_obj), "Child span should inherit not-sampled decision")
      
      Span::end(service2_span)
    }
    
    Span::end(root_span)
  }
  
  // 验证采样率大约为10%
  let sampled_rate = Int::to_float(Array::length(sampled_spans)) / Int::to_float(100)
  assert_true(sampled_rate >= 0.05 && sampled_rate <= 0.15, "Sampling rate should be approximately 10%")
  
  assert_true(true)
}

test "跨服务错误传播一致性" {
  // 测试跨服务的错误传播一致性
  let service1_tracer = TracerProvider::with_service_name("service-1")
  let service2_tracer = TracerProvider::with_service_name("service-2")
  let service3_tracer = TracerProvider::with_service_name("service-3")
  
  let tracer1 = TracerProvider::get_tracer(service1_tracer, "service1.tracer")
  let tracer2 = TracerProvider::get_tracer(service2_tracer, "service2.tracer")
  let tracer3 = TracerProvider::get_tracer(service3_tracer, "service3.tracer")
  
  // 在服务1中创建span并设置错误状态
  let root_span = Tracer::start_span(tracer1, "error.propagation.test")
  Span::set_status(root_span, Error, Some("Initial error in service-1"))
  Span::add_event(root_span, "error.occurred", [
    ("error.type", "ValidationError"),
    ("error.code", "ERR_001"),
    ("error.message", "Invalid input parameter")
  ])
  
  let root_ctx = Span::span_context(root_span)
  
  // 传播到服务2
  let headers = ContextPropagator::inject(root_ctx)
  let service2_ctx = ContextPropagator::extract(headers)
  let service2_span = Tracer::start_span_with_context(tracer2, "service2.error.handling", service2_ctx)
  
  // 在服务2中添加错误处理信息
  Span::add_event(service2_span, "error.received", [
    ("source.service", "service-1"),
    ("error.type", "ValidationError"),
    ("handling.strategy", "retry")
  ])
  
  // 模拟重试失败
  Span::add_event(service2_span, "retry.failed", [
    ("retry.count", "3"),
    ("final.error", "Max retries exceeded")
  ])
  
  Span::set_status(service2_span, Error, Some("Error propagation from service-1"))
  
  let service2_ctx_obj = Span::span_context(service2_span)
  
  // 传播到服务3
  let headers2 = ContextPropagator::inject(service2_ctx_obj)
  let service3_ctx = ContextPropagator::extract(headers2)
  let service3_span = Tracer::start_span_with_context(tracer3, "service3.error.handling", service3_ctx)
  
  // 在服务3中添加错误处理信息
  Span::add_event(service3_span, "error.received", [
    ("source.service", "service-2"),
    ("original.service", "service-1"),
    ("error.type", "ValidationError"),
    ("handling.strategy", "fallback")
  ])
  
  // 模拟降级处理
  Span::add_event(service3_span, "fallback.activated", [
    ("fallback.type", "default_response"),
    ("fallback.reason", "Original service unavailable")
  ])
  
  Span::set_status(service3_span, Ok, Some("Recovered with fallback"))
  
  Span::end(service3_span)
  Span::end(service2_span)
  Span::end(root_span)
  
  // 验证错误传播的一致性
  let service1_status = Span::get_status(root_span)
  let service2_status = Span::get_status(service2_span)
  let service3_status = Span::get_status(service3_span)
  
  // 验证错误状态传播
  assert_eq(Status::code(service1_status), Error)
  assert_eq(Status::code(service2_status), Error)
  assert_eq(Status::code(service3_status), Ok)  // 服务3成功恢复
  
  // 验证错误消息传播
  assert_true(String::contains(Status::description(service1_status), "Initial error in service-1"))
  assert_true(String::contains(Status::description(service2_status), "Error propagation from service-1"))
  assert_true(String::contains(Status::description(service3_status), "Recovered with fallback"))
  
  assert_true(true)
}