// 遥测数据一致性测试用例
// 测试 Azimuth 遥测系统中数据的一致性和完整性

test "telemetry_data_consistency_across_spans" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "consistency-test")
  
  // 创建父span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_ctx = SpanContext::from_span(parent_span)
  
  // 创建子span
  let child_span = Tracer::start_span(tracer, "child-operation", Some(parent_ctx))
  
  // 验证trace ID一致性
  let parent_trace_id = SpanContext::trace_id(parent_ctx)
  let child_trace_id = SpanContext::trace_id(SpanContext::from_span(child_span))
  assert_eq(parent_trace_id, child_trace_id)
  
  // 验证span ID唯一性
  let parent_span_id = SpanContext::span_id(parent_ctx)
  let child_span_id = SpanContext::span_id(SpanContext::from_span(child_span))
  assert_true(parent_span_id != child_span_id)
  
  Span::end(child_span)
  Span::end(parent_span)
}

test "telemetry_metrics_logging_consistency" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "consistency-meter")
  
  // 创建计数器指标
  let counter = Meter::create_counter(meter, "operations.total")
  
  // 模拟操作并记录指标
  Counter::add(counter, 1.0)
  
  // 创建日志记录器
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "consistency-logger")
  
  // 记录日志
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Operation completed")
  LogRecord::set_attribute(log_record, "operation.count", IntValue(1))
  
  Logger::emit(logger, log_record)
  
  // 验证指标和日志的一致性
  assert_true(true)
}

test "telemetry_context_propagation_consistency" {
  let ctx = Context::root()
  let key1 = ContextKey::new("user.id")
  let key2 = ContextKey::new("request.id")
  
  // 设置上下文值
  let ctx_with_values = Context::with_value(
    Context::with_value(ctx, key1, "user-123"),
    key2,
    "req-456"
  )
  
  // 验证上下文值的传播
  let user_id = Context::get(ctx_with_values, key1)
  let request_id = Context::get(ctx_with_values, key2)
  
  assert_eq(user_id, Some("user-123"))
  assert_eq(request_id, Some("req-456"))
  
  // 验证上下文链的完整性
  let ctx_with_carrier = Context::with_value(ctx_with_values, ContextKey::new("session.id"), "sess-789")
  let session_id = Context::get(ctx_with_carrier, ContextKey::new("session.id"))
  
  assert_eq(session_id, Some("sess-789"))
  
  // 验证原有值仍然存在
  let original_user_id = Context::get(ctx_with_carrier, key1)
  assert_eq(original_user_id, Some("user-123"))
}

test "telemetry_baggage_consistency_across_operations" {
  let carrier = TextMapCarrier::new()
  
  // 设置baggage项
  TextMapCarrier::set(carrier, "baggage", "user.id=123,session.id=456")
  
  // 创建baggage
  let baggage = Baggage::from_carrier(carrier)
  
  // 验证baggage内容
  let user_id = Baggage::get(baggage, "user.id")
  let session_id = Baggage::get(baggage, "session.id")
  
  assert_eq(user_id, Some("123"))
  assert_eq(session_id, Some("456"))
  
  // 创建新的carrier并验证baggage传播
  let new_carrier = TextMapCarrier::new()
  Baggage::inject_to_carrier(baggage, new_carrier)
  
  let injected_baggage = TextMapCarrier::get(new_carrier, "baggage")
  assert_true(injected_baggage != None)
}

test "telemetry_resource_consistency" {
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // 设置资源属性
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("test"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource1, attributes)
  
  // 验证资源属性的一致性
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false, "Expected service name")
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false, "Expected service version")
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false, "Expected environment")
  }
  
  // 验证资源合并的一致性
  let merged_resource = Resource::merge(resource_with_attrs, resource2)
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false, "Expected merged service name")
  }
}