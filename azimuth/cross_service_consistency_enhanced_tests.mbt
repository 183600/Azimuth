// Cross Service Consistency Enhanced Tests - 跨服务一致性增强测试
// 专注于分布式追踪中跨服务的数据一致性、传播和验证

test "跨服务Trace ID和Span ID一致性验证测试" {
  // 创建统一的Trace ID
  let unified_trace_id = "0af7651916cd43dd8448eb211c80319c"
  
  // 服务A：创建根Span
  let service_a_span_id = "b7ad6b7169203331"
  let service_a_span_ctx = SpanContext::new(unified_trace_id, service_a_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 服务B：创建子Span
  let service_b_span_id = "b7ad6b7169203332"
  let service_b_span_ctx = SpanContext::new(unified_trace_id, service_b_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 服务C：创建另一个子Span
  let service_c_span_id = "b7ad6b7169203333"
  let service_c_span_ctx = SpanContext::new(unified_trace_id, service_c_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 验证所有服务使用相同的Trace ID
  assert_eq(SpanContext::trace_id(service_a_span_ctx), unified_trace_id)
  assert_eq(SpanContext::trace_id(service_b_span_ctx), unified_trace_id)
  assert_eq(SpanContext::trace_id(service_c_span_ctx), unified_trace_id)
  
  // 验证每个服务有唯一的Span ID
  assert_true(SpanContext::span_id(service_a_span_ctx) != SpanContext::span_id(service_b_span_ctx))
  assert_true(SpanContext::span_id(service_b_span_ctx) != SpanContext::span_id(service_c_span_ctx))
  assert_true(SpanContext::span_id(service_a_span_ctx) != SpanContext::span_id(service_c_span_ctx))
  
  // 验证所有Span都是有效的
  assert_true(SpanContext::is_valid(service_a_span_ctx))
  assert_true(SpanContext::is_valid(service_b_span_ctx))
  assert_true(SpanContext::is_valid(service_c_span_ctx))
  
  // 验证采样状态一致性
  assert_true(SpanContext::is_sampled(service_a_span_ctx))
  assert_true(SpanContext::is_sampled(service_b_span_ctx))
  assert_true(SpanContext::is_sampled(service_c_span_ctx))
}

test "跨服务Baggage传播一致性测试" {
  // 服务A：创建初始Baggage
  let service_a_baggage = Baggage::new()
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "user.id", "user-12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "request.id", "req-67890")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "tenant.id", "tenant-abc")
  
  // 验证服务A的Baggage
  let user_id = Baggage::get_entry(service_a_baggage, "user.id")
  let request_id = Baggage::get_entry(service_a_baggage, "request.id")
  let tenant_id = Baggage::get_entry(service_a_baggage, "tenant.id")
  
  assert_true(user_id is Some)
  match user_id {
    Some(value) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  assert_true(request_id is Some)
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    _ => assert_true(false)
  }
  
  assert_true(tenant_id is Some)
  match tenant_id {
    Some(value) => assert_eq(value, "tenant-abc")
    _ => assert_true(false)
  }
  
  // 服务B：继承并添加新的Baggage项
  let service_b_baggage = service_a_baggage
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "service.b.timestamp", "1735689600")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "service.b.operation", "process_data")
  
  // 验证服务B保留了原有的Baggage项
  let b_user_id = Baggage::get_entry(service_b_baggage, "user.id")
  let b_request_id = Baggage::get_entry(service_b_baggage, "request.id")
  let b_tenant_id = Baggage::get_entry(service_b_baggage, "tenant.id")
  
  assert_true(b_user_id is Some)
  match b_user_id {
    Some(value) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  // 验证服务B新增的Baggage项
  let b_timestamp = Baggage::get_entry(service_b_baggage, "service.b.timestamp")
  let b_operation = Baggage::get_entry(service_b_baggage, "service.b.operation")
  
  assert_true(b_timestamp is Some)
  match b_timestamp {
    Some(value) => assert_eq(value, "1735689600")
    _ => assert_true(false)
  }
  
  // 服务C：继续传播Baggage
  let service_c_baggage = service_b_baggage
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "service.c.result", "success")
  
  // 验证服务C保留了所有之前的Baggage项
  let c_user_id = Baggage::get_entry(service_c_baggage, "user.id")
  let c_timestamp = Baggage::get_entry(service_c_baggage, "service.b.timestamp")
  let c_result = Baggage::get_entry(service_c_baggage, "service.c.result")
  
  assert_true(c_user_id is Some)
  match c_user_id {
    Some(value) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  assert_true(c_timestamp is Some)
  match c_timestamp {
    Some(value) => assert_eq(value, "1735689600")
    _ => assert_true(false)
  }
  
  assert_true(c_result is Some)
  match c_result {
    Some(value) => assert_eq(value, "success")
    _ => assert_true(false)
  }
}

test "跨服务上下文传播完整性测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 服务A：添加上下文信息
  let service_a_key = ContextKey::new("service.a.correlation.id")
  let service_a_ctx = Context::with_value(root_ctx, service_a_key, "corr-12345")
  
  // 服务B：继承并添加上下文
  let service_b_key = ContextKey::new("service.b.session.id")
  let service_b_ctx = Context::with_value(service_a_ctx, service_b_key, "sess-67890")
  
  // 服务C：继续添加上下文
  let service_c_key = ContextKey::new("service.c.operation.id")
  let service_c_ctx = Context::with_value(service_b_ctx, service_c_key, "op-abcdef")
  
  // 验证上下文传播的完整性
  let a_correlation_id = Context::get(service_c_ctx, service_a_key)
  let b_session_id = Context::get(service_c_ctx, service_b_key)
  let c_operation_id = Context::get(service_c_ctx, service_c_key)
  
  assert_true(a_correlation_id is Some)
  match a_correlation_id {
    Some(value) => assert_eq(value, "corr-12345")
    _ => assert_true(false)
  }
  
  assert_true(b_session_id is Some)
  match b_session_id {
    Some(value) => assert_eq(value, "sess-67890")
    _ => assert_true(false)
  }
  
  assert_true(c_operation_id is Some)
  match c_operation_id {
    Some(value) => assert_eq(value, "op-abcdef")
    _ => assert_true(false)
  }
  
  // 验证原始上下文不受影响
  let original_a_value = Context::get(service_a_ctx, service_a_key)
  let missing_b_value = Context::get(service_a_ctx, service_b_key)
  
  assert_true(original_a_value is Some)
  match original_a_value {
    Some(value) => assert_eq(value, "corr-12345")
    _ => assert_true(false)
  }
  
  assert_true(missing_b_value is None)
}

test "跨服务HTTP头传播一致性测试" {
  // 创建传播载体
  let carrier = TextMapCarrier::new()
  
  // 服务A：设置追踪头
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "user.id=user-12345,request.id=req-67890")
  TextMapCarrier::set(carrier, "x-correlation-id", "corr-12345")
  TextMapCarrier::set(carrier, "x-request-id", "req-67890")
  
  // 验证服务A设置的头部
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  let correlation_id = TextMapCarrier::get(carrier, "x-correlation-id")
  let request_id = TextMapCarrier::get(carrier, "x-request-id")
  
  assert_true(traceparent is Some)
  match traceparent {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    _ => assert_true(false)
  }
  
  assert_true(tracestate is Some)
  match tracestate {
    Some(value) => assert_eq(value, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    _ => assert_true(false)
  }
  
  // 服务B：添加自己的头部
  TextMapCarrier::set(carrier, "x-service-b-timestamp", "1735689600")
  TextMapCarrier::set(carrier, "x-service-b-operation", "process_data")
  
  // 验证服务B添加的头部
  let b_timestamp = TextMapCarrier::get(carrier, "x-service-b-timestamp")
  let b_operation = TextMapCarrier::get(carrier, "x-service-b-operation")
  
  assert_true(b_timestamp is Some)
  match b_timestamp {
    Some(value) => assert_eq(value, "1735689600")
    _ => assert_true(false)
  }
  
  // 验证原有头部仍然存在
  let original_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(original_traceparent is Some)
  match original_traceparent {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    _ => assert_true(false)
  }
  
  // 服务C：继续添加头部
  TextMapCarrier::set(carrier, "x-service-c-result", "success")
  TextMapCarrier::set(carrier, "x-service-c-duration", "150")
  
  // 验证所有头部都存在
  let c_result = TextMapCarrier::get(carrier, "x-service-c-result")
  let c_duration = TextMapCarrier::get(carrier, "x-service-c-duration")
  let still_correlation_id = TextMapCarrier::get(carrier, "x-correlation-id")
  
  assert_true(c_result is Some)
  match c_result {
    Some(value) => assert_eq(value, "success")
    _ => assert_true(false)
  }
  
  assert_true(still_correlation_id is Some)
  match still_correlation_id {
    Some(value) => assert_eq(value, "corr-12345")
    _ => assert_true(false)
  }
}

test "跨服务资源属性一致性测试" {
  // 创建基础资源
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("api-gateway")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_resource = Resource::with_attributes(base_resource, base_attrs)
  
  // 服务A：添加特定属性
  let service_a_attrs = [
    ("service.a.name", StringValue("user-service")),
    ("service.a.port", StringValue("8081")),
    ("service.a.dependency", StringValue("database"))
  ]
  let service_a_resource = Resource::with_attributes(base_resource, service_a_attrs)
  
  // 验证服务A继承了基础属性并添加了自己的属性
  let service_name = Resource::get_attribute(service_a_resource, "service.name")
  let service_a_name = Resource::get_attribute(service_a_resource, "service.a.name")
  let service_a_port = Resource::get_attribute(service_a_resource, "service.a.port")
  
  assert_true(service_name is Some)
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "api-gateway")
    _ => assert_true(false)
  }
  
  assert_true(service_a_name is Some)
  match service_a_name {
    Some(StringValue(value)) => assert_eq(value, "user-service")
    _ => assert_true(false)
  }
  
  // 服务B：添加不同的属性
  let service_b_attrs = [
    ("service.b.name", StringValue("order-service")),
    ("service.b.port", StringValue("8082")),
    ("service.b.dependency", StringValue("message-queue"))
  ]
  let service_b_resource = Resource::with_attributes(base_resource, service_b_attrs)
  
  // 验证服务B也继承了基础属性
  let b_service_name = Resource::get_attribute(service_b_resource, "service.name")
  let b_service_b_name = Resource::get_attribute(service_b_resource, "service.b.name")
  
  assert_true(b_service_name is Some)
  match b_service_name {
    Some(StringValue(value)) => assert_eq(value, "api-gateway")
    _ => assert_true(false)
  }
  
  assert_true(b_service_b_name is Some)
  match b_service_b_name {
    Some(StringValue(value)) => assert_eq(value, "order-service")
    _ => assert_true(false)
  }
  
  // 验证服务间的一致性
  let a_base_name = Resource::get_attribute(service_a_resource, "service.name")
  let b_base_name = Resource::get_attribute(service_b_resource, "service.name")
  
  assert_true(a_base_name is Some && b_base_name is Some)
  match (a_base_name, b_base_name) {
    (Some(StringValue(a_val)), Some(StringValue(b_val))) => assert_eq(a_val, b_val)
    _ => assert_true(false)
  }
}

test "跨服务时间序列和时序一致性测试" {
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 服务A：记录时间戳
  let service_a_timestamp = base_timestamp
  let service_a_duration = 1000000L // 1ms in nanoseconds
  
  // 服务B：相对时间戳
  let service_b_timestamp = base_timestamp + service_a_duration + 500000L // 1.5ms later
  let service_b_duration = 2000000L // 2ms
  
  // 服务C：最终时间戳
  let service_c_timestamp = service_b_timestamp + service_b_duration + 300000L // 2.3ms later
  let service_c_duration = 800000L // 0.8ms
  
  // 验证时间序列的合理性
  assert_true(service_a_timestamp > 0L)
  assert_true(service_b_timestamp > service_a_timestamp)
  assert_true(service_c_timestamp > service_b_timestamp)
  
  // 验证持续时间的合理性
  assert_true(service_a_duration > 0L)
  assert_true(service_b_duration > 0L)
  assert_true(service_c_duration > 0L)
  
  // 计算总持续时间
  let total_duration = service_c_timestamp - service_a_timestamp
  let expected_total = service_a_duration + 500000L + service_b_duration + 300000L + service_c_duration
  
  assert_true(total_duration >= expected_total)
  
  // 创建带时间戳的日志记录
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A processing started"),
    None,
    Some(service_a_timestamp),
    Some(service_a_timestamp + 100000L),
    Some("trace-12345"),
    Some("span-a-67890"),
    None
  )
  
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B processing started"),
    None,
    Some(service_b_timestamp),
    Some(service_b_timestamp + 200000L),
    Some("trace-12345"),
    Some("span-b-67890"),
    None
  )
  
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Service C processing completed"),
    None,
    Some(service_c_timestamp),
    Some(service_c_timestamp + 80000L),
    Some("trace-12345"),
    Some("span-c-67890"),
    None
  )
  
  // 验证日志记录的时间戳
  assert_true(LogRecord::timestamp(service_a_log) is Some)
  match LogRecord::timestamp(service_a_log) {
    Some(timestamp) => assert_eq(timestamp, service_a_timestamp)
    _ => assert_true(false)
  }
  
  assert_true(LogRecord::timestamp(service_b_log) is Some)
  match LogRecord::timestamp(service_b_log) {
    Some(timestamp) => assert_eq(timestamp, service_b_timestamp)
    _ => assert_true(false)
  }
  
  // 验证Trace ID一致性
  assert_true(LogRecord::trace_id(service_a_log) is Some)
  assert_true(LogRecord::trace_id(service_b_log) is Some)
  assert_true(LogRecord::trace_id(service_c_log) is Some)
  
  match (LogRecord::trace_id(service_a_log), LogRecord::trace_id(service_b_log), LogRecord::trace_id(service_c_log)) {
    (Some(trace_a), Some(trace_b), Some(trace_c)) => {
      assert_eq(trace_a, trace_b)
      assert_eq(trace_b, trace_c)
      assert_eq(trace_a, "trace-12345")
    }
    _ => assert_true(false)
  }
}