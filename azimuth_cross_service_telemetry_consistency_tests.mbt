// Azimuth 跨服务遥测一致性测试
// 测试跨多个微服务环境下的遥测数据一致性和传播可靠性

test "跨服务追踪ID一致性测试" {
  // 创建初始追踪上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let initial_span_id = "00f067aa0ba902b7"
  let root_span_ctx = SpanContext::new(trace_id, initial_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 服务A创建子Span
  let service_a_span_id = "b7ad6b7169203331"
  let service_a_span_ctx = SpanContext::new(trace_id, service_a_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 验证服务A的追踪ID与根追踪ID一致
  assert_eq(SpanContext::trace_id(service_a_span_ctx), trace_id)
  assert_ne(SpanContext::span_id(service_a_span_ctx), initial_span_id)
  assert_true(SpanContext::is_sampled(service_a_span_ctx))
  
  // 服务B创建子Span
  let service_b_span_id = "c8be7c827a314442"
  let service_b_span_ctx = SpanContext::new(trace_id, service_b_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 验证服务B的追踪ID与根追踪ID一致
  assert_eq(SpanContext::trace_id(service_b_span_ctx), trace_id)
  assert_ne(SpanContext::span_id(service_b_span_ctx), initial_span_id)
  assert_ne(SpanContext::span_id(service_b_span_ctx), service_a_span_id)
  assert_true(SpanContext::is_sampled(service_b_span_ctx))
  
  // 服务C创建子Span
  let service_c_span_id = "d9cf8d938b425553"
  let service_c_span_ctx = SpanContext::new(trace_id, service_c_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 验证服务C的追踪ID与根追踪ID一致
  assert_eq(SpanContext::trace_id(service_c_span_ctx), trace_id)
  assert_ne(SpanContext::span_id(service_c_span_ctx), initial_span_id)
  assert_ne(SpanContext::span_id(service_c_span_ctx), service_a_span_id)
  assert_ne(SpanContext::span_id(service_c_span_ctx), service_b_span_id)
  assert_true(SpanContext::is_sampled(service_c_span_ctx))
  
  // 验证所有服务的追踪状态一致性
  assert_eq(SpanContext::trace_state(root_span_ctx), "rojo=00f067aa0ba902b7")
  assert_eq(SpanContext::trace_state(service_a_span_ctx), "rojo=00f067aa0ba902b7")
  assert_eq(SpanContext::trace_state(service_b_span_ctx), "rojo=00f067aa0ba902b7")
  assert_eq(SpanContext::trace_state(service_c_span_ctx), "rojo=00f067aa0ba902b7")
}

test "跨服务属性传播一致性测试" {
  // 创建根服务属性
  let root_attrs = Attributes::new()
  Attributes::set(root_attrs, "user.id", StringValue("user-12345"))
  Attributes::set(root_attrs, "request.id", StringValue("req-abcdef"))
  Attributes::set(root_attrs, "service.version", StringValue("1.0.0"))
  
  // 服务A添加特定属性
  Attributes::set(root_attrs, "service.a.name", StringValue("user-service"))
  Attributes::set(root_attrs, "service.a.operation", StringValue("get-user"))
  Attributes::set(root_attrs, "service.a.duration", IntValue(150))
  
  // 服务B添加特定属性
  Attributes::set(root_attrs, "service.b.name", StringValue("order-service"))
  Attributes::set(root_attrs, "service.b.operation", StringValue("create-order"))
  Attributes::set(root_attrs, "service.b.duration", IntValue(200))
  
  // 服务C添加特定属性
  Attributes::set(root_attrs, "service.c.name", StringValue("payment-service"))
  Attributes::set(root_attrs, "service.c.operation", StringValue("process-payment"))
  Attributes::set(root_attrs, "service.c.duration", IntValue(300))
  
  // 验证根属性在所有服务中保持一致
  let user_id = Attributes::get(root_attrs, "user.id")
  match user_id {
    Some(StringValue(id)) => assert_eq(id, "user-12345")
    _ => assert_true(false)
  }
  
  let request_id = Attributes::get(root_attrs, "request.id")
  match request_id {
    Some(StringValue(id)) => assert_eq(id, "req-abcdef")
    _ => assert_true(false)
  }
  
  let service_version = Attributes::get(root_attrs, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // 验证各服务特定属性
  let service_a_name = Attributes::get(root_attrs, "service.a.name")
  match service_a_name {
    Some(StringValue(name)) => assert_eq(name, "user-service")
    _ => assert_true(false)
  }
  
  let service_b_name = Attributes::get(root_attrs, "service.b.name")
  match service_b_name {
    Some(StringValue(name)) => assert_eq(name, "order-service")
    _ => assert_true(false)
  }
  
  let service_c_name = Attributes::get(root_attrs, "service.c.name")
  match service_c_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
}

test "跨服务 baggage 传播一致性测试" {
  // 创建根 baggage
  let root_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(root_baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-001")
  
  // 服务A添加特定 baggage 条目
  let service_a_baggage = Baggage::set_entry(baggage_with_tenant, "service.a.region", "us-east-1")
  let service_a_baggage_final = Baggage::set_entry(service_a_baggage, "service.a.instance", "instance-a-1")
  
  // 服务B添加特定 baggage 条目
  let service_b_baggage = Baggage::set_entry(service_a_baggage_final, "service.b.region", "us-west-2")
  let service_b_baggage_final = Baggage::set_entry(service_b_baggage, "service.b.instance", "instance-b-1")
  
  // 服务C添加特定 baggage 条目
  let service_c_baggage = Baggage::set_entry(service_b_baggage_final, "service.c.region", "eu-west-1")
  let service_c_baggage_final = Baggage::set_entry(service_c_baggage, "service.c.instance", "instance-c-1")
  
  // 验证根 baggage 条目在所有服务中保持一致
  let user_id = Baggage::get_entry(service_c_baggage_final, "user.id")
  match user_id {
    Some(id) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(service_c_baggage_final, "session.id")
  match session_id {
    Some(id) => assert_eq(id, "session-abcdef")
    None => assert_true(false)
  }
  
  let tenant_id = Baggage::get_entry(service_c_baggage_final, "tenant.id")
  match tenant_id {
    Some(id) => assert_eq(id, "tenant-001")
    None => assert_true(false)
  }
  
  // 验证各服务特定 baggage 条目
  let service_a_region = Baggage::get_entry(service_c_baggage_final, "service.a.region")
  match service_a_region {
    Some(region) => assert_eq(region, "us-east-1")
    None => assert_true(false)
  }
  
  let service_b_region = Baggage::get_entry(service_c_baggage_final, "service.b.region")
  match service_b_region {
    Some(region) => assert_eq(region, "us-west-2")
    None => assert_true(false)
  }
  
  let service_c_region = Baggage::get_entry(service_c_baggage_final, "service.c.region")
  match service_c_region {
    Some(region) => assert_eq(region, "eu-west-1")
    None => assert_true(false)
  }
}

test "跨服务采样决策一致性测试" {
  // 创建根采样决策
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let root_span_id = "00f067aa0ba902b7"
  
  // 测试采样决策为 true 的情况
  let sampled_root_ctx = SpanContext::new(trace_id, root_span_id, true, "")
  
  // 服务A继承采样决策
  let service_a_span_id = "b7ad6b7169203331"
  let service_a_ctx = SpanContext::new(trace_id, service_a_span_id, true, "")
  assert_true(SpanContext::is_sampled(service_a_ctx))
  
  // 服务B继承采样决策
  let service_b_span_id = "c8be7c827a314442"
  let service_b_ctx = SpanContext::new(trace_id, service_b_span_id, true, "")
  assert_true(SpanContext::is_sampled(service_b_ctx))
  
  // 服务C继承采样决策
  let service_c_span_id = "d9cf8d938b425553"
  let service_c_ctx = SpanContext::new(trace_id, service_c_span_id, true, "")
  assert_true(SpanContext::is_sampled(service_c_ctx))
  
  // 测试采样决策为 false 的情况
  let not_sampled_root_ctx = SpanContext::new(trace_id, root_span_id, false, "")
  
  // 服务A继承采样决策
  let service_a_not_sampled_ctx = SpanContext::new(trace_id, service_a_span_id, false, "")
  assert_false(SpanContext::is_sampled(service_a_not_sampled_ctx))
  
  // 服务B继承采样决策
  let service_b_not_sampled_ctx = SpanContext::new(trace_id, service_b_span_id, false, "")
  assert_false(SpanContext::is_sampled(service_b_not_sampled_ctx))
  
  // 服务C继承采样决策
  let service_c_not_sampled_ctx = SpanContext::new(trace_id, service_c_span_id, false, "")
  assert_false(SpanContext::is_sampled(service_c_not_sampled_ctx))
}

test "跨服务时间同步一致性测试" {
  // 模拟不同服务的时间戳
  let root_start_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  let service_a_start_time = root_start_time + 50L // 服务A在根开始50ms后开始
  let service_b_start_time = service_a_start_time + 100L // 服务B在服务A开始100ms后开始
  let service_c_start_time = service_b_start_time + 150L // 服务C在服务B开始150ms后开始
  
  // 模拟各服务的处理时间
  let service_a_duration = 150L
  let service_b_duration = 200L
  let service_c_duration = 300L
  
  // 计算各服务的结束时间
  let service_a_end_time = service_a_start_time + service_a_duration
  let service_b_end_time = service_b_start_time + service_b_duration
  let service_c_end_time = service_c_start_time + service_c_duration
  
  // 验证时间顺序一致性
  assert_true(service_a_start_time >= root_start_time)
  assert_true(service_b_start_time >= service_a_start_time)
  assert_true(service_c_start_time >= service_b_start_time)
  
  assert_true(service_a_end_time >= service_a_start_time)
  assert_true(service_b_end_time >= service_b_start_time)
  assert_true(service_c_end_time >= service_c_start_time)
  
  // 验证时间跨度合理性
  assert_eq(service_a_end_time - service_a_start_time, service_a_duration)
  assert_eq(service_b_end_time - service_b_start_time, service_b_duration)
  assert_eq(service_c_end_time - service_c_start_time, service_c_duration)
  
  // 验证整体时间跨度
  let total_duration = service_c_end_time - root_start_time
  let expected_total_duration = 50L + 100L + 150L + 300L // 累计所有间隔和最后一个服务的持续时间
  assert_eq(total_duration, expected_total_duration)
}

test "跨服务错误传播一致性测试" {
  // 创建根服务错误
  let root_error_attrs = Attributes::new()
  Attributes::set(root_error_attrs, "error.type", StringValue("ValidationError"))
  Attributes::set(root_error_attrs, "error.message", StringValue("Invalid input parameter"))
  Attributes::set(root_error_attrs, "error.stack", StringValue("at validateInput (input.js:42)"))
  
  // 服务A传播错误并添加上下文
  Attributes::set(root_error_attrs, "service.a.error.context", StringValue("User validation failed"))
  Attributes::set(root_error_attrs, "service.a.error.code", IntValue(400))
  
  // 服务B传播错误并添加上下文
  Attributes::set(root_error_attrs, "service.b.error.context", StringValue("Order creation failed due to user validation error"))
  Attributes::set(root_error_attrs, "service.b.error.code", IntValue(400))
  
  // 服务C传播错误并添加上下文
  Attributes::set(root_error_attrs, "service.c.error.context", StringValue("Payment processing failed due to order creation error"))
  Attributes::set(root_error_attrs, "service.c.error.code", IntValue(400))
  
  // 验证根错误信息在所有服务中保持一致
  let error_type = Attributes::get(root_error_attrs, "error.type")
  match error_type {
    Some(StringValue(type)) => assert_eq(type, "ValidationError")
    _ => assert_true(false)
  }
  
  let error_message = Attributes::get(root_error_attrs, "error.message")
  match error_message {
    Some(StringValue(message)) => assert_eq(message, "Invalid input parameter")
    _ => assert_true(false)
  }
  
  let error_stack = Attributes::get(root_error_attrs, "error.stack")
  match error_stack {
    Some(StringValue(stack)) => assert_eq(stack, "at validateInput (input.js:42)")
    _ => assert_true(false)
  }
  
  // 验证各服务特定错误上下文
  let service_a_context = Attributes::get(root_error_attrs, "service.a.error.context")
  match service_a_context {
    Some(StringValue(context)) => assert_eq(context, "User validation failed")
    _ => assert_true(false)
  }
  
  let service_b_context = Attributes::get(root_error_attrs, "service.b.error.context")
  match service_b_context {
    Some(StringValue(context)) => assert_eq(context, "Order creation failed due to user validation error")
    _ => assert_true(false)
  }
  
  let service_c_context = Attributes::get(root_error_attrs, "service.c.error.context")
  match service_c_context {
    Some(StringValue(context)) => assert_eq(context, "Payment processing failed due to order creation error")
    _ => assert_true(false)
  }
  
  // 验证所有错误代码一致
  let service_a_code = Attributes::get(root_error_attrs, "service.a.error.code")
  match service_a_code {
    Some(IntValue(code)) => assert_eq(code, 400)
    _ => assert_true(false)
  }
  
  let service_b_code = Attributes::get(root_error_attrs, "service.b.error.code")
  match service_b_code {
    Some(IntValue(code)) => assert_eq(code, 400)
    _ => assert_true(false)
  }
  
  let service_c_code = Attributes::get(root_error_attrs, "service.c.error.code")
  match service_c_code {
    Some(IntValue(code)) => assert_eq(code, 400)
    _ => assert_true(false)
  }
}