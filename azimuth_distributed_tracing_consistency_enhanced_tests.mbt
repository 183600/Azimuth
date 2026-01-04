// Azimuth Enhanced Distributed Tracing Consistency Tests
// 增强版分布式追踪一致性测试，确保跨服务追踪数据的一致性和完整性

test "分布式追踪上下文传播一致性测试" {
  // 创建根追踪上下文
  let root_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let root_span_id = "00f067aa0ba902b7"
  let root_context = azimuth::SpanContext::new(root_trace_id, root_span_id, true, "rojo=00f067aa0ba902b7")
  
  // 验证根上下文
  assert_eq(azimuth::SpanContext::trace_id(root_context), root_trace_id)
  assert_eq(azimuth::SpanContext::span_id(root_context), root_span_id)
  assert_true(azimuth::SpanContext::is_sampled(root_context))
  assert_true(azimuth::SpanContext::is_valid(root_context))
  
  // 模拟服务A中的span创建
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::SpanKind::Server, root_context)
  let service_a_context = azimuth::Span::span_context(service_a_span)
  
  // 验证服务A的追踪上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(service_a_context), root_trace_id)
  assert_not_eq(azimuth::SpanContext::span_id(service_a_context), root_span_id)
  assert_true(azimuth::SpanContext::is_sampled(service_a_context))
  
  // 模拟服务A调用服务B
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::SpanKind::Server, service_a_context)
  let service_b_context = azimuth::Span::span_context(service_b_span)
  
  // 验证服务B的追踪上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(service_b_context), root_trace_id)
  assert_not_eq(azimuth::SpanContext::span_id(service_b_context), service_a_context.span_id)
  assert_true(azimuth::SpanContext::is_sampled(service_b_context))
  
  // 模拟服务B调用服务C
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::SpanKind::Server, service_b_context)
  let service_c_context = azimuth::Span::span_context(service_c_span)
  
  // 验证服务C的追踪上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(service_c_context), root_trace_id)
  assert_not_eq(azimuth::SpanContext::span_id(service_c_context), service_b_context.span_id)
  assert_true(azimuth::SpanContext::is_sampled(service_c_context))
  
  // 验证所有span的追踪ID一致
  assert_eq(azimuth::SpanContext::trace_id(root_context), 
           azimuth::SpanContext::trace_id(service_a_context))
  assert_eq(azimuth::SpanContext::trace_id(service_a_context), 
           azimuth::SpanContext::trace_id(service_b_context))
  assert_eq(azimuth::SpanContext::trace_id(service_b_context), 
           azimuth::SpanContext::trace_id(service_c_context))
  
  // 验证所有span的span ID唯一
  assert_not_eq(azimuth::SpanContext::span_id(root_context), 
                azimuth::SpanContext::span_id(service_a_context))
  assert_not_eq(azimuth::SpanContext::span_id(service_a_context), 
                azimuth::SpanContext::span_id(service_b_context))
  assert_not_eq(azimuth::SpanContext::span_id(service_b_context), 
                azimuth::SpanContext::span_id(service_c_context))
}

test "跨服务 baggage 传播一致性测试" {
  // 创建根 baggage
  let root_baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(root_baggage, "user.id", "user-12345")
  let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // 验证 baggage 条目
  match azimuth::Baggage::get_entry(final_baggage, "user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  match azimuth::Baggage::get_entry(final_baggage, "request.id") {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  // 模拟服务间 baggage 传播
  let service_a_baggage = azimuth::Baggage::set_entry(final_baggage, "service.a.timestamp", "1640995200")
  let service_b_baggage = azimuth::Baggage::set_entry(service_a_baggage, "service.b.timestamp", "1640995260")
  let service_c_baggage = azimuth::Baggage::set_entry(service_b_baggage, "service.c.timestamp", "1640995320")
  
  // 验证 baggage 传播一致性
  match azimuth::Baggage::get_entry(service_c_baggage, "user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  match azimuth::Baggage::get_entry(service_c_baggage, "request.id") {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  match azimuth::Baggage::get_entry(service_c_baggage, "service.a.timestamp") {
    Some(value) => assert_eq(value, "1640995200")
    None => assert_true(false)
  }
  
  match azimuth::Baggage::get_entry(service_c_baggage, "service.b.timestamp") {
    Some(value) => assert_eq(value, "1640995260")
    None => assert_true(false)
  }
  
  match azimuth::Baggage::get_entry(service_c_baggage, "service.c.timestamp") {
    Some(value) => assert_eq(value, "1640995320")
    None => assert_true(false)
  }
}

test "分布式追踪采样决策一致性测试" {
  // 测试采样决策在跨服务调用中的一致性
  let sampled_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let sampled_span_id = "00f067aa0ba902b7"
  let sampled_context = azimuth::SpanContext::new(sampled_trace_id, sampled_span_id, true, "")
  
  // 验证采样决策传播
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::SpanKind::Server, sampled_context)
  let service_a_context = azimuth::Span::span_context(service_a_span)
  assert_true(azimuth::SpanContext::is_sampled(service_a_context))
  
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::SpanKind::Server, service_a_context)
  let service_b_context = azimuth::Span::span_context(service_b_span)
  assert_true(azimuth::SpanContext::is_sampled(service_b_context))
  
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::SpanKind::Server, service_b_context)
  let service_c_context = azimuth::Span::span_context(service_c_span)
  assert_true(azimuth::SpanContext::is_sampled(service_c_context))
  
  // 测试非采样决策传播
  let not_sampled_trace_id = "4bf92f3577b34da6a3ce929d0e0e4737"
  let not_sampled_span_id = "00f067aa0ba902b8"
  let not_sampled_context = azimuth::SpanContext::new(not_sampled_trace_id, not_sampled_span_id, false, "")
  
  let service_a_not_sampled_span = azimuth::Span::new("service-a-operation", azimuth::SpanKind::Server, not_sampled_context)
  let service_a_not_sampled_context = azimuth::Span::span_context(service_a_not_sampled_span)
  assert_false(azimuth::SpanContext::is_sampled(service_a_not_sampled_context))
  
  let service_b_not_sampled_span = azimuth::Span::new("service-b-operation", azimuth::SpanKind::Server, service_a_not_sampled_context)
  let service_b_not_sampled_context = azimuth::Span::span_context(service_b_not_sampled_span)
  assert_false(azimuth::SpanContext::is_sampled(service_b_not_sampled_context))
  
  let service_c_not_sampled_span = azimuth::Span::new("service-c-operation", azimuth::SpanKind::Server, service_b_not_sampled_context)
  let service_c_not_sampled_context = azimuth::Span::span_context(service_c_not_sampled_span)
  assert_false(azimuth::SpanContext::is_sampled(service_c_not_sampled_context))
}

test "分布式追踪时间戳一致性测试" {
  // 测试分布式追踪中时间戳的一致性和顺序
  let base_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  // 创建根 span
  let root_context = azimuth::SpanContext::new("trace-1", "span-1", true, "")
  let root_span = azimuth::Span::new("root-operation", azimuth::SpanKind::Server, root_context)
  
  // 设置根 span 时间戳
  azimuth::Span::set_start_time(root_span, base_time)
  azimuth::Span::set_end_time(root_span, base_time + 1000L)
  
  // 创建子 span
  let child_context = azimuth::Span::span_context(root_span)
  let child_span = azimuth::Span::new("child-operation", azimuth::SpanKind::Client, child_context)
  
  // 设置子 span 时间戳
  azimuth::Span::set_start_time(child_span, base_time + 100L)
  azimuth::Span::set_end_time(child_span, base_time + 800L)
  
  // 验证时间戳顺序
  assert_true(azimuth::Span::start_time(root_span) <= azimuth::Span::start_time(child_span))
  assert_true(azimuth::Span::end_time(child_span) <= azimuth::Span::end_time(root_span))
  
  // 验证持续时间计算
  let root_duration = azimuth::Span::end_time(root_span) - azimuth::Span::start_time(root_span)
  let child_duration = azimuth::Span::end_time(child_span) - azimuth::Span::start_time(child_span)
  
  assert_eq(root_duration, 1000L)
  assert_eq(child_duration, 700L)
  assert_true(child_duration <= root_duration)
  
  // 创建孙 span
  let grandchild_context = azimuth::Span::span_context(child_span)
  let grandchild_span = azimuth::Span::new("grandchild-operation", azimuth::SpanKind::Internal, grandchild_context)
  
  // 设置孙 span 时间戳
  azimuth::Span::set_start_time(grandchild_span, base_time + 200L)
  azimuth::Span::set_end_time(grandchild_span, base_time + 600L)
  
  // 验证孙 span 时间戳顺序
  assert_true(azimuth::Span::start_time(child_span) <= azimuth::Span::start_time(grandchild_span))
  assert_true(azimuth::Span::end_time(grandchild_span) <= azimuth::Span::end_time(child_span))
  
  // 验证孙 span 持续时间
  let grandchild_duration = azimuth::Span::end_time(grandchild_span) - azimuth::Span::start_time(grandchild_span)
  assert_eq(grandchild_duration, 400L)
  assert_true(grandchild_duration <= child_duration)
}

test "分布式追踪属性传播一致性测试" {
  // 测试分布式追踪中属性的传播和一致性
  let root_context = azimuth::SpanContext::new("trace-1", "span-1", true, "")
  let root_span = azimuth::Span::new("root-operation", azimuth::SpanKind::Server, root_context)
  
  // 设置根 span 属性
  let root_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(root_attrs, "service.name", azimuth::AttributeValue::StringValue("root-service"))
  azimuth::Attributes::set(root_attrs, "service.version", azimuth::AttributeValue::StringValue("1.0.0"))
  azimuth::Attributes::set(root_attrs, "operation.type", azimuth::AttributeValue::StringValue("http"))
  azimuth::Span::set_attributes(root_span, root_attrs)
  
  // 创建子 span 并继承属性
  let child_context = azimuth::Span::span_context(root_span)
  let child_span = azimuth::Span::new("child-operation", azimuth::SpanKind::Client, child_context)
  
  // 设置子 span 特有属性
  let child_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(child_attrs, "service.name", azimuth::AttributeValue::StringValue("child-service"))
  azimuth::Attributes::set(child_attrs, "service.version", azimuth::AttributeValue::StringValue("2.0.0"))
  azimuth::Attributes::set(child_attrs, "target.service", azimuth::AttributeValue::StringValue("external-api"))
  azimuth::Span::set_attributes(child_span, child_attrs)
  
  // 验证属性传播
  let retrieved_root_attrs = azimuth::Span::attributes(root_span)
  match azimuth::Attributes::get(retrieved_root_attrs, "service.name") {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "root-service")
    _ => assert_true(false)
  }
  
  match azimuth::Attributes::get(retrieved_root_attrs, "service.version") {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  let retrieved_child_attrs = azimuth::Span::attributes(child_span)
  match azimuth::Attributes::get(retrieved_child_attrs, "service.name") {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "child-service")
    _ => assert_true(false)
  }
  
  match azimuth::Attributes::get(retrieved_child_attrs, "service.version") {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "2.0.0")
    _ => assert_true(false)
  }
  
  match azimuth::Attributes::get(retrieved_child_attrs, "target.service") {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "external-api")
    _ => assert_true(false)
  }
}

test "分布式追踪错误传播一致性测试" {
  // 测试分布式追踪中错误信息的传播和一致性
  let root_context = azimuth::SpanContext::new("trace-1", "span-1", true, "")
  let root_span = azimuth::Span::new("root-operation", azimuth::SpanKind::Server, root_context)
  
  // 模拟正常操作
  azimuth::Span::set_status(root_span, azimuth::StatusCode::Ok, None)
  
  // 创建子 span 并模拟错误
  let child_context = azimuth::Span::span_context(root_span)
  let child_span = azimuth::Span::new("child-operation", azimuth::SpanKind::Client, child_context)
  
  // 设置错误状态
  azimuth::Span::set_status(child_span, azimuth::StatusCode::Error, Some("Connection timeout"))
  azimuth::Span::add_event(child_span, "exception", Some([
    ("exception.type", azimuth::AttributeValue::StringValue("TimeoutException")),
    ("exception.message", azimuth::AttributeValue::StringValue("Connection timeout after 30 seconds"))
  ]))
  
  // 验证错误状态
  assert_eq(azimuth::Span::status(child_span), azimuth::StatusCode::Error)
  
  // 创建另一个子 span 并模拟不同错误
  let sibling_context = azimuth::Span::span_context(root_span)
  let sibling_span = azimuth::Span::new("sibling-operation", azimuth::SpanKind::Server, sibling_context)
  
  // 设置不同错误状态
  azimuth::Span::set_status(sibling_span, azimuth::StatusCode::Error, Some("Invalid request"))
  azimuth::Span::add_event(sibling_span, "exception", Some([
    ("exception.type", azimuth::AttributeValue::StringValue("ValidationException")),
    ("exception.message", azimuth::AttributeValue::StringValue("Invalid request parameters"))
  ]))
  
  // 验证不同错误状态
  assert_eq(azimuth::Span::status(sibling_span), azimuth::StatusCode::Error)
  
  // 验证根 span 状态仍然正常
  assert_eq(azimuth::Span::status(root_span), azimuth::StatusCode::Ok)
  
  // 验证错误事件记录
  let child_events = azimuth::Span::events(child_span)
  assert_true(child_events.length() > 0)
  
  let sibling_events = azimuth::Span::events(sibling_span)
  assert_true(sibling_events.length() > 0)
}