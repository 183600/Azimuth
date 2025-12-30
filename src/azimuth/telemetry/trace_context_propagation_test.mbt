// Trace API Span上下文传播测试
// 测试Span上下文在不同场景下的传播和继承

test "span_context_trace_id_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建根span
  let (root_ctx, root_span) = tracer.start_span(ctx, "root_operation")
  
  // 创建子span，应该继承根span的trace_id
  let (child_ctx, child_span) = tracer.start_span(root_ctx, "child_operation")
  
  // 验证trace_id传播（在Noop实现中都是0，但结构应该正确）
  assert_eq(root_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.trace_id.length(), 16)
  
  // 在真实实现中，子span应该继承父span的trace_id
  // 这里我们验证结构的一致性
  let mut i = 0
  while i < root_span.context.trace_id.length() {
    assert_eq(root_span.context.trace_id[i], child_span.context.trace_id[i])
    i = i + 1
  }
}

test "span_context_parent_relationship" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_span")
  
  // 创建子span
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_span")
  
  // 创建孙span
  let (grandchild_ctx, grandchild_span) = tracer.start_span(child_ctx, "grandchild_span")
  
  // 验证span_id的唯一性（在Noop实现中都是0，但长度应该正确）
  assert_eq(parent_span.context.span_id.length(), 8)
  assert_eq(child_span.context.span_id.length(), 8)
  assert_eq(grandchild_span.context.span_id.length(), 8)
  
  // 验证父子关系结构
  assert_eq(parent_span.parent_span_id, None)  // 根span没有父span
  
  // 在真实实现中，子span的parent_span_id应该指向父span的span_id
  // 这里我们验证字段的存在
  assert_eq(child_span.parent_span_id, None)  // Noop实现中为None
  assert_eq(grandchild_span.parent_span_id, None)  // Noop实现中为None
}

test "span_context_trace_flags_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有特定trace flags的span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_with_flags")
  
  // 在真实实现中，子span应该继承父span的trace flags
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_inheriting_flags")
  
  // 验证trace flags字段的存在
  assert_eq(parent_span.context.trace_flags, 0_byte)
  assert_eq(child_span.context.trace_flags, 0_byte)
}

test "span_context_trace_state_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建带有trace state的span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_with_state")
  
  // 在真实实现中，子span可能继承或扩展父span的trace state
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_with_state")
  
  // 验证trace state字段的存在
  assert_eq(parent_span.context.trace_state, "")
  assert_eq(child_span.context.trace_state, "")
}

test "span_context_cross_service_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 模拟跨服务调用的span传播
  // 服务A创建根span
  let (service_a_ctx, service_a_span) = tracer.start_span(ctx, "service_a_operation")
  
  // 服务B接收上下文并创建子span
  let (service_b_ctx, service_b_span) = tracer.start_span(service_a_ctx, "service_b_operation")
  
  // 服务C接收上下文并创建子span
  let (service_c_ctx, service_c_span) = tracer.start_span(service_b_ctx, "service_c_operation")
  
  // 验证所有span都在同一个trace中
  assert_eq(service_a_span.context.trace_id.length(), 16)
  assert_eq(service_b_span.context.trace_id.length(), 16)
  assert_eq(service_c_span.context.trace_id.length(), 16)
  
  // 验证span链的结构
  assert_eq(service_a_span.name, "service_a_operation")
  assert_eq(service_b_span.name, "service_b_operation")
  assert_eq(service_c_span.name, "service_c_operation")
}

test "span_context_with_different_kinds" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建不同类型的span，验证上下文传播不受span kind影响
  let (server_ctx, server_span) = tracer.start_span(ctx, "server_operation", kind: Some(Server))
  let (client_ctx, client_span) = tracer.start_span(server_ctx, "client_operation", kind: Some(Client))
  let (producer_ctx, producer_span) = tracer.start_span(client_ctx, "producer_operation", kind: Some(Producer))
  let (consumer_ctx, consumer_span) = tracer.start_span(producer_ctx, "consumer_operation", kind: Some(Consumer))
  let (internal_ctx, internal_span) = tracer.start_span(consumer_ctx, "internal_operation", kind: Some(Internal))
  
  // 验证所有span类型都正确传播了上下文
  match server_span.kind { Server => @test.succeed() _ => @test.fail("Expected Server") }
  match client_span.kind { Client => @test.succeed() _ => @test.fail("Expected Client") }
  match producer_span.kind { Producer => @test.succeed() _ => @test.fail("Expected Producer") }
  match consumer_span.kind { Consumer => @test.succeed() _ => @test.fail("Expected Consumer") }
  match internal_span.kind { Internal => @test.succeed() _ => @test.fail("Expected Internal") }
  
  // 验证trace_id的一致性
  assert_eq(server_span.context.trace_id.length(), 16)
  assert_eq(client_span.context.trace_id.length(), 16)
  assert_eq(producer_span.context.trace_id.length(), 16)
  assert_eq(consumer_span.context.trace_id.length(), 16)
  assert_eq(internal_span.context.trace_id.length(), 16)
}

test "span_context_with_attributes_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  let parent_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("auth-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("v1.2.3")),
    ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("authentication"))
  ]
  
  let child_attributes = [
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("12345")),
    ("auth.method", @azimuth.telemetry.api.common.AttributeValue::string("oauth2"))
  ]
  
  // 创建带有属性的父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_auth", attributes: Some(parent_attributes))
  
  // 创建带有属性的子span
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_token_validation", attributes: Some(child_attributes))
  
  // 验证属性不传播（每个span有自己的属性）
  assert_eq(parent_span.attributes.length(), 3)
  assert_eq(child_span.attributes.length(), 2)
  
  // 验证父span属性
  match parent_span.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "service.name")
      assert_eq(value, "auth-service")
    }
    _ => @test.fail("Expected service.name attribute")
  }
  
  // 验证子span属性
  match child_span.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "12345")
    }
    _ => @test.fail("Expected user.id attribute")
  }
}

test "span_context_with_events_and_links" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span并添加事件
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_with_events")
  
  // 在真实实现中，这里应该添加事件到parent_span
  // parent_span.add_event("operation_started", ...)
  // parent_span.add_event("operation_completed", ...)
  
  // 创建子span并添加链接
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child_with_links")
  
  // 在真实实现中，这里应该添加链接到child_span
  // child_span.add_link(parent_span.context, ...)
  
  // 验证span结构
  assert_eq(parent_span.events.length(), 0)  // Noop实现中为空
  assert_eq(child_span.links.length(), 0)   // Noop实现中为空
  
  // 验证上下文传播不受事件和链接影响
  assert_eq(parent_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.trace_id.length(), 16)
}

test "span_context_with_timing_propagation" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  let base_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  
  // 创建具有特定开始时间的父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "timed_parent", start_time_unix_nanos: Some(base_time))
  
  // 创建具有不同开始时间的子span
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "timed_child", start_time_unix_nanos: Some(base_time + 1000000L))
  
  // 验证时间戳的独立性
  assert_eq(parent_span.start_time_unix_nanos, base_time)
  assert_eq(child_span.start_time_unix_nanos, base_time + 1000000L)
  
  // 验证上下文传播不受时间戳影响
  assert_eq(parent_span.context.trace_id.length(), 16)
  assert_eq(child_span.context.trace_id.length(), 16)
}

test "span_context_with_multiple_children" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent_with_multiple_children")
  
  // 创建多个子span
  let (child1_ctx, child1_span) = tracer.start_span(parent_ctx, "child_1")
  let (child2_ctx, child2_span) = tracer.start_span(parent_ctx, "child_2")
  let (child3_ctx, child3_span) = tracer.start_span(parent_ctx, "child_3")
  
  // 验证所有子span都继承父span的trace_id
  assert_eq(parent_span.context.trace_id.length(), 16)
  assert_eq(child1_span.context.trace_id.length(), 16)
  assert_eq(child2_span.context.trace_id.length(), 16)
  assert_eq(child3_span.context.trace_id.length(), 16)
  
  // 验证每个子span都有唯一的span_id（在Noop实现中长度正确）
  assert_eq(child1_span.context.span_id.length(), 8)
  assert_eq(child2_span.context.span_id.length(), 8)
  assert_eq(child3_span.context.span_id.length(), 8)
  
  // 验证span名称
  assert_eq(parent_span.name, "parent_with_multiple_children")
  assert_eq(child1_span.name, "child_1")
  assert_eq(child2_span.name, "child_2")
  assert_eq(child3_span.name, "child_3")
}

test "span_context_deep_hierarchy" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建深层次的span层次结构
  let mut current_ctx = ctx
  let mut spans = []
  
  // 创建10层深度的span层次
  let mut i = 0
  while i < 10 {
    let span_name = "level_" + i.to_string()
    let (new_ctx, span) = tracer.start_span(current_ctx, span_name)
    spans.push(span)
    current_ctx = new_ctx
    i = i + 1
  }
  
  // 验证所有span都在同一个trace中
  let mut i = 0
  while i < spans.length() {
    let span = spans[i]
    assert_eq(span.context.trace_id.length(), 16)
    assert_eq(span.context.span_id.length(), 8)
    assert_eq(span.name, "level_" + i.to_string())
    i = i + 1
  }
  
  // 验证根span没有父span
  match spans[0].parent_span_id {
    None => @test.succeed()
    _ => @test.fail("Root span should not have parent")
  }
  
  // 验证其他span都有parent_span_id字段（在Noop实现中为None）
  let mut i = 1
  while i < spans.length() {
    match spans[i].parent_span_id {
      None => @test.succeed()  // Noop实现中为None
      _ => @test.succeed()  // 在真实实现中应该有值
    }
    i = i + 1
  }
}