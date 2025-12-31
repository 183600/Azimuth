// Azimuth Telemetry - Trace API Integration Test
// 遥测链路追踪API集成测试

test "trace_api_basic_functionality" {
  // 测试TracerProvider基本功能
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer", Some("1.0.0"))
  
  // 测试创建span
  let ctx = context::Context::empty()
  let (new_ctx, span) = tracer.start_span(ctx, "test-span", Some(trace::Internal), Some([("test-key", common::AttributeValue::string("test-value"))]))
  
  // 验证span基本属性
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, trace::Internal)
  assert_eq(span.attributes.length(), 1)
  assert_eq(span.attributes[0].0, "test-key")
  match span.attributes[0].1 {
    common::StringValue(v) => assert_eq(v, "test-value")
    _ => assert(false, "Expected string attribute value")
  }
  
  // 验证span context
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  assert_eq(span.context.trace_flags, 0_byte)
  assert_eq(span.context.trace_state, "")
}

test "trace_api_span_kinds" {
  // 测试所有SpanKind类型
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  let ctx = context::Context::empty()
  
  // 测试Internal span
  let (_, internal_span) = tracer.start_span(ctx, "internal-span", Some(trace::Internal))
  assert_eq(internal_span.kind, trace::Internal)
  
  // 测试Server span
  let (_, server_span) = tracer.start_span(ctx, "server-span", Some(trace::Server))
  assert_eq(server_span.kind, trace::Server)
  
  // 测试Client span
  let (_, client_span) = tracer.start_span(ctx, "client-span", Some(trace::Client))
  assert_eq(client_span.kind, trace::Client)
  
  // 测试Producer span
  let (_, producer_span) = tracer.start_span(ctx, "producer-span", Some(trace::Producer))
  assert_eq(producer_span.kind, trace::Producer)
  
  // 测试Consumer span
  let (_, consumer_span) = tracer.start_span(ctx, "consumer-span", Some(trace::Consumer))
  assert_eq(consumer_span.kind, trace::Consumer)
}

test "trace_api_span_lifecycle" {
  // 测试span生命周期
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  let ctx = context::Context::empty()
  
  // 创建span
  let (_, mut span) = tracer.start_span(ctx, "lifecycle-test", Some(trace::Server))
  
  // 验证初始状态
  assert_eq(span.status, trace::Unset)
  assert_eq(span.end_time_unix_nanos, None)
  assert_eq(span.status_description, None)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  assert_eq(span.parent_span_id, None)
  
  // 在实际实现中，这里应该测试span的状态变更
  // 但由于Noop实现的限制，我们主要验证结构
}

test "trace_api_context_operations" {
  // 测试上下文操作
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  
  let key = context::create_key("trace-key")
  let ctx = context::Context::empty()
  let enriched_ctx = ctx.with_value(key, "trace-value")
  
  // 测试在上下文中创建span
  let (new_ctx, span) = tracer.start_span(enriched_ctx, "context-test")
  
  // 验证上下文保持
  let retrieved_value = new_ctx.get(key)
  match retrieved_value {
    Some(v) => assert_eq(v, "trace-value")
    None => assert(false, "Expected value in context")
  }
}

test "trace_api_span_attributes" {
  // 测试span属性处理
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  let ctx = context::Context::empty()
  
  // 测试不同类型的属性值
  let attributes = [
    ("string-attr", common::AttributeValue::string("string-value")),
    ("int-attr", common::AttributeValue::int(42L)),
    ("float-attr", common::AttributeValue::float(3.14)),
    ("bool-attr", common::AttributeValue::bool(true)),
    ("array-string-attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("array-int-attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("array-float-attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array-bool-attr", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  let (_, span) = tracer.start_span(ctx, "attributes-test", Some(trace::Internal), Some(attributes))
  
  // 验证属性数量和类型
  assert_eq(span.attributes.length(), 8)
  
  // 验证字符串属性
  match span.attributes[0].1 {
    common::StringValue(v) => assert_eq(v, "string-value")
    _ => assert(false, "Expected string value")
  }
  
  // 验证整数属性
  match span.attributes[1].1 {
    common::IntValue(v) => assert_eq(v, 42L)
    _ => assert(false, "Expected int value")
  }
  
  // 验证浮点属性
  match span.attributes[2].1 {
    common::FloatValue(v) => assert_eq(v, 3.14)
    _ => assert(false, "Expected float value")
  }
  
  // 验证布尔属性
  match span.attributes[3].1 {
    common::BoolValue(v) => assert_eq(v, true)
    _ => assert(false, "Expected bool value")
  }
}

test "trace_api_span_relationships" {
  // 测试span关系（父子关系）
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  let ctx = context::Context::empty()
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent-span", Some(trace::Server))
  
  // 在实际实现中，子span应该从父span的上下文创建
  // 这里我们模拟父子关系
  let (_, child_span) = tracer.start_span(parent_ctx, "child-span", Some(trace::Internal))
  
  // 验证span基本属性
  assert_eq(parent_span.name, "parent-span")
  assert_eq(parent_span.kind, trace::Server)
  assert_eq(child_span.name, "child-span")
  assert_eq(child_span.kind, trace::Internal)
}

test "trace_api_error_conditions" {
  // 测试错误条件和边界情况
  let provider = trace::NoopTracerProvider::{}
  let tracer = provider.get_tracer("test-tracer")
  let ctx = context::Context::empty()
  
  // 测试空名称
  let (_, empty_name_span) = tracer.start_span(ctx, "", Some(trace::Internal))
  assert_eq(empty_name_span.name, "")
  
  // 测试长名称
  let long_name = "a".repeat(1000)
  let (_, long_name_span) = tracer.start_span(ctx, long_name, Some(trace::Internal))
  assert_eq(long_name_span.name.length(), 1000)
  
  // 测试空属性数组
  let (_, no_attrs_span) = tracer.start_span(ctx, "no-attrs", Some(trace::Internal), Some([]))
  assert_eq(no_attrs_span.attributes.length(), 0)
  
  // 测试None属性（应该使用默认空数组）
  let (_, none_attrs_span) = tracer.start_span(ctx, "none-attrs", Some(trace::Internal), None)
  assert_eq(none_attrs_span.attributes.length(), 0)
}