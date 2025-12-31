// 跨服务追踪测试用例

test "trace_context_creation_and_validation" {
  // 测试追踪上下文的创建和验证
  
  // 创建有效的追踪ID和Span ID
  let trace_id = azimuth::telemetry::api::trace::generate_trace_id()
  let span_id = azimuth::telemetry::api::trace::generate_span_id()
  
  // 验证ID格式
  assert_eq(trace_id.length(), 32)  // 16 bytes = 32 hex chars
  assert_eq(span_id.length(), 16)   // 8 bytes = 16 hex chars
  
  // 验证ID只包含有效字符
  assert_eq(trace_id.matches("^[0-9a-fA-F]{32}$"), true)
  assert_eq(span_id.matches("^[0-9a-fA-F]{16}$"), true)
  
  // 创建Span上下文
  let span_context = azimuth::telemetry::api::trace::SpanContext::new(trace_id, span_id, true, false)
  assert_eq(span_context.trace_id, trace_id)
  assert_eq(span_context.span_id, span_id)
  assert_eq(span_context.is_sampled, true)
  assert_eq(span_context.is_remote, false)
}

test "w3c_trace_context_propagation" {
  // 测试W3C Trace Context的传播
  
  // 创建原始Span上下文
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b9c7c989f97918e1"
  let span_context = azimuth::telemetry::api::trace::SpanContext::new(trace_id, span_id, true, false)
  
  // 创建W3C Trace Context传播器
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  
  // 注入到载体
  let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  let ctx = azimuth::telemetry::api::context::Context::empty()
    .with_value(azimuth::telemetry::api::context::create_key("span_context"), span_context)
  
  propagator.inject(ctx, carrier)
  
  // 验证注入的traceparent头
  let traceparent = carrier.get("traceparent").unwrap()
  assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b9c7c989f97918e1-01")
  
  // 从载体提取
  let extracted_ctx = propagator.extract(carrier)
  let extracted_span_context = extracted_ctx.get(azimuth::telemetry::api::context::create_key("span_context")).unwrap()
  
  // 验证提取的上下文
  assert_eq(extracted_span_context.trace_id, trace_id)
  assert_eq(extracted_span_context.span_id, span_id)
  assert_eq(extracted_span_context.is_sampled, true)
  assert_eq(extracted_span_context.is_remote, true)  // 提取的上下文标记为远程
}

test "distributed_tracing_flow" {
  // 测试分布式追踪流程
  
  // 服务A：创建根Span
  let tracer_a = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("service-a", "1.0.0")
  let (ctx_a, root_span) = tracer_a.start_span(
    azimuth::telemetry::api::context::Context::empty(),
    "http.request",
    azimuth::telemetry::api::trace::SpanKind::Server,
    azimuth::telemetry::api::common::Attributes::empty()
      .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
      .with("http.url", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
  )
  
  root_span.set_attribute("user.id", azimuth::telemetry::api::common::AttributeValue::String("user123"))
  root_span.add_event("request.start", azimuth::telemetry::api::common::Attributes::empty())
  
  // 服务A调用服务B：传播上下文
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  propagator.inject(ctx_a, carrier)
  
  // 验证传播的头部
  let traceparent = carrier.get("traceparent").unwrap()
  assert_eq(traceparent.starts_with("00-"), true)
  
  // 服务B：接收并处理请求
  let ctx_b = propagator.extract(carrier)
  let tracer_b = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("service-b", "2.1.0")
  let (ctx_b_child, span_b) = tracer_b.start_span(
    ctx_b,
    "database.query",
    azimuth::telemetry::api::trace::SpanKind::Client,
    azimuth::telemetry::api::common::Attributes::empty()
      .with("db.system", azimuth::telemetry::api::common::AttributeValue::String("postgresql"))
      .with("db.statement", azimuth::telemetry::api::common::AttributeValue::String("SELECT * FROM users"))
  )
  
  span_b.set_attribute("db.rows", azimuth::telemetry::api::common::AttributeValue::Int64(25))
  span_b.add_event("query.start", azimuth::telemetry::api::common::Attributes::empty())
  
  // 服务B调用服务C：继续传播
  let carrier_b = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  propagator.inject(ctx_b_child, carrier_b)
  
  // 服务C：处理最终请求
  let ctx_c = propagator.extract(carrier_b)
  let tracer_c = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("service-c", "3.0.0")
  let (_, span_c) = tracer_c.start_span(
    ctx_c,
    "cache.operation",
    azimuth::telemetry::api::trace::SpanKind::Internal,
    azimuth::telemetry::api::common::Attributes::empty()
      .with("cache.system", azimuth::telemetry::api::common::AttributeValue::String("redis"))
      .with("cache.operation", azimuth::telemetry::api::common::AttributeValue::String("get"))
  )
  
  span_c.set_attribute("cache.hit", azimuth::telemetry::api::common::AttributeValue::Bool(true))
  
  // 结束所有Span
  span_c.end()
  span_b.end()
  root_span.end()
  
  // 验证追踪链的完整性
  let root_context = root_span.span_context()
  let b_context = span_b.span_context()
  let c_context = span_c.span_context()
  
  assert_eq(b_context.trace_id, root_context.trace_id)
  assert_eq(c_context.trace_id, root_context.trace_id)
  assert_eq(b_context.trace_id, c_context.trace_id)
  
  // 验证父子关系
  assert_eq(b_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(c_context.parent_span_id.unwrap(), b_context.span_id)
}

test "cross_service_baggage_propagation" {
  // 测试跨服务的Baggage传播
  
  // 创建Baggage传播器
  let baggage_propagator = azimuth::telemetry::api::propagation::W3CBaggagePropagator::new()
  
  // 服务A：创建初始Baggage
  let baggage_a = azimuth::telemetry::api::context::Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("user.role", "admin")
    .with_entry("request.id", "req-abc-123")
    .with_entry("feature.flags", "new_ui,experimental")
  
  let ctx_a = azimuth::telemetry::api::context::Context::empty()
    .with_value(azimuth::telemetry::api::context::create_key("baggage"), baggage_a)
  
  // 注入Baggage
  let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  baggage_propagator.inject(ctx_a, carrier)
  
  // 验证baggage头
  let baggage_header = carrier.get("baggage").unwrap()
  assert_eq(baggage_header.contains("user.id=12345"), true)
  assert_eq(baggage_header.contains("user.role=admin"), true)
  assert_eq(baggage_header.contains("request.id=req-abc-123"), true)
  assert_eq(baggage_header.contains("feature.flags=new_ui,experimental"), true)
  
  // 服务B：提取Baggage并添加新条目
  let ctx_b = baggage_propagator.extract(carrier)
  let extracted_baggage = ctx_b.get(azimuth::telemetry::api::context::create_key("baggage")).unwrap()
  
  let baggage_b = extracted_baggage
    .with_entry("service.name", "auth-service")
    .with_entry("operation.type", "authentication")
  
  let ctx_b_enhanced = ctx_b.with_value(azimuth::telemetry::api::context::create_key("baggage"), baggage_b)
  
  // 继续传播
  let carrier_b = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  baggage_propagator.inject(ctx_b_enhanced, carrier_b)
  
  // 服务C：提取并验证完整的Baggage
  let ctx_c = baggage_propagator.extract(carrier_b)
  let final_baggage = ctx_c.get(azimuth::telemetry::api::context::create_key("baggage")).unwrap()
  
  // 验证所有Baggage条目
  assert_eq(final_baggage.get("user.id").unwrap(), "12345")
  assert_eq(final_baggage.get("user.role").unwrap(), "admin")
  assert_eq(final_baggage.get("request.id").unwrap(), "req-abc-123")
  assert_eq(final_baggage.get("feature.flags").unwrap(), "new_ui,experimental")
  assert_eq(final_baggage.get("service.name").unwrap(), "auth-service")
  assert_eq(final_baggage.get("operation.type").unwrap(), "authentication")
}

test "composite_propagator_integration" {
  // 测试复合传播器的集成
  
  // 创建复合传播器（Trace Context + Baggage）
  let composite_propagator = azimuth::telemetry::api::propagation::CompositePropagator::new([
    azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new(),
    azimuth::telemetry::api::propagation::W3CBaggagePropagator::new()
  ])
  
  // 服务A：创建完整的上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let span_context = azimuth::telemetry::api::trace::SpanContext::new(trace_id, span_id, true, false)
  
  let baggage = azimuth::telemetry::api::context::Baggage::empty()
    .with_entry("user.id", "user789")
    .with_entry("tenant.id", "tenant-001")
  
  let ctx_a = azimuth::telemetry::api::context::Context::empty()
    .with_value(azimuth::telemetry::api::context::create_key("span_context"), span_context)
    .with_value(azimuth::telemetry::api::context::create_key("baggage"), baggage)
  
  // 注入复合上下文
  let carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  composite_propagator.inject(ctx_a, carrier)
  
  // 验证注入的头部
  assert_eq(carrier.get("traceparent").is_some(), true)
  assert_eq(carrier.get("baggage").is_some(), true)
  
  // 服务B：提取复合上下文
  let ctx_b = composite_propagator.extract(carrier)
  let extracted_span_context = ctx_b.get(azimuth::telemetry::api::context::create_key("span_context")).unwrap()
  let extracted_baggage = ctx_b.get(azimuth::telemetry::api::context::create_key("baggage")).unwrap()
  
  // 验证提取的上下文
  assert_eq(extracted_span_context.trace_id, trace_id)
  assert_eq(extracted_span_context.span_id, span_id)
  assert_eq(extracted_span_context.is_remote, true)
  
  assert_eq(extracted_baggage.get("user.id").unwrap(), "user789")
  assert_eq(extracted_baggage.get("tenant.id").unwrap(), "tenant-001")
}

test "trace_context_edge_cases" {
  // 测试追踪上下文的边界情况
  
  // 测试无效的traceparent格式
  let invalid_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  invalid_carrier.set("traceparent", "invalid-format")
  
  let propagator = azimuth::telemetry::api::propagation::W3CTraceContextPropagator::new()
  let ctx = propagator.extract(invalid_carrier)
  
  // 无效格式应该返回空上下文
  let span_context = ctx.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(span_context.is_none(), true)
  
  // 测试版本不支持的traceparent
  let unsupported_version_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  unsupported_version_carrier.set("traceparent", "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  
  let ctx_unsupported = propagator.extract(unsupported_version_carrier)
  let unsupported_span_context = ctx_unsupported.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(unsupported_span_context.is_none(), true)
  
  // 测试空traceparent
  let empty_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  empty_carrier.set("traceparent", "")
  
  let ctx_empty = propagator.extract(empty_carrier)
  let empty_span_context = ctx_empty.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(empty_span_context.is_none(), true)
  
  // 测试部分缺失的traceparent
  let partial_carrier = azimuth::telemetry::api::propagation::TextMapCarrier::new()
  partial_carrier.set("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736")
  
  let ctx_partial = propagator.extract(partial_carrier)
  let partial_span_context = ctx_partial.get(azimuth::telemetry::api::context::create_key("span_context"))
  assert_eq(partial_span_context.is_none(), true)
}

test "span_hierarchy_and_relationships" {
  // 测试Span层次结构和关系
  
  // 创建根Span
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("test-service")
  let (root_ctx, root_span) = tracer.start_span(
    azimuth::telemetry::api::context::Context::empty(),
    "root.operation",
    azimuth::telemetry::api::trace::SpanKind::Server
  )
  
  // 创建第一层子Span
  let (child1_ctx, child1_span) = tracer.start_span(
    root_ctx,
    "child.operation.1",
    azimuth::telemetry::api::trace::SpanKind::Client
  )
  
  let (child2_ctx, child2_span) = tracer.start_span(
    root_ctx,
    "child.operation.2",
    azimuth::telemetry::api::trace::SpanKind::Internal
  )
  
  // 创建第二层子Span
  let (grandchild_ctx, grandchild_span) = tracer.start_span(
    child1_ctx,
    "grandchild.operation",
    azimuth::telemetry::api::trace::SpanKind::Client
  )
  
  // 验证Span关系
  let root_context = root_span.span_context()
  let child1_context = child1_span.span_context()
  let child2_context = child2_span.span_context()
  let grandchild_context = grandchild_span.span_context()
  
  // 所有Span应该有相同的trace ID
  assert_eq(child1_context.trace_id, root_context.trace_id)
  assert_eq(child2_context.trace_id, root_context.trace_id)
  assert_eq(grandchild_context.trace_id, root_context.trace_id)
  
  // 验证父子关系
  assert_eq(child1_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(child2_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(grandchild_context.parent_span_id.unwrap(), child1_context.span_id)
  
  // 验证Span ID的唯一性
  assert_eq(child1_context.span_id != child2_context.span_id, true)
  assert_eq(child1_context.span_id != grandchild_context.span_id, true)
  assert_eq(child2_context.span_id != grandchild_context.span_id, true)
  
  // 结束Span（按正确顺序）
  grandchild_span.end()
  child1_span.end()
  child2_span.end()
  root_span.end()
}

test "async_tracing_context_propagation" {
  // 测试异步场景下的追踪上下文传播
  
  // 创建根Span
  let tracer = azimuth::telemetry::api::trace::TracerProvider::global().get_tracer("async-service")
  let (root_ctx, root_span) = tracer.start_span(
    azimuth::telemetry::api::context::Context::empty(),
    "async.operation",
    azimuth::telemetry::api::trace::SpanKind::Server
  )
  
  // 模拟异步操作1
  let async1_span = tracer.start_span(
    root_ctx,
    "async.task.1",
    azimuth::telemetry::api::trace::SpanKind::Internal
  ).1
  
  // 模拟异步操作2
  let async2_span = tracer.start_span(
    root_ctx,
    "async.task.2",
    azimuth::telemetry::api::trace::SpanKind::Internal
  ).1
  
  // 模拟嵌套异步操作
  let (nested_ctx, nested_span) = tracer.start_span(
    root_ctx,
    "async.nested",
    azimuth::telemetry::api::trace::SpanKind::Internal
  )
  
  let nested_async_span = tracer.start_span(
    nested_ctx,
    "async.nested.task",
    azimuth::telemetry::api::trace::SpanKind::Internal
  ).1
  
  // 验证所有异步操作都正确关联到根Span
  let root_context = root_span.span_context()
  let async1_context = async1_span.span_context()
  let async2_context = async2_span.span_context()
  let nested_context = nested_span.span_context()
  let nested_async_context = nested_async_span.span_context()
  
  assert_eq(async1_context.trace_id, root_context.trace_id)
  assert_eq(async2_context.trace_id, root_context.trace_id)
  assert_eq(nested_context.trace_id, root_context.trace_id)
  assert_eq(nested_async_context.trace_id, root_context.trace_id)
  
  // 验证父子关系
  assert_eq(async1_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(async2_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(nested_context.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(nested_async_context.parent_span_id.unwrap(), nested_context.span_id)
  
  // 结束所有Span
  nested_async_span.end()
  nested_span.end()
  async1_span.end()
  async2_span.end()
  root_span.end()
}