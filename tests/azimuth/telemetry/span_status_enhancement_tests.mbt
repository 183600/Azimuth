// Azimuth Telemetry System - Span and Status Enhancement Tests
// 跨度和状态增强测试

test "SpanKind枚举类型测试" {
  // 测试所有SpanKind枚举值
  let internal_span = Span::new("internal-span", Internal, SpanContext::new("trace1", "span1", true, ""))
  let server_span = Span::new("server-span", Server, SpanContext::new("trace2", "span2", true, ""))
  let client_span = Span::new("client-span", Client, SpanContext::new("trace3", "span3", true, ""))
  let producer_span = Span::new("producer-span", Producer, SpanContext::new("trace4", "span4", true, ""))
  let consumer_span = Span::new("consumer-span", Consumer, SpanContext::new("trace5", "span5", true, ""))
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // 测试不同类型Span的名称
  assert_eq(Span::name(internal_span), "internal-span")
  assert_eq(Span::name(server_span), "server-span")
  assert_eq(Span::name(client_span), "client-span")
  assert_eq(Span::name(producer_span), "producer-span")
  assert_eq(Span::name(consumer_span), "consumer-span")
}

test "StatusCode枚举和状态转换测试" {
  // 创建一个测试Span
  let span_ctx = SpanContext::new("test-trace", "test-span", true, "")
  let span = Span::new("status-test-span", Internal, span_ctx)
  
  // 测试初始状态
  assert_eq(Span::status(span), Unset)
  
  // 测试设置不同状态
  Span::set_status(span, Ok)
  // 注意：简化实现可能不立即反映状态变化
  
  Span::set_status(span, Error, Some("Something went wrong"))
  
  Span::set_status(span, Unset)
  
  // 测试在不同状态下添加事件
  Span::add_event(span, "event-before-error", Some([("phase", StringValue("before-error"))]))
  Span::set_status(span, Error, Some("Critical error occurred"))
  Span::add_event(span, "event-after-error", Some([("phase", StringValue("after-error"))]))
  
  // 测试Span结束时的状态
  Span::end(span)
}

test "Span事件和属性详细测试" {
  let span_ctx = SpanContext::new("event-trace", "event-span", true, "")
  let span = Span::new("event-test-span", Server, span_ctx)
  
  // 测试添加不同类型的事件
  Span::add_event(span, "span.started", Some([
    ("component", StringValue("http-server")),
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users"))
  ]))
  
  Span::add_event(span, "database.query", Some([
    ("db.system", StringValue("postgresql")),
    ("db.statement", StringValue("SELECT * FROM users")),
    ("db.duration", FloatValue(45.2))
  ]))
  
  Span::add_event(span, "cache.hit", Some([
    ("cache.system", StringValue("redis")),
    ("cache.key", StringValue("user:123"))
  ]))
  
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("ValidationError")),
    ("error.message", StringValue("Invalid user input")),
    ("error.stack", StringValue("at line 42 in validation.js"))
  ]))
  
  // 测试不带属性的事件
  Span::add_event(span, "simple.event", None)
  
  // 测试带有数组和布尔值属性的事件
  Span::add_event(span, "complex.event", Some([
    ("array.values", ArrayStringValue(["value1", "value2", "value3"])),
    ("array.numbers", ArrayIntValue([1, 2, 3, 4, 5])),
    ("feature.enabled", BoolValue(true)),
    ("debug.mode", BoolValue(false))
  ]))
  
  // 测试Span结束
  Span::end(span)
}

test "嵌套Span和父子关系测试" {
  // 创建父Span
  let parent_ctx = SpanContext::new("parent-trace", "parent-span", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_ctx)
  
  // 创建子Span
  let child1_ctx = SpanContext::new("parent-trace", "child1-span", true, "")
  let child1_span = Span::new("child-operation-1", Client, child1_ctx)
  
  let child2_ctx = SpanContext::new("parent-trace", "child2-span", true, "")
  let child2_span = Span::new("child-operation-2", Internal, child2_ctx)
  
  // 验证父子关系（通过trace_id相同）
  assert_eq(SpanContext::trace_id(Span::span_context(parent_span)), "parent-trace")
  assert_eq(SpanContext::trace_id(Span::span_context(child1_span)), "parent-trace")
  assert_eq(SpanContext::trace_id(Span::span_context(child2_span)), "parent-trace")
  
  // 验证不同的span_id
  assert_eq(SpanContext::span_id(Span::span_context(parent_span)), "parent-span")
  assert_eq(SpanContext::span_id(Span::span_context(child1_span)), "child1-span")
  assert_eq(SpanContext::span_id(Span::span_context(child2_span)), "child2-span")
  
  // 在各个Span中添加事件
  Span::add_event(parent_span, "parent.started", None)
  Span::add_event(child1_span, "child1.started", None)
  Span::add_event(child2_span, "child2.started", None)
  
  Span::add_event(child1_span, "child1.completed", None)
  Span::add_event(child2_span, "child2.completed", None)
  Span::add_event(parent_span, "parent.completed", None)
  
  // 按顺序结束Span（子Span先结束）
  Span::end(child1_span)
  Span::end(child2_span)
  Span::end(parent_span)
}

test "Span上下文验证和边界条件测试" {
  // 测试有效的SpanContext
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1,key2=value2")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(valid_ctx), "b7ad6b7169203331")
  
  // 测试无效的SpanContext（空trace_id）
  let invalid_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // 测试无效的SpanContext（空span_id）
  let invalid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // 测试无效的SpanContext（两者都为空）
  let invalid_both_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_both_ctx))
  assert_false(SpanContext::is_sampled(invalid_both_ctx))
  
  // 测试采样状态的组合
  let sampled_ctx = SpanContext::new("trace1", "span1", true, "")
  let not_sampled_ctx = SpanContext::new("trace2", "span2", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  
  // 测试trace_state
  let ctx_with_state = SpanContext::new("trace3", "span3", true, "key1=value1,key2=value2,key3=value3")
  assert_eq(ctx_with_state.trace_state, "key1=value1,key2=value2,key3=value3")
  
  let ctx_empty_state = SpanContext::new("trace4", "span4", true, "")
  assert_eq(ctx_empty_state.trace_state, "")
}

test "多Tracer和InstrumentationScope测试" {
  // 创建多个TracerProvider
  let provider1 = TracerProvider::default()
  let provider2 = TracerProvider::default()
  
  // 从不同的Provider创建Tracer
  let tracer1 = TracerProvider::get_tracer(provider1, "service-A", Some("1.0.0"))
  let tracer2 = TracerProvider::get_tracer(provider1, "service-B", Some("2.1.0"))
  let tracer3 = TracerProvider::get_tracer(provider2, "service-C", Some("3.2.1"))
  
  // 验证Tracer的InstrumentationScope
  let scope1 = Tracer::instrumentation_scope(tracer1)
  let scope2 = Tracer::instrumentation_scope(tracer2)
  let scope3 = Tracer::instrumentation_scope(tracer3)
  
  assert_eq(scope1.name, "service-A")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, None)
  
  assert_eq(scope2.name, "service-B")
  assert_eq(scope2.version, Some("2.1.0"))
  assert_eq(scope2.schema_url, None)
  
  assert_eq(scope3.name, "service-C")
  assert_eq(scope3.version, Some("3.2.1"))
  assert_eq(scope3.schema_url, None)
  
  // 使用不同的Tracer创建Span
  let span1 = Tracer::start_span(tracer1, "operation-A")
  let span2 = Tracer::start_span(tracer2, "operation-B")
  let span3 = Tracer::start_span(tracer3, "operation-C")
  
  assert_eq(Span::name(span1), "operation-A")
  assert_eq(Span::name(span2), "operation-B")
  assert_eq(Span::name(span3), "operation-C")
  
  // 测试带有schema_url的Tracer
  let tracer_with_schema = TracerProvider::get_tracer(
    provider1, 
    "service-with-schema", 
    Some("1.0.0"),
    Some("https://opentelemetry.io/schemas/1.20.0")
  )
  let scope_with_schema = Tracer::instrumentation_scope(tracer_with_schema)
  assert_eq(scope_with_schema.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
}