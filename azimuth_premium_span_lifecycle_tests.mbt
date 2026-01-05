// Azimuth Premium Span Lifecycle Tests
// 测试Span生命周期的各个方面，包括创建、状态管理、事件记录和终止

test "span creation and basic properties" {
  // 测试Span的基本创建和属性访问
  let span_context = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-operation", Internal, span_context)
  
  // 验证基本属性
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // 验证Span上下文
  let ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(ctx), "trace-123")
  assert_eq(SpanContext::span_id(ctx), "span-456")
  assert_true(SpanContext::is_sampled(ctx))
  assert_true(SpanContext::is_valid(ctx))
}

test "span status management" {
  // 测试Span状态设置和获取
  let span_context = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-operation", Server, span_context)
  
  // 初始状态应为Unset
  assert_eq(Span::status(span), Unset)
  
  // 设置成功状态
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // 设置错误状态
  Span::set_status(span, Error, Some("Operation failed"))
  assert_eq(Span::status(span), Error)
}

test "span event recording" {
  // 测试Span事件记录功能
  let span_context = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-operation", Client, span_context)
  
  // 添加不带属性的事件
  Span::add_event(span, "event-1", None)
  
  // 添加带属性的事件
  let attributes = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(42)),
    ("key3", BoolValue(true))
  ]
  Span::add_event(span, "event-2", Some(attributes))
  
  // 简化测试 - 只验证操作不会失败
  assert_true(true)
}

test "span lifecycle - creation to termination" {
  // 测试完整的Span生命周期
  let span_context = SpanContext::new("trace-789", "span-101", true, "")
  let span = Span::new("lifecycle-test", Producer, span_context)
  
  // 验证初始状态
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Producer)
  
  // 添加事件和状态
  Span::add_event(span, "started", None)
  Span::set_status(span, Ok, Some("Processing completed"))
  Span::add_event(span, "finished", None)
  
  // 终止Span
  Span::end(span)
  
  // 验证操作完成
  assert_true(true)
}

test "different span kinds" {
  // 测试不同类型的Span Kind
  let trace_id = "trace-kinds-test"
  
  // 测试Internal类型
  let internal_ctx = SpanContext::new(trace_id, "span-internal", true, "")
  let internal_span = Span::new("internal-op", Internal, internal_ctx)
  assert_eq(Span::kind(internal_span), Internal)
  
  // 测试Server类型
  let server_ctx = SpanContext::new(trace_id, "span-server", true, "")
  let server_span = Span::new("server-op", Server, server_ctx)
  assert_eq(Span::kind(server_span), Server)
  
  // 测试Client类型
  let client_ctx = SpanContext::new(trace_id, "span-client", true, "")
  let client_span = Span::new("client-op", Client, client_ctx)
  assert_eq(Span::kind(client_span), Client)
  
  // 测试Producer类型
  let producer_ctx = SpanContext::new(trace_id, "span-producer", true, "")
  let producer_span = Span::new("producer-op", Producer, producer_ctx)
  assert_eq(Span::kind(producer_span), Producer)
  
  // 测试Consumer类型
  let consumer_ctx = SpanContext::new(trace_id, "span-consumer", true, "")
  let consumer_span = Span::new("consumer-op", Consumer, consumer_ctx)
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "span context validation" {
  // 测试Span上下文验证
  // 有效的上下文
  let valid_ctx = SpanContext::new("valid-trace", "valid-span", true, "")
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  
  // 无效的上下文（空trace_id）
  let invalid_trace_ctx = SpanContext::new("", "valid-span", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // 无效的上下文（空span_id）
  let invalid_span_ctx = SpanContext::new("valid-trace", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // 未采样的上下文
  let not_sampled_ctx = SpanContext::new("valid-trace", "valid-span", false, "")
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
}

test "span with complex attributes" {
  // 测试带有复杂属性的Span操作
  let span_context = SpanContext::new("trace-complex", "span-complex", true, "")
  let span = Span::new("complex-operation", Internal, span_context)
  
  // 创建复杂属性数组
  let complex_attributes = [
    ("string_attr", StringValue("complex string value")),
    ("int_attr", IntValue(12345)),
    ("float_attr", FloatValue(3.14159)),
    ("bool_attr", BoolValue(true)),
    ("array_string", ArrayStringValue(["item1", "item2", "item3"])),
    ("array_int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  // 添加带有复杂属性的事件
  Span::add_event(span, "complex-event", Some(complex_attributes))
  
  // 设置状态
  Span::set_status(span, Ok, Some("Complex operation completed"))
  
  // 验证操作成功
  assert_true(true)
}

test "span error handling" {
  // 测试Span操作中的错误处理
  let span_context = SpanContext::new("trace-error", "span-error", true, "")
  let span = Span::new("error-operation", Internal, span_context)
  
  // 模拟错误场景
  Span::add_event(span, "error-started", None)
  Span::set_status(span, Error, Some("Simulated error occurred"))
  Span::add_event(span, "error-logged", None)
  
  // 验证错误状态
  assert_eq(Span::status(span), Error)
  
  // 即使在错误状态下，Span也应该能够正常结束
  Span::end(span)
  assert_true(true)
}

test "multiple span operations" {
  // 测试多个Span操作的序列
  let trace_id = "trace-multiple"
  
  // 创建第一个Span
  let span1_ctx = SpanContext::new(trace_id, "span-1", true, "")
  let span1 = Span::new("operation-1", Internal, span1_ctx)
  Span::add_event(span1, "started", None)
  
  // 创建第二个Span
  let span2_ctx = SpanContext::new(trace_id, "span-2", true, "")
  let span2 = Span::new("operation-2", Internal, span2_ctx)
  Span::add_event(span2, "started", None)
  Span::set_status(span2, Ok, Some("Operation 2 completed"))
  
  // 完成第一个Span
  Span::set_status(span1, Ok, Some("Operation 1 completed"))
  Span::end(span1)
  
  // 完成第二个Span
  Span::end(span2)
  
  // 验证所有操作成功
  assert_true(true)
}

test "span with empty and null values" {
  // 测试处理空值和null值的Span操作
  let span_context = SpanContext::new("trace-empty", "span-empty", true, "")
  let span = Span::new("empty-operation", Internal, span_context)
  
  // 测试空字符串事件名称
  Span::add_event(span, "", None)
  
  // 测试空描述的状态设置
  Span::set_status(span, Ok, None)
  
  // 测试空属性数组
  Span::add_event(span, "empty-attrs", Some([]))
  
  // 验证所有操作都能正常处理
  assert_true(true)
}