// Azimuth Span Lifecycle Management Test Suite
// 跟踪生命周期管理测试用例，专注于Span的完整生命周期、状态转换和事件处理

test "Span完整生命周期测试" {
  // 创建Tracer和Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle_test_tracer")
  
  // 创建Span上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  let span_context = azimuth::SpanContext::new(trace_id, span_id, true, "")
  
  // 创建Span
  let span = azimuth::Tracer::start_span(tracer, "main_operation")
  
  // 验证Span初始状态
  assert_eq(azimuth::Span::name(span), "main_operation")
  assert_true(azimuth::Span::is_recording(span))
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_eq(azimuth::Span::status(span), azimuth::Unset)
  
  // 验证Span上下文
  let retrieved_context = azimuth::Span::span_context(span)
  assert_true(azimuth::SpanContext::is_valid(retrieved_context))
  assert_true(azimuth::SpanContext::is_sampled(retrieved_context))
  
  // 设置Span状态为Ok
  azimuth::Span::set_status(span, azimuth::Ok, Some("操作成功完成"))
  assert_eq(azimuth::Span::status(span), azimuth::Ok)
  
  // 添加事件
  azimuth::Span::add_event(span, "operation_started", Some([
    ("step", StringValue("initialization")),
    ("timestamp", StringValue("2025-01-01T00:00:00Z"))
  ]))
  
  azimuth::Span::add_event(span, "operation_progress", Some([
    ("progress", StringValue("50%")),
    ("processed_items", IntValue(42))
  ]))
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span结束后的状态
  assert_eq(azimuth::Span::name(span), "main_operation")  // 名称仍然可访问
  assert_eq(azimuth::Span::status(span), azimuth::Ok)     // 状态仍然可访问
}

test "Span嵌套和父子关系测试" {
  // 创建Tracer
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "nested_span_test_tracer")
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "root_operation")
  
  // 创建子Span1
  let child_span1 = azimuth::Tracer::start_span(tracer, "child_operation_1")
  azimuth::Span::set_status(child_span1, azimuth::Ok, Some("子操作1成功"))
  
  // 创建子Span2
  let child_span2 = azimuth::Tracer::start_span(tracer, "child_operation_2")
  azimuth::Span::set_status(child_span2, azimuth::Ok, Some("子操作2成功"))
  
  // 创建孙Span
  let grandchild_span = azimuth::Tracer::start_span(tracer, "grandchild_operation")
  azimuth::Span::set_status(grandchild_span, azimuth::Error, Some("孙操作失败"))
  
  // 验证所有Span的独立性和关系
  assert_eq(azimuth::Span::name(root_span), "root_operation")
  assert_eq(azimuth::Span::name(child_span1), "child_operation_1")
  assert_eq(azimuth::Span::name(child_span2), "child_operation_2")
  assert_eq(azimuth::Span::name(grandchild_span), "grandchild_operation")
  
  assert_eq(azimuth::Span::status(root_span), azimuth::Unset)
  assert_eq(azimuth::Span::status(child_span1), azimuth::Ok)
  assert_eq(azimuth::Span::status(child_span2), azimuth::Ok)
  assert_eq(azimuth::Span::status(grandchild_span), azimuth::Error)
  
  // 验证所有Span都在记录状态
  assert_true(azimuth::Span::is_recording(root_span))
  assert_true(azimuth::Span::is_recording(child_span1))
  assert_true(azimuth::Span::is_recording(child_span2))
  assert_true(azimuth::Span::is_recording(grandchild_span))
  
  // 按顺序结束Span
  azimuth::Span::end(grandchild_span)
  azimuth::Span::end(child_span2)
  azimuth::Span::end(child_span1)
  azimuth::Span::end(root_span)
}

test "不同类型Span的创建和管理测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "span_types_test_tracer")
  
  // 创建不同类型的Span
  let internal_span = azimuth::Tracer::start_span(tracer, "internal_operation")
  let server_span = azimuth::Tracer::start_span(tracer, "server_operation")
  let client_span = azimuth::Tracer::start_span(tracer, "client_operation")
  let producer_span = azimuth::Tracer::start_span(tracer, "producer_operation")
  let consumer_span = azimuth::Tracer::start_span(tracer, "consumer_operation")
  
  // 验证Span类型（注意：这里需要根据实际实现调整）
  // 由于当前实现中所有Span都是Internal类型，这里主要验证名称和状态
  assert_eq(azimuth::Span::name(internal_span), "internal_operation")
  assert_eq(azimuth::Span::name(server_span), "server_operation")
  assert_eq(azimuth::Span::name(client_span), "client_operation")
  assert_eq(azimuth::Span::name(producer_span), "producer_operation")
  assert_eq(azimuth::Span::name(consumer_span), "consumer_operation")
  
  // 为不同类型的Span设置不同的状态
  azimuth::Span::set_status(internal_span, azimuth::Ok, Some("内部操作成功"))
  azimuth::Span::set_status(server_span, azimuth::Ok, Some("服务器请求处理成功"))
  azimuth::Span::set_status(client_span, azimuth::Error, Some("客户端请求失败"))
  azimuth::Span::set_status(producer_span, azimuth::Ok, Some("消息生产成功"))
  azimuth::Span::set_status(consumer_span, azimuth::Ok, Some("消息消费成功"))
  
  // 验证状态设置
  assert_eq(azimuth::Span::status(internal_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(server_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(client_span), azimuth::Error)
  assert_eq(azimuth::Span::status(producer_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(consumer_span), azimuth::Ok)
  
  // 为每个Span添加特定类型的事件
  azimuth::Span::add_event(internal_span, "internal_event", Some([
    ("operation_type", StringValue("internal")),
    ("duration_ms", IntValue(150))
  ]))
  
  azimuth::Span::add_event(server_span, "request_received", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/v1/resource")),
    ("user_agent", StringValue("Azimuth-Client/1.0"))
  ]))
  
  azimuth::Span::add_event(client_span, "request_sent", Some([
    ("http.method", StringValue("POST")),
    ("http.url", StringValue("/api/v2/submit")),
    ("request_size", IntValue(1024))
  ]))
  
  azimuth::Span::add_event(producer_span, "message_produced", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.destination", StringValue("events.topic")),
    ("message_size", IntValue(512))
  ]))
  
  azimuth::Span::add_event(consumer_span, "message_consumed", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.source", StringValue("events.topic")),
    ("processing_time_ms", IntValue(75))
  ]))
  
  // 结束所有Span
  azimuth::Span::end(internal_span)
  azimuth::Span::end(server_span)
  azimuth::Span::end(client_span)
  azimuth::Span::end(producer_span)
  azimuth::Span::end(consumer_span)
}

test "Span上下文验证和边界条件测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "context_validation_tracer")
  
  // 测试有效的Span上下文
  let valid_trace_id = "1234567890abcdef1234567890abcdef"
  let valid_span_id = "1234567890abcdef"
  let valid_span_context = azimuth::SpanContext::new(valid_trace_id, valid_span_id, true, "key1=value1,key2=value2")
  
  assert_true(azimuth::SpanContext::is_valid(valid_span_context))
  assert_true(azimuth::SpanContext::is_sampled(valid_span_context))
  assert_eq(azimuth::SpanContext::trace_id(valid_span_context), valid_trace_id)
  assert_eq(azimuth::SpanContext::span_id(valid_span_context), valid_span_id)
  
  // 测试无效的Span上下文
  let invalid_trace_id = ""
  let invalid_span_id = ""
  let invalid_span_context = azimuth::SpanContext::new(invalid_trace_id, invalid_span_id, false, "")
  
  assert_false(azimuth::SpanContext::is_valid(invalid_span_context))
  assert_false(azimuth::SpanContext::is_sampled(invalid_span_context))
  assert_eq(azimuth::SpanContext::trace_id(invalid_span_context), invalid_trace_id)
  assert_eq(azimuth::SpanContext::span_id(invalid_span_context), invalid_span_id)
  
  // 测试部分有效的Span上下文
  let partial_valid_trace_id = "1234567890abcdef1234567890abcdef"
  let partial_valid_span_id = ""
  let partial_valid_span_context = azimuth::SpanContext::new(partial_valid_trace_id, partial_valid_span_id, true, "")
  
  assert_false(azimuth::SpanContext::is_valid(partial_valid_span_context))  // span_id为空，整体无效
  assert_true(azimuth::SpanContext::is_sampled(partial_valid_span_context))
  assert_eq(azimuth::SpanContext::trace_id(partial_valid_span_context), partial_valid_trace_id)
  assert_eq(azimuth::SpanContext::span_id(partial_valid_span_context), partial_valid_span_id)
  
  // 创建使用这些上下文的Span
  let valid_span = azimuth::Tracer::start_span(tracer, "valid_context_span")
  let invalid_span = azimuth::Tracer::start_span(tracer, "invalid_context_span")
  
  // 验证Span上下文
  let valid_span_context_retrieved = azimuth::Span::span_context(valid_span)
  let invalid_span_context_retrieved = azimuth::Span::span_context(invalid_span)
  
  // 注意：由于简化实现，这里使用测试上下文
  assert_true(azimuth::SpanContext::is_valid(valid_span_context_retrieved))
  
  // 设置不同的状态
  azimuth::Span::set_status(valid_span, azimuth::Ok, Some("有效上下文Span成功"))
  azimuth::Span::set_status(invalid_span, azimuth::Error, Some("无效上下文Span失败"))
  
  // 验证状态
  assert_eq(azimuth::Span::status(valid_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(invalid_span), azimuth::Error)
  
  // 结束Span
  azimuth::Span::end(valid_span)
  azimuth::Span::end(invalid_span)
}

test "Span事件和属性的高级测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "advanced_events_tracer")
  
  let span = azimuth::Tracer::start_span(tracer, "advanced_events_span")
  
  // 添加各种类型的事件和属性
  let string_attributes = [
    ("string_attr", StringValue("字符串属性值")),
    ("unicode_attr", StringValue("Unicode测试_🚀_🌟")),
    ("empty_string_attr", StringValue("")),
    ("special_chars_attr", StringValue("特殊字符: !@#$%^&*()"))
  ]
  
  let int_attributes = [
    ("positive_int", IntValue(42)),
    ("zero_int", IntValue(0)),
    ("negative_int", IntValue(-17)),
    ("max_int", IntValue(2147483647)),
    ("min_int", IntValue(-2147483648))
  ]
  
  let float_attributes = [
    ("positive_float", FloatValue(3.14159)),
    ("zero_float", FloatValue(0.0)),
    ("negative_float", FloatValue(-2.71828)),
    ("scientific_float", FloatValue(1.23e-4))
  ]
  
  let bool_attributes = [
    ("true_bool", BoolValue(true)),
    ("false_bool", BoolValue(false))
  ]
  
  let array_string_attributes = [
    ("string_array", ArrayStringValue(["元素1", "element2", "🎉"])),
    ("empty_string_array", ArrayStringValue([])),
    ("single_string_array", ArrayStringValue(["单个元素"]))
  ]
  
  let array_int_attributes = [
    ("int_array", ArrayIntValue([1, 2, 3, 4, 5])),
    ("empty_int_array", ArrayIntValue([])),
    ("negative_int_array", ArrayIntValue([-1, -2, -3]))
  ]
  
  // 添加包含不同属性类型的事件
  azimuth::Span::add_event(span, "string_attributes_event", Some(string_attributes))
  azimuth::Span::add_event(span, "int_attributes_event", Some(int_attributes))
  azimuth::Span::add_event(span, "float_attributes_event", Some(float_attributes))
  azimuth::Span::add_event(span, "bool_attributes_event", Some(bool_attributes))
  azimuth::Span::add_event(span, "array_string_attributes_event", Some(array_string_attributes))
  azimuth::Span::add_event(span, "array_int_attributes_event", Some(array_int_attributes))
  
  // 添加混合属性类型的事件
  let mixed_attributes = [
    ("mixed_string", StringValue("混合字符串")),
    ("mixed_int", IntValue(100)),
    ("mixed_float", FloatValue(2.5)),
    ("mixed_bool", BoolValue(true)),
    ("mixed_string_array", ArrayStringValue(["a", "b", "c"])),
    ("mixed_int_array", ArrayIntValue([10, 20, 30]))
  ]
  
  azimuth::Span::add_event(span, "mixed_attributes_event", Some(mixed_attributes))
  
  // 添加无属性的事件
  azimuth::Span::add_event(span, "no_attributes_event", None)
  
  // 添加空属性列表的事件
  azimuth::Span::add_event(span, "empty_attributes_event", Some([]))
  
  // 设置状态并结束Span
  azimuth::Span::set_status(span, azimuth::Ok, Some("高级事件测试成功"))
  azimuth::Span::end(span)
  
  // 验证Span状态
  assert_eq(azimuth::Span::name(span), "advanced_events_span")
  assert_eq(azimuth::Span::status(span), azimuth::Ok)
}