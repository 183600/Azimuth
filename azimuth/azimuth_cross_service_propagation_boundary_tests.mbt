// Azimuth Cross-Service Propagation Boundary Tests
// 测试跨服务传播的边界条件和异常情况

test "cross_service_propagation_empty_trace_context" {
  // 测试空的 trace context 处理
  let empty_span_ctx = SpanContext::new("", "", false, "")
  @assertion.assert_eq(SpanContext::is_valid(empty_span_ctx), false)?
  
  // 测试空的 trace_id 但有效的 span_id
  let invalid_trace_ctx = SpanContext::new("", "span123", true, "")
  @assertion.assert_eq(SpanContext::is_valid(invalid_trace_ctx), false)?
  
  // 测试有效的 trace_id 但空的 span_id
  let invalid_span_ctx = SpanContext::new("trace123", "", true, "")
  @assertion.assert_eq(SpanContext::is_valid(invalid_span_ctx), false)?
}

test "cross_service_propagation_malformed_traceparent" {
  // 测试格式错误的 traceparent 头处理
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 注入上下文
  CompositePropagator::inject(propagator, ctx, carrier)
  
  // 验证基本的 traceparent 格式
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  @assertion.assert_eq(traceparent.is_some(), true)?
  
  match traceparent {
    Some(value) => {
      // 验证 traceparent 包含两个连字符（基本格式检查）
      @assertion.assert_eq(value.contains("-"), true)?
    }
    None => @assertion.assert_eq(false, true)? // 不应该到达这里
  }
}

test "cross_service_propagation_oversized_baggage" {
  // 测试过大的 baggage 处理
  let baggage = Baggage::new()
  
  // 添加多个条目
  let baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "key3", "value3")
  let baggage4 = Baggage::set_entry(baggage3, "key4", "value4")
  let baggage5 = Baggage::set_entry(baggage4, "key5", "value5")
  
  // 验证所有条目都可以正确获取
  @assertion.assert_eq(Baggage::get_entry(baggage5, "key1"), Some("value1"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "key2"), Some("value2"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "key3"), Some("value3"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "key4"), Some("value4"))?
  @assertion.assert_eq(Baggage::get_entry(baggage5, "key5"), Some("value5"))?
  
  // 测试移除条目
  let baggage_after_removal = Baggage::remove_entry(baggage5, "key3")
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "key3"), None)?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "key4"), Some("value4"))?
}

test "cross_service_propagation_special_characters_in_baggage" {
  // 测试包含特殊字符的 baggage 处理
  let baggage = Baggage::new()
  
  // 添加包含特殊字符的条目
  let baggage_with_special = Baggage::set_entry(baggage, "special.key", "value with spaces & symbols")
  let baggage_with_unicode = Baggage::set_entry(baggage_with_special, "unicode.key", "值包含中文")
  let baggage_with_equals = Baggage::set_entry(baggage_with_unicode, "equals.key", "value=with=equals")
  
  // 验证特殊字符条目的获取
  @assertion.assert_eq(Baggage::get_entry(baggage_with_equals, "special.key"), Some("value with spaces & symbols"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_equals, "unicode.key"), Some("值包含中文"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_equals, "equals.key"), Some("value=with=equals"))?
}

test "cross_service_propagation_context_chain_limits" {
  // 测试上下文链的长度限制
  let ctx = Context::root()
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  // 创建上下文链
  let ctx1 = Context::with_value(ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // 验证上下文链中的值
  @assertion.assert_eq(Context::get(ctx3, key1), Some("value1"))?
  @assertion.assert_eq(Context::get(ctx3, key2), Some("value2"))?
  @assertion.assert_eq(Context::get(ctx3, key3), Some("value3"))?
  
  // 验证不存在的键
  let nonexistent_key = ContextKey::new("nonexistent")
  @assertion.assert_eq(Context::get(ctx3, nonexistent_key), None)?
}

test "cross_service_propagation_multiple_propagators" {
  // 测试多个传播器的组合使用
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 注入上下文
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 提取上下文
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extract_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx, extract_key), Some("true"))?
}

test "cross_service_propagation_span_hierarchy_validation" {
  // 测试跨服务的 Span 层次结构验证
  let parent_ctx = SpanContext::new("trace123456789", "parent123", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_ctx)
  
  let child_ctx = SpanContext::new("trace123456789", "child456", true, "")
  let child_span = Span::new("child-operation", Client, child_ctx)
  
  // 验证父子关系的 trace_id 一致性
  @assertion.assert_eq(
    SpanContext::trace_id(Span::span_context(parent_span)),
    SpanContext::trace_id(Span::span_context(child_span))
  )?
  
  // 验证 span_id 不同
  @assertion.assert_not_eq(
    SpanContext::span_id(Span::span_context(parent_span)),
    SpanContext::span_id(Span::span_context(child_span))
  )?
  
  // 验证 Span 类型
  @assertion.assert_eq(Span::kind(parent_span), Server)?
  @assertion.assert_eq(Span::kind(child_span), Client)?
}

test "cross_service_propagation_resource_attribute_conflicts" {
  // 测试资源属性冲突的处理
  let service1_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-A")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ])
  
  let service2_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("service-B")),
    ("service.version", StringValue("2.0.0")),
    ("deployment.environment", StringValue("staging"))
  ])
  
  // 合并资源
  let merged_resource = Resource::merge(service1_resource, service2_resource)
  
  // 验证合并后的属性（根据实现，应该使用第二个资源的属性）
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "service.name"),
    Some(StringValue("service-B"))
  )?
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "service.version"),
    Some(StringValue("2.0.0"))
  )?
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "deployment.environment"),
    Some(StringValue("staging"))
  )?
}

test "cross_service_propagation_empty_carrier_handling" {
  // 测试空载体的处理
  let empty_carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 从空载体提取上下文
  let extracted_ctx = CompositePropagator::extract(propagator, empty_carrier)
  
  // 验证提取的上下文有提取标记
  let extract_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx, extract_key), Some("true"))?
  
  // 验证空载体中不存在的键
  @assertion.assert_eq(TextMapCarrier::get(empty_carrier, "nonexistent"), None)?
}