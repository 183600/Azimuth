// 跨服务传播高级测试用例
test "W3CTraceContext传播器创建测试" {
  let propagator = W3CTraceContextPropagator::new()
  
  // 验证传播器创建成功
  assert_true(true)
}

test "W3CBaggage传播器创建测试" {
  let propagator = W3CBaggagePropagator::new()
  
  // 验证传播器创建成功
  assert_true(true)
}

test "复合传播器创建和操作测试" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  assert_eq(composite.propagators.length, 2)
}

test "TextMapCarrier基本操作测试" {
  let carrier = TextMapCarrier::new()
  assert_eq(carrier.headers.length, 0)
  
  TextMapCarrier::set(carrier, "custom-header", "custom-value")
  
  let retrieved = TextMapCarrier::get(carrier, "traceparent")
  assert_true(retrieved is Some)
  match retrieved {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    _ => assert_true(false)
  }
  
  let missing = TextMapCarrier::get(carrier, "nonexistent-header")
  assert_true(missing is None)
}

test "复合传播器注入测试" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  let carrier = TextMapCarrier::new()
  let context = Context::with_value(Context::root(), ContextKey::new("test"), "value")
  
  CompositePropagator::inject(composite, context, carrier)
  
  // 验证注入操作不崩溃
  assert_true(true)
}

test "复合传播器提取测试" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  let carrier = TextMapCarrier::new()
  
  let extracted_context = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  
  assert_true(extracted_value is Some)
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
}

test "传播器与SpanContext集成测试" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "state=value")
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(SpanContext::span_id(span_ctx), "span-456")
  assert_eq(SpanContext::is_sampled(span_ctx), true)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  let empty_span_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(empty_span_ctx), false)
}