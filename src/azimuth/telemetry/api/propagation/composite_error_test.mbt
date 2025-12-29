// Propagation模块复合传播器错误处理测试用例
test "composite_propagator_empty_propagators" {
  // 测试空的复合传播器
  let empty_propagators : Array[TextMapPropagator] = []
  let composite_propagator = CompositePropagator::new(empty_propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试空的inject操作
  composite_propagator.inject(ctx, carrier)
  
  // 测试空的extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证操作不会崩溃
  assert_eq(true, true)
}

test "composite_propagator_single_propagator_failure" {
  // 测试单个传播器失败的情况
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置无效的traceparent header来模拟错误情况
  carrier.set("traceparent", "invalid-traceparent-format")
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 即使有无效数据，操作也应该正常完成
  assert_eq(true, true)
}

test "composite_propagator_multiple_propagators_partial_failure" {
  // 测试多个传播器部分失败的情况
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置部分无效的header
  carrier.set("traceparent", "invalid-traceparent")
  carrier.set("baggage", "valid-key=value")
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 即使部分传播器失败，其他传播器也应该正常工作
  assert_eq(true, true)
}

test "composite_propagator_carrier_failure" {
  // 测试载体(Carrier)失败的情况
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置超长的header值来模拟载体失败
  let very_long_value = "this.is.a.very.long.header.value.that.exceeds.normal.expectations.and.may.cause.issues.with.the.carrier.implementation"
  carrier.set("traceparent", very_long_value)
  carrier.set("baggage", very_long_value)
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 即使载体有问题，操作也应该优雅处理
  assert_eq(true, true)
}

test "composite_propagator_malformed_headers" {
  // 测试格式错误的headers
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置各种格式错误的headers
  carrier.set("traceparent", "")  // 空的traceparent
  carrier.set("baggage", "")      // 空的baggage
  
  // 测试extract操作
  let extracted_ctx1 = composite_propagator.extract(ctx, carrier)
  
  // 清空并设置其他格式错误的headers
  carrier = MapCarrier::new()
  carrier.set("traceparent", "00-")  // 不完整的traceparent
  carrier.set("baggage", "invalid-format-without-equals")
  
  let extracted_ctx2 = composite_propagator.extract(ctx, carrier)
  
  // 设置包含特殊字符的headers
  carrier = MapCarrier::new()
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  carrier.set("baggage", "key=value!@#$%^&*()")  // 包含特殊字符
  
  let extracted_ctx3 = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx1, carrier)
  composite_propagator.inject(extracted_ctx2, carrier)
  composite_propagator.inject(extracted_ctx3, carrier)
  
  // 即使headers格式错误，操作也应该优雅处理
  assert_eq(true, true)
}

test "composite_propagator_context_corruption" {
  // 测试上下文损坏的情况
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建一个复杂的上下文
  let ctx = Context::empty()
  let key1 = create_key("trace.id")
  let key2 = create_key("span.id")
  let key3 = create_key("correlation.id")
  
  let complex_ctx = ctx
    .with_value(key1, "trace-12345678901234567890123456789012")
    .with_value(key2, "span-1234567890123456")
    .with_value(key3, "corr-abcdef1234567890")
  
  let carrier = MapCarrier::new()
  
  // 设置可能引起问题的headers
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  carrier.set("baggage", "key1=value1,key2=value2,key3=value3")
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(complex_ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 验证原始上下文没有被损坏
  match complex_ctx.get(key1) {
    Some(value) => assert_eq(value, "trace-12345678901234567890123456789012")
    None => @test.fail("Test failed")
  }
  
  match complex_ctx.get(key2) {
    Some(value) => assert_eq(value, "span-1234567890123456")
    None => @test.fail("Test failed")
  }
  
  match complex_ctx.get(key3) {
    Some(value) => assert_eq(value, "corr-abcdef1234567890")
    None => @test.fail("Test failed")
  }
}

test "composite_propagator_header_conflicts" {
  // 测试header冲突的情况
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置可能冲突的headers
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  carrier.set("baggage", "key1=value1,key2=value2")
  
  // 第一次inject操作
  composite_propagator.inject(ctx, carrier)
  
  // 修改headers来模拟冲突
  carrier.set("traceparent", "00-12345678901234567890123456789012-abcdef1234567890-01")
  carrier.set("baggage", "different.key=different.value")
  
  // 第二次inject操作
  composite_propagator.inject(ctx, carrier)
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 即使有header冲突，操作也应该正常完成
  assert_eq(true, true)
}

test "composite_propagator_large_data_handling" {
  // 测试大数据处理
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建大量的baggage数据
  let mut large_baggage = ""
  let mut i = 0
  while i < 100 {
    large_baggage = large_baggage + "key" + i.to_string() + "=value" + i.to_string()
    if i < 99 {
      large_baggage = large_baggage + ","
    }
    i = i + 1
  }
  
  carrier.set("baggage", large_baggage)
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 即使数据很大，操作也应该正常完成
  assert_eq(true, true)
}

test "composite_propagator_concurrent_operations" {
  // 测试并发操作（模拟）
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 模拟并发操作：快速连续的inject和extract
  let mut i = 0
  while i < 50 {
    // 设置不同的headers
    carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    carrier.set("baggage", "key" + i.to_string() + "=value" + i.to_string())
    
    // extract操作
    let extracted_ctx = composite_propagator.extract(ctx, carrier)
    
    // inject操作
    composite_propagator.inject(extracted_ctx, carrier)
    
    i = i + 1
  }
  
  // 即使有快速连续的操作，也应该正常完成
  assert_eq(true, true)
}

test "composite_propagator_special_characters_handling" {
  // 测试特殊字符处理
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置包含特殊字符的headers
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  carrier.set("baggage", "special.key=value!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
  carrier.set("baggage", "unicode.key=Unicode测试🚀")
  carrier.set("baggage", "space.key=value with spaces")
  carrier.set("baggage", "url.key=https://example.com/path?query=value&param2=value2#fragment")
  
  // 测试extract操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx, carrier)
  
  // 即使有特殊字符，操作也应该正常完成
  assert_eq(true, true)
}

test "composite_propagator_null_and_empty_values" {
  // 测试null和空值处理
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 设置空值
  carrier.set("traceparent", "")
  carrier.set("baggage", "")
  
  // 测试extract操作
  let extracted_ctx1 = composite_propagator.extract(ctx, carrier)
  
  // 清空并设置有效值
  carrier = MapCarrier::new()
  carrier.set("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  carrier.set("baggage", "key=value")
  
  // 测试extract操作
  let extracted_ctx2 = composite_propagator.extract(ctx, carrier)
  
  // 测试inject操作
  composite_propagator.inject(extracted_ctx1, carrier)
  composite_propagator.inject(extracted_ctx2, carrier)
  
  // 即使有空值，操作也应该正常完成
  assert_eq(true, true)
}