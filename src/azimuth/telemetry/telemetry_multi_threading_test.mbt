// 多线程测试 - 模拟并发访问场景
test "multi_threading_concurrent_span_creation" {
  // 模拟并发Span创建场景
  
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let mut spans = []
  
  // 模拟多个"线程"同时创建Span
  let mut thread_id = 0
  while thread_id < 5 {
    let mut span_id = 0
    while span_id < 20 {
      let (_, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
        ctx,
        "thread_" + thread_id.to_string() + "_span_" + span_id.to_string(),
        if thread_id % 2 == 0 { @azimuth.telemetry.api.trace.Server } else { @azimuth.telemetry.api.trace.Client },
        Some([
          ("thread.id", @azimuth.telemetry.api.common.AttributeValue::int(thread_id.to_int64())),
          ("span.id", @azimuth.telemetry.api.common.AttributeValue::int(span_id.to_int64())),
          ("concurrent", @azimuth.telemetry.api.common.AttributeValue::bool(true))
        ])
      )
      spans.push(span)
      span_id = span_id + 1
    }
    thread_id = thread_id + 1
  }
  
  // 验证所有Span都创建成功
  assert_eq(spans.length(), 100)
  
  // 验证不同"线程"的Span
  assert_eq(spans[0].name, "thread_0_span_0")
  assert_eq(spans[19].name, "thread_0_span_19")
  assert_eq(spans[20].name, "thread_1_span_0")
  assert_eq(spans[99].name, "thread_4_span_19")
  
  // 验证Span属性
  match spans[0].attributes[0].1 {
    @azimuth.telemetry.api.common.IntValue(thread_id_attr) => assert_eq(thread_id_attr, 0L)
    _ => @test.fail("Test failed")
  }
  
  match spans[99].attributes[1].1 {
    @azimuth.telemetry.api.common.IntValue(span_id_attr) => assert_eq(span_id_attr, 19L)
    _ => @test.fail("Test failed")
  }
}

test "multi_threading_concurrent_context_operations" {
  // 模拟并发Context操作场景
  
  let mut contexts = []
  let mut keys = []
  
  // 创建多个Context和键
  let mut thread_id = 0
  while thread_id < 10 {
    let ctx = @azimuth.telemetry.api.context.Context::empty()
    let key = @azimuth.telemetry.api.context.create_key("thread_key_" + thread_id.to_string())
    keys.push(key)
    
    // 每个线程执行多次Context操作
    let mut operation_id = 0
    let mut current_ctx = ctx
    while operation_id < 10 {
      current_ctx = current_ctx.with_value(
        key,
        "thread_" + thread_id.to_string() + "_value_" + operation_id.to_string()
      )
      operation_id = operation_id + 1
    }
    contexts.push(current_ctx)
    thread_id = thread_id + 1
  }
  
  // 验证Context操作结果
  assert_eq(contexts.length(), 10)
  assert_eq(keys.length(), 10)
  
  // 验证特定Context的值
  let value_0 = contexts[0].get(keys[0])
  match value_0 {
    Some(v) => assert_eq(v, "thread_0_value_9")  // 最后一个值
    None => @test.fail("Test failed")
  }
  
  let value_9 = contexts[9].get(keys[9])
  match value_9 {
    Some(v) => assert_eq(v, "thread_9_value_9")  // 最后一个值
    None => @test.fail("Test failed")
  }
}

test "multi_threading_concurrent_attribute_operations" {
  // 模拟并发属性操作场景
  
  let mut attribute_sets = []
  
  // 模拟多个"线程"同时创建属性集合
  let mut thread_id = 0
  while thread_id < 10 {
    let mut attributes = []
    let mut attr_id = 0
    while attr_id < 10 {
      attributes.push((
        "thread_" + thread_id.to_string() + "_attr_" + attr_id.to_string(),
        @azimuth.telemetry.api.common.AttributeValue::string("value_" + thread_id.to_string() + "_" + attr_id.to_string())
      ))
      attr_id = attr_id + 1
    }
    attribute_sets.push(attributes)
    thread_id = thread_id + 1
  }
  
  // 验证属性集合创建
  assert_eq(attribute_sets.length(), 10)
  assert_eq(attribute_sets[0].length(), 10)
  assert_eq(attribute_sets[9].length(), 10)
  
  // 验证特定属性
  match attribute_sets[0][0].1 {
    @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value, "value_0_0")
    _ => @test.fail("Test failed")
  }
  
  match attribute_sets[9][9].1 {
    @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value, "value_9_9")
    _ => @test.fail("Test failed")
  }
}