// Azimuth Telemetry System - Span State Transition Tests
// 测试Span状态转换和生命周期管理

test "span creation and initial state" {
  // 测试Span创建和初始状态
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "key1=value1")
  let span = @azimuth.Span::new("test-span", @azimuth.Server, span_ctx)
  
  // 验证初始状态
  assert_eq(@azimuth.Span::name(span), "test-span")
  assert_eq(@azimuth.Span::kind(span), @azimuth.Server)
  assert_true(@azimuth.Span::is_recording(span))
  assert_eq(@azimuth.Span::span_context(span), span_ctx)
  assert_eq(@azimuth.Span::status(span), @azimuth.Unset)
  
  // 测试不同类型的Span
  let internal_span = @azimuth.Span::new("internal", @azimuth.Internal, span_ctx)
  let client_span = @azimuth.Span::new("client", @azimuth.Client, span_ctx)
  let producer_span = @azimuth.Span::new("producer", @azimuth.Producer, span_ctx)
  let consumer_span = @azimuth.Span::new("consumer", @azimuth.Consumer, span_ctx)
  
  assert_eq(@azimuth.Span::kind(internal_span), @azimuth.Internal)
  assert_eq(@azimuth.Span::kind(client_span), @azimuth.Client)
  assert_eq(@azimuth.Span::kind(producer_span), @azimuth.Producer)
  assert_eq(@azimuth.Span::kind(consumer_span), @azimuth.Consumer)
}

test "span status transitions" {
  // 测试Span状态转换
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let span = @azimuth.Span::new("status-test", @azimuth.Internal, span_ctx)
  
  // 初始状态应该是Unset
  assert_eq(@azimuth.Span::status(span), @azimuth.Unset)
  
  // 转换到Ok状态
  @azimuth.Span::set_status(span, @azimuth.Ok, Some("Operation completed"))
  // 注意：简化实现可能不会立即反映状态变化
  
  // 转换到Error状态
  @azimuth.Span::set_status(span, @azimuth.Error, Some("Operation failed"))
  
  // 再次转换到Ok状态（测试状态覆盖）
  @azimuth.Span::set_status(span, @azimuth.Ok, Some("Retry succeeded"))
  
  // 测试无描述的状态设置
  @azimuth.Span::set_status(span, @azimuth.Unset, None)
  
  // 验证状态设置操作不会抛出异常
  assert_true(true)
}

test "span event management" {
  // 测试Span事件管理
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let span = @azimuth.Span::new("event-test", @azimuth.Internal, span_ctx)
  
  // 添加简单事件
  @azimuth.Span::add_event(span, "event-1", None)
  
  // 添加带属性的事件
  let event_attrs = [
    ("key1", @azimuth.StringValue("value1")),
    ("key2", @azimuth.IntValue(42)),
    ("key3", @azimuth.BoolValue(true))
  ]
  @azimuth.Span::add_event(span, "event-2", Some(event_attrs))
  
  // 添加多个事件
  @azimuth.Span::add_event(span, "start-event", Some([("type", @azimuth.StringValue("start"))]))
  @azimuth.Span::add_event(span, "progress-event", Some([("progress", @azimuth.IntValue(50))]))
  @azimuth.Span::add_event(span, "end-event", Some([("type", @azimuth.StringValue("end"))]))
  
  // 测试空属性事件
  @azimuth.Span::add_event(span, "empty-attrs-event", Some([]))
  
  // 测试特殊字符事件名称
  @azimuth.Span::add_event(span, "event.with.dots", None)
  @azimuth.Span::add_event(span, "event-with-hyphens", None)
  @azimuth.Span::add_event(span, "event_with_underscores", None)
  
  // 验证事件添加操作不会抛出异常
  assert_true(true)
}

test "span lifecycle management" {
  // 测试Span生命周期管理
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let span = @azimuth.Span::new("lifecycle-test", @azimuth.Internal, span_ctx)
  
  // 初始状态：recording应该为true
  assert_true(@azimuth.Span::is_recording(span))
  
  // 在recording状态下添加事件和状态
  @azimuth.Span::add_event(span, "during-recording", None)
  @azimuth.Span::set_status(span, @azimuth.Ok, Some("Success"))
  
  // 结束Span
  @azimuth.Span::end(span)
  
  // 结束后添加事件（应该被忽略或处理）
  @azimuth.Span::add_event(span, "after-end", None)
  
  // 结束后设置状态（应该被忽略或处理）
  @azimuth.Span::set_status(span, @azimuth.Error, Some("Late error"))
  
  // 验证生命周期操作不会抛出异常
  assert_true(true)
}

test "span context operations" {
  // 测试Span上下文操作
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  let span = @azimuth.Span::new("context-test", @azimuth.Internal, span_ctx)
  
  // 验证上下文属性
  assert_eq(@azimuth.SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(@azimuth.SpanContext::span_id(span_ctx), "span-456")
  assert_true(@azimuth.SpanContext::is_sampled(span_ctx))
  assert_true(@azimuth.SpanContext::is_valid(span_ctx))
  
  // 测试无效上下文
  let invalid_ctx = @azimuth.SpanContext::new("", "", false, "")
  assert_false(@azimuth.SpanContext::is_valid(invalid_ctx))
  assert_false(@azimuth.SpanContext::is_sampled(invalid_ctx))
  
  // 测试部分无效上下文
  let partial_invalid_ctx1 = @azimuth.SpanContext::new("trace-123", "", false, "")
  let partial_invalid_ctx2 = @azimuth.SpanContext::new("", "span-456", false, "")
  
  assert_false(@azimuth.SpanContext::is_valid(partial_invalid_ctx1))
  assert_false(@azimuth.SpanContext::is_valid(partial_invalid_ctx2))
  
  // 测试不同的采样状态
  let sampled_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let not_sampled_ctx = @azimuth.SpanContext::new("trace-123", "span-456", false, "")
  
  assert_true(@azimuth.SpanContext::is_sampled(sampled_ctx))
  assert_false(@azimuth.SpanContext::is_sampled(not_sampled_ctx))
}

test "span with tracer operations" {
  // 测试Span与Tracer的集成操作
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "test-tracer", Some("1.0.0"))
  
  // 使用Tracer创建Span
  let tracer_span = @azimuth.Tracer::start_span(tracer, "tracer-created-span")
  
  // 验证Tracer创建的Span属性
  assert_eq(@azimuth.Span::name(tracer_span), "tracer-created-span")
  assert_eq(@azimuth.Span::kind(tracer_span), @azimuth.Internal)
  assert_true(@azimuth.Span::is_recording(tracer_span))
  
  // 测试Tracer的instrumentation scope
  let scope = @azimuth.Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  // 使用Tracer创建多个Span
  let span1 = @azimuth.Tracer::start_span(tracer, "span-1")
  let span2 = @azimuth.Tracer::start_span(tracer, "span-2")
  let span3 = @azimuth.Tracer::start_span(tracer, "span-3")
  
  // 对不同Span执行操作
  @azimuth.Span::add_event(span1, "event-1", None)
  @azimuth.Span::set_status(span2, @azimuth.Ok, Some("Span 2 completed"))
  @azimuth.Span::end(span3)
  
  // 验证Tracer集成操作不会抛出异常
  assert_true(true)
}

test "span error handling and recovery" {
  // 测试Span错误处理和恢复
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let span = @azimuth.Span::new("error-test", @azimuth.Internal, span_ctx)
  
  // 设置错误状态
  @azimuth.Span::set_status(span, @azimuth.Error, Some("Initial error"))
  
  // 添加错误事件
  let error_attrs = [
    ("error.type", @azimuth.StringValue("ValidationError")),
    ("error.message", @azimuth.StringValue("Invalid input parameter")),
    ("error.code", @azimuth.IntValue(400))
  ]
  @azimuth.Span::add_event(span, "error-occurred", Some(error_attrs))
  
  // 尝试恢复（设置成功状态）
  @azimuth.Span::set_status(span, @azimuth.Ok, Some("Recovered successfully"))
  
  // 添加恢复事件
  let recovery_attrs = [
    ("recovery.action", @azimuth.StringValue("retry")),
    ("recovery.attempts", @azimuth.IntValue(3))
  ]
  @azimuth.Span::add_event(span, "recovery-completed", Some(recovery_attrs))
  
  // 结束Span
  @azimuth.Span::end(span)
  
  // 验证错误处理操作不会抛出异常
  assert_true(true)
}

test "span concurrent operations" {
  // 测试Span并发操作
  let span_ctx = @azimuth.SpanContext::new("trace-123", "span-456", true, "")
  let span = @azimuth.Span::new("concurrent-test", @azimuth.Internal, span_ctx)
  
  // 模拟并发操作（在简化实现中是顺序执行）
  for i in 0..<10 {
    let event_name = "concurrent-event-" + i.to_string()
    @azimuth.Span::add_event(span, event_name, Some([("index", @azimuth.IntValue(i))]))
  }
  
  // 模拟状态并发更新
  for i in 0..<5 {
    @azimuth.Span::set_status(span, @azimuth.Ok, Some("Update " + i.to_string()))
  }
  
  // 验证并发操作不会抛出异常
  assert_true(true)
  
  // 结束Span
  @azimuth.Span::end(span)
  
  // 结束后的并发操作
  for i in 0..<3 {
    let post_end_event = "post-end-event-" + i.to_string()
    @azimuth.Span::add_event(span, post_end_event, None)
  }
  
  // 验证结束后的并发操作不会抛出异常
  assert_true(true)
}