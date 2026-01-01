// Comprehensive Span lifecycle management tests for Azimuth telemetry system
import "azimuth/azimuth"

// Test 1: Basic Span lifecycle
pub test "基本Span生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
  
  // 创建Span
  let span = azimuth::Tracer::start_span(tracer, "lifecycle-operation")
  
  // 验证Span初始状态
  assert_eq(azimuth::Span::name(span), "lifecycle-operation")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  // 设置状态
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span结束后状态
  assert_false(azimuth::Span::is_recording(span))
}

// Test 2: Span with different kinds
pub test "不同类型Span生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "span-kind-test")
  
  // Server Span
  let server_span = azimuth::Tracer::start_span(tracer, "server-operation")
  let server_span_with_kind = azimuth::Span::new("server-operation", azimuth::Server, azimuth::Span::span_context(server_span))
  assert_eq(azimuth::Span::kind(server_span_with_kind), azimuth::Server)
  azimuth::Span::end(server_span_with_kind)
  
  // Client Span
  let client_span = azimuth::Tracer::start_span(tracer, "client-operation")
  let client_span_with_kind = azimuth::Span::new("client-operation", azimuth::Client, azimuth::Span::span_context(client_span))
  assert_eq(azimuth::Span::kind(client_span_with_kind), azimuth::Client)
  azimuth::Span::end(client_span_with_kind)
  
  // Producer Span
  let producer_span = azimuth::Tracer::start_span(tracer, "producer-operation")
  let producer_span_with_kind = azimuth::Span::new("producer-operation", azimuth::Producer, azimuth::Span::span_context(producer_span))
  assert_eq(azimuth::Span::kind(producer_span_with_kind), azimuth::Producer)
  azimuth::Span::end(producer_span_with_kind)
  
  // Consumer Span
  let consumer_span = azimuth::Tracer::start_span(tracer, "consumer-operation")
  let consumer_span_with_kind = azimuth::Span::new("consumer-operation", azimuth::Consumer, azimuth::Span::span_context(consumer_span))
  assert_eq(azimuth::Span::kind(consumer_span_with_kind), azimuth::Consumer)
  azimuth::Span::end(consumer_span_with_kind)
}

// Test 3: Span parent-child relationships
pub test "Span父子关系生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "parent-child-test")
  
  // 创建父Span
  let parent_span = azimuth::Tracer::start_span(tracer, "parent-operation")
  let parent_ctx = azimuth::Span::span_context(parent_span)
  
  // 创建子Span
  let child_span1 = azimuth::Tracer::start_span(tracer, "child-operation-1")
  let child_span2 = azimuth::Tracer::start_span(tracer, "child-operation-2")
  
  // 验证父子关系
  assert_eq(azimuth::Span::name(parent_span), "parent-operation")
  assert_eq(azimuth::Span::name(child_span1), "child-operation-1")
  assert_eq(azimuth::Span::name(child_span2), "child-operation-2")
  
  // 添加事件到父Span
  azimuth::Span::add_event(parent_span, "parent.start", Some([("operation", azimuth::StringValue("parent"))]))
  
  // 添加事件到子Span
  azimuth::Span::add_event(child_span1, "child.start", Some([("operation", azimuth::StringValue("child1"))]))
  azimuth::Span::add_event(child_span2, "child.start", Some([("operation", azimuth::StringValue("child2"))]))
  
  // 结束子Span
  azimuth::Span::end(child_span1)
  azimuth::Span::end(child_span2)
  
  // 添加结束事件到父Span
  azimuth::Span::add_event(parent_span, "parent.children.completed", None)
  
  // 结束父Span
  azimuth::Span::end(parent_span)
}

// Test 4: Span attribute management
pub test "Span属性管理生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "attribute-test")
  
  let span = azimuth::Tracer::start_span(tracer, "attribute-operation")
  
  // 添加属性
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue("user123"))
  azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("test"))
  azimuth::Attributes::set(attrs, "retry.count", azimuth::IntValue(3))
  
  // 添加事件与属性
  azimuth::Span::add_event(span, "operation.start", Some(attrs))
  
  // 更新属性
  let updated_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(updated_attrs, "operation.status", azimuth::StringValue("in_progress"))
  azimuth::Attributes::set(updated_attrs, "progress.percentage", azimuth::FloatValue(50.0))
  
  azimuth::Span::add_event(span, "operation.progress", Some(updated_attrs))
  
  // 最终属性
  let final_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(final_attrs, "operation.status", azimuth::StringValue("completed"))
  azimuth::Attributes::set(final_attrs, "progress.percentage", azimuth::FloatValue(100.0))
  azimuth::Attributes::set(final_attrs, "duration.ms", azimuth::IntValue(150))
  
  azimuth::Span::add_event(span, "operation.complete", Some(final_attrs))
  
  // 设置最终状态
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 结束Span
  azimuth::Span::end(span)
}

// Test 5: Span error handling lifecycle
pub test "Span错误处理生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-test")
  
  let span = azimuth::Tracer::start_span(tracer, "error-operation")
  
  // 添加开始事件
  azimuth::Span::add_event(span, "operation.start", None)
  
  // 模拟错误情况
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("ConnectionTimeout"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Database connection timeout after 30 seconds"))
  azimuth::Attributes::set(error_attrs, "retry.attempt", azimuth::IntValue(3))
  
  // 添加错误事件
  azimuth::Span::add_event(span, "error.occurred", Some(error_attrs))
  
  // 设置错误状态
  azimuth::Span::set_status(span, azimuth::Error)
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span已结束
  assert_false(azimuth::Span::is_recording(span))
}

// Test 6: Long-running Span lifecycle
pub test "长时间运行Span生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "long-running-test")
  
  let span = azimuth::Tracer::start_span(tracer, "long-running-operation")
  
  // 模拟长时间运行的操作
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 添加进度事件
  for i in 1..6 {
    let progress_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(progress_attrs, "progress.step", azimuth::IntValue(i))
    azimuth::Attributes::set(progress_attrs, "progress.percentage", azimuth::FloatValue(i.to_double() * 20.0))
    
    azimuth::Span::add_event(span, "progress.update", Some(progress_attrs))
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 添加完成事件
  let completion_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(completion_attrs, "duration.nanos", azimuth::IntValue(duration.to_int()))
  azimuth::Attributes::set(completion_attrs, "total.steps", azimuth::IntValue(5))
  
  azimuth::Span::add_event(span, "operation.completed", Some(completion_attrs))
  
  // 设置成功状态
  azimuth::Span::set_status(span, azimuth::Ok)
  
  // 结束Span
  azimuth::Span::end(span)
}

// Test 7: Span context propagation
pub test "Span上下文传播生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "context-propagation-test")
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "root-operation")
  let root_ctx = azimuth::Span::span_context(root_span)
  
  // 验证根Span上下文
  assert_true(azimuth::SpanContext::is_valid(root_ctx))
  assert_true(azimuth::SpanContext::is_sampled(root_ctx))
  
  // 添加根Span事件
  azimuth::Span::add_event(root_span, "root.started", None)
  
  // 创建子Span
  let child_span = azimuth::Tracer::start_span(tracer, "child-operation")
  let child_ctx = azimuth::Span::span_context(child_span)
  
  // 验证子Span上下文
  assert_true(azimuth::SpanContext::is_valid(child_ctx))
  assert_true(azimuth::SpanContext::is_sampled(child_ctx))
  
  // 添加子Span事件
  azimuth::Span::add_event(child_span, "child.started", Some([("parent.trace", azimuth::StringValue(azimuth::SpanContext::trace_id(root_ctx)))]))
  
  // 创建孙Span
  let grandchild_span = azimuth::Tracer::start_span(tracer, "grandchild-operation")
  let grandchild_ctx = azimuth::Span::span_context(grandchild_span)
  
  // 添加孙Span事件
  azimuth::Span::add_event(grandchild_span, "grandchild.started", Some([
    ("parent.trace", azimuth::StringValue(azimuth::SpanContext::trace_id(child_ctx))),
    ("root.trace", azimuth::StringValue(azimuth::SpanContext::trace_id(root_ctx)))
  ]))
  
  // 按顺序结束Span（自底向上）
  azimuth::Span::end(grandchild_span)
  azimuth::Span::add_event(child_span, "child.completed", None)
  azimuth::Span::end(child_span)
  azimuth::Span::add_event(root_span, "root.completed", None)
  azimuth::Span::end(root_span)
  
  // 验证所有Span都已结束
  assert_false(azimuth::Span::is_recording(grandchild_span))
  assert_false(azimuth::Span::is_recording(child_span))
  assert_false(azimuth::Span::is_recording(root_span))
}

// Test 8: Span resource cleanup
pub test "Span资源清理生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "cleanup-test")
  
  // 创建多个Span
  let spans = []
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "cleanup-operation-" + i.to_string())
    
    // 为每个Span添加属性和事件
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "span.index", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("cleanup"))
    
    azimuth::Span::add_event(span, "span.initialized", Some(attrs))
    spans.push(span)
  }
  
  // 验证所有Span都在记录状态
  for span in spans {
    assert_true(azimuth::Span::is_recording(span))
  }
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::add_event(span, "span.finalized", None)
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
  }
  
  // 验证所有Span都已结束且资源已清理
  for span in spans {
    assert_false(azimuth::Span::is_recording(span))
  }
}