// Span属性和事件管理测试
// 测试Span的高级功能和事件处理

test "Span属性设置和获取测试" {
  // 创建SpanContext和Span
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  let span = Span::new("test-span", Server, span_ctx)
  
  // 测试Span基本属性
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  
  // 测试Span状态管理
  assert_eq(Span::status(span), Unset)
  
  // 测试设置状态为Ok
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // 测试设置状态为Error
  Span::set_status(span, Error, Some("Operation failed with error"))
  
  // 测试Span结束
  Span::end(span)
}

test "Span事件管理测试" {
  // 创建Span
  let span_ctx = SpanContext::new("trace-789", "span-101112", true, "")
  let span = Span::new("event-test-span", Client, span_ctx)
  
  // 测试添加简单事件
  Span::add_event(span, "operation.started", None)
  
  // 测试添加带属性的事件
  let event_attrs = [
    ("operation.type", StringValue("database.query")),
    ("operation.duration_ms", IntValue(150)),
    ("operation.success", BoolValue(true)),
    ("operation.result_count", IntValue(42))
  ]
  Span::add_event(span, "operation.completed", Some(event_attrs))
  
  // 测试添加错误事件
  let error_attrs = [
    ("error.type", StringValue("ConnectionTimeout")),
    ("error.message", StringValue("Database connection timed out after 30 seconds")),
    ("error.retry_count", IntValue(3))
  ]
  Span::add_event(span, "operation.error", Some(error_attrs))
  
  // 测试添加多个事件
  Span::add_event(span, "cache.hit", Some([("cache.key", StringValue("user:123"))]))
  Span::add_event(span, "cache.miss", Some([("cache.key", StringValue("user:456"))]))
  Span::add_event(span, "network.request", Some([
    ("http.method", StringValue("POST")),
    ("http.url", StringValue("https://api.example.com/data")),
    ("http.status_code", IntValue(200))
  ]))
  
  // 结束Span
  Span::end(span)
}

test "不同类型Span的创建和管理测试" {
  let span_ctx = SpanContext::new("trace-types", "span-types", true, "")
  
  // 测试Internal Span
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  assert_eq(Span::kind(internal_span), Internal)
  Span::add_event(internal_span, "internal.processing", None)
  Span::end(internal_span)
  
  // 测试Server Span
  let server_span = Span::new("api-request", Server, span_ctx)
  assert_eq(Span::kind(server_span), Server)
  Span::add_event(server_span, "request.received", Some([
    ("http.method", StringValue("GET")),
    ("http.path", StringValue("/api/users"))
  ]))
  Span::set_status(server_span, Ok, Some("Request processed successfully"))
  Span::end(server_span)
  
  // 测试Client Span
  let client_span = Span::new("external-api-call", Client, span_ctx)
  assert_eq(Span::kind(client_span), Client)
  Span::add_event(client_span, "request.sent", Some([
    ("service.name", StringValue("payment-service")),
    ("endpoint", StringValue("/process-payment"))
  ]))
  Span::end(client_span)
  
  // 测试Producer Span
  let producer_span = Span::new("message-producer", Producer, span_ctx)
  assert_eq(Span::kind(producer_span), Producer)
  Span::add_event(producer_span, "message.published", Some([
    ("topic", StringValue("user-events")),
    ("message.id", StringValue("msg-12345")),
    ("message.size", IntValue(1024))
  ]))
  Span::end(producer_span)
  
  // 测试Consumer Span
  let consumer_span = Span::new("message-consumer", Consumer, span_ctx)
  assert_eq(Span::kind(consumer_span), Consumer)
  Span::add_event(consumer_span, "message.received", Some([
    ("topic", StringValue("user-events")),
    ("message.id", StringValue("msg-12345")),
    ("consumer.group", StringValue("user-processor"))
  ]))
  Span::end(consumer_span)
}

test "Span嵌套和父子关系测试" {
  // 创建父Span
  let parent_ctx = SpanContext::new("trace-nested", "span-parent", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_ctx)
  
  // 在父Span中添加事件
  Span::add_event(parent_span, "parent.started", None)
  
  // 模拟子Span操作（在实际实现中，子Span应该有父子关系）
  let child1_ctx = SpanContext::new("trace-nested", "span-child1", true, "")
  let child1_span = Span::new("child-operation-1", Internal, child1_ctx)
  Span::add_event(child1_span, "child1.started", None)
  Span::add_event(child1_span, "child1.processing", Some([
    ("step", IntValue(1)),
    ("operation", StringValue("validation"))
  ]))
  Span::end(child1_span)
  
  // 继续父Span操作
  Span::add_event(parent_span, "parent.child1_completed", None)
  
  // 创建第二个子Span
  let child2_ctx = SpanContext::new("trace-nested", "span-child2", true, "")
  let child2_span = Span::new("child-operation-2", Internal, child2_ctx)
  Span::add_event(child2_span, "child2.started", None)
  Span::add_event(child2_span, "child2.processing", Some([
    ("step", IntValue(2)),
    ("operation", StringValue("transformation"))
  ]))
  Span::set_status(child2_span, Ok, Some("Child operation completed"))
  Span::end(child2_span)
  
  // 完成父Span
  Span::add_event(parent_span, "parent.all_children_completed", None)
  Span::set_status(parent_span, Ok, Some("Parent operation completed successfully"))
  Span::end(parent_span)
}

test "Span错误处理和异常场景测试" {
  let span_ctx = SpanContext::new("trace-error", "span-error", true, "")
  
  // 测试正常流程中的错误处理
  let normal_span = Span::new("normal-operation", Internal, span_ctx)
  Span::add_event(normal_span, "operation.started", None)
  Span::add_event(normal_span, "operation.warning", Some([
    ("warning.type", StringValue("PerformanceDegradation")),
    ("warning.message", StringValue("Operation taking longer than expected"))
  ]))
  Span::set_status(normal_span, Ok, Some("Operation completed with warnings"))
  Span::end(normal_span)
  
  // 测试错误流程
  let error_span = Span::new("error-operation", Internal, span_ctx)
  Span::add_event(error_span, "operation.started", None)
  Span::add_event(error_span, "operation.error", Some([
    ("error.type", StringValue("ValidationError")),
    ("error.message", StringValue("Invalid input parameter")),
    ("error.code", IntValue(400))
  ]))
  Span::set_status(error_span, Error, Some("Operation failed due to validation error"))
  Span::end(error_span)
  
  // 测试异常流程
  let exception_span = Span::new("exception-operation", Internal, span_ctx)
  Span::add_event(exception_span, "operation.started", None)
  Span::add_event(exception_span, "operation.exception", Some([
    ("exception.type", StringValue("NullPointerException")),
    ("exception.message", StringValue("Unexpected null reference")),
    ("exception.stack_trace", StringValue("at com.example.Service.process(Service.java:123)"))
  ]))
  Span::set_status(exception_span, Error, Some("Operation failed due to unexpected exception"))
  Span::end(exception_span)
  
  // 测试超时场景
  let timeout_span = Span::new("timeout-operation", Internal, span_ctx)
  Span::add_event(timeout_span, "operation.started", None)
  Span::add_event(timeout_span, "operation.timeout", Some([
    ("timeout.duration_ms", IntValue(30000)),
    ("timeout.reason", StringValue("External service not responding"))
  ]))
  Span::set_status(timeout_span, Error, Some("Operation timed out"))
  Span::end(timeout_span)
}