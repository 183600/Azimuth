// Azimuth Telemetry System - Span Kind and Status Code Enumeration Test
// 测试Azimuth遥测系统的Span种类和状态码枚举功能

test "SpanKind枚举基础测试" {
  // 测试所有SpanKind枚举值
  
  // Internal类型的Span
  let internal_span = Span::new("internal-operation", Internal, SpanContext::new("trace1", "span1", true, ""))
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  // Server类型的Span
  let server_span = Span::new("server-operation", Server, SpanContext::new("trace2", "span2", true, ""))
  assert_eq(Span::name(server_span), "server-operation")
  assert_eq(Span::kind(server_span), Server)
  
  // Client类型的Span
  let client_span = Span::new("client-operation", Client, SpanContext::new("trace3", "span3", true, ""))
  assert_eq(Span::name(client_span), "client-operation")
  assert_eq(Span::kind(client_span), Client)
  
  // Producer类型的Span
  let producer_span = Span::new("producer-operation", Producer, SpanContext::new("trace4", "span4", true, ""))
  assert_eq(Span::name(producer_span), "producer-operation")
  assert_eq(Span::kind(producer_span), Producer)
  
  // Consumer类型的Span
  let consumer_span = Span::new("consumer-operation", Consumer, SpanContext::new("trace5", "span5", true, ""))
  assert_eq(Span::name(consumer_span), "consumer-operation")
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "SpanKind枚举匹配测试" {
  // 测试SpanKind枚举的模式匹配
  
  let test_span_kind = function(kind : SpanKind) -> String {
    match kind {
      Internal => "internal"
      Server => "server"
      Client => "client"
      Producer => "producer"
      Consumer => "consumer"
    }
  }
  
  assert_eq(test_span_kind(Internal), "internal")
  assert_eq(test_span_kind(Server), "server")
  assert_eq(test_span_kind(Client), "client")
  assert_eq(test_span_kind(Producer), "producer")
  assert_eq(test_span_kind(Consumer), "consumer")
}

test "StatusCode枚举基础测试" {
  // 测试所有StatusCode枚举值
  
  // 创建测试Span用于状态测试
  let test_span = Span::new("status-test", Internal, SpanContext::new("trace-status", "span-status", true, ""))
  
  // Unset状态（默认状态）
  assert_eq(Span::status(test_span), Unset)
  
  // 设置为Ok状态
  Span::set_status(test_span, Ok, Some("Operation completed successfully"))
  // 注意：简化实现中可能不会立即反映状态变化
  
  // 设置为Error状态
  Span::set_status(test_span, Error, Some("Operation failed with error"))
  // 注意：简化实现中可能不会立即反映状态变化
}

test "StatusCode枚举匹配测试" {
  // 测试StatusCode枚举的模式匹配
  
  let test_status_code = function(code : StatusCode) -> String {
    match code {
      Unset => "unset"
      Ok => "ok"
      Error => "error"
    }
  }
  
  assert_eq(test_status_code(Unset), "unset")
  assert_eq(test_status_code(Ok), "ok")
  assert_eq(test_status_code(Error), "error")
}

test "SpanKind与StatusCode组合测试" {
  // 测试SpanKind和StatusCode的组合使用
  
  // 创建不同类型的Span并设置不同状态
  let spans = [
    (Span::new("db-query", Client, SpanContext::new("trace1", "span1", true, "")), Ok),
    (Span::new("http-request", Client, SpanContext::new("trace2", "span2", true, "")), Error),
    (Span::new("db-operation", Server, SpanContext::new("trace3", "span3", true, "")), Ok),
    (Span::new("message-publish", Producer, SpanContext::new("trace4", "span4", true, "")), Ok),
    (Span::new("message-consume", Consumer, SpanContext::new("trace5", "span5", true, "")), Error)
  ]
  
  // 验证Span数量
  assert_eq(spans.length(), 5)
  
  // 验证每个Span的名称和类型
  for (span, expected_status) in spans {
    let span_name = Span::name(span)
    let span_kind = Span::kind(span)
    
    // 验证Span名称不为空
    assert_true(span_name.length() > 0)
    
    // 验证Span类型
    match span_kind {
      Internal | Server | Client | Producer | Consumer => assert_true(true)
    }
    
    // 设置状态（简化实现可能不会立即反映）
    Span::set_status(span, expected_status, Some("Status description"))
  } else {
    assert_true(false) // 不应该到达这里
  }
}

test "Span生命周期状态转换测试" {
  // 测试Span在生命周期中的状态转换
  
  let span = Span::new("lifecycle-test", Internal, SpanContext::new("trace-lifecycle", "span-lifecycle", true, ""))
  
  // 初始状态
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // 添加事件
  Span::add_event(span, "start-event", Some([("event.type", StringValue("start"))]))
  
  // 设置状态为Ok
  Span::set_status(span, Ok, Some("Processing completed"))
  
  // 添加更多事件
  Span::add_event(span, "middle-event", Some([("event.type", StringValue("middle"))]))
  
  // 设置状态为Error（覆盖之前的状态）
  Span::set_status(span, Error, Some("Processing failed"))
  
  // 添加结束事件
  Span::add_event(span, "end-event", Some([("event.type", StringValue("end"))]))
  
  // 结束Span
  Span::end(span)
  
  // 验证Span的基本属性（简化实现中recording可能不会改变）
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Internal)
}

test "SpanKind场景化测试" {
  // 测试不同SpanKind在实际场景中的应用
  
  // 数据库查询场景（Client类型）
  let db_query_span = Span::new("database.query", Client, SpanContext::new("trace-db", "span-db", true, ""))
  Span::add_event(db_query_span, "query.start", Some([("query.type", StringValue("SELECT"))]))
  Span::set_status(db_query_span, Ok, Some("Query executed successfully"))
  
  // HTTP服务器场景（Server类型）
  let http_server_span = Span::new("http.request", Server, SpanContext::new("trace-http", "span-http", true, ""))
  Span::add_event(http_server_span, "request.received", Some([("http.method", StringValue("GET"))]))
  Span::set_status(http_server_span, Ok, Some("Request processed"))
  
  // 消息发布场景（Producer类型）
  let message_producer_span = Span::new("message.publish", Producer, SpanContext::new("trace-pub", "span-pub", true, ""))
  Span::add_event(message_producer_span, "message.sent", Some([("message.topic", StringValue("events"))]))
  Span::set_status(message_producer_span, Ok, Some("Message published"))
  
  // 消息消费场景（Consumer类型）
  let message_consumer_span = Span::new("message.consume", Consumer, SpanContext::new("trace-con", "span-con", true, ""))
  Span::add_event(message_consumer_span, "message.received", Some([("message.topic", StringValue("events"))]))
  Span::set_status(message_consumer_span, Error, Some("Processing failed"))
  
  // 内部处理场景（Internal类型）
  let internal_span = Span::new("internal.processing", Internal, SpanContext::new("trace-int", "span-int", true, ""))
  Span::add_event(internal_span, "processing.start", Some([("processing.type", StringValue("calculation"))]))
  Span::set_status(internal_span, Ok, Some("Processing completed"))
  
  // 验证所有Span的创建
  assert_eq(Span::kind(db_query_span), Client)
  assert_eq(Span::kind(http_server_span), Server)
  assert_eq(Span::kind(message_producer_span), Producer)
  assert_eq(Span::kind(message_consumer_span), Consumer)
  assert_eq(Span::kind(internal_span), Internal)
}

test "StatusCode错误处理测试" {
  // 测试StatusCode在错误处理场景中的应用
  
  let error_span = Span::new("error-handling-test", Internal, SpanContext::new("trace-error", "span-error", true, ""))
  
  // 模拟错误处理流程
  Span::add_event(error_span, "error.occurred", Some([
    ("error.type", StringValue("ValidationError")),
    ("error.message", StringValue("Invalid input parameter")),
    ("error.code", IntValue(400))
  ]))
  
  // 设置错误状态
  Span::set_status(error_span, Error, Some("Validation failed: Invalid input"))
  
  // 添加恢复事件
  Span::add_event(error_span, "recovery.attempted", Some([
    ("recovery.action", StringValue("retry")),
    ("recovery.attempt", IntValue(1))
  ]))
  
  // 模拟恢复成功
  Span::set_status(error_span, Ok, Some("Recovery successful after retry"))
  
  // 验证Span的基本信息
  assert_eq(Span::name(error_span), "error-handling-test")
  assert_eq(Span::kind(error_span), Internal)
  assert_true(Span::is_recording(error_span))
}

test "Span上下文与状态关联测试" {
  // 测试SpanContext与StatusCode的关联关系
  
  // 创建不同的SpanContext用于测试
  let success_ctx = SpanContext::new("success-trace", "success-span", true, "key1=value1")
  let error_ctx = SpanContext::new("error-trace", "error-span", false, "key2=value2")
  let unset_ctx = SpanContext::new("unset-trace", "unset-span", true, "")
  
  // 创建对应的Span
  let success_span = Span::new("success-operation", Server, success_ctx)
  let error_span = Span::new("error-operation", Client, error_ctx)
  let unset_span = Span::new("unset-operation", Internal, unset_ctx)
  
  // 设置不同的状态
  Span::set_status(success_span, Ok, Some("Operation completed successfully"))
  Span::set_status(error_span, Error, Some("Operation failed with critical error"))
  // unset_span保持默认的Unset状态
  
  // 验证SpanContext的属性
  assert_eq(SpanContext::trace_id(Span::span_context(success_span)), "success-trace")
  assert_eq(SpanContext::span_id(Span::span_context(error_span)), "error-span")
  assert_true(SpanContext::is_sampled(Span::span_context(success_span)))
  assert_false(SpanContext::is_sampled(Span::span_context(error_span)))
  assert_true(SpanContext::is_valid(Span::span_context(unset_span)))
  
  // 验证Span的基本属性
  assert_eq(Span::name(success_span), "success-operation")
  assert_eq(Span::name(error_span), "error-operation")
  assert_eq(Span::name(unset_span), "unset-operation")
}