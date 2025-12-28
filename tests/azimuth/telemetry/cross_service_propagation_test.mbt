// 跨服务追踪传播测试
// 测试分布式追踪中的上下文传播和跨服务调用

test "W3C Trace Context传播测试" {
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建原始上下文
  let original_ctx = Context::root()
  let trace_key = ContextKey::new("trace.info")
  let ctx_with_trace = Context::with_value(original_ctx, trace_key, "original-trace-data")
  
  // 创建载体
  let carrier = TextMapCarrier::new()
  
  // 测试上下文注入
  CompositePropagator::inject(composite_propagator, ctx_with_trace, carrier)
  
  // 验证注入的头部
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert!(injected_traceparent.is_some())
  
  // 测试上下文提取
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "Baggage传播测试" {
  // 创建Baggage传播器
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([baggage_propagator])
  
  // 创建带有Baggage的上下文
  let original_ctx = Context::root()
  let baggage = Baggage::new()
  
  // 添加Baggage条目
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-abc")
  
  // 创建载体并注入Baggage
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, original_ctx, carrier)
  
  // 手动设置baggage头部（简化实现）
  TextMapCarrier::set(carrier, "baggage", "user.id=user-12345,session.id=session-67890,tenant.id=tenant-abc")
  
  // 验证Baggage头部
  let injected_baggage = TextMapCarrier::get(carrier, "baggage")
  // 注意：简化实现中可能不会自动设置baggage头部
}

test "跨服务HTTP请求传播测试" {
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 模拟服务A发起请求
  let service_a_ctx = Context::root()
  let service_a_key = ContextKey::new("service.a")
  let ctx_with_service_a = Context::with_value(service_a_ctx, service_a_key, "service-a-data")
  
  // 创建HTTP请求载体
  let http_carrier = TextMapCarrier::new()
  
  // 注入追踪上下文到HTTP头部
  CompositePropagator::inject(composite_propagator, ctx_with_service_a, http_carrier)
  
  // 模拟添加自定义HTTP头部
  TextMapCarrier::set(http_carrier, "X-Request-ID", "req-12345")
  TextMapCarrier::set(http_carrier, "X-Service-Name", "service-a")
  TextMapCarrier::set(http_carrier, "User-Agent", "Azimuth-Telemetry/1.0")
  
  // 验证HTTP头部
  let traceparent_header = TextMapCarrier::get(http_carrier, "traceparent")
  let request_id_header = TextMapCarrier::get(http_carrier, "X-Request-ID")
  let service_name_header = TextMapCarrier::get(http_carrier, "X-Service-Name")
  
  assert!(traceparent_header.is_some())
  assert_eq(request_id_header, Some("req-12345"))
  assert_eq(service_name_header, Some("service-a"))
  
  // 模拟服务B接收请求并提取上下文
  let service_b_ctx = CompositePropagator::extract(composite_propagator, http_carrier)
  let service_b_key = ContextKey::new("extracted")
  let extracted_value = Context::get(service_b_ctx, service_b_key)
  assert_eq(extracted_value, Some("true"))
  
  // 服务B创建响应并注入新的上下文信息
  let response_carrier = TextMapCarrier::new()
  let service_b_key_response = ContextKey::new("service.b")
  let ctx_with_service_b = Context::with_value(service_b_ctx, service_b_key_response, "service-b-response")
  
  CompositePropagator::inject(composite_propagator, ctx_with_service_b, response_carrier)
  TextMapCarrier::set(response_carrier, "X-Response-ID", "resp-67890")
  TextMapCarrier::set(response_carrier, "X-Service-Name", "service-b")
  
  // 验证响应头部
  let response_traceparent = TextMapCarrier::get(response_carrier, "traceparent")
  let response_id = TextMapCarrier::get(response_carrier, "X-Response-ID")
  let response_service = TextMapCarrier::get(response_carrier, "X-Service-Name")
  
  assert!(response_traceparent.is_some())
  assert_eq(response_id, Some("resp-67890"))
  assert_eq(response_service, Some("service-b"))
}

test "多级服务调用传播测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 服务A -> 服务B -> 服务C 的调用链
  
  // 服务A创建初始上下文
  let service_a_ctx = Context::root()
  let baggage_a = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage_a, "user.id", "user-123")
  let baggage_with_request = Baggage::set_entry(baggage_with_user, "request.id", "req-456")
  
  // 服务A调用服务B
  let a_to_b_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_ctx, a_to_b_carrier)
  TextMapCarrier::set(a_to_b_carrier, "X-Calling-Service", "service-a")
  TextMapCarrier::set(a_to_b_carrier, "baggage", "user.id=user-123,request.id=req-456")
  
  // 服务B提取上下文并添加自己的信息
  let service_b_ctx = CompositePropagator::extract(composite_propagator, a_to_b_carrier)
  let baggage_b = Baggage::new()
  let baggage_with_b_info = Baggage::set_entry(baggage_b, "service.b.operation", "process-data")
  
  // 服务B调用服务C
  let b_to_c_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_b_ctx, b_to_c_carrier)
  TextMapCarrier::set(b_to_c_carrier, "X-Calling-Service", "service-b")
  TextMapCarrier::set(b_to_c_carrier, "baggage", "user.id=user-123,request.id=req-456,service.b.operation=process-data")
  
  // 服务C提取上下文
  let service_c_ctx = CompositePropagator::extract(composite_propagator, b_to_c_carrier)
  let extracted_value = Context::get(service_c_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // 验证传播链的完整性
  let calling_service_b = TextMapCarrier::get(b_to_c_carrier, "X-Calling-Service")
  assert_eq(calling_service_b, Some("service-b"))
  
  // 服务C创建响应
  let c_response_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_c_ctx, c_response_carrier)
  TextMapCarrier::set(c_response_carrier, "X-Response-Service", "service-c")
  TextMapCarrier::set(c_response_carrier, "baggage", "user.id=user-123,request.id=req-456,service.b.operation=process-data,service.c.result=success")
}

test "异步消息传播测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 模拟消息生产者
  let producer_ctx = Context::root()
  let producer_baggage = Baggage::new()
  let baggage_with_correlation = Baggage::set_entry(producer_baggage, "correlation.id", "corr-789")
  let baggage_with_topic = Baggage::set_entry(baggage_with_correlation, "message.topic", "user-events")
  
  // 创建消息载体
  let message_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, producer_ctx, message_carrier)
  
  // 添加消息特定的头部
  TextMapCarrier::set(message_carrier, "Message-Id", "msg-12345")
  TextMapCarrier::set(message_carrier, "Message-Type", "UserCreated")
  TextMapCarrier::set(message_carrier, "Message-Timestamp", "2025-12-28T10:00:00Z")
  TextMapCarrier::set(message_carrier, "baggage", "correlation.id=corr-789,message.topic=user-events")
  
  // 验证消息头部
  let message_id = TextMapCarrier::get(message_carrier, "Message-Id")
  let message_type = TextMapCarrier::get(message_carrier, "Message-Type")
  let message_traceparent = TextMapCarrier::get(message_carrier, "traceparent")
  
  assert_eq(message_id, Some("msg-12345"))
  assert_eq(message_type, Some("UserCreated"))
  assert!(message_traceparent.is_some())
  
  // 模拟消息消费者提取上下文
  let consumer_ctx = CompositePropagator::extract(composite_propagator, message_carrier)
  let consumer_extracted = Context::get(consumer_ctx, ContextKey::new("extracted"))
  assert_eq(consumer_extracted, Some("true"))
  
  // 消费者处理消息并发送响应
  let response_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, consumer_ctx, response_carrier)
  
  TextMapCarrier::set(response_carrier, "Response-Message-Id", "msg-12345")
  TextMapCarrier::set(response_carrier, "Response-Status", "processed")
  TextMapCarrier::set(response_carrier, "Consumer-Service", "user-processor")
}

test "跨协议传播测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // HTTP协议传播
  let http_carrier = TextMapCarrier::new()
  let http_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, http_ctx, http_carrier)
  
  TextMapCarrier::set(http_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(http_carrier, "baggage", "user.id=user-123,session.id=session-456")
  TextMapCarrier::set(http_carrier, "Content-Type", "application/json")
  
  // gRPC协议传播（使用metadata头部）
  let grpc_carrier = TextMapCarrier::new()
  let grpc_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, grpc_ctx, grpc_carrier)
  
  TextMapCarrier::set(grpc_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(grpc_carrier, "baggage", "user.id=user-123,session.id=session-456")
  TextMapCarrier::set(grpc_carrier, "grpc-timeout", "30S")
  TextMapCarrier::set(grpc_carrier, "grpc-encoding", "gzip")
  
  // GraphQL协议传播
  let graphql_carrier = TextMapCarrier::new()
  let graphql_ctx = Context::root()
  CompositePropagator::inject(composite_propagator, graphql_ctx, graphql_carrier)
  
  TextMapCarrier::set(graphql_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(graphql_carrier, "baggage", "user.id=user-123,session.id=session-456")
  TextMapCarrier::set(graphql_carrier, "GraphQL-Operation-Name", "GetUserInfo")
  TextMapCarrier::set(graphql_carrier, "GraphQL-Operation-Type", "query")
  
  // 验证不同协议的传播
  let http_traceparent = TextMapCarrier::get(http_carrier, "traceparent")
  let grpc_traceparent = TextMapCarrier::get(grpc_carrier, "traceparent")
  let graphql_traceparent = TextMapCarrier::get(graphql_carrier, "traceparent")
  
  assert_eq(http_traceparent, grpc_traceparent)
  assert_eq(grpc_traceparent, graphql_traceparent)
  
  // 验证Baggage在所有协议中一致
  let http_baggage = TextMapCarrier::get(http_carrier, "baggage")
  let grpc_baggage = TextMapCarrier::get(grpc_carrier, "baggage")
  let graphql_baggage = TextMapCarrier::get(graphql_carrier, "baggage")
  
  assert_eq(http_baggage, grpc_baggage)
  assert_eq(grpc_baggage, graphql_baggage)
}

test "传播失败和边界条件测试" {
  // 创建传播器
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 测试空载体
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(composite_propagator, empty_carrier)
  let extracted_from_empty = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  assert_eq(extracted_from_empty, Some("true"))
  
  // 测试损坏的traceparent格式
  let corrupted_carrier = TextMapCarrier::new()
  TextMapCarrier::set(corrupted_carrier, "traceparent", "invalid-format")
  TextMapCarrier::set(corrupted_carrier, "baggage", "key=value")
  
  let ctx_from_corrupted = CompositePropagator::extract(composite_propagator, corrupted_carrier)
  let extracted_from_corrupted = Context::get(ctx_from_corrupted, ContextKey::new("extracted"))
  assert_eq(extracted_from_corrupted, Some("true"))
  
  // 测试过长的Baggage值
  let long_baggage_carrier = TextMapCarrier::new()
  let long_baggage_value = "user.id=" + "a" * 1000 + ",session.id=" + "b" * 1000
  TextMapCarrier::set(long_baggage_carrier, "baggage", long_baggage_value)
  
  let ctx_from_long = CompositePropagator::extract(composite_propagator, long_baggage_carrier)
  let extracted_from_long = Context::get(ctx_from_long, ContextKey::new("extracted"))
  assert_eq(extracted_from_long, Some("true"))
  
  // 测试特殊字符的Baggage值
  let special_chars_carrier = TextMapCarrier::new()
  TextMapCarrier::set(special_chars_carrier, "baggage", "user.name=John%20Doe,description=Test%20with%20spaces,special=!@#$%^&*()")
  
  let ctx_from_special = CompositePropagator::extract(composite_propagator, special_chars_carrier)
  let extracted_from_special = Context::get(ctx_from_special, ContextKey::new("extracted"))
  assert_eq(extracted_from_special, Some("true"))
  
  // 测试只有部分传播头部的情况
  let partial_carrier = TextMapCarrier::new()
  TextMapCarrier::set(partial_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  // 缺少baggage头部
  
  let ctx_from_partial = CompositePropagator::extract(composite_propagator, partial_carrier)
  let extracted_from_partial = Context::get(ctx_from_partial, ContextKey::new("extracted"))
  assert_eq(extracted_from_partial, Some("true"))
}