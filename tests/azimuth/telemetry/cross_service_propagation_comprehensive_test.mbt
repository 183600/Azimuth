// Azimuth Telemetry System - Cross-Service Trace Propagation Test Suite
// 跨服务追踪传播测试用例

test "W3C TraceContext 传播完整流程测试" {
  // 初始化传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建原始SpanContext
  let original_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let original_span_id = "00f067aa0ba902b7"
  let original_ctx = SpanContext::new(original_trace_id, original_span_id, true, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 创建包含SpanContext的Context
  let root_context = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let context_with_trace = Context::with_value(root_context, trace_key, original_trace_id)
  
  // 创建TextMapCarrier用于HTTP头传播
  let carrier = TextMapCarrier::new()
  
  // 测试注入操作
  CompositePropagator::inject(composite_propagator, context_with_trace, carrier)
  
  // 验证注入的traceparent头
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert!(injected_traceparent.is_some(), "traceparent头应该被注入")
  
  // 模拟跨服务传播 - 提取操作
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_trace_value = Context::get(extracted_context, ContextKey::new("extracted"))
  
  assert_eq(extracted_trace_value, Some("true"), "应该成功提取追踪上下文")
  
  // 验证提取的数据完整性
  let traceparent_again = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(traceparent_again, injected_traceparent, "多次获取应该返回相同值")
}

test "多级服务调用追踪传播测试" {
  // 模拟服务A -> 服务B -> 服务C的调用链
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 服务A：创建根Span
  let service_a_trace_id = "servicea-trace-12345"
  let service_a_span_id = "servicea-span-67890"
  let service_a_ctx = SpanContext::new(service_a_trace_id, service_a_span_id, true, "")
  
  let service_a_context = Context::root()
  let service_a_context_with_trace = Context::with_value(
    service_a_context, 
    ContextKey::new("service.name"), 
    "service-a"
  )
  
  // 服务A调用服务B：注入追踪信息
  let carrier_a_to_b = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_a_context_with_trace, carrier_a_to_b)
  
  // 服务B：提取追踪信息并创建子Span
  let service_b_context = CompositePropagator::extract(propagator, carrier_a_to_b)
  let service_b_context_with_name = Context::with_value(
    service_b_context,
    ContextKey::new("service.name"),
    "service-b"
  )
  
  // 服务B调用服务C：注入追踪信息
  let carrier_b_to_c = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_b_context_with_name, carrier_b_to_c)
  
  // 服务C：提取追踪信息
  let service_c_context = CompositePropagator::extract(propagator, carrier_b_to_c)
  let service_c_name = Context::get(service_c_context, ContextKey::new("service.name"))
  
  // 验证追踪传播链的完整性
  let extracted_value = Context::get(service_c_context, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"), "追踪信息应该传播到服务C")
  
  // 验证各级服务都能正确传播
  let traceparent_a_b = TextMapCarrier::get(carrier_a_to_b, "traceparent")
  let traceparent_b_c = TextMapCarrier::get(carrier_b_to_c, "traceparent")
  
  assert!(traceparent_a_b.is_some(), "服务A到B的传播应该包含traceparent")
  assert!(traceparent_b_c.is_some(), "服务B到C的传播应该包含traceparent")
}

test "Baggage跨服务传播测试" {
  // 初始化传播器
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 创建包含Baggage的Context
  let root_context = Context::root()
  let baggage = Baggage::new()
  
  // 添加多个Baggage条目
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_locale = Baggage::set_entry(baggage_with_session, "user.locale", "zh-CN")
  let baggage_with_tier = Baggage::set_entry(baggage_with_locale, "service.tier", "premium")
  
  // 创建包含Baggage信息的Context
  let context_with_baggage = Context::with_value(
    root_context,
    ContextKey::new("baggage.data"),
    "user.id=user-12345,session.id=session-abcdef,user.locale=zh-CN,service.tier=premium"
  )
  
  // 注入Baggage信息到Carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_with_baggage, carrier)
  
  // 提取Baggage信息
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_baggage = Context::get(extracted_context, ContextKey::new("baggage.data"))
  
  // 验证Baggage传播
  assert!(extracted_baggage.is_some(), "Baggage信息应该被正确传播")
  
  // 验证各个Baggage条目
  let user_id = Baggage::get_entry(baggage_with_tier, "user.id")
  let session_id = Baggage::get_entry(baggage_with_tier, "session.id")
  let locale = Baggage::get_entry(baggage_with_tier, "user.locale")
  let tier = Baggage::get_entry(baggage_with_tier, "service.tier")
  
  assert_eq(user_id, Some("user-12345"))
  assert_eq(session_id, Some("session-abcdef"))
  assert_eq(locale, Some("zh-CN"))
  assert_eq(tier, Some("premium"))
}

test "异步消息追踪传播测试" {
  // 模拟消息队列中的追踪传播
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 生产者：创建消息并注入追踪信息
  let producer_trace_id = "msg-trace-98765"
  let producer_span_id = "msg-span-43210"
  let producer_ctx = SpanContext::new(producer_trace_id, producer_span_id, true, "")
  
  let producer_context = Context::root()
  let producer_context_with_msg = Context::with_value(
    producer_context,
    ContextKey::new("message.id"),
    "msg-123456789"
  )
  
  // 创建消息头Carrier
  let message_headers = TextMapCarrier::new()
  CompositePropagator::inject(propagator, producer_context_with_msg, message_headers)
  
  // 模拟消息传输过程中的头信息
  TextMapCarrier::set(message_headers, "message-type", "event")
  TextMapCarrier::set(message_headers, "message-version", "1.0")
  TextMapCarrier::set(message_headers, "message-timestamp", "1735689600")
  
  // 消费者：从消息头中提取追踪信息
  let consumer_context = CompositePropagator::extract(propagator, message_headers)
  let consumer_context_with_msg = Context::with_value(
    consumer_context,
    ContextKey::new("consumer.id"),
    "consumer-987654321"
  )
  
  // 验证消息追踪传播
  let extracted_value = Context::get(consumer_context, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"), "消息追踪信息应该被正确提取")
  
  // 验证消息头信息完整性
  let msg_type = TextMapCarrier::get(message_headers, "message-type")
  let msg_version = TextMapCarrier::get(message_headers, "message-version")
  let msg_timestamp = TextMapCarrier::get(message_headers, "message-timestamp")
  
  assert_eq(msg_type, Some("event"))
  assert_eq(msg_version, Some("1.0"))
  assert_eq(msg_timestamp, Some("1735689600"))
}

test "微服务架构复杂追踪传播测试" {
  // 模拟复杂的微服务调用链：API Gateway -> Auth Service -> User Service -> Order Service
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // API Gateway：创建根追踪
  let gateway_trace_id = "gateway-trace-abc123"
  let gateway_context = Context::root()
  let gateway_context_with_info = Context::with_value(
    gateway_context,
    ContextKey::new("gateway.request.id"),
    "req-xyz789"
  )
  
  // API Gateway -> Auth Service
  let carrier_to_auth = TextMapCarrier::new()
  CompositePropagator::inject(propagator, gateway_context_with_info, carrier_to_auth)
  TextMapCarrier::set(carrier_to_auth, "service.target", "auth-service")
  
  // Auth Service：处理认证
  let auth_context = CompositePropagator::extract(propagator, carrier_to_auth)
  let auth_context_with_token = Context::with_value(
    auth_context,
    ContextKey::new("auth.token"),
    "jwt-token-12345"
  )
  
  // Auth Service -> User Service
  let carrier_to_user = TextMapCarrier::new()
  CompositePropagator::inject(propagator, auth_context_with_token, carrier_to_user)
  TextMapCarrier::set(carrier_to_user, "service.target", "user-service")
  
  // User Service：获取用户信息
  let user_context = CompositePropagator::extract(propagator, carrier_to_user)
  let user_context_with_info = Context::with_value(
    user_context,
    ContextKey::new("user.id"),
    "user-67890"
  )
  
  // User Service -> Order Service
  let carrier_to_order = TextMapCarrier::new()
  CompositePropagator::inject(propagator, user_context_with_info, carrier_to_order)
  TextMapCarrier::set(carrier_to_order, "service.target", "order-service")
  
  // Order Service：处理订单
  let order_context = CompositePropagator::extract(propagator, carrier_to_order)
  
  // 验证整个调用链的追踪传播
  let order_extracted = Context::get(order_context, ContextKey::new("extracted"))
  assert_eq(order_extracted, Some("true"), "追踪信息应该传播到Order Service")
  
  // 验证各服务间的传播头
  let auth_traceparent = TextMapCarrier::get(carrier_to_auth, "traceparent")
  let user_traceparent = TextMapCarrier::get(carrier_to_user, "traceparent")
  let order_traceparent = TextMapCarrier::get(carrier_to_order, "traceparent")
  
  assert!(auth_traceparent.is_some(), "Auth Service应该收到traceparent")
  assert!(user_traceparent.is_some(), "User Service应该收到traceparent")
  assert!(order_traceparent.is_some(), "Order Service应该收到traceparent")
  
  // 验证服务目标标识
  let auth_target = TextMapCarrier::get(carrier_to_auth, "service.target")
  let user_target = TextMapCarrier::get(carrier_to_user, "service.target")
  let order_target = TextMapCarrier::get(carrier_to_order, "service.target")
  
  assert_eq(auth_target, Some("auth-service"))
  assert_eq(user_target, Some("user-service"))
  assert_eq(order_target, Some("order-service"))
}

test "跨协议追踪传播测试" {
  // 测试HTTP到gRPC再到HTTP的追踪传播
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // HTTP客户端：初始请求
  let http_context = Context::root()
  let http_context_with_protocol = Context::with_value(
    http_context,
    ContextKey::new("protocol"),
    "HTTP/1.1"
  )
  
  let http_carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, http_context_with_protocol, http_carrier)
  TextMapCarrier::set(http_carrier, "content-type", "application/json")
  
  // HTTP到gRPC转换
  let grpc_context = CompositePropagator::extract(propagator, http_carrier)
  let grpc_context_with_metadata = Context::with_value(
    grpc_context,
    ContextKey::new("protocol"),
    "gRPC"
  )
  
  let grpc_carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, grpc_context_with_metadata, grpc_carrier)
  TextMapCarrier::set(grpc_carrier, "grpc-content-type", "application/grpc")
  
  // gRPC到HTTP转换
  let final_http_context = CompositePropagator::extract(propagator, grpc_carrier)
  let final_http_context_with_protocol = Context::with_value(
    final_http_context,
    ContextKey::new("protocol"),
    "HTTP/2.0"
  )
  
  // 验证跨协议传播
  let final_extracted = Context::get(final_http_context, ContextKey::new("extracted"))
  assert_eq(final_extracted, Some("true"), "追踪信息应该跨协议传播")
  
  // 验证各协议的载体信息
  let http_traceparent = TextMapCarrier::get(http_carrier, "traceparent")
  let grpc_traceparent = TextMapCarrier::get(grpc_carrier, "traceparent")
  
  assert!(http_traceparent.is_some(), "HTTP载体应该包含traceparent")
  assert!(grpc_traceparent.is_some(), "gRPC载体应该包含traceparent")
  
  // 验证协议特定头
  let http_content_type = TextMapCarrier::get(http_carrier, "content-type")
  let grpc_content_type = TextMapCarrier::get(grpc_carrier, "grpc-content-type")
  
  assert_eq(http_content_type, Some("application/json"))
  assert_eq(grpc_content_type, Some("application/grpc"))
}