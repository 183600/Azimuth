// 跨服务追踪测试用例 - 测试微服务架构中的分布式追踪

use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, create_key}
use azimuth.telemetry.api.propagation.{TextMapCarrier, TextMapPropagator, W3CTraceContextPropagator, W3CBaggagePropagator, CompositePropagator, MapCarrier}

// 服务调用链结构
pub struct ServiceCallChain {
  services : Array[String]
  operations : Array[String]
  trace_contexts : Array[SpanContext]
  timestamps : Array[Int64]
}

// 跨服务追踪上下文
pub struct CrossServiceTraceContext {
  root_trace_id : String
  service_chain : Array[String]
  operation_chain : Array[String]
  baggage_items : Array[(String, String)]
  correlation_id : String
}

// 模拟HTTP请求头
pub struct HttpHeaders {
  headers : Array[(String, String)]
}

impl HttpHeaders {
  pub fn new() -> HttpHeaders {
    HttpHeaders::{ headers: [] }
  }
  
  pub fn add_header(self : HttpHeaders, key : String, value : String) -> HttpHeaders {
    HttpHeaders::{ headers: self.headers.push((key, value)) }
  }
  
  pub fn get_header(self : HttpHeaders, key : String) -> String? {
    let mut i = 0
    while i < self.headers.length() {
      let (k, v) = self.headers[i]
      if k.to_lowercase() == key.to_lowercase() {
        return Some(v)
      }
      i = i + 1
    }
    None
  }
}

// HTTP请求头载体实现
impl HttpHeaders for TextMapCarrier {
  pub fn get(self : HttpHeaders, key : String) -> String? {
    self.get_header(key)
  }
  
  pub fn set(self : HttpHeaders, key : String, value : String) -> Unit {
    // 简化实现
    ()
  }
  
  pub fn keys(self : HttpHeaders) -> Array[String] {
    let keys = []
    let mut i = 0
    while i < self.headers.length() {
      let (k, _) = self.headers[i]
      keys.push(k)
      i = i + 1
    }
    keys
  }
}

test "cross_service_trace_propagation" {
  // 测试跨服务追踪传播
  
  // 1. 创建服务调用链
  let services = ["api-gateway", "auth-service", "user-service", "order-service", "payment-service"]
  let operations = ["authenticate", "get_user", "create_order", "process_payment"]
  
  // 2. 初始化追踪器
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("cross-service-test", Some("1.0.0"))
  
  // 3. 创建传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 4. 模拟服务调用链
  let ctx = Context::empty()
  let correlation_id = "req-12345-abcde"
  
  // 在API网关创建根span
  let (api_gateway_ctx, api_gateway_span) = tracer.start_span(
    ctx.with_value(create_key("correlation.id"), correlation_id),
    "api-gateway-request",
    Server,
    Some([
      ("service.name", AttributeValue::string("api-gateway")),
      ("operation.name", AttributeValue::string("handle_request")),
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("/api/orders")),
      ("correlation.id", AttributeValue::string(correlation_id))
    ])
  )
  
  // 5. 模拟跨服务调用
  let mut service_chain = []
  let mut trace_contexts = []
  
  // API网关调用认证服务
  let auth_headers = HttpHeaders::new()
  composite_propagator.inject(api_gateway_ctx, auth_headers)
  
  // 添加业务头
  let auth_headers_with_business = auth_headers
    .add_header("X-Request-ID", correlation_id)
    .add_header("X-Source-Service", "api-gateway")
    .add_header("X-Target-Service", "auth-service")
  
  // 认证服务提取追踪上下文
  let auth_extracted_ctx = composite_propagator.extract(Context::empty(), auth_headers_with_business)
  let (auth_ctx, auth_span) = tracer.start_span(
    auth_extracted_ctx,
    "authenticate-user",
    Server,
    Some([
      ("service.name", AttributeValue::string("auth-service")),
      ("operation.name", AttributeValue::string("authenticate")),
      ("parent.service", AttributeValue::string("api-gateway")),
      ("correlation.id", AttributeValue::string(correlation_id))
    ])
  )
  
  // 认证服务调用用户服务
  let user_headers = HttpHeaders::new()
  composite_propagator.inject(auth_ctx, user_headers)
  let user_headers_with_business = user_headers
    .add_header("X-Request-ID", correlation_id)
    .add_header("X-Source-Service", "auth-service")
    .add_header("X-Target-Service", "user-service")
  
  // 用户服务提取追踪上下文
  let user_extracted_ctx = composite_propagator.extract(Context::empty(), user_headers_with_business)
  let (user_ctx, user_span) = tracer.start_span(
    user_extracted_ctx,
    "get-user-profile",
    Server,
    Some([
      ("service.name", AttributeValue::string("user-service")),
      ("operation.name", AttributeValue::string("get_user")),
      ("parent.service", AttributeValue::string("auth-service")),
      ("correlation.id", AttributeValue::string(correlation_id))
    ])
  )
  
  // 6. 验证追踪上下文传播
  // 验证所有span都有相同的trace_id
  let api_trace_id = api_gateway_span.context.trace_id
  let auth_trace_id = auth_span.context.trace_id
  let user_trace_id = user_span.context.trace_id
  
  assert_eq(api_trace_id, auth_trace_id)
  assert_eq(auth_trace_id, user_trace_id)
  
  // 验证span_id不同
  assert_eq(api_gateway_span.context.span_id != auth_span.context.span_id, true)
  assert_eq(auth_span.context.span_id != user_span.context.span_id, true)
  
  // 7. 验证服务调用链属性
  let mut found_api_gateway_service = false
  let mut found_auth_service = false
  let mut found_user_service = false
  let mut found_correlation_id = false
  
  // 检查API网关span
  let mut i = 0
  while i < api_gateway_span.attributes.length() {
    let (name, value) = api_gateway_span.attributes[i]
    match (name, value) {
      ("service.name", StringValue(service_name)) => {
        if service_name == "api-gateway" {
          found_api_gateway_service = true
        }
      }
      ("correlation.id", StringValue(correlation)) => {
        if correlation == correlation_id {
          found_correlation_id = true
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 检查认证服务span
  i = 0
  while i < auth_span.attributes.length() {
    let (name, value) = auth_span.attributes[i]
    match (name, value) {
      ("service.name", StringValue(service_name)) => {
        if service_name == "auth-service" {
          found_auth_service = true
        }
      }
      ("parent.service", StringValue(parent_service)) => {
        assert_eq(parent_service, "api-gateway")
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 检查用户服务span
  i = 0
  while i < user_span.attributes.length() {
    let (name, value) = user_span.attributes[i]
    match (name, value) {
      ("service.name", StringValue(service_name)) => {
        if service_name == "user-service" {
          found_user_service = true
        }
      }
      ("parent.service", StringValue(parent_service)) => {
        assert_eq(parent_service, "auth-service")
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_api_gateway_service, true)
  assert_eq(found_auth_service, true)
  assert_eq(found_user_service, true)
  assert_eq(found_correlation_id, true)
}

test "cross_service_baggage_propagation" {
  // 测试跨服务baggage传播
  
  // 1. 创建初始baggage
  let initial_baggage = [
    ("user.id", "12345"),
    ("user.role", "premium"),
    ("request.source", "mobile_app"),
    ("session.id", "sess-abcdef"),
    ("tenant.id", "tenant-001")
  ]
  
  // 2. 创建传播器
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 3. 在服务A中设置baggage
  let service_a_headers = HttpHeaders::new()
  let ctx = Context::empty()
  
  // 模拟设置baggage到上下文
  let baggage_ctx = ctx
    .with_value(create_key("user.id"), "12345")
    .with_value(create_key("user.role"), "premium")
    .with_value(create_key("request.source"), "mobile_app")
    .with_value(create_key("session.id"), "sess-abcdef")
    .with_value(create_key("tenant.id"), "tenant-001")
  
  baggage_propagator.inject(baggage_ctx, service_a_headers)
  
  // 4. 服务B提取baggage并添加新的baggage项
  let service_b_extracted_ctx = baggage_propagator.extract(Context::empty(), service_a_headers)
  
  // 服务B添加新的baggage项
  let service_b_ctx = service_b_extracted_ctx
    .with_value(create_key("service.b.operation"), "process_data")
    .with_value(create_key("service.b.duration"), "150ms")
  
  // 5. 服务B调用服务C，传播所有baggage
  let service_b_headers = HttpHeaders::new()
  baggage_propagator.inject(service_b_ctx, service_b_headers)
  
  // 6. 服务C提取所有baggage
  let service_c_extracted_ctx = baggage_propagator.extract(Context::empty(), service_b_headers)
  
  // 7. 验证baggage传播
  // 验证原始baggage项被保留
  match service_c_extracted_ctx.get(create_key("user.id")) {
    Some(user_id) => assert_eq(user_id, "12345")
    None => @test.fail("Test failed")
  }
  
  match service_c_extracted_ctx.get(create_key("user.role")) {
    Some(user_role) => assert_eq(user_role, "premium")
    None => @test.fail("Test failed")
  }
  
  match service_c_extracted_ctx.get(create_key("request.source")) {
    Some(request_source) => assert_eq(request_source, "mobile_app")
    None => @test.fail("Test failed")
  }
  
  match service_c_extracted_ctx.get(create_key("session.id")) {
    Some(session_id) => assert_eq(session_id, "sess-abcdef")
    None => @test.fail("Test failed")
  }
  
  match service_c_extracted_ctx.get(create_key("tenant.id")) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-001")
    None => @test.fail("Test failed")
  }
  
  // 验证服务B添加的baggage项
  match service_c_extracted_ctx.get(create_key("service.b.operation")) {
    Some(operation) => assert_eq(operation, "process_data")
    None => @test.fail("Test failed")
  }
  
  match service_c_extracted_ctx.get(create_key("service.b.duration")) {
    Some(duration) => assert_eq(duration, "150ms")
    None => @test.fail("Test failed")
  }
  
  // 8. 测试baggage头格式
  match service_b_headers.get_header("baggage") {
    Some(baggage_header) => {
      // 验证包含所有baggage项
      assert_eq(baggage_header.contains("user.id=12345"), true)
      assert_eq(baggage_header.contains("user.role=premium"), true)
      assert_eq(baggage_header.contains("request.source=mobile_app"), true)
      assert_eq(baggage_header.contains("session.id=sess-abcdef"), true)
      assert_eq(baggage_header.contains("tenant.id=tenant-001"), true)
      assert_eq(baggage_header.contains("service.b.operation=process_data"), true)
      assert_eq(baggage_header.contains("service.b.duration=150ms"), true)
    }
    None => @test.fail("Test failed")
  }
}

test "cross_service_async_operations" {
  // 测试跨服务异步操作追踪
  
  // 1. 创建异步服务调用场景
  let services = ["order-service", "inventory-service", "notification-service", "analytics-service"]
  
  // 2. 初始化追踪器
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("async-service-test", Some("1.0.0"))
  
  // 3. 创建传播器
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::{}, W3CBaggagePropagator::{}])
  
  // 4. 模拟订单服务创建订单时的异步调用
  let ctx = Context::empty()
  let order_id = "order-12345"
  
  // 创建订单处理的主span
  let (order_ctx, order_span) = tracer.start_span(
    ctx.with_value(create_key("order.id"), order_id),
    "process-order",
    Server,
    Some([
      ("service.name", AttributeValue::string("order-service")),
      ("operation.name", AttributeValue::string("create_order")),
      ("order.id", AttributeValue::string(order_id)),
      ("order.type", AttributeValue::string("online"))
    ])
  )
  
  // 5. 模拟异步调用库存服务
  let inventory_headers = HttpHeaders::new()
  composite_propagator.inject(order_ctx, inventory_headers)
  
  let inventory_extracted_ctx = composite_propagator.extract(Context::empty(), inventory_headers)
  let (inventory_ctx, inventory_span) = tracer.start_span(
    inventory_extracted_ctx,
    "check-inventory",
    Client,
    Some([
      ("service.name", AttributeValue::string("inventory-service")),
      ("operation.name", AttributeValue::string("check_stock")),
      ("order.id", AttributeValue::string(order_id)),
      ("async.call", AttributeValue::bool(true))
    ])
  )
  
  // 6. 模拟异步调用通知服务
  let notification_headers = HttpHeaders::new()
  composite_propagator.inject(order_ctx, notification_headers)
  
  let notification_extracted_ctx = composite_propagator.extract(Context::empty(), notification_headers)
  let (notification_ctx, notification_span) = tracer.start_span(
    notification_extracted_ctx,
    "send-notification",
    Client,
    Some([
      ("service.name", AttributeValue::string("notification-service")),
      ("operation.name", AttributeValue::string("send_email")),
      ("order.id", AttributeValue::string(order_id)),
      ("async.call", AttributeValue::bool(true))
    ])
  )
  
  // 7. 模拟异步调用分析服务
  let analytics_headers = HttpHeaders::new()
  composite_propagator.inject(order_ctx, analytics_headers)
  
  let analytics_extracted_ctx = composite_propagator.extract(Context::empty(), analytics_headers)
  let (analytics_ctx, analytics_span) = tracer.start_span(
    analytics_extracted_ctx,
    "record-analytics",
    Client,
    Some([
      ("service.name", AttributeValue::string("analytics-service")),
      ("operation.name", AttributeValue::string("track_event")),
      ("order.id", AttributeValue::string(order_id)),
      ("async.call", AttributeValue::bool(true))
    ])
  )
  
  // 8. 验证异步操作的追踪上下文
  // 验证所有异步调用都有相同的trace_id
  assert_eq(order_span.context.trace_id, inventory_span.context.trace_id)
  assert_eq(order_span.context.trace_id, notification_span.context.trace_id)
  assert_eq(order_span.context.trace_id, analytics_span.context.trace_id)
  
  // 验证所有span都有不同的span_id
  assert_eq(order_span.context.span_id != inventory_span.context.span_id, true)
  assert_eq(order_span.context.span_id != notification_span.context.span_id, true)
  assert_eq(order_span.context.span_id != analytics_span.context.span_id, true)
  assert_eq(inventory_span.context.span_id != notification_span.context.span_id, true)
  assert_eq(inventory_span.context.span_id != analytics_span.context.span_id, true)
  assert_eq(notification_span.context.span_id != analytics_span.context.span_id, true)
  
  // 9. 验证异步调用属性
  let mut found_async_calls = false
  let mut found_order_ids = 0
  
  let async_spans = [inventory_span, notification_span, analytics_span]
  let mut i = 0
  while i < async_spans.length() {
    let span = async_spans[i]
    
    let mut found_async_call = false
    let mut found_order_id = false
    
    let mut j = 0
    while j < span.attributes.length() {
      let (name, value) = span.attributes[j]
      match (name, value) {
        ("async.call", BoolValue(async_call)) => {
          if async_call {
            found_async_call = true
          }
        }
        ("order.id", StringValue(order)) => {
          if order == order_id {
            found_order_id = true
            found_order_ids = found_order_ids + 1
          }
        }
        _ => ()
      }
      j = j + 1
    }
    
    assert_eq(found_async_call, true)
    assert_eq(found_order_id, true)
    
    if found_async_call {
      found_async_calls = true
    }
    
    i = i + 1
  }
  
  assert_eq(found_async_calls, true)
  assert_eq(found_order_ids, 3)  // 三个异步调用都应该有order.id
}

test "cross_service_error_propagation" {
  // 测试跨服务错误传播
  
  // 1. 创建服务调用链
  let services = ["frontend", "api-gateway", "auth-service"]
  
  // 2. 初始化追踪器
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-propagation-test", Some("1.0.0"))
  
  // 3. 创建传播器
  let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::{}, W3CBaggagePropagator::{}])
  
  // 4. 模拟错误场景：认证服务返回错误
  let ctx = Context::empty()
  let request_id = "req-67890"
  
  // 前端发起请求
  let (frontend_ctx, frontend_span) = tracer.start_span(
    ctx.with_value(create_key("request.id"), request_id),
    "user-login",
    Client,
    Some([
      ("service.name", AttributeValue::string("frontend")),
      ("operation.name", AttributeValue::string("login")),
      ("request.id", AttributeValue::string(request_id))
    ])
  )
  
  // API网关处理请求
  let gateway_headers = HttpHeaders::new()
  composite_propagator.inject(frontend_ctx, gateway_headers)
  
  let gateway_extracted_ctx = composite_propagator.extract(Context::empty(), gateway_headers)
  let (gateway_ctx, gateway_span) = tracer.start_span(
    gateway_extracted_ctx,
    "authenticate-request",
    Server,
    Some([
      ("service.name", AttributeValue::string("api-gateway")),
      ("operation.name", AttributeValue::string("authenticate")),
      ("request.id", AttributeValue::string(request_id))
    ])
  )
  
  // 认证服务处理并返回错误
  let auth_headers = HttpHeaders::new()
  composite_propagator.inject(gateway_ctx, auth_headers)
  
  let auth_extracted_ctx = composite_propagator.extract(Context::empty(), auth_headers)
  let (auth_ctx, auth_span) = tracer.start_span(
    auth_extracted_ctx,
    "validate-credentials",
    Server,
    Some([
      ("service.name", AttributeValue::string("auth-service")),
      ("operation.name", AttributeValue::string("validate")),
      ("request.id", AttributeValue::string(request_id))
    ])
  )
  
  // 5. 在认证服务中创建错误span
  let error_auth_span = Span::{
    ..auth_span,
    status: Error,
    status_description: Some("Invalid credentials provided"),
    events: [
      SpanEvent{
        name: "authentication_failed",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [
          ("error.type", AttributeValue::string("authentication_error")),
          ("error.message", AttributeValue::string("Invalid username or password")),
          ("user.id", AttributeValue::string("user-123")),
          ("attempt.count", AttributeValue::int(3L))
        ]
      }
    ],
    attributes: auth_span.attributes.push([
      ("error.code", AttributeValue::string("AUTH_001")),
      ("error.type", AttributeValue::string("authentication_failed")),
      ("http.status_code", AttributeValue::int(401L))
    ])
  }
  
  // 6. 验证错误传播
  // 验证错误span的状态
  assert_eq(error_auth_span.status, Error)
  assert_eq(error_auth_span.status_description, Some("Invalid credentials provided"))
  
  // 验证错误事件
  assert_eq(error_auth_span.events.length(), 1)
  let error_event = error_auth_span.events[0]
  assert_eq(error_event.name, "authentication_failed")
  
  // 验证错误事件属性
  let mut found_error_type = false
  let mut found_error_message = false
  let mut found_user_id = false
  let mut found_attempt_count = false
  
  let mut i = 0
  while i < error_event.attributes.length() {
    let (name, value) = error_event.attributes[i]
    match (name, value) {
      ("error.type", StringValue(error_type)) => {
        if error_type == "authentication_error" {
          found_error_type = true
        }
      }
      ("error.message", StringValue(error_message)) => {
        if error_message == "Invalid username or password" {
          found_error_message = true
        }
      }
      ("user.id", StringValue(user)) => {
        if user == "user-123" {
          found_user_id = true
        }
      }
      ("attempt.count", IntValue(count)) => {
        if count == 3L {
          found_attempt_count = true
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_error_type, true)
  assert_eq(found_error_message, true)
  assert_eq(found_user_id, true)
  assert_eq(found_attempt_count, true)
  
  // 验证错误span属性
  let mut found_error_code = false
  let mut found_http_status = false
  
  i = 0
  while i < error_auth_span.attributes.length() {
    let (name, value) = error_auth_span.attributes[i]
    match (name, value) {
      ("error.code", StringValue(error_code)) => {
        if error_code == "AUTH_001" {
          found_error_code = true
        }
      }
      ("http.status_code", IntValue(status_code)) => {
        if status_code == 401L {
          found_http_status = true
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_error_code, true)
  assert_eq(found_http_status, true)
  
  // 7. 验证错误上下文传播
  // 所有span应该有相同的trace_id
  assert_eq(frontend_span.context.trace_id, gateway_span.context.trace_id)
  assert_eq(gateway_span.context.trace_id, error_auth_span.context.trace_id)
  
  // 验证请求ID在所有span中传播
  let spans = [frontend_span, gateway_span, error_auth_span]
  let mut found_request_ids = 0
  
  let mut i = 0
  while i < spans.length() {
    let span = spans[i]
    let mut found_request_id = false
    
    let mut j = 0
    while j < span.attributes.length() {
      let (name, value) = span.attributes[j]
      match (name, value) {
        ("request.id", StringValue(req_id)) => {
          if req_id == request_id {
            found_request_id = true
            found_request_ids = found_request_ids + 1
          }
        }
        _ => ()
      }
      j = j + 1
    }
    
    assert_eq(found_request_id, true)
    i = i + 1
  }
  
  assert_eq(found_request_ids, 3)  // 所有三个span都应该有request.id
}