// Azimuth Enhanced Distributed Tracing Propagation Test Suite
// 增强型分布式追踪链路传播测试套件

// Test 1: 跨服务追踪上下文传播
test "cross-service trace context propagation" {
  // 初始化追踪提供者
  let tracer_provider = @azimuth.TracerProvider::new()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "service-a")
  
  // 创建根span
  let root_span = @azimuth.Tracer::start_span(tracer, "service-a.operation")
  @azimuth.Span::set_attribute(root_span, "service.name", "service-a")
  @azimuth.Span::set_attribute(root_span, "operation.type", "user.request")
  
  // 获取span上下文
  let span_context = @azimuth.Span::span_context(root_span)
  let trace_id = @azimuth.SpanContext::trace_id(span_context)
  let span_id = @azimuth.SpanContext::span_id(span_context)
  
  // 创建传播器
  let propagator = @azimuth.TraceContextPropagator::new()
  
  // 创建载体用于传播
  let carrier = @azimuth.TextMapCarrier::new()
  
  // 注入追踪上下文
  @azimuth.TraceContextPropagator::inject(propagator, span_context, carrier)
  
  // 验证注入的上下文
  let trace_parent = @azimuth.TextMapCarrier::get(carrier, "traceparent")
  let trace_state = @azimuth.TextMapCarrier::get(carrier, "tracestate")
  
  assert_true(trace_parent.length() > 0)
  assert_true(@azimuth.String::contains(trace_parent, trace_id))
  
  // 模拟服务B接收上下文
  let service_b_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "service-b")
  let extracted_context = @azimuth.TraceContextPropagator::extract(propagator, carrier)
  
  // 验证提取的上下文
  assert_eq(@azimuth.SpanContext::trace_id(extracted_context), trace_id)
  assert_ne(@azimuth.SpanContext::span_id(extracted_context), span_id) // 应该是不同的span ID
  
  // 在服务B中创建子span
  let child_span = @azimuth.Tracer::start_span(service_b_tracer, "service-b.operation", @azimuth.SpanKind::Internal, Some(extracted_context))
  @azimuth.Span::set_attribute(child_span, "service.name", "service-b")
  @azimuth.Span::set_attribute(child_span, "operation.type", "data.processing")
  
  // 验证父子关系
  let child_context = @azimuth.Span::span_context(child_span)
  assert_eq(@azimuth.SpanContext::trace_id(child_context), trace_id)
  assert_eq(@azimuth.SpanContext::parent_span_id(child_context), span_id)
  
  // 结束spans
  @azimuth.Span::end(child_span)
  @azimuth.Span::end(root_span)
  
  assert_true(true)
}

// Test 2: 复杂分布式调用链追踪
test "complex distributed call chain tracing" {
  let tracer_provider = @azimuth.TracerProvider::new()
  let propagator = @azimuth.TraceContextPropagator::new()
  let carrier = @azimuth.TextMapCarrier::new()
  
  // 服务A：API网关
  let gateway_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "api-gateway")
  let gateway_span = @azimuth.Tracer::start_span(gateway_tracer, "api-gateway.request")
  @azimuth.Span::set_attribute(gateway_span, "http.method", "POST")
  @azimuth.Span::set_attribute(gateway_span, "http.url", "/api/v1/process")
  @azimuth.Span::set_attribute(gateway_span, "user.id", "user-12345")
  
  // 注入上下文到载体
  let gateway_context = @azimuth.Span::span_context(gateway_span)
  @azimuth.TraceContextPropagator::inject(propagator, gateway_context, carrier)
  
  // 服务B：认证服务
  let auth_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "auth-service")
  let auth_context = @azimuth.TraceContextPropagator::extract(propagator, carrier)
  let auth_span = @azimuth.Tracer::start_span(auth_tracer, "auth-service.validate", @azimuth.SpanKind::Server, Some(auth_context))
  @azimuth.Span::set_attribute(auth_span, "auth.method", "jwt")
  @azimuth.Span::set_attribute(auth_span, "auth.result", "success")
  
  // 注入认证上下文
  let auth_carrier = @azimuth.TextMapCarrier::new()
  let auth_span_context = @azimuth.Span::span_context(auth_span)
  @azimuth.TraceContextPropagator::inject(propagator, auth_span_context, auth_carrier)
  
  // 服务C：业务逻辑服务
  let business_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "business-service")
  let business_context = @azimuth.TraceContextPropagator::extract(propagator, auth_carrier)
  let business_span = @azimuth.Tracer::start_span(business_tracer, "business-service.process", @azimuth.SpanKind::Server, Some(business_context))
  @azimuth.Span::set_attribute(business_span, "operation.type", "data.transformation")
  @azimuth.Span::set_attribute(business_span, "data.size", 1024)
  
  // 服务D：数据库服务
  let db_tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "database-service")
  let db_carrier = @azimuth.TextMapCarrier::new()
  let business_span_context = @azimuth.Span::span_context(business_span)
  @azimuth.TraceContextPropagator::inject(propagator, business_span_context, db_carrier)
  
  let db_context = @azimuth.TraceContextPropagator::extract(propagator, db_carrier)
  let db_span = @azimuth.Tracer::start_span(db_tracer, "database-service.query", @azimuth.SpanKind::Client, Some(db_context))
  @azimuth.Span::set_attribute(db_span, "db.type", "postgresql")
  @azimuth.Span::set_attribute(db_span, "db.statement", "SELECT * FROM users WHERE id = $1")
  @azimuth.Span::set_attribute(db_span, "db.rows", 1)
  
  // 验证追踪链路完整性
  let db_span_context = @azimuth.Span::span_context(db_span)
  let business_span_context = @azimuth.Span::span_context(business_span)
  let auth_span_context = @azimuth.Span::span_context(auth_span)
  let gateway_span_context = @azimuth.Span::span_context(gateway_span)
  
  // 所有span应该有相同的trace ID
  assert_eq(@azimuth.SpanContext::trace_id(db_span_context), @azimuth.SpanContext::trace_id(business_span_context))
  assert_eq(@azimuth.SpanContext::trace_id(business_span_context), @azimuth.SpanContext::trace_id(auth_span_context))
  assert_eq(@azimuth.SpanContext::trace_id(auth_span_context), @azimuth.SpanContext::trace_id(gateway_span_context))
  
  // 验证父子关系
  assert_eq(@azimuth.SpanContext::parent_span_id(business_span_context), @azimuth.SpanContext::span_id(auth_span_context))
  assert_eq(@azimuth.SpanContext::parent_span_id(db_span_context), @azimuth.SpanContext::span_id(business_span_context))
  
  // 结束所有spans（按正确顺序）
  @azimuth.Span::end(db_span)
  @azimuth.Span::end(business_span)
  @azimuth.Span::end(auth_span)
  @azimuth.Span::end(gateway_span)
  
  assert_true(true)
}

// Test 3: 异步操作追踪上下文传播
test "async operation trace context propagation" {
  let tracer_provider = @azimuth.TracerProvider::new()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "async-service")
  let propagator = @azimuth.TraceContextPropagator::new()
  
  // 创建父span
  let parent_span = @azimuth.Tracer::start_span(tracer, "async.operation")
  let parent_context = @azimuth.Span::span_context(parent_span)
  
  // 模拟异步操作1
  let async1_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, parent_context, async1_carrier)
  
  let async1_context = @azimuth.TraceContextPropagator::extract(propagator, async1_carrier)
  let async1_span = @azimuth.Tracer::start_span(tracer, "async.task1", @azimuth.SpanKind::Internal, Some(async1_context))
  @azimuth.Span::set_attribute(async1_span, "task.id", "task-001")
  @azimuth.Span::set_attribute(async1_span, "task.type", "data.fetch")
  
  // 模拟异步操作2
  let async2_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, parent_context, async2_carrier)
  
  let async2_context = @azimuth.TraceContextPropagator::extract(propagator, async2_carrier)
  let async2_span = @azimuth.Tracer::start_span(tracer, "async.task2", @azimuth.SpanKind::Internal, Some(async2_context))
  @azimuth.Span::set_attribute(async2_span, "task.id", "task-002")
  @azimuth.Span::set_attribute(async2_span, "task.type", "data.process")
  
  // 模拟异步操作3（嵌套异步）
  let async3_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, parent_context, async3_carrier)
  
  let async3_context = @azimuth.TraceContextPropagator::extract(propagator, async3_carrier)
  let async3_span = @azimuth.Tracer::start_span(tracer, "async.task3", @azimuth.SpanKind::Internal, Some(async3_context))
  @azimuth.Span::set_attribute(async3_span, "task.id", "task-003")
  @azimuth.Span::set_attribute(async3_span, "task.type", "data.store")
  
  // 验证所有异步操作共享相同的trace ID
  let async1_context = @azimuth.Span::span_context(async1_span)
  let async2_context = @azimuth.Span::span_context(async2_span)
  let async3_context = @azimuth.Span::span_context(async3_span)
  
  assert_eq(@azimuth.SpanContext::trace_id(async1_context), @azimuth.SpanContext::trace_id(parent_context))
  assert_eq(@azimuth.SpanContext::trace_id(async2_context), @azimuth.SpanContext::trace_id(parent_context))
  assert_eq(@azimuth.SpanContext::trace_id(async3_context), @azimuth.SpanContext::trace_id(parent_context))
  
  // 验证父子关系
  assert_eq(@azimuth.SpanContext::parent_span_id(async1_context), @azimuth.SpanContext::span_id(parent_context))
  assert_eq(@azimuth.SpanContext::parent_span_id(async2_context), @azimuth.SpanContext::span_id(parent_context))
  assert_eq(@azimuth.SpanContext::parent_span_id(async3_context), @azimuth.SpanContext::span_id(parent_context))
  
  // 结束所有spans
  @azimuth.Span::end(async1_span)
  @azimuth.Span::end(async2_span)
  @azimuth.Span::end(async3_span)
  @azimuth.Span::end(parent_span)
  
  assert_true(true)
}

// Test 4: 追踪上下文 baggage 传播
test "trace context baggage propagation" {
  let tracer_provider = @azimuth.TracerProvider::new()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "baggage-test")
  let propagator = @azimuth.CompositePropagator::new([
    @azimuth.TraceContextPropagator::new(),
    @azimuth.BaggagePropagator::new()
  ])
  
  // 创建父span并添加 baggage
  let parent_span = @azimuth.Tracer::start_span(tracer, "baggage.test")
  let parent_context = @azimuth.Span::span_context(parent_span)
  
  // 添加 baggage 项
  let baggage = @azimuth.Baggage::new()
  @azimuth.Baggage::set_entry(baggage, "user.id", "user-12345")
  @azimuth.Baggage::set_entry(baggage, "user.role", "admin")
  @azimuth.Baggage::set_entry(baggage, "request.id", "req-67890")
  @azimuth.Baggage::set_entry(baggage, "tenant.id", "tenant-001")
  
  let context_with_baggage = @azimuth.Context::with_baggage(parent_context, baggage)
  
  // 注入上下文到载体
  let carrier = @azimuth.TextMapCarrier::new()
  @azimuth.CompositePropagator::inject(propagator, context_with_baggage, carrier)
  
  // 验证 baggage 已注入
  let baggage_header = @azimuth.TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header.length() > 0)
  assert_true(@azimuth.String::contains(baggage_header, "user.id=user-12345"))
  assert_true(@azimuth.String::contains(baggage_header, "user.role=admin"))
  
  // 提取上下文
  let extracted_context = @azimuth.CompositePropagator::extract(propagator, carrier)
  let extracted_baggage = @azimuth.Context::baggage(extracted_context)
  
  // 验证 baggage 项
  assert_eq(@azimuth.Baggage::get_value(extracted_baggage, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_value(extracted_baggage, "user.role"), Some("admin"))
  assert_eq(@azimuth.Baggage::get_value(extracted_baggage, "request.id"), Some("req-67890"))
  assert_eq(@azimuth.Baggage::get_value(extracted_baggage, "tenant.id"), Some("tenant-001"))
  
  // 创建子span并验证 baggage 传播
  let child_span = @azimuth.Tracer::start_span(tracer, "child.operation", @azimuth.SpanKind::Internal, Some(extracted_context))
  let child_context = @azimuth.Span::span_context(child_span)
  let child_baggage = @azimuth.Context::baggage(child_context)
  
  // 验证子span继承了 baggage
  assert_eq(@azimuth.Baggage::get_value(child_baggage, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_value(child_baggage, "user.role"), Some("admin"))
  
  // 在子span中修改 baggage
  @azimuth.Baggage::set_entry(child_baggage, "operation.type", "data.processing")
  @azimuth.Baggage::set_entry(child_baggage, "operation.id", "op-11111")
  
  // 验证修改后的 baggage
  assert_eq(@azimuth.Baggage::get_value(child_baggage, "operation.type"), Some("data.processing"))
  assert_eq(@azimuth.Baggage::get_value(child_baggage, "operation.id"), Some("op-11111"))
  
  // 结束spans
  @azimuth.Span::end(child_span)
  @azimuth.Span::end(parent_span)
  
  assert_true(true)
}

// Test 5: 跨协议追踪上下文传播
test "cross-protocol trace context propagation" {
  let tracer_provider = @azimuth.TracerProvider::new()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "protocol-test")
  let propagator = @azimuth.TraceContextPropagator::new()
  
  // 创建初始span
  let initial_span = @azimuth.Tracer::start_span(tracer, "http.request")
  let initial_context = @azimuth.Span::span_context(initial_span)
  
  // HTTP协议传播
  let http_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, initial_context, http_carrier)
  
  // 添加HTTP特定头
  @azimuth.TextMapCarrier::set(http_carrier, "authorization", "Bearer token123")
  @azimuth.TextMapCarrier::set(http_carrier, "content-type", "application/json")
  @azimuth.TextMapCarrier::set(http_carrier, "x-request-id", "req-http-001")
  
  // 验证HTTP头
  assert_eq(@azimuth.TextMapCarrier::get(http_carrier, "traceparent").length(), 55) // W3C traceparent格式
  assert_eq(@azimuth.TextMapCarrier::get(http_carrier, "authorization"), "Bearer token123")
  
  // 从HTTP载体提取上下文
  let http_extracted_context = @azimuth.TraceContextPropagator::extract(propagator, http_carrier)
  assert_eq(@azimuth.SpanContext::trace_id(http_extracted_context), @azimuth.SpanContext::trace_id(initial_context))
  
  // gRPC协议传播
  let grpc_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, http_extracted_context, grpc_carrier)
  
  // 添加gRPC特定元数据
  @azimuth.TextMapCarrier::set(grpc_carrier, "grpc-timeout", "30S")
  @azimuth.TextMapCarrier::set(grpc-carrier, "grpc-encoding", "gzip")
  @azimuth.TextMapCarrier::set(grpc_carrier, "x-request-id", "req-grpc-001")
  
  // 验证gRPC元数据
  assert_eq(@azimuth.TextMapCarrier::get(grpc_carrier, "traceparent").length(), 55)
  assert_eq(@azimuth.TextMapCarrier::get(grpc_carrier, "grpc-timeout"), "30S")
  
  // 从gRPC载体提取上下文
  let grpc_extracted_context = @azimuth.TraceContextPropagator::extract(propagator, grpc_carrier)
  assert_eq(@azimuth.SpanContext::trace_id(grpc_extracted_context), @azimuth.SpanContext::trace_id(initial_context))
  
  // 消息队列协议传播
  let mq_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.TraceContextPropagator::inject(propagator, grpc_extracted_context, mq_carrier)
  
  // 添加消息队列特定属性
  @azimuth.TextMapCarrier::set(mq_carrier, "message-id", "msg-12345")
  @azimuth.TextMapCarrier::set(mq_carrier, "correlation-id", "corr-67890")
  @azimuth.TextMapCarrier::set(mq_carrier, "message-timestamp", "2025-01-02T10:30:00Z")
  
  // 验证消息队列属性
  assert_eq(@azimuth.TextMapCarrier::get(mq_carrier, "traceparent").length(), 55)
  assert_eq(@azimuth.TextMapCarrier::get(mq_carrier, "message-id"), "msg-12345")
  
  // 从消息队列载体提取上下文
  let mq_extracted_context = @azimuth.TraceContextPropagator::extract(propagator, mq_carrier)
  assert_eq(@azimuth.SpanContext::trace_id(mq_extracted_context), @azimuth.SpanContext::trace_id(initial_context))
  
  // 创建最终span并验证完整追踪链
  let final_span = @azimuth.Tracer::start_span(tracer, "message.processed", @azimuth.SpanKind::Consumer, Some(mq_extracted_context))
  let final_context = @azimuth.Span::span_context(final_span)
  
  // 验证追踪链完整性
  assert_eq(@azimuth.SpanContext::trace_id(final_context), @azimuth.SpanContext::trace_id(initial_context))
  assert_eq(@azimuth.SpanContext::parent_span_id(final_context), @azimuth.SpanContext::span_id(initial_context))
  
  // 结束所有spans
  @azimuth.Span::end(final_span)
  @azimuth.Span::end(initial_span)
  
  assert_true(true)
}