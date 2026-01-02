// 分布式追踪传播测试
// 测试Azimuth遥测系统的分布式追踪上下文传播功能

test "HTTP头部追踪上下文传播" {
  // 创建初始追踪上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "http.propagation.test")
  
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // 创建传播器
  let propagator = HttpTraceContextPropagator::new()
  
  // 将追踪上下文注入到HTTP头部
  let carrier = HttpHeaderCarrier::new()
  propagator.inject(parent_ctx, carrier)
  
  // 验证追踪头部存在
  let traceparent = carrier.get("traceparent")
  let traceresponse = carrier.get("traceresponse")
  
  assert_true(traceparent.is_some())
  assert_eq(traceparent.some().split("-").length(), 4)  // version-trace-id-parent-id-flags
  
  // 模拟HTTP请求传输
  let request_headers = carrier.to_map()
  let receiving_carrier = HttpHeaderCarrier::from_map(request_headers)
  
  // 在接收端提取追踪上下文
  let extracted_ctx = propagator.extract(receiving_carrier)
  
  // 验证提取的上下文
  assert_true(SpanContext::is_valid(extracted_ctx))
  assert_eq(SpanContext::trace_id(extracted_ctx), SpanContext::trace_id(parent_ctx))
  assert_eq(SpanContext::span_id(extracted_ctx), SpanContext::span_id(parent_ctx))
  
  // 创建子span并验证父子关系
  let child_span = Tracer::start_span_with_context(tracer, "child.operation", extracted_ctx)
  let child_ctx = Span::span_context(child_span)
  
  assert_eq(SpanContext::trace_id(child_ctx), SpanContext::trace_id(parent_ctx))
  assert_true(SpanContext::is_valid(child_ctx))
  
  Span::end(child_span)
  Span::end(parent_span)
}

test "跨服务追踪传播" {
  // 模拟微服务架构中的追踪传播
  let services = ["auth-service", "user-service", "order-service", "payment-service"]
  let propagator = HttpTraceContextPropagator::new()
  
  // 在第一个服务中创建追踪
  let auth_tracer = TracerProvider::default().get_tracer("auth-service")
  let auth_span = auth_tracer.start_span("user.authentication")
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "operation.name", "authenticate")
  
  let auth_ctx = Span::span_context(auth_span)
  
  // 传播到下一个服务
  let auth_carrier = HttpHeaderCarrier::new()
  propagator.inject(auth_ctx, auth_carrier)
  
  // 在用户服务中接收并继续追踪
  let user_carrier = HttpHeaderCarrier::from_map(auth_carrier.to_map())
  let user_ctx = propagator.extract(user_carrier)
  
  let user_tracer = TracerProvider::default().get_tracer("user-service")
  let user_span = user_tracer.start_span_with_context("user.profile.fetch", user_ctx)
  Span::set_attribute(user_span, "service.name", "user-service")
  Span::set_attribute(user_span, "operation.name", "fetch_profile")
  Span::set_attribute(user_span, "parent.service", "auth-service")
  
  // 继续传播到订单服务
  let user_carrier_out = HttpHeaderCarrier::new()
  propagator.inject(Span::span_context(user_span), user_carrier_out)
  
  let order_carrier = HttpHeaderCarrier::from_map(user_carrier_out.to_map())
  let order_ctx = propagator.extract(order_carrier)
  
  let order_tracer = TracerProvider::default().get_tracer("order-service")
  let order_span = order_tracer.start_span_with_context("order.creation", order_ctx)
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "operation.name", "create_order")
  Span::set_attribute(order_span, "parent.service", "user-service")
  
  // 验证所有span都在同一个追踪中
  let auth_trace_id = SpanContext::trace_id(Span::span_context(auth_span))
  let user_trace_id = SpanContext::trace_id(Span::span_context(user_span))
  let order_trace_id = SpanContext::trace_id(Span::span_context(order_span))
  
  assert_eq(auth_trace_id, user_trace_id)
  assert_eq(user_trace_id, order_trace_id)
  
  Span::end(order_span)
  Span::end(user_span)
  Span::end(auth_span)
}

test " baggage跨服务传播" {
  // 测试baggage项在分布式系统中的传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.propagation.test")
  
  let span = Tracer::start_span(tracer, "baggage.test.operation")
  let ctx = Span::span_context(span)
  
  // 创建baggage传播器
  let baggage_propagator = BaggagePropagator::new()
  
  // 设置初始baggage
  let initial_baggage = Baggage::new()
  initial_baggage = Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  initial_baggage = Baggage::set_entry(initial_baggage, "request.id", "req-abcdef")
  initial_baggage = Baggage::set_entry(initial_baggage, "tenant.id", "tenant-789")
  initial_baggage = Baggage::set_entry(initial_baggage, "session.id", "sess-123456")
  
  // 将baggage注入到载体
  let carrier = HttpHeaderCarrier::new()
  baggage_propagator.inject(ctx, initial_baggage, carrier)
  
  // 验证baggage头部
  let baggage_header = carrier.get("baggage")
  assert_true(baggage_header.is_some())
  
  // 在接收端提取baggage
  let receiving_carrier = HttpHeaderCarrier::from_map(carrier.to_map())
  let extracted_baggage = baggage_propagator.extract(ctx, receiving_carrier)
  
  // 验证提取的baggage项
  let user_id = Baggage::get_value(extracted_baggage, "user.id")
  let request_id = Baggage::get_value(extracted_baggage, "request.id")
  let tenant_id = Baggage::get_value(extracted_baggage, "tenant.id")
  let session_id = Baggage::get_value(extracted_baggage, "session.id")
  
  assert_eq(user_id, Some("user-12345"))
  assert_eq(request_id, Some("req-abcdef"))
  assert_eq(tenant_id, Some("tenant-789"))
  assert_eq(session_id, Some("sess-123456"))
  
  // 测试baggage在多个服务间的传递
  let service2_carrier = HttpHeaderCarrier::new()
  baggage_propagator.inject(ctx, extracted_baggage, service2_carrier)
  
  let service3_carrier = HttpHeaderCarrier::from_map(service2_carrier.to_map())
  let final_baggage = baggage_propagator.extract(ctx, service3_carrier)
  
  // 验证baggage在多次传递后仍然完整
  let final_user_id = Baggage::get_value(final_baggage, "user.id")
  assert_eq(final_user_id, Some("user-12345"))
  
  Span::end(span)
}

test "复合传播器测试" {
  // 测试同时使用多个传播器（追踪上下文和baggage）
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "composite.propagation.test")
  
  let span = Tracer::start_span(tracer, "composite.test.operation")
  let ctx = Span::span_context(span)
  
  // 创建复合传播器
  let trace_propagator = HttpTraceContextPropagator::new()
  let baggage_propagator = BaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 设置baggage
  let baggage = Baggage::new()
  baggage = Baggage::set_entry(baggage, "service.version", "1.2.3")
  baggage = Baggage::set_entry(baggage, "deployment.env", "production")
  baggage = Baggage::set_entry(baggage, "region", "us-west-2")
  
  // 使用复合传播器注入
  let carrier = HttpHeaderCarrier::new()
  composite_propagator.inject(ctx, baggage, carrier)
  
  // 验证包含所有必要的头部
  let traceparent = carrier.get("traceparent")
  let baggage_header = carrier.get("baggage")
  
  assert_true(traceparent.is_some())
  assert_true(baggage_header.is_some())
  
  // 在接收端使用复合传播器提取
  let receiving_carrier = HttpHeaderCarrier::from_map(carrier.to_map())
  let extraction_result = composite_propagator.extract(receiving_carrier)
  
  // 验证提取的结果
  let extracted_ctx = extraction_result.context
  let extracted_baggage = extraction_result.baggage
  
  assert_true(SpanContext::is_valid(extracted_ctx))
  assert_eq(SpanContext::trace_id(extracted_ctx), SpanContext::trace_id(ctx))
  
  let service_version = Baggage::get_value(extracted_baggage, "service.version")
  let deployment_env = Baggage::get_value(extracted_baggage, "deployment.env")
  let region = Baggage::get_value(extracted_baggage, "region")
  
  assert_eq(service_version, Some("1.2.3"))
  assert_eq(deployment_env, Some("production"))
  assert_eq(region, Some("us-west-2"))
  
  Span::end(span)
}

test "异步消息队列追踪传播" {
  // 测试在异步消息队列系统中的追踪传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async.propagation.test")
  
  // 生产者端创建span
  let producer_span = Tracer::start_span(tracer, "message.produce")
  Span::set_attribute(producer_span, "messaging.system", "rabbitmq")
  Span::set_attribute(producer_span, "messaging.destination", "user.events")
  Span::set_attribute(producer_span, "messaging.message_id", "msg-12345")
  
  let producer_ctx = Span::span_context(producer_span)
  
  // 创建消息传播器
  let message_propagator = MessagePropagator::new()
  
  // 将追踪上下文注入到消息属性
  let message = Message::new("user.created", "{\"user_id\": \"user-123\"}")
  let enriched_message = message_propagator.inject(producer_ctx, message)
  
  // 验证消息包含追踪信息
  let trace_context = enriched_message.get_property("trace_context")
  assert_true(trace_context.is_some())
  
  // 模拟消息队列传输
  let transported_message = Message::from_json(enriched_message.to_json())
  
  // 消费者端提取追踪上下文
  let consumer_ctx = message_propagator.extract(transported_message)
  
  // 验证提取的上下文
  assert_true(SpanContext::is_valid(consumer_ctx))
  assert_eq(SpanContext::trace_id(consumer_ctx), SpanContext::trace_id(producer_ctx))
  
  // 消费者创建消费者span
  let consumer_span = Tracer::start_span_with_context(tracer, "message.consume", consumer_ctx)
  Span::set_attribute(consumer_span, "messaging.system", "rabbitmq")
  Span::set_attribute(consumer_span, "messaging.destination", "user.events")
  Span::set_attribute(consumer_span, "messaging.message_id", "msg-12345")
  Span::set_attribute(consumer_span, "messaging.operation", "process")
  
  // 验证父子关系
  let consumer_trace_id = SpanContext::trace_id(Span::span_context(consumer_span))
  let producer_trace_id = SpanContext::trace_id(Span::span_context(producer_span))
  
  assert_eq(consumer_trace_id, producer_trace_id)
  
  Span::end(consumer_span)
  Span::end(producer_span)
}

test "传播边界条件和错误处理" {
  // 测试传播过程中的边界条件和错误处理
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "boundary.propagation.test")
  
  let span = Tracer::start_span(tracer, "boundary.test.operation")
  let ctx = Span::span_context(span)
  
  let propagator = HttpTraceContextPropagator::new()
  
  // 测试空载体处理
  let empty_carrier = HttpHeaderCarrier::new()
  propagator.inject(ctx, empty_carrier)
  
  let empty_extraction = propagator.extract(empty_carrier)
  assert_true(SpanContext::is_valid(empty_extraction))
  
  // 测试损坏的追踪头部
  let corrupted_carrier = HttpHeaderCarrier::new()
  corrupted_carrier.set("traceparent", "invalid-traceparent-format")
  
  let corrupted_extraction = propagator.extract(corrupted_carrier)
  assert_false(SpanContext::is_valid(corrupted_extraction))
  
  // 测试部分缺失的头部
  let partial_carrier = HttpHeaderCarrier::new()
  partial_carrier.set("traceparent", "00-12345678901234567890123456789012-1234567890123456-01")
  // 缺少必要的部分
  
  let partial_extraction = propagator.extract(partial_carrier)
  // 应该能够处理部分有效的头部
  
  // 测试超长头部值处理
  let long_carrier = HttpHeaderCarrier::new()
  let long_trace_id = "a" * 32
  let long_span_id = "b" * 16
  long_carrier.set("traceparent", "00-" + long_trace_id + "-" + long_span_id + "-01")
  
  let long_extraction = propagator.extract(long_carrier)
  assert_true(SpanContext::is_valid(long_extraction))
  
  // 测试特殊字符处理
  let special_carrier = HttpHeaderCarrier::new()
  special_carrier.set("traceparent", "00-12345678901234567890123456789012-1234567890123456-01")
  special_carrier.set("baggage", "key1=value1,key2=value with spaces,key3=value@#$%^&*()")
  
  let baggage_propagator = BaggagePropagator::new()
  let special_baggage = baggage_propagator.extract(ctx, special_carrier)
  
  let value1 = Baggage::get_value(special_baggage, "key1")
  let value2 = Baggage::get_value(special_baggage, "key2")
  let value3 = Baggage::get_value(special_baggage, "key3")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value with spaces"))
  assert_eq(value3, Some("value@#$%^&*()"))
  
  Span::end(span)
}