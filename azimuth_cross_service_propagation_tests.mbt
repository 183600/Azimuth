// Azimuth Cross-Service Telemetry Data Propagation Tests
// 跨服务遥测数据传播测试 - 验证遥测数据在微服务架构中的正确传播

test "跨服务HTTP请求Trace传播" {
  // 测试通过HTTP请求在服务间传播trace上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.http")
  
  // 服务A创建根span
  let service_a_span = Tracer::start_span(tracer, "service.a.http.request")
  Span::set_attribute(service_a_span, "service.name", StringValue("service.a"))
  Span::set_attribute(service_a_span, "http.method", StringValue("GET"))
  Span::set_attribute(service_a_span, "http.url", StringValue("http://service-b/api/data"))
  
  // 模拟HTTP请求传播
  let propagator = TraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入trace上下文到HTTP头
  let service_a_context = Span::get_context(service_a_span)
  Propagator::inject(propagator, service_a_context, carrier)
  
  // 服务B从HTTP头提取trace上下文
  let service_b_context = Propagator::extract(propagator, carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.http.process", service_b_context)
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  Span::set_attribute(service_b_span, "http.method", StringValue("GET"))
  Span::set_attribute(service_b_span, "http.target", StringValue("/api/data"))
  
  // 验证trace上下文传播
  let service_a_trace_id = SpanContext::get_trace_id(service_a_context)
  let service_b_trace_id = SpanContext::get_trace_id(Span::get_context(service_b_span))
  
  assert_eq(service_a_trace_id, service_b_trace_id)
  assert_eq(SpanContext::get_parent_span_id(Span::get_context(service_b_span)), SpanContext::get_span_id(service_a_context))
  
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "跨服务消息队列Trace传播" {
  // 测试通过消息队列在服务间传播trace上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.message")
  
  // 生产者服务创建span
  let producer_span = Tracer::start_span(tracer, "producer.send.message")
  Span::set_attribute(producer_span, "service.name", StringValue("producer.service"))
  Span::set_attribute(producer_span, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(producer_span, "messaging.destination", StringValue("events.queue"))
  Span::set_attribute(producer_span, "messaging.message_id", StringValue("msg-12345"))
  
  // 模拟消息传播
  let propagator = TraceContextPropagator::new()
  let message_carrier = TextMapCarrier::new()
  
  // 注入trace上下文到消息属性
  let producer_context = Span::get_context(producer_span)
  Propagator::inject(propagator, producer_context, message_carrier)
  
  // 消费者服务从消息属性提取trace上下文
  let consumer_context = Propagator::extract(propagator, message_carrier)
  let consumer_span = Tracer::start_span_with_context(tracer, "consumer.process.message", consumer_context)
  Span::set_attribute(consumer_span, "service.name", StringValue("consumer.service"))
  Span::set_attribute(consumer_span, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(consumer_span, "messaging.destination", StringValue("events.queue"))
  Span::set_attribute(consumer_span, "messaging.message_id", StringValue("msg-12345"))
  Span::set_attribute(consumer_span, "messaging.operation", StringValue("process"))
  
  // 验证trace上下文传播
  let producer_trace_id = SpanContext::get_trace_id(producer_context)
  let consumer_trace_id = SpanContext::get_trace_id(Span::get_context(consumer_span))
  
  assert_eq(producer_trace_id, consumer_trace_id)
  assert_eq(SpanContext::get_parent_span_id(Span::get_context(consumer_span)), SpanContext::get_span_id(producer_context))
  
  Span::end(consumer_span)
  Span::end(producer_span)
  
  assert_true(true)
}

test "跨服务Baggage传播" {
  // 测试Baggage在跨服务调用中的传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.baggage")
  
  // 服务A设置baggage
  let service_a_context = Context::root()
  let user_id_key = ContextKey::new("user.id")
  let session_id_key = ContextKey::new("session.id")
  let request_id_key = ContextKey::new("request.id")
  
  let service_a_context_with_baggage = service_a_context
    |> Context::with_value(user_id_key, "user-12345")
    |> Context::with_value(session_id_key, "session-abcdef")
    |> Context::with_value(request_id_key, "req-67890")
  
  let service_a_span = Tracer::start_span_with_context(tracer, "service.a.operation", service_a_context_with_baggage)
  Span::set_attribute(service_a_span, "service.name", StringValue("service.a"))
  
  // 模拟baggage传播
  let baggage_propagator = BaggagePropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入baggage
  let service_a_span_context = Span::get_context(service_a_span)
  BaggagePropagator::inject(baggage_propagator, service_a_span_context, carrier)
  
  // 服务B提取baggage
  let service_b_context = BaggagePropagator::extract(baggage_propagator, carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.operation", service_b_context)
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  
  // 验证baggage传播
  let service_b_span_context = Span::get_context(service_b_span)
  let extracted_user_id = Context::get(service_b_span_context, user_id_key)
  let extracted_session_id = Context::get(service_b_span_context, session_id_key)
  let extracted_request_id = Context::get(service_b_span_context, request_id_key)
  
  match extracted_user_id {
    Some(id) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
  
  match extracted_session_id {
    Some(id) => assert_eq(id, "session-abcdef")
    None => assert_true(false)
  }
  
  match extracted_request_id {
    Some(id) => assert_eq(id, "req-67890")
    None => assert_true(false)
  }
  
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "跨服务异步调用Trace传播" {
  // 测试异步调用中的trace上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.async")
  
  // 主服务创建根span
  let main_service_span = Tracer::start_span(tracer, "main.service.async.operation")
  Span::set_attribute(main_service_span, "service.name", StringValue("main.service"))
  Span::set_attribute(main_service_span, "operation.type", StringValue("async.workflow"))
  
  let main_context = Span::get_context(main_service_span)
  
  // 模拟多个异步调用
  let async_contexts = []
  for i = 0; i < 3; i = i + 1 {
    // 为每个异步调用创建子span
    let async_span = Tracer::start_span_with_parent(tracer, "async.call." + i.to_string(), Some(main_context))
    Span::set_attribute(async_span, "service.name", StringValue("async.service." + i.to_string()))
    Span::set_attribute(async_span, "async.index", IntValue(i))
    
    // 模拟异步操作传播上下文
    let async_propagator = TraceContextPropagator::new()
    let async_carrier = TextMapCarrier::new()
    let async_context = Span::get_context(async_span)
    
    Propagator::inject(async_propagator, async_context, async_carrier)
    
    // 异步服务提取上下文
    let async_service_context = Propagator::extract(async_propagator, async_carrier)
    let async_service_span = Tracer::start_span_with_context(tracer, "async.service.operation", async_service_context)
    Span::set_attribute(async_service_span, "service.name", StringValue("async.service." + i.to_string()))
    Span::set_attribute(async_service_span, "operation.type", StringValue("async.process"))
    
    // 验证异步调用trace传播
    let main_trace_id = SpanContext::get_trace_id(main_context)
    let async_service_trace_id = SpanContext::get_trace_id(Span::get_context(async_service_span))
    
    assert_eq(main_trace_id, async_service_trace_id)
    assert_eq(SpanContext::get_parent_span_id(Span::get_context(async_service_span)), SpanContext::get_span_id(async_context))
    
    async_contexts.push(Span::get_context(async_service_span))
    Span::end(async_service_span)
    Span::end(async_span)
  }
  
  Span::end(main_service_span)
  
  // 验证所有异步操作都有相同的trace ID
  let main_trace_id = SpanContext::get_trace_id(main_context)
  for async_context in async_contexts {
    assert_eq(SpanContext::get_trace_id(async_context), main_trace_id)
  }
  
  assert_true(true)
}

test "跨服务gRPC调用Trace传播" {
  // 测试gRPC调用中的trace上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.grpc")
  
  // gRPC客户端创建span
  let grpc_client_span = Tracer::start_span(tracer, "grpc.client.call")
  Span::set_attribute(grpc_client_span, "service.name", StringValue("grpc.client"))
  Span::set_attribute(grpc_client_span, "rpc.system", StringValue("grpc"))
  Span::set_attribute(grpc_client_span, "rpc.service", StringValue("example.Service"))
  Span::set_attribute(grpc_client_span, "rpc.method", StringValue("GetUserData"))
  
  // 模拟gRPC元数据传播
  let grpc_propagator = TraceContextPropagator::new()
  let grpc_metadata = TextMapCarrier::new()
  
  // 注入trace上下文到gRPC元数据
  let client_context = Span::get_context(grpc_client_span)
  Propagator::inject(grpc_propagator, client_context, grpc_metadata)
  
  // gRPC服务器从元数据提取trace上下文
  let server_context = Propagator::extract(grpc_propagator, grpc_metadata)
  let grpc_server_span = Tracer::start_span_with_context(tracer, "grpc.server.process", server_context)
  Span::set_attribute(grpc_server_span, "service.name", StringValue("grpc.server"))
  Span::set_attribute(grpc_server_span, "rpc.system", StringValue("grpc"))
  Span::set_attribute(grpc_server_span, "rpc.service", StringValue("example.Service"))
  Span::set_attribute(grpc_server_span, "rpc.method", StringValue("GetUserData"))
  
  // 验证gRPC调用trace传播
  let client_trace_id = SpanContext::get_trace_id(client_context)
  let server_trace_id = SpanContext::get_trace_id(Span::get_context(grpc_server_span))
  
  assert_eq(client_trace_id, server_trace_id)
  assert_eq(SpanContext::get_parent_span_id(Span::get_context(grpc_server_span)), SpanContext::get_span_id(grpc_client_span))
  
  Span::end(grpc_server_span)
  Span::end(grpc_client_span)
  
  assert_true(true)
}

test "跨服务数据库操作Trace传播" {
  // 测试数据库操作中的trace上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.database")
  
  // 应用服务创建span
  let app_service_span = Tracer::start_span(tracer, "app.service.operation")
  Span::set_attribute(app_service_span, "service.name", StringValue("app.service"))
  Span::set_attribute(app_service_span, "operation.type", StringValue("business.logic"))
  
  // 数据库客户端创建子span
  let db_client_span = Tracer::start_span_with_parent(tracer, "db.client.query", Some(Span::get_context(app_service_span)))
  Span::set_attribute(db_client_span, "service.name", StringValue("database.client"))
  Span::set_attribute(db_client_span, "db.system", StringValue("postgresql"))
  Span::set_attribute(db_client_span, "db.name", StringValue("production_db"))
  Span::set_attribute(db_client_span, "db.operation", StringValue("SELECT"))
  Span::set_attribute(db_client_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  
  // 模拟数据库连接传播
  let db_propagator = TraceContextPropagator::new()
  let db_connection_carrier = TextMapCarrier::new()
  
  // 注入trace上下文到数据库连接属性
  let db_client_context = Span::get_context(db_client_span)
  Propagator::inject(db_propagator, db_client_context, db_connection_carrier)
  
  // 数据库服务器从连接属性提取trace上下文
  let db_server_context = Propagator::extract(db_propagator, db_connection_carrier)
  let db_server_span = Tracer::start_span_with_context(tracer, "db.server.execute", db_server_context)
  Span::set_attribute(db_server_span, "service.name", StringValue("database.server"))
  Span::set_attribute(db_server_span, "db.system", StringValue("postgresql"))
  Span::set_attribute(db_server_span, "db.name", StringValue("production_db"))
  Span::set_attribute(db_server_span, "db.operation", StringValue("SELECT"))
  Span::set_attribute(db_server_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  
  // 验证数据库操作trace传播
  let app_trace_id = SpanContext::get_trace_id(Span::get_context(app_service_span))
  let db_client_trace_id = SpanContext::get_trace_id(db_client_context)
  let db_server_trace_id = SpanContext::get_trace_id(Span::get_context(db_server_span))
  
  assert_eq(app_trace_id, db_client_trace_id)
  assert_eq(db_client_trace_id, db_server_trace_id)
  assert_eq(SpanContext::get_parent_span_id(db_client_context), SpanContext::get_span_id(Span::get_context(app_service_span)))
  assert_eq(SpanContext::get_parent_span_id(db_server_context), SpanContext::get_span_id(db_client_context))
  
  Span::end(db_server_span)
  Span::end(db_client_span)
  Span::end(app_service_span)
  
  assert_true(true)
}

test "跨服务链式调用Trace传播" {
  // 测试多级服务链式调用中的trace上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.chain")
  
  // 服务A创建根span
  let service_a_span = Tracer::start_span(tracer, "service.a.entry")
  Span::set_attribute(service_a_span, "service.name", StringValue("service.a"))
  Span::set_attribute(service_a_span, "operation.type", StringValue("entry.point"))
  
  let service_a_context = Span::get_context(service_a_span)
  
  // 服务A调用服务B
  let service_a_to_b_propagator = TraceContextPropagator::new()
  let service_a_to_b_carrier = TextMapCarrier::new()
  Propagator::inject(service_a_to_b_propagator, service_a_context, service_a_to_b_carrier)
  
  let service_b_context = Propagator::extract(service_a_to_b_propagator, service_a_to_b_carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.process", service_b_context)
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  Span::set_attribute(service_b_span, "operation.type", StringValue("middle.process"))
  
  let service_b_context_span = Span::get_context(service_b_span)
  
  // 服务B调用服务C
  let service_b_to_c_propagator = TraceContextPropagator::new()
  let service_b_to_c_carrier = TextMapCarrier::new()
  Propagator::inject(service_b_to_c_propagator, service_b_context_span, service_b_to_c_carrier)
  
  let service_c_context = Propagator::extract(service_b_to_c_propagator, service_b_to_c_carrier)
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.final", service_c_context)
  Span::set_attribute(service_c_span, "service.name", StringValue("service.c"))
  Span::set_attribute(service_c_span, "operation.type", StringValue("final.process"))
  
  // 验证链式调用trace传播
  let service_a_trace_id = SpanContext::get_trace_id(service_a_context)
  let service_b_trace_id = SpanContext::get_trace_id(service_b_context)
  let service_c_trace_id = SpanContext::get_trace_id(service_c_context)
  
  // 所有服务应该有相同的trace ID
  assert_eq(service_a_trace_id, service_b_trace_id)
  assert_eq(service_b_trace_id, service_c_trace_id)
  
  // 验证父子关系链
  assert_eq(SpanContext::get_parent_span_id(service_b_context), SpanContext::get_span_id(service_a_context))
  assert_eq(SpanContext::get_parent_span_id(service_c_context), SpanContext::get_span_id(service_b_context_span))
  
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  assert_true(true)
}

test "跨服务混合传播方式一致性" {
  // 测试不同传播方式在跨服务调用中的一致性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.mixed")
  
  // 服务A创建根span
  let service_a_span = Tracer::start_span(tracer, "service.a.mixed.propagation")
  Span::set_attribute(service_a_span, "service.name", StringValue("service.a"))
  Span::set_attribute(service_a_span, "operation.type", StringValue("mixed.propagation.test"))
  
  let service_a_context = Span::get_context(service_a_span)
  
  // 设置baggage
  let baggage_propagator = BaggagePropagator::new()
  let user_id_key = ContextKey::new("user.id")
  let service_a_context_with_baggage = Context::with_value(service_a_context, user_id_key, "user-12345")
  
  // HTTP传播到服务B
  let http_propagator = TraceContextPropagator::new()
  let http_carrier = TextMapCarrier::new()
  Propagator::inject(http_propagator, service_a_context_with_baggage, http_carrier)
  BaggagePropagator::inject(baggage_propagator, service_a_context_with_baggage, http_carrier)
  
  let service_b_http_context = Propagator::extract(http_propagator, http_carrier)
  let service_b_http_context_with_baggage = BaggagePropagator::extract(baggage_propagator, http_carrier)
  let service_b_span = Tracer::start_span_with_context(tracer, "service.b.http.received", service_b_http_context)
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  Span::set_attribute(service_b_span, "propagation.method", StringValue("http"))
  
  // 消息队列传播到服务C
  let message_carrier = TextMapCarrier::new()
  Propagator::inject(http_propagator, service_a_context_with_baggage, message_carrier)
  BaggagePropagator::inject(baggage_propagator, service_a_context_with_baggage, message_carrier)
  
  let service_c_message_context = Propagator::extract(http_propagator, message_carrier)
  let service_c_message_context_with_baggage = BaggagePropagator::extract(baggage_propagator, message_carrier)
  let service_c_span = Tracer::start_span_with_context(tracer, "service.c.message.received", service_c_message_context)
  Span::set_attribute(service_c_span, "service.name", StringValue("service.c"))
  Span::set_attribute(service_c_span, "propagation.method", StringValue("message"))
  
  // 验证混合传播方式的一致性
  let service_a_trace_id = SpanContext::get_trace_id(service_a_context)
  let service_b_trace_id = SpanContext::get_trace_id(Span::get_context(service_b_span))
  let service_c_trace_id = SpanContext::get_trace_id(Span::get_context(service_c_span))
  
  // 所有服务应该有相同的trace ID
  assert_eq(service_a_trace_id, service_b_trace_id)
  assert_eq(service_b_trace_id, service_c_trace_id)
  
  // 验证baggage在不同传播方式中的一致性
  let service_b_user_id = Context::get(Span::get_context(service_b_span), user_id_key)
  let service_c_user_id = Context::get(Span::get_context(service_c_span), user_id_key)
  
  match service_b_user_id {
    Some(id) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
  
  match service_c_user_id {
    Some(id) => assert_eq(id, "user-12345")
    None => assert_true(false)
  }
  
  Span::end(service_b_span)
  Span::end(service_c_span)
  Span::end(service_a_span)
  
  assert_true(true)
}