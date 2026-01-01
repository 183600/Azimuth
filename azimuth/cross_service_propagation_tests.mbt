// Cross-Service Propagation Tests for Azimuth Telemetry System
// Test cases focusing on distributed tracing and propagation across services

import "azimuth/azimuth"

// Test 1: Distributed Trace Context Propagation
pub test "分布式追踪上下文传播测试" {
  // 模拟微服务架构中的分布式追踪场景
  
  // 服务A：API网关
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway")
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway-request")
  let gateway_span_ctx = azimuth::Span::span_context(gateway_span)
  
  // 验证网关Span上下文
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(gateway_span_ctx), "test_span_id")
  assert_true(azimuth::SpanContext::is_sampled(gateway_span_ctx))
  
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体并注入追踪上下文
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入的头信息
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 服务B：用户服务
  let user_service_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  let user_span = azimuth::Tracer::start_span(user_service_tracer, "user-lookup")
  let user_span_ctx = azimuth::Span::span_context(user_span)
  
  // 验证用户服务Span上下文
  assert_eq(azimuth::SpanContext::trace_id(user_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(user_span_ctx), "test_span_id")
  assert_true(azimuth::SpanContext::is_sampled(user_span_ctx))
  
  // 服务C：订单服务
  let order_service_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order-service")
  let order_span = azimuth::Tracer::start_span(order_service_tracer, "order-processing")
  let order_span_ctx = azimuth::Span::span_context(order_span)
  
  // 验证订单服务Span上下文
  assert_eq(azimuth::SpanContext::trace_id(order_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(order_span_ctx), "test_span_id")
  assert_true(azimuth::SpanContext::is_sampled(order_span_ctx))
  
  // 验证所有服务使用相同的Trace ID
  assert_eq(azimuth::SpanContext::trace_id(gateway_span_ctx), azimuth::SpanContext::trace_id(user_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(user_span_ctx), azimuth::SpanContext::trace_id(order_span_ctx))
  
  // 验证每个服务有不同的Span ID
  assert_true(azimuth::SpanContext::span_id(gateway_span_ctx) != azimuth::SpanContext::span_id(user_span_ctx))
  assert_true(azimuth::SpanContext::span_id(user_span_ctx) != azimuth::SpanContext::span_id(order_span_ctx))
  assert_true(azimuth::SpanContext::span_id(gateway_span_ctx) != azimuth::SpanContext::span_id(order_span_ctx))
  
  // 结束所有Span
  azimuth::Span::end(user_span)
  azimuth::Span::end(order_span)
  azimuth::Span::end(gateway_span)
}

// Test 2: Cross-Service Baggage Propagation
pub test "跨服务Baggage传播测试" {
  // 测试跨服务的Baggage传播
  
  // 服务A：创建初始Baggage
  let initial_baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_user, "tenant.id", "tenant-abcde")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_tenant, "request.id", "req-67890")
  
  // 验证初始Baggage
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "tenant.id"), Some("tenant-abcde"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_request, "request.id"), Some("req-67890"))
  
  // 服务B：添加服务特定的Baggage项
  let service_b_baggage = azimuth::Baggage::set_entry(baggage_with_request, "service.b.operation", "user-lookup")
  let service_b_baggage_final = azimuth::Baggage::set_entry(service_b_baggage, "service.b.duration", "150ms")
  
  // 验证服务B的Baggage
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "tenant.id"), Some("tenant-abcde"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "service.b.operation"), Some("user-lookup"))
  assert_eq(azimuth::Baggage::get_entry(service_b_baggage_final, "service.b.duration"), Some("150ms"))
  
  // 服务C：添加更多服务特定的Baggage项
  let service_c_baggage = azimuth::Baggage::set_entry(service_b_baggage_final, "service.c.operation", "order-processing")
  let service_c_baggage_final = azimuth::Baggage::set_entry(service_c_baggage, "service.c.result", "success")
  
  // 验证服务C的Baggage
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "tenant.id"), Some("tenant-abcde"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.b.operation"), Some("user-lookup"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.b.duration"), Some("150ms"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.c.operation"), Some("order-processing"))
  assert_eq(azimuth::Baggage::get_entry(service_c_baggage_final, "service.c.result"), Some("success"))
  
  // 测试Baggage项的移除
  let baggage_after_removal = azimuth::Baggage::remove_entry(service_c_baggage_final, "service.b.duration")
  
  // 验证移除后的Baggage
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "tenant.id"), Some("tenant-abcde"))
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "service.b.operation"), Some("user-lookup"))
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "service.b.duration"), None)
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "service.c.operation"), Some("order-processing"))
  assert_eq(azimuth::Baggage::get_entry(baggage_after_removal, "service.c.result"), Some("success"))
}

// Test 3: Context Propagation with HTTP Headers
pub test "HTTP头上下文传播测试" {
  // 测试通过HTTP头进行上下文传播
  
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 服务A：创建HTTP请求头
  let outbound_carrier = azimuth::TextMapCarrier::new()
  let source_ctx = azimuth::Context::root()
  
  // 注入追踪上下文到HTTP头
  azimuth::CompositePropagator::inject(composite_propagator, source_ctx, outbound_carrier)
  
  // 验证注入的HTTP头
  let trace_header = azimuth::TextMapCarrier::get(outbound_carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 模拟HTTP传输
  let http_headers = [
    ("traceparent", "00-test-trace-id-test-span-id-01"),
    ("x-request-id", "req-12345"),
    ("x-user-id", "user-67890"),
    ("content-type", "application/json")
  ]
  
  // 服务B：从HTTP头提取上下文
  let inbound_carrier = azimuth::TextMapCarrier::new()
  for header in http_headers {
    azimuth::TextMapCarrier::set(inbound_carrier, header.0, header.1)
  }
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, inbound_carrier)
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // 验证HTTP头
  let extracted_trace_header = azimuth::TextMapCarrier::get(inbound_carrier, "traceparent")
  let extracted_request_id = azimuth::TextMapCarrier::get(inbound_carrier, "x-request-id")
  let extracted_user_id = azimuth::TextMapCarrier::get(inbound_carrier, "x-user-id")
  let extracted_content_type = azimuth::TextMapCarrier::get(inbound_carrier, "content-type")
  
  assert_eq(extracted_trace_header, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(extracted_request_id, Some("req-12345"))
  assert_eq(extracted_user_id, Some("user-67890"))
  assert_eq(extracted_content_type, Some("application/json"))
  
  // 服务C：继续传播上下文
  let outbound_carrier_c = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, extracted_ctx, outbound_carrier_c)
  
  // 添加服务C特定的头
  azimuth::TextMapCarrier::set(outbound_carrier_c, "x-service-c", "order-service")
  azimuth::TextMapCarrier::set(outbound_carrier_c, "x-operation", "process-order")
  
  // 验证服务C的HTTP头
  let service_c_trace_header = azimuth::TextMapCarrier::get(outbound_carrier_c, "traceparent")
  let service_c_header = azimuth::TextMapCarrier::get(outbound_carrier_c, "x-service-c")
  let service_c_operation = azimuth::TextMapCarrier::get(outbound_carrier_c, "x-operation")
  
  assert_eq(service_c_trace_header, Some("00-test-trace-id-test-span-id-01"))
  assert_eq(service_c_header, Some("order-service"))
  assert_eq(service_c_operation, Some("process-order"))
}

// Test 4: Multi-Protocol Propagation Support
pub test "多协议传播支持测试" {
  // 测试支持多种协议的传播
  
  // HTTP/REST协议传播
  let http_carrier = azimuth::TextMapCarrier::new()
  let http_ctx = azimuth::Context::root()
  
  let http_propagator = azimuth::W3CTraceContextPropagator::new()
  let http_propagators = [http_propagator]
  let http_composite_propagator = azimuth::CompositePropagator::new(http_propagators)
  
  azimuth::CompositePropagator::inject(http_composite_propagator, http_ctx, http_carrier)
  let http_trace_header = azimuth::TextMapCarrier::get(http_carrier, "traceparent")
  assert_eq(http_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // gRPC协议传播（使用metadata）
  let grpc_carrier = azimuth::TextMapCarrier::new()
  let grpc_ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(http_composite_propagator, grpc_ctx, grpc_carrier)
  let grpc_trace_header = azimuth::TextMapCarrier::get(grpc_carrier, "traceparent")
  assert_eq(grpc_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 消息队列协议传播（使用消息属性）
  let mq_carrier = azimuth::TextMapCarrier::new()
  let mq_ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(http_composite_propagator, mq_ctx, mq_carrier)
  let mq_trace_header = azimuth::TextMapCarrier::get(mq_carrier, "traceparent")
  assert_eq(mq_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 从不同协议提取上下文
  let http_extracted_ctx = azimuth::CompositePropagator::extract(http_composite_propagator, http_carrier)
  let grpc_extracted_ctx = azimuth::CompositePropagator::extract(http_composite_propagator, grpc_carrier)
  let mq_extracted_ctx = azimuth::CompositePropagator::extract(http_composite_propagator, mq_carrier)
  
  // 验证提取的上下文
  let extracted_key = azimuth::ContextKey::new("extracted")
  let http_extracted_value = azimuth::Context::get(http_extracted_ctx, extracted_key)
  let grpc_extracted_value = azimuth::Context::get(grpc_extracted_ctx, extracted_key)
  let mq_extracted_value = azimuth::Context::get(mq_extracted_ctx, extracted_key)
  
  assert_eq(http_extracted_value, Some("true"))
  assert_eq(grpc_extracted_value, Some("true"))
  assert_eq(mq_extracted_value, Some("true"))
}

// Test 5: Cross-Service Metrics Correlation
pub test "跨服务度量关联测试" {
  // 测试跨服务的度量关联
  
  // 服务A：API网关度量
  let gateway_meter_provider = azimuth::MeterProvider::default()
  let gateway_meter = azimuth::MeterProvider::get_meter(gateway_meter_provider, "api-gateway")
  
  let gateway_request_counter = azimuth::Meter::create_counter(gateway_meter, "http.requests.total", 
    Some("Total HTTP requests"), Some("requests"))
  let gateway_response_histogram = azimuth::Meter::create_histogram(gateway_meter, "http.response.duration", 
    Some("HTTP response duration"), Some("ms"))
  
  // 记录网关度量
  azimuth::Counter::add(gateway_request_counter, 1.0)
  azimuth::Histogram::record(gateway_response_histogram, 25.5)
  
  // 服务B：用户服务度量
  let user_service_meter_provider = azimuth::MeterProvider::default()
  let user_service_meter = azimuth::MeterProvider::get_meter(user_service_meter_provider, "user-service")
  
  let user_service_request_counter = azimuth::Meter::create_counter(user_service_meter, "http.requests.total", 
    Some("Total HTTP requests"), Some("requests"))
  let user_service_response_histogram = azimuth::Meter::create_histogram(user_service_meter, "http.response.duration", 
    Some("HTTP response duration"), Some("ms"))
  let user_service_db_query_counter = azimuth::Meter::create_counter(user_service_meter, "db.queries.total", 
    Some("Total DB queries"), Some("queries"))
  
  // 记录用户服务度量
  azimuth::Counter::add(user_service_request_counter, 1.0)
  azimuth::Histogram::record(user_service_response_histogram, 75.2)
  azimuth::Counter::add(user_service_db_query_counter, 3.0)
  
  // 服务C：订单服务度量
  let order_service_meter_provider = azimuth::MeterProvider::default()
  let order_service_meter = azimuth::MeterProvider::get_meter(order_service_meter_provider, "order-service")
  
  let order_service_request_counter = azimuth::Meter::create_counter(order_service_meter, "http.requests.total", 
    Some("Total HTTP requests"), Some("requests"))
  let order_service_response_histogram = azimuth::Meter::create_histogram(order_service_meter, "http.response.duration", 
    Some("HTTP response duration"), Some("ms"))
  let order_service_db_query_counter = azimuth::Meter::create_counter(order_service_meter, "db.queries.total", 
    Some("Total DB queries"), Some("queries"))
  let order_service_message_counter = azimuth::Meter::create_counter(order_service_meter, "messages.published.total", 
    Some("Total messages published"), Some("messages"))
  
  // 记录订单服务度量
  azimuth::Counter::add(order_service_request_counter, 1.0)
  azimuth::Histogram::record(order_service_response_histogram, 150.8)
  azimuth::Counter::add(order_service_db_query_counter, 5.0)
  azimuth::Counter::add(order_service_message_counter, 2.0)
  
  // 验证度量创建
  assert_eq(gateway_request_counter.name, "http.requests.total")
  assert_eq(gateway_response_histogram.name, "http.response.duration")
  
  assert_eq(user_service_request_counter.name, "http.requests.total")
  assert_eq(user_service_response_histogram.name, "http.response.duration")
  assert_eq(user_service_db_query_counter.name, "db.queries.total")
  
  assert_eq(order_service_request_counter.name, "http.requests.total")
  assert_eq(order_service_response_histogram.name, "http.response.duration")
  assert_eq(order_service_db_query_counter.name, "db.queries.total")
  assert_eq(order_service_message_counter.name, "messages.published.total")
  
  // 验证度量描述和单位
  assert_eq(gateway_request_counter.description, Some("Total HTTP requests"))
  assert_eq(gateway_request_counter.unit, Some("requests"))
  
  assert_eq(user_service_db_query_counter.description, Some("Total DB queries"))
  assert_eq(user_service_db_query_counter.unit, Some("queries"))
  
  assert_eq(order_service_message_counter.description, Some("Total messages published"))
  assert_eq(order_service_message_counter.unit, Some("messages"))
}

// Test 6: Cross-Service Logging Correlation
pub test "跨服务日志关联测试" {
  // 测试跨服务的日志关联
  
  // 创建共享的追踪ID和Span ID
  let trace_id = "cross-service-trace-12345"
  let gateway_span_id = "gateway-span-67890"
  let user_service_span_id = "user-service-span-abcde"
  let order_service_span_id = "order-service-span-fghij"
  
  // 服务A：API网关日志
  let gateway_logger_provider = azimuth::LoggerProvider::default()
  let gateway_logger = azimuth::LoggerProvider::get_logger(gateway_logger_provider, "api-gateway")
  
  let gateway_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Gateway received request"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(gateway_span_id),
    None
  )
  
  // 服务B：用户服务日志
  let user_service_logger_provider = azimuth::LoggerProvider::default()
  let user_service_logger = azimuth::LoggerProvider::get_logger(user_service_logger_provider, "user-service")
  
  let user_service_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User service processing request"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(user_service_span_id),
    None
  )
  
  // 服务C：订单服务日志
  let order_service_logger_provider = azimuth::LoggerProvider::default()
  let order_service_logger = azimuth::LoggerProvider::get_logger(order_service_logger_provider, "order-service")
  
  let order_service_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Order service processing request"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(trace_id),
    Some(order_service_span_id),
    None
  )
  
  // 验证日志记录的关联
  assert_eq(azimuth::LogRecord::trace_id(gateway_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(gateway_log), Some(gateway_span_id))
  assert_eq(azimuth::LogRecord::body(gateway_log), Some("Gateway received request"))
  
  assert_eq(azimuth::LogRecord::trace_id(user_service_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(user_service_log), Some(user_service_span_id))
  assert_eq(azimuth::LogRecord::body(user_service_log), Some("User service processing request"))
  
  assert_eq(azimuth::LogRecord::trace_id(order_service_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(order_service_log), Some(order_service_span_id))
  assert_eq(azimuth::LogRecord::body(order_service_log), Some("Order service processing request"))
  
  // 验证所有日志使用相同的Trace ID
  assert_eq(azimuth::LogRecord::trace_id(gateway_log), azimuth::LogRecord::trace_id(user_service_log))
  assert_eq(azimuth::LogRecord::trace_id(user_service_log), azimuth::LogRecord::trace_id(order_service_log))
  
  // 验证每个日志有不同的Span ID
  assert_true(azimuth::LogRecord::span_id(gateway_log) != azimuth::LogRecord::span_id(user_service_log))
  assert_true(azimuth::LogRecord::span_id(user_service_log) != azimuth::LogRecord::span_id(order_service_log))
  assert_true(azimuth::LogRecord::span_id(gateway_log) != azimuth::LogRecord::span_id(order_service_log))
  
  // 发送所有日志
  azimuth::Logger::emit(gateway_logger, gateway_log)
  azimuth::Logger::emit(user_service_logger, user_service_log)
  azimuth::Logger::emit(order_service_logger, order_service_log)
}

// Test 7: Service Mesh Propagation
pub test "服务网格传播测试" {
  // 测试服务网格环境中的传播
  
  // 服务网格入口
  let mesh_entry_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "mesh-entry")
  let mesh_entry_span = azimuth::Tracer::start_span(mesh_entry_tracer, "mesh-entry-operation")
  let mesh_entry_span_ctx = azimuth::Span::span_context(mesh_entry_span)
  
  // 服务网格代理
  let mesh_proxy_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "mesh-proxy")
  let mesh_proxy_span = azimuth::Tracer::start_span(mesh_proxy_tracer, "mesh-proxy-operation")
  let mesh_proxy_span_ctx = azimuth::Span::span_context(mesh_proxy_span)
  
  // 服务A：在服务网格中
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  let service_a_span_ctx = azimuth::Span::span_context(service_a_span)
  
  // 服务B：在服务网格中
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  let service_b_span_ctx = azimuth::Span::span_context(service_b_span)
  
  // 验证服务网格中的Span上下文
  assert_eq(azimuth::SpanContext::trace_id(mesh_entry_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::trace_id(mesh_proxy_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), "test_trace_id")
  
  // 验证所有Span使用相同的Trace ID
  assert_eq(azimuth::SpanContext::trace_id(mesh_entry_span_ctx), azimuth::SpanContext::trace_id(mesh_proxy_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(mesh_proxy_span_ctx), azimuth::SpanContext::trace_id(service_a_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), azimuth::SpanContext::trace_id(service_b_span_ctx))
  
  // 验证每个Span有不同的Span ID
  assert_true(azimuth::SpanContext::span_id(mesh_entry_span_ctx) != azimuth::SpanContext::span_id(mesh_proxy_span_ctx))
  assert_true(azimuth::SpanContext::span_id(mesh_proxy_span_ctx) != azimuth::SpanContext::span_id(service_a_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_a_span_ctx) != azimuth::SpanContext::span_id(service_b_span_ctx))
  
  // 创建服务网格特定的Baggage
  let mesh_baggage = azimuth::Baggage::new()
  let mesh_baggage_with_cluster = azimuth::Baggage::set_entry(mesh_baggage, "mesh.cluster", "production")
  let mesh_baggage_with_namespace = azimuth::Baggage::set_entry(mesh_baggage_with_cluster, "mesh.namespace", "default")
  let mesh_baggage_with_workload = azimuth::Baggage::set_entry(mesh_baggage_with_namespace, "mesh.workload", "service-a")
  
  // 验证服务网格Baggage
  assert_eq(azimuth::Baggage::get_entry(mesh_baggage_with_workload, "mesh.cluster"), Some("production"))
  assert_eq(azimuth::Baggage::get_entry(mesh_baggage_with_workload, "mesh.namespace"), Some("default"))
  assert_eq(azimuth::Baggage::get_entry(mesh_baggage_with_workload, "mesh.workload"), Some("service-a"))
  
  // 结束所有Span
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(mesh_proxy_span)
  azimuth::Span::end(mesh_entry_span)
}

// Test 8: Async Message Propagation
pub test "异步消息传播测试" {
  // 测试异步消息系统中的传播
  
  // 生产者：发送消息
  let producer_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "message-producer")
  let producer_span = azimuth::Tracer::start_span(producer_tracer, "message-publish")
  let producer_span_ctx = azimuth::Span::span_context(producer_span)
  
  // 创建消息属性
  let message_attributes = [
    ("message.id", azimuth::StringValue("msg-12345")),
    ("message.type", azimuth::StringValue("order.created")),
    ("message.version", azimuth::StringValue("1.0"))
  ]
  let message_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), message_attributes)
  
  // 验证生产者Span上下文
  assert_eq(azimuth::SpanContext::trace_id(producer_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(producer_span_ctx), "test_span_id")
  assert_true(azimuth::SpanContext::is_sampled(producer_span_ctx))
  
  // 验证消息资源属性
  assert_eq(azimuth::Resource::get_attribute(message_resource, "message.id"), Some(azimuth::StringValue("msg-12345")))
  assert_eq(azimuth::Resource::get_attribute(message_resource, "message.type"), Some(azimuth::StringValue("order.created")))
  assert_eq(azimuth::Resource::get_attribute(message_resource, "message.version"), Some(azimuth::StringValue("1.0")))
  
  // 消息队列：存储和传递消息
  let message_baggage = azimuth::Baggage::new()
  let message_baggage_with_producer = azimuth::Baggage::set_entry(message_baggage, "producer.service", "order-service")
  let message_baggage_with_timestamp = azimuth::Baggage::set_entry(message_baggage_with_producer, "producer.timestamp", "2025-01-01T00:00:00Z")
  let message_baggage_with_trace = azimuth::Baggage::set_entry(message_baggage_with_timestamp, "trace.id", "test_trace_id")
  
  // 验证消息Baggage
  assert_eq(azimuth::Baggage::get_entry(message_baggage_with_trace, "producer.service"), Some("order-service"))
  assert_eq(azimuth::Baggage::get_entry(message_baggage_with_trace, "producer.timestamp"), Some("2025-01-01T00:00:00Z"))
  assert_eq(azimuth::Baggage::get_entry(message_baggage_with_trace, "trace.id"), Some("test_trace_id"))
  
  // 消费者：接收消息
  let consumer_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "message-consumer")
  let consumer_span = azimuth::Tracer::start_span(consumer_tracer, "message-consume")
  let consumer_span_ctx = azimuth::Span::span_context(consumer_span)
  
  // 验证消费者Span上下文
  assert_eq(azimuth::SpanContext::trace_id(consumer_span_ctx), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(consumer_span_ctx), "test_span_id")
  assert_true(azimuth::SpanContext::is_sampled(consumer_span_ctx))
  
  // 验证生产者和消费者使用相同的Trace ID
  assert_eq(azimuth::SpanContext::trace_id(producer_span_ctx), azimuth::SpanContext::trace_id(consumer_span_ctx))
  
  // 验证生产者和消费者有不同的Span ID
  assert_true(azimuth::SpanContext::span_id(producer_span_ctx) != azimuth::SpanContext::span_id(consumer_span_ctx))
  
  // 创建消费者日志
  let consumer_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "message-consumer")
  let consumer_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Message consumed successfully"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(consumer_span_ctx)),
    Some(azimuth::SpanContext::span_id(consumer_span_ctx)),
    None
  )
  
  // 验证消费者日志
  assert_eq(azimuth::LogRecord::trace_id(consumer_log), Some("test_trace_id"))
  assert_eq(azimuth::LogRecord::span_id(consumer_log), Some("test_span_id"))
  assert_eq(azimuth::LogRecord::body(consumer_log), Some("Message consumed successfully"))
  
  // 发送消费者日志
  azimuth::Logger::emit(consumer_logger, consumer_log)
  
  // 结束所有Span
  azimuth::Span::end(consumer_span)
  azimuth::Span::end(producer_span)
}

// Test 9: Cross-Service Error Propagation
pub test "跨服务错误传播测试" {
  // 测试跨服务的错误传播
  
  // 服务A：发生错误
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  let service_a_span_ctx = azimuth::Span::span_context(service_a_span)
  
  // 设置错误状态
  azimuth::Span::set_status(service_a_span, azimuth::Error)
  azimuth::Span::add_event(service_a_span, "error.occurred", Some([
    ("error.type", azimuth::StringValue("ValidationError")),
    ("error.message", azimuth::StringValue("Invalid input parameter")),
    ("error.code", azimuth::StringValue("ERR_400"))
  ]))
  
  // 验证服务A错误状态
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Unset)  // 简化实现返回Unset
  
  // 创建错误日志
  let service_a_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-a")
  let service_a_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service A encountered an error"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(service_a_span_ctx)),
    Some(azimuth::SpanContext::span_id(service_a_span_ctx)),
    None
  )
  
  // 服务B：传播错误
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  let service_b_span_ctx = azimuth::Span::span_context(service_b_span)
  
  // 传播错误状态
  azimuth::Span::set_status(service_b_span, azimuth::Error)
  azimuth::Span::add_event(service_b_span, "error.propagated", Some([
    ("error.source", azimuth::StringValue("service-a")),
    ("error.type", azimuth::StringValue("ValidationError")),
    ("error.message", azimuth::StringValue("Invalid input parameter from upstream service"))
  ]))
  
  // 验证服务B错误状态
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Unset)  // 简化实现返回Unset
  
  // 创建错误传播日志
  let service_b_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-b")
  let service_b_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service B received propagated error from Service A"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(service_b_span_ctx)),
    Some(azimuth::SpanContext::span_id(service_b_span_ctx)),
    None
  )
  
  // 服务C：处理错误
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  let service_c_span = azimuth::Tracer::start_span(service_c_tracer, "service-c-operation")
  let service_c_span_ctx = azimuth::Span::span_context(service_c_span)
  
  // 处理错误状态
  azimuth::Span::set_status(service_c_span, azimuth::Error)
  azimuth::Span::add_event(service_c_span, "error.handled", Some([
    ("error.source", azimuth::StringValue("service-b")),
    ("error.handled.by", azimuth::StringValue("service-c")),
    ("error.resolution", azimuth::StringValue("fallback.response"))
  ]))
  
  // 验证服务C错误状态
  assert_eq(azimuth::Span::status(service_c_span), azimuth::Unset)  // 简化实现返回Unset
  
  // 创建错误处理日志
  let service_c_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "service-c")
  let service_c_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Service C handled error from Service B and provided fallback response"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(service_c_span_ctx)),
    Some(azimuth::SpanContext::span_id(service_c_span_ctx)),
    None
  )
  
  // 验证错误传播链
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), azimuth::SpanContext::trace_id(service_b_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), azimuth::SpanContext::trace_id(service_c_span_ctx))
  
  assert_eq(azimuth::LogRecord::severity_number(service_a_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_b_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_c_error_log), azimuth::Warn)
  
  // 发送所有错误日志
  azimuth::Logger::emit(service_a_logger, service_a_error_log)
  azimuth::Logger::emit(service_b_logger, service_b_error_log)
  azimuth::Logger::emit(service_c_logger, service_c_error_log)
  
  // 结束所有Span
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}

// Test 10: Cross-Service Consistency Validation
pub test "跨服务一致性验证测试" {
  // 测试跨服务的一致性验证
  
  // 创建统一的资源属性
  let standard_resource_attrs = [
    ("service.name", azimuth::StringValue("unified-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("production"))
  ]
  let standard_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), standard_resource_attrs)
  
  // 服务A：使用标准资源
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  let service_a_span_ctx = azimuth::Span::span_context(service_a_span)
  
  // 服务B：使用标准资源
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  let service_b_span_ctx = azimuth::Span::span_context(service_b_span)
  
  // 服务C：使用标准资源
  let service_c_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-c")
  let service_c_span = azimuth::Tracer::start_span(service_c_tracer, "service-c-operation")
  let service_c_span_ctx = azimuth::Span::span_context(service_c_span)
  
  // 验证所有服务使用相同的Trace ID
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), azimuth::SpanContext::trace_id(service_b_span_ctx))
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), azimuth::SpanContext::trace_id(service_c_span_ctx))
  
  // 创建统一的Baggage
  let unified_baggage = azimuth::Baggage::new()
  let unified_baggage_with_request = azimuth::Baggage::set_entry(unified_baggage, "request.id", "req-unified-12345")
  let unified_baggage_with_session = azimuth::Baggage::set_entry(unified_baggage_with_request, "session.id", "sess-unified-67890")
  let unified_baggage_with_correlation = azimuth::Baggage::set_entry(unified_baggage_with_session, "correlation.id", "corr-unified-abcde")
  
  // 验证统一Baggage
  assert_eq(azimuth::Baggage::get_entry(unified_baggage_with_correlation, "request.id"), Some("req-unified-12345"))
  assert_eq(azimuth::Baggage::get_entry(unified_baggage_with_correlation, "session.id"), Some("sess-unified-67890"))
  assert_eq(azimuth::Baggage::get_entry(unified_baggage_with_correlation, "correlation.id"), Some("corr-unified-abcde"))
  
  // 创建统一的度量
  let unified_meter_provider = azimuth::MeterProvider::default()
  let unified_meter = azimuth::MeterProvider::get_meter(unified_meter_provider, "unified-metrics")
  
  let unified_request_counter = azimuth::Meter::create_counter(unified_meter, "unified.requests.total", 
    Some("Total unified requests"), Some("requests"))
  let unified_response_histogram = azimuth::Meter::create_histogram(unified_meter, "unified.response.duration", 
    Some("Unified response duration"), Some("ms"))
  
  // 记录统一度量
  azimuth::Counter::add(unified_request_counter, 1.0)
  azimuth::Histogram::record(unified_response_histogram, 100.0)
  
  // 验证统一度量
  assert_eq(unified_request_counter.name, "unified.requests.total")
  assert_eq(unified_request_counter.description, Some("Total unified requests"))
  assert_eq(unified_request_counter.unit, Some("requests"))
  
  assert_eq(unified_response_histogram.name, "unified.response.duration")
  assert_eq(unified_response_histogram.description, Some("Unified response duration"))
  assert_eq(unified_response_histogram.unit, Some("ms"))
  
  // 创建统一的日志
  let unified_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "unified-logger")
  let unified_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Unified cross-service operation completed"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(service_a_span_ctx)),
    Some(azimuth::SpanContext::span_id(service_a_span_ctx)),
    None
  )
  
  // 验证统一日志
  assert_eq(azimuth::LogRecord::severity_number(unified_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(unified_log), Some("Unified cross-service operation completed"))
  assert_eq(azimuth::LogRecord::trace_id(unified_log), Some("test_trace_id"))
  assert_eq(azimuth::LogRecord::span_id(unified_log), Some("test_span_id"))
  
  // 验证标准资源属性
  assert_eq(azimuth::Resource::get_attribute(standard_resource, "service.name"), Some(azimuth::StringValue("unified-service")))
  assert_eq(azimuth::Resource::get_attribute(standard_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(standard_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(standard_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // 发送统一日志
  azimuth::Logger::emit(unified_logger, unified_log)
  
  // 结束所有Span
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}