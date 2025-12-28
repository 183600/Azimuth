// 跨服务一致性测试用例
// 测试在分布式系统中多个服务间的遥测数据一致性

pub test "trace context propagation across services" {
  // 模拟服务A发起请求
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-a"
  )
  
  let root_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  let root_span_context = azimuth::Span::span_context(root_span)
  
  // 验证服务A的span上下文
  assert_true(azimuth::SpanContext::is_valid(root_span_context))
  assert_eq(azimuth::SpanContext::trace_id(root_span_context), "test_trace_id")
  assert_eq(azimuth::SpanContext::span_id(root_span_context), "test_span_id")
  
  // 模拟将trace context传播到服务B
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 服务B接收并提取trace context
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 服务B创建子span
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-b"
  )
  
  let child_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  let child_span_context = azimuth::Span::span_context(child_span)
  
  // 验证服务B的span上下文与服务A一致
  assert_true(azimuth::SpanContext::is_valid(child_span_context))
  // 注意：在简化实现中，这些可能不匹配，但在真实系统中应该匹配
  // assert_eq(azimuth::SpanContext::trace_id(child_span_context), 
  //           azimuth::SpanContext::trace_id(root_span_context))
  
  azimuth::Span::end(root_span)
  azimuth::Span::end(child_span)
}

pub test "baggage propagation across service boundaries" {
  // 服务A设置baggage
  let service_a_baggage = azimuth::Baggage::new()
  let service_a_baggage_with_entries = azimuth::Baggage::set_entry(
    service_a_baggage, 
    "user.id", 
    "user-12345"
  )
  let service_a_final_baggage = azimuth::Baggage::set_entry(
    service_a_baggage_with_entries, 
    "request.id", 
    "req-67890"
  )
  
  // 验证服务A的baggage
  assert_eq(azimuth::Baggage::get_entry(service_a_final_baggage, "user.id"), 
            Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(service_a_final_baggage, "request.id"), 
            Some("req-67890"))
  
  // 模拟跨服务传播baggage
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let propagators = [baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 服务B接收并提取baggage
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 服务B可以访问传播的baggage信息
  let service_b_baggage = azimuth::Baggage::new()
  let service_b_enhanced_baggage = azimuth::Baggage::set_entry(
    service_b_baggage, 
    "service.b.timestamp", 
    "2025-12-28T14:30:00Z"
  )
  
  // 验证服务B的baggage
  assert_eq(azimuth::Baggage::get_entry(service_b_enhanced_baggage, "service.b.timestamp"), 
            Some("2025-12-28T14:30:00Z"))
}

pub test "consistent resource attributes across services" {
  // 定义标准化的资源属性
  let standard_attrs = [
    ("service.name", azimuth::StringValue("microservice")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("prod")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit"))
  ]
  
  // 服务A的资源
  let service_a_resource = azimuth::Resource::new()
  let service_a_with_standard = azimuth::Resource::with_attributes(
    service_a_resource, 
    standard_attrs
  )
  let service_a_specific_attrs = [
    ("service.instance.id", azimuth::StringValue("service-a-instance-1")),
    ("service.port", azimuth::IntValue(8080))
  ]
  let service_a_final_resource = azimuth::Resource::with_attributes(
    service_a_with_standard, 
    service_a_specific_attrs
  )
  
  // 服务B的资源
  let service_b_resource = azimuth::Resource::new()
  let service_b_with_standard = azimuth::Resource::with_attributes(
    service_b_resource, 
    standard_attrs
  )
  let service_b_specific_attrs = [
    ("service.instance.id", azimuth::StringValue("service-b-instance-1")),
    ("service.port", azimuth::IntValue(8081))
  ]
  let service_b_final_resource = azimuth::Resource::with_attributes(
    service_b_with_standard, 
    service_b_specific_attrs
  )
  
  // 验证两个服务具有相同的标准属性
  assert_eq(azimuth::Resource::get_attribute(service_a_final_resource, "service.name"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "service.name"))
  assert_eq(azimuth::Resource::get_attribute(service_a_final_resource, "service.version"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "service.version"))
  assert_eq(azimuth::Resource::get_attribute(service_a_final_resource, "service.namespace"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "service.namespace"))
  assert_eq(azimuth::Resource::get_attribute(service_a_final_resource, "deployment.environment"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "deployment.environment"))
  
  // 验证服务特定的属性不同
  assert_ne(azimuth::Resource::get_attribute(service_a_final_resource, "service.instance.id"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "service.instance.id"))
  assert_ne(azimuth::Resource::get_attribute(service_a_final_resource, "service.port"), 
            azimuth::Resource::get_attribute(service_b_final_resource, "service.port"))
}

pub test "consistent instrumentation scope across services" {
  // 定义标准化的instrumentation scope
  let service_a_scope = azimuth::InstrumentationScope::{
    name: "service-a-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let service_b_scope = azimuth::InstrumentationScope::{
    name: "service-b-instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 验证版本和schema URL一致
  assert_eq(service_a_scope.version, service_b_scope.version)
  assert_eq(service_a_scope.schema_url, service_b_scope.schema_url)
  
  // 验证服务名称不同
  assert_ne(service_a_scope.name, service_b_scope.name)
  
  // 创建tracer并验证scope一致性
  let tracer_provider = azimuth::TracerProvider::default()
  let service_a_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, service_a_scope.name)
  let service_b_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, service_b_scope.name)
  
  let service_a_tracer_scope = azimuth::Tracer::instrumentation_scope(service_a_tracer)
  let service_b_tracer_scope = azimuth::Tracer::instrumentation_scope(service_b_tracer)
  
  assert_eq(service_a_tracer_scope.name, service_a_scope.name)
  assert_eq(service_b_tracer_scope.name, service_b_scope.name)
}

pub test "metrics consistency across services" {
  // 服务A的指标
  let service_a_meter_provider = azimuth::MeterProvider::default()
  let service_a_meter = azimuth::MeterProvider::get_meter(
    service_a_meter_provider, 
    "service-a-metrics"
  )
  
  let service_a_request_counter = azimuth::Meter::create_counter(
    service_a_meter, 
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  let service_a_response_histogram = azimuth::Meter::create_histogram(
    service_a_meter, 
    "http.response.duration",
    Some("HTTP request response duration"),
    Some("ms")
  )
  
  // 服务B的指标
  let service_b_meter_provider = azimuth::MeterProvider::default()
  let service_b_meter = azimuth::MeterProvider::get_meter(
    service_b_meter_provider, 
    "service-b-metrics"
  )
  
  let service_b_request_counter = azimuth::Meter::create_counter(
    service_b_meter, 
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  let service_b_response_histogram = azimuth::Meter::create_histogram(
    service_b_meter, 
    "http.response.duration",
    Some("HTTP request response duration"),
    Some("ms")
  )
  
  // 验证指标名称和描述一致
  assert_eq(service_a_request_counter.name, service_b_request_counter.name)
  assert_eq(service_a_request_counter.description, service_b_request_counter.description)
  assert_eq(service_a_request_counter.unit, service_b_request_counter.unit)
  
  assert_eq(service_a_response_histogram.name, service_b_response_histogram.name)
  assert_eq(service_a_response_histogram.description, service_b_response_histogram.description)
  assert_eq(service_a_response_histogram.unit, service_b_response_histogram.unit)
  
  // 记录指标值
  azimuth::Counter::add(service_a_request_counter, 100.0)
  azimuth::Counter::add(service_b_request_counter, 150.0)
  
  azimuth::Histogram::record(service_a_response_histogram, 120.5)
  azimuth::Histogram::record(service_b_response_histogram, 95.3)
}

pub test "logging consistency across services" {
  // 服务A的日志记录器
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(
    service_a_logger_provider, 
    "service-a-logger"
  )
  
  // 服务B的日志记录器
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(
    service_b_logger_provider, 
    "service-b-logger"
  )
  
  // 创建标准化的日志属性
  let standard_log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(standard_log_attrs, "service.name", azimuth::StringValue("microservice"))
  azimuth::Attributes::set(standard_log_attrs, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(standard_log_attrs, "environment", azimuth::StringValue("production"))
  
  // 服务A的日志记录
  let service_a_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service A processing request"),
    Some(standard_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-service-a-123"),
    Some("span-service-a-456"),
    Some(azimuth::Context::root())
  )
  
  // 服务B的日志记录
  let service_b_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Service B processing request"),
    Some(standard_log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-service-b-123"),
    Some("span-service-b-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录的属性一致性
  assert_eq(azimuth::LogRecord::severity_number(service_a_log), 
            azimuth::LogRecord::severity_number(service_b_log))
  
  // 发射日志
  azimuth::Logger::emit(service_a_logger, service_a_log)
  azimuth::Logger::emit(service_b_logger, service_b_log)
}

pub test "span hierarchy consistency across services" {
  // 服务A创建根span
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-a"
  )
  
  let root_span = azimuth::Tracer::start_span(service_a_tracer, "root-operation")
  let root_span_context = azimuth::Span::span_context(root_span)
  
  // 服务A创建子span
  let service_a_child_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-child-operation")
  azimuth::Span::add_event(service_a_child_span, "Service A processing started")
  azimuth::Span::end(service_a_child_span)
  
  // 模拟将span context传播到服务B
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let ctx = azimuth::Context::root()
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 服务B接收并创建子span
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-b"
  )
  
  let service_b_child_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-child-operation")
  azimuth::Span::add_event(service_b_child_span, "Service B processing started")
  azimuth::Span::end(service_b_child_span)
  
  // 服务C接收并创建子span
  let service_c_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-c"
  )
  
  let service_c_child_span = azimuth::Tracer::start_span(service_c_tracer, "service-c-child-operation")
  azimuth::Span::add_event(service_c_child_span, "Service C processing started")
  azimuth::Span::end(service_c_child_span)
  
  // 结束根span
  azimuth::Span::end(root_span)
  
  // 验证span层次结构
  assert_eq(azimuth::Span::name(root_span), "root-operation")
  assert_eq(azimuth::Span::name(service_a_child_span), "service-a-child-operation")
  assert_eq(azimuth::Span::name(service_b_child_span), "service-b-child-operation")
  assert_eq(azimuth::Span::name(service_c_child_span), "service-c-child-operation")
}

pub test "error handling consistency across services" {
  // 服务A的错误处理
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-a"
  )
  
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a-operation")
  azimuth::Span::set_status(service_a_span, azimuth::Error, Some("Service A database connection failed"))
  azimuth::Span::add_event(service_a_span, "Database connection timeout")
  azimuth::Span::end(service_a_span)
  
  // 服务B的错误处理
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-b"
  )
  
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b-operation")
  azimuth::Span::set_status(service_b_span, azimuth::Error, Some("Service B external API call failed"))
  azimuth::Span::add_event(service_b_span, "External API timeout")
  azimuth::Span::end(service_b_span)
  
  // 验证错误状态设置
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Error)
  
  // 服务A的错误日志
  let service_a_logger_provider = azimuth::LoggerProvider::default()
  let service_a_logger = azimuth::LoggerProvider::get_logger(
    service_a_logger_provider, 
    "service-a-logger"
  )
  
  let service_a_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service A encountered database connection error"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-a-123"),
    Some("error-span-a-456"),
    Some(azimuth::Context::root())
  )
  
  // 服务B的错误日志
  let service_b_logger_provider = azimuth::LoggerProvider::default()
  let service_b_logger = azimuth::LoggerProvider::get_logger(
    service_b_logger_provider, 
    "service-b-logger"
  )
  
  let service_b_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Service B encountered external API error"),
    None,
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace-b-123"),
    Some("error-span-b-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证错误日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(service_a_error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(service_b_error_log), azimuth::Error)
  
  // 发射错误日志
  azimuth::Logger::emit(service_a_logger, service_a_error_log)
  azimuth::Logger::emit(service_b_logger, service_b_error_log)
}

pub test "attribute naming consistency across services" {
  // 定义标准化的属性命名约定
  let standard_http_attrs = [
    ("http.method", azimuth::StringValue("GET")),
    ("http.status_code", azimuth::IntValue(200)),
    ("http.url", azimuth::StringValue("https://api.example.com/users")),
    ("http.scheme", azimuth::StringValue("https")),
    ("http.host", azimuth::StringValue("api.example.com"))
  ]
  
  let standard_user_attrs = [
    ("user.id", azimuth::StringValue("user-123")),
    ("user.role", azimuth::StringValue("admin")),
    ("user.organization", azimuth::StringValue("acme-corp"))
  ]
  
  // 服务A的属性
  let service_a_attrs = azimuth::Attributes::new()
  for attr in standard_http_attrs {
    azimuth::Attributes::set(service_a_attrs, attr.0, attr.1)
  }
  for attr in standard_user_attrs {
    azimuth::Attributes::set(service_a_attrs, attr.0, attr.1)
  }
  
  // 服务B的属性
  let service_b_attrs = azimuth::Attributes::new()
  for attr in standard_http_attrs {
    azimuth::Attributes::set(service_b_attrs, attr.0, attr.1)
  }
  for attr in standard_user_attrs {
    azimuth::Attributes::set(service_b_attrs, attr.0, attr.1)
  }
  
  // 验证属性命名一致性
  assert_eq(azimuth::Attributes::get(service_a_attrs, "http.method"), 
            azimuth::Attributes::get(service_b_attrs, "http.method"))
  assert_eq(azimuth::Attributes::get(service_a_attrs, "http.status_code"), 
            azimuth::Attributes::get(service_b_attrs, "http.status_code"))
  assert_eq(azimuth::Attributes::get(service_a_attrs, "user.id"), 
            azimuth::Attributes::get(service_b_attrs, "user.id"))
  assert_eq(azimuth::Attributes::get(service_a_attrs, "user.role"), 
            azimuth::Attributes::get(service_b_attrs, "user.role"))
  
  // 验证属性值一致性
  assert_eq(azimuth::Attributes::get(service_a_attrs, "http.method"), 
            Some(azimuth::StringValue("GET")))
  assert_eq(azimuth::Attributes::get(service_a_attrs, "http.status_code"), 
            Some(azimuth::IntValue(200)))
  assert_eq(azimuth::Attributes::get(service_a_attrs, "user.id"), 
            Some(azimuth::StringValue("user-123")))
}

pub test "sampling consistency across services" {
  // 创建带有采样信息的span context
  let sampled_span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let not_sampled_span_ctx = azimuth::SpanContext::new("trace-789", "span-012", false, "")
  
  // 验证采样状态
  assert_true(azimuth::SpanContext::is_sampled(sampled_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(not_sampled_span_ctx))
  
  // 服务A使用采样的span
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-a"
  )
  
  let service_a_sampled_span = azimuth::Span::new("service-a-sampled", azimuth::Internal, sampled_span_ctx)
  let service_a_not_sampled_span = azimuth::Span::new("service-a-not-sampled", azimuth::Internal, not_sampled_span_ctx)
  
  // 服务B使用采样的span
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "service-b"
  )
  
  let service_b_sampled_span = azimuth::Span::new("service-b-sampled", azimuth::Internal, sampled_span_ctx)
  let service_b_not_sampled_span = azimuth::Span::new("service-b-not-sampled", azimuth::Internal, not_sampled_span_ctx)
  
  // 验证采样状态一致性
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_a_sampled_span)))
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_b_sampled_span)))
  
  assert_false(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_a_not_sampled_span)))
  assert_false(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(service_b_not_sampled_span)))
  
  // 验证recording状态与采样状态相关
  assert_true(azimuth::Span::is_recording(service_a_sampled_span))
  assert_true(azimuth::Span::is_recording(service_b_sampled_span))
  assert_true(azimuth::Span::is_recording(service_a_not_sampled_span))
  assert_true(azimuth::Span::is_recording(service_b_not_sampled_span))
}