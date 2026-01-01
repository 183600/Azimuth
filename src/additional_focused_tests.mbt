// Additional Focused MoonBit Test Suite for Azimuth Telemetry System
// This file contains focused test cases covering specific functionality areas

// Test 1: Advanced Span Attributes Operations
pub test "高级Span属性操作测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "advanced-attr-test")
  let span = azimuth::Tracer::start_span(tracer, "advanced-span-operations")
  
  // 测试批量属性设置
  let batch_attrs = [
    ("user.id", azimuth::StringValue("user-12345")),
    ("request.id", azimuth::StringValue("req-67890")),
    ("operation.type", azimuth::StringValue("database.query")),
    ("db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.type", azimuth::StringValue("postgresql")),
    ("db.instance", azimuth::StringValue("prod-db-01")),
    ("http.method", azimuth::StringValue("GET")),
    ("http.status_code", azimuth::IntValue(200)),
    ("http.url", azimuth::StringValue("/api/v1/users/12345")),
    ("response.size", azimuth::IntValue(1024)),
    ("duration.ms", azimuth::IntValue(150))
  ]
  
  // 批量设置属性
  for attr in batch_attrs {
    azimuth::Span::set_attribute(span, attr.0, attr.1)
  }
  
  // 验证属性设置
  let user_id = azimuth::Span::get_attribute(span, "user.id")
  let request_id = azimuth::Span::get_attribute(span, "request.id")
  let operation_type = azimuth::Span::get_attribute(span, "operation.type")
  let http_status = azimuth::Span::get_attribute(span, "http.status_code")
  
  assert_eq(user_id, Some(azimuth::StringValue("user-12345")))
  assert_eq(request_id, Some(azimuth::StringValue("req-67890")))
  assert_eq(operation_type, Some(azimuth::StringValue("database.query")))
  assert_eq(http_status, Some(azimuth::IntValue(200)))
  
  // 测试属性更新
  azimuth::Span::set_attribute(span, "http.status_code", azimuth::IntValue(500))
  let updated_status = azimuth::Span::get_attribute(span, "http.status_code")
  assert_eq(updated_status, Some(azimuth::IntValue(500)))
  
  azimuth::Span::end(span)
}

// Test 2: Metrics Aggregation Operations
pub test "度量聚合操作测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建各种度量类型
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active_connections")
  let queue_size = azimuth::Meter::create_gauge(meter, "http.queue.size", Some("Current queue size"), Some("items"))
  
  // 模拟度量数据收集
  for i in 0..100 {
    // 记录请求计数
    azimuth::Counter::add(request_counter, 1.0)
    
    // 记录响应时间（模拟不同范围的响应时间）
    let response_time = match i % 4 {
      0 => 50.0,
      1 => 100.0,
      2 => 200.0,
      _ => 500.0
    }
    azimuth::Histogram::record(response_histogram, response_time)
    
    // 模拟活跃连接数变化
    if i % 10 == 0 {
      azimuth::UpDownCounter::add(active_connections, 5.0)
    } else if i % 15 == 0 {
      azimuth::UpDownCounter::add(active_connections, -3.0)
    }
  }
  
  // 验证度量创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP response duration"))
  assert_eq(response_histogram.unit, Some("ms"))
  assert_eq(active_connections.name, "http.active_connections")
  assert_eq(queue_size.name, "http.queue.size")
  assert_eq(queue_size.description, Some("Current queue size"))
  assert_eq(queue_size.unit, Some("items"))
  
  // 测试度量仪器转换
  let counter_instrument = azimuth::Histogram::as_instrument(response_histogram)
  assert_eq(azimuth::Instrument::name(counter_instrument), "http.response.duration")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("HTTP response duration"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("ms"))
}

// Test 3: Context Propagation Complex Scenarios
pub test "复杂上下文传播场景测试" {
  // 创建根上下文
  let root_ctx = azimuth::Context::root()
  
  // 设置多层上下文键值对
  let trace_id_key = azimuth::ContextKey::new("trace.id")
  let user_id_key = azimuth::ContextKey::new("user.id")
  let session_id_key = azimuth::ContextKey::new("session.id")
  let request_id_key = azimuth::ContextKey::new("request.id")
  let correlation_id_key = azimuth::ContextKey::new("correlation.id")
  
  // 逐层构建上下文
  let ctx_with_trace = azimuth::Context::with_value(root_ctx, trace_id_key, "trace-abc123")
  let ctx_with_user = azimuth::Context::with_value(ctx_with_trace, user_id_key, "user-456789")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_user, session_id_key, "session-def789")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_session, request_id_key, "req-ghi012")
  let final_ctx = azimuth::Context::with_value(ctx_with_request, correlation_id_key, "corr-jkl345")
  
  // 验证所有上下文值都正确传播
  let trace_id = azimuth::Context::get(final_ctx, trace_id_key)
  let user_id = azimuth::Context::get(final_ctx, user_id_key)
  let session_id = azimuth::Context::get(final_ctx, session_id_key)
  let request_id = azimuth::Context::get(final_ctx, request_id_key)
  let correlation_id = azimuth::Context::get(final_ctx, correlation_id_key)
  
  assert_eq(trace_id, Some("trace-abc123"))
  assert_eq(user_id, Some("user-456789"))
  assert_eq(session_id, Some("session-def789"))
  assert_eq(request_id, Some("req-ghi012"))
  assert_eq(correlation_id, Some("corr-jkl345"))
  
  // 测试Baggage的复杂传播
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-456789")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_user, "tenant.id", "tenant-001")
  let baggage_with_region = azimuth::Baggage::set_entry(baggage_with_tenant, "region", "us-west-2")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_region, "environment", "production")
  
  // 验证Baggage中的所有条目
  let user_baggage = azimuth::Baggage::get_entry(final_baggage, "user.id")
  let tenant_baggage = azimuth::Baggage::get_entry(final_baggage, "tenant.id")
  let region_baggage = azimuth::Baggage::get_entry(final_baggage, "region")
  let env_baggage = azimuth::Baggage::get_entry(final_baggage, "environment")
  
  assert_eq(user_baggage, Some("user-456789"))
  assert_eq(tenant_baggage, Some("tenant-001"))
  assert_eq(region_baggage, Some("us-west-2"))
  assert_eq(env_baggage, Some("production"))
}

// Test 4: Log Record Advanced Features
pub test "日志记录高级功能测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "advanced-logger-test")
  
  // 创建带有丰富属性的日志记录
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "event.category", azimuth::StringValue("security"))
  azimuth::Attributes::set(log_attrs, "event.type", azimuth::StringValue("authentication"))
  azimuth::Attributes::set(log_attrs, "user.id", azimuth::StringValue("user-123"))
  azimuth::Attributes::set(log_attrs, "source.ip", azimuth::StringValue("192.168.1.100"))
  azimuth::Attributes::set(log_attrs, "user.agent", azimuth::StringValue("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
  azimuth::Attributes::set(log_attrs, "authentication.method", azimuth::StringValue("oauth2"))
  azimuth::Attributes::set(log_attrs, "authentication.result", azimuth::StringValue("success"))
  azimuth::Attributes::set(log_attrs, "session.duration.ms", azimuth::IntValue(2500))
  
  // 创建不同严重程度的日志记录
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User authentication successful"),
    Some(log_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("auth-trace-123"),
    Some("auth-span-456"),
    Some(azimuth::Context::root())
  )
  
  let warning_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Authentication token will expire soon"),
    Some(log_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("auth-trace-123"),
    Some("auth-span-457"),
    Some(azimuth::Context::root())
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Authentication failed due to invalid credentials"),
    Some(log_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("auth-trace-124"),
    Some("auth-span-458"),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warning_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  
  assert_eq(azimuth::LogRecord::body(info_log), Some("User authentication successful"))
  assert_eq(azimuth::LogRecord::body(warning_log), Some("Authentication token will expire soon"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Authentication failed due to invalid credentials"))
  
  // 发出日志记录
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warning_log)
  azimuth::Logger::emit(logger, error_log)
}

// Test 5: Resource Merge Strategy Advanced
pub test "资源合并策略高级测试" {
  // 创建多个资源实例
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("azimuth-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("production"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  let env_resource = azimuth::Resource::new()
  let env_attrs = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("deployment.region", azimuth::StringValue("us-west-2")),
    ("deployment.zone", azimuth::StringValue("us-west-2a"))
  ]
  let env_resource_with_attrs = azimuth::Resource::with_attributes(env_resource, env_attrs)
  
  let host_resource = azimuth::Resource::new()
  let host_attrs = [
    ("host.name", azimuth::StringValue("prod-host-01")),
    ("host.type", azimuth::StringValue("ec2.instance")),
    ("host.image.id", azimuth::StringValue("ami-1234567890abcdef0"))
  ]
  let host_resource_with_attrs = azimuth::Resource::with_attributes(host_resource, host_attrs)
  
  let process_resource = azimuth::Resource::new()
  let process_attrs = [
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("azimuth-service")),
    ("process.executable.path", azimuth::StringValue("/opt/azimuth/bin/azimuth-service")),
    ("process.command_args", azimuth::ArrayStringValue(["--config", "/etc/azimuth/config.yaml", "--log-level", "info"]))
  ]
  let process_resource_with_attrs = azimuth::Resource::with_attributes(process_resource, process_attrs)
  
  // 测试多级资源合并
  let merged_step1 = azimuth::Resource::merge(base_resource_with_attrs, env_resource_with_attrs)
  let merged_step2 = azimuth::Resource::merge(merged_step1, host_resource_with_attrs)
  let final_resource = azimuth::Resource::merge(merged_step2, process_resource_with_attrs)
  
  // 验证合并后的资源包含所有属性
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), Some(azimuth::StringValue("azimuth-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.namespace"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "deployment.zone"), Some(azimuth::StringValue("us-west-2a")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "host.name"), Some(azimuth::StringValue("prod-host-01")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "host.type"), Some(azimuth::StringValue("ec2.instance")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "host.image.id"), Some(azimuth::StringValue("ami-1234567890abcdef0")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "process.pid"), Some(azimuth::IntValue(12345)))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "process.executable.name"), Some(azimuth::StringValue("azimuth-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "process.executable.path"), Some(azimuth::StringValue("/opt/azimuth/bin/azimuth-service")))
}

// Test 6: Propagator Complex Injection and Extraction
pub test "传播器复杂注入和提取测试" {
  // 创建复合传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let custom_propagator = azimuth::CustomPropagator::new("custom-header")
  
  let propagators = [trace_propagator, baggage_propagator, custom_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建带有丰富上下文的上下文
  let ctx = azimuth::Context::root()
  let trace_key = azimuth::ContextKey::new("trace.id")
  let span_key = azimuth::ContextKey::new("span.id")
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  
  let enriched_ctx = azimuth::Context::with_value(ctx, trace_key, "trace-987654321")
  let enriched_ctx2 = azimuth::Context::with_value(enriched_ctx, span_key, "span-123456789")
  let enriched_ctx3 = azimuth::Context::with_value(enriched_ctx2, user_key, "user-987654321")
  let final_ctx = azimuth::Context::with_value(enriched_ctx3, session_key, "session-abcdef123456")
  
  // 创建Baggage
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-987654321")
  let baggage_with_tenant = azimuth::Baggage::set_entry(baggage_with_user, "tenant.id", "tenant-456789")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_tenant, "request.source", "web.frontend")
  
  // 注入到载体
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, final_ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  let custom_header = azimuth::TextMapCarrier::get(carrier, "custom-header")
  
  assert_eq(trace_header, Some("00-trace-987654321-span-123456789-01"))
  assert_eq(baggage_header, Some("user.id=user-987654321,tenant.id=tenant-456789,request.source=web.frontend"))
  assert_eq(custom_header, Some("trace-987654321"))
  
  // 从载体提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文
  let extracted_trace = azimuth::Context::get(extracted_ctx, trace_key)
  let extracted_span = azimuth::Context::get(extracted_ctx, span_key)
  let extracted_user = azimuth::Context::get(extracted_ctx, user_key)
  let extracted_session = azimuth::Context::get(extracted_ctx, session_key)
  
  assert_eq(extracted_trace, Some("trace-987654321"))
  assert_eq(extracted_span, Some("span-123456789"))
  assert_eq(extracted_user, Some("user-987654321"))
  assert_eq(extracted_session, Some("session-abcdef123456"))
}

// Test 7: Span Event and Link Operations
pub test "Span事件和链接操作测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "span-events-test")
  let span = azimuth::Tracer::start_span(tracer, "operation-with-events")
  
  // 添加事件
  let event_attrs1 = azimuth::Attributes::new()
  azimuth::Attributes::set(event_attrs1, "event.type", azimuth::StringValue("database.query.start"))
  azimuth::Attributes::set(event_attrs1, "db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = $1"))
  azimuth::Attributes::set(event_attrs1, "db.type", azimuth::StringValue("postgresql"))
  
  azimuth::Span::add_event(span, "Database query started", Some(event_attrs1))
  
  // 添加更多事件
  let event_attrs2 = azimuth::Attributes::new()
  azimuth::Attributes::set(event_attrs2, "event.type", azimuth::StringValue("database.query.end"))
  azimuth::Attributes::set(event_attrs2, "db.rows_affected", azimuth::IntValue(1))
  azimuth::Attributes::set(event_attrs2, "db.duration.ms", azimuth::IntValue(45))
  
  azimuth::Span::add_event(span, "Database query completed", Some(event_attrs2))
  
  // 添加错误事件
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("ConnectionTimeout"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Connection to database timed out after 30 seconds"))
  azimuth::Attributes::set(error_attrs, "error.retry_count", azimuth::IntValue(3))
  
  azimuth::Span::add_event(span, "Database connection error", Some(error_attrs))
  
  // 创建链接到其他Span
  let linked_span_ctx1 = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  let linked_span_ctx2 = azimuth::SpanContext::new("trace-789", "span-012", true, "")
  
  let link_attrs1 = azimuth::Attributes::new()
  azimuth::Attributes::set(link_attrs1, "link.type", azimuth::StringValue("parent"))
  azimuth::Attributes::set(link_attrs1, "service.name", azimuth::StringValue("auth-service"))
  
  let link_attrs2 = azimuth::Attributes::new()
  azimuth::Attributes::set(link_attrs2, "link.type", azimuth::StringValue("preceding"))
  azimuth::Attributes::set(link_attrs2, "service.name", azimuth::StringValue("cache-service"))
  
  azimuth::Span::add_link(span, linked_span_ctx1, Some(link_attrs1))
  azimuth::Span::add_link(span, linked_span_ctx2, Some(link_attrs2))
  
  // 设置Span状态
  azimuth::Span::set_status(span, azimuth::Ok, "Operation completed successfully")
  
  // 验证Span属性
  assert_eq(azimuth::Span::name(span), "operation-with-events")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  azimuth::Span::end(span)
}

// Test 8: Metrics Bucket and Histogram Advanced Operations
pub test "度量桶和直方图高级操作测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "histogram-advanced-test")
  
  // 创建带有自定义桶边界的直方图
  let response_histogram = azimuth::Meter::create_histogram_with_buckets(
    meter, 
    "http.response.duration.advanced", 
    Some("Advanced HTTP response duration histogram"), 
    Some("ms"),
    [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0, 2500.0, 5000.0]
  )
  
  // 模拟各种响应时间
  let response_times = [
    5.0, 15.0, 30.0, 45.0, 75.0, 120.0, 200.0, 350.0, 600.0, 1200.0,
    8.0, 22.0, 40.0, 65.0, 95.0, 180.0, 300.0, 450.0, 800.0, 1500.0,
    12.0, 28.0, 55.0, 85.0, 150.0, 250.0, 400.0, 700.0, 1100.0, 2000.0
  ]
  
  // 记录响应时间
  for response_time in response_times {
    azimuth::Histogram::record(response_histogram, response_time)
  }
  
  // 验证直方图属性
  assert_eq(response_histogram.name, "http.response.duration.advanced")
  assert_eq(response_histogram.description, Some("Advanced HTTP response duration histogram"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  // 创建另一个直方图用于比较
  let size_histogram = azimuth::Meter::create_histogram(
    meter, 
    "http.response.size", 
    Some("HTTP response size in bytes"), 
    Some("bytes")
  )
  
  // 记录响应大小
  let response_sizes = [1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288]
  for size in response_sizes {
    azimuth::Histogram::record(size_histogram, size.to_double())
  }
  
  // 验证大小直方图
  assert_eq(size_histogram.name, "http.response.size")
  assert_eq(size_histogram.description, Some("HTTP response size in bytes"))
  assert_eq(size_histogram.unit, Some("bytes"))
  
  // 测试度量仪器转换
  let histogram_instrument = azimuth::Histogram::as_instrument(response_histogram)
  assert_eq(azimuth::Instrument::name(histogram_instrument), "http.response.duration.advanced")
  assert_eq(azimuth::Instrument::description(histogram_instrument), Some("Advanced HTTP response duration histogram"))
  assert_eq(azimuth::Instrument::unit(histogram_instrument), Some("ms"))
}

// Test 9: Instrumentation Scope Configuration
pub test "仪表作用域配置测试" {
  // 创建带有不同配置的Tracer
  let tracer_provider = azimuth::TracerProvider::default()
  
  let tracer1 = azimuth::TracerProvider::get_tracer(tracer_provider, "service-a")
  let tracer2 = azimuth::TracerProvider::get_tracer(tracer_provider, "service-b", Some("2.0.0"))
  let tracer3 = azimuth::TracerProvider::get_tracer_with_schema(
    tracer_provider, 
    "service-c", 
    Some("3.1.0"), 
    Some("https://opentelemetry.io/schemas/1.20.0")
  )
  
  // 验证仪表作用域配置
  let scope1 = azimuth::Tracer::instrumentation_scope(tracer1)
  let scope2 = azimuth::Tracer::instrumentation_scope(tracer2)
  let scope3 = azimuth::Tracer::instrumentation_scope(tracer3)
  
  assert_eq(scope1.name, "service-a")
  assert_eq(scope1.version, None)
  assert_eq(scope1.schema_url, None)
  
  assert_eq(scope2.name, "service-b")
  assert_eq(scope2.version, Some("2.0.0"))
  assert_eq(scope2.schema_url, None)
  
  assert_eq(scope3.name, "service-c")
  assert_eq(scope3.version, Some("3.1.0"))
  assert_eq(scope3.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 创建带有不同配置的Meter
  let meter_provider = azimuth::MeterProvider::default()
  
  let meter1 = azimuth::MeterProvider::get_meter(meter_provider, "metrics-a")
  let meter2 = azimuth::MeterProvider::get_meter(meter_provider, "metrics-b", Some("1.5.0"))
  let meter3 = azimuth::MeterProvider::get_meter_with_schema(
    meter_provider, 
    "metrics-c", 
    Some("2.2.0"), 
    Some("https://opentelemetry.io/schemas/1.20.0")
  )
  
  // 验证Meter的仪表作用域配置
  let meter_scope1 = azimuth::Meter::instrumentation_scope(meter1)
  let meter_scope2 = azimuth::Meter::instrumentation_scope(meter2)
  let meter_scope3 = azimuth::Meter::instrumentation_scope(meter3)
  
  assert_eq(meter_scope1.name, "metrics-a")
  assert_eq(meter_scope1.version, None)
  assert_eq(meter_scope1.schema_url, None)
  
  assert_eq(meter_scope2.name, "metrics-b")
  assert_eq(meter_scope2.version, Some("1.5.0"))
  assert_eq(meter_scope2.schema_url, None)
  
  assert_eq(meter_scope3.name, "metrics-c")
  assert_eq(meter_scope3.version, Some("2.2.0"))
  assert_eq(meter_scope3.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 使用不同配置的Tracer创建Span
  let span1 = azimuth::Tracer::start_span(tracer1, "operation-a")
  let span2 = azimuth::Tracer::start_span(tracer2, "operation-b")
  let span3 = azimuth::Tracer::start_span(tracer3, "operation-c")
  
  // 验证Span与正确的Tracer关联
  assert_eq(azimuth::Span::name(span1), "operation-a")
  assert_eq(azimuth::Span::name(span2), "operation-b")
  assert_eq(azimuth::Span::name(span3), "operation-c")
  
  azimuth::Span::end(span1)
  azimuth::Span::end(span2)
  azimuth::Span::end(span3)
}