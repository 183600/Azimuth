// 分布式追踪端到端一致性测试
// 测试完整的分布式请求链路中的遥测数据一致性

test "distributed request tracing consistency" {
  // 模拟客户端发起请求到API网关
  let gateway_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "api-gateway"
  )
  
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway-process-request")
  let gateway_span_context = azimuth::Span::span_context(gateway_span)
  
  // API网关添加事件和属性
  azimuth::Span::add_event(gateway_span, "Request received", Some([
    ("http.method", azimuth::StringValue("GET")),
    ("http.url", azimuth::StringValue("/api/v1/users")),
    ("user.agent", azimuth::StringValue("Mozilla/5.0"))
  ]))
  
  // 验证网关span状态
  assert_true(azimuth::SpanContext::is_valid(gateway_span_context))
  assert_eq(azimuth::Span::name(gateway_span), "gateway-process-request")
  
  // 模拟传播到认证服务
  let auth_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "auth-service"
  )
  
  let auth_span = azimuth::Tracer::start_span(auth_tracer, "authenticate-user")
  azimuth::Span::add_event(auth_span, "Authentication started")
  azimuth::Span::add_event(auth_span, "Token validated")
  azimuth::Span::add_event(auth_span, "User authenticated", Some([
    ("user.id", azimuth::StringValue("user-12345")),
    ("user.role", azimuth::StringValue("premium"))
  ]))
  azimuth::Span::end(auth_span)
  
  // 模拟传播到业务服务
  let business_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "business-service"
  )
  
  let business_span = azimuth::Tracer::start_span(business_tracer, "process-business-logic")
  azimuth::Span::add_event(business_span, "Business logic started")
  
  // 模拟传播到数据库服务
  let db_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "database-service"
  )
  
  let db_span = azimuth::Tracer::start_span(db_tracer, "database-query")
  azimuth::Span::add_event(db_span, "Query executed", Some([
    ("db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.type", azimuth::StringValue("postgresql")),
    ("db.connection.pool", azimuth::StringValue("primary"))
  ]))
  azimuth::Span::end(db_span)
  
  // 业务服务继续处理
  azimuth::Span::add_event(business_span, "Data processed")
  azimuth::Span::add_event(business_span, "Response prepared")
  azimuth::Span::end(business_span)
  
  // API网关完成处理
  azimuth::Span::add_event(gateway_span, "Response sent", Some([
    ("http.status_code", azimuth::IntValue(200)),
    ("response.size", azimuth::IntValue(1024))
  ]))
  azimuth::Span::set_status(gateway_span, azimuth::Ok, None)
  azimuth::Span::end(gateway_span)
  
  // 验证所有span的层次关系和一致性
  assert_eq(azimuth::Span::status(gateway_span), azimuth::Ok)
  assert_true(azimuth::SpanContext::is_valid(azimuth::Span::span_context(auth_span)))
  assert_true(azimuth::SpanContext::is_valid(azimuth::Span::span_context(business_span)))
  assert_true(azimuth::SpanContext::is_valid(azimuth::Span::span_context(db_span)))
}

test "cross-service metrics correlation" {
  // 定义标准化的指标名称和属性
  let standard_metric_attrs = [
    ("service.operation", azimuth::StringValue("user-profile")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("environment", azimuth::StringValue("production"))
  ]
  
  // 服务A：用户服务指标
  let user_service_meter = azimuth::MeterProvider::get_meter(
    azimuth::MeterProvider::default(), 
    "user-service"
  )
  
  let user_request_counter = azimuth::Meter::create_counter(
    user_service_meter, 
    "service.requests.total",
    Some("Total number of service requests"),
    Some("requests")
  )
  
  let user_duration_histogram = azimuth::Meter::create_histogram(
    user_service_meter, 
    "service.request.duration",
    Some("Service request duration"),
    Some("ms")
  )
  
  // 记录用户服务指标
  azimuth::Counter::add(user_request_counter, 1.0)
  azimuth::Histogram::record(user_duration_histogram, 45.2)
  
  // 服务B：认证服务指标
  let auth_service_meter = azimuth::MeterProvider::get_meter(
    azimuth::MeterProvider::default(), 
    "auth-service"
  )
  
  let auth_request_counter = azimuth::Meter::create_counter(
    auth_service_meter, 
    "service.requests.total",
    Some("Total number of service requests"),
    Some("requests")
  )
  
  let auth_duration_histogram = azimuth::Meter::create_histogram(
    auth_service_meter, 
    "service.request.duration",
    Some("Service request duration"),
    Some("ms")
  )
  
  // 记录认证服务指标
  azimuth::Counter::add(auth_request_counter, 1.0)
  azimuth::Histogram::record(auth_duration_histogram, 12.8)
  
  // 服务C：缓存服务指标
  let cache_service_meter = azimuth::MeterProvider::get_meter(
    azimuth::MeterProvider::default(), 
    "cache-service"
  )
  
  let cache_hit_counter = azimuth::Meter::create_counter(
    cache_service_meter, 
    "cache.hits.total",
    Some("Total number of cache hits"),
    Some("hits")
  )
  
  let cache_miss_counter = azimuth::Meter::create_counter(
    cache_service_meter, 
    "cache.misses.total",
    Some("Total number of cache misses"),
    Some("misses")
  )
  
  // 记录缓存服务指标
  azimuth::Counter::add(cache_hit_counter, 3.0)
  azimuth::Counter::add(cache_miss_counter, 1.0)
  
  // 验证指标名称和描述的一致性
  assert_eq(user_request_counter.name, auth_request_counter.name)
  assert_eq(user_request_counter.description, auth_request_counter.description)
  assert_eq(user_request_counter.unit, auth_request_counter.unit)
  
  assert_eq(user_duration_histogram.name, auth_duration_histogram.name)
  assert_eq(user_duration_histogram.description, auth_duration_histogram.description)
  assert_eq(user_duration_histogram.unit, auth_duration_histogram.unit)
  
  // 验证缓存指标的独特性
  assert_ne(cache_hit_counter.name, cache_miss_counter.name)
  assert_eq(cache_hit_counter.unit, "hits")
  assert_eq(cache_miss_counter.unit, "misses")
}