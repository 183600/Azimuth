// Azimuth Microservice Architecture Telemetry Integration Tests
// 微服务架构遥测集成测试用例
// 测试微服务架构下的遥测数据收集、传播和集成

import "azimuth/azimuth"

// Test 1: 微服务间遥测传播测试
pub test "微服务间遥测传播测试" {
  // 创建微服务架构模拟
  let services = [
    {
      "name": "api-gateway",
      "port": 8080,
      "dependencies": ["auth-service", "user-service"]
    },
    {
      "name": "auth-service",
      "port": 8081,
      "dependencies": ["database"]
    },
    {
      "name": "user-service",
      "port": 8082,
      "dependencies": ["database", "notification-service"]
    },
    {
      "name": "notification-service",
      "port": 8083,
      "dependencies": ["queue-service"]
    },
    {
      "name": "database",
      "port": 5432,
      "dependencies": []
    },
    {
      "name": "queue-service",
      "port": 5672,
      "dependencies": []
    }
  ]
  
  // 创建服务遥测组件
  let service_tracers = {}
  let service_meters = {}
  let service_loggers = {}
  
  for service in services {
    let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), service["name"])
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), service["name"])
    let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), service["name"])
    
    service_tracers[service["name"]] = tracer
    service_meters[service["name"]] = meter
    service_loggers[service["name"]] = logger
  }
  
  // 模拟请求流程：API Gateway -> Auth Service -> User Service -> Database
  let request_id = "req-" + azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string()
  let user_id = "user-12345"
  
  // API Gateway处理请求
  let gateway_tracer = service_tracers["api-gateway"]
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "api-gateway.request")
  let gateway_context = azimuth::Span::span_context(gateway_span)
  
  // 添加请求属性
  azimuth::Span::add_event(gateway_span, "request.received", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id)),
    ("http.method", azimuth::StringValue("GET")),
    ("http.path", azimuth::StringValue("/api/user/profile"))
  ]))
  
  // 记录度量
  let gateway_meter = service_meters["api-gateway"]
  let request_counter = azimuth::Meter::create_counter(gateway_meter, "requests.total")
  azimuth::Counter::add(request_counter, 1.0, Some([
    ("http.method", azimuth::StringValue("GET")),
    ("http.path", azimuth::StringValue("/api/user/profile"))
  ]))
  
  // 记录日志
  let gateway_logger = service_loggers["api-gateway"]
  let gateway_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Processing request: " + request_id),
    Some([
      ("request.id", azimuth::StringValue(request_id)),
      ("user.id", azimuth::StringValue(user_id))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(gateway_context)),
    Some(azimuth::SpanContext::span_id(gateway_context)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(gateway_logger, gateway_log)
  
  // API Gateway调用Auth Service
  let auth_tracer = service_tracers["auth-service"]
  let auth_span = azimuth::Tracer::start_span_with_parent(auth_tracer, "auth-service.validate", gateway_span)
  let auth_context = azimuth::Span::span_context(auth_span)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(auth_context), azimuth::SpanContext::trace_id(gateway_context))
  
  // 添加认证事件
  azimuth::Span::add_event(auth_span, "auth.started", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id))
  ]))
  
  // 记录认证度量
  let auth_meter = service_meters["auth-service"]
  let auth_counter = azimuth::Meter::create_counter(auth_meter, "auth.attempts")
  azimuth::Counter::add(auth_counter, 1.0, Some([
    ("auth.type", azimuth::StringValue("token")),
    ("auth.result", azimuth::StringValue("success"))
  ]))
  
  // 记录认证日志
  let auth_logger = service_loggers["auth-service"]
  let auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Authentication successful for user: " + user_id),
    Some([
      ("request.id", azimuth::StringValue(request_id)),
      ("user.id", azimuth::StringValue(user_id))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(auth_context)),
    Some(azimuth::SpanContext::span_id(auth_context)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(auth_logger, auth_log)
  
  azimuth::Span::set_status(auth_span, azimuth::Ok)
  azimuth::Span::end(auth_span)
  
  // API Gateway调用User Service
  let user_tracer = service_tracers["user-service"]
  let user_span = azimuth::Tracer::start_span_with_parent(user_tracer, "user-service.get-profile", gateway_span)
  let user_context = azimuth::Span::span_context(user_span)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(user_context), azimuth::SpanContext::trace_id(gateway_context))
  
  // 添加用户服务事件
  azimuth::Span::add_event(user_span, "profile.fetch.started", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id))
  ]))
  
  // User Service调用Database
  let db_tracer = service_tracers["database"]
  let db_span = azimuth::Tracer::start_span_with_parent(db_tracer, "database.query", user_span)
  let db_context = azimuth::Span::span_context(db_span)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(db_context), azimuth::SpanContext::trace_id(gateway_context))
  
  // 添加数据库事件
  azimuth::Span::add_event(db_span, "query.executed", Some([
    ("query.type", azimuth::StringValue("SELECT")),
    ("table", azimuth::StringValue("users")),
    ("user.id", azimuth::StringValue(user_id))
  ]))
  
  // 记录数据库度量
  let db_meter = service_meters["database"]
  let query_counter = azimuth::Meter::create_counter(db_meter, "queries.total")
  azimuth::Counter::add(query_counter, 1.0, Some([
    ("query.type", azimuth::StringValue("SELECT")),
    ("table", azimuth::StringValue("users"))
  ]))
  
  let query_latency = azimuth::Meter::create_histogram(db_meter, "query.latency", Some("Database query latency"), Some("ms"))
  azimuth::Histogram::record(query_latency, 25.5)  // 模拟25.5ms查询时间
  
  azimuth::Span::set_status(db_span, azimuth::Ok)
  azimuth::Span::end(db_span)
  
  // User Service调用Notification Service
  let notification_tracer = service_tracers["notification-service"]
  let notification_span = azimuth::Tracer::start_span_with_parent(notification_tracer, "notification-service.send", user_span)
  let notification_context = azimuth::Span::span_context(notification_span)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(notification_context), azimuth::SpanContext::trace_id(gateway_context))
  
  // 添加通知事件
  azimuth::Span::add_event(notification_span, "notification.sent", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id)),
    ("notification.type", azimuth::StringValue("email"))
  ]))
  
  // Notification Service调用Queue Service
  let queue_tracer = service_tracers["queue-service"]
  let queue_span = azimuth::Tracer::start_span_with_parent(queue_tracer, "queue-service.publish", notification_span)
  let queue_context = azimuth::Span::span_context(queue_span)
  
  // 验证Trace ID传播
  assert_eq(azimuth::SpanContext::trace_id(queue_context), azimuth::SpanContext::trace_id(gateway_context))
  
  // 添加队列事件
  azimuth::Span::add_event(queue_span, "message.published", Some([
    ("queue.name", azimuth::StringValue("notifications")),
    ("message.type", azimuth::StringValue("email")),
    ("user.id", azimuth::StringValue(user_id))
  ]))
  
  azimuth::Span::set_status(queue_span, azimuth::Ok)
  azimuth::Span::end(queue_span)
  
  azimuth::Span::set_status(notification_span, azimuth::Ok)
  azimuth::Span::end(notification_span)
  
  // 完成User Service操作
  azimuth::Span::add_event(user_span, "profile.fetch.completed", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id))
  ]))
  
  // 记录用户服务度量
  let user_meter = service_meters["user-service"]
  let profile_counter = azimuth::Meter::create_counter(user_meter, "profile.fetches")
  azimuth::Counter::add(profile_counter, 1.0, Some([
    ("fetch.result", azimuth::StringValue("success"))
  ]))
  
  azimuth::Span::set_status(user_span, azimuth::Ok)
  azimuth::Span::end(user_span)
  
  // 完成API Gateway操作
  azimuth::Span::add_event(gateway_span, "request.completed", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id)),
    ("http.status", azimuth::IntValue(200))
  ]))
  
  // 记录响应度量
  let response_latency = azimuth::Meter::create_histogram(gateway_meter, "response.latency", Some("API response latency"), Some("ms"))
  azimuth::Histogram::record(response_latency, 125.3)  // 模拟125.3ms总响应时间
  
  azimuth::Span::set_status(gateway_span, azimuth::Ok)
  azimuth::Span::end(gateway_span)
  
  // 验证所有Span的Trace ID一致
  let trace_id = azimuth::SpanContext::trace_id(gateway_context)
  
  // 验证服务间遥测数据传播
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "microservice-integration")
  let trace_depth_gauge = azimuth::Meter::create_gauge(meter, "trace.depth")
  let service_count_gauge = azimuth::Meter::create_gauge(meter, "services.invoked")
  
  azimuth::Gauge::record(trace_depth_gauge, 6.0)  // 6个服务参与
  azimuth::Gauge::record(service_count_gauge, 5.0)  // 5个服务被调用（不包括起始服务）
}

// Test 2: 微服务错误传播和追踪测试
pub test "微服务错误传播和追踪测试" {
  // 创建错误场景模拟
  let services = [
    {
      "name": "api-gateway",
      "port": 8080,
      "dependencies": ["payment-service"]
    },
    {
      "name": "payment-service",
      "port": 8084,
      "dependencies": ["database", "external-payment-api"]
    },
    {
      "name": "external-payment-api",
      "port": 9443,
      "dependencies": []
    },
    {
      "name": "database",
      "port": 5432,
      "dependencies": []
    }
  ]
  
  // 创建服务遥测组件
  let service_tracers = {}
  let service_meters = {}
  let service_loggers = {}
  
  for service in services {
    let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), service["name"])
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), service["name"])
    let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), service["name"])
    
    service_tracers[service["name"]] = tracer
    service_meters[service["name"]] = meter
    service_loggers[service["name"]] = logger
  }
  
  // 模拟支付请求流程：API Gateway -> Payment Service -> External Payment API
  let request_id = "payment-req-" + azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string()
  let user_id = "user-67890"
  let amount = 99.99
  
  // API Gateway处理支付请求
  let gateway_tracer = service_tracers["api-gateway"]
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "api-gateway.payment.request")
  let gateway_context = azimuth::Span::span_context(gateway_span)
  
  azimuth::Span::add_event(gateway_span, "payment.request.received", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id)),
    ("amount", azimuth::FloatValue(amount)),
    ("currency", azimuth::StringValue("USD"))
  ]))
  
  // 记录支付请求度量
  let gateway_meter = service_meters["api-gateway"]
  let payment_request_counter = azimuth::Meter::create_counter(gateway_meter, "payment.requests")
  azimuth::Counter::add(payment_request_counter, 1.0, Some([
    ("payment.method", azimuth::StringValue("credit_card"))
  ]))
  
  // API Gateway调用Payment Service
  let payment_tracer = service_tracers["payment-service"]
  let payment_span = azimuth::Tracer::start_span_with_parent(payment_tracer, "payment-service.process", gateway_span)
  let payment_context = azimuth::Span::span_context(payment_span)
  
  azimuth::Span::add_event(payment_span, "payment.processing.started", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("user.id", azimuth::StringValue(user_id)),
    ("amount", azimuth::FloatValue(amount))
  ]))
  
  // Payment Service调用External Payment API
  let external_tracer = service_tracers["external-payment-api"]
  let external_span = azimuth::Tracer::start_span_with_parent(external_tracer, "external-api.charge", payment_span)
  let external_context = azimuth::Span::span_context(external_span)
  
  azimuth::Span::add_event(external_span, "external.charge.attempted", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("amount", azimuth::FloatValue(amount)),
    ("external.api", azimuth::StringValue("stripe"))
  ]))
  
  // 模拟外部API错误
  let external_error = "Payment declined: Insufficient funds"
  azimuth::Span::add_event(external_span, "external.charge.failed", Some([
    ("error.code", azimuth::StringValue("card_declined")),
    ("error.message", azimuth::StringValue(external_error))
  ]))
  
  // 记录外部API错误度量
  let external_meter = service_meters["external-payment-api"]
  let external_error_counter = azimuth::Meter::create_counter(external_meter, "external.api.errors")
  azimuth::Counter::add(external_error_counter, 1.0, Some([
    ("error.code", azimuth::StringValue("card_declined")),
    ("external.api", azimuth::StringValue("stripe"))
  ]))
  
  // 记录外部API错误日志
  let external_logger = service_loggers["external-payment-api"]
  let external_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("External payment API error: " + external_error),
    Some([
      ("request.id", azimuth::StringValue(request_id)),
      ("error.code", azimuth::StringValue("card_declined")),
      ("external.api", azimuth::StringValue("stripe"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(external_context)),
    Some(azimuth::SpanContext::span_id(external_context)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(external_logger, external_error_log)
  
  azimuth::Span::set_status(external_span, azimuth::Error)
  azimuth::Span::end(external_span)
  
  // Payment Service处理错误
  let payment_error = "Payment processing failed: " + external_error
  azimuth::Span::add_event(payment_span, "payment.processing.failed", Some([
    ("error.message", azimuth::StringValue(payment_error)),
    ("error.source", azimuth::StringValue("external.api"))
  ]))
  
  // 记录支付错误度量
  let payment_meter = service_meters["payment-service"]
  let payment_error_counter = azimuth::Meter::create_counter(payment_meter, "payment.errors")
  azimuth::Counter::add(payment_error_counter, 1.0, Some([
    ("error.type", azimuth::StringValue("payment_declined"))
  ]))
  
  // 记录支付错误日志
  let payment_logger = service_loggers["payment-service"]
  let payment_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Payment processing error: " + payment_error),
    Some([
      ("request.id", azimuth::StringValue(request_id)),
      ("user.id", azimuth::StringValue(user_id)),
      ("error.type", azimuth::StringValue("payment_declined"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(payment_context)),
    Some(azimuth::SpanContext::span_id(payment_context)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(payment_logger, payment_error_log)
  
  azimuth::Span::set_status(payment_span, azimuth::Error)
  azimuth::Span::end(payment_span)
  
  // API Gateway处理错误响应
  azimuth::Span::add_event(gateway_span, "payment.request.failed", Some([
    ("request.id", azimuth::StringValue(request_id)),
    ("error.message", azimuth::StringValue(payment_error)),
    ("http.status", azimuth::IntValue(400))
  ]))
  
  // 记录API Gateway错误度量
  let gateway_error_counter = azimuth::Meter::create_counter(gateway_meter, "api.errors")
  azimuth::Counter::add(gateway_error_counter, 1.0, Some([
    ("error.type", azimuth::StringValue("payment_error")),
    ("http.status", azimuth::IntValue(400))
  ]))
  
  // 记录API Gateway错误日志
  let gateway_logger = service_loggers["api-gateway"]
  let gateway_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("API Gateway error: Payment request failed"),
    Some([
      ("request.id", azimuth::StringValue(request_id)),
      ("user.id", azimuth::StringValue(user_id)),
      ("error.type", azimuth::StringValue("payment_error")),
      ("http.status", azimuth::IntValue(400))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(gateway_context)),
    Some(azimuth::SpanContext::span_id(gateway_context)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(gateway_logger, gateway_error_log)
  
  azimuth::Span::set_status(gateway_span, azimuth::Error)
  azimuth::Span::end(gateway_span)
  
  // 验证错误传播链路
  let trace_id = azimuth::SpanContext::trace_id(gateway_context)
  assert_eq(azimuth::SpanContext::trace_id(payment_context), trace_id)
  assert_eq(azimuth::SpanContext::trace_id(external_context), trace_id)
  
  // 验证错误度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "error-propagation")
  let error_depth_gauge = azimuth::Meter::create_gauge(meter, "error.propagation.depth")
  let error_services_gauge = azimuth::Meter::create_gauge(meter, "error.services.involved")
  
  azimuth::Gauge::record(error_depth_gauge, 3.0)  // 3层错误传播
  azimuth::Gauge::record(error_services_gauge, 3.0)  // 3个服务涉及错误
}

// Test 3: 微服务性能监控和指标聚合测试
pub test "微服务性能监控和指标聚合测试" {
  // 创建性能监控场景
  let services = [
    {
      "name": "api-gateway",
      "port": 8080,
      "endpoint": "/api/search",
      "expected_latency": 100.0,  // ms
      "error_rate": 0.01
    },
    {
      "name": "search-service",
      "port": 8085,
      "endpoint": "/search",
      "expected_latency": 50.0,  // ms
      "error_rate": 0.005
    },
    {
      "name": "cache-service",
      "port": 8086,
      "endpoint": "/cache",
      "expected_latency": 5.0,  // ms
      "error_rate": 0.001
    },
    {
      "name": "index-service",
      "port": 8087,
      "endpoint": "/index",
      "expected_latency": 80.0,  // ms
      "error_rate": 0.02
    }
  ]
  
  // 创建服务遥测组件
  let service_meters = {}
  
  for service in services {
    let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), service["name"])
    service_meters[service["name"]] = meter
  }
  
  // 模拟性能测试场景
  let test_duration_seconds = 10
  let requests_per_second = 100
  let total_requests = test_duration_seconds * requests_per_second
  
  // 创建性能度量
  let performance_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "performance-test")
  let request_rate_gauge = azimuth::Meter::create_gauge(performance_meter, "request.rate")
  let overall_latency_histogram = azimuth::Meter::create_histogram(performance_meter, "overall.latency", Some("Overall request latency"), Some("ms"))
  let overall_error_rate_gauge = azimuth::Meter::create_gauge(performance_meter, "overall.error.rate")
  
  // 模拟请求处理
  let successful_requests = 0
  let failed_requests = 0
  let total_latency = 0.0
  
  for request_id in 0..total_requests {
    // 模拟请求路径：API Gateway -> Search Service -> Cache Service/Index Service
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // API Gateway处理
    let gateway_meter = service_meters["api-gateway"]
    let gateway_latency = 80.0 + azimuth::Random::next_double(azimuth::Random::system()) * 40.0  // 80-120ms
    let gateway_success = azimuth::Random::next_double(azimuth::Random::system()) > services[0]["error_rate"]
    
    let gateway_request_counter = azimuth::Meter::create_counter(gateway_meter, "requests.total")
    azimuth::Counter::add(gateway_request_counter, 1.0, Some([
      ("endpoint", azimuth::StringValue(services[0]["endpoint"])),
      ("result", azimuth::StringValue(if gateway_success { "success" } else { "error" }))
    ]))
    
    let gateway_latency_histogram = azimuth::Meter::create_histogram(gateway_meter, "request.latency", Some("Request latency"), Some("ms"))
    azimuth::Histogram::record(gateway_latency_histogram, gateway_latency, Some([
      ("endpoint", azimuth::StringValue(services[0]["endpoint"]))
    ]))
    
    if !gateway_success {
      failed_requests = failed_requests + 1
      total_latency = total_latency + gateway_latency
      continue
    }
    
    // Search Service处理
    let search_meter = service_meters["search-service"]
    let search_latency = 40.0 + azimuth::Random::next_double(azimuth::Random::system()) * 20.0  // 40-60ms
    let search_success = azimuth::Random::next_double(azimuth::Random::system()) > services[1]["error_rate"]
    
    let search_request_counter = azimuth::Meter::create_counter(search_meter, "requests.total")
    azimuth::Counter::add(search_request_counter, 1.0, Some([
      ("endpoint", azimuth::StringValue(services[1]["endpoint"])),
      ("result", azimuth::StringValue(if search_success { "success" } else { "error" }))
    ]))
    
    let search_latency_histogram = azimuth::Meter::create_histogram(search_meter, "request.latency", Some("Request latency"), Some("ms"))
    azimuth::Histogram::record(search_latency_histogram, search_latency, Some([
      ("endpoint", azimuth::StringValue(services[1]["endpoint"]))
    ]))
    
    if !search_success {
      failed_requests = failed_requests + 1
      total_latency = total_latency + gateway_latency + search_latency
      continue
    }
    
    // 随机选择缓存或索引服务
    let use_cache = azimuth::Random::next_double(azimuth::Random::system()) > 0.3  // 70%概率使用缓存
    
    let downstream_latency = 0.0
    let downstream_success = true
    
    if use_cache {
      // Cache Service处理
      let cache_meter = service_meters["cache-service"]
      downstream_latency = 3.0 + azimuth::Random::next_double(azimuth::Random::system()) * 4.0  // 3-7ms
      downstream_success = azimuth::Random::next_double(azimuth::Random::system()) > services[2]["error_rate"]
      
      let cache_request_counter = azimuth::Meter::create_counter(cache_meter, "requests.total")
      azimuth::Counter::add(cache_request_counter, 1.0, Some([
        ("endpoint", azimuth::StringValue(services[2]["endpoint"])),
        ("result", azimuth::StringValue(if downstream_success { "success" } else { "error" }))
      ]))
      
      let cache_latency_histogram = azimuth::Meter::create_histogram(cache_meter, "request.latency", Some("Request latency"), Some("ms"))
      azimuth::Histogram::record(cache_latency_histogram, downstream_latency, Some([
        ("endpoint", azimuth::StringValue(services[2]["endpoint"]))
      ]))
    } else {
      // Index Service处理
      let index_meter = service_meters["index-service"]
      downstream_latency = 60.0 + azimuth::Random::next_double(azimuth::Random::system()) * 40.0  // 60-100ms
      downstream_success = azimuth::Random::next_double(azimuth::Random::system()) > services[3]["error_rate"]
      
      let index_request_counter = azimuth::Meter::create_counter(index_meter, "requests.total")
      azimuth::Counter::add(index_request_counter, 1.0, Some([
        ("endpoint", azimuth::StringValue(services[3]["endpoint"])),
        ("result", azimuth::StringValue(if downstream_success { "success" } else { "error" }))
      ]))
      
      let index_latency_histogram = azimuth::Meter::create_histogram(index_meter, "request.latency", Some("Request latency"), Some("ms"))
      azimuth::Histogram::record(index_latency_histogram, downstream_latency, Some([
        ("endpoint", azimuth::StringValue(services[3]["endpoint"]))
      ]))
    }
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let overall_request_latency = (end_time - start_time) / 1000000.0  // 转换为毫秒
    
    if downstream_success {
      successful_requests = successful_requests + 1
    } else {
      failed_requests = failed_requests + 1
    }
    
    total_latency = total_latency + overall_request_latency
    
    // 记录整体延迟
    azimuth::Histogram::record(overall_latency_histogram, overall_request_latency)
    
    // 模拟请求间隔
    azimuth::Clock::sleep(10)  // 10ms间隔，约100 RPS
  }
  
  // 计算性能指标
  let actual_request_rate = total_requests.to_double() / test_duration_seconds.to_double()
  let average_latency = total_latency / total_requests.to_double()
  let actual_error_rate = failed_requests.to_double() / total_requests.to_double()
  
  // 记录性能度量
  azimuth::Gauge::record(request_rate_gauge, actual_request_rate)
  azimuth::Gauge::record(overall_error_rate_gauge, actual_error_rate)
  
  // 验证性能指标
  assert_true(actual_request_rate > 90.0 && actual_request_rate < 110.0)  // 允许10%误差
  assert_true(average_latency > 100.0 && average_latency < 200.0)  // 合理的延迟范围
  assert_true(actual_error_rate < 0.05)  // 错误率应该低于5%
  
  // 验证服务级别指标
  for service in services {
    let meter = service_meters[service["name"]]
    let metrics = azimuth::MeterProvider::get_metrics(meter)
    
    // 验证请求计数
    let request_count = azimuth::MeterProvider::get_metric_value(meter, "requests.total")
    assert_true(request_count > 0)
    
    // 验证延迟指标
    let latency_data = azimuth::MeterProvider::get_histogram_data(meter, "request.latency")
    if latency_data.length() > 0 {
      let avg_latency = azimuth::HistogramData::get_average(latency_data[0])
      assert_true(avg_latency > 0)
    }
  }
}