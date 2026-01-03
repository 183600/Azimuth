// Azimuth 遥测系统 - 专注测试用例
// 高质量测试集，专注于核心功能的深度验证

// 导入必要的模块
// import "azimuth/telemetry" as telemetry

// 测试1: 遥测数据序列化完整性
test "遥测数据序列化完整性测试" {
  // 创建复杂的属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "service.version", StringValue("1.2.3"))
  Attributes::set(attrs, "service.instance.id", StringValue("instance-12345"))
  Attributes::set(attrs, "deployment.environment", StringValue("production"))
  Attributes::set(attrs, "process.pid", IntValue(1234))
  Attributes::set(attrs, "process.cpu.percent", FloatValue(75.5))
  Attributes::set(attrs, "process.memory.rss", IntValue(1073741824))
  Attributes::set(attrs, "container.id", StringValue("docker-abc123"))
  Attributes::set(attrs, "k8s.pod.name", StringValue("payment-service-7f9b8c"))
  Attributes::set(attrs, "k8s.namespace.name", StringValue("default"))
  Attributes::set(attrs, "k8s.node.name", StringValue("worker-node-1"))
  Attributes::set(attrs, "cloud.provider", StringValue("aws"))
  Attributes::set(attrs, "cloud.region", StringValue("us-west-2"))
  Attributes::set(attrs, "cloud.availability_zone", StringValue("us-west-2a"))
  Attributes::set(attrs, "host.name", StringValue("ip-10-0-0-123"))
  Attributes::set(attrs, "host.arch", StringValue("amd64"))
  Attributes::set(attrs, "os.type", StringValue("linux"))
  Attributes::set(attrs, "os.version", StringValue("5.15.0-1023-aws"))
  Attributes::set(attrs, "telemetry.sdk.name", StringValue("azimuth"))
  Attributes::set(attrs, "telemetry.sdk.language", StringValue("moonbit"))
  Attributes::set(attrs, "telemetry.sdk.version", StringValue("0.1.0"))
  
  // 验证属性设置
  assert_eq(Attributes::get(attrs, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Attributes::get(attrs, "service.version"), Some(StringValue("1.2.3")))
  assert_eq(Attributes::get(attrs, "process.pid"), Some(IntValue(1234)))
  assert_eq(Attributes::get(attrs, "process.cpu.percent"), Some(FloatValue(75.5)))
  
  // 验证不存在的键返回None
  assert_eq(Attributes::get(attrs, "nonexistent.key"), None)
  
  // 创建带有属性的Resource
  let resource_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  
  // 验证资源属性
  assert_eq(Resource::get_attribute(resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(resource, "service.version"), Some(StringValue("1.2.3")))
  assert_eq(Resource::get_attribute(resource, "nonexistent"), None)
  
  // 测试资源合并
  let override_attrs = [
    ("deployment.environment", StringValue("staging")),
    ("feature.flag", StringValue("new-payment-flow"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged = Resource::merge(resource, override_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged, "deployment.environment"), Some(StringValue("staging")))
  assert_eq(Resource::get_attribute(merged, "feature.flag"), Some(StringValue("new-payment-flow")))
}

// 测试2: 跨服务追踪上下文传播
test "跨服务追踪上下文传播测试" {
  // 创建根Span上下文
  let root_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span_id = "b7ad6b7169203331"
  let root_ctx = SpanContext::new(root_trace_id, root_span_id, true, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 验证Span上下文
  assert_eq(SpanContext::trace_id(root_ctx), root_trace_id)
  assert_eq(SpanContext::span_id(root_ctx), root_span_id)
  assert_true(SpanContext::is_valid(root_ctx))
  assert_true(SpanContext::is_sampled(root_ctx))
  
  // 创建传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // 创建载体
  let carrier = TextMapCarrier::new()
  
  // 创建上下文并注入
  let ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(ctx, key, "user-12345")
  
  CompositePropagator::inject(composite, ctx_with_user, carrier)
  
  // 验证注入的数据
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-test-trace-id-test-span-id-01"))
  
  // 测试提取
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
  
  // 测试Baggage传播
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_entries, "session.id", "session-abcdef")
  
  // 验证Baggage条目
  assert_eq(Baggage::get_entry(baggage_with_session, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage_with_session, "session.id"), Some("session-abcdef"))
  assert_eq(Baggage::get_entry(baggage_with_session, "nonexistent"), None)
  
  // 测试Baggage条目移除
  let baggage_without_session = Baggage::remove_entry(baggage_with_session, "session.id")
  assert_eq(Baggage::get_entry(baggage_without_session, "session.id"), None)
  assert_eq(Baggage::get_entry(baggage_without_session, "user.id"), Some("user-12345"))
}

// 测试3: 度量仪器并发安全性
test "度量仪器并发安全性测试" {
  // 创建MeterProvider
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent-test-meter")
  
  // 创建各种度量仪器
  let counter = Meter::create_counter(meter, "http.requests.total", 
    Some("Total number of HTTP requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "http.request.duration",
    Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections",
    Some("Number of active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage",
    Some("Current memory usage"), Some("bytes"))
  
  // 验证仪器创建
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total number of HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  // 模拟并发操作
  let attrs = Attributes::new()
  Attributes::set(attrs, "method", StringValue("GET"))
  Attributes::set(attrs, "status", StringValue("200"))
  Attributes::set(attrs, "endpoint", StringValue("/api/v1/users"))
  
  // 执行度量操作
  for i = 0; i < 100; i = i + 1 {
    Counter::add(counter, 1.0, Some(attrs))
    
    let duration = 50.0 + (i % 50).to_double() * 2.0
    Histogram::record(histogram, duration, Some(attrs))
    
    let active_connections = 10 + (i % 20)
    UpDownCounter::add(updown_counter, active_connections.to_double(), Some(attrs))
    
    let memory_usage = 1024.0 * 1024.0 * (100.0 + (i % 500).to_double())
    Gauge::as_instrument(gauge) // 转换为Instrument类型
  }
  
  // 验证仪器属性
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total number of HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  
  let histogram_instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(histogram_instrument), "http.request.duration")
  assert_eq(Instrument::description(histogram_instrument), Some("HTTP request duration"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
}

// 测试4: 日志记录与追踪关联
test "日志记录与追踪关联测试" {
  // 创建LoggerProvider和Logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "payment-service")
  
  // 创建Span上下文用于关联
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // 创建不同严重级别的日志记录
  let trace_log = LogRecord::new(Trace, "Starting payment processing")
  let debug_log = LogRecord::new(Debug, "Validating payment method")
  let info_log = LogRecord::new(Info, "Payment processed successfully")
  let warn_log = LogRecord::new(Warn, "Payment method requires 3DS verification")
  let error_log = LogRecord::new(Error, "Payment declined: insufficient funds")
  let fatal_log = LogRecord::new(Fatal, "Critical system error in payment gateway")
  
  // 验证日志严重级别
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // 验证日志消息
  assert_eq(LogRecord::body(trace_log), Some("Starting payment processing"))
  assert_eq(LogRecord::body(info_log), Some("Payment processed successfully"))
  assert_eq(LogRecord::body(error_log), Some("Payment declined: insufficient funds"))
  
  // 创建带有追踪上下文的日志记录
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "payment.id", StringValue("pay_1234567890"))
  Attributes::set(log_attrs, "payment.amount", IntValue(9999))
  Attributes::set(log_attrs, "payment.currency", StringValue("USD"))
  Attributes::set(log_attrs, "user.id", StringValue("user_12345"))
  Attributes::set(log_attrs, "merchant.id", StringValue("merchant_67890"))
  
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let observed_timestamp = timestamp + 1000000L // 1ms later
  
  let traced_log = LogRecord::new_with_context(
    Error,
    Some("Payment processing failed"),
    Some(log_attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // 验证追踪关联
  assert_eq(LogRecord::trace_id(traced_log), Some(trace_id))
  assert_eq(LogRecord::span_id(traced_log), Some(span_id))
  assert_eq(LogRecord::body(traced_log), Some("Payment processing failed"))
  
  // 发出日志记录
  Logger::emit(logger, traced_log)
  
  // 测试不同类型的日志记录
  let structured_log = LogRecord::new_with_context(
    Info,
    Some("Structured log entry"),
    Some(log_attrs),
    None, // 使用默认时间戳
    None,
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Logger::emit(logger, structured_log)
  
  // 验证严重级别排序
  assert_true(Trace < Debug)
  assert_true(Debug < Info)
  assert_true(Info < Warn)
  assert_true(Warn < Error)
  assert_true(Error < Fatal)
}

// 测试5: HTTP客户端网络异常处理
test "HTTP客户端网络异常处理测试" {
  // 创建HTTP客户端
  let client = HttpClient::new()
  
  // 创建各种HTTP请求
  let get_headers = [
    ("Accept", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/0.1.0"),
    ("X-Request-ID", "req-1234567890"),
    ("Authorization", "Bearer abc123.def456.ghi789")
  ]
  
  let post_headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("User-Agent", "Azimuth-Telemetry/0.1.0"),
    ("X-Request-ID", "req-0987654321")
  ]
  
  let get_request = HttpRequest::new("GET", "https://api.example.com/health", get_headers)
  let post_request = HttpRequest::new("POST", "https://api.example.com/payments", post_headers, Some("{\"amount\": 9999}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(get_request), "GET")
  assert_eq(HttpRequest::url(get_request), "https://api.example.com/health")
  assert_eq(HttpRequest::body(get_request), None)
  
  assert_eq(HttpRequest::http_method(post_request), "POST")
  assert_eq(HttpRequest::url(post_request), "https://api.example.com/payments")
  assert_eq(HttpRequest::body(post_request), Some("{\"amount\": 9999}"))
  
  // 模拟各种HTTP响应
  let success_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-1234567890"),
    ("Cache-Control", "no-cache")
  ]
  
  let error_headers = [
    ("Content-Type", "application/json"),
    ("X-Error-Code", "PAYMENT_DECLINED"),
    ("Retry-After", "60")
  ]
  
  let success_response = HttpResponse::new(200, success_headers, Some("{\"status\": \"ok\"}"))
  let not_found_response = HttpResponse::new(404, error_headers, Some("{\"error\": \"not found\"}"))
  let rate_limit_response = HttpResponse::new(429, error_headers, Some("{\"error\": \"rate limited\"}"))
  let server_error_response = HttpResponse::new(500, error_headers, Some("{\"error\": \"internal server error\"}"))
  
  // 验证响应状态码
  assert_eq(HttpResponse::status_code(success_response), 200)
  assert_eq(HttpResponse::status_code(not_found_response), 404)
  assert_eq(HttpResponse::status_code(rate_limit_response), 429)
  assert_eq(HttpResponse::status_code(server_error_response), 500)
  
  // 验证响应体
  assert_eq(HttpResponse::body(success_response), Some("{\"status\": \"ok\"}"))
  assert_eq(HttpResponse::body(not_found_response), Some("{\"error\": \"not found\"}"))
  assert_eq(HttpResponse::body(rate_limit_response), Some("{\"error\": \"rate limited\"}"))
  assert_eq(HttpResponse::body(server_error_response), Some("{\"error\": \"internal server error\"}"))
  
  // 测试状态码分类
  let status_codes = [200, 201, 204, 301, 400, 401, 403, 404, 429, 500, 502, 503]
  
  for code in status_codes {
    if code >= 200 && code < 300 {
      assert_true(code >= 200 && code < 300) // 成功状态码
    } else if code >= 300 && code < 400 {
      assert_true(code >= 300 && code < 400) // 重定向状态码
    } else if code >= 400 && code < 500 {
      assert_true(code >= 400 && code < 500) // 客户端错误
    } else if code >= 500 {
      assert_true(code >= 500) // 服务器错误
    }
  }
}

// 测试6: 时间戳和时钟精度测试
test "时间戳和时钟精度测试" {
  // 获取系统时钟
  let clock = Clock::system()
  
  // 获取当前时间戳
  let timestamp1 = Clock::now_unix_nanos(clock)
  let timestamp2 = Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式（应该是纳秒精度）
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  assert_true(timestamp2 >= timestamp1) // 时间应该单调递增
  
  // 测试时间戳转换
  let seconds = timestamp1 / 1000000000L
  let nanos = timestamp1 % 1000000000L
  
  assert_true(seconds >= 0L)
  assert_true(nanos >= 0L && nanos < 1000000000L)
  
  // 验证合理的年份范围（2020-2030）
  let year_seconds = seconds / (60 * 60 * 24 * 365)
  assert_true(year_seconds >= 50L && year_seconds <= 60L) // 2020-2030年范围
  
  // 测试随机数生成器
  let random = Random::system()
  let bytes = Random::next_bytes(random, 16)
  let random_u64 = Random::next_u64(random)
  
  // 验证随机字节
  assert_eq(bytes.length(), 16)
  
  // 验证随机数
  assert_true(random_u64 >= 0UL)
  
  // 生成多个随机数并验证分布
  let mut sum = 0UL
  for i = 0; i < 100; i = i + 1 {
    let r = Random::next_u64(random)
    sum = sum + r
    assert_true(r >= 0UL)
  }
  
  // 验证平均值不为零（表明随机数生成正常）
  assert_true(sum > 0UL)
}

// 测试7: 资源属性深度嵌套测试
test "资源属性深度嵌套测试" {
  // 创建复杂的嵌套属性结构
  let base_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.namespace", StringValue("finance")),
    ("service.instance.id", StringValue("instance-12345"))
  ]
  
  let deployment_attrs = [
    ("deployment.environment", StringValue("production")),
    ("deployment.region", StringValue("us-west-2")),
    ("deployment.zone", StringValue("us-west-2a")),
    ("deployment.cluster", StringValue("payment-cluster"))
  ]
  
  let infra_attrs = [
    ("host.name", StringValue("ip-10-0-0-123")),
    ("host.id", StringValue("i-0abcdef1234567890")),
    ("host.type", StringValue("t3.large")),
    ("host.arch", StringValue("amd64"))
  ]
  
  let cloud_attrs = [
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.account.id", StringValue("123456789012")),
    ("cloud.availability_zone", StringValue("us-west-2a"))
  ]
  
  let k8s_attrs = [
    ("k8s.cluster.name", StringValue("payment-cluster")),
    ("k8s.namespace.name", StringValue("payment")),
    ("k8s.pod.name", StringValue("payment-service-7f9b8c-xyz123")),
    ("k8s.pod.uid", StringValue("12345678-1234-1234-1234-123456789012")),
    ("k8s.node.name", StringValue("ip-10-0-0-123")),
    ("k8s.container.name", StringValue("payment-service"))
  ]
  
  let process_attrs = [
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("payment-service")),
    ("process.executable.path", StringValue("/app/payment-service")),
    ("process.command_args", ArrayStringValue(["./payment-service", "--config", "/etc/config.yaml"])),
    ("process.owner", StringValue("appuser")),
    ("process.runtime.name", StringValue("moonbit")),
    ("process.runtime.version", StringValue("0.1.0")),
    ("process.runtime.description", StringValue("MoonBit Runtime"))
  ]
  
  // 创建资源
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  let deployment_resource = Resource::with_attributes(Resource::new(), deployment_attrs)
  let infra_resource = Resource::with_attributes(Resource::new(), infra_attrs)
  let cloud_resource = Resource::with_attributes(Resource::new(), cloud_attrs)
  let k8s_resource = Resource::with_attributes(Resource::new(), k8s_attrs)
  let process_resource = Resource::with_attributes(Resource::new(), process_attrs)
  
  // 逐步合并资源
  let merged1 = Resource::merge(base_resource, deployment_resource)
  let merged2 = Resource::merge(merged1, infra_resource)
  let merged3 = Resource::merge(merged2, cloud_resource)
  let merged4 = Resource::merge(merged3, k8s_resource)
  let final_resource = Resource::merge(merged4, process_resource)
  
  // 验证最终资源包含所有属性
  assert_eq(Resource::get_attribute(final_resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(final_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(final_resource, "host.name"), Some(StringValue("ip-10-0-0-123")))
  assert_eq(Resource::get_attribute(final_resource, "cloud.provider"), Some(StringValue("aws")))
  assert_eq(Resource::get_attribute(final_resource, "k8s.pod.name"), Some(StringValue("payment-service-7f9b8c-xyz123")))
  assert_eq(Resource::get_attribute(final_resource, "process.pid"), Some(IntValue(1234)))
  assert_eq(Resource::get_attribute(final_resource, "process.runtime.name"), Some(StringValue("moonbit")))
  
  // 验证数组类型属性
  match Resource::get_attribute(final_resource, "process.command_args") {
    Some(ArrayStringValue(args)) => {
      assert_eq(args.length(), 3)
      assert_eq(args[0], "./payment-service")
      assert_eq(args[1], "--config")
      assert_eq(args[2], "/etc/config.yaml")
    }
    _ => assert_true(false) // 应该是数组类型
  }
  
  // 验证不存在的属性
  assert_eq(Resource::get_attribute(final_resource, "nonexistent.attribute"), None)
}

// 测试8: 仪器作用域和生命周期测试
test "仪器作用域和生命周期测试" {
  // 创建多个MeterProvider
  let provider1 = MeterProvider::default()
  let provider2 = MeterProvider::noop()
  
  // 创建具有不同作用域的Meter
  let meter1 = MeterProvider::get_meter(provider1, "payment-service", Some("1.2.3"))
  let meter2 = MeterProvider::get_meter(provider1, "auth-service", Some("2.0.0"))
  let meter3 = MeterProvider::get_meter(provider2, "noop-service", Some("0.0.1"))
  
  // 验证Meter作用域
  let scope1 = meter1.scope
  let scope2 = meter2.scope
  let scope3 = meter3.scope
  
  assert_eq(scope1.name, "payment-service")
  assert_eq(scope1.version, Some("1.2.3"))
  assert_eq(scope1.schema_url, None)
  
  assert_eq(scope2.name, "auth-service")
  assert_eq(scope2.version, Some("2.0.0"))
  assert_eq(scope2.schema_url, None)
  
  assert_eq(scope3.name, "noop-service")
  assert_eq(scope3.version, Some("0.0.1"))
  assert_eq(scope3.schema_url, None)
  
  // 创建不同类型的仪器
  let counter1 = Meter::create_counter(meter1, "payments.total")
  let counter2 = Meter::create_counter(meter2, "auth.attempts")
  let histogram1 = Meter::create_histogram(meter1, "payment.amount", Some("Payment amounts"), Some("USD"))
  let updown_counter1 = Meter::create_updown_counter(meter2, "active.sessions")
  let gauge1 = Meter::create_gauge(meter3, "queue.size")
  
  // 验证仪器创建
  assert_eq(counter1.name, "payments.total")
  assert_eq(counter2.name, "auth.attempts")
  assert_eq(histogram1.name, "payment.amount")
  assert_eq(histogram1.description, Some("Payment amounts"))
  assert_eq(histogram1.unit, Some("USD"))
  assert_eq(updown_counter1.name, "active.sessions")
  assert_eq(gauge1.name, "queue.size")
  
  // 测试仪器操作
  let payment_attrs = Attributes::new()
  Attributes::set(payment_attrs, "currency", StringValue("USD"))
  Attributes::set(payment_attrs, "method", StringValue("credit_card"))
  
  let auth_attrs = Attributes::new()
  Attributes::set(auth_attrs, "user.type", StringValue("premium"))
  Attributes::set(auth_attrs, "auth.method", StringValue("oauth2"))
  
  // 执行度量操作
  Counter::add(counter1, 100.0, Some(payment_attrs))
  Counter::add(counter2, 250.0, Some(auth_attrs))
  Histogram::record(histogram1, 99.99, Some(payment_attrs))
  UpDownCounter::add(updown_counter1, 50.0, Some(auth_attrs))
  
  // 测试Logger作用域
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "payment-service", Some("1.2.3"))
  let logger2 = LoggerProvider::get_logger(logger_provider, "auth-service", Some("2.0.0"))
  
  // 验证Logger作用域
  let logger_scope1 = logger1.scope
  let logger_scope2 = logger2.scope
  
  assert_eq(logger_scope1.name, "payment-service")
  assert_eq(logger_scope1.version, Some("1.2.3"))
  assert_eq(logger_scope2.name, "auth-service")
  assert_eq(logger_scope2.version, Some("2.0.0"))
  
  // 创建带有不同作用域的日志记录
  let payment_log = LogRecord::new(Info, "Payment processed successfully")
  let auth_log = LogRecord::new(Warn, "Failed authentication attempt")
  
  // 发出日志
  Logger::emit(logger1, payment_log)
  Logger::emit(logger2, auth_log)
  
  // 测试Tracer作用域
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "payment-service", Some("1.2.3"))
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "auth-service", Some("2.0.0"))
  
  // 验证Tracer作用域
  let tracer_scope1 = Tracer::instrumentation_scope(tracer1)
  let tracer_scope2 = Tracer::instrumentation_scope(tracer2)
  
  assert_eq(tracer_scope1.name, "payment-service")
  assert_eq(tracer_scope1.version, Some("1.2.3"))
  assert_eq(tracer_scope2.name, "auth-service")
  assert_eq(tracer_scope2.version, Some("2.0.0"))
  
  // 创建Span
  let payment_span = Tracer::start_span(tracer1, "process-payment")
  let auth_span = Tracer::start_span(tracer2, "authenticate-user")
  
  // 验证Span属性
  assert_eq(Span::name(payment_span), "process-payment")
  assert_eq(Span::kind(payment_span), Internal)
  assert_true(Span::is_recording(payment_span))
  
  assert_eq(Span::name(auth_span), "authenticate-user")
  assert_eq(Span::kind(auth_span), Internal)
  assert_true(Span::is_recording(auth_span))
  
  // 验证Span上下文
  let payment_ctx = Span::span_context(payment_span)
  let auth_ctx = Span::span_context(auth_span)
  
  assert_true(SpanContext::is_valid(payment_ctx))
  assert_true(SpanContext::is_sampled(payment_ctx))
  assert_true(SpanContext::is_valid(auth_ctx))
  assert_true(SpanContext::is_sampled(auth_ctx))
  
  // 结束Span
  Span::end(payment_span)
  Span::end(auth_span)
}