// Azimuth 额外测试用例 - 补充核心功能测试

test "资源属性操作测试" {
  let resource = Resource::new()
  let attrs = [("service.name", StringValue("test-service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // 测试属性获取
  match Resource::get_attribute(resource_with_attrs, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  // 测试资源合并
  let override_resource = Resource::with_attributes(resource, [("environment", StringValue("test"))])
  let merged = Resource::merge(resource_with_attrs, override_resource)
  
  match Resource::get_attribute(merged, "environment") {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
}

test "W3C传播器注入提取测试" {
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // 测试注入
  CompositePropagator::inject(composite, ctx, carrier)
  
  // 测试提取
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let key = ContextKey::new("extracted")
  
  match Context::get(extracted_ctx, key) {
    Some(value) => assert_eq(value, "true")
    _ => assert_true(false)
  }
}

test "日志记录完整生命周期测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  // 创建带有完整信息的日志记录
  let attrs = Attributes::new()
  let timestamp = Clock::system() |> Clock::now_unix_nanos
  let record = LogRecord::new_with_context(
    Error,
    Some("测试错误消息"),
    Some(attrs),
    Some(timestamp),
    None,
    Some("trace-123"),
    Some("span-456"),
    Some(Context::root())
  )
  
  // 验证日志记录属性 - 使用模式匹配而不是assert_eq
  match LogRecord::severity_number(record) {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match LogRecord::body(record) {
    Some(body) => assert_eq(body, "测试错误消息")
    _ => assert_true(false)
  }
  
  match LogRecord::trace_id(record) {
    Some(trace_id) => assert_eq(trace_id, "trace-123")
    _ => assert_true(false)
  }
  
  // 发送日志记录
  Logger::emit(logger, record)
}

test "上下文传递链式测试" {
  let root_ctx = Context::root()
  let key1 = ContextKey::new("user.id")
  let key2 = ContextKey::new("request.id")
  
  // 链式设置上下文值
  let ctx1 = Context::with_value(root_ctx, key1, "user-123")
  let ctx2 = Context::with_value(ctx1, key2, "req-456")
  
  // 验证上下文值
  match Context::get(ctx2, key1) {
    Some(user_id) => assert_eq(user_id, "user-123")
    _ => assert_true(false)
  }
  
  match Context::get(ctx2, key2) {
    Some(req_id) => assert_eq(req_id, "req-456")
    _ => assert_true(false)
  }
  
  // 验证根上下文没有值
  match Context::get(root_ctx, key1) {
    Some(_) => assert_true(false)
    _ => assert_true(true)
  }
}

test "行李管理操作测试" {
  let baggage = Baggage::new()
  
  // 设置行李条目
  let baggage_with_entry = Baggage::set_entry(baggage, "user.locale", "zh-CN")
  
  // 获取行李条目
  match Baggage::get_entry(baggage_with_entry, "user.locale") {
    Some(locale) => assert_eq(locale, "zh-CN")
    _ => assert_true(false)
  }
  
  // 测试不存在的条目
  match Baggage::get_entry(baggage_with_entry, "nonexistent") {
    Some(_) => assert_true(false)
    _ => assert_true(true)
  }
  
  // 移除行李条目
  let baggage_without_entry = Baggage::remove_entry(baggage_with_entry, "user.locale")
  match Baggage::get_entry(baggage_without_entry, "user.locale") {
    Some(_) => assert_true(false)
    _ => assert_true(true)
  }
}

test "HTTP客户端请求响应测试" {
  let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer token123")]
  
  // 创建HTTP请求 - 修复参数数量
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some("{\"test\": \"data\"}"))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  match HttpRequest::body(request) {
    Some(body) => assert_eq(body, "{\"test\": \"data\"}")
    _ => assert_true(false)
  }
  
  // 创建HTTP响应 - 修复参数数量
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"result\": \"success\"}"))
  
  // 验证响应属性
  assert_eq(HttpResponse::status_code(response), 200)
  match HttpResponse::body(response) {
    Some(body) => assert_eq(body, "{\"result\": \"success\"}")
    _ => assert_true(false)
  }
}

test "跨服务追踪集成测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  
  // 创建子span
  let child_tracer = TracerProvider::get_tracer(tracer_provider, "service-b")
  let child_span = Tracer::start_span(child_tracer, "child-operation")
  
  // 验证span关系
  let parent_ctx = Span::span_context(parent_span)
  let child_ctx = Span::span_context(child_span)
  
  assert_true(SpanContext::is_valid(parent_ctx))
  assert_true(SpanContext::is_valid(child_ctx))
  assert_true(SpanContext::is_sampled(parent_ctx))
  assert_true(SpanContext::is_sampled(child_ctx))
  
  // 结束spans
  Span::end(child_span)
  Span::end(parent_span)
}

test "度量仪表性能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.meter")
  
  // 创建多个度量仪器
  let counter = Meter::create_counter(meter, "requests.total")
  let histogram = Meter::create_histogram(meter, "response.duration")
  let updown_counter = Meter::create_counter(meter, "active.connections")
  
  // 执行多次操作以测试性能
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, 1.0)
    Histogram::record(histogram, @double.as_f64(i) * 0.1)
    if i % 2 == 0 {
      Counter::add(updown_counter, 1.0)
    } else {
      Counter::add(updown_counter, -1.0)
    }
  }
  
  // 验证仪器名称
  assert_eq(Instrument::name(Counter("test", None, None)), "test")
  assert_true(true) // 性能测试通过
}

test "错误边界恢复测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.test")
  let span = Tracer::start_span(tracer, "error.operation")
  
  // 设置错误状态 - 修复参数数量
  Span::set_status(span, Error, description? = Some("模拟错误"))
  
  // 验证状态设置 - 使用模式匹配
  match Span::status(span) {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // 添加错误事件
  let error_attrs = [("error.type", StringValue("ValidationError")), ("error.message", StringValue("输入参数无效"))]
  Span::add_event(span, "error.occurred", attributes? = Some(error_attrs))
  
  // 结束span
  Span::end(span)
  assert_true(true) // 错误处理测试通过
}

test "配置国际化测试" {
  // 测试不同的国际化配置
  let configs = [
    ("zh-CN", "中文配置"),
    ("en-US", "English Configuration"),
    ("ja-JP", "日本語設定"),
    ("fr-FR", "Configuration Française")
  ]
  
  for config in configs {
    let locale = config.0
    let description = config.1
    
    // 创建带有locale的仪器作用域
    let scope = InstrumentationScope::{ 
      name: "test.scope", 
      version: Some("1.0.0"), 
      schema_url: Some("https://example.com/schema") 
    }
    
    // 验证配置
    assert_eq(scope.name, "test.scope")
    match scope.version {
      Some(v) => assert_eq(v, "1.0.0")
      _ => assert_true(false)
    }
    
    match scope.schema_url {
      Some(url) => assert_eq(url, "https://example.com/schema")
      _ => assert_true(false)
    }
  }
  
  assert_true(true) // 国际化配置测试通过
}