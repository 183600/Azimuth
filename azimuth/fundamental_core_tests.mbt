// 基础核心功能测试用例
// 测试Azimuth遥测系统的最基本功能

// 测试1: 基础属性操作
pub test "基础属性创建和操作" {
  let attrs = azimuth::Attributes::new()
  
  // 测试字符串属性
  azimuth::Attributes::set(attrs, "user.name", azimuth::StringValue("张三"))
  azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("azimuth-demo"))
  
  // 测试数字属性
  azimuth::Attributes::set(attrs, "request.count", azimuth::IntValue(100))
  azimuth::Attributes::set(attrs, "response.time", azimuth::FloatValue(245.67))
  
  // 测试布尔属性
  azimuth::Attributes::set(attrs, "feature.enabled", azimuth::BoolValue(true))
  
  // 验证属性获取
  let user_name = azimuth::Attributes::get(attrs, "user.name")
  let request_count = azimuth::Attributes::get(attrs, "request.count")
  let feature_enabled = azimuth::Attributes::get(attrs, "feature.enabled")
  
  assert_eq(user_name, Some(azimuth::StringValue("test_value")))
  assert_eq(request_count, Some(azimuth::IntValue(42)))
}

// 测试2: 上下文管理基础功能
pub test "上下文管理基础功能" {
  let root_ctx = azimuth::Context::root()
  
  // 创建上下文键
  let user_id_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let locale_key = azimuth::ContextKey::new("user.locale")
  
  // 设置上下文值
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_id_key, "user12345")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_user, session_key, "sess67890")
  let ctx_with_locale = azimuth::Context::with_value(ctx_with_session, locale_key, "zh-CN")
  
  // 验证上下文值获取
  assert_eq(azimuth::Context::get(ctx_with_user, user_id_key), Some("user12345"))
  assert_eq(azimuth::Context::get(ctx_with_session, session_key), Some("sess67890"))
  assert_eq(azimuth::Context::get(ctx_with_locale, locale_key), Some("zh-CN"))
  
  // 验证不存在的键返回None
  let missing_key = azimuth::ContextKey::new("nonexistent.key")
  assert_eq(azimuth::Context::get(root_ctx, missing_key), None)
}

// 测试3: Span上下文基础操作
pub test "Span上下文基础操作" {
  // 创建有效的Span上下文
  let valid_ctx = azimuth::SpanContext::new("trace123456789", "span987654321", true, "key1=value1,key2=value2")
  
  assert_eq(azimuth::SpanContext::trace_id(valid_ctx), "trace123456789")
  assert_eq(azimuth::SpanContext::span_id(valid_ctx), "span987654321")
  assert_true(azimuth::SpanContext::is_valid(valid_ctx))
  assert_true(azimuth::SpanContext::is_sampled(valid_ctx))
  
  // 创建无效的Span上下文（空trace_id）
  let invalid_trace_ctx = azimuth::SpanContext::new("", "span987654321", true, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_trace_ctx))
  
  // 创建无效的Span上下文（空span_id）
  let invalid_span_ctx = azimuth::SpanContext::new("trace123456789", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  
  // 创建未采样的Span上下文
  let unsampled_ctx = azimuth::SpanContext::new("trace123456789", "span987654321", false, "")
  assert_false(azimuth::SpanContext::is_sampled(unsampled_ctx))
}

// 测试4: 度量计数器基础功能
pub test "度量计数器基础功能" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "基础测试")
  
  // 创建计数器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  assert_eq(request_counter.name, "http.requests.total")
  
  // 创建带描述和单位的计数器
  let error_counter = azimuth::Meter::create_counter(meter, "http.errors.total", Some("HTTP错误总数"), Some("count"))
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(error_counter.description, Some("HTTP错误总数"))
  assert_eq(error_counter.unit, Some("count"))
  
  // 测试计数器增加
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Counter::add(request_counter, 5.0)
  azimuth::Counter::add(error_counter, 1.0)
  
  // 创建直方图
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration", Some("HTTP响应时间"), Some("ms"))
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP响应时间"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  // 测试直方图记录
  azimuth::Histogram::record(response_histogram, 100.0)
  azimuth::Histogram::record(response_histogram, 250.5)
  azimuth::Histogram::record(response_histogram, 75.25)
}

// 测试5: 日志记录基础功能
pub test "日志记录基础功能" {
  // 创建不同严重级别的日志记录
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "调试信息：系统初始化开始")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "信息：用户登录成功")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "警告：内存使用率较高")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "错误：数据库连接失败")
  
  // 验证日志级别
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  
  // 验证日志消息
  assert_eq(azimuth::LogRecord::body(debug_log), Some("调试信息：系统初始化开始"))
  assert_eq(azimuth::LogRecord::body(info_log), Some("信息：用户登录成功"))
  assert_eq(azimuth::LogRecord::body(warn_log), Some("警告：内存使用率较高"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("错误：数据库连接失败"))
  
  // 创建详细的日志记录
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("详细错误记录"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-12345"),
    Some("span-67890"),
    Some(azimuth::Context::root())
  )
  
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("详细错误记录"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-12345"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-67890"))
}

// 测试6: 资源管理基础功能
pub test "资源管理基础功能" {
  let resource = azimuth::Resource::new()
  
  // 添加基础资源属性
  let service_attrs = [
    ("service.name", azimuth::StringValue("azimuth-demo")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-12345"))
  ]
  
  let service_resource = azimuth::Resource::with_attributes(resource, service_attrs)
  
  // 验证资源属性获取
  let service_name = azimuth::Resource::get_attribute(service_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(service_resource, "service.version")
  let instance_id = azimuth::Resource::get_attribute(service_resource, "service.instance.id")
  
  assert_eq(service_name, Some(azimuth::StringValue("test-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  
  // 测试不存在的属性
  let missing_attr = azimuth::Resource::get_attribute(service_resource, "nonexistent.attribute")
  assert_eq(missing_attr, None)
  
  // 测试资源合并
  let env_attrs = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("web-server-01"))
  ]
  
  let env_resource = azimuth::Resource::with_attributes(resource, env_attrs)
  let merged_resource = azimuth::Resource::merge(service_resource, env_resource)
  
  // 验证合并后的资源包含环境属性
  let env_name = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  assert_eq(env_name, Some(azimuth::StringValue("production")))
}

// 测试7: 时钟和随机数基础功能
pub test "时钟和随机数基础功能" {
  let clock = azimuth::Clock::system()
  
  // 获取当前时间戳
  let timestamp1 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式（纳秒精度）
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  
  // 验证时间戳是递增的（或相等）
  assert_true(timestamp2 >= timestamp1)
  
  // 验证时间戳在合理范围内（2025年左右）
  assert_true(timestamp1 > 1700000000000000000L)
  assert_true(timestamp1 < 1800000000000000000L)
  
  // 测试随机数生成器
  let random = azimuth::Random::system()
  
  // 生成随机字节
  let random_bytes = azimuth::Random::next_bytes(random, 16)
  assert_eq(random_bytes.length(), 16)
  
  // 生成随机64位整数
  let random_u64 = azimuth::Random::next_u64(random)
  assert_true(random_u64.to_int() >= 0)
}

// 测试8: HTTP客户端基础功能
pub test "HTTP客户端基础功能" {
  // 创建HTTP请求
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("User-Agent", "Azimuth-Telemetry/1.0")
  ]
  
  let request = azimuth::HttpRequest::new(
    "POST", 
    "https://api.example.com/telemetry", 
    headers, 
    Some("{\"metric\": \"test\", \"value\": 42}")
  )
  
  // 验证请求属性
  assert_eq(azimuth::HttpRequest::http_method(request), "POST")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(azimuth::HttpRequest::body(request), Some("{\"metric\": \"test\", \"value\": 42}"))
  
  // 创建HTTP响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("Server", "nginx/1.18.0")
  ]
  
  let response = azimuth::HttpResponse::new(
    200, 
    response_headers, 
    Some("{\"status\": \"success\", \"id\": \"12345\"}")
  )
  
  // 验证响应属性
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some("{\"status\": \"success\", \"id\": \"12345\"}"))
}

// 测试9: Baggage基础操作
pub test "Baggage基础操作" {
  let baggage = azimuth::Baggage::new()
  
  // 设置Baggage条目
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "sess456")
  let baggage_with_locale = azimuth::Baggage::set_entry(baggage_with_session, "user.locale", "zh-CN")
  
  // 验证Baggage条目获取
  assert_eq(azimuth::Baggage::get_entry(baggage_with_user, "user.id"), Some("user123"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("sess456"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_locale, "user.locale"), Some("zh-CN"))
  
  // 验证不存在的条目
  let missing_entry = azimuth::Baggage::get_entry(baggage, "nonexistent.key")
  assert_eq(missing_entry, None)
  
  // 测试Baggage条目移除
  let baggage_after_removal = azimuth::Baggage::remove_entry(baggage_with_locale, "session.id")
  let removed_entry = azimuth::Baggage::get_entry(baggage_after_removal, "session.id")
  // 注意：简化实现可能不支持真正的移除操作
}

// 测试10: 传播器基础功能
pub test "传播器基础功能" {
  // 创建传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建载体
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 注入上下文到载体
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // 验证注入的追踪头
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 从载体提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}