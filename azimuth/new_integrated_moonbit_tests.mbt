// 新增MoonBit测试用例 - 集成测试套件
// 涵盖Azimuth遥测系统的核心功能和边界情况

// 测试1: 分布式追踪端到端流程
pub test "分布式追踪端到端流程" {
  // 创建根追踪上下文
  let trace_id = "trace-abc123def456"
  let root_span_id = "span-root001"
  let span_ctx = azimuth::SpanContext::new(trace_id, root_span_id, true, "")
  
  // 创建追踪器和根Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "e2e-test-service")
  let root_span = azimuth::Span::new("api-request", azimuth::Server, span_ctx)
  
  // 添加事件和属性
  azimuth::Span::add_event(root_span, "request.received", Some([("endpoint", azimuth::StringValue("/api/data"))]))
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("GET"))
  azimuth::Span::set_attribute(root_span, "user.id", azimuth::StringValue("user-789"))
  
  // 创建子Span - 数据库查询
  let db_span_ctx = azimuth::SpanContext::new(trace_id, "span-db002", true, "")
  let db_span = azimuth::Span::new("database.query", azimuth::Client, db_span_ctx)
  azimuth::Span::set_attribute(db_span, "db.statement", azimuth::StringValue("SELECT * FROM users"))
  azimuth::Span::set_attribute(db_span, "db.type", azimuth::StringValue("postgresql"))
  
  // 创建子Span - 缓存操作
  let cache_span_ctx = azimuth::SpanContext::new(trace_id, "span-cache003", true, "")
  let cache_span = azimuth::Span::new("cache.get", azimuth::Internal, cache_span_ctx)
  azimuth::Span::set_attribute(cache_span, "cache.key", azimuth::StringValue("user:789"))
  azimuth::Span::set_attribute(cache_span, "cache.hit", azimuth::BoolValue(false))
  
  // 设置Span状态并结束
  azimuth::Span::set_status(cache_span, azimuth::Ok)
  azimuth::Span::end(cache_span)
  
  azimuth::Span::set_status(db_span, azimuth::Ok)
  azimuth::Span::end(db_span)
  
  azimuth::Span::add_event(root_span, "response.sent", Some([("status.code", azimuth::IntValue(200))]))
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::end(root_span)
  
  // 验证追踪上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(azimuth::SpanContext::span_id(span_ctx), root_span_id)
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
}

// 测试2: 度量聚合和统计功能
pub test "度量聚合和统计功能" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建多种类型的度量工具
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let active_connections_gauge = azimuth::Meter::create_gauge(meter, "http.connections.active", Some("Active connections"), Some("connections"))
  
  // 模拟度量数据记录
  // 记录请求计数
  azimuth::Counter::add(request_counter, 1.0, Some([("method", azimuth::StringValue("GET")), ("status", azimuth::StringValue("200"))]))
  azimuth::Counter::add(request_counter, 1.0, Some([("method", azimuth::StringValue("POST")), ("status", azimuth::StringValue("201"))]))
  azimuth::Counter::add(request_counter, 1.0, Some([("method", azimuth::StringValue("GET")), ("status", azimuth::StringValue("404"))]))
  azimuth::Counter::add(request_counter, 1.0, Some([("method", azimuth::StringValue("GET")), ("status", azimuth::StringValue("200"))]))
  
  // 记录响应时间分布
  azimuth::Histogram::record(response_time_histogram, 45.2, Some([("endpoint", azimuth::StringValue("/api/users"))]))
  azimuth::Histogram::record(response_time_histogram, 128.7, Some([("endpoint", azimuth::StringValue("/api/data"))]))
  azimuth::Histogram::record(response_time_histogram, 23.1, Some([("endpoint", azimuth::StringValue("/api/users"))]))
  azimuth::Histogram::record(response_time_histogram, 256.3, Some([("endpoint", azimuth::StringValue("/api/reports"))]))
  azimuth::Histogram::record(response_time_histogram, 67.8, Some([("endpoint", azimuth::StringValue("/api/users"))]))
  
  // 验证度量工具属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(response_time_histogram.description, Some("HTTP response time"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  assert_eq(active_connections_gauge.name, "http.connections.active")
}

// 测试3: 日志关联和上下文传播
pub test "日志关联和上下文传播" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "context-propagation-test")
  
  // 创建带有追踪上下文的日志记录
  let trace_id = "trace-xyz789uvw012"
  let span_id = "span-log001"
  let ctx = azimuth::Context::root()
  
  // 添加上下文信息
  let user_key = azimuth::ContextKey::new("user.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let ctx_with_user = azimuth::Context::with_value(ctx, user_key, "user-456")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_key, "req-abc123")
  
  // 创建不同严重级别的日志记录
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User request processed successfully"),
    Some([("component", azimuth::StringValue("auth-service"))]),
    Some(1735689600000000000L),
    Some(1735689600000000100L),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_request)
  )
  
  let warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Deprecated API endpoint used"),
    Some([("endpoint", azimuth::StringValue("/api/old")), ("user.agent", azimuth::StringValue("legacy-client/1.0"))]),
    Some(1735689600000001000L),
    Some(1735689600000001100L),
    Some(trace_id),
    Some("span-log002"),
    Some(ctx_with_request)
  )
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Database connection failed"),
    Some([("error.code", azimuth::StringValue("DB_CONN_FAILED")), ("retry.count", azimuth::IntValue(3))]),
    Some(1735689600000002000L),
    Some(1735689600000002100L),
    Some(trace_id),
    Some("span-log003"),
    Some(ctx_with_request)
  )
  
  // 发出日志记录
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(info_log), Some("User request processed successfully"))
  assert_eq(azimuth::LogRecord::trace_id(info_log), Some(trace_id))
  assert_eq(azimuth::LogRecord::span_id(info_log), Some(span_id))
  
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::body(warn_log), Some("Deprecated API endpoint used"))
  
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(error_log), Some("Database connection failed"))
}

// 测试4: 资源合并策略和优先级
pub test "资源合并策略和优先级" {
  // 创建基础资源
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("azimuth-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.namespace", azimuth::StringValue("production"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // 创建环境特定资源
  let env_resource = azimuth::Resource::new()
  let env_attrs = [
    ("deployment.environment", azimuth::StringValue("production")),
    ("host.name", azimuth::StringValue("prod-server-01")),
    ("region", azimuth::StringValue("us-west-2"))
  ]
  let env_resource_with_attrs = azimuth::Resource::with_attributes(env_resource, env_attrs)
  
  // 创建运行时资源
  let runtime_resource = azimuth::Resource::new()
  let runtime_attrs = [
    ("process.id", azimuth::StringValue("12345")),
    ("process.executable.name", azimuth::StringValue("azimuth")),
    ("process.runtime.name", azimuth::StringValue("moonbit"))
  ]
  let runtime_resource_with_attrs = azimuth::Resource::with_attributes(runtime_resource, runtime_attrs)
  
  // 测试资源合并策略 - 基础 + 环境
  let merged_env_resource = azimuth::Resource::merge(base_resource_with_attrs, env_resource_with_attrs)
  
  // 验证合并后的资源包含所有属性
  assert_eq(azimuth::Resource::get_attribute(merged_env_resource, "service.name"), Some(azimuth::StringValue("azimuth-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_env_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_env_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_env_resource, "host.name"), Some(azimuth::StringValue("prod-server-01")))
  
  // 测试资源合并策略 - (基础 + 环境) + 运行时
  let final_resource = azimuth::Resource::merge(merged_env_resource, runtime_resource_with_attrs)
  
  // 验证最终资源包含所有属性
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), Some(azimuth::StringValue("azimuth-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "process.id"), Some(azimuth::StringValue("12345")))
  
  // 测试属性覆盖 - 创建具有相同键的资源
  let override_resource = azimuth::Resource::new()
  let override_attrs = [
    ("service.name", azimuth::StringValue("override-service")),  // 应该覆盖基础值
    ("new.attribute", azimuth::StringValue("new-value"))        // 新增属性
  ]
  let override_resource_with_attrs = azimuth::Resource::with_attributes(override_resource, override_attrs)
  
  // 测试覆盖行为
  let override_merged = azimuth::Resource::merge(base_resource_with_attrs, override_resource_with_attrs)
  
  // 验证覆盖结果
  assert_eq(azimuth::Resource::get_attribute(override_merged, "service.name"), Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(override_merged, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(override_merged, "new.attribute"), Some(azimuth::StringValue("new-value")))
}

// 测试5: 传播器组合和注入提取
pub test "传播器组合和注入提取" {
  // 创建多种传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 创建带有追踪和行李信息的上下文
  let ctx = azimuth::Context::root()
  
  // 添加追踪信息
  let trace_key = azimuth::ContextKey::new("trace.state")
  let ctx_with_trace = azimuth::Context::with_value(ctx, trace_key, "trace-info")
  
  // 添加行李信息
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-789")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-xyz")
  
  let baggage_key = azimuth::ContextKey::new("baggage")
  let ctx_with_baggage = azimuth::Context::with_value(ctx_with_trace, baggage_key, baggage_with_session)
  
  // 创建载体并注入上下文
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_baggage, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  
  // 验证追踪头部存在
  assert_true(trace_header != None)
  
  // 验证行李头部存在
  assert_true(baggage_header != None)
  
  // 创建新的载体和提取上下文
  let extraction_carrier = azimuth::TextMapCarrier::new()
  
  // 复制注入的头部到新载体
  match (trace_header) {
    Some(header) => azimuth::TextMapCarrier::set(extraction_carrier, "traceparent", header)
    None => ()
  }
  
  match (baggage_header) {
    Some(header) => azimuth::TextMapCarrier::set(extraction_carrier, "baggage", header)
    None => ()
  }
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, extraction_carrier)
  
  // 验证提取的上下文包含预期信息
  let extracted_trace = azimuth::Context::get(extracted_ctx, trace_key)
  let extracted_baggage = azimuth::Context::get(extracted_ctx, baggage_key)
  
  assert_true(extracted_trace != None)
  assert_true(extracted_baggage != None)
}

// 测试6: 错误边界和异常处理
pub test "错误边界和异常处理" {
  // 测试错误的Span上下文处理
  let invalid_trace_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(invalid_trace_ctx))
  
  // 测试空属性键值处理
  let attrs = azimuth::Attributes::new()
  
  // 测试空键
  azimuth::Attributes::set(attrs, "", azimuth::StringValue("empty-key-value"))
  let empty_key_value = azimuth::Attributes::get(attrs, "")
  
  // 测试空值
  azimuth::Attributes::set(attrs, "empty.value", azimuth::StringValue(""))
  let empty_value = azimuth::Attributes::get(attrs, "empty.value")
  
  // 测试特殊字符键
  let special_key = "key.with.special.chars!@#$%^&*()"
  azimuth::Attributes::set(attrs, special_key, azimuth::StringValue("special-value"))
  let special_value = azimuth::Attributes::get(attrs, special_key)
  
  // 测试错误记录
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "error-test")
  let error_span = azimuth::Tracer::start_span(tracer, "error-operation")
  
  // 记录错误
  azimuth::Span::record_error(error_span, "Test error message")
  azimuth::Span::record_exception(error_span, "TestException", "Detailed exception information")
  
  // 设置错误状态
  azimuth::Span::set_status(error_span, azimuth::Error, "Operation failed due to test error")
  
  // 验证错误状态
  let status = azimuth::Span::get_status(error_span)
  assert_eq(status.code, azimuth::Error)
  assert_eq(status.message, Some("Operation failed due to test error"))
  
  // 测试错误日志记录
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "error-logger")
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Critical system error occurred"),
    Some([
      ("error.type", azimuth::StringValue("SystemError")),
      ("error.code", azimuth::StringValue("SYS_001")),
      ("error.severity", azimuth::StringValue("critical")),
      ("stack.trace", azimuth::StringValue("at module.function (line:column)"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("error-trace"),
    Some("error-span"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, error_log)
  
  // 验证错误日志属性
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(error_log), Some("Critical system error occurred"))
}

// 测试7: 并发安全和资源管理
pub test "并发安全和资源管理" {
  // 创建共享资源
  let shared_attrs = azimuth::Attributes::new()
  let shared_baggage = azimuth::Baggage::new()
  
  // 模拟并发操作 - 设置多个属性
  azimuth::Attributes::set(shared_attrs, "concurrent.1", azimuth::StringValue("value1"))
  azimuth::Attributes::set(shared_attrs, "concurrent.2", azimuth::StringValue("value2"))
  azimuth::Attributes::set(shared_attrs, "concurrent.3", azimuth::StringValue("value3"))
  azimuth::Attributes::set(shared_attrs, "concurrent.4", azimuth::StringValue("value4"))
  azimuth::Attributes::set(shared_attrs, "concurrent.5", azimuth::StringValue("value5"))
  
  // 模拟并发行李操作
  let baggage_1 = azimuth::Baggage::set_entry(shared_baggage, "thread.1", "data1")
  let baggage_2 = azimuth::Baggage::set_entry(baggage_1, "thread.2", "data2")
  let baggage_3 = azimuth::Baggage::set_entry(baggage_2, "thread.3", "data3")
  let baggage_4 = azimuth::Baggage::set_entry(baggage_3, "thread.4", "data4")
  let baggage_5 = azimuth::Baggage::set_entry(baggage_4, "thread.5", "data5")
  
  // 验证所有属性都正确设置
  let val1 = azimuth::Attributes::get(shared_attrs, "concurrent.1")
  let val2 = azimuth::Attributes::get(shared_attrs, "concurrent.2")
  let val3 = azimuth::Attributes::get(shared_attrs, "concurrent.3")
  let val4 = azimuth::Attributes::get(shared_attrs, "concurrent.4")
  let val5 = azimuth::Attributes::get(shared_attrs, "concurrent.5")
  
  assert_eq(val1, Some(azimuth::StringValue("value1")))
  assert_eq(val2, Some(azimuth::StringValue("value2")))
  assert_eq(val3, Some(azimuth::StringValue("value3")))
  assert_eq(val4, Some(azimuth::StringValue("value4")))
  assert_eq(val5, Some(azimuth::StringValue("value5")))
  
  // 验证行李条目
  let bag_val1 = azimuth::Baggage::get_entry(baggage_5, "thread.1")
  let bag_val2 = azimuth::Baggage::get_entry(baggage_5, "thread.2")
  let bag_val3 = azimuth::Baggage::get_entry(baggage_5, "thread.3")
  let bag_val4 = azimuth::Baggage::get_entry(baggage_5, "thread.4")
  let bag_val5 = azimuth::Baggage::get_entry(baggage_5, "thread.5")
  
  assert_eq(bag_val1, Some("data1"))
  assert_eq(bag_val2, Some("data2"))
  assert_eq(bag_val3, Some("data3"))
  assert_eq(bag_val4, Some("data4"))
  assert_eq(bag_val5, Some("data5"))
  
  // 测试资源清理
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "resource-test")
  
  // 创建多个Span
  let span1 = azimuth::Tracer::start_span(tracer, "resource.span.1")
  let span2 = azimuth::Tracer::start_span(tracer, "resource.span.2")
  let span3 = azimuth::Tracer::start_span(tracer, "resource.span.3")
  
  // 设置属性
  azimuth::Span::set_attribute(span1, "resource.test", azimuth::StringValue("span1"))
  azimuth::Span::set_attribute(span2, "resource.test", azimuth::StringValue("span2"))
  azimuth::Span::set_attribute(span3, "resource.test", azimuth::StringValue("span3"))
  
  // 结束所有Span
  azimuth::Span::end(span1)
  azimuth::Span::end(span2)
  azimuth::Span::end(span3)
}

// 测试8: 国际化和本地化支持
pub test "国际化和本地化支持" {
  // 测试多语言属性值
  let i18n_attrs = azimuth::Attributes::new()
  
  // 中文支持
  azimuth::Attributes::set(i18n_attrs, "zh.服务名", azimuth::StringValue("方位角遥测系统"))
  azimuth::Attributes::set(i18n_attrs, "zh.描述", azimuth::StringValue("高性能分布式追踪解决方案"))
  azimuth::Attributes::set(i18n_attrs, "zh.状态", azimuth::StringValue("运行正常"))
  
  // 日文支持
  azimuth::Attributes::set(i18n_attrs, "ja.サービス名", azimuth::StringValue("アジマス遠隔測定システム"))
  azimuth::Attributes::set(i18n_attrs, "ja.説明", azimuth::StringValue("高性能分散追跡ソリューション"))
  azimuth::Attributes::set(i18n_attrs, "ja.ステータス", azimuth::StringValue("正常稼働中"))
  
  // 韩文支持
  azimuth::Attributes::set(i18n_attrs, "ko.서비스명", azimuth::StringValue("아지머스 원격 측정 시스템"))
  azimuth::Attributes::set(i18n_attrs, "ko.설명", azimuth::StringValue("고성능 분산 추적 솔루션"))
  
  // 阿拉伯文支持
  azimuth::Attributes::set(i18n_attrs, "ar.اسم_الخدمة", azimuth::StringValue("نظام القياس عن بعد أزيموث"))
  azimuth::Attributes::set(i18n_attrs, "ar.الوصف", azimuth::StringValue("حل تتبع موزع عالي الأداء"))
  
  // 表情符号和特殊符号
  azimuth::Attributes::set(i18n_attrs, "symbols.test", azimuth::StringValue("🚀📊📈🔍⚡🌟"))
  azimuth::Attributes::set(i18n_attrs, "math.symbols", azimuth::StringValue("∑∏∫∆∇∂"))
  azimuth::Attributes::set(i18n_attrs, "currency.symbols", azimuth::StringValue("¥€£¢₹₽₩"))
  
  // 验证多语言属性
  let zh_service = azimuth::Attributes::get(i18n_attrs, "zh.服务名")
  let ja_service = azimuth::Attributes::get(i18n_attrs, "ja.サービス名")
  let ko_service = azimuth::Attributes::get(i18n_attrs, "ko.서비스명")
  let ar_service = azimuth::Attributes::get(i18n_attrs, "ar.اسم_الخدمة")
  let symbols = azimuth::Attributes::get(i18n_attrs, "symbols.test")
  let math_symbols = azimuth::Attributes::get(i18n_attrs, "math.symbols")
  let currency_symbols = azimuth::Attributes::get(i18n_attrs, "currency.symbols")
  
  assert_eq(zh_service, Some(azimuth::StringValue("方位角遥测系统")))
  assert_eq(ja_service, Some(azimuth::StringValue("アジマス遠隔測定システム")))
  assert_eq(ko_service, Some(azimuth::StringValue("아지머스 원격 측정 시스템")))
  assert_eq(ar_service, Some(azimuth::StringValue("نظام القياس عن بعد أزيموث")))
  assert_eq(symbols, Some(azimuth::StringValue("🚀📊📈🔍⚡🌟")))
  assert_eq(math_symbols, Some(azimuth::StringValue("∑∏∫∆∇∂")))
  assert_eq(currency_symbols, Some(azimuth::StringValue("¥€£¢₹₽₩")))
  
  // 测试多语言日志记录
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "i18n-logger")
  
  let zh_log = azimuth::LogRecord::new(azimuth::Info, Some("系统启动完成"))
  let ja_log = azimuth::LogRecord::new(azimuth::Info, Some("システムの起動が完了しました"))
  let ko_log = azimuth::LogRecord::new(azimuth::Info, Some("시스템 시작이 완료되었습니다"))
  let ar_log = azimuth::LogRecord::new(azimuth::Info, Some("اكتمل بدء تشغيل النظام"))
  
  azimuth::Logger::emit(logger, zh_log)
  azimuth::Logger::emit(logger, ja_log)
  azimuth::Logger::emit(logger, ko_log)
  azimuth::Logger::emit(logger, ar_log)
  
  // 验证多语言日志内容
  assert_eq(azimuth::LogRecord::body(zh_log), Some("系统启动完成"))
  assert_eq(azimuth::LogRecord::body(ja_log), Some("システムの起動が完了しました"))
  assert_eq(azimuth::LogRecord::body(ko_log), Some("시스템 시작이 완료되었습니다"))
  assert_eq(azimuth::LogRecord::body(ar_log), Some("اكتمل بدء تشغيل النظام"))
}

// 测试9: 性能基准测试
pub test "性能基准测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 测试属性操作性能
  let attrs = azimuth::Attributes::new()
  
  // 批量设置属性
  for i in 0..1000 {
    let key = "perf.key." + i.to_string()
    let value = "perf.value." + i.to_string()
    azimuth::Attributes::set(attrs, key, azimuth::StringValue(value))
  }
  
  let attr_set_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let attr_set_duration = attr_set_time - start_time
  
  // 批量获取属性
  for i in 0..1000 {
    let key = "perf.key." + i.to_string()
    let value = azimuth::Attributes::get(attrs, key)
    assert_true(value != None)
  }
  
  let attr_get_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let attr_get_duration = attr_get_time - attr_set_time
  
  // 测试Span创建性能
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "perf-test")
  let spans = []
  
  for i in 0..100 {
    let span_name = "perf-span-" + i.to_string()
    let span = azimuth::Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  let span_create_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let span_create_duration = span_create_time - attr_get_time
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let span_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let span_end_duration = span_end_time - span_create_time
  
  // 测试度量操作性能
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "perf-meter")
  let counter = azimuth::Meter::create_counter(meter, "perf.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "perf.histogram")
  
  for i in 0..500 {
    azimuth::Counter::add(counter, i.to_double())
    azimuth::Histogram::record(histogram, i.to_double() * 1.5)
  }
  
  let metrics_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let metrics_duration = metrics_time - span_end_time
  
  let total_time = metrics_time - start_time
  
  // 验证性能在合理范围内
  assert_true(attr_set_duration < 5000000000L)  // 属性设置 < 5秒
  assert_true(attr_get_duration < 3000000000L)  // 属性获取 < 3秒
  assert_true(span_create_duration < 2000000000L)  // Span创建 < 2秒
  assert_true(span_end_duration < 2000000000L)  // Span结束 < 2秒
  assert_true(metrics_duration < 2000000000L)  // 度量操作 < 2秒
  assert_true(total_time < 15000000000L)  // 总时间 < 15秒
}

// 测试10: 端到端遥测工作流
pub test "端到端遥测工作流" {
  // 1. 初始化资源
  let resource = azimuth::Resource::new()
  let resource_attrs = [
    ("service.name", azimuth::StringValue("e2e-test-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("deployment.environment", azimuth::StringValue("staging")),
    ("service.instance.id", azimuth::StringValue("instance-e2e-001"))
  ]
  let service_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  // 2. 创建追踪器和根Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "e2e-test-service", Some("2.1.0"))
  
  let root_span_ctx = azimuth::SpanContext::new("trace-e2e-12345", "span-e2e-root", true, "")
  let root_span = azimuth::Span::new("user.request", azimuth::Server, root_span_ctx)
  
  // 3. 添加请求属性和事件
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("POST"))
  azimuth::Span::set_attribute(root_span, "http.target", azimuth::StringValue("/api/process"))
  azimuth::Span::set_attribute(root_span, "user.id", azimuth::StringValue("user-e2e-789"))
  azimuth::Span::set_attribute(root_span, "client.ip", azimuth::StringValue("192.168.1.100"))
  
  azimuth::Span::add_event(root_span, "authentication.success", Some([("auth.method", azimuth::StringValue("oauth2"))]))
  
  // 4. 创建子Span - 业务逻辑处理
  let business_span_ctx = azimuth::SpanContext::new("trace-e2e-12345", "span-e2e-business", true, "")
  let business_span = azimuth::Span::new("business.logic.process", azimuth::Internal, business_span_ctx)
  
  azimuth::Span::set_attribute(business_span, "business.operation", azimuth::StringValue("data.transformation"))
  azimuth::Span::set_attribute(business_span, "data.size", azimuth::IntValue(1024))
  
  // 5. 创建度量并记录
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "e2e-test-meter", Some("2.1.0"))
  
  let request_counter = azimuth::Meter::create_counter(meter, "requests.total")
  let processing_time_histogram = azimuth::Meter::create_histogram(meter, "processing.time.ms")
  let data_size_histogram = azimuth::Meter::create_histogram(meter, "data.size.bytes")
  
  azimuth::Counter::add(request_counter, 1.0, Some([("endpoint", azimuth::StringValue("/api/process")), ("method", azimuth::StringValue("POST"))]))
  azimuth::Histogram::record(processing_time_histogram, 245.6, Some([("operation", azimuth::StringValue("data.transformation")]))
  azimuth::Histogram::record(data_size_histogram, 1024.0, Some([("operation", azimuth::StringValue("data.transformation")]))
  
  // 6. 创建日志记录器并记录日志
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "e2e-test-logger", Some("2.1.0"))
  
  let info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Business logic processing completed successfully"),
    Some([
      ("operation.type", azimuth::StringValue("data.transformation")),
      ("records.processed", azimuth::IntValue(50)),
      ("processing.time.ms", azimuth::IntValue(245))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-e2e-12345"),
    Some("span-e2e-business"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, info_log)
  
  // 7. 结束业务逻辑Span
  azimuth::Span::set_status(business_span, azimuth::Ok)
  azimuth::Span::end(business_span)
  
  // 8. 创建子Span - 数据库操作
  let db_span_ctx = azimuth::SpanContext::new("trace-e2e-12345", "span-e2e-database", true, "")
  let db_span = azimuth::Span::new("database.operation", azimuth::Client, db_span_ctx)
  
  azimuth::Span::set_attribute(db_span, "db.system", azimuth::StringValue("postgresql"))
  azimuth::Span::set_attribute(db_span, "db.statement", azimuth::StringValue("INSERT INTO processed_data VALUES (...)"))
  azimuth::Span::set_attribute(db_span, "db.operation", azimuth::StringValue("INSERT"))
  
  // 记录数据库度量
  let db_counter = azimuth::Meter::create_counter(meter, "database.operations")
  let db_latency_histogram = azimuth::Meter::create_histogram(meter, "database.latency.ms")
  
  azimuth::Counter::add(db_counter, 1.0, Some([("operation", azimuth::StringValue("INSERT")), ("table", azimuth::StringValue("processed_data"))]))
  azimuth::Histogram::record(db_latency_histogram, 45.2, Some([("operation", azimuth::StringValue("INSERT"))]))
  
  // 记录数据库日志
  let db_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Database operation completed successfully"),
    Some([
      ("db.operation", azimuth::StringValue("INSERT")),
      ("db.table", azimuth::StringValue("processed_data")),
      ("rows.affected", azimuth::IntValue(1)),
      ("db.latency.ms", azimuth::IntValue(45))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("trace-e2e-12345"),
    Some("span-e2e-database"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, db_log)
  
  // 9. 结束数据库Span
  azimuth::Span::set_status(db_span, azimuth::Ok)
  azimuth::Span::end(db_span)
  
  // 10. 结束根Span
  azimuth::Span::add_event(root_span, "response.sent", Some([("status.code", azimuth::IntValue(200)), ("response.size", azimuth::IntValue(512))]))
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::end(root_span)
  
  // 11. 验证所有组件创建正确
  assert_eq(azimuth::Span::name(root_span), "user.request")
  assert_eq(azimuth::Span::kind(root_span), azimuth::Server)
  
  assert_eq(azimuth::Span::name(business_span), "business.logic.process")
  assert_eq(azimuth::Span::kind(business_span), azimuth::Internal)
  
  assert_eq(azimuth::Span::name(db_span), "database.operation")
  assert_eq(azimuth::Span::kind(db_span), azimuth::Client)
  
  assert_eq(request_counter.name, "requests.total")
  assert_eq(processing_time_histogram.name, "processing.time.ms")
  assert_eq(data_size_histogram.name, "data.size.bytes")
  
  assert_eq(logger.scope.name, "e2e-test-logger")
  assert_eq(logger.scope.version, Some("2.1.0"))
  
  // 12. 验证资源属性
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.name"), Some(azimuth::StringValue("e2e-test-service")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "deployment.environment"), Some(azimuth::StringValue("staging")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.instance.id"), Some(azimuth::StringValue("instance-e2e-001")))
}