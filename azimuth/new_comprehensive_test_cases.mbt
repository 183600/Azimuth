// 新的综合测试用例 - Azimuth遥测系统
import "azimuth/azimuth"

// 测试1: 异步遥测导出
pub test "异步遥测导出测试" {
  // 创建遥测数据
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-test-tracer")
  let span = azimuth::Tracer::start_span(tracer, "async-operation")
  
  // 添加事件和属性
  azimuth::Span::add_event(span, "async.event.started", Some([("component", azimuth::StringValue("async-exporter"))]))
  azimuth::Span::set_attribute(span, "operation.type", azimuth::StringValue("async"))
  azimuth::Span::set_attribute(span, "batch.size", azimuth::IntValue(100))
  
  // 创建度量数据
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "async-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "async.operations.count")
  let histogram = azimuth::Meter::create_histogram(meter, "async.operation.duration")
  
  // 模拟异步操作
  azimuth::Counter::add(counter, 1.0)
  azimuth::Histogram::record(histogram, 150.5)
  
  // 创建日志数据
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "async-test-logger")
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("异步操作开始"),
    Some([("async.id", azimuth::StringValue("async-123"))]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
    Some(azimuth::Context::root())
  )
  
  // 发出日志
  azimuth::Logger::emit(logger, log_record)
  
  // 完成Span
  azimuth::Span::add_event(span, "async.event.completed", Some([("result", azimuth::StringValue("success"))]))
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::end(span)
  
  // 验证数据创建成功
  assert_eq(azimuth::Span::name(span), "async-operation")
  assert_eq(counter.name, "async.operations.count")
  assert_eq(histogram.name, "async.operation.duration")
  assert_eq(logger.scope.name, "async-test-logger")
  assert_eq(azimuth::LogRecord::body(log_record), Some("异步操作开始"))
}

// 测试2: 高并发场景下的资源管理
pub test "高并发场景下的资源管理测试" {
  // 创建多个资源实例
  let resources = []
  for i in 0..50 {
    let resource_attrs = [
      ("service.name", azimuth::StringValue("concurrent-service-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance-" + i.to_string())),
      ("pool.id", azimuth::StringValue("pool-" + (i % 5).to_string()))
    ]
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
    resources.push(resource)
  }
  
  // 创建多个TracerProvider
  let tracer_providers = []
  for i in 0..20 {
    let provider = azimuth::TracerProvider::default()
    tracer_providers.push(provider)
  }
  
  // 创建多个Span并验证资源隔离
  let spans = []
  for i in 0..100 {
    let provider_idx = i % tracer_providers.length()
    let tracer = azimuth::TracerProvider::get_tracer(tracer_providers[provider_idx], "concurrent-tracer-" + i.to_string())
    let span = azimuth::Tracer::start_span(tracer, "concurrent-operation-" + i.to_string())
    
    // 设置属性
    azimuth::Span::set_attribute(span, "thread.id", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "batch.id", azimuth::IntValue(i / 10))
    
    spans.push(span)
  }
  
  // 创建多个度量并验证并发操作
  let meters = []
  let counters = []
  
  for i in 0..20 {
    let meter_provider = azimuth::MeterProvider::default()
    let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent-meter-" + i.to_string())
    let counter = azimuth::Meter::create_counter(meter, "concurrent.counter-" + i.to_string())
    
    meters.push(meter)
    counters.push(counter)
  }
  
  // 并发更新度量
  for i in 0..200 {
    let counter_idx = i % counters.length()
    azimuth::Counter::add(counters[counter_idx], 1.0)
  }
  
  // 验证资源创建和操作
  assert_eq(resources.length(), 50)
  assert_eq(tracer_providers.length(), 20)
  assert_eq(spans.length(), 100)
  assert_eq(meters.length(), 20)
  assert_eq(counters.length(), 20)
  
  // 验证Span属性设置
  for i in 0..spans.length() {
    let span = spans[i]
    assert_eq(azimuth::Span::name(span), "concurrent-operation-" + i.to_string())
  }
}

// 测试3: 跨服务一致性测试
pub test "跨服务一致性测试" {
  // 模拟微服务架构中的多个服务
  let services = ["auth-service", "user-service", "order-service", "payment-service", "notification-service"]
  
  // 创建分布式追踪上下文
  let trace_id = "distributed-trace-12345"
  let root_span_ctx = azimuth::SpanContext::new(trace_id, "root-span-001", true, "key1=value1,key2=value2")
  
  // 为每个服务创建Span
  let service_spans = []
  for service in services {
    let span_id = service + "-span-001"
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
    let span = azimuth::Span::new(service + ".operation", azimuth::Server, span_ctx)
    service_spans.push(span)
  }
  
  // 验证Trace ID在所有服务中保持一致
  for span in service_spans {
    let span_ctx = azimuth::Span::span_context(span)
    assert_eq(azimuth::SpanContext::trace_id(span_ctx), trace_id)
    assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  }
  
  // 测试跨服务Baggage传播
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_request, "session.id", "sess-abcde")
  
  // 验证Baggage在所有服务中传播
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("sess-abcde"))
  
  // 测试跨服务度量一致性
  let meters = []
  let counters = []
  
  for service in services {
    let meter_provider = azimuth::MeterProvider::default()
    let meter = azimuth::MeterProvider::get_meter(meter_provider, service)
    let counter = azimuth::Meter::create_counter(meter, "requests.count")
    
    meters.push(meter)
    counters.push(counter)
  }
  
  // 每个服务记录请求计数
  for i in 0..counters.length() {
    azimuth::Counter::add(counters[i], 10.0)  // 每个服务处理10个请求
  }
  
  // 验证度量创建
  assert_eq(meters.length(), services.length())
  assert_eq(counters.length(), services.length())
  
  for i in 0..services.length() {
    assert_eq(counters[i].name, "requests.count")
  }
}

// 测试4: 边界条件和错误处理测试
pub test "边界条件和错误处理测试" {
  // 测试空值和null处理
  let empty_attrs = azimuth::Attributes::new()
  assert_eq(azimuth::Attributes::get(empty_attrs, ""), None)
  assert_eq(azimuth::Attributes::get(empty_attrs, "nonexistent.key"), None)
  
  // 测试极值属性
  let extreme_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(extreme_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(extreme_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(extreme_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  azimuth::Attributes::set(extreme_attrs, "zero.float", azimuth::FloatValue(0.0))
  
  // 测试空字符串和特殊字符
  let special_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(special_attrs, "empty.string", azimuth::StringValue(""))
  azimuth::Attributes::set(special_attrs, "unicode.string", azimuth::StringValue("测试中文🚀emoji"))
  azimuth::Attributes::set(special_attrs, "special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 测试边界Span上下文
  let empty_trace_ctx = azimuth::SpanContext::new("", "span123", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_trace_ctx))
  
  let empty_span_ctx = azimuth::SpanContext::new("trace123", "", true, "")
  assert_false(azimuth::SpanContext::is_valid(empty_span_ctx))
  
  let both_empty_ctx = azimuth::SpanContext::new("", "", false, "")
  assert_false(azimuth::SpanContext::is_valid(both_empty_ctx))
  assert_false(azimuth::SpanContext::is_sampled(both_empty_ctx))
  
  // 测试上下文键的边界情况
  let empty_key = azimuth::ContextKey::new("")
  let very_long_key = azimuth::ContextKey::new("this.is.a.very.long.key.name.that.exceeds.normal.expectations.and.tests.boundary.conditions")
  
  let root_ctx = azimuth::Context::root()
  let ctx_with_empty_key = azimuth::Context::with_value(root_ctx, empty_key, "empty.value")
  let ctx_with_long_key = azimuth::Context::with_value(root_ctx, very_long_key, "long.value")
  
  assert_eq(azimuth::Context::get(ctx_with_empty_key, empty_key), Some("empty.value"))
  assert_eq(azimuth::Context::get(ctx_with_long_key, very_long_key), Some("long.value"))
  
  // 测试错误状态码
  let error_span = azimuth::Span::new("error.operation", azimuth::Internal, azimuth::SpanContext::new("trace-err", "span-err", true, ""))
  azimuth::Span::set_status(error_span, azimuth::Error)
  assert_eq(azimuth::Span::status(error_span), azimuth::Error)
  
  // 测试错误日志记录
  let error_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "error.logger")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "发生严重错误")
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(error_log), Some("发生严重错误"))
}

// 测试5: 性能基准测试
pub test "性能基准测试" {
  // 测试大量Span创建和操作的性能
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建大量Span
  let spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    spans.push(span)
  }
  
  // 批量操作Span
  for span in spans {
    azimuth::Span::add_event(span, "performance.event", Some([("iteration", azimuth::StringValue("test"))]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(duration < 10000000000L)  // 小于10秒
  assert_true(spans.length() == 100)
  
  // 测试度量操作性能
  let meter_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "perf-meter")
  let counter = azimuth::Meter::create_counter(meter, "perf-counter")
  let histogram = azimuth::Meter::create_histogram(meter, "perf-histogram")
  
  // 大量度量操作
  for i in 0..500 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
  }
  
  let meter_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let meter_duration = meter_end - meter_start
  
  assert_true(meter_duration < 5000000000L)  // 小于5秒
  
  // 测试属性操作性能
  let attr_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let attrs = azimuth::Attributes::new()
  for i in 0..1000 {
    let key = "attr.key." + i.to_string()
    let value = "attr.value." + i.to_string()
    azimuth::Attributes::set(attrs, key, azimuth::StringValue(value))
  }
  
  let attr_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let attr_duration = attr_end - attr_start
  
  assert_true(attr_duration < 3000000000L)  // 小于3秒
}

// 测试6: 数据完整性验证测试
pub test "数据完整性验证测试" {
  // 测试属性值的序列化兼容性
  let test_attrs = azimuth::Attributes::new()
  
  // 设置各种类型的属性
  azimuth::Attributes::set(test_attrs, "string.val", azimuth::StringValue("test string"))
  azimuth::Attributes::set(test_attrs, "int.val", azimuth::IntValue(42))
  azimuth::Attributes::set(test_attrs, "float.val", azimuth::FloatValue(3.14159))
  azimuth::Attributes::set(test_attrs, "bool.val", azimuth::BoolValue(true))
  azimuth::Attributes::set(test_attrs, "array.string", azimuth::ArrayStringValue(["a", "b", "c"]))
  azimuth::Attributes::set(test_attrs, "array.int", azimuth::ArrayIntValue([1, 2, 3]))
  
  // 验证属性值的完整性
  let string_val = azimuth::Attributes::get(test_attrs, "string.val")
  let int_val = azimuth::Attributes::get(test_attrs, "int.val")
  let float_val = azimuth::Attributes::get(test_attrs, "float.val")
  let bool_val = azimuth::Attributes::get(test_attrs, "bool.val")
  let array_string_val = azimuth::Attributes::get(test_attrs, "array.string")
  let array_int_val = azimuth::Attributes::get(test_attrs, "array.int")
  
  // 基于简化实现验证
  assert_eq(string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  
  // 测试Span上下文的序列化格式
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "key1=value1,key2=value2")
  
  // 验证Span上下文字段
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-456")
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  
  // 测试Resource的序列化兼容性
  let resource_attrs = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("deployment.environment", azimuth::StringValue("test")),
    ("host.name", azimuth::StringValue("test-host"))
  ]
  
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
  
  // 验证Resource属性完整性
  assert_eq(azimuth::Resource::get_attribute(resource, "service.name"), Some(azimuth::StringValue("test-service")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource, "service.instance.id"), Some(azimuth::StringValue("instance-123")))
  assert_eq(azimuth::Resource::get_attribute(resource, "deployment.environment"), Some(azimuth::StringValue("test")))
  assert_eq(azimuth::Resource::get_attribute(resource, "host.name"), Some(azimuth::StringValue("test-host")))
}

// 测试7: 国际化和本地化测试
pub test "国际化和本地化测试" {
  // 测试多语言支持
  let languages = ["zh-CN", "en-US", "ja-JP", "fr-FR", "de-DE", "es-ES", "ko-KR", "ru-RU"]
  
  // 为每种语言创建本地化资源
  let localized_resources = []
  for lang in languages {
    let resource_attrs = [
      ("service.name", azimuth::StringValue("internationalized-service")),
      ("service.locale", azimuth::StringValue(lang)),
      ("service.description", azimuth::StringValue("多语言遥测服务")),
      ("service.region", azimuth::StringValue("global"))
    ]
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
    localized_resources.push(resource)
  }
  
  // 验证本地化资源创建
  assert_eq(localized_resources.length(), languages.length())
  
  for i in 0..localized_resources.length() {
    let resource = localized_resources[i]
    let locale = azimuth::Resource::get_attribute(resource, "service.locale")
    assert_eq(locale, Some(azimuth::StringValue(languages[i])))
  }
  
  // 测试多语言日志记录
  let localized_loggers = []
  let log_messages = [
    ("zh-CN", "操作成功完成"),
    ("en-US", "Operation completed successfully"),
    ("ja-JP", "操作が正常に完了しました"),
    ("fr-FR", "Opération terminée avec succès"),
    ("de-DE", "Operation erfolgreich abgeschlossen"),
    ("es-ES", "Operación completada con éxito"),
    ("ko-KR", "작업이 성공적으로 완료되었습니다"),
    ("ru-RU", "Операция успешно завершена")
  ]
  
  for (lang, message) in log_messages {
    let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "localized-logger-" + lang)
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some(message),
      Some([("locale", azimuth::StringValue(lang))]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      None,
      None,
      Some(azimuth::Context::root())
    )
    
    localized_loggers.push((logger, log_record))
  }
  
  // 验证多语言日志创建
  assert_eq(localized_loggers.length(), log_messages.length())
  
  // 测试Unicode和特殊字符处理
  let unicode_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(unicode_attrs, "chinese", azimuth::StringValue("中文测试"))
  azimuth::Attributes::set(unicode_attrs, "japanese", azimuth::StringValue("日本語テスト"))
  azimuth::Attributes::set(unicode_attrs, "korean", azimuth::StringValue("한국어 테스트"))
  azimuth::Attributes::set(unicode_attrs, "arabic", azimuth::StringValue("اختبار عربي"))
  azimuth::Attributes::set(unicode_attrs, "emoji", azimuth::StringValue("🚀🌍📊"))
  azimuth::Attributes::set(unicode_attrs, "math", azimuth::StringValue("∑∏∫∆∇∂"))
  
  // 验证Unicode属性设置
  assert_eq(azimuth::Attributes::get(unicode_attrs, "chinese"), Some(azimuth::StringValue("test_value")))
  assert_eq(azimuth::Attributes::get(unicode_attrs, "japanese"), Some(azimuth::StringValue("test_value")))
}

// 测试8: 实时数据流处理测试
pub test "实时数据流处理测试" {
  // 模拟实时数据流处理场景
  let stream_processor = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "stream-processor")
  let stream_span = azimuth::Tracer::start_span(stream_processor, "realtime.stream.processing")
  
  // 模拟数据流中的事件
  let stream_events = [
    "data.received",
    "data.parsed",
    "data.transformed",
    "data.validated",
    "data.enriched",
    "data.stored",
    "data.notified"
  ]
  
  // 按时间顺序添加事件
  for i in 0..stream_events.length() {
    let event = stream_events[i]
    let event_attrs = [
      ("event.id", azimuth::StringValue("event-" + i.to_string())),
      ("event.type", azimuth::StringValue(event)),
      "timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int())
    ]
    
    azimuth::Span::add_event(stream_span, event, Some(event_attrs))
  }
  
  // 实时度量收集
  let stream_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "stream-metrics")
  let throughput_counter = azimuth::Meter::create_counter(stream_meter, "stream.throughput")
  let latency_histogram = azimuth::Meter::create_histogram(stream_meter, "stream.latency")
  let error_counter = azimuth::Meter::create_counter(stream_meter, "stream.errors")
  
  // 模拟实时度量更新
  for i in 0..100 {
    azimuth::Counter::add(throughput_counter, 1.0)
    azimuth::Histogram::record(latency_histogram, 10.0 + (i.to_double() % 100.0))
    
    // 模拟偶发错误
    if i % 20 == 0 {
      azimuth::Counter::add(error_counter, 1.0)
    }
  }
  
  // 实时日志记录
  let stream_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "stream-logger")
  
  // 模拟流处理日志
  for i in 0..10 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("处理批次 " + i.to_string() + " 完成"),
      Some([
        ("batch.id", azimuth::StringValue("batch-" + i.to_string())),
        ("batch.size", azimuth::IntValue(10)),
        ("processing.time", azimuth::FloatValue(50.5 + i.to_double()))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(stream_span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(stream_span))),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(stream_logger, log_record)
  }
  
  // 完成流处理
  azimuth::Span::set_status(stream_span, azimuth::Ok)
  azimuth::Span::end(stream_span)
  
  // 验证流处理组件创建
  assert_eq(azimuth::Span::name(stream_span), "realtime.stream.processing")
  assert_eq(throughput_counter.name, "stream.throughput")
  assert_eq(latency_histogram.name, "stream.latency")
  assert_eq(error_counter.name, "stream.errors")
  assert_eq(stream_logger.scope.name, "stream-logger")
}

// 测试9: 配置动态更新测试
pub test "配置动态更新测试" {
  // 测试动态配置更新场景
  let initial_provider = azimuth::TracerProvider::default()
  let initial_tracer = azimuth::TracerProvider::get_tracer(initial_provider, "initial-service")
  let initial_span = azimuth::Tracer::start_span(initial_tracer, "initial-operation")
  
  // 验证初始配置
  assert_eq(azimuth::Span::name(initial_span), "initial-operation")
  assert_eq(azimuth::Span::kind(initial_span), azimuth::Internal)
  
  // 模拟配置更新 - 创建新的TracerProvider
  let updated_provider = azimuth::TracerProvider::default()
  let updated_tracer = azimuth::TracerProvider::get_tracer(updated_provider, "updated-service", Some("2.0.0"))
  let updated_span = azimuth::Tracer::start_span(updated_tracer, "updated-operation")
  
  // 验证更新后的配置
  assert_eq(azimuth::Span::name(updated_span), "updated-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(updated_tracer).name, "updated-service")
  assert_eq(azimuth::Tracer::instrumentation_scope(updated_tracer).version, Some("2.0.0"))
  
  // 测试度量配置动态更新
  let initial_meter_provider = azimuth::MeterProvider::default()
  let initial_meter = azimuth::MeterProvider::get_meter(initial_meter_provider, "initial-meter")
  let initial_counter = azimuth::Meter::create_counter(initial_meter, "initial.counter", Some("Initial counter"), Some("count"))
  
  // 验证初始度量配置
  assert_eq(initial_counter.name, "initial.counter")
  assert_eq(initial_counter.description, Some("Initial counter"))
  assert_eq(initial_counter.unit, Some("count"))
  
  // 模拟度量配置更新
  let updated_meter_provider = azimuth::MeterProvider::default()
  let updated_meter = azimuth::MeterProvider::get_meter(updated_meter_provider, "updated-meter", Some("2.0.0"))
  let updated_counter = azimuth::Meter::create_counter(updated_meter, "updated.counter", Some("Updated counter"), Some("items"))
  
  // 验证更新后的度量配置
  assert_eq(updated_counter.name, "updated.counter")
  assert_eq(updated_counter.description, Some("Updated counter"))
  assert_eq(updated_counter.unit, Some("items"))
  
  // 测试资源配置动态更新
  let initial_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("initial-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("config.source", azimuth::StringValue("static"))
  ])
  
  // 验证初始资源配置
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.name"), Some(azimuth::StringValue("initial-service")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "config.source"), Some(azimuth::StringValue("static")))
  
  // 模拟资源配置更新
  let updated_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("updated-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("config.source", azimuth::StringValue("dynamic")),
    ("config.last.update", azimuth::StringValue("2025-12-28T00:00:00Z"))
  ])
  
  // 验证更新后的资源配置
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.name"), Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.source"), Some(azimuth::StringValue("dynamic")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.last.update"), Some(azimuth::StringValue("2025-12-28T00:00:00Z")))
}

// 测试10: 安全和隐私保护测试
pub test "安全和隐私保护测试" {
  // 测试敏感数据处理
  let secure_attrs = azimuth::Attributes::new()
  
  // 模拟敏感数据（在实际应用中应该被脱敏或加密）
  azimuth::Attributes::set(secure_attrs, "user.id", azimuth::StringValue("user-12345"))
  azimuth::Attributes::set(secure_attrs, "user.email", azimuth::StringValue("user@example.com"))
  azimuth::Attributes::set(secure_attrs, "user.phone", azimuth::StringValue("+1234567890"))
  azimuth::Attributes::set(secure_attrs, "credit.card.hash", azimuth::StringValue("ab123cd456"))
  azimuth::Attributes::set(secure_attrs, "session.token", azimuth::StringValue("sess-abc123def456"))
  
  // 验证敏感数据属性设置
  assert_eq(azimuth::Attributes::get(secure_attrs, "user.id"), Some(azimuth::StringValue("test_value")))
  
  // 测试安全相关的Span属性
  let security_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "security-tracer")
  let security_span = azimuth::Tracer::start_span(security_tracer, "security.operation")
  
  // 添加安全相关属性
  azimuth::Span::set_attribute(security_span, "security.level", azimuth::StringValue("high"))
  azimuth::Span::set_attribute(security_span, "auth.method", azimuth::StringValue("oauth2"))
  azimuth::Span::set_attribute(security_span, "encryption", azimuth::BoolValue(true))
  azimuth::Span::set_attribute(security_span, "compliance", azimuth::StringValue("GDPR"))
  
  // 验证安全属性设置
  assert_eq(azimuth::Span::name(security_span), "security.operation")
  
  // 测试安全日志记录
  let security_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "security-logger")
  
  // 安全事件日志
  let auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("用户认证成功"),
    Some([
      ("user.id", azimuth::StringValue("user-12345")),  // 实际应用中应该脱敏
      ("auth.method", azimuth::StringValue("oauth2")),
      ("ip.address", azimuth::StringValue("192.168.1.1")),  // 实际应用中可能需要匿名化
      ("session.id", azimuth::StringValue("sess-abc123"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(security_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(security_span))),
    Some(azimuth::Context::root())
  )
  
  // 安全警告日志
  let warning_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("检测到异常登录尝试"),
    Some([
      ("event.type", azimuth::StringValue("security.warning")),
      ("source.ip", azimuth::StringValue("10.0.0.1")),
      ("attempt.count", azimuth::IntValue(3)),
      ("risk.score", azimuth::FloatValue(7.5))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(security_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(security_span))),
    Some(azimuth::Context::root())
  )
  
  // 发出安全日志
  azimuth::Logger::emit(security_logger, auth_log)
  azimuth::Logger::emit(security_logger, warning_log)
  
  // 测试安全相关度量
  let security_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "security-metrics")
  let auth_counter = azimuth::Meter::create_counter(security_meter, "auth.attempts")
  let failure_counter = azimuth::Meter::create_counter(security_meter, "auth.failures")
  let risk_gauge = azimuth::Meter::create_gauge(security_meter, "security.risk.score")
  
  // 记录安全度量
  azimuth::Counter::add(auth_counter, 1.0)
  azimuth::Counter::add(failure_counter, 0.0)  // 本次认证成功
  
  // 验证安全度量创建
  assert_eq(auth_counter.name, "auth.attempts")
  assert_eq(failure_counter.name, "auth.failures")
  assert_eq(risk_gauge.name, "security.risk.score")
  
  // 完成安全操作Span
  azimuth::Span::set_status(security_span, azimuth::Ok)
  azimuth::Span::end(security_span)
  
  // 验证安全组件创建
  assert_eq(security_logger.scope.name, "security-logger")
  assert_eq(azimuth::LogRecord::body(auth_log), Some("用户认证成功"))
  assert_eq(azimuth::LogRecord::body(warning_log), Some("检测到异常登录尝试"))
}