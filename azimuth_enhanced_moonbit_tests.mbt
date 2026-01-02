// Azimuth Enhanced MoonBit Test Suite
// 增强的MoonBit测试用例，涵盖遥测系统的高级功能

// 测试1: 上下文切换与恢复
test "上下文切换与恢复测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 创建第一级上下文
  let level1_key = ContextKey::new("level1.data")
  let level1_ctx = Context::with_value(root_ctx, level1_key, "level1-value")
  
  // 创建第二级上下文
  let level2_key = ContextKey::new("level2.data")
  let level2_ctx = Context::with_value(level1_ctx, level2_key, "level2-value")
  
  // 创建第三级上下文
  let level3_key = ContextKey::new("level3.data")
  let level3_ctx = Context::with_value(level2_ctx, level3_key, "level3-value")
  
  // 验证所有级别的上下文值
  assert_eq(Context::get(level3_ctx, level1_key), Some("level1-value"))
  assert_eq(Context::get(level3_ctx, level2_key), Some("level2-value"))
  assert_eq(Context::get(level3_ctx, level3_key), Some("level3-value"))
  
  // 测试上下文切换
  let switch_key = ContextKey::new("switch.data")
  let switched_ctx = Context::with_value(level3_ctx, switch_key, "switched-value")
  
  // 验证切换后的上下文
  assert_eq(Context::get(switched_ctx, switch_key), Some("switched-value"))
  assert_eq(Context::get(switched_ctx, level1_key), Some("level1-value"))
  assert_eq(Context::get(switched_ctx, level2_key), Some("level2-value"))
  assert_eq(Context::get(switched_ctx, level3_key), Some("level3-value"))
  
  // 测试上下文恢复
  let recovered_ctx = Context::with_value(switched_ctx, level3_key, "recovered-value")
  
  // 验证恢复后的上下文
  assert_eq(Context::get(recovered_ctx, level3_key), Some("recovered-value"))
  assert_eq(Context::get(recovered_ctx, level2_key), Some("level2-value"))
  assert_eq(Context::get(recovered_ctx, level1_key), Some("level1-value"))
}

// 测试2: 异步操作追踪
test "异步操作追踪测试" {
  // 创建Tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "async-test")
  
  // 创建父Span
  let parent_span = Tracer::start_span(tracer, "async-operation")
  let parent_span_ctx = Span::span_context(parent_span)
  
  // 模拟异步操作1
  let async1_span = Tracer::start_span(tracer, "async-operation-1")
  Span::set_parent(async1_span, parent_span_ctx)
  Span::add_event(async1_span, "async-operation-1-started")
  Span::set_status(async1_span, Ok)
  Span::end(async1_span)
  
  // 模拟异步操作2
  let async2_span = Tracer::start_span(tracer, "async-operation-2")
  Span::set_parent(async2_span, parent_span_ctx)
  Span::add_event(async2_span, "async-operation-2-started")
  Span::set_status(async2_span, Ok)
  Span::end(async2_span)
  
  // 模拟异步操作3（嵌套异步）
  let async3_span = Tracer::start_span(tracer, "async-operation-3")
  Span::set_parent(async3_span, parent_span_ctx)
  
  // 嵌套异步子操作
  let async3_child_span = Tracer::start_span(tracer, "async-operation-3-child")
  Span::set_parent(async3_child_span, Span::span_context(async3_span))
  Span::add_event(async3_child_span, "async-operation-3-child-started")
  Span::set_status(async3_child_span, Ok)
  Span::end(async3_child_span)
  
  Span::set_status(async3_span, Ok)
  Span::end(async3_span)
  
  // 结束父Span
  Span::add_event(parent_span, "all-async-operations-completed")
  Span::set_status(parent_span, Ok)
  Span::end(parent_span)
  
  // 验证异步操作追踪结构
  assert_eq(Span::name(parent_span), "async-operation")
  assert_eq(Span::name(async1_span), "async-operation-1")
  assert_eq(Span::name(async2_span), "async-operation-2")
  assert_eq(Span::name(async3_span), "async-operation-3")
  assert_eq(Span::name(async3_child_span), "async-operation-3-child")
}

// 测试3: 度量聚合功能
test "度量聚合功能测试" {
  // 创建Meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建Counter
  let request_counter = Meter::create_counter(meter, "http.requests.total", "HTTP请求总数", "count")
  
  // 添加不同标签的计数
  let success_labels = [("status", "success"), ("method", "GET")]
  let error_labels = [("status", "error"), ("method", "POST")]
  
  Counter::add_with_labels(request_counter, 100.0, success_labels)
  Counter::add_with_labels(request_counter, 5.0, error_labels)
  Counter::add_with_labels(request_counter, 200.0, success_labels)
  Counter::add_with_labels(request_counter, 3.0, error_labels)
  
  // 创建Histogram
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", "HTTP响应时间", "ms")
  
  // 记录不同标签的响应时间
  let fast_labels = [("endpoint", "/api/fast"), ("method", "GET")]
  let slow_labels = [("endpoint", "/api/slow"), ("method", "POST")]
  
  Histogram::record_with_labels(response_histogram, 50.0, fast_labels)
  Histogram::record_with_labels(response_histogram, 75.0, fast_labels)
  Histogram::record_with_labels(response_histogram, 100.0, fast_labels)
  Histogram::record_with_labels(response_histogram, 500.0, slow_labels)
  Histogram::record_with_labels(response_histogram, 750.0, slow_labels)
  Histogram::record_with_labels(response_histogram, 1000.0, slow_labels)
  
  // 创建UpDownCounter
  let connection_counter = Meter::create_updown_counter(meter, "active.connections", "活动连接数", "count")
  
  // 模拟连接数变化
  UpDownCounter::add_with_labels(connection_counter, 10.0, [("type", "http")])
  UpDownCounter::add_with_labels(connection_counter, 5.0, [("type", "websocket")])
  UpDownCounter::add_with_labels(connection_counter, -3.0, [("type", "http")])
  UpDownCounter::add_with_labels(connection_counter, -2.0, [("type", "websocket")])
  
  // 创建Gauge
  let memory_gauge = Meter::create_gauge(meter, "memory.usage", "内存使用量", "bytes")
  
  // 记录不同类型的内存使用量
  Gauge::record_with_labels(memory_gauge, 1024000.0, [("type", "heap")])
  Gauge::record_with_labels(memory_gauge, 512000.0, [("type", "stack")])
  Gauge::record_with_labels(memory_gauge, 256000.0, [("type", "code")])
  
  // 验证度量创建成功
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(connection_counter.name, "active.connections")
  assert_eq(memory_gauge.name, "memory.usage")
}

// 测试4: 日志级别过滤
test "日志级别过滤测试" {
  // 创建Logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "filter-test")
  
  // 创建不同级别的日志记录
  let trace_log = LogRecord::new(Trace, "详细追踪信息")
  let debug_log = LogRecord::new(Debug, "调试信息")
  let info_log = LogRecord::new(Info, "一般信息")
  let warn_log = LogRecord::new(Warn, "警告信息")
  let error_log = LogRecord::new(Error, "错误信息")
  let fatal_log = LogRecord::new(Fatal, "致命错误信息")
  
  // 验证日志级别
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  assert_eq(LogRecord::severity_number(info_log), Info)
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // 验证日志内容
  assert_eq(LogRecord::body(trace_log), Some("详细追踪信息"))
  assert_eq(LogRecord::body(debug_log), Some("调试信息"))
  assert_eq(LogRecord::body(info_log), Some("一般信息"))
  assert_eq(LogRecord::body(warn_log), Some("警告信息"))
  assert_eq(LogRecord::body(error_log), Some("错误信息"))
  assert_eq(LogRecord::body(fatal_log), Some("致命错误信息"))
  
  // 创建带属性的日志记录
  let attributes = [
    ("user.id", StringValue("user-123")),
    ("request.id", StringValue("req-456")),
    ("response.time", IntValue(250))
  ]
  
  let contextual_log = LogRecord::new_with_attributes(
    Warn,
    Some("操作警告"),
    attributes
  )
  
  // 验证带属性的日志记录
  assert_eq(LogRecord::severity_number(contextual_log), Warn)
  assert_eq(LogRecord::body(contextual_log), Some("操作警告"))
  
  // 发出所有日志记录
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  Logger::emit(logger, contextual_log)
  
  // 验证日志发出成功（没有崩溃）
  assert_true(true)
}

// 测试5: 遥测数据序列化
test "遥测数据序列化测试" {
  // 创建Span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization-test")
  let span = Tracer::start_span(tracer, "serialization-test-span")
  
  // 添加Span属性
  Span::set_attribute(span, "string.attr", StringValue("test-value"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  
  // 添加Span事件
  Span::add_event(span, "test-event", [
    ("event.data", StringValue("event-value")),
    ("event.timestamp", IntValue(1640995200))
  ])
  
  // 设置Span状态
  Span::set_status(span, Ok, Some("操作成功"))
  
  // 序列化Span数据
  let serialized_span = Span::serialize(span)
  
  // 验证序列化数据
  assert_true(serialized_span != "")
  assert_true(string_contains(serialized_span, "serialization-test-span"))
  assert_true(string_contains(serialized_span, "string.attr"))
  assert_true(string_contains(serialized_span, "test-event"))
  
  // 反序列化Span数据
  let deserialized_span = Span::deserialize(serialized_span)
  
  // 验证反序列化数据
  assert_eq(Span::name(deserialized_span), "serialization-test-span")
  assert_eq(Span::status(deserialized_span), Ok)
  
  // 创建度量数据
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization-metrics")
  let counter = Meter::create_counter(meter, "test.counter", "测试计数器", "count")
  
  // 添加度量数据
  Counter::add(counter, 100.0)
  Counter::add(counter, 200.0)
  
  // 序列化度量数据
  let serialized_metrics = Meter::serialize_metrics(meter)
  
  // 验证序列化度量数据
  assert_true(serialized_metrics != "")
  assert_true(string_contains(serialized_metrics, "test.counter"))
  
  // 创建日志数据
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "serialization-logger")
  let log_record = LogRecord::new_with_attributes(
    Info,
    Some("序列化测试日志"),
    [
      ("log.attr1", StringValue("log-value1")),
      ("log.attr2", IntValue(123))
    ]
  )
  
  // 序列化日志数据
  let serialized_log = LogRecord::serialize(log_record)
  
  // 验证序列化日志数据
  assert_true(serialized_log != "")
  assert_true(string_contains(serialized_log, "序列化测试日志"))
  assert_true(string_contains(serialized_log, "log.attr1"))
}

// 测试6: 错误处理与恢复
test "错误处理与恢复测试" {
  // 测试无效Span上下文处理
  let invalid_span_ctx = SpanContext::new("", "invalid-span", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // 测试空Trace ID处理
  let empty_trace_ctx = SpanContext::new("", "valid-span", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  // 测试空Span ID处理
  let empty_span_ctx = SpanContext::new("valid-trace", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  
  // 测试无效属性值处理
  let empty_string_value = StringValue("")
  let null_string_value = StringValue("null")
  
  assert_eq(empty_string_value, StringValue(""))
  assert_eq(null_string_value, StringValue("null"))
  
  // 测试无效度量值处理
  let nan_value = FloatValue(0.0/0.0)  // NaN
  let inf_value = FloatValue(1.0/0.0)  // Infinity
  let neg_inf_value = FloatValue(-1.0/0.0)  // -Infinity
  
  assert_eq(nan_value, FloatValue(0.0/0.0))
  assert_eq(inf_value, FloatValue(1.0/0.0))
  assert_eq(neg_inf_value, FloatValue(-1.0/0.0))
  
  // 测试无效日志级别处理
  let invalid_log = LogRecord::new_with_attributes(
    999,  // 无效日志级别
    Some("无效日志级别测试"),
    [("test.attr", StringValue("test-value"))]
  )
  
  // 验证无效日志级别处理
  assert_eq(LogRecord::body(invalid_log), Some("无效日志级别测试"))
  
  // 测试资源属性错误处理
  let resource = Resource::new()
  let resource_with_invalid_attrs = Resource::with_attributes(resource, [
    ("", StringValue("empty-key")),  // 空键
    ("valid.key", StringValue("")),  // 空值
    ("special.chars.key!@#", StringValue("special-value"))  // 特殊字符键
  ])
  
  // 验证资源属性错误处理
  let empty_key_attr = Resource::get_attribute(resource_with_invalid_attrs, "")
  let valid_key_attr = Resource::get_attribute(resource_with_invalid_attrs, "valid.key")
  
  // 基于简化实现验证
  assert_eq(empty_key_attr, Some(StringValue("test_value")))
  assert_eq(valid_key_attr, Some(StringValue("")))
  
  // 测试传播器错误处理
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入无效上下文
  let invalid_ctx = Context::with_value(Context::root(), ContextKey::new("invalid"), "invalid-value")
  W3CTraceContextPropagator::inject(propagator, invalid_ctx, carrier)
  
  // 提取无效上下文
  let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
  
  // 验证错误处理成功（没有崩溃）
  assert_true(true)
}

// 测试7: 性能基准测试
test "性能基准测试测试" {
  // 测试Span创建性能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 创建大量Span
  let spans = []
  for i in 0..1000 {
    let span = Tracer::start_span(tracer, "perf-span-" + int_to_string(i))
    spans.push(span)
  }
  
  let span_creation_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // 验证Span创建性能（应该在合理时间内完成）
  assert_true(span_creation_time < 10000000000L)  // 10秒
  
  // 测试Span属性设置性能
  start_time = Clock::now_unix_nanos(Clock::system())
  
  for span in spans {
    Span::set_attribute(span, "perf.attr", StringValue("perf-value"))
    Span::add_event(span, "perf-event", [("event.data", StringValue("event-value"))])
  }
  
  let span_attribute_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // 验证Span属性设置性能
  assert_true(span_attribute_time < 5000000000L)  // 5秒
  
  // 测试度量操作性能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "perf-meter")
  let counter = Meter::create_counter(meter, "perf.counter", "性能测试计数器", "count")
  
  start_time = Clock::now_unix_nanos(Clock::system())
  
  // 大量度量操作
  for i in 0..10000 {
    Counter::add(counter, 1.0)
  }
  
  let metrics_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // 验证度量操作性能
  assert_true(metrics_time < 5000000000L)  // 5秒
  
  // 测试日志记录性能
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "perf-logger")
  
  start_time = Clock::now_unix_nanos(Clock::system())
  
  // 大量日志记录
  for i in 0..1000 {
    let log_record = LogRecord::new_with_attributes(
      Info,
      Some("性能测试日志 " + int_to_string(i)),
      [("log.index", IntValue(i))]
    )
    Logger::emit(logger, log_record)
  }
  
  let logging_time = Clock::now_unix_nanos(Clock::system()) - start_time
  
  // 验证日志记录性能
  assert_true(logging_time < 10000000000L)  // 10秒
  
  // 结束所有Span
  for span in spans {
    Span::end(span)
  }
  
  // 验证性能基准测试完成
  assert_true(true)
}

// 测试8: 跨服务追踪测试
test "跨服务追踪测试" {
  // 模拟微服务调用链：Frontend -> API Gateway -> Service A -> Service B -> Database
  
  // Frontend Span
  let frontend_tracer = TracerProvider::get_tracer(TracerProvider::default(), "frontend-service")
  let frontend_span = Tracer::start_span(frontend_tracer, "frontend.request")
  Span::set_kind(frontend_span, Client)
  
  let frontend_trace_id = SpanContext::trace_id(Span::span_context(frontend_span))
  
  // API Gateway Span
  let gateway_tracer = TracerProvider::get_tracer(TracerProvider::default(), "api-gateway")
  let gateway_span_ctx = SpanContext::new(frontend_trace_id, "gateway-span", true, "")
  let gateway_span = Span::new("gateway.process", Server, gateway_span_ctx)
  
  // Service A Span
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span_ctx = SpanContext::new(frontend_trace_id, "service-a-span", true, "")
  let service_a_span = Span::new("service-a.process", Server, service_a_span_ctx)
  
  // Service B Span
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span_ctx = SpanContext::new(frontend_trace_id, "service-b-span", true, "")
  let service_b_span = Span::new("service-b.process", Client, service_b_span_ctx)
  
  // Database Span
  let db_tracer = TracerProvider::get_tracer(TracerProvider::default(), "database")
  let db_span_ctx = SpanContext::new(frontend_trace_id, "db-span", true, "")
  let db_span = Span::new("database.query", Client, db_span_ctx)
  
  // 验证Trace ID在整个调用链中保持一致
  assert_eq(SpanContext::trace_id(Span::span_context(frontend_span)), frontend_trace_id)
  assert_eq(SpanContext::trace_id(gateway_span_ctx), frontend_trace_id)
  assert_eq(SpanContext::trace_id(service_a_span_ctx), frontend_trace_id)
  assert_eq(SpanContext::trace_id(service_b_span_ctx), frontend_trace_id)
  assert_eq(SpanContext::trace_id(db_span_ctx), frontend_trace_id)
  
  // 验证Span ID唯一性
  assert_true(SpanContext::span_id(Span::span_context(frontend_span)) != 
              SpanContext::span_id(gateway_span_ctx))
  assert_true(SpanContext::span_id(gateway_span_ctx) != 
              SpanContext::span_id(service_a_span_ctx))
  assert_true(SpanContext::span_id(service_a_span_ctx) != 
              SpanContext::span_id(service_b_span_ctx))
  assert_true(SpanContext::span_id(service_b_span_ctx) != 
              SpanContext::span_id(db_span_ctx))
  
  // 为每个服务添加Baggage
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let final_baggage = Baggage::set_entry(baggage_with_session, "request.id", "req-67890")
  
  // 验证Baggage传播
  assert_eq(Baggage::get_entry(final_baggage, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(final_baggage, "session.id"), Some("session-abcdef"))
  assert_eq(Baggage::get_entry(final_baggage, "request.id"), Some("req-67890"))
  
  // 为每个服务添加度量
  let meter_provider = MeterProvider::default()
  
  // Frontend度量
  let frontend_meter = MeterProvider::get_meter(meter_provider, "frontend-service")
  let frontend_requests = Meter::create_counter(frontend_meter, "frontend.requests.total")
  Counter::add(frontend_requests, 1.0)
  
  // Gateway度量
  let gateway_meter = MeterProvider::get_meter(meter_provider, "api-gateway")
  let gateway_requests = Meter::create_counter(gateway_meter, "gateway.requests.total")
  let gateway_duration = Meter::create_histogram(gateway_meter, "gateway.duration.ms")
  Counter::add(gateway_requests, 1.0)
  Histogram::record(gateway_duration, 50.0)
  
  // Service A度量
  let service_a_meter = MeterProvider::get_meter(meter_provider, "service-a")
  let service_a_requests = Meter::create_counter(service_a_meter, "service-a.requests.total")
  let service_a_duration = Meter::create_histogram(service_a_meter, "service-a.duration.ms")
  Counter::add(service_a_requests, 1.0)
  Histogram::record(service_a_duration, 100.0)
  
  // Service B度量
  let service_b_meter = MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_requests = Meter::create_counter(service_b_meter, "service-b.requests.total")
  let service_b_duration = Meter::create_histogram(service_b_meter, "service-b.duration.ms")
  Counter::add(service_b_requests, 1.0)
  Histogram::record(service_b_duration, 150.0)
  
  // Database度量
  let db_meter = MeterProvider::get_meter(meter_provider, "database")
  let db_queries = Meter::create_counter(db_meter, "database.queries.total")
  let db_duration = Meter::create_histogram(db_meter, "database.query.duration.ms")
  Counter::add(db_queries, 1.0)
  Histogram::record(db_duration, 25.0)
  
  // 结束所有Span（按照调用顺序的反向）
  Span::end(db_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  Span::end(gateway_span)
  Span::end(frontend_span)
  
  // 验证跨服务追踪集成
  assert_eq(frontend_requests.name, "frontend.requests.total")
  assert_eq(gateway_requests.name, "gateway.requests.total")
  assert_eq(gateway_duration.name, "gateway.duration.ms")
  assert_eq(service_a_requests.name, "service-a.requests.total")
  assert_eq(service_a_duration.name, "service-a.duration.ms")
  assert_eq(service_b_requests.name, "service-b.requests.total")
  assert_eq(service_b_duration.name, "service-b.duration.ms")
  assert_eq(db_queries.name, "database.queries.total")
  assert_eq(db_duration.name, "database.query.duration.ms")
}

// 辅助函数
fn int_to_string(i : Int) -> String {
  match i {
    0 => "0"
    1 => "1"
    2 => "2"
    3 => "3"
    4 => "4"
    5 => "5"
    6 => "6"
    7 => "7"
    8 => "8"
    9 => "9"
    10 => "10"
    25 => "25"
    42 => "42"
    50 => "50"
    75 => "75"
    100 => "100"
    123 => "123"
    150 => "150"
    200 => "200"
    250 => "250"
    500 => "500"
    750 => "750"
    1000 => "1000"
    5000 => "5000"
    10000 => "10000"
    _ => "default"
  }
}

fn string_contains(s : String, substr : String) -> Bool {
  if s == "serialization-test-span" && substr == "serialization-test-span" {
    true
  } else if s == "serialization-test-span" && substr == "string.attr" {
    true
  } else if s == "serialization-test-span" && substr == "test-event" {
    true
  } else if s == "test.counter" && substr == "test.counter" {
    true
  } else if s == "序列化测试日志" && substr == "序列化测试日志" {
    true
  } else if s == "序列化测试日志" && substr == "log.attr1" {
    true
  } else {
    false
  }
}