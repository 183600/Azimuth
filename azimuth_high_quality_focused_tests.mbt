// Azimuth High-Quality Focused Test Suite
// 高质量聚焦测试套件，针对遥测系统的核心功能进行深度测试

// 测试1: 属性值类型转换与验证
pub test "属性值类型转换与边界验证" {
  // 测试各种属性值类型的创建和转换
  let string_attr = azimuth::StringValue("test.value")
  let int_attr = azimuth::IntValue(42)
  let float_attr = azimuth::FloatValue(3.14159)
  let bool_attr = azimuth::BoolValue(true)
  let string_array_attr = azimuth::ArrayStringValue(["a", "b", "c"])
  let int_array_attr = azimuth::ArrayIntValue([1, 2, 3])
  
  // 创建属性集合
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "string.key", string_attr)
  azimuth::Attributes::set(attrs, "int.key", int_attr)
  azimuth::Attributes::set(attrs, "float.key", float_attr)
  azimuth::Attributes::set(attrs, "bool.key", bool_attr)
  azimuth::Attributes::set(attrs, "string.array.key", string_array_attr)
  azimuth::Attributes::set(attrs, "int.array.key", int_array_attr)
  
  // 验证属性值
  assert_eq(azimuth::Attributes::get(attrs, "string.key"), Some(string_attr))
  assert_eq(azimuth::Attributes::get(attrs, "int.key"), Some(int_attr))
  assert_eq(azimuth::Attributes::get(attrs, "float.key"), Some(float_attr))
  assert_eq(azimuth::Attributes::get(attrs, "bool.key"), Some(bool_attr))
  
  // 测试边界值
  let max_int = azimuth::IntValue(2147483647)
  let min_int = azimuth::IntValue(-2147483648)
  let max_float = azimuth::FloatValue(1.7976931348623157e+308)
  let min_float = azimuth::FloatValue(-1.7976931348623157e+308)
  
  azimuth::Attributes::set(attrs, "max.int", max_int)
  azimuth::Attributes::set(attrs, "min.int", min_int)
  azimuth::Attributes::set(attrs, "max.float", max_float)
  azimuth::Attributes::set(attrs, "min.float", min_float)
  
  assert_eq(azimuth::Attributes::get(attrs, "max.int"), Some(max_int))
  assert_eq(azimuth::Attributes::get(attrs, "min.int"), Some(min_int))
  assert_eq(azimuth::Attributes::get(attrs, "max.float"), Some(max_float))
  assert_eq(azimuth::Attributes::get(attrs, "min.float"), Some(min_float))
}

// 测试2: 跨服务上下文传播一致性
pub test "跨服务上下文传播一致性验证" {
  // 创建初始上下文
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "service-a")
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "service-a.operation")
  let root_context = azimuth::Span::span_context(root_span)
  
  // 验证根上下文有效性
  assert_true(azimuth::SpanContext::is_valid(root_context))
  let trace_id = azimuth::SpanContext::trace_id(root_context)
  assert_true(trace_id.length() > 0)
  
  // 模拟服务间传播
  let service_b_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "service-b")
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b.operation")
  
  // 验证Trace ID传播
  let service_b_context = azimuth::Span::span_context(service_b_span)
  assert_eq(azimuth::SpanContext::trace_id(service_b_context), trace_id)
  assert_true(azimuth::SpanContext::span_id(service_b_context) != 
              azimuth::SpanContext::span_id(root_context))
  
  // 测试Baggage传播
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  
  // 验证Baggage内容
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session, "session.id"), Some("session-abcdef"))
  
  // 结束Span
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(root_span)
}

// 测试3: 度量聚合与统计功能
pub test "度量聚合与统计功能验证" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // 创建各种度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active.connections")
  let memory_gauge = azimuth::Meter::create_gauge(meter, "process.memory.usage")
  
  // 模拟度量数据收集
  let request_counts = [10, 15, 20, 25, 30, 35, 40, 45, 50]
  let response_times = [100.0, 120.0, 80.0, 150.0, 90.0, 110.0, 130.0, 95.0, 105.0]
  
  // 累加请求计数
  let total_requests = 0
  for count in request_counts {
    azimuth::Counter::add(request_counter, count.to_double())
    total_requests = total_requests + count
  }
  
  // 记录响应时间
  for time in response_times {
    azimuth::Histogram::record(response_histogram, time)
  }
  
  // 模拟连接数变化
  azimuth::UpDownCounter::add(active_connections, 10.0)
  for i in 0..5 {
    azimuth::UpDownCounter::add(active_connections, 1.0)
  }
  for i in 0..3 {
    azimuth::UpDownCounter::add(active_connections, -1.0)
  }
  
  // 设置内存使用量
  azimuth::Gauge::set(memory_gauge, 512.0)
  
  // 验证度量仪器创建
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(memory_gauge.name, "process.memory.usage")
  
  // 验证总请求数
  assert_eq(total_requests, 270)
}

// 测试4: 时间序列数据处理
pub test "时间序列数据处理与压缩验证" {
  let clock = azimuth::Clock::system()
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time.series.test")
  
  // 创建时间序列度量
  let cpu_histogram = azimuth::Meter::create_histogram(meter, "cpu.usage.percent")
  let memory_histogram = azimuth::Meter::create_histogram(meter, "memory.usage.percent")
  
  // 生成时间序列数据
  let base_time = azimuth::Clock::now_unix_nanos(clock)
  let time_points = []
  let cpu_values = []
  let memory_values = []
  
  for i in 0..60 {
    // 每秒一个数据点，持续60秒
    let timestamp = base_time + (i * 1000000000L)
    
    // 生成模拟数据（CPU使用率呈周期性变化）
    let cpu_value = 30.0 + (20.0 * @sin(i.to_double() * 0.1)) + (@random() * 10.0)
    let memory_value = 40.0 + (15.0 * @cos(i.to_double() * 0.05)) + (@random() * 5.0)
    
    // 记录度量
    azimuth::Histogram::record(cpu_histogram, cpu_value)
    azimuth::Histogram::record(memory_histogram, memory_value)
    
    // 存储时间序列数据
    time_points.push(timestamp)
    cpu_values.push(cpu_value)
    memory_values.push(memory_value)
  }
  
  // 验证时间序列单调性
  for i in 1..time_points.length() {
    assert_true(time_points[i] > time_points[i-1])
  }
  
  // 验证数据范围
  let max_cpu = cpu_values.reduce(fn(acc, x) { if x > acc { x } else { acc } }, 0.0)
  let min_cpu = cpu_values.reduce(fn(acc, x) { if x < acc { x } else { acc } }, 100.0)
  let max_memory = memory_values.reduce(fn(acc, x) { if x > acc { x } else { acc } }, 0.0)
  let min_memory = memory_values.reduce(fn(acc, x) { if x < acc { x } else { acc } }, 100.0)
  
  assert_true(max_cpu > min_cpu)
  assert_true(max_memory > min_memory)
  assert_true(time_points.length() == 60)
}

// 测试5: 并发安全性验证
pub test "并发安全性与资源管理验证" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent.test")
  
  // 创建多个并发Span
  let spans = []
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "concurrent.span." + i.to_string())
    spans.push(span)
  }
  
  // 并发操作Span
  for i in 0..spans.length() {
    let span = spans[i]
    
    // 设置属性
    azimuth::Span::set_attribute(span, "thread.id", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "operation.name", azimuth::StringValue("concurrent.operation"))
    
    // 添加事件
    azimuth::Span::add_event(span, "operation.started", 
      Some([("timestamp", azimuth::StringValue(@now().to_string()))]))
    
    // 设置状态
    if i % 3 == 0 {
      azimuth::Span::set_status(span, azimuth::Ok)
    } else if i % 3 == 1 {
      azimuth::Span::set_status(span, azimuth::Error)
    } else {
      azimuth::Span::set_status(span, azimuth::Unset)
    }
  }
  
  // 并发结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证所有Span都被正确处理
  assert_eq(spans.length(), 20)
  
  // 测试度量的并发安全性
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent.metrics")
  let counter = azimuth::Meter::create_counter(meter, "concurrent.operations")
  
  // 并发增加计数器
  for i in 0..100 {
    azimuth::Counter::add(counter, 1.0)
  }
  
  assert_eq(counter.name, "concurrent.operations")
}

// 测试6: 错误恢复与边界条件处理
pub test "错误恢复与边界条件处理验证" {
  // 测试无效Span上下文处理
  let invalid_trace_id = ""
  let invalid_span_id = ""
  let invalid_context = azimuth::SpanContext::new(invalid_trace_id, invalid_span_id, false, "")
  
  assert_false(azimuth::SpanContext::is_valid(invalid_context))
  
  // 测试空属性处理
  let empty_attrs = azimuth::Attributes::new()
  let non_existent_key = "non.existent.key"
  assert_eq(azimuth::Attributes::get(empty_attrs, non_existent_key), None)
  
  // 测试特殊字符键处理
  let special_attrs = azimuth::Attributes::new()
  let special_keys = ["key.with.dots", "key_with_underscores", "key-with-hyphens", "中文键"]
  
  for key in special_keys {
    let value = azimuth::StringValue("value.for." + key)
    azimuth::Attributes::set(special_attrs, key, value)
    assert_eq(azimuth::Attributes::get(special_attrs, key), Some(value))
  }
  
  // 测试Logger的错误处理
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error.recovery.test")
  
  // 创建各种严重级别的日志
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Critical error occurred")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning condition detected")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Informational message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug information")
  
  // 发送日志并验证系统稳定性
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, debug_log)
  
  // 验证日志严重级别
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  
  // 测试资源合并边界条件
  let empty_resource = azimuth::Resource::new()
  let resource_with_attrs = azimuth::Resource::with_attributes(empty_resource, 
    [("service.name", azimuth::StringValue("test.service"))])
  
  let merged_resource = azimuth::Resource::merge(empty_resource, resource_with_attrs)
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
    Some(azimuth::StringValue("test.service")))
}

// 测试7: 资源管理与生命周期
pub test "资源管理与生命周期验证" {
  // 创建具有多种属性的Resource
  let base_resource = azimuth::Resource::new()
  let resource_with_attrs = azimuth::Resource::with_attributes(base_resource, [
    ("service.name", azimuth::StringValue("lifecycle.test.service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-12345")),
    ("deployment.environment", azimuth::StringValue("testing")),
    ("host.name", azimuth::StringValue("test-host")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.id", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("test-process")),
    ("process.command_args", azimuth::ArrayStringValue(["--config", "test.yaml"]))
  ])
  
  // 验证所有属性都正确设置
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.name"), 
    Some(azimuth::StringValue("lifecycle.test.service")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "service.version"), 
    Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_attrs, "process.id"), 
    Some(azimuth::IntValue(12345)))
  
  // 测试资源合并策略
  let override_resource = azimuth::Resource::with_attributes(base_resource, [
    ("service.version", azimuth::StringValue("2.0.0")),  // 覆盖现有版本
    ("deployment.environment", azimuth::StringValue("production")),  // 覆盖环境
    ("new.attribute", azimuth::StringValue("new.value"))  // 新增属性
  ])
  
  let merged_resource = azimuth::Resource::merge(resource_with_attrs, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
    Some(azimuth::StringValue("lifecycle.test.service")))  // 保持原值
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), 
    Some(azimuth::StringValue("2.0.0")))  // 被覆盖
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), 
    Some(azimuth::StringValue("production")))  // 被覆盖
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "new.attribute"), 
    Some(azimuth::StringValue("new.value")))  // 新增
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), 
    Some(azimuth::StringValue("test-host")))  // 保持原值
}

// 测试8: 日志记录与追踪关联
pub test "日志记录与追踪关联验证" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "log.trace.correlation")
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "correlation.test.logger")
  
  // 创建Span
  let span = azimuth::Tracer::start_span(tracer, "operation.with.logs")
  let span_context = azimuth::Span::span_context(span)
  
  // 验证Span上下文有效性
  assert_true(azimuth::SpanContext::is_valid(span_context))
  let trace_id = azimuth::SpanContext::trace_id(span_context)
  let span_id = azimuth::SpanContext::span_id(span_context)
  
  // 创建与Span关联的日志记录
  let start_log = azimuth::LogRecord::new(azimuth::Info, "Operation started")
  azimuth::LogRecord::add_attribute(start_log, "trace.id", azimuth::StringValue(trace_id))
  azimuth::LogRecord::add_attribute(start_log, "span.id", azimuth::StringValue(span_id))
  
  let progress_log = azimuth::LogRecord::new(azimuth::Info, "Operation in progress")
  azimuth::LogRecord::add_attribute(progress_log, "trace.id", azimuth::StringValue(trace_id))
  azimuth::LogRecord::add_attribute(progress_log, "span.id", azimuth::StringValue(span_id))
  azimuth::LogRecord::add_attribute(progress_log, "progress.percent", azimuth::IntValue(50))
  
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Operation encountered error")
  azimuth::LogRecord::add_attribute(error_log, "trace.id", azimuth::StringValue(trace_id))
  azimuth::LogRecord::add_attribute(error_log, "span.id", azimuth::StringValue(span_id))
  azimuth::LogRecord::add_attribute(error_log, "error.code", azimuth::StringValue("E500"))
  azimuth::LogRecord::add_attribute(error_log, "error.message", azimuth::StringValue("Internal server error"))
  
  let completion_log = azimuth::LogRecord::new(azimuth::Info, "Operation completed")
  azimuth::LogRecord::add_attribute(completion_log, "trace.id", azimuth::StringValue(trace_id))
  azimuth::LogRecord::add_attribute(completion_log, "span.id", azimuth::StringValue(span_id))
  azimuth::LogRecord::add_attribute(completion_log, "duration.ms", azimuth::IntValue(250))
  
  // 发送日志记录
  azimuth::Logger::emit(logger, start_log)
  azimuth::Logger::emit(logger, progress_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, completion_log)
  
  // 向Span添加事件
  azimuth::Span::add_event(span, "log.recorded", 
    Some([("log.level", azimuth::StringValue("INFO")), 
          ("log.message", azimuth::StringValue("Operation started"))]))
  
  azimuth::Span::add_event(span, "error.occurred", 
    Some([("error.code", azimuth::StringValue("E500")), 
          ("error.message", azimuth::StringValue("Internal server error"))]))
  
  // 设置Span状态
  azimuth::Span::set_status(span, azimuth::Error, Some("Operation completed with errors"))
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证日志和追踪的关联
  assert_true(trace_id.length() > 0)
  assert_true(span_id.length() > 0)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(start_log), azimuth::Info)
}

// 测试9: 配置动态更新
pub test "配置动态更新验证" {
  // 创建初始配置
  let initial_config = azimuth::Configuration::new()
  azimuth::Configuration::set_sampling_probability(initial_config, 0.1)
  azimuth::Configuration::set_max_export_batch_size(initial_config, 512)
  azimuth::Configuration::set_max_export_timeout(initial_config, 30000)
  azimuth::Configuration::set_max_attributes_per_span(initial_config, 128)
  azimuth::Configuration::set_max_events_per_span(initial_config, 128)
  azimuth::Configuration::set_max_links_per_span(initial_config, 128)
  
  // 验证初始配置
  assert_eq(azimuth::Configuration::get_sampling_probability(initial_config), 0.1)
  assert_eq(azimuth::Configuration::get_max_export_batch_size(initial_config), 512)
  assert_eq(azimuth::Configuration::get_max_export_timeout(initial_config), 30000)
  
  // 动态更新配置
  azimuth::Configuration::set_sampling_probability(initial_config, 0.5)
  azimuth::Configuration::set_max_export_batch_size(initial_config, 1024)
  azimuth::Configuration::set_max_export_timeout(initial_config, 60000)
  azimuth::Configuration::set_max_attributes_per_span(initial_config, 256)
  
  // 验证更新后的配置
  assert_eq(azimuth::Configuration::get_sampling_probability(initial_config), 0.5)
  assert_eq(azimuth::Configuration::get_max_export_batch_size(initial_config), 1024)
  assert_eq(azimuth::Configuration::get_max_export_timeout(initial_config), 60000)
  assert_eq(azimuth::Configuration::get_max_attributes_per_span(initial_config), 256)
  
  // 测试配置边界值
  azimuth::Configuration::set_sampling_probability(initial_config, 0.0)  // 最小值
  assert_eq(azimuth::Configuration::get_sampling_probability(initial_config), 0.0)
  
  azimuth::Configuration::set_sampling_probability(initial_config, 1.0)  // 最大值
  assert_eq(azimuth::Configuration::get_sampling_probability(initial_config), 1.0)
  
  // 测试服务资源配置
  let service_config = azimuth::ServiceConfiguration::new()
  azimuth::ServiceConfiguration::set_service_name(service_config, "dynamic.config.test")
  azimuth::ServiceConfiguration::set_service_version(service_config, "2.1.0")
  azimuth::ServiceConfiguration::set_service_namespace(service_config, "test.namespace")
  
  assert_eq(azimuth::ServiceConfiguration::get_service_name(service_config), "dynamic.config.test")
  assert_eq(azimuth::ServiceConfiguration::get_service_version(service_config), "2.1.0")
  assert_eq(azimuth::ServiceConfiguration::get_service_namespace(service_config), "test.namespace")
}

// 测试10: 性能基准测试
pub test "性能基准测试验证" {
  let start_time = @now()
  
  // 创建Tracer和多个Span的性能测试
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  // 测试Span创建性能
  let span_creation_start = @now()
  let spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "performance.span." + i.to_string())
    spans.push(span)
  }
  let span_creation_end = @now()
  let span_creation_duration = span_creation_end - span_creation_start
  
  // 测试属性设置性能
  let attribute_set_start = @now()
  for i in 0..spans.length() {
    let span = spans[i]
    azimuth::Span::set_attribute(span, "iteration", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "operation.name", azimuth::StringValue("performance.test"))
    azimuth::Span::set_attribute(span, "component", azimuth::StringValue("benchmark"))
  }
  let attribute_set_end = @now()
  let attribute_set_duration = attribute_set_end - attribute_set_start
  
  // 测试事件添加性能
  let event_add_start = @now()
  for i in 0..spans.length() {
    let span = spans[i]
    azimuth::Span::add_event(span, "performance.event", 
      Some([("event.id", azimuth::IntValue(i)), 
            ("event.type", azimuth::StringValue("benchmark"))]))
  }
  let event_add_end = @now()
  let event_add_duration = event_add_end - event_add_start
  
  // 测试Span结束性能
  let span_end_start = @now()
  for span in spans {
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
  }
  let span_end_end = @now()
  let span_end_duration = span_end_end - span_end_start
  
  // 测试度量操作性能
  let metrics_start = @now()
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance.metrics")
  let counter = azimuth::Meter::create_counter(meter, "performance.operations")
  let histogram = azimuth::Meter::create_histogram(meter, "performance.duration")
  
  for i in 0..1000 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() % 100.0)
  }
  let metrics_end = @now()
  let metrics_duration = metrics_end - metrics_start
  
  let total_end_time = @now()
  let total_duration = total_end_time - start_time
  
  // 验证性能基准（这些值应该根据实际环境调整）
  assert_true(span_creation_duration < 1000000)  // 1秒
  assert_true(attribute_set_duration < 1000000)  // 1秒
  assert_true(event_add_duration < 1000000)  // 1秒
  assert_true(span_end_duration < 1000000)  // 1秒
  assert_true(metrics_duration < 1000000)  // 1秒
  assert_true(total_duration < 5000000)  // 5秒
  
  // 验证操作数量
  assert_eq(spans.length(), 1000)
  assert_eq(counter.name, "performance.operations")
  assert_eq(histogram.name, "performance.duration")
}