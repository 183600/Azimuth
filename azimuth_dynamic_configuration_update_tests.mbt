// 配置动态更新测试
// 测试Azimuth遥测系统在运行时动态配置更新的能力

// 测试1: TracerProvider动态配置更新
pub test "TracerProvider动态配置更新测试" {
  // 测试初始TracerProvider配置
  let initial_provider = azimuth::TracerProvider::default()
  let initial_tracer = azimuth::TracerProvider::get_tracer(initial_provider, "initial-service")
  let initial_span = azimuth::Tracer::start_span(initial_tracer, "initial-operation")
  
  // 验证初始配置
  assert_eq(azimuth::Span::name(initial_span), "initial-operation")
  assert_eq(azimuth::Span::kind(initial_span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(initial_span))
  
  // 模拟配置更新 - 创建新的TracerProvider
  let updated_provider = azimuth::TracerProvider::default()
  let updated_tracer = azimuth::TracerProvider::get_tracer(updated_provider, "updated-service", Some("2.0.0"))
  let updated_span = azimuth::Tracer::start_span(updated_tracer, "updated-operation")
  
  // 验证更新后的配置
  assert_eq(azimuth::Span::name(updated_span), "updated-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(updated_tracer).name, "updated-service")
  assert_eq(azimuth::Tracer::instrumentation_scope(updated_tracer).version, Some("2.0.0"))
  
  // 验证初始Tracer仍然正常工作
  let another_initial_span = azimuth::Tracer::start_span(initial_tracer, "another-initial-operation")
  assert_eq(azimuth::Span::name(another_initial_span), "another-initial-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(initial_tracer).name, "initial-service")
  
  // 测试多个Tracer的配置隔离
  let tracer_a = azimuth::TracerProvider::get_tracer(initial_provider, "service-a")
  let tracer_b = azimuth::TracerProvider::get_tracer(updated_provider, "service-b")
  
  let span_a = azimuth::Tracer::start_span(tracer_a, "operation-a")
  let span_b = azimuth::Tracer::start_span(tracer_b, "operation-b")
  
  assert_eq(azimuth::Tracer::instrumentation_scope(tracer_a).name, "service-a")
  assert_eq(azimuth::Tracer::instrumentation_scope(tracer_b).name, "service-b")
  
  // 验证Span属性
  assert_eq(azimuth::Span::name(span_a), "operation-a")
  assert_eq(azimuth::Span::name(span_b), "operation-b")
}

// 测试2: MeterProvider动态配置更新
pub test "MeterProvider动态配置更新测试" {
  // 测试初始MeterProvider配置
  let initial_meter_provider = azimuth::MeterProvider::default()
  let initial_meter = azimuth::MeterProvider::get_meter(initial_meter_provider, "initial-meter")
  let initial_counter = azimuth::Meter::create_counter(initial_meter, "initial.counter", Some("Initial counter"), Some("count"))
  
  // 验证初始配置
  assert_eq(initial_counter.name, "initial.counter")
  assert_eq(initial_counter.description, Some("Initial counter"))
  assert_eq(initial_counter.unit, Some("count"))
  
  // 执行初始度量操作
  azimuth::Counter::add(initial_counter, 10.0)
  
  // 模拟配置更新 - 创建新的MeterProvider
  let updated_meter_provider = azimuth::MeterProvider::default()
  let updated_meter = azimuth::MeterProvider::get_meter(updated_meter_provider, "updated-meter", Some("2.0.0"))
  let updated_counter = azimuth::Meter::create_counter(updated_meter, "updated.counter", Some("Updated counter"), Some("items"))
  
  // 验证更新后的配置
  assert_eq(updated_counter.name, "updated.counter")
  assert_eq(updated_counter.description, Some("Updated counter"))
  assert_eq(updated_counter.unit, Some("items"))
  assert_eq(azimuth::Meter::instrumentation_scope(updated_meter).name, "updated-meter")
  assert_eq(azimuth::Meter::instrumentation_scope(updated_meter).version, Some("2.0.0"))
  
  // 执行更新后的度量操作
  azimuth::Counter::add(updated_counter, 20.0)
  
  // 验证初始Meter仍然正常工作
  let another_initial_counter = azimuth::Meter::create_counter(initial_meter, "another.initial.counter")
  azimuth::Counter::add(another_initial_counter, 5.0)
  
  // 测试不同MeterProvider的隔离性
  let histogram_a = azimuth::Meter::create_histogram(initial_meter, "histogram.a", Some("Histogram A"), Some("ms"))
  let histogram_b = azimuth::Meter::create_histogram(updated_meter, "histogram.b", Some("Histogram B"), Some("ms"))
  
  azimuth::Histogram::record(histogram_a, 100.0)
  azimuth::Histogram::record(histogram_b, 200.0)
  
  // 验证Histogram属性
  assert_eq(histogram_a.name, "histogram.a")
  assert_eq(histogram_a.description, Some("Histogram A"))
  assert_eq(histogram_a.unit, Some("ms"))
  
  assert_eq(histogram_b.name, "histogram.b")
  assert_eq(histogram_b.description, Some("Histogram B"))
  assert_eq(histogram_b.unit, Some("ms"))
}

// 测试3: LoggerProvider动态配置更新
pub test "LoggerProvider动态配置更新测试" {
  // 测试初始LoggerProvider配置
  let initial_logger_provider = azimuth::LoggerProvider::default()
  let initial_logger = azimuth::LoggerProvider::get_logger(initial_logger_provider, "initial-logger")
  
  // 验证初始配置
  assert_eq(initial_logger.scope.name, "initial-logger")
  
  // 创建初始日志记录
  let initial_log = azimuth::LogRecord::new(azimuth::Info, "Initial log message")
  azimuth::Logger::emit(initial_logger, initial_log)
  
  // 模拟配置更新 - 创建新的LoggerProvider
  let updated_logger_provider = azimuth::LoggerProvider::default()
  let updated_logger = azimuth::LoggerProvider::get_logger(updated_logger_provider, "updated-logger", Some("2.0.0"))
  
  // 验证更新后的配置
  assert_eq(updated_logger.scope.name, "updated-logger")
  assert_eq(updated_logger.scope.version, Some("2.0.0"))
  
  // 创建更新后的日志记录
  let updated_log = azimuth::LogRecord::new(azimuth::Info, "Updated log message")
  azimuth::Logger::emit(updated_logger, updated_log)
  
  // 验证初始Logger仍然正常工作
  let another_initial_log = azimuth::LogRecord::new(azimuth::Warn, "Another initial log message")
  azimuth::Logger::emit(initial_logger, another_initial_log)
  
  // 测试不同LoggerProvider的隔离性
  let detailed_log_a = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed log A"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-a"),
    Some("span-a"),
    Some(azimuth::Context::root())
  )
  
  let detailed_log_b = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed log B"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000001L),
    None,
    Some("trace-b"),
    Some("span-b"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(initial_logger, detailed_log_a)
  azimuth::Logger::emit(updated_logger, detailed_log_b)
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::body(detailed_log_a), Some("Detailed log A"))
  assert_eq(azimuth::LogRecord::body(detailed_log_b), Some("Detailed log B"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log_a), Some("trace-a"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log_b), Some("trace-b"))
}

// 测试4: Resource动态配置更新
pub test "Resource动态配置更新测试" {
  // 测试初始Resource配置
  let initial_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("initial-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("deployment.environment", azimuth::StringValue("development")),
    ("config.source", azimuth::StringValue("static"))
  ])
  
  // 验证初始资源配置
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.name"), Some(azimuth::StringValue("initial-service")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "deployment.environment"), Some(azimuth::StringValue("development")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "config.source"), Some(azimuth::StringValue("static")))
  
  // 模拟配置更新 - 创建新的Resource
  let updated_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("updated-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("config.source", azimuth::StringValue("dynamic")),
    ("config.last.update", azimuth::StringValue("2025-12-28T00:00:00Z"))
  ])
  
  // 验证更新后的资源配置
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.name"), Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.source"), Some(azimuth::StringValue("dynamic")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.last.update"), Some(azimuth::StringValue("2025-12-28T00:00:00Z")))
  
  // 验证初始Resource保持不变
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.name"), Some(azimuth::StringValue("initial-service")))
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "config.source"), Some(azimuth::StringValue("static")))
  
  // 测试Resource合并的动态更新
  let base_attrs = [
    ("service.namespace", azimuth::StringValue("production")),
    ("region", azimuth::StringValue("us-west-1"))
  ]
  
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), base_attrs)
  
  let merged_initial = azimuth::Resource::merge(initial_resource, base_resource)
  let merged_updated = azimuth::Resource::merge(updated_resource, base_resource)
  
  // 验证合并后的Resource
  assert_eq(azimuth::Resource::get_attribute(merged_initial, "service.name"), Some(azimuth::StringValue("initial-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_initial, "service.namespace"), Some(azimuth::StringValue("production")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_updated, "service.name"), Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_updated, "service.namespace"), Some(azimuth::StringValue("production")))
}

// 测试5: Propagator动态配置更新
pub test "Propagator动态配置更新测试" {
  // 测试初始Propagator配置
  let initial_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let initial_propagators = [initial_trace_propagator]
  let initial_composite_propagator = azimuth::CompositePropagator::new(initial_propagators)
  
  // 验证初始配置
  let initial_carrier = azimuth::TextMapCarrier::new()
  let initial_ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(initial_composite_propagator, initial_ctx, initial_carrier)
  let initial_extracted_ctx = azimuth::CompositePropagator::extract(initial_composite_propagator, initial_carrier)
  
  // 模拟配置更新 - 添加Baggage Propagator
  let updated_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let updated_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let updated_propagators = [updated_trace_propagator, updated_baggage_propagator]
  let updated_composite_propagator = azimuth::CompositePropagator::new(updated_propagators)
  
  // 验证更新后的配置
  let updated_carrier = azimuth::TextMapCarrier::new()
  let updated_ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(updated_composite_propagator, updated_ctx, updated_carrier)
  let updated_extracted_ctx = azimuth::CompositePropagator::extract(updated_composite_propagator, updated_carrier)
  
  // 验证提取结果
  let key = azimuth::ContextKey::new("extracted")
  let initial_extracted_value = azimuth::Context::get(initial_extracted_ctx, key)
  let updated_extracted_value = azimuth::Context::get(updated_extracted_ctx, key)
  
  // 基于简化实现进行验证
  assert_eq(initial_extracted_value, Some("true"))
  assert_eq(updated_extracted_value, Some("true"))
  
  // 验证初始Propagator仍然正常工作
  let another_initial_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(initial_composite_propagator, initial_ctx, another_initial_carrier)
  let another_initial_extracted_ctx = azimuth::CompositePropagator::extract(initial_composite_propagator, another_initial_carrier)
  let another_initial_extracted_value = azimuth::Context::get(another_initial_extracted_ctx, key)
  
  assert_eq(another_initial_extracted_value, Some("true"))
  
  // 测试不同Propagator的隔离性
  let carrier_a = azimuth::TextMapCarrier::new()
  let carrier_b = azimuth::TextMapCarrier::new()
  
  azimuth::CompositePropagator::inject(initial_composite_propagator, initial_ctx, carrier_a)
  azimuth::CompositePropagator::inject(updated_composite_propagator, updated_ctx, carrier_b)
  
  let extracted_a = azimuth::CompositePropagator::extract(initial_composite_propagator, carrier_a)
  let extracted_b = azimuth::CompositePropagator::extract(updated_composite_propagator, carrier_b)
  
  let extracted_value_a = azimuth::Context::get(extracted_a, key)
  let extracted_value_b = azimuth::Context::get(extracted_b, key)
  
  assert_eq(extracted_value_a, Some("true"))
  assert_eq(extracted_value_b, Some("true"))
}

// 测试6: 采样配置动态更新
pub test "采样配置动态更新测试" {
  // 测试初始采样配置
  let sampled_span_ctx = azimuth::SpanContext::new("trace-sampled", "span-sampled", true, "")
  let unsampled_span_ctx = azimuth::SpanContext::new("trace-unsampled", "span-unsampled", false, "")
  
  // 验证初始采样配置
  assert_true(azimuth::SpanContext::is_sampled(sampled_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(unsampled_span_ctx))
  
  // 创建使用不同采样配置的Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "sampling-test-tracer")
  
  let sampled_span = azimuth::Span::new("sampled-operation", azimuth::Internal, sampled_span_ctx)
  let unsampled_span = azimuth::Span::new("unsampled-operation", azimuth::Internal, unsampled_span_ctx)
  
  // 验证Span采样状态
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(sampled_span)))
  assert_false(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(unsampled_span)))
  
  // 模拟采样配置更新 - 创建新的Span上下文
  let updated_sampled_span_ctx = azimuth::SpanContext::new("trace-updated-sampled", "span-updated-sampled", true, "sampling=updated")
  let updated_unsampled_span_ctx = azimuth::SpanContext::new("trace-updated-unsampled", "span-updated-unsampled", false, "sampling=updated")
  
  // 验证更新后的采样配置
  assert_true(azimuth::SpanContext::is_sampled(updated_sampled_span_ctx))
  assert_false(azimuth::SpanContext::is_sampled(updated_unsampled_span_ctx))
  assert_eq(azimuth::SpanContext::trace_state(updated_sampled_span_ctx), "sampling=updated")
  assert_eq(azimuth::SpanContext::trace_state(updated_unsampled_span_ctx), "sampling=updated")
  
  // 创建使用更新后采样配置的Span
  let updated_sampled_span = azimuth::Span::new("updated-sampled-operation", azimuth::Internal, updated_sampled_span_ctx)
  let updated_unsampled_span = azimuth::Span::new("updated-unsampled-operation", azimuth::Internal, updated_unsampled_span_ctx)
  
  // 验证更新后Span的采样状态
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(updated_sampled_span)))
  assert_false(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(updated_unsampled_span)))
  
  // 测试采样配置的隔离性
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(sampled_span)))
  assert_false(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(unsampled_span)))
  assert_eq(azimuth::SpanContext::trace_state(azimuth::Span::span_context(sampled_span)), "")
  assert_eq(azimuth::SpanContext::trace_state(azimuth::Span::span_context(unsampled_span)), "")
}

// 测试7: 度量仪器配置动态更新
pub test "度量仪器配置动态更新测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "dynamic-instrument-meter")
  
  // 测试初始度量仪器配置
  let initial_counter = azimuth::Meter::create_counter(meter, "initial.counter", Some("Initial counter"), Some("count"))
  let initial_histogram = azimuth::Meter::create_histogram(meter, "initial.histogram", Some("Initial histogram"), Some("ms"))
  
  // 验证初始配置
  assert_eq(initial_counter.name, "initial.counter")
  assert_eq(initial_counter.description, Some("Initial counter"))
  assert_eq(initial_counter.unit, Some("count"))
  
  assert_eq(initial_histogram.name, "initial.histogram")
  assert_eq(initial_histogram.description, Some("Initial histogram"))
  assert_eq(initial_histogram.unit, Some("ms"))
  
  // 执行初始度量操作
  azimuth::Counter::add(initial_counter, 5.0)
  azimuth::Histogram::record(initial_histogram, 100.0)
  
  // 模拟配置更新 - 创建新的度量仪器
  let updated_counter = azimuth::Meter::create_counter(meter, "updated.counter", Some("Updated counter"), Some("items"))
  let updated_histogram = azimuth::Meter::create_histogram(meter, "updated.histogram", Some("Updated histogram"), Some("seconds"))
  
  // 验证更新后的配置
  assert_eq(updated_counter.name, "updated.counter")
  assert_eq(updated_counter.description, Some("Updated counter"))
  assert_eq(updated_counter.unit, Some("items"))
  
  assert_eq(updated_histogram.name, "updated.histogram")
  assert_eq(updated_histogram.description, Some("Updated histogram"))
  assert_eq(updated_histogram.unit, Some("seconds"))
  
  // 执行更新后的度量操作
  azimuth::Counter::add(updated_counter, 10.0)
  azimuth::Histogram::record(updated_histogram, 2.5)
  
  // 验证初始度量仪器仍然正常工作
  azimuth::Counter::add(initial_counter, 3.0)
  azimuth::Histogram::record(initial_histogram, 150.0)
  
  // 测试不同度量仪器的隔离性
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "updown.counter", Some("UpDown counter"), Some("units"))
  let gauge = azimuth::Meter::create_gauge(meter, "gauge.metric", Some("Gauge metric"), Some("percent"))
  
  // 执行操作
  azimuth::UpDownCounter::add(updown_counter, 7.0)
  
  // 验证度量仪器属性
  assert_eq(updown_counter.name, "updown.counter")
  assert_eq(updown_counter.description, Some("UpDown counter"))
  assert_eq(updown_counter.unit, Some("units"))
  
  assert_eq(gauge.name, "gauge.metric")
  assert_eq(gauge.description, Some("Gauge metric"))
  assert_eq(gauge.unit, Some("percent"))
  
  // 验证仪器转换的隔离性
  let initial_instrument = azimuth::Histogram::as_instrument(initial_histogram)
  let updated_instrument = azimuth::Histogram::as_instrument(updated_histogram)
  
  assert_eq(azimuth::Instrument::name(initial_instrument), "initial.histogram")
  assert_eq(azimuth::Instrument::name(updated_instrument), "updated.histogram")
}

// 测试8: 日志级别配置动态更新
pub test "日志级别配置动态更新测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "dynamic-level-logger")
  
  // 测试初始日志级别配置
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, "Trace level message")
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug level message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info level message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning level message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error level message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal level message")
  
  // 验证初始日志级别
  assert_eq(azimuth::LogRecord::severity_number(trace_log), azimuth::Trace)
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 发送所有级别的日志
  azimuth::Logger::emit(logger, trace_log)
  azimuth::Logger::emit(logger, debug_log)
  azimuth::Logger::emit(logger, info_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, fatal_log)
  
  // 模拟日志级别配置更新 - 创建新的Logger
  let updated_logger = azimuth::LoggerProvider::get_logger(logger_provider, "updated-level-logger", Some("2.0.0"))
  
  // 验证更新后的配置
  assert_eq(updated_logger.scope.name, "updated-level-logger")
  assert_eq(updated_logger.scope.version, Some("2.0.0"))
  
  // 创建更新后的日志记录
  let updated_info_log = azimuth::LogRecord::new(azimuth::Info, "Updated info level message")
  let updated_error_log = azimuth::LogRecord::new(azimuth::Error, "Updated error level message")
  
  // 验证日志级别
  assert_eq(azimuth::LogRecord::severity_number(updated_info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(updated_error_log), azimuth::Error)
  
  // 发送更新后的日志
  azimuth::Logger::emit(updated_logger, updated_info_log)
  azimuth::Logger::emit(updated_logger, updated_error_log)
  
  // 验证初始Logger仍然正常工作
  let another_info_log = azimuth::LogRecord::new(azimuth::Info, "Another info level message")
  azimuth::Logger::emit(logger, another_info_log)
  
  // 测试不同Logger的隔离性
  let detailed_log_a = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed log A"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some("trace-a"),
    Some("span-a"),
    Some(azimuth::Context::root())
  )
  
  let detailed_log_b = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Detailed log B"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000001L),
    None,
    Some("trace-b"),
    Some("span-b"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, detailed_log_a)
  azimuth::Logger::emit(updated_logger, detailed_log_b)
  
  // 验证日志记录属性
  assert_eq(azimuth::LogRecord::severity_number(detailed_log_a), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(detailed_log_b), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_log_a), Some("Detailed log A"))
  assert_eq(azimuth::LogRecord::body(detailed_log_b), Some("Detailed log B"))
}

// 测试9: 批量配置动态更新
pub test "批量配置动态更新测试" {
  // 创建初始配置集合
  let initial_tracer_provider = azimuth::TracerProvider::default()
  let initial_meter_provider = azimuth::MeterProvider::default()
  let initial_logger_provider = azimuth::LoggerProvider::default()
  let initial_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("batch-initial-service")),
    ("config.version", azimuth::StringValue("1.0.0"))
  ])
  
  // 创建初始组件
  let initial_tracer = azimuth::TracerProvider::get_tracer(initial_tracer_provider, "batch-initial-tracer")
  let initial_meter = azimuth::MeterProvider::get_meter(initial_meter_provider, "batch-initial-meter")
  let initial_logger = azimuth::LoggerProvider::get_logger(initial_logger_provider, "batch-initial-logger")
  
  // 执行初始操作
  let initial_span = azimuth::Tracer::start_span(initial_tracer, "batch-initial-operation")
  let initial_counter = azimuth::Meter::create_counter(initial_meter, "batch.initial.counter")
  let initial_log = azimuth::LogRecord::new(azimuth::Info, "Batch initial log")
  
  azimuth::Counter::add(initial_counter, 1.0)
  azimuth::Logger::emit(initial_logger, initial_log)
  
  // 验证初始配置
  assert_eq(azimuth::Span::name(initial_span), "batch-initial-operation")
  assert_eq(initial_counter.name, "batch.initial.counter")
  assert_eq(initial_logger.scope.name, "batch-initial-logger")
  assert_eq(azimuth::Resource::get_attribute(initial_resource, "service.name"), Some(azimuth::StringValue("batch-initial-service")))
  
  // 批量更新配置
  let updated_tracer_provider = azimuth::TracerProvider::default()
  let updated_meter_provider = azimuth::MeterProvider::default()
  let updated_logger_provider = azimuth::LoggerProvider::default()
  let updated_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("batch-updated-service")),
    ("config.version", azimuth::StringValue("2.0.0")),
    ("config.last.update", azimuth::StringValue("2025-12-28T00:00:00Z"))
  ])
  
  // 创建更新后的组件
  let updated_tracer = azimuth::TracerProvider::get_tracer(updated_tracer_provider, "batch-updated-tracer", Some("2.0.0"))
  let updated_meter = azimuth::MeterProvider::get_meter(updated_meter_provider, "batch-updated-meter", Some("2.0.0"))
  let updated_logger = azimuth::LoggerProvider::get_logger(updated_logger_provider, "batch-updated-logger", Some("2.0.0"))
  
  // 执行更新后的操作
  let updated_span = azimuth::Tracer::start_span(updated_tracer, "batch-updated-operation")
  let updated_counter = azimuth::Meter::create_counter(updated_meter, "batch.updated.counter")
  let updated_log = azimuth::LogRecord::new(azimuth::Info, "Batch updated log")
  
  azimuth::Counter::add(updated_counter, 2.0)
  azimuth::Logger::emit(updated_logger, updated_log)
  
  // 验证更新后的配置
  assert_eq(azimuth::Span::name(updated_span), "batch-updated-operation")
  assert_eq(updated_counter.name, "batch.updated.counter")
  assert_eq(updated_logger.scope.name, "batch-updated-logger")
  assert_eq(updated_logger.scope.version, Some("2.0.0"))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.name"), Some(azimuth::StringValue("batch-updated-service")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.last.update"), Some(azimuth::StringValue("2025-12-28T00:00:00Z")))
  
  // 验证初始组件仍然正常工作
  let another_initial_span = azimuth::Tracer::start_span(initial_tracer, "another-batch-initial-operation")
  let another_initial_counter = azimuth::Meter::create_counter(initial_meter, "another.batch.initial.counter")
  let another_initial_log = azimuth::LogRecord::new(azimuth::Info, "Another batch initial log")
  
  azimuth::Counter::add(another_initial_counter, 3.0)
  azimuth::Logger::emit(initial_logger, another_initial_log)
  
  assert_eq(azimuth::Span::name(another_initial_span), "another-batch-initial-operation")
  assert_eq(another_initial_counter.name, "another.batch.initial.counter")
  assert_eq(initial_logger.scope.name, "batch-initial-logger")
  
  // 验证配置隔离性
  assert_eq(azimuth::Tracer::instrumentation_scope(initial_tracer).name, "batch-initial-tracer")
  assert_eq(azimuth::Tracer::instrumentation_scope(updated_tracer).name, "batch-updated-tracer")
  assert_eq(azimuth::Meter::instrumentation_scope(initial_meter).name, "batch-initial-meter")
  assert_eq(azimuth::Meter::instrumentation_scope(updated_meter).name, "batch-updated-meter")
  assert_eq(initial_logger.scope.name, "batch-initial-logger")
  assert_eq(updated_logger.scope.name, "batch-updated-logger")
}

// 测试10: 配置更新一致性验证
pub test "配置更新一致性验证测试" {
  // 创建配置一致性测试场景
  let base_tracer_provider = azimuth::TracerProvider::default()
  let base_meter_provider = azimuth::MeterProvider::default()
  let base_logger_provider = azimuth::LoggerProvider::default()
  
  // 创建基础组件
  let base_tracer = azimuth::TracerProvider::get_tracer(base_tracer_provider, "consistency-base-tracer")
  let base_meter = azimuth::MeterProvider::get_meter(base_meter_provider, "consistency-base-meter")
  let base_logger = azimuth::LoggerProvider::get_logger(base_logger_provider, "consistency-base-logger")
  
  // 执行基础操作
  let base_span = azimuth::Tracer::start_span(base_tracer, "consistency-base-operation")
  let base_counter = azimuth::Meter::create_counter(base_meter, "consistency.base.counter")
  let base_log = azimuth::LogRecord::new(azimuth::Info, "Consistency base log")
  
  azimuth::Counter::add(base_counter, 10.0)
  azimuth::Logger::emit(base_logger, base_log)
  
  // 验证基础配置
  assert_eq(azimuth::Span::name(base_span), "consistency-base-operation")
  assert_eq(base_counter.name, "consistency.base.counter")
  assert_eq(base_logger.scope.name, "consistency-base-logger")
  
  // 分阶段更新配置
  // 阶段1：更新Tracer
  let stage1_tracer_provider = azimuth::TracerProvider::default()
  let stage1_tracer = azimuth::TracerProvider::get_tracer(stage1_tracer_provider, "stage1-tracer", Some("1.1.0"))
  
  let stage1_span = azimuth::Tracer::start_span(stage1_tracer, "stage1-operation")
  
  // 验证阶段1配置
  assert_eq(azimuth::Span::name(stage1_span), "stage1-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(stage1_tracer).name, "stage1-tracer")
  assert_eq(azimuth::Tracer::instrumentation_scope(stage1_tracer).version, Some("1.1.0"))
  
  // 验证基础组件仍然正常
  let another_base_span = azimuth::Tracer::start_span(base_tracer, "another-consistency-base-operation")
  assert_eq(azimuth::Span::name(another_base_span), "consistency-base-operation")
  
  // 阶段2：更新Meter
  let stage2_meter_provider = azimuth::MeterProvider::default()
  let stage2_meter = azimuth::MeterProvider::get_meter(stage2_meter_provider, "stage2-meter", Some("1.2.0"))
  
  let stage2_counter = azimuth::Meter::create_counter(stage2_meter, "stage2.counter")
  azimuth::Counter::add(stage2_counter, 20.0)
  
  // 验证阶段2配置
  assert_eq(stage2_counter.name, "stage2.counter")
  assert_eq(azimuth::Meter::instrumentation_scope(stage2_meter).name, "stage2-meter")
  assert_eq(azimuth::Meter::instrumentation_scope(stage2_meter).version, Some("1.2.0"))
  
  // 验证基础组件仍然正常
  let another_base_counter = azimuth::Meter::create_counter(base_meter, "another.consistency.base.counter")
  azimuth::Counter::add(another_base_counter, 30.0)
  assert_eq(another_base_counter.name, "another.consistency.base.counter")
  
  // 阶段3：更新Logger
  let stage3_logger_provider = azimuth::LoggerProvider::default()
  let stage3_logger = azimuth::LoggerProvider::get_logger(stage3_logger_provider, "stage3-logger", Some("1.3.0"))
  
  let stage3_log = azimuth::LogRecord::new(azimuth::Info, "Stage3 log")
  azimuth::Logger::emit(stage3_logger, stage3_log)
  
  // 验证阶段3配置
  assert_eq(stage3_logger.scope.name, "stage3-logger")
  assert_eq(stage3_logger.scope.version, Some("1.3.0"))
  
  // 验证基础组件仍然正常
  let another_base_log = azimuth::LogRecord::new(azimuth::Info, "Another consistency base log")
  azimuth::Logger::emit(base_logger, another_base_log)
  assert_eq(base_logger.scope.name, "consistency-base-logger")
  
  // 最终一致性验证
  // 验证所有阶段的组件都正常工作
  let final_base_span = azimuth::Tracer::start_span(base_tracer, "final-base-operation")
  let final_stage1_span = azimuth::Tracer::start_span(stage1_tracer, "final-stage1-operation")
  
  let final_base_counter = azimuth::Meter::create_counter(base_meter, "final.base.counter")
  let final_stage2_counter = azimuth::Meter::create_counter(stage2_meter, "final.stage2.counter")
  
  let final_base_log = azimuth::LogRecord::new(azimuth::Info, "Final base log")
  let final_stage3_log = azimuth::LogRecord::new(azimuth::Info, "Final stage3 log")
  
  // 执行最终操作
  azimuth::Counter::add(final_base_counter, 40.0)
  azimuth::Counter::add(final_stage2_counter, 50.0)
  azimuth::Logger::emit(base_logger, final_base_log)
  azimuth::Logger::emit(stage3_logger, final_stage3_log)
  
  // 验证最终配置一致性
  assert_eq(azimuth::Span::name(final_base_span), "final-base-operation")
  assert_eq(azimuth::Span::name(final_stage1_span), "final-stage1-operation")
  assert_eq(final_base_counter.name, "final.base.counter")
  assert_eq(final_stage2_counter.name, "final.stage2.counter")
  assert_eq(base_logger.scope.name, "consistency-base-logger")
  assert_eq(stage3_logger.scope.name, "stage3-logger")
  
  // 验证配置隔离性
  assert_true(azimuth::Tracer::instrumentation_scope(base_tracer).name != azimuth::Tracer::instrumentation_scope(stage1_tracer).name)
  assert_true(azimuth::Meter::instrumentation_scope(base_meter).name != azimuth::Meter::instrumentation_scope(stage2_meter).name)
  assert_true(base_logger.scope.name != stage3_logger.scope.name)
}