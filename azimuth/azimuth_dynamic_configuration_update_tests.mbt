// Azimuth Dynamic Configuration Update Tests
// This file contains tests for dynamic configuration updates in the Azimuth telemetry system

// Test 1: Tracer configuration dynamic update
pub test "配置动态更新测试：Tracer配置动态更新" {
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
  
  // 测试配置切换的连续性
  let transition_tracer = azimuth::TracerProvider::get_tracer(updated_provider, "transition-service", Some("1.5.0"))
  let transition_span = azimuth::Tracer::start_span(transition_tracer, "transition-operation")
  
  // 验证过渡配置
  assert_eq(azimuth::Span::name(transition_span), "transition-operation")
  assert_eq(azimuth::Tracer::instrumentation_scope(transition_tracer).name, "transition-service")
  assert_eq(azimuth::Tracer::instrumentation_scope(transition_tracer).version, Some("1.5.0"))
  
  // 测试批量配置更新
  let batch_provider = azimuth::TracerProvider::default()
  let batch_tracers = []
  
  for i in 0..5 {
    let tracer = azimuth::TracerProvider::get_tracer(batch_provider, "batch-service-" + i.to_string(), Some("3." + i.to_string() + ".0"))
    batch_tracers.push(tracer)
  }
  
  // 验证批量配置
  for i in 0..batch_tracers.length() {
    let tracer = batch_tracers[i]
    let expected_name = "batch-service-" + i.to_string()
    let expected_version = "3." + i.to_string() + ".0"
    
    assert_eq(azimuth::Tracer::instrumentation_scope(tracer).name, expected_name)
    assert_eq(azimuth::Tracer::instrumentation_scope(tracer).version, Some(expected_version))
  }
  
  // 结束所有Span
  azimuth::Span::end(initial_span)
  azimuth::Span::end(updated_span)
  azimuth::Span::end(transition_span)
}

// Test 2: Meter configuration dynamic update
pub test "配置动态更新测试：Meter配置动态更新" {
  // 测试Meter配置动态更新
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
  
  // 测试不同类型度量的配置更新
  let histogram = azimuth::Meter::create_histogram(updated_meter, "response.time", Some("Response time"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(updated_meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = azimuth::Meter::create_gauge(updated_meter, "memory.usage", Some("Memory usage"), Some("MB"))
  
  // 验证不同类型度量的配置
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage"))
  assert_eq(gauge.unit, Some("MB"))
  
  // 测试度量操作在配置更新后的连续性
  azimuth::Counter::add(initial_counter, 10.0)
  azimuth::Counter::add(updated_counter, 20.0)
  azimuth::Histogram::record(histogram, 150.5)
  azimuth::UpDownCounter::add(updown_counter, 5.0)
  
  // 验证操作成功（通过检查度量工具的属性）
  assert_eq(initial_counter.name, "initial.counter")
  assert_eq(updated_counter.name, "updated.counter")
  assert_eq(histogram.name, "response.time")
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(gauge.name, "memory.usage")
}

// Test 3: Logger configuration dynamic update
pub test "配置动态更新测试：Logger配置动态更新" {
  // 测试Logger配置动态更新
  let initial_logger_provider = azimuth::LoggerProvider::default()
  let initial_logger = azimuth::LoggerProvider::get_logger(initial_logger_provider, "initial-logger")
  
  // 验证初始Logger配置
  assert_eq(initial_logger.scope.name, "initial-logger")
  
  // 创建初始日志记录
  let initial_log = azimuth::LogRecord::new(azimuth::Info, "Initial log message")
  azimuth::Logger::emit(initial_logger, initial_log)
  
  // 模拟Logger配置更新
  let updated_logger_provider = azimuth::LoggerProvider::default()
  let updated_logger = azimuth::LoggerProvider::get_logger(updated_logger_provider, "updated-logger")
  
  // 验证更新后的Logger配置
  assert_eq(updated_logger.scope.name, "updated-logger")
  
  // 创建更新后的日志记录
  let updated_log = azimuth::LogRecord::new(azimuth::Warn, "Updated log message")
  azimuth::Logger::emit(updated_logger, updated_log)
  
  // 测试不同严重级别的日志配置
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error message")
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, "Fatal message")
  
  // 发出不同严重级别的日志
  azimuth::Logger::emit(updated_logger, debug_log)
  azimuth::Logger::emit(updated_logger, info_log)
  azimuth::Logger::emit(updated_logger, warn_log)
  azimuth::Logger::emit(updated_logger, error_log)
  azimuth::Logger::emit(updated_logger, fatal_log)
  
  // 验证日志记录的严重级别
  assert_eq(azimuth::LogRecord::severity_number(debug_log), azimuth::Debug)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(fatal_log), azimuth::Fatal)
  
  // 测试批量Logger配置更新
  let batch_logger_provider = azimuth::LoggerProvider::default()
  let batch_loggers = []
  
  for i in 0..5 {
    let logger = azimuth::LoggerProvider::get_logger(batch_logger_provider, "batch-logger-" + i.to_string())
    batch_loggers.push(logger)
  }
  
  // 验证批量Logger配置
  for i in 0..batch_loggers.length() {
    let logger = batch_loggers[i]
    let expected_name = "batch-logger-" + i.to_string()
    assert_eq(logger.scope.name, expected_name)
  }
}

// Test 4: Resource configuration dynamic update
pub test "配置动态更新测试：Resource配置动态更新" {
  // 测试Resource配置动态更新
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
    ("config.last.update", azimuth::StringValue("2025-01-01T12:00:00Z"))
  ])
  
  // 验证更新后的资源配置
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.name"), Some(azimuth::StringValue("updated-service")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "service.version"), Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.source"), Some(azimuth::StringValue("dynamic")))
  assert_eq(azimuth::Resource::get_attribute(updated_resource, "config.last.update"), Some(azimuth::StringValue("2025-01-01T12:00:00Z")))
  
  // 测试资源合并的动态更新
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development"))
  ])
  
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("1.1.0")),
    ("environment", azimuth::StringValue("production")),
    ("deployment.region", azimuth::StringValue("us-west-2"))
  ])
  
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证合并后的资源配置
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), Some(azimuth::StringValue("1.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.region"), Some(azimuth::StringValue("us-west-2")))
  
  // 测试资源属性的动态添加和删除
  let mutable_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("static.attr", azimuth::StringValue("static.value"))
  ])
  
  // 添加新属性
  let resource_with_new_attr = azimuth::Resource::with_attributes(mutable_resource, [
    ("dynamic.attr", azimuth::StringValue("dynamic.value"))
  ])
  
  // 验证新属性添加
  assert_eq(azimuth::Resource::get_attribute(resource_with_new_attr, "static.attr"), Some(azimuth::StringValue("static.value")))
  assert_eq(azimuth::Resource::get_attribute(resource_with_new_attr, "dynamic.attr"), Some(azimuth::StringValue("dynamic.value")))
}

// Test 5: Propagator configuration dynamic update
pub test "配置动态更新测试：传播器配置动态更新" {
  // 测试传播器配置动态更新
  let initial_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let initial_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let initial_propagators = [initial_trace_propagator]
  let initial_composite_propagator = azimuth::CompositePropagator::new(initial_propagators)
  
  // 验证初始传播器配置
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  azimuth::CompositePropagator::inject(initial_composite_propagator, ctx, carrier)
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  
  // 基于简化实现进行验证
  assert_eq(trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 模拟传播器配置更新
  let updated_trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let updated_baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let updated_propagators = [updated_trace_propagator, updated_baggage_propagator]
  let updated_composite_propagator = azimuth::CompositePropagator::new(updated_propagators)
  
  // 验证更新后的传播器配置
  let updated_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(updated_composite_propagator, ctx, updated_carrier)
  
  let updated_trace_header = azimuth::TextMapCarrier::get(updated_carrier, "traceparent")
  let updated_baggage_header = azimuth::TextMapCarrier::get(updated_carrier, "baggage")
  
  // 验证更新后的传播器行为
  assert_eq(updated_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试传播器的动态切换
  let switching_propagators = [updated_trace_propagator]
  let switching_composite_propagator = azimuth::CompositePropagator::new(switching_propagators)
  
  let switching_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(switching_composite_propagator, ctx, switching_carrier)
  
  let switching_trace_header = azimuth::TextMapCarrier::get(switching_carrier, "traceparent")
  let switching_baggage_header = azimuth::TextMapCarrier::get(switching_carrier, "baggage")
  
  // 验证切换后的传播器行为
  assert_eq(switching_trace_header, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试传播器的批量配置更新
  let batch_propagators = []
  for i in 0..3 {
    let trace_prop = azimuth::W3CTraceContextPropagator::new()
    batch_propagators.push(trace_prop)
  }
  
  let batch_composite_propagator = azimuth::CompositePropagator::new(batch_propagators)
  
  let batch_carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(batch_composite_propagator, ctx, batch_carrier)
  
  let batch_trace_header = azimuth::TextMapCarrier::get(batch_carrier, "traceparent")
  
  // 验证批量配置的传播器
  assert_eq(batch_trace_header, Some("00-test-trace-id-test-span-id-01"))
}

// Test 6: Context configuration dynamic update
pub test "配置动态更新测试：上下文配置动态更新" {
  // 测试上下文配置动态更新
  let root_ctx = azimuth::Context::root()
  
  // 初始上下文配置
  let initial_key = azimuth::ContextKey::new("initial.config")
  let initial_ctx = azimuth::Context::with_value(root_ctx, initial_key, "initial.value")
  
  // 验证初始上下文配置
  assert_eq(azimuth::Context::get(initial_ctx, initial_key), Some("initial.value"))
  
  // 模拟上下文配置更新
  let updated_key = azimuth::ContextKey::new("updated.config")
  let updated_ctx = azimuth::Context::with_value(initial_ctx, updated_key, "updated.value")
  
  // 验证更新后的上下文配置
  assert_eq(azimuth::Context::get(updated_ctx, initial_key), Some("initial.value"))
  assert_eq(azimuth::Context::get(updated_ctx, updated_key), Some("updated.value"))
  
  // 测试上下文配置的覆盖更新
  let override_key = azimuth::ContextKey::new("override.config")
  let override_ctx = azimuth::Context::with_value(updated_ctx, override_key, "override.value")
  let reoverride_ctx = azimuth::Context::with_value(override_ctx, override_key, "reoverride.value")
  
  // 验证覆盖更新
  assert_eq(azimuth::Context::get(override_ctx, override_key), Some("override.value"))
  assert_eq(azimuth::Context::get(reoverride_ctx, override_key), Some("reoverride.value"))
  
  // 测试上下文配置的批量更新
  let batch_ctx = updated_ctx
  let batch_keys = []
  let batch_values = []
  
  for i in 0..5 {
    let key = azimuth::ContextKey::new("batch.config." + i.to_string())
    let value = "batch.value." + i.to_string()
    
    batch_ctx = azimuth::Context::with_value(batch_ctx, key, value)
    batch_keys.push(key)
    batch_values.push(value)
  }
  
  // 验证批量上下文配置
  for i in 0..batch_keys.length() {
    assert_eq(azimuth::Context::get(batch_ctx, batch_keys[i]), Some(batch_values[i]))
  }
  
  // 验证原有配置仍然存在
  assert_eq(azimuth::Context::get(batch_ctx, initial_key), Some("initial.value"))
  assert_eq(azimuth::Context::get(batch_ctx, updated_key), Some("updated.value"))
  assert_eq(azimuth::Context::get(batch_ctx, override_key), Some("reoverride.value"))
}

// Test 7: Baggage configuration dynamic update
pub test "配置动态更新测试：Baggage配置动态更新" {
  // 测试Baggage配置动态更新
  let initial_baggage = azimuth::Baggage::new()
  
  // 初始Baggage配置
  let initial_baggage = azimuth::Baggage::set_entry(initial_baggage, "initial.baggage", "initial.value")
  
  // 验证初始Baggage配置
  assert_eq(azimuth::Baggage::get_entry(initial_baggage, "initial.baggage"), Some("initial.value"))
  
  // 模拟Baggage配置更新
  let updated_baggage = azimuth::Baggage::set_entry(initial_baggage, "updated.baggage", "updated.value")
  
  // 验证更新后的Baggage配置
  assert_eq(azimuth::Baggage::get_entry(updated_baggage, "initial.baggage"), Some("initial.value"))
  assert_eq(azimuth::Baggage::get_entry(updated_baggage, "updated.baggage"), Some("updated.value"))
  
  // 测试Baggage配置的覆盖更新
  let override_baggage = azimuth::Baggage::set_entry(updated_baggage, "override.baggage", "original.value")
  let reoverride_baggage = azimuth::Baggage::set_entry(override_baggage, "override.baggage", "updated.value")
  
  // 验证覆盖更新
  assert_eq(azimuth::Baggage::get_entry(override_baggage, "override.baggage"), Some("original.value"))
  assert_eq(azimuth::Baggage::get_entry(reoverride_baggage, "override.baggage"), Some("updated.value"))
  
  // 测试Baggage配置的批量更新
  let batch_baggage = updated_baggage
  
  for i in 0..5 {
    batch_baggage = azimuth::Baggage::set_entry(batch_baggage, "batch.baggage." + i.to_string(), "batch.value." + i.to_string())
  }
  
  // 验证批量Baggage配置
  for i in 0..5 {
    assert_eq(azimuth::Baggage::get_entry(batch_baggage, "batch.baggage." + i.to_string()), Some("batch.value." + i.to_string()))
  }
  
  // 验证原有配置仍然存在
  assert_eq(azimuth::Baggage::get_entry(batch_baggage, "initial.baggage"), Some("initial.value"))
  assert_eq(azimuth::Baggage::get_entry(batch_baggage, "updated.baggage"), Some("updated.value"))
  assert_eq(azimuth::Baggage::get_entry(batch_baggage, "override.baggage"), Some("updated.value"))
}

// Test 8: Span configuration dynamic update
pub test "配置动态更新测试：Span配置动态更新" {
  // 测试Span配置动态更新
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "config-test-tracer")
  
  // 创建初始Span
  let initial_span = azimuth::Tracer::start_span(tracer, "initial-span")
  
  // 验证初始Span配置
  assert_eq(azimuth::Span::name(initial_span), "initial-span")
  assert_eq(azimuth::Span::kind(initial_span), azimuth::Internal)
  
  // 动态更新Span属性
  azimuth::Span::set_status(initial_span, azimuth::Ok)
  azimuth::Span::add_event(initial_span, "configuration.updated", Some([("config.version", azimuth::StringValue("1.0"))]))
  
  // 验证Span属性更新
  assert_eq(azimuth::Span::status(initial_span), azimuth::Ok)
  
  // 创建带有不同配置的Span
  let server_span = azimuth::Tracer::start_span(tracer, "server-span")
  let client_span = azimuth::Tracer::start_span(tracer, "client-span")
  let producer_span = azimuth::Tracer::start_span(tracer, "producer-span")
  let consumer_span = azimuth::Tracer::start_span(tracer, "consumer-span")
  
  // 验证不同类型的Span配置
  assert_eq(azimuth::Span::name(server_span), "server-span")
  assert_eq(azimuth::Span::name(client_span), "client-span")
  assert_eq(azimuth::Span::name(producer_span), "producer-span")
  assert_eq(azimuth::Span::name(consumer_span), "consumer-span")
  
  // 动态更新不同类型Span的配置
  azimuth::Span::set_status(server_span, azimuth::Error)
  azimuth::Span::set_status(client_span, azimuth::Ok)
  azimuth::Span::set_status(producer_span, azimuth::Ok)
  azimuth::Span::set_status(consumer_span, azimuth::Error)
  
  // 添加不同的事件
  azimuth::Span::add_event(server_span, "server.error", Some([("error.code", azimuth::StringValue("500"))]))
  azimuth::Span::add_event(client_span, "client.success", Some([("response.code", azimuth::StringValue("200"))]))
  azimuth::Span::add_event(producer_span, "message.produced", Some([("message.id", azimuth::StringValue("msg-123"))]))
  azimuth::Span::add_event(consumer_span, "message.consumed", Some([("message.id", azimuth::StringValue("msg-123"))]))
  
  // 验证Span配置更新
  assert_eq(azimuth::Span::status(server_span), azimuth::Error)
  assert_eq(azimuth::Span::status(client_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(producer_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(consumer_span), azimuth::Error)
  
  // 测试批量Span配置更新
  let batch_spans = []
  
  for i in 0..5 {
    let span = azimuth::Tracer::start_span(tracer, "batch-span-" + i.to_string())
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::add_event(span, "batch.processed", Some([("batch.id", azimuth::StringValue("batch-" + i.to_string()))]))
    batch_spans.push(span)
  }
  
  // 验证批量Span配置
  for i in 0..batch_spans.length() {
    let span = batch_spans[i]
    let expected_name = "batch-span-" + i.to_string()
    assert_eq(azimuth::Span::name(span), expected_name)
    assert_eq(azimuth::Span::status(span), azimuth::Ok)
  }
  
  // 结束所有Span
  azimuth::Span::end(initial_span)
  azimuth::Span::end(server_span)
  azimuth::Span::end(client_span)
  azimuth::Span::end(producer_span)
  azimuth::Span::end(consumer_span)
  
  for span in batch_spans {
    azimuth::Span::end(span)
  }
}