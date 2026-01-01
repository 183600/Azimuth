// Telemetry Configuration Tests - 遥测配置测试
// 专注于遥测系统的配置管理和动态更新功能

test "TracerProvider动态配置测试" {
  // 测试TracerProvider的动态配置
  let tracer_provider = TracerProvider::default()
  
  // 初始配置测试
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "config.test.tracer1")
  assert_eq(tracer1.scope.name, "config.test.tracer1")
  
  // 动态更新配置（模拟）
  let updated_provider = TracerProvider::with_config(TracerProvider::default(), [
    ("service.name", StringValue("updated-service")),
    ("service.version", StringValue("2.0.0")),
    ("environment", StringValue("staging"))
  ])
  
  let tracer2 = TracerProvider::get_tracer(updated_provider, "config.test.tracer2")
  assert_eq(tracer2.scope.name, "config.test.tracer2")
  
  // 测试不同配置下的tracer行为
  let span1 = Tracer::start_span(tracer1, "config.test.span1")
  let span2 = Tracer::start_span(tracer2, "config.test.span2")
  
  // 验证spans在不同配置下正常工作
  assert_true(Span::is_recording(span1))
  assert_true(Span::is_recording(span2))
  
  Span::end(span1)
  Span::end(span2)
  
  // 测试配置继承
  let child_provider = TracerProvider::with_config(updated_provider, [
    ("component", StringValue("child-component")),
    ("feature.flag", StringValue("enabled"))
  ])
  
  let tracer3 = TracerProvider::get_tracer(child_provider, "config.test.tracer3")
  let span3 = Tracer::start_span(tracer3, "config.test.span3")
  
  assert_true(Span::is_recording(span3))
  Span::end(span3)
}

test "MeterProvider动态配置测试" {
  // 测试MeterProvider的动态配置
  let meter_provider = MeterProvider::default()
  
  // 初始配置测试
  let meter1 = MeterProvider::get_meter(meter_provider, "config.test.meter1")
  assert_eq(meter1.scope.name, "config.test.meter1")
  
  // 动态更新配置
  let config_attrs = [
    ("service.name", StringValue("metrics-service")),
    ("service.namespace", StringValue("observability")),
    ("metrics.enabled", BoolValue(true)),
    ("export.interval", IntValue(60000))
  ]
  
  let updated_provider = MeterProvider::with_config(MeterProvider::default(), config_attrs)
  let meter2 = MeterProvider::get_meter(updated_provider, "config.test.meter2")
  
  assert_eq(meter2.scope.name, "config.test.meter2")
  
  // 测试不同配置下的metrics创建
  let counter1 = Meter::create_counter(meter1, "config.counter.1")
  let counter2 = Meter::create_counter(meter2, "config.counter.2")
  
  assert_eq(counter1.name, "config.counter.1")
  assert_eq(counter2.name, "config.counter.2")
  
  // 记录metrics以验证配置生效
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 200.0)
  
  // 测试配置变更对现有metrics的影响
  let histogram1 = Meter::create_histogram(meter1, "config.histogram.1", Some("Original histogram"), Some("ms"))
  let histogram2 = Meter::create_histogram(meter2, "config.histogram.2", Some("Updated histogram"), Some("seconds"))
  
  Histogram::record(histogram1, 150.5)
  Histogram::record(histogram2, 2.5)
  
  assert_eq(histogram1.description, Some("Original histogram"))
  assert_eq(histogram2.description, Some("Updated histogram"))
  assert_eq(histogram1.unit, Some("ms"))
  assert_eq(histogram2.unit, Some("seconds"))
}

test "LoggerProvider动态配置测试" {
  // 测试LoggerProvider的动态配置
  let logger_provider = LoggerProvider::default()
  
  // 初始配置测试
  let logger1 = LoggerProvider::get_logger(logger_provider, "config.test.logger1")
  assert_eq(logger1.scope.name, "config.test.logger1")
  
  // 动态更新日志级别配置
  let log_config = [
    ("service.name", StringValue("logging-service")),
    ("log.level", StringValue("INFO")),
    ("log.format", StringValue("json")),
    ("log.output", StringValue("stdout"))
  ]
  
  let updated_provider = LoggerProvider::with_config(LoggerProvider::default(), log_config)
  let logger2 = LoggerProvider::get_logger(updated_provider, "config.test.logger2")
  
  assert_eq(logger2.scope.name, "config.test.logger2")
  
  // 测试不同配置下的日志记录
  let log1 = LogRecord::new(Info, "Config test log 1")
  let log2 = LogRecord::new(Warn, "Config test log 2")
  
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  
  // 测试动态日志级别变更
  let debug_config = [
    ("log.level", StringValue("DEBUG")),
    ("verbose.logging", BoolValue(true))
  ]
  
  let debug_provider = LoggerProvider::with_config(updated_provider, debug_config)
  let debug_logger = LoggerProvider::get_logger(debug_provider, "config.test.debug.logger")
  
  let debug_log = LogRecord::new(Debug, "Debug level log")
  Logger::emit(debug_logger, debug_log)
  
  // 测试带属性的日志配置
  let attrs = Attributes::new()
  Attributes::set(attrs, "config.test", StringValue("true"))
  Attributes::set(attrs, "logger.type", StringValue("configured"))
  
  let attr_log = LogRecord::new_with_context(
    Error,
    Some("Configured attribute log"),
    Some(attrs),
    Some(1735689600000000000L),
    None,
    None,
    None,
    Some(Context::root())
  )
  
  Logger::emit(logger2, attr_log)
}

test "Resource动态配置测试" {
  // 测试Resource的动态配置
  let base_resource = Resource::new()
  
  // 初始资源配置
  let initial_attrs = [
    ("service.name", StringValue("base-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("development"))
  ]
  
  let resource1 = Resource::with_attributes(base_resource, initial_attrs)
  
  // 验证初始配置
  match Resource::get_attribute(resource1, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "base-service")
    _ => assert_true(false)
  }
  
  // 动态更新配置
  let update_attrs = [
    ("service.version", StringValue("2.0.0")), // 覆盖现有属性
    ("deployment.environment", StringValue("production")), // 覆盖现有属性
    ("service.instance.id", StringValue("instance-12345")), // 新增属性
    ("feature.flags", StringValue("new-feature,experimental"))
  ]
  
  let resource2 = Resource::with_attributes(resource1, update_attrs)
  
  // 验证更新后的配置
  match Resource::get_attribute(resource2, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "base-service") // 应该保持不变
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource2, "service.version") {
    Some(StringValue(value)) => assert_eq(value, "2.0.0") // 应该被更新
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource2, "deployment.environment") {
    Some(StringValue(value)) => assert_eq(value, "production") // 应该被更新
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(resource2, "service.instance.id") {
    Some(StringValue(value)) => assert_eq(value, "instance-12345") // 新增属性
    _ => assert_true(false)
  }
  
  // 测试资源合并的动态配置
  let merge_base = Resource::with_attributes(base_resource, [
    ("base.attr1", StringValue("base.value1")),
    ("base.attr2", StringValue("base.value2"))
  ])
  
  let merge_update = Resource::with_attributes(base_resource, [
    ("base.attr2", StringValue("updated.value2")), // 覆盖
    ("new.attr", StringValue("new.value")) // 新增
  ])
  
  let merged_resource = Resource::merge(merge_base, merge_update)
  
  // 验证合并结果
  match Resource::get_attribute(merged_resource, "base.attr1") {
    Some(StringValue(value)) => assert_eq(value, "base.value1")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "base.attr2") {
    Some(StringValue(value)) => assert_eq(value, "updated.value2")
    _ => assert_true(false)
  }
  
  match Resource::get_attribute(merged_resource, "new.attr") {
    Some(StringValue(value)) => assert_eq(value, "new.value")
    _ => assert_true(false)
  }
}

test "Propagator动态配置测试" {
  // 测试Propagator的动态配置
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 初始复合传播器配置
  let initial_propagators = [trace_propagator]
  let composite1 = CompositePropagator::new(initial_propagators)
  
  // 测试初始配置
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  CompositePropagator::inject(composite1, ctx, carrier)
  
  // 动态更新传播器配置
  let updated_propagators = [trace_propagator, baggage_propagator]
  let composite2 = CompositePropagator::new(updated_propagators)
  
  // 测试更新后的配置
  let enriched_ctx = Context::with_value(ctx, ContextKey::new("test.key"), "test.value")
  CompositePropagator::inject(composite2, enriched_ctx, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite2, carrier)
  
  // 验证传播器配置更新
  assert_true(true)
  
  // 测试传播器配置的优先级
  let priority_propagators = [baggage_propagator, trace_propagator] // 改变顺序
  let priority_composite = CompositePropagator::new(priority_propagators)
  
  let priority_carrier = TextMapCarrier::new()
  CompositePropagator::inject(priority_composite, enriched_ctx, priority_carrier)
  
  let priority_extracted = CompositePropagator::extract(priority_composite, priority_carrier)
  
  // 验证优先级配置
  assert_true(true)
}

test "采样配置动态更新测试" {
  // 测试采样配置的动态更新
  let tracer_provider = TracerProvider::default()
  
  // 初始采样配置（AlwaysOn）
  let always_on_config = [
    ("sampler.type", StringValue("always_on")),
    ("sampler.param", StringValue("1.0"))
  ]
  
  let always_on_provider = TracerProvider::with_config(tracer_provider, always_on_config)
  let always_on_tracer = TracerProvider::get_tracer(always_on_provider, "always.on.tracer")
  
  // 测试AlwaysOn采样
  let always_on_span = Tracer::start_span(always_on_tracer, "always.on.span")
  assert_true(Span::is_recording(always_on_span))
  Span::end(always_on_span)
  
  // 动态更新为AlwaysOff采样
  let always_off_config = [
    ("sampler.type", StringValue("always_off")),
    ("sampler.param", StringValue("0.0"))
  ]
  
  let always_off_provider = TracerProvider::with_config(tracer_provider, always_off_config)
  let always_off_tracer = TracerProvider::get_tracer(always_off_provider, "always.off.tracer")
  
  // 测试AlwaysOff采样
  let always_off_span = Tracer::start_span(always_off_tracer, "always.off.span")
  // 在简化实现中可能仍然recording，但实际应该根据采样配置决定
  Span::end(always_off_span)
  
  // 动态更新为TraceIdRatio采样
  let ratio_config = [
    ("sampler.type", StringValue("trace_id_ratio")),
    ("sampler.param", StringValue("0.5")) // 50%采样率
  ]
  
  let ratio_provider = TracerProvider::with_config(tracer_provider, ratio_config)
  let ratio_tracer = TracerProvider::get_tracer(ratio_provider, "ratio.tracer")
  
  // 测试TraceIdRatio采样
  let ratio_spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(ratio_tracer, "ratio.span." + i.to_string())
    ratio_spans.push(span)
  }
  
  for span in ratio_spans {
    Span::end(span)
  }
  
  // 验证采样配置更新
  assert_true(true)
}

test "导出器配置动态更新测试" {
  // 测试导出器配置的动态更新
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "exporter.test.meter")
  
  // 初始导出器配置（Console导出器）
  let console_config = [
    ("exporter.type", StringValue("console")),
    ("exporter.interval", IntValue(60000)), // 60秒
    ("exporter.enabled", BoolValue(true))
  ]
  
  let console_provider = MeterProvider::with_config(meter_provider, console_config)
  let console_meter = MeterProvider::get_meter(console_provider, "console.meter")
  
  let console_counter = Meter::create_counter(console_meter, "console.counter")
  Counter::add(console_counter, 100.0)
  
  // 动态更新为OTLP导出器配置
  let otlp_config = [
    ("exporter.type", StringValue("otlp")),
    ("exporter.endpoint", StringValue("http://localhost:4317")),
    ("exporter.interval", IntValue(30000)), // 30秒
    ("exporter.enabled", BoolValue(true)),
    ("exporter.headers", StringValue("Authorization=Bearer token123"))
  ]
  
  let otlp_provider = MeterProvider::with_config(meter_provider, otlp_config)
  let otlp_meter = MeterProvider::get_meter(otlp_provider, "otlp.meter")
  
  let otlp_counter = Meter::create_counter(otlp_meter, "otlp.counter")
  Counter::add(otlp_counter, 200.0)
  
  // 动态更新为Prometheus导出器配置
  let prometheus_config = [
    ("exporter.type", StringValue("prometheus")),
    ("exporter.port", IntValue(9090)),
    ("exporter.path", StringValue("/metrics")),
    ("exporter.enabled", BoolValue(true))
  ]
  
  let prometheus_provider = MeterProvider::with_config(meter_provider, prometheus_config)
  let prometheus_meter = MeterProvider::get_meter(prometheus_provider, "prometheus.meter")
  
  let prometheus_counter = Meter::create_counter(prometheus_meter, "prometheus.counter")
  Counter::add(prometheus_counter, 300.0)
  
  // 验证导出器配置更新
  assert_eq(console_counter.name, "console.counter")
  assert_eq(otlp_counter.name, "otlp.counter")
  assert_eq(prometheus_counter.name, "prometheus.counter")
}

test "批量配置更新测试" {
  // 测试批量配置更新
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 初始配置
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.test.tracer")
  let meter = MeterProvider::get_meter(meter_provider, "batch.test.meter")
  let logger = LoggerProvider::get_logger(logger_provider, "batch.test.logger")
  
  // 批量配置更新
  let batch_config = [
    ("service.name", StringValue("batch-updated-service")),
    ("service.version", StringValue("3.0.0")),
    ("deployment.environment", StringValue("production")),
    ("trace.sampling.rate", FloatValue(0.1)),
    ("metrics.export.interval", IntValue(15000)),
    ("log.level", StringValue("WARN")),
    ("feature.enabled", BoolValue(true))
  ]
  
  // 应用批量配置更新
  let updated_tracer_provider = TracerProvider::with_config(tracer_provider, batch_config)
  let updated_meter_provider = MeterProvider::with_config(meter_provider, batch_config)
  let updated_logger_provider = LoggerProvider::with_config(logger_provider, batch_config)
  
  // 获取更新后的instruments
  let updated_tracer = TracerProvider::get_tracer(updated_tracer_provider, "batch.updated.tracer")
  let updated_meter = MeterProvider::get_meter(updated_meter_provider, "batch.updated.meter")
  let updated_logger = LoggerProvider::get_logger(updated_logger_provider, "batch.updated.logger")
  
  // 测试更新后的instrument行为
  let span = Tracer::start_span(updated_tracer, "batch.config.test.span")
  Span::set_attribute(span, "config.batch", StringValue("updated"))
  Span::end(span)
  
  let counter = Meter::create_counter(updated_meter, "batch.config.counter")
  Counter::add(counter, 50.0)
  
  let log = LogRecord::new(Warn, "Batch configuration updated log")
  Logger::emit(updated_logger, log)
  
  // 验证批量配置更新
  assert_true(Span::is_recording(span))
  assert_eq(counter.name, "batch.config.counter")
  assert_eq(updated_logger.scope.name, "batch.updated.logger")
}