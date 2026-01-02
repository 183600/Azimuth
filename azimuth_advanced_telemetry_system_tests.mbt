// Azimuth Advanced Telemetry System Tests
// 高级遥测系统测试用例 - 专注于核心遥测功能的深度测试

// 测试1: 资源管理测试
test "资源管理功能测试" {
  let resource1 = Resource::create()
  let resource2 = Resource::create()
  
  // 设置资源属性
  Resource::set_attribute(resource1, "service.name", "azimuth-service")
  Resource::set_attribute(resource1, "service.version", "1.0.0")
  Resource::set_attribute(resource1, "deployment.environment", "production")
  
  Resource::set_attribute(resource2, "host.name", "server-01")
  Resource::set_attribute(resource2, "host.id", "host-12345")
  Resource::set_attribute(resource2, "os.type", "linux")
  
  // 合并资源
  let merged_resource = Resource::merge(resource1, resource2)
  
  // 验证合并后的属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), "azimuth-service")
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), "1.0.0")
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), "production")
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), "server-01")
  assert_eq(Resource::get_attribute(merged_resource, "host.id"), "host-12345")
  assert_eq(Resource::get_attribute(merged_resource, "os.type"), "linux")
  
  // 验证资源有效性
  assert_true(Resource::is_valid(merged_resource))
}

// 测试2: 日志记录测试
test "日志记录功能测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger")
  
  // 创建日志记录器
  let log_record1 = Logger::create_log_record(logger, LogLevel::INFO)
  let log_record2 = Logger::create_log_record(logger, LogLevel::WARN)
  let log_record3 = Logger::create_log_record(logger, LogLevel::ERROR)
  
  // 设置日志属性
  LogRecord::set_body(log_record1, "Application started successfully")
  LogRecord::set_attribute(log_record1, "event.id", "evt-001")
  LogRecord::set_attribute(log_record1, "user.id", "user-123")
  
  LogRecord::set_body(log_record2, "High memory usage detected")
  LogRecord::set_attribute(log_record2, "memory.usage", "85%")
  LogRecord::set_attribute(log_record2, "threshold", "80%")
  
  LogRecord::set_body(log_record3, "Database connection failed")
  LogRecord::set_attribute(log_record3, "error.code", "DB_CONN_FAILED")
  LogRecord::set_attribute(log_record3, "retry.count", "3")
  
  // 发出日志
  Logger::emit(log_record1)
  Logger::emit(log_record2)
  Logger::emit(log_record3)
  
  // 验证日志级别
  assert_eq(LogRecord::get_severity(log_record1), LogLevel::INFO)
  assert_eq(LogRecord::get_severity(log_record2), LogLevel::WARN)
  assert_eq(LogRecord::get_severity(log_record3), LogLevel::ERROR)
  
  // 验证日志内容
  assert_eq(LogRecord::get_body(log_record1), "Application started successfully")
  assert_eq(LogRecord::get_body(log_record2), "High memory usage detected")
  assert_eq(LogRecord::get_body(log_record3), "Database connection failed")
}

// 测试3: 链路追踪测试
test "链路追踪功能测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "trace.test")
  
  // 创建父span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  Span::set_attribute(parent_span, "operation.type", "parent")
  Span::set_attribute(parent_span, "user.id", "user-456")
  
  // 创建子span
  let child_span1 = Tracer::start_span(tracer, "child.operation.1")
  Span::set_attribute(child_span1, "operation.type", "child")
  Span::set_attribute(child_span1, "service.name", "auth-service")
  
  let child_span2 = Tracer::start_span(tracer, "child.operation.2")
  Span::set_attribute(child_span2, "operation.type", "child")
  Span::set_attribute(child_span2, "service.name", "data-service")
  
  // 创建孙span
  let grandchild_span = Tracer::start_span(tracer, "grandchild.operation")
  Span::set_attribute(grandchild_span, "operation.type", "grandchild")
  Span::set_attribute(grandchild_span, "database.query", "SELECT * FROM users")
  
  // 设置span状态
  Span::set_status(parent_span, Ok)
  Span::set_status(child_span1, Ok)
  Span::set_status(child_span2, Error("Timeout occurred"))
  Span::set_status(grandchild_span, Ok)
  
  // 结束span（按正确顺序）
  Span::end(grandchild_span)
  Span::end(child_span2)
  Span::end(child_span1)
  Span::end(parent_span)
  
  // 验证span属性
  assert_eq(Span::get_attribute(parent_span, "operation.type"), "parent")
  assert_eq(Span::get_attribute(child_span1, "service.name"), "auth-service")
  assert_eq(Span::get_attribute(grandchild_span, "database.query"), "SELECT * FROM users")
  
  // 验证span状态
  assert_true(Span::is_finished(parent_span))
  assert_true(Span::is_finished(child_span1))
  assert_true(Span::is_finished(child_span2))
  assert_true(Span::is_finished(grandchild_span))
}

// 测试4: 指标聚合测试
test "指标聚合功能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics.aggregation.test")
  
  // 创建指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let active_gauge = Meter::create_gauge(meter, "active.connections")
  let updown_counter = Meter::create_updown_counter(meter, "active.sessions")
  
  // 添加计数器数据
  Counter::add(request_counter, 10.0, [ ("method", "GET"), ("status", "200") ])
  Counter::add(request_counter, 5.0, [ ("method", "POST"), ("status", "201") ])
  Counter::add(request_counter, 2.0, [ ("method", "GET"), ("status", "404") ])
  Counter::add(request_counter, 1.0, [ ("method", "POST"), ("status", "500") ])
  
  // 添加直方图数据
  Histogram::record(response_histogram, 0.1, [ ("endpoint", "/api/users") ])
  Histogram::record(response_histogram, 0.2, [ ("endpoint", "/api/users") ])
  Histogram::record(response_histogram, 0.15, [ ("endpoint", "/api/orders") ])
  Histogram::record(response_histogram, 0.3, [ ("endpoint", "/api/orders") ])
  
  // 设置仪表数据
  Gauge::set(active_gauge, 25.0, [ ("server", "web-01") ])
  Gauge::set(active_gauge, 30.0, [ ("server", "web-02") ])
  
  // 添加上下计数器数据
  UpDownCounter::add(updown_counter, 15.0, [ ("session.type", "user") ])
  UpDownCounter::add(updown_counter, -3.0, [ ("session.type", "user") ])
  UpDownCounter::add(updown_counter, 8.0, [ ("session.type", "admin") ])
  
  // 聚合指标数据
  let aggregated_data = Meter::collect_metrics(meter)
  
  // 验证聚合结果
  assert_true(MetricData::has_counter(aggregated_data, "http.requests.total"))
  assert_true(MetricData::has_histogram(aggregated_data, "http.response.duration"))
  assert_true(MetricData::has_gauge(aggregated_data, "active.connections"))
  assert_true(MetricData::has_updown_counter(aggregated_data, "active.sessions"))
  
  // 验证具体值
  assert_eq(MetricData::get_counter_sum(aggregated_data, "http.requests.total"), 18.0)
  assert_eq(MetricData::get_gauge_value(aggregated_data, "active.connections", [ ("server", "web-01") ]), 25.0)
  assert_eq(MetricData::get_updown_counter_value(aggregated_data, "active.sessions", [ ("session.type", "user") ]), 12.0)
}

// 测试5: 采样策略测试
test "采样策略功能测试" {
  let sampler_provider = SamplerProvider::default()
  
  // 创建不同类型的采样器
  let always_on_sampler = SamplerProvider::create_always_on_sampler(sampler_provider)
  let always_off_sampler = SamplerProvider::create_always_off_sampler(sampler_provider)
  let trace_id_ratio_sampler = SamplerProvider::create_trace_id_ratio_sampler(sampler_provider, 0.5)
  
  // 创建测试上下文
  let context1 = SamplingContext::create("trace-id-001", "span-id-001")
  let context2 = SamplingContext::create("trace-id-002", "span-id-002")
  let context3 = SamplingContext::create("trace-id-003", "span-id-003")
  
  // 测试AlwaysOn采样器
  let decision1 = Sampler::should_sample(always_on_sampler, context1)
  assert_eq(SamplingDecision::get_decision(decision1), Decision::RecordAndSample)
  
  // 测试AlwaysOff采样器
  let decision2 = Sampler::should_sample(always_off_sampler, context2)
  assert_eq(SamplingDecision::get_decision(decision2), Decision::Drop)
  
  // 测试TraceIdRatio采样器
  let decision3 = Sampler::should_sample(trace_id_ratio_sampler, context3)
  // 结果取决于trace-id，但应该是有效的决策
  assert_true(SamplingDecision::is_valid(decision3))
  
  // 验证采样属性
  let attributes = SamplingDecision::get_attributes(decision1)
  assert_true(AttributeList::is_valid(attributes))
}

// 测试6: 传播器测试
test "传播器功能测试" {
  let propagator_provider = PropagatorProvider::default()
  let composite_propagator = PropagatorProvider::create_composite_propagator(propagator_provider)
  
  // 创建测试上下文
  let context = Context::create()
  let span_context = SpanContext::create("trace-id-123", "span-id-456", true, true)
  Context::set_span_context(context, span_context)
  
  // 设置baggage
  Baggage::set(context, "user.id", "user-789")
  Baggage::set(context, "request.id", "req-12345")
  Baggage::set(context, "session.id", "sess-98765")
  
  // 注入传播
  let carrier = TextMapCarrier::create()
  Propagator::inject(composite_propagator, context, carrier)
  
  // 验证注入的数据
  assert_true(TextMapCarrier::has_key(carrier, "traceparent"))
  assert_true(TextMapCarrier::has_key(carrier, "baggage"))
  
  // 提取传播
  let extracted_context = Context::create()
  Propagator::extract(composite_propagator, extracted_context, carrier)
  
  // 验证提取的上下文
  let extracted_span_context = Context::get_span_context(extracted_context)
  assert_true(SpanContext::is_valid(extracted_span_context))
  assert_eq(SpanContext::get_trace_id(extracted_span_context), "trace-id-123")
  assert_eq(SpanContext::get_span_id(extracted_span_context), "span-id-456")
  
  // 验证提取的baggage
  assert_eq(Baggage::get(extracted_context, "user.id"), "user-789")
  assert_eq(Baggage::get(extracted_context, "request.id"), "req-12345")
  assert_eq(Baggage::get(extracted_context, "session.id"), "sess-98765")
}

// 测试7: 批处理测试
test "批处理功能测试" {
  let batch_processor = BatchProcessor::create(100, 5000) // 100 items or 5 seconds
  
  // 创建测试数据
  let span1 = TestSpan::create("span-1", "operation-1")
  let span2 = TestSpan::create("span-2", "operation-2")
  let span3 = TestSpan::create("span-3", "operation-3")
  let span4 = TestSpan::create("span-4", "operation-4")
  let span5 = TestSpan::create("span-5", "operation-5")
  
  // 添加到批处理器
  BatchProcessor::add(batch_processor, span1)
  BatchProcessor::add(batch_processor, span2)
  BatchProcessor::add(batch_processor, span3)
  BatchProcessor::add(batch_processor, span4)
  BatchProcessor::add(batch_processor, span5)
  
  // 验证批处理状态
  assert_eq(BatchProcessor::get_pending_count(batch_processor), 5)
  assert_false(BatchProcessor::is_ready(batch_processor)) // 不到100个
  
  // 强制刷新
  BatchProcessor::flush(batch_processor)
  
  // 验证刷新后状态
  assert_eq(BatchProcessor::get_pending_count(batch_processor), 0)
  assert_true(BatchProcessor::is_flushed(batch_processor))
  
  // 测试批处理配置
  let config = BatchProcessor::get_config(batch_processor)
  assert_eq(Config::get_max_batch_size(config), 100)
  assert_eq(Config::get_max_export_timeout(config), 5000)
  assert_true(Config::is_valid(config))
}

// 测试8: 配置管理测试
test "配置管理功能测试" {
  let config_manager = ConfigManager::create()
  
  // 设置配置项
  ConfigManager::set(config_manager, "telemetry.enabled", "true")
  ConfigManager::set(config_manager, "telemetry.service.name", "azimuth-test")
  ConfigManager::set(config_manager, "telemetry.service.version", "1.0.0")
  ConfigManager::set(config_manager, "telemetry.sampling.probability", "0.1")
  ConfigManager::set(config_manager, "telemetry.exporter.endpoint", "http://localhost:4317")
  
  // 获取配置项
  assert_eq(ConfigManager::get(config_manager, "telemetry.enabled"), "true")
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.name"), "azimuth-test")
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.version"), "1.0.0")
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling.probability"), "0.1")
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporter.endpoint"), "http://localhost:4317")
  
  // 测试配置更新
  ConfigManager::update(config_manager, "telemetry.sampling.probability", "0.5")
  assert_eq(ConfigManager::get(config_manager, "telemetry.sampling.probability"), "0.5")
  
  // 测试配置删除
  ConfigManager::remove(config_manager, "telemetry.exporter.endpoint")
  assert_eq(ConfigManager::get(config_manager, "telemetry.exporter.endpoint"), "")
  
  // 测试配置验证
  let validation_result = ConfigManager::validate(config_manager)
  assert_true(ValidationResult::is_valid(validation_result))
  assert_eq(ValidationResult::get_error_count(validation_result), 0)
  
  // 测试配置导出
  let exported_config = ConfigManager::export(config_manager)
  assert_true(ConfigExport::contains(exported_config, "telemetry.enabled"))
  assert_true(ConfigExport::contains(exported_config, "telemetry.service.name"))
  assert_true(ConfigExport::contains(exported_config, "telemetry.service.version"))
  assert_true(ConfigExport::contains(exported_config, "telemetry.sampling.probability"))
}