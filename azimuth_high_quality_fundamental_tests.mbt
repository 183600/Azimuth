// Azimuth 高质量基础功能测试用例
// 专注于遥测系统的基础功能和核心操作测试

// 测试1: 遥测数据创建和验证
test "遥测数据创建和基本验证" {
  // 创建遥测数据点
  let telemetry_data = TelemetryData::new()
  
  // 设置基本信息
  TelemetryData::set_trace_id(telemetry_data, "trace-001")
  TelemetryData::set_span_id(telemetry_data, "span-001")
  TelemetryData::set_timestamp(telemetry_data, Time::now_unix_nanos())
  
  // 添加基本属性
  TelemetryData::add_attribute(telemetry_data, "service.name", "payment-service")
  TelemetryData::add_attribute(telemetry_data, "operation.name", "process-payment")
  TelemetryData::add_attribute(telemetry_data, "user.id", "user-12345")
  TelemetryData::add_attribute(telemetry_data, "transaction.id", "txn-abc123")
  
  // 添加数值属性
  TelemetryData::add_attribute(telemetry_data, "response.time", 125.5)
  TelemetryData::add_attribute(telemetry_data, "status.code", 200)
  TelemetryData::add_attribute(telemetry_data, "retry.count", 0)
  
  // 验证数据完整性
  assert_eq(TelemetryData::get_trace_id(telemetry_data), Some("trace-001"))
  assert_eq(TelemetryData::get_span_id(telemetry_data), Some("span-001"))
  assert_true(TelemetryData::get_timestamp(telemetry_data) > 0)
  
  // 验证属性
  assert_eq(TelemetryData::get_attribute(telemetry_data, "service.name"), Some("payment-service"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "operation.name"), Some("process-payment"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "user.id"), Some("user-12345"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "transaction.id"), Some("txn-abc123"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "response.time"), Some(125.5))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "status.code"), Some(200))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "retry.count"), Some(0))
  
  // 验证属性数量
  let attributes = TelemetryData::get_all_attributes(telemetry_data)
  assert_eq(attributes.length(), 7)
}

// 测试2: 指标收集器基础功能
test "指标收集器基础功能测试" {
  // 创建指标收集器
  let collector = MetricsCollector::new("test-collector")
  
  // 创建计数器指标
  let request_counter = MetricsCollector::create_counter(collector, "http.requests.total", [
    ("method", "GET"),
    ("endpoint", "/api/users")
  ])
  
  // 创建直方图指标
  let latency_histogram = MetricsCollector::create_histogram(collector, "http.request.duration", [
    ("method", "GET"),
    ("endpoint", "/api/users")
  ])
  
  // 创建仪表指标
  let active_connections = MetricsCollector::create_gauge(collector, "http.connections.active")
  
  // 记录指标数据
  Counter::add(request_counter, 10.0)
  Counter::add(request_counter, 5.0)
  
  Histogram::record(latency_histogram, 0.125)
  Histogram::record(latency_histogram, 0.250)
  Histogram::record(latency_histogram, 0.075)
  
  Gauge::set(active_connections, 25.0)
  Gauge::set(active_connections, 30.0)
  
  // 验证指标数据
  assert_eq(Counter::value(request_counter), 15.0)
  assert_eq(Gauge::value(active_connections), 30.0)
  
  // 验证直方图统计
  let histogram_stats = Histogram::get_stats(latency_histogram)
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 0.125 + 0.250 + 0.075)
  assert_true(histogram_stats.min <= 0.075)
  assert_true(histogram_stats.max >= 0.250)
  
  // 验证收集器状态
  let metrics = MetricsCollector::get_all_metrics(collector)
  assert_eq(metrics.length(), 3)
}

// 测试3: 追踪上下文管理
test "追踪上下文管理基础测试" {
  // 创建追踪提供者
  let tracer_provider = TracerProvider::new("test-provider")
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "root-operation")
  let root_context = Span::get_context(root_span)
  
  // 验证根span上下文
  assert_true(Context::is_valid(root_context))
  assert_true(Context::get_trace_id(root_context).length() > 0)
  assert_true(Context::get_span_id(root_context).length() > 0)
  
  // 创建子span
  let child_span = Tracer::start_span_with_parent(tracer, "child-operation", root_span)
  let child_context = Span::get_context(child_span)
  
  // 验证父子关系
  assert_eq(Context::get_trace_id(root_context), Context::get_trace_id(child_context))
  assert_ne(Context::get_span_id(root_context), Context::get_span_id(child_context))
  assert_eq(Context::get_parent_span_id(child_context), Some(Context::get_span_id(root_context)))
  
  // 添加span属性
  Span::set_attribute(child_span, "operation.type", "database")
  Span::set_attribute(child_span, "db.statement", "SELECT * FROM users")
  Span::set_attribute(child_span, "db.rows", 100)
  
  // 添加span事件
  Span::add_event(child_span, "query.start", [
    ("timestamp", Time::now_unix_nanos()),
    ("query", "SELECT * FROM users")
  ])
  
  Span::add_event(child_span, "query.complete", [
    ("timestamp", Time::now_unix_nanos()),
    ("rows.returned", 100),
    ("duration", 0.025)
  ])
  
  // 验证span属性
  assert_eq(Span::get_attribute(child_span, "operation.type"), Some("database"))
  assert_eq(Span::get_attribute(child_span, "db.statement"), Some("SELECT * FROM users"))
  assert_eq(Span::get_attribute(child_span, "db.rows"), Some(100))
  
  // 验证span事件
  let events = Span::get_events(child_span)
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "query.start")
  assert_eq(events[1].name, "query.complete")
  
  // 结束spans
  Span::end(child_span)
  Span::end(root_span)
  
  // 验证span状态
  assert_eq(Span::get_status(child_span), SpanStatus::Ok)
  assert_eq(Span::get_status(root_span), SpanStatus::Ok)
}

// 测试4: 日志记录器基础功能
test "日志记录器基础功能测试" {
  // 创建日志提供者
  let logger_provider = LoggerProvider::new("test-provider")
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // 创建日志记录
  let log_record = Logger::create_log_record(logger)
  
  // 设置日志属性
  LogRecord::set_severity(log_record, LogSeverity::Info)
  LogRecord::set_body(log_record, "User authentication successful")
  LogRecord::set_timestamp(log_record, Time::now_unix_nanos())
  
  // 添加日志属性
  LogRecord::add_attribute(log_record, "user.id", "user-12345")
  LogRecord::add_attribute(log_record, "auth.method", "oauth2")
  LogRecord::add_attribute(log_record, "response.time", 150)
  LogRecord::add_attribute(log_record, "ip.address", "192.168.1.100")
  LogRecord::add_attribute(log_record, "user.agent", "Mozilla/5.0")
  
  // 发送日志
  Logger::emit(log_record)
  
  // 验证日志属性
  assert_eq(LogRecord::get_severity(log_record), LogSeverity::Info)
  assert_eq(LogRecord::get_body(log_record), Some("User authentication successful"))
  assert_true(LogRecord::get_timestamp(log_record) > 0)
  
  // 验证日志属性
  assert_eq(LogRecord::get_attribute(log_record, "user.id"), Some("user-12345"))
  assert_eq(LogRecord::get_attribute(log_record, "auth.method"), Some("oauth2"))
  assert_eq(LogRecord::get_attribute(log_record, "response.time"), Some(150))
  assert_eq(LogRecord::get_attribute(log_record, "ip.address"), Some("192.168.1.100"))
  assert_eq(LogRecord::get_attribute(log_record, "user.agent"), Some("Mozilla/5.0"))
  
  // 测试不同严重级别的日志
  let debug_log = Logger::create_log_record(logger)
  LogRecord::set_severity(debug_log, LogSeverity::Debug)
  LogRecord::set_body(debug_log, "Debug information")
  
  let warn_log = Logger::create_log_record(logger)
  LogRecord::set_severity(warn_log, LogSeverity::Warn)
  LogRecord::set_body(warn_log, "Warning message")
  
  let error_log = Logger::create_log_record(logger)
  LogRecord::set_severity(error_log, LogSeverity::Error)
  LogRecord::set_body(error_log, "Error occurred")
  
  // 验证严重级别
  assert_eq(LogRecord::get_severity(debug_log), LogSeverity::Debug)
  assert_eq(LogRecord::get_severity(warn_log), LogSeverity::Warn)
  assert_eq(LogRecord::get_severity(error_log), LogSeverity::Error)
}

// 测试5: 资源管理基础功能
test "资源管理基础功能测试" {
  // 创建基础资源
  let resource = Resource::new()
  
  // 添加服务属性
  Resource::add_attribute(resource, "service.name", "api-gateway")
  Resource::add_attribute(resource, "service.version", "1.2.3")
  Resource::add_attribute(resource, "service.instance.id", "instance-abc123")
  
  // 添加主机属性
  Resource::add_attribute(resource, "host.name", "prod-web-01")
  Resource::add_attribute(resource, "host.arch", "amd64")
  Resource::add_attribute(resource, "os.type", "linux")
  Resource::add_attribute(resource, "os.version", "ubuntu-20.04")
  
  // 添加进程属性
  Resource::add_attribute(resource, "process.pid", 12345)
  Resource::add_attribute(resource, "process.executable.name", "api-gateway")
  Resource::add_attribute(resource, "process.command_line", "./api-gateway --port=8080")
  
  // 验证资源属性
  assert_eq(Resource::get_attribute(resource, "service.name"), Some("api-gateway"))
  assert_eq(Resource::get_attribute(resource, "service.version"), Some("1.2.3"))
  assert_eq(Resource::get_attribute(resource, "service.instance.id"), Some("instance-abc123"))
  assert_eq(Resource::get_attribute(resource, "host.name"), Some("prod-web-01"))
  assert_eq(Resource::get_attribute(resource, "host.arch"), Some("amd64"))
  assert_eq(Resource::get_attribute(resource, "os.type"), Some("linux"))
  assert_eq(Resource::get_attribute(resource, "os.version"), Some("ubuntu-20.04"))
  assert_eq(Resource::get_attribute(resource, "process.pid"), Some(12345))
  assert_eq(Resource::get_attribute(resource, "process.executable.name"), Some("api-gateway"))
  assert_eq(Resource::get_attribute(resource, "process.command_line"), Some("./api-gateway --port=8080"))
  
  // 验证属性数量
  let attributes = Resource::get_all_attributes(resource)
  assert_eq(attributes.length(), 10)
  
  // 测试资源合并
  let additional_resource = Resource::new()
  Resource::add_attribute(additional_resource, "deployment.environment", "production")
  Resource::add_attribute(additional_resource, "telemetry.sdk.name", "azimuth")
  Resource::add_attribute(additional_resource, "telemetry.sdk.version", "1.0.0")
  
  let merged_resource = Resource::merge(resource, additional_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some("api-gateway"))
  assert_eq(Resource::get_attribute(merged_resource, "deployment.environment"), Some("production"))
  assert_eq(Resource::get_attribute(merged_resource, "telemetry.sdk.name"), Some("azimuth"))
  
  // 验证合并后的属性数量
  let merged_attributes = Resource::get_all_attributes(merged_resource)
  assert_eq(merged_attributes.length(), 13)
}

// 测试6: 上下文传播基础功能
test "上下文传播基础功能测试" {
  // 创建根上下文
  let root_ctx = Context::root()
  
  // 添加追踪上下文
  let trace_ctx = Context::with_trace_id(root_ctx, "trace-789")
  let span_ctx = Context::with_span_id(trace_ctx, "span-456")
  
  // 添加 baggage 项
  let user_id_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(span_ctx, user_id_key, "user-123")
  
  let session_id_key = ContextKey::new("session.id")
  let ctx_with_session = Context::with_value(ctx_with_user, session_id_key, "session-abc")
  
  let request_id_key = ContextKey::new("request.id")
  let final_ctx = Context::with_value(ctx_with_session, request_id_key, "req-xyz")
  
  // 验证上下文值
  assert_eq(Context::get_value(final_ctx, user_id_key), Some("user-123"))
  assert_eq(Context::get_value(final_ctx, session_id_key), Some("session-abc"))
  assert_eq(Context::get_value(final_ctx, request_id_key), Some("req-xyz"))
  
  // 验证追踪信息
  assert_eq(Context::get_trace_id(final_ctx), Some("trace-789"))
  assert_eq(Context::get_span_id(final_ctx), Some("span-456"))
  
  // 创建传播器
  let propagator = TextMapPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  Propagator::inject(propagator, final_ctx, carrier)
  
  // 验证注入结果
  let headers = TextMapCarrier::get_all(carrier)
  assert_true(headers.length() > 0)
  assert_true(TextMapCarrier::contains(carrier, "traceparent"))
  assert_true(TextMapCarrier::contains(carrier, "baggage"))
  
  // 提取上下文
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // 验证提取结果
  assert_eq(Context::get_trace_id(extracted_ctx), Some("trace-789"))
  assert_eq(Context::get_span_id(extracted_ctx), Some("span-456"))
  assert_eq(Context::get_value(extracted_ctx, user_id_key), Some("user-123"))
  assert_eq(Context::get_value(extracted_ctx, session_id_key), Some("session-abc"))
  assert_eq(Context::get_value(extracted_ctx, request_id_key), Some("req-xyz"))
}

// 测试7: 采样器基础功能
test "采样器基础功能测试" {
  // 创建AlwaysOn采样器
  let always_on_sampler = Sampler::always_on()
  
  let sampling_context = SamplingContext::new()
  SamplingContext::set_trace_id(sampling_context, "trace-001")
  SamplingContext::set_span_name(sampling_context, "test-span")
  
  // 测试AlwaysOn采样器
  let always_on_result = Sampler::should_sample(always_on_sampler, sampling_context)
  assert_eq(always_on_result.decision, SamplingDecision::RecordAndSample)
  assert_true(always_on_result.attributes.is_empty())
  
  // 创建AlwaysOff采样器
  let always_off_sampler = Sampler::always_off()
  
  // 测试AlwaysOff采样器
  let always_off_result = Sampler::should_sample(always_off_sampler, sampling_context)
  assert_eq(always_off_result.decision, SamplingDecision::Drop)
  assert_true(always_off_result.attributes.is_empty())
  
  // 创建TraceIdRatio采样器
  let ratio_sampler = Sampler::trace_id_ratio(0.5) // 50%采样率
  
  // 测试TraceIdRatio采样器
  let ratio_result = Sampler::should_sample(ratio_sampler, sampling_context)
  assert_true(ratio_result.decision == SamplingDecision::RecordAndSample || 
              ratio_result.decision == SamplingDecision::Drop)
  
  // 创建ParentBased采样器
  let parent_based_sampler = Sampler::parent_based(always_on_sampler)
  
  // 设置父span上下文
  SamplingContext::set_parent_context(sampling_context, Context::with_sampling_decision(
    Context::root(), 
    SamplingDecision::RecordAndSample
  ))
  
  // 测试ParentBased采样器
  let parent_based_result = Sampler::should_sample(parent_based_sampler, sampling_context)
  assert_eq(parent_based_result.decision, SamplingDecision::RecordAndSample)
}

// 测试8: 批处理器基础功能
test "批处理器基础功能测试" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new()
  
  // 配置批处理参数
  BatchProcessor::set_max_batch_size(batch_processor, 100)
  BatchProcessor::set_max_export_timeout(batch_processor, 5000) // 5秒
  BatchProcessor::set_max_export_batch_size(batch_processor, 50)
  BatchProcessor::set_schedule_delay(batch_processor, 1000) // 1秒
  
  // 验证配置
  assert_eq(BatchProcessor::get_max_batch_size(batch_processor), 100)
  assert_eq(BatchProcessor::get_max_export_timeout(batch_processor), 5000)
  assert_eq(BatchProcessor::get_max_export_batch_size(batch_processor), 50)
  assert_eq(BatchProcessor::get_schedule_delay(batch_processor), 1000)
  
  // 创建测试数据
  let telemetry_items = []
  for i in 0..=10 {
    let item = TelemetryData::new()
    TelemetryData::set_trace_id(item, "trace-#{i}")
    TelemetryData::set_span_id(item, "span-#{i}")
    TelemetryData::add_attribute(item, "index", i)
    telemetry_items = telemetry_items.push(item)
  }
  
  // 添加到批处理器
  for item in telemetry_items {
    BatchProcessor::add_item(batch_processor, item)
  }
  
  // 验证队列状态
  assert_eq(BatchProcessor::get_queue_size(batch_processor), 11)
  
  // 强制刷新批处理
  let flush_result = BatchProcessor::force_flush(batch_processor)
  assert_true(flush_result.success)
  assert_eq(flush_result.exported_items, 11)
  
  // 验证队列已清空
  assert_eq(BatchProcessor::get_queue_size(batch_processor), 0)
}

// 测试9: 配置管理基础功能
test "配置管理基础功能测试" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 设置基础配置
  ConfigManager::set_service_name(config_manager, "test-service")
  ConfigManager::set_service_version(config_manager, "1.0.0")
  ConfigManager::set_environment(config_manager, "test")
  
  // 设置遥测配置
  ConfigManager::set_sampling_rate(config_manager, 0.1)
  ConfigManager::set_exporter_endpoint(config_manager, "http://localhost:4317")
  ConfigManager::set_batch_size(config_manager, 512)
  ConfigManager::set_export_timeout(config_manager, 30000)
  
  // 设置资源属性
  ConfigManager::add_resource_attribute(config_manager, "host.name", "test-host")
  ConfigManager::add_resource_attribute(config_manager, "deployment.region", "us-west-2")
  
  // 验证基础配置
  assert_eq(ConfigManager::get_service_name(config_manager), Some("test-service"))
  assert_eq(ConfigManager::get_service_version(config_manager), Some("1.0.0"))
  assert_eq(ConfigManager::get_environment(config_manager), Some("test"))
  
  // 验证遥测配置
  assert_eq(ConfigManager::get_sampling_rate(config_manager), 0.1)
  assert_eq(ConfigManager::get_exporter_endpoint(config_manager), Some("http://localhost:4317"))
  assert_eq(ConfigManager::get_batch_size(config_manager), 512)
  assert_eq(ConfigManager::get_export_timeout(config_manager), 30000)
  
  // 验证资源属性
  assert_eq(ConfigManager::get_resource_attribute(config_manager, "host.name"), Some("test-host"))
  assert_eq(ConfigManager::get_resource_attribute(config_manager, "deployment.region"), Some("us-west-2"))
  
  // 测试配置更新
  ConfigManager::update_sampling_rate(config_manager, 0.5)
  ConfigManager::update_batch_size(config_manager, 1024)
  
  // 验证更新后的配置
  assert_eq(ConfigManager::get_sampling_rate(config_manager), 0.5)
  assert_eq(ConfigManager::get_batch_size(config_manager), 1024)
  
  // 测试配置导出
  let exported_config = ConfigManager::export_config(config_manager)
  assert_true(exported_config.contains("test-service"))
  assert_true(exported_config.contains("1.0.0"))
  assert_true(exported_config.contains("test"))
  
  // 测试配置导入
  let new_config_manager = ConfigManager::new()
  ConfigManager::import_config(new_config_manager, exported_config)
  
  // 验证导入结果
  assert_eq(ConfigManager::get_service_name(new_config_manager), Some("test-service"))
  assert_eq(ConfigManager::get_service_version(new_config_manager), Some("1.0.0"))
  assert_eq(ConfigManager::get_environment(new_config_manager), Some("test"))
}

// 测试10: 导出器基础功能
test "导出器基础功能测试" {
  // 创建内存导出器（用于测试）
  let memory_exporter = MemoryExporter::new()
  
  // 创建测试数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::set_trace_id(telemetry_data, "trace-export-001")
  TelemetryData::set_span_id(telemetry_data, "span-export-001")
  TelemetryData::add_attribute(telemetry_data, "test.name", "export-test")
  TelemetryData::add_attribute(telemetry_data, "test.value", 42)
  
  // 导出数据
  let export_result = Exporter::export(memory_exporter, telemetry_data)
  
  // 验证导出结果
  assert_true(export_result.success)
  assert_eq(export_result.exported_items, 1)
  
  // 验证导出器状态
  assert_eq(MemoryExporter::get_item_count(memory_exporter), 1)
  
  // 获取导出的数据
  let exported_items = MemoryExporter::get_items(memory_exporter)
  assert_eq(exported_items.length(), 1)
  
  let exported_item = exported_items[0]
  assert_eq(TelemetryData::get_trace_id(exported_item), Some("trace-export-001"))
  assert_eq(TelemetryData::get_span_id(exported_item), Some("span-export-001"))
  assert_eq(TelemetryData::get_attribute(exported_item, "test.name"), Some("export-test"))
  assert_eq(TelemetryData::get_attribute(exported_item, "test.value"), Some(42))
  
  // 批量导出测试
  let batch_items = []
  for i in 0..=5 {
    let item = TelemetryData::new()
    TelemetryData::set_trace_id(item, "trace-batch-#{i}")
    TelemetryData::set_span_id(item, "span-batch-#{i}")
    TelemetryData::add_attribute(item, "batch.index", i)
    batch_items = batch_items.push(item)
  }
  
  let batch_export_result = Exporter::export_batch(memory_exporter, batch_items)
  
  // 验证批量导出结果
  assert_true(batch_export_result.success)
  assert_eq(batch_export_result.exported_items, 6)
  
  // 验证导出器状态
  assert_eq(MemoryExporter::get_item_count(memory_exporter), 7) // 1 + 6
  
  // 清空导出器
  MemoryExporter::clear(memory_exporter)
  assert_eq(MemoryExporter::get_item_count(memory_exporter), 0)
  
  // 测试导出器关闭
  let shutdown_result = Exporter::shutdown(memory_exporter)
  assert_true(shutdown_result.success)
}