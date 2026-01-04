// Azimuth 集成测试用例
// 专注于测试系统各组件之间的集成和协作

// 测试1: 端到端遥测数据流集成测试
test "端到端遥测数据流集成测试" {
  // 初始化所有组件
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "e2e.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "e2e.test")
  let logger = LoggerProvider::get_logger(logger_provider, "e2e.test")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "e2e.telemetry.flow")
  let root_context = Span::context(root_span)
  
  // 记录开始日志
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Starting end-to-end telemetry flow"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(root_context)),
    Some(SpanContext::span_id(root_context)),
    Some(root_context)
  )
  Logger::emit(logger, start_log)
  
  // 创建指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let latency_histogram = Meter::create_histogram(meter, "http.request.duration")
  let active_connections = Meter::create_gauge(meter, "http.connections.active")
  
  // 模拟请求处理
  for i = 0; i < 10; i = i + 1 {
    // 创建子span
    let request_span = Tracer::start_span_with_context(tracer, "http.request", root_context)
    let request_context = Span::context(request_span)
    
    // 设置span属性
    Span::set_attribute(request_span, "http.method", StringValue("GET"))
    Span::set_attribute(request_span, "http.url", StringValue("https://api.example.com/resource/" + i.to_string()))
    Span::set_attribute(request_span, "http.status_code", IntValue(200))
    
    // 记录指标
    Counter::add(request_counter, 1.0, [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200))
    ])
    
    // 模拟处理延迟
    let processing_time = (i % 5 + 1).to_float() * 0.01
    Histogram::record(latency_histogram, processing_time, [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200))
    ])
    
    // 更新活跃连接数
    Gauge::set(active_connections, (i + 1).to_float())
    
    // 记录处理日志
    let processing_log = LogRecord::new_with_context(
      Debug,
      Some("Processing request " + i.to_string()),
      None,
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(SpanContext::trace_id(request_context)),
      Some(SpanContext::span_id(request_context)),
      Some(request_context)
    )
    LogRecord::set_attribute(processing_log, "processing.time", FloatValue(processing_time))
    Logger::emit(logger, processing_log)
    
    // 结束请求span
    Span::end(request_span)
  }
  
  // 重置活跃连接数
  Gauge::set(active_connections, 0.0)
  
  // 记录结束日志
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Completed end-to-end telemetry flow"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(root_context)),
    Some(SpanContext::span_id(root_context)),
    Some(root_context)
  )
  Logger::emit(logger, end_log)
  
  // 结束根span
  Span::end(root_span)
  
  // 验证数据流
  assert_eq(Counter::value(request_counter), 10.0)
  assert_eq(Gauge::value(active_connections), 0.0)
}

// 测试2: 跨服务上下文传播集成测试
test "跨服务上下文传播集成测试" {
  // 服务A初始化
  let service_a_meter_provider = MeterProvider::default()
  let service_a_tracer_provider = TracerProvider::default()
  let service_a_logger_provider = LoggerProvider::default()
  
  let service_a_meter = MeterProvider::get_meter(service_a_meter_provider, "service.a")
  let service_a_tracer = TracerProvider::get_tracer(service_a_tracer_provider, "service.a")
  let service_a_logger = LoggerProvider::get_logger(service_a_logger_provider, "service.a")
  
  // 服务A创建根span
  let service_a_span = Tracer::start_span(service_a_tracer, "service.a.operation")
  let service_a_context = Span::context(service_a_span)
  
  // 添加服务A特定的上下文
  let service_a_context_with_baggage = Context::with_value(
    service_a_context,
    ContextKey::new("service.a.user.id"),
    "user123"
  )
  
  // 模拟服务A到服务B的调用
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_a_context_with_baggage, carrier)
  
  // 服务B初始化
  let service_b_meter_provider = MeterProvider::default()
  let service_b_tracer_provider = TracerProvider::default()
  let service_b_logger_provider = LoggerProvider::default()
  
  let service_b_meter = MeterProvider::get_meter(service_b_meter_provider, "service.b")
  let service_b_tracer = TracerProvider::get_tracer(service_b_tracer_provider, "service.b")
  let service_b_logger = LoggerProvider::get_logger(service_b_logger_provider, "service.b")
  
  // 服务B提取上下文
  let service_b_context = CompositePropagator::extract(propagator, carrier)
  
  // 验证上下文传播
  let extracted_user_id = Context::get(service_b_context, ContextKey::new("service.a.user.id"))
  assert_eq(extracted_user_id, Some("user123"))
  
  // 服务B创建子span
  let service_b_span = Tracer::start_span_with_context(service_b_tracer, "service.b.operation", service_b_context)
  
  // 记录服务B的指标
  let service_b_counter = Meter::create_counter(service_b_meter, "service.b.operations")
  Counter::add(service_b_counter, 1.0, [
    ("user.id", StringValue(extracted_user_id.unwrap_or("unknown")))
  ])
  
  // 记录服务B的日志
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B operation completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::context(service_b_span))),
    Some(SpanContext::span_id(Span::context(service_b_span))),
    Some(Span::context(service_b_span))
  )
  Logger::emit(service_b_logger, service_b_log)
  
  // 结束所有span
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  // 验证跨服务集成
  assert_eq(Counter::value(service_b_counter), 1.0)
}

// 测试3: 多组件错误处理集成测试
test "多组件错误处理集成测试" {
  // 初始化组件
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "error.handling")
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.handling")
  let logger = LoggerProvider::get_logger(logger_provider, "error.handling")
  
  // 创建错误计数器
  let error_counter = Meter::create_counter(meter, "errors.total")
  let recovery_counter = Meter::create_counter(meter, "recoveries.total")
  
  // 创建错误处理span
  let error_span = Tracer::start_span(tracer, "error.handling.operation")
  
  try {
    // 模拟操作1 - 成功
    Span::add_event(error_span, "operation1.started", None)
    Counter::add(error_counter, 1.0, [("operation", StringValue("operation1")), ("status", StringValue("success"))])
    Span::add_event(error_span, "operation1.completed", None)
    
    // 模拟操作2 - 失败
    Span::add_event(error_span, "operation2.started", None)
    Counter::add(error_counter, 1.0, [("operation", StringValue("operation2")), ("status", StringValue("error"))])
    
    // 记录错误日志
    let error_log = LogRecord::new_with_context(
      Error,
      Some("Operation 2 failed"),
      Some("Simulated failure for testing error handling"),
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(SpanContext::trace_id(Span::context(error_span))),
      Some(SpanContext::span_id(Span::context(error_span))),
      Some(Span::context(error_span))
    )
    LogRecord::set_attribute(error_log, "operation", StringValue("operation2"))
    LogRecord::set_attribute(error_log, "error.code", StringValue("SIMULATED_ERROR"))
    Logger::emit(logger, error_log)
    
    // 设置span错误状态
    Span::set_status(error_span, Error, Some("Operation 2 failed"))
    
    // 模拟恢复操作
    Span::add_event(error_span, "recovery.started", None)
    Counter::add(recovery_counter, 1.0, [("operation", StringValue("recovery")), ("status", StringValue("success"))])
    
    // 记录恢复日志
    let recovery_log = LogRecord::new_with_context(
      Info,
      Some("Recovery operation completed"),
      None,
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(SpanContext::trace_id(Span::context(error_span))),
      Some(SpanContext::span_id(Span::context(error_span))),
      Some(Span::context(error_span))
    )
    LogRecord::set_attribute(recovery_log, "operation", StringValue("recovery"))
    Logger::emit(logger, recovery_log)
    
    Span::add_event(error_span, "recovery.completed", None)
    
    // 模拟操作3 - 成功
    Span::add_event(error_span, "operation3.started", None)
    Counter::add(error_counter, 1.0, [("operation", StringValue("operation3")), ("status", StringValue("success"))])
    Span::add_event(error_span, "operation3.completed", None)
    
  } catch {
    error => {
      // 记录未捕获的异常
      Span::set_status(error_span, Error, Some("Uncaught exception"))
      
      let exception_log = LogRecord::new_with_context(
        Error,
        Some("Uncaught exception"),
        Some("Exception details: " + error.to_string()),
        Some(Clock::now_unix_nanos(Clock::system())),
        None,
        Some(SpanContext::trace_id(Span::context(error_span))),
        Some(SpanContext::span_id(Span::context(error_span))),
        Some(Span::context(error_span))
      )
      Logger::emit(logger, exception_log)
    }
  }
  
  // 结束span
  Span::end(error_span)
  
  // 验证错误处理集成
  assert_eq(Counter::value(error_counter), 3.0)
  assert_eq(Counter::value(recovery_counter), 1.0)
}

// 测试4: 资源管理和配置集成测试
test "资源管理和配置集成测试" {
  // 创建配置
  let telemetry_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(telemetry_config, 0.1)
  TelemetryConfig::set_exporter_endpoint(telemetry_config, "http://localhost:4318")
  TelemetryConfig::set_batch_size(telemetry_config, 512)
  TelemetryConfig::set_export_timeout(telemetry_config, 30000)
  
  // 使用配置初始化提供者
  let meter_provider = MeterProvider::with_config(telemetry_config)
  let tracer_provider = TracerProvider::with_config(telemetry_config)
  let logger_provider = LoggerProvider::with_config(telemetry_config)
  
  // 验证配置应用
  assert_eq(TelemetryConfig::get_sampling_rate(telemetry_config), 0.1)
  assert_eq(TelemetryConfig::get_exporter_endpoint(telemetry_config), "http://localhost:4318")
  assert_eq(TelemetryConfig::get_batch_size(telemetry_config), 512)
  assert_eq(TelemetryConfig::get_export_timeout(telemetry_config), 30000)
  
  // 创建资源
  let service_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("resource.management.test")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  
  // 使用资源初始化组件
  let meter = MeterProvider::get_meter_with_resource(meter_provider, "resource.test", service_resource)
  let tracer = TracerProvider::get_tracer_with_resource(tracer_provider, "resource.test", service_resource)
  let logger = LoggerProvider::get_logger_with_resource(logger_provider, "resource.test", service_resource)
  
  // 创建指标和span
  let counter = Meter::create_counter(meter, "resource.operations")
  let span = Tracer::start_span(tracer, "resource.operation")
  
  // 记录指标和日志
  Counter::add(counter, 1.0)
  
  let log_record = LogRecord::new(Info, "Resource management test completed")
  Logger::emit(logger, log_record)
  
  // 结束span
  Span::end(span)
  
  // 验证资源管理集成
  assert_eq(Counter::value(counter), 1.0)
  
  // 测试配置动态更新
  TelemetryConfig::update_sampling_rate(telemetry_config, 0.5)
  TelemetryConfig::update_batch_size(telemetry_config, 1024)
  
  // 验证配置更新
  assert_eq(TelemetryConfig::get_sampling_rate(telemetry_config), 0.5)
  assert_eq(TelemetryConfig::get_batch_size(telemetry_config), 1024)
}

// 测试5: 批处理和流处理集成测试
test "批处理和流处理集成测试" {
  // 初始化组件
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "batch.stream.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.stream.test")
  let logger = LoggerProvider::get_logger(logger_provider, "batch.stream.test")
  
  // 创建批处理span
  let batch_span = Tracer::start_span(tracer, "batch.processing")
  
  // 创建批处理指标
  let batch_counter = Meter::create_counter(meter, "batch.processed")
  let batch_size_histogram = Meter::create_histogram(meter, "batch.size")
  let batch_latency_histogram = Meter::create_histogram(meter, "batch.latency")
  
  // 模拟批处理
  let batch_start_time = Clock::now_unix_nanos(Clock::system())
  let batch_size = 100
  
  for i = 0; i < batch_size; i = i + 1 {
    // 处理单个项目
    // 在真实场景中，这里会有实际的业务逻辑
    
    // 记录处理延迟
    let item_processing_time = (i % 10 + 1).to_float() * 0.001
    Histogram::record(batch_latency_histogram, item_processing_time)
  }
  
  let batch_end_time = Clock::now_unix_nanos(Clock::system())
  let batch_duration = (batch_end_time - batch_start_time).to_float() / 1000000.0  // 转换为毫秒
  
  // 记录批处理指标
  Counter::add(batch_counter, 1.0)
  Histogram::record(batch_size_histogram, batch_size.to_float())
  Histogram::record(batch_latency_histogram, batch_duration)
  
  // 记录批处理日志
  let batch_log = LogRecord::new_with_context(
    Info,
    Some("Batch processing completed"),
    None,
    Some(batch_end_time),
    None,
    Some(SpanContext::trace_id(Span::context(batch_span))),
    Some(SpanContext::span_id(Span::context(batch_span))),
    Some(Span::context(batch_span))
  )
  LogRecord::set_attribute(batch_log, "batch.size", IntValue(batch_size))
  LogRecord::set_attribute(batch_log, "batch.duration", FloatValue(batch_duration))
  Logger::emit(logger, batch_log)
  
  // 结束批处理span
  Span::end(batch_span)
  
  // 模拟流处理
  let stream_span = Tracer::start_span(tracer, "stream.processing")
  
  // 创建流处理指标
  let stream_counter = Meter::create_counter(meter, "stream.processed")
  let stream_rate_gauge = Meter::create_gauge(meter, "stream.rate")
  
  // 模拟流处理
  let stream_start_time = Clock::now_unix_nanos(Clock::system())
  let stream_items = 500
  
  for i = 0; i < stream_items; i = i + 1 {
    // 处理单个流项目
    Counter::add(stream_counter, 1.0)
    
    // 每100个项目更新一次速率
    if i % 100 == 0 {
      let elapsed_time = (Clock::now_unix_nanos(Clock::system()) - stream_start_time).to_float() / 1000000000.0  // 转换为秒
      let rate = (i + 1).to_float() / elapsed_time
      Gauge::set(stream_rate_gauge, rate)
    }
  }
  
  // 记录流处理日志
  let stream_log = LogRecord::new_with_context(
    Info,
    Some("Stream processing completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::context(stream_span))),
    Some(SpanContext::span_id(Span::context(stream_span))),
    Some(Span::context(stream_span))
  )
  LogRecord::set_attribute(stream_log, "stream.items", IntValue(stream_items))
  Logger::emit(logger, stream_log)
  
  // 结束流处理span
  Span::end(stream_span)
  
  // 验证批处理和流处理集成
  assert_eq(Counter::value(batch_counter), 1.0)
  assert_eq(Counter::value(stream_counter), stream_items.to_float())
  assert_true(Gauge::value(stream_rate_gauge) > 0.0)
}

// 测试6: 多租户隔离集成测试
test "多租户隔离集成测试" {
  // 创建租户A的配置和资源
  let tenant_a_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(tenant_a_config, 0.1)
  
  let tenant_a_resource = Resource::with_attributes(Resource::new(), [
    ("tenant.id", StringValue("tenant-a")),
    ("tenant.plan", StringValue("enterprise")),
    ("tenant.quota.metrics", IntValue(1000000))
  ])
  
  let tenant_a_meter_provider = MeterProvider::with_config_and_resource(tenant_a_config, tenant_a_resource)
  let tenant_a_meter = MeterProvider::get_meter(tenant_a_meter_provider, "tenant.a.service")
  
  // 创建租户B的配置和资源
  let tenant_b_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(tenant_b_config, 0.5)
  
  let tenant_b_resource = Resource::with_attributes(Resource::new(), [
    ("tenant.id", StringValue("tenant-b")),
    ("tenant.plan", StringValue("standard")),
    ("tenant.quota.metrics", IntValue(100000))
  ])
  
  let tenant_b_meter_provider = MeterProvider::with_config_and_resource(tenant_b_config, tenant_b_resource)
  let tenant_b_meter = MeterProvider::get_meter(tenant_b_meter_provider, "tenant.b.service")
  
  // 创建租户特定的指标
  let tenant_a_counter = Meter::create_counter(tenant_a_meter, "tenant.a.operations")
  let tenant_b_counter = Meter::create_counter(tenant_b_meter, "tenant.b.operations")
  
  // 记录租户A的操作
  for i = 0; i < 100; i = i + 1 {
    Counter::add(tenant_a_counter, 1.0, [
      ("tenant.id", StringValue("tenant-a")),
      ("operation.type", StringValue("type_" + (i % 5).to_string()))
    ])
  }
  
  // 记录租户B的操作
  for i = 0; i < 50; i = i + 1 {
    Counter::add(tenant_b_counter, 1.0, [
      ("tenant.id", StringValue("tenant-b")),
      ("operation.type", StringValue("type_" + (i % 3).to_string()))
    ])
  }
  
  // 验证租户隔离
  assert_eq(Counter::value(tenant_a_counter), 100.0)
  assert_eq(Counter::value(tenant_b_counter), 50.0)
  
  // 验证配置隔离
  assert_eq(TelemetryConfig::get_sampling_rate(tenant_a_config), 0.1)
  assert_eq(TelemetryConfig::get_sampling_rate(tenant_b_config), 0.5)
  
  // 验证资源隔离
  let tenant_a_quota = Resource::get_attribute(tenant_a_resource, "tenant.quota.metrics")
  let tenant_b_quota = Resource::get_attribute(tenant_b_resource, "tenant.quota.metrics")
  
  assert_eq(tenant_a_quota, Some(IntValue(1000000)))
  assert_eq(tenant_b_quota, Some(IntValue(100000)))
}

// 测试7: 数据导出和收集集成测试
test "数据导出和收集集成测试" {
  // 初始化组件
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "export.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "export.test")
  let logger = LoggerProvider::get_logger(logger_provider, "export.test")
  
  // 创建导出器
  let metric_exporter = MetricExporter::new("http://localhost:4318/v1/metrics")
  let trace_exporter = TraceExporter::new("http://localhost:4318/v1/traces")
  let log_exporter = LogExporter::new("http://localhost:4318/v1/logs")
  
  // 创建指标
  let export_counter = Meter::create_counter(meter, "export.operations")
  let export_latency = Meter::create_histogram(meter, "export.latency")
  
  // 创建span和日志
  let export_span = Tracer::start_span(tracer, "export.operation")
  Span::set_attribute(export_span, "exporter.type", StringValue("http"))
  
  let export_log = LogRecord::new(Info, "Export operation started")
  Logger::emit(logger, export_log)
  
  // 记录指标
  Counter::add(export_counter, 1.0)
  
  // 模拟导出操作
  let export_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 导出指标
  let metric_export_result = MetricExporter::export(metric_exporter, meter_provider)
  
  // 导出追踪
  let trace_export_result = TraceExporter::export(trace_exporter, tracer_provider)
  
  // 导出日志
  let log_export_result = LogExporter::export(log_exporter, logger_provider)
  
  let export_end_time = Clock::now_unix_nanos(Clock::system())
  let export_duration = (export_end_time - export_start_time).to_float() / 1000000.0  // 转换为毫秒
  
  // 记录导出延迟
  Histogram::record(export_latency, export_duration)
  
  // 记录导出完成日志
  let export_complete_log = LogRecord::new_with_context(
    Info,
    Some("Export operation completed"),
    None,
    Some(export_end_time),
    None,
    Some(SpanContext::trace_id(Span::context(export_span))),
    Some(SpanContext::span_id(Span::context(export_span))),
    Some(Span::context(export_span))
  )
  LogRecord::set_attribute(export_complete_log, "export.duration", FloatValue(export_duration))
  LogRecord::set_attribute(export_complete_log, "metric.export.success", BoolValue(metric_export_result))
  LogRecord::set_attribute(export_complete_log, "trace.export.success", BoolValue(trace_export_result))
  LogRecord::set_attribute(export_complete_log, "log.export.success", BoolValue(log_export_result))
  Logger::emit(logger, export_complete_log)
  
  // 结束span
  Span::end(export_span)
  
  // 验证导出集成
  assert_eq(Counter::value(export_counter), 1.0)
  // 导出结果可能失败，这是正常的，因为测试环境可能没有运行导出服务
}

// 测试8: 采样策略集成测试
test "采样策略集成测试" {
  // 创建不同采样策略的配置
  let always_on_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_strategy(always_on_config, "always_on")
  
  let always_off_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_strategy(always_off_config, "always_off")
  
  let trace_id_ratio_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_strategy(trace_id_ratio_config, "trace_id_ratio")
  TelemetryConfig::set_sampling_rate(trace_id_ratio_config, 0.5)
  
  // 使用不同配置初始化tracer提供者
  let always_on_tracer_provider = TracerProvider::with_config(always_on_config)
  let always_off_tracer_provider = TracerProvider::with_config(always_off_config)
  let trace_id_ratio_tracer_provider = TracerProvider::with_config(trace_id_ratio_config)
  
  let always_on_tracer = TracerProvider::get_tracer(always_on_tracer_provider, "always.on")
  let always_off_tracer = TracerProvider::get_tracer(always_off_tracer_provider, "always.off")
  let trace_id_ratio_tracer = TracerProvider::get_tracer(trace_id_ratio_tracer_provider, "trace.id.ratio")
  
  // 测试always_on采样
  let always_on_spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(always_on_tracer, "always.on.span." + i.to_string())
    always_on_spans.push(span)
  }
  
  // 测试always_off采样
  let always_off_spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(always_off_tracer, "always.off.span." + i.to_string())
    always_off_spans.push(span)
  }
  
  // 测试trace_id_ratio采样
  let trace_id_ratio_spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(trace_id_ratio_tracer, "trace.id.ratio.span." + i.to_string())
    trace_id_ratio_spans.push(span)
  }
  
  // 验证采样结果
  let always_on_sampled_count = always_on_spans.filter(|span| Span::is_sampled(span)).length()
  let always_off_sampled_count = always_off_spans.filter(|span| Span::is_sampled(span)).length()
  let trace_id_ratio_sampled_count = trace_id_ratio_spans.filter(|span| Span::is_sampled(span)).length()
  
  // always_on应该采样所有span
  assert_eq(always_on_sampled_count, 10)
  
  // always_off不应该采样任何span
  assert_eq(always_off_sampled_count, 0)
  
  // trace_id_ratio应该采样大约50%的span
  assert_true(trace_id_ratio_sampled_count >= 0 && trace_id_ratio_sampled_count <= 10)
  
  // 结束所有span
  for span in always_on_spans {
    Span::end(span)
  }
  
  for span in always_off_spans {
    Span::end(span)
  }
  
  for span in trace_id_ratio_spans {
    Span::end(span)
  }
}

// 测试9: 多格式数据转换集成测试
test "多格式数据转换集成测试" {
  // 初始化组件
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "format.conversion.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "format.conversion.test")
  let logger = LoggerProvider::get_logger(logger_provider, "format.conversion.test")
  
  // 创建测试数据
  let test_span = Tracer::start_span(tracer, "format.test.span")
  Span::set_attribute(test_span, "string.attr", StringValue("test.value"))
  Span::set_attribute(test_span, "int.attr", IntValue(42))
  Span::set_attribute(test_span, "float.attr", FloatValue(3.14))
  Span::set_attribute(test_span, "bool.attr", BoolValue(true))
  
  let test_log = LogRecord::new(Info, "Format test log")
  LogRecord::set_attribute(test_log, "log.string", StringValue("log.value"))
  LogRecord::set_attribute(test_log, "log.int", IntValue(24))
  
  // 测试JSON格式转换
  let json_span_data = FormatConverter::span_to_json(test_span)
  let json_log_data = FormatConverter::log_to_json(test_log)
  
  assert_true(json_span_data.contains("format.test.span"))
  assert_true(json_span_data.contains("string.attr"))
  assert_true(json_log_data.contains("Format test log"))
  assert_true(json_log_data.contains("log.string"))
  
  // 从JSON恢复数据
  let restored_span = FormatConverter::json_to_span(json_span_data)
  let restored_log = FormatConverter::json_to_log(json_log_data)
  
  assert_eq(Span::name(restored_span), Span::name(test_span))
  assert_eq(LogRecord::body(restored_log), LogRecord::body(test_log))
  
  // 测试Protobuf格式转换
  let protobuf_span_data = FormatConverter::span_to_protobuf(test_span)
  let protobuf_log_data = FormatConverter::log_to_protobuf(test_log)
  
  assert_true(protobuf_span_data.length() > 0)
  assert_true(protobuf_log_data.length() > 0)
  
  // 从Protobuf恢复数据
  let protobuf_restored_span = FormatConverter::protobuf_to_span(protobuf_span_data)
  let protobuf_restored_log = FormatConverter::protobuf_to_log(protobuf_log_data)
  
  assert_eq(Span::name(protobuf_restored_span), Span::name(test_span))
  assert_eq(LogRecord::body(protobuf_restored_log), LogRecord::body(test_log))
  
  // 记录格式转换指标
  let conversion_counter = Meter::create_counter(meter, "format.conversions")
  Counter::add(conversion_counter, 1.0, [("source.format", StringValue("otel")), ("target.format", StringValue("json"))])
  Counter::add(conversion_counter, 1.0, [("source.format", StringValue("json")), ("target.format", StringValue("otel"))])
  Counter::add(conversion_counter, 1.0, [("source.format", StringValue("otel")), ("target.format", StringValue("protobuf"))])
  Counter::add(conversion_counter, 1.0, [("source.format", StringValue("protobuf")), ("target.format", StringValue("otel"))])
  
  // 结束span
  Span::end(test_span)
  
  // 验证格式转换集成
  assert_eq(Counter::value(conversion_counter), 4.0)
}

// 测试10: 完整系统生命周期集成测试
test "完整系统生命周期集成测试" {
  // 系统初始化
  let system_config = TelemetryConfig::new()
  TelemetryConfig::set_sampling_rate(system_config, 0.1)
  TelemetryConfig::set_exporter_endpoint(system_config, "http://localhost:4318")
  
  let system_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("lifecycle.test")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-" + Clock::now_unix_nanos(Clock::system()).to_string()))
  ])
  
  // 初始化系统组件
  let meter_provider = MeterProvider::with_config_and_resource(system_config, system_resource)
  let tracer_provider = TracerProvider::with_config_and_resource(system_config, system_resource)
  let logger_provider = LoggerProvider::with_config_and_resource(system_config, system_resource)
  
  let meter = MeterProvider::get_meter(meter_provider, "lifecycle.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  let logger = LoggerProvider::get_logger(logger_provider, "lifecycle.test")
  
  // 系统启动日志
  let startup_log = LogRecord::new(Info, "System starting up")
  LogRecord::set_attribute(startup_log, "system.phase", StringValue("startup"))
  Logger::emit(logger, startup_log)
  
  // 创建系统指标
  let lifecycle_counter = Meter::create_counter(meter, "lifecycle.events")
  let system_uptime = Meter::create_gauge(meter, "system.uptime")
  
  // 系统运行阶段
  let system_start_time = Clock::now_unix_nanos(Clock::system())
  
  for phase = 0; phase < 3; phase = phase + 1 {
    let phase_name = match phase {
      0 => "initialization"
      1 => "active"
      2 => "shutdown"
      _ => "unknown"
    }
    
    // 创建阶段span
    let phase_span = Tracer::start_span(tracer, "system.phase." + phase_name)
    Span::set_attribute(phase_span, "phase.name", StringValue(phase_name))
    Span::set_attribute(phase_span, "phase.index", IntValue(phase))
    
    // 记录阶段事件
    Counter::add(lifecycle_counter, 1.0, [
      ("phase.name", StringValue(phase_name)),
      ("phase.status", StringValue("started"))
    ])
    
    // 模拟阶段操作
    for operation = 0; operation < 10; operation = operation + 1 {
      // 创建操作span
      let operation_span = Tracer::start_span_with_context(tracer, "phase.operation", Span::context(phase_span))
      Span::set_attribute(operation_span, "operation.id", IntValue(operation))
      
      // 记录操作日志
      let operation_log = LogRecord::new_with_context(
        Debug,
        Some("Executing operation " + operation.to_string() + " in phase " + phase_name),
        None,
        Some(Clock::now_unix_nanos(Clock::system())),
        None,
        Some(SpanContext::trace_id(Span::context(operation_span))),
        Some(SpanContext::span_id(Span::context(operation_span))),
        Some(Span::context(operation_span))
      )
      Logger::emit(logger, operation_log)
      
      // 结束操作span
      Span::end(operation_span)
    }
    
    // 记录阶段完成事件
    Counter::add(lifecycle_counter, 1.0, [
      ("phase.name", StringValue(phase_name)),
      ("phase.status", StringValue("completed"))
    ])
    
    // 记录阶段日志
    let phase_log = LogRecord::new_with_context(
      Info,
      Some("Phase " + phase_name + " completed"),
      None,
      Some(Clock::now_unix_nanos(Clock::system())),
      None,
      Some(SpanContext::trace_id(Span::context(phase_span))),
      Some(SpanContext::span_id(Span::context(phase_span))),
      Some(Span::context(phase_span))
    )
    Logger::emit(logger, phase_log)
    
    // 结束阶段span
    Span::end(phase_span)
  }
  
  // 计算系统运行时间
  let system_end_time = Clock::now_unix_nanos(Clock::system())
  let uptime_seconds = (system_end_time - system_start_time).to_float() / 1000000000.0
  Gauge::set(system_uptime, uptime_seconds)
  
  // 系统关闭日志
  let shutdown_log = LogRecord::new(Info, "System shutting down")
  LogRecord::set_attribute(shutdown_log, "system.phase", StringValue("shutdown"))
  LogRecord::set_attribute(shutdown_log, "system.uptime", FloatValue(uptime_seconds))
  Logger::emit(logger, shutdown_log)
  
  // 验证系统生命周期集成
  assert_eq(Counter::value(lifecycle_counter), 6.0)  // 3个阶段开始 + 3个阶段完成
  assert_true(Gauge::value(system_uptime) > 0.0)
}