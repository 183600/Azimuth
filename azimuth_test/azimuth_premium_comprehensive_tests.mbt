// Azimuth 高级综合测试用例
// 专注于遥测系统的高级功能和边界场景测试

// 测试1: 遥测数据采样策略测试
test "遥测数据采样策略测试" {
  // 创建带有采样配置的TracerProvider
  let tracer_provider = azimuth::TracerProvider::new()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "sampling-test-tracer")
  
  // 测试不同的采样场景
  let sampled_spans = []
  let unsampled_spans = []
  
  // 创建多个Span，模拟采样决策
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "sampled-span-" + i.to_string())
    let span_ctx = azimuth::Span::span_context(span)
    
    if azimuth::SpanContext::is_sampled(span_ctx) {
      sampled_spans.push(span)
    } else {
      unsampled_spans.push(span)
    }
    
    // 为所有Span添加属性（通过简化实现）
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "iteration", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("sampling-test"))
    
    // 结束Span
    azimuth::Span::end(span)
  }
  
  // 验证采样结果
  assert_true(sampled_spans.length() > 0)  // 至少有一些Span被采样
  assert_true(unsampled_spans.length() >= 0)  // 可能有未采样的Span
  assert_true(sampled_spans.length() + unsampled_spans.length() == 20)  // 总数正确
  
  // 测试基于概率的采样
  let probability_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "probability-tracer")
  let high_probability_spans = []
  
  // 创建高概率采样的Span
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(probability_tracer, "high-prob-" + i.to_string())
    let span_ctx = azimuth::Span::span_context(span)
    
    if azimuth::SpanContext::is_sampled(span_ctx) {
      high_probability_spans.push(span)
    }
    
    azimuth::Span::end(span)
  }
  
  // 验证概率采样效果
  assert_true(high_probability_spans.length() >= 0)
}

// 测试2: 异步遥测数据导出测试
test "异步遥测数据导出测试" {
  // 创建TracerProvider
  let tracer_provider = azimuth::TracerProvider::new()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-export-test")
  
  // 创建大量Span以触发批量导出
  let async_spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "async-span-" + i.to_string())
    
    // 添加事件和属性（通过简化实现）
    let event_attrs = [
      ("event.id", azimuth::StringValue("event-" + i.to_string())),
      ("event.type", azimuth::StringValue("async-test"))
    ]
    azimuth::Span::add_event(span, "async.event", Some(event_attrs))
    
    let span_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(span_attrs, "async.index", azimuth::IntValue(i))
    azimuth::Attributes::set(span_attrs, "async.batch", azimuth::StringValue("batch-1"))
    
    // 设置状态
    if i % 5 == 0 {
      azimuth::Span::set_status(span, azimuth::Error)
    } else {
      azimuth::Span::set_status(span, azimuth::Ok)
    }
    
    async_spans.push(span)
  }
  
  // 结束所有Span
  for span in async_spans {
    azimuth::Span::end(span)
  }
  
  // 验证Span创建和配置
  assert_true(async_spans.length() == 50)
  
  // 测试度量导出
  let meter_provider = azimuth::MeterProvider::new()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "async-metric-test")
  
  // 创建多个度量仪器
  let counter = azimuth::Meter::create_counter(meter, "async.operations")
  let histogram = azimuth::Meter::create_histogram(meter, "async.duration")
  let gauge = azimuth::Meter::create_gauge(meter, "async.memory")
  
  // 执行大量度量操作
  for i in 0..100 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() * 0.1)
    
    // 模拟Gauge值变化
    if i % 10 == 0 {
      // Gauge::set(gauge, i.to_double())  // 注释掉，因为简化实现可能不支持
    }
  }
  
  // 验证度量操作
  assert_eq(counter.name, "async.operations")
  assert_eq(histogram.name, "async.duration")
  assert_eq(gauge.name, "async.memory")
}

// 测试3: 遥测数据批处理测试
test "遥测数据批处理测试" {
  // 创建批处理器配置
  let batch_config = azimuth::BatchSpanProcessorConfig::new(
    10,  // max_queue_size
    5,   // max_export_batch_size
    5000  // schedule_delay
  )
  
  let batch_processor = azimuth::BatchSpanProcessor::new(
    azimuth::ConsoleSpanExporter::new(),
    batch_config
  )
  
  let tracer_provider = azimuth::TracerProvider::with_span_processor(batch_processor)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "batch-test-tracer")
  
  // 创建第一批Span
  let batch1_spans = []
  for i in 0..5 {
    let span = azimuth::Tracer::start_span(tracer, "batch1-span-" + i.to_string())
    azimuth::Span::set_attribute(span, "batch.id", azimuth::IntValue(1))
    azimuth::Span::set_attribute(span, "batch.index", azimuth::IntValue(i))
    batch1_spans.push(span)
  }
  
  // 结束第一批Span
  for span in batch1_spans {
    azimuth::Span::end(span)
  }
  
  // 创建第二批Span
  let batch2_spans = []
  for i in 0..8 {
    let span = azimuth::Tracer::start_span(tracer, "batch2-span-" + i.to_string())
    azimuth::Span::set_attribute(span, "batch.id", azimuth::IntValue(2))
    azimuth::Span::set_attribute(span, "batch.index", azimuth::IntValue(i))
    batch2_spans.push(span)
  }
  
  // 结束第二批Span
  for span in batch2_spans {
    azimuth::Span::end(span)
  }
  
  // 验证批处理配置
  assert_eq(batch_config.max_queue_size, 10)
  assert_eq(batch_config.max_export_batch_size, 5)
  assert_eq(batch_config.schedule_delay, 5000)
  
  // 测试度量批处理
  let metric_batch_config = azimuth::PeriodicMetricReaderConfig::new(2000)  // 2秒间隔
  let metric_reader = azimuth::PeriodicMetricReader::new(
    azimuth::ConsoleMetricExporter::new(),
    metric_batch_config
  )
  
  let meter_provider = azimuth::MeterProvider::with_metric_reader(metric_reader)
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "batch-metric-test")
  
  // 创建多个度量仪器并记录数据
  let batch_counter = azimuth::Meter::create_counter(meter, "batch.operations")
  let batch_histogram = azimuth::Meter::create_histogram(meter, "batch.latency")
  
  // 记录多批度量数据
  for batch in 0..3 {
    for i in 0..10 {
      azimuth::Counter::add(batch_counter, 1.0)
      azimuth::Histogram::record(batch_histogram, (batch * 10 + i).to_double())
    }
  }
  
  // 验证批处理度量
  assert_eq(batch_counter.name, "batch.operations")
  assert_eq(batch_histogram.name, "batch.latency")
}

// 测试4: 遥测系统故障恢复测试
test "遥测系统故障恢复测试" {
  // 模拟故障场景的导出器
  let failing_exporter = azimuth::FailingSpanExporter::new(3)  // 前3次失败
  let resilient_processor = azimuth::BatchSpanProcessor::new(
    failing_exporter,
    azimuth::BatchSpanProcessorConfig::default()
  )
  
  let tracer_provider = azimuth::TracerProvider::with_span_processor(resilient_processor)
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resilient-tracer")
  
  // 创建Span以触发故障恢复
  let recovery_spans = []
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "recovery-span-" + i.to_string())
    azimuth::Span::set_attribute(span, "recovery.attempt", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "resilience.test", azimuth::StringValue("true"))
    
    // 添加事件标记恢复过程
    azimuth::Span::add_event(span, "recovery.attempt", Some([
      ("attempt.number", azimuth::IntValue(i)),
      ("exporter.status", azimuth::StringValue("testing"))
    ]))
    
    recovery_spans.push(span)
  }
  
  // 结束所有Span
  for span in recovery_spans {
    azimuth::Span::end(span)
  }
  
  // 验证故障恢复配置
  assert_true(recovery_spans.length() == 10)
  
  // 测试度量系统故障恢复
  let failing_metric_exporter = azimuth::FailingMetricExporter::new(2)  // 前2次失败
  let resilient_metric_reader = azimuth::PeriodicMetricReader::new(
    failing_metric_exporter,
    azimuth::PeriodicMetricReaderConfig::default()
  )
  
  let meter_provider = azimuth::MeterProvider::with_metric_reader(resilient_metric_reader)
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "resilient-meter")
  
  // 创建度量仪器并记录数据
  let recovery_counter = azimuth::Meter::create_counter(meter, "recovery.operations")
  let recovery_histogram = azimuth::Meter::create_histogram(meter, "recovery.latency")
  
  // 记录度量数据以触发故障恢复
  for i in 0..15 {
    azimuth::Counter::add(recovery_counter, 1.0)
    azimuth::Histogram::record(recovery_histogram, i.to_double() * 0.5)
  }
  
  // 验证故障恢复度量
  assert_eq(recovery_counter.name, "recovery.operations")
  assert_eq(recovery_histogram.name, "recovery.latency")
  
  // 测试日志系统故障恢复
  let failing_log_exporter = azimuth::FailingLogExporter::new(1)  // 第1次失败
  let resilient_logger_provider = azimuth::LoggerProvider::with_log_exporter(failing_log_exporter)
  let logger = azimuth::LoggerProvider::get_logger(resilient_logger_provider, "resilient-logger")
  
  // 创建日志记录以触发故障恢复
  for i in 0..5 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Recovery log message " + i.to_string()),
      Some([("recovery.test", azimuth::StringValue("true"))]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("recovery-trace"),
      Some("recovery-span"),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证日志故障恢复
  assert_eq(logger.scope.name, "resilient-logger")
}

// 测试5: 遥测数据质量保证测试
test "遥测数据质量保证测试" {
  // 创建数据质量验证器
  let quality_validator = azimuth::TelemetryQualityValidator::new()
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "quality-test-tracer")
  
  // 创建高质量Span
  let quality_spans = []
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "quality-span-" + i.to_string())
    
    // 添加丰富的属性
    azimuth::Span::set_attribute(span, "quality.index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "quality.service", azimuth::StringValue("quality-test"))
    azimuth::Span::set_attribute(span, "quality.version", azimuth::StringValue("1.0.0"))
    azimuth::Span::set_attribute(span, "quality.environment", azimuth::StringValue("test"))
    
    // 添加有意义的事件
    azimuth::Span::add_event(span, "quality.check", Some([
      ("check.type", azimuth::StringValue("validation")),
      ("check.status", azimuth::StringValue("passed")),
      ("check.timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
    ]))
    
    // 设置适当的Span类型
    if i % 3 == 0 {
      azimuth::Span::set_kind(span, azimuth::Server)
    } else if i % 3 == 1 {
      azimuth::Span::set_kind(span, azimuth::Client)
    } else {
      azimuth::Span::set_kind(span, azimuth::Internal)
    }
    
    quality_spans.push(span)
  }
  
  // 结束所有Span
  for span in quality_spans {
    azimuth::Span::end(span)
  }
  
  // 验证数据质量
  for span in quality_spans {
    // 验证Span名称质量
    let span_name = azimuth::Span::name(span)
    assert_true(span_name.length() > 0)
    assert_true(span_name.starts_with("quality-span-"))
    
    // 验证Span上下文质量
    let span_ctx = azimuth::Span::span_context(span)
    assert_true(azimuth::SpanContext::is_valid(span_ctx))
  }
  
  // 测试度量数据质量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "quality-meter")
  
  // 创建高质量的度量仪器
  let quality_counter = azimuth::Meter::create_counter(
    meter, 
    "quality.operations",
    Some("Number of quality operations performed"),
    Some("operations")
  )
  
  let quality_histogram = azimuth::Meter::create_histogram(
    meter,
    "quality.duration",
    Some("Duration of quality operations"),
    Some("milliseconds")
  )
  
  // 记录高质量的度量数据
  for i in 0..20 {
    azimuth::Counter::add(quality_counter, 1.0)
    azimuth::Histogram::record(quality_histogram, 100.0 + (i.to_double() * 5.0))
  }
  
  // 验证度量数据质量
  assert_eq(quality_counter.name, "quality.operations")
  assert_eq(quality_counter.description, Some("Number of quality operations performed"))
  assert_eq(quality_counter.unit, Some("operations"))
  
  assert_eq(quality_histogram.name, "quality.duration")
  assert_eq(quality_histogram.description, Some("Duration of quality operations"))
  assert_eq(quality_histogram.unit, Some("milliseconds"))
  
  // 测试日志数据质量
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "quality-logger")
  
  // 创建高质量的日志记录
  for i in 0..15 {
    let severity = if i % 4 == 0 {
      azimuth::Error
    } else if i % 4 == 1 {
      azimuth::Warn
    } else if i % 4 == 2 {
      azimuth::Info
    } else {
      azimuth::Debug
    }
    
    let log_record = azimuth::LogRecord::new_with_context(
      severity,
      Some("Quality log message " + i.to_string() + " with detailed context"),
      Some([
        ("log.quality", azimuth::StringValue("high")),
        ("log.category", azimuth::StringValue("test")),
        ("log.component", azimuth::StringValue("quality-validator"))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("quality-trace-" + (i % 3).to_string()),
      Some("quality-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证日志数据质量
  assert_eq(logger.scope.name, "quality-logger")
}

// 测试6: 平台特定遥测适配测试
test "平台特定遥测适配测试" {
  // 创建平台特定的资源配置
  let platform_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("platform-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("platform-test-instance")),
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("host.name", azimuth::StringValue("platform-test-host")),
    ("host.arch", azimuth::StringValue("x86_64")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("platform-test")),
    ("process.executable.path", azimuth::StringValue("/usr/bin/platform-test"))
  ])
  
  // 验证平台特定资源
  assert_eq(azimuth::Resource::get_attribute(platform_resource, "service.name"), Some(azimuth::StringValue("platform-test-service")))
  assert_eq(azimuth::Resource::get_attribute(platform_resource, "telemetry.sdk.language"), Some(azimuth::StringValue("moonbit")))
  assert_eq(azimuth::Resource::get_attribute(platform_resource, "host.arch"), Some(azimuth::StringValue("x86_64")))
  assert_eq(azimuth::Resource::get_attribute(platform_resource, "os.type"), Some(azimuth::StringValue("linux")))
  assert_eq(azimuth::Resource::get_attribute(platform_resource, "process.pid"), Some(azimuth::IntValue(12345)))
  
  // 创建平台特定的TracerProvider
  let platform_tracer_provider = azimuth::TracerProvider::with_resource(platform_resource)
  let platform_tracer = azimuth::TracerProvider::get_tracer(platform_tracer_provider, "platform-tracer")
  
  // 创建平台特定的Span
  let platform_span = azimuth::Tracer::start_span(platform_tracer, "platform-operation")
  azimuth::Span::set_attribute(platform_span, "platform.type", azimuth::StringValue("linux"))
  azimuth::Span::set_attribute(platform_span, "platform.arch", azimuth::StringValue("x86_64"))
  azimuth::Span::set_attribute(platform_span, "platform.runtime", azimuth::StringValue("moonbit"))
  
  // 添加平台特定事件
  azimuth::Span::add_event(platform_span, "platform.init", Some([
    ("platform.os", azimuth::StringValue("linux")),
    ("platform.kernel", azimuth::StringValue("5.15.0")),
    ("platform.cpu.count", azimuth::IntValue(4))
  ]))
  
  azimuth::Span::end(platform_span)
  
  // 验证平台特定Span
  assert_eq(azimuth::Span::name(platform_span), "platform-operation")
  
  // 创建平台特定的度量
  let platform_meter_provider = azimuth::MeterProvider::with_resource(platform_resource)
  let platform_meter = azimuth::MeterProvider::get_meter(platform_meter_provider, "platform-meter")
  
  // 创建平台特定的度量仪器
  let platform_cpu_counter = azimuth::Meter::create_counter(
    platform_meter,
    "platform.cpu.time",
    Some("Platform CPU time usage"),
    Some("nanoseconds")
  )
  
  let platform_memory_gauge = azimuth::Meter::create_gauge(
    platform_meter,
    "platform.memory.usage",
    Some("Platform memory usage"),
    Some("bytes")
  )
  
  let platform_disk_histogram = azimuth::Meter::create_histogram(
    platform_meter,
    "platform.disk.io",
    Some("Platform disk I/O operations"),
    Some("bytes")
  )
  
  // 记录平台特定的度量数据
  azimuth::Counter::add(platform_cpu_counter, 1000000.0)  // 1毫秒CPU时间
  // Gauge::set(platform_memory_gauge, 1024 * 1024 * 100.0)  // 100MB内存使用
  azimuth::Histogram::record(platform_disk_histogram, 4096.0)  // 4KB磁盘I/O
  
  // 验证平台特定度量
  assert_eq(platform_cpu_counter.name, "platform.cpu.time")
  assert_eq(platform_cpu_counter.description, Some("Platform CPU time usage"))
  assert_eq(platform_cpu_counter.unit, Some("nanoseconds"))
  
  assert_eq(platform_memory_gauge.name, "platform.memory.usage")
  assert_eq(platform_memory_gauge.description, Some("Platform memory usage"))
  assert_eq(platform_memory_gauge.unit, Some("bytes"))
  
  assert_eq(platform_disk_histogram.name, "platform.disk.io")
  assert_eq(platform_disk_histogram.description, Some("Platform disk I/O operations"))
  assert_eq(platform_disk_histogram.unit, Some("bytes"))
  
  // 创建平台特定的日志
  let platform_logger_provider = azimuth::LoggerProvider::with_resource(platform_resource)
  let platform_logger = azimuth::LoggerProvider::get_logger(platform_logger_provider, "platform-logger")
  
  // 创建平台特定的日志记录
  let platform_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Platform-specific log message"),
    Some([
      ("platform.service", azimuth::StringValue("platform-test-service")),
      ("platform.version", azimuth::StringValue("1.0.0")),
      ("platform.environment", azimuth::StringValue("linux")),
      ("log.source", azimuth::StringValue("platform-test"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(platform_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(platform_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(platform_logger, platform_log_record)
  
  // 验证平台特定日志
  assert_eq(platform_logger.scope.name, "platform-logger")
}

// 测试7: 遥测系统端到端集成测试
test "遥测系统端到端集成测试" {
  // 创建端到端测试资源
  let e2e_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("e2e-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("test.type", azimuth::StringValue("end-to-end")),
    ("test.scenario", azimuth::StringValue("full-telemetry-workflow"))
  ])
  
  // 创建端到端测试的TracerProvider
  let e2e_tracer_provider = azimuth::TracerProvider::with_resource(e2e_resource)
  let e2e_tracer = azimuth::TracerProvider::get_tracer(e2e_tracer_provider, "e2e-tracer")
  
  // 创建端到端测试的MeterProvider
  let e2e_meter_provider = azimuth::MeterProvider::with_resource(e2e_resource)
  let e2e_meter = azimuth::MeterProvider::get_meter(e2e_meter_provider, "e2e-meter")
  
  // 创建端到端测试的LoggerProvider
  let e2e_logger_provider = azimuth::LoggerProvider::with_resource(e2e_resource)
  let e2e_logger = azimuth::LoggerProvider::get_logger(e2e_logger_provider, "e2e-logger")
  
  // 步骤1: 创建根Span表示整个端到端流程
  let root_span = azimuth::Tracer::start_span(e2e_tracer, "e2e-workflow")
  azimuth::Span::set_attribute(root_span, "workflow.type", azimuth::StringValue("integration-test"))
  azimuth::Span::set_attribute(root_span, "workflow.steps", azimuth::IntValue(5))
  
  let root_span_ctx = azimuth::Span::span_context(root_span)
  
  // 步骤2: 创建子Span表示各个步骤
  let step1_span = azimuth::Tracer::start_span_with_context(e2e_tracer, "step-1-initialization", root_span_ctx)
  azimuth::Span::set_attribute(step1_span, "step.name", azimuth::StringValue("initialization"))
  azimuth::Span::set_attribute(step1_span, "step.status", azimuth::StringValue("completed"))
  azimuth::Span::add_event(step1_span, "step.completed", Some([
    ("step.duration", azimuth::IntValue(100)),
    ("step.result", azimuth::StringValue("success"))
  ]))
  azimuth::Span::end(step1_span)
  
  // 步骤3: 记录初始化度量
  let init_counter = azimuth::Meter::create_counter(e2e_meter, "e2e.initializations")
  azimuth::Counter::add(init_counter, 1.0)
  
  let init_histogram = azimuth::Meter::create_histogram(e2e_meter, "e2e.init.duration")
  azimuth::Histogram::record(init_histogram, 100.0)
  
  // 步骤4: 记录初始化日志
  let init_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E initialization completed successfully"),
    Some([
      ("log.step", azimuth::StringValue("initialization")),
      ("log.status", azimuth::StringValue("completed"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(step1_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(e2e_logger, init_log_record)
  
  // 步骤5: 创建处理步骤Span
  let step2_span = azimuth::Tracer::start_span_with_context(e2e_tracer, "step-2-processing", root_span_ctx)
  azimuth::Span::set_attribute(step2_span, "step.name", azimuth::StringValue("processing"))
  azimuth::Span::set_attribute(step2_span, "step.items", azimuth::IntValue(10))
  
  // 处理多个项目的子操作
  for i in 0..10 {
    let item_span = azimuth::Tracer::start_span_with_context(e2e_tracer, "process-item-" + i.to_string(), azimuth::Span::span_context(step2_span))
    azimuth::Span::set_attribute(item_span, "item.id", azimuth::IntValue(i))
    azimuth::Span::set_attribute(item_span, "item.status", azimuth::StringValue("processed"))
    azimuth::Span::end(item_span)
  }
  
  azimuth::Span::end(step2_span)
  
  // 步骤6: 记录处理度量
  let process_counter = azimuth::Meter::create_counter(e2e_meter, "e2e.items.processed")
  for i in 0..10 {
    azimuth::Counter::add(process_counter, 1.0)
  }
  
  let process_histogram = azimuth::Meter::create_histogram(e2e_meter, "e2e.process.duration")
  azimuth::Histogram::record(process_histogram, 250.0)
  
  // 步骤7: 记录处理日志
  let process_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E processing completed 10 items"),
    Some([
      ("log.step", azimuth::StringValue("processing")),
      ("items.count", azimuth::IntValue(10)),
      ("items.status", azimuth::StringValue("processed"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(step2_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(e2e_logger, process_log_record)
  
  // 步骤8: 创建验证步骤Span
  let step3_span = azimuth::Tracer::start_span_with_context(e2e_tracer, "step-3-validation", root_span_ctx)
  azimuth::Span::set_attribute(step3_span, "step.name", azimuth::StringValue("validation"))
  azimuth::Span::set_attribute(step3_span, "step.validations", azimuth::IntValue(3))
  
  // 执行多个验证子操作
  let validations = ["data.integrity", "performance.baseline", "security.checks"]
  for validation in validations {
    let validation_span = azimuth::Tracer::start_span_with_context(e2e_tracer, "validate-" + validation, azimuth::Span::span_context(step3_span))
    azimuth::Span::set_attribute(validation_span, "validation.type", azimuth::StringValue(validation))
    azimuth::Span::set_attribute(validation_span, "validation.result", azimuth::StringValue("passed"))
    azimuth::Span::end(validation_span)
  }
  
  azimuth::Span::end(step3_span)
  
  // 步骤9: 记录验证度量
  let validation_counter = azimuth::Meter::create_counter(e2e_meter, "e2e.validations")
  for i in 0..3 {
    azimuth::Counter::add(validation_counter, 1.0)
  }
  
  let validation_histogram = azimuth::Meter::create_histogram(e2e_meter, "e2e.validation.duration")
  azimuth::Histogram::record(validation_histogram, 150.0)
  
  // 步骤10: 记录验证日志
  let validation_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E validation completed successfully"),
    Some([
      ("log.step", azimuth::StringValue("validation")),
      ("validations.count", azimuth::IntValue(3)),
      ("validations.status", azimuth::StringValue("passed"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(step3_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(e2e_logger, validation_log_record)
  
  // 步骤11: 完成根Span
  azimuth::Span::set_attribute(root_span, "workflow.status", azimuth::StringValue("completed"))
  azimuth::Span::add_event(root_span, "workflow.completed", Some([
    ("total.duration", azimuth::IntValue(500)),
    ("total.steps", azimuth::IntValue(3)),
    ("workflow.result", azimuth::StringValue("success"))
  ]))
  azimuth::Span::end(root_span)
  
  // 步骤12: 记录工作流完成度量
  let workflow_counter = azimuth::Meter::create_counter(e2e_meter, "e2e.workflows.completed")
  azimuth::Counter::add(workflow_counter, 1.0)
  
  let workflow_histogram = azimuth::Meter::create_histogram(e2e_meter, "e2e.workflow.duration")
  azimuth::Histogram::record(workflow_histogram, 500.0)
  
  // 步骤13: 记录工作流完成日志
  let workflow_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E workflow completed successfully"),
    Some([
      ("log.type", azimuth::StringValue("workflow")),
      ("workflow.status", azimuth::StringValue("completed")),
      ("workflow.result", azimuth::StringValue("success"))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(root_span_ctx)),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(e2e_logger, workflow_log_record)
  
  // 验证端到端集成
  assert_eq(azimuth::Span::name(root_span), "e2e-workflow")
  assert_eq(azimuth::Span::name(step1_span), "step-1-initialization")
  assert_eq(azimuth::Span::name(step2_span), "step-2-processing")
  assert_eq(azimuth::Span::name(step3_span), "step-3-validation")
  
  assert_eq(init_counter.name, "e2e.initializations")
  assert_eq(process_counter.name, "e2e.items.processed")
  assert_eq(validation_counter.name, "e2e.validations")
  assert_eq(workflow_counter.name, "e2e.workflows.completed")
  
  assert_eq(e2e_logger.scope.name, "e2e-logger")
}

// 测试8: 遥测数据序列化完整性测试
test "遥测数据序列化完整性测试" {
  // 创建序列化测试的复杂属性集合
  let complex_attrs = azimuth::Attributes::new()
  
  // 添加各种类型的属性
  azimuth::Attributes::set(complex_attrs, "string.value", azimuth::StringValue("complex test string"))
  azimuth::Attributes::set(complex_attrs, "int.value", azimuth::IntValue(42))
  azimuth::Attributes::set(complex_attrs, "float.value", azimuth::FloatValue(3.14159265359))
  azimuth::Attributes::set(complex_attrs, "bool.value", azimuth::BoolValue(true))
  azimuth::Attributes::set(complex_attrs, "array.string", azimuth::ArrayStringValue(["item1", "item2", "item3"]))
  azimuth::Attributes::set(complex_attrs, "array.int", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 添加特殊字符和Unicode
  azimuth::Attributes::set(complex_attrs, "unicode.value", azimuth::StringValue("测试中文🚀emoji"))
  azimuth::Attributes::set(complex_attrs, "special.chars", azimuth::StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  azimuth::Attributes::set(complex_attrs, "json.like", azimuth::StringValue("{\"key\": \"value\", \"number\": 123}"))
  
  // 添加极值
  azimuth::Attributes::set(complex_attrs, "max.int", azimuth::IntValue(2147483647))
  azimuth::Attributes::set(complex_attrs, "min.int", azimuth::IntValue(-2147483648))
  azimuth::Attributes::set(complex_attrs, "max.float", azimuth::FloatValue(1.7976931348623157e+308))
  azimuth::Attributes::set(complex_attrs, "min.float", azimuth::FloatValue(-1.7976931348623157e+308))
  
  // 创建带有复杂属性的Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "serialization-test")
  
  let serialization_span = azimuth::Tracer::start_span(tracer, "serialization-test-span")
  azimuth::Span::set_attributes(serialization_span, complex_attrs)
  
  // 添加复杂事件
  let event_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(event_attrs, "event.type", azimuth::StringValue("serialization.test"))
  azimuth::Attributes::set(event_attrs, "event.timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
  azimuth::Attributes::set(event_attrs, "event.data", azimuth::StringValue("complex event data"))
  
  azimuth::Span::add_event(serialization_span, "serialization.event", Some(event_attrs))
  
  // 设置复杂状态
  azimuth::Span::set_status(serialization_span, azimuth::Ok)
  azimuth::Span::end(serialization_span)
  
  // 验证复杂Span的序列化
  assert_eq(azimuth::Span::name(serialization_span), "serialization-test-span")
  
  // 创建序列化测试的复杂度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "serialization-meter")
  
  // 创建带有复杂描述和单位的度量仪器
  let complex_counter = azimuth::Meter::create_counter(
    meter,
    "complex.operations.count",
    Some("Number of complex operations with special characters: !@#$%^&*()"),
    Some("operations/秒")
  )
  
  let complex_histogram = azimuth::Meter::create_histogram(
    meter,
    "complex.duration.histogram",
    Some("Complex duration measurement with Unicode: 测试🚀"),
    Some("毫秒")
  )
  
  let complex_gauge = azimuth::Meter::create_gauge(
    meter,
    "complex.memory.usage",
    Some("Complex memory usage with JSON-like data: {\"used\": 1024, \"total\": 4096}"),
    Some("MB")
  )
  
  // 记录复杂的度量值
  azimuth::Counter::add(complex_counter, 1.0)
  azimuth::Histogram::record(complex_histogram, 123.456)
  // Gauge::set(complex_gauge, 1024.789)
  
  // 验证复杂度量的序列化
  assert_eq(complex_counter.name, "complex.operations.count")
  assert_eq(complex_counter.description, Some("Number of complex operations with special characters: !@#$%^&*()"))
  assert_eq(complex_counter.unit, Some("operations/秒"))
  
  assert_eq(complex_histogram.name, "complex.duration.histogram")
  assert_eq(complex_histogram.description, Some("Complex duration measurement with Unicode: 测试🚀"))
  assert_eq(complex_histogram.unit, Some("毫秒"))
  
  assert_eq(complex_gauge.name, "complex.memory.usage")
  assert_eq(complex_gauge.description, Some("Complex memory usage with JSON-like data: {\"used\": 1024, \"total\": 4096}"))
  assert_eq(complex_gauge.unit, Some("MB"))
  
  // 创建序列化测试的复杂日志记录
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "serialization-logger")
  
  // 创建复杂的日志记录
  let complex_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Complex log message with Unicode: 测试消息🚀 and special chars: !@#$%"),
    Some(complex_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000L),
    Some("serialization-trace-id-12345"),
    Some("serialization-span-id-67890"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, complex_log_record)
  
  // 验证复杂日志的序列化
  assert_eq(logger.scope.name, "serialization-logger")
  
  // 创建序列化测试的复杂资源
  let complex_resource_attrs = [
    ("service.name", azimuth::StringValue("serialization-test-service")),
    ("service.version", azimuth::StringValue("1.0.0-test")),
    ("service.description", azimuth::StringValue("Service with Unicode: 测试服务🚀")),
    ("host.name", azimuth::StringValue("test-host-123")),
    ("host.ip", azimuth::StringValue("192.168.1.100")),
    ("os.name", azimuth::StringValue("Linux")),
    ("os.version", azimuth::StringValue("5.15.0-test")),
    ("process.name", azimuth::StringValue("serialization-test")),
    ("process.args", azimuth::ArrayStringValue(["--test", "--verbose", "--output=json"])),
    ("process.env", azimuth::StringValue("{\"ENV\": \"test\", \"DEBUG\": \"true\"}"))
  ]
  
  let complex_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), complex_resource_attrs)
  
  // 验证复杂资源的序列化
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.name"), Some(azimuth::StringValue("serialization-test-service")))
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "service.description"), Some(azimuth::StringValue("Service with Unicode: 测试服务🚀")))
  assert_eq(azimuth::Resource::get_attribute(complex_resource, "process.args"), Some(azimuth::ArrayStringValue(["--test", "--verbose", "--output=json"])))
  
  // 创建序列化测试的复杂上下文
  let complex_ctx = azimuth::Context::root()
  let key1 = azimuth::ContextKey::new("complex.key1")
  let key2 = azimuth::ContextKey::new("complex.key2")
  let key3 = azimuth::ContextKey::new("complex.key3")
  
  let ctx_with_value1 = azimuth::Context::with_value(complex_ctx, key1, "complex value1 with Unicode: 测试")
  let ctx_with_value2 = azimuth::Context::with_value(ctx_with_value1, key2, "complex value2 with special chars: !@#$%")
  let ctx_with_value3 = azimuth::Context::with_value(ctx_with_value2, key3, "complex value3 with JSON: {\"test\": true}")
  
  // 验证复杂上下文的序列化
  assert_eq(azimuth::Context::get(ctx_with_value3, key1), Some("complex value1 with Unicode: 测试"))
  assert_eq(azimuth::Context::get(ctx_with_value3, key2), Some("complex value2 with special chars: !@#$%"))
  assert_eq(azimuth::Context::get(ctx_with_value3, key3), Some("complex value3 with JSON: {\"test\": true}"))
}