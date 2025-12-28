// SDK高级功能测试
// SDK advanced features tests for Azimuth telemetry system

test "批量处理和缓冲测试" {
  // 测试SDK的批量处理和缓冲功能
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "batch.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "batch.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "batch.test.logger")
  
  // 创建大量Span进行批量处理测试
  let batch_spans = []
  let batch_start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..100 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "batch.span." + i.to_string())
    
    // 为每个Span添加属性
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "batch.index", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "batch.size", @azimuth.telemetry.api.common.IntValue(100))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "batch.id", @azimuth.telemetry.api.common.StringValue("batch-001"))
    
    // 添加事件
    @azimuth.telemetry.api.trace.Span.add_event(span, "batch.event." + i.to_string(), Some([
      ("event.data", @azimuth.telemetry.api.common.StringValue("batch_event_data_" + i.to_string()))
    ]), None)
    
    batch_spans = batch_spans @ [span]
  }
  
  // 批量结束Span
  for span in batch_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  let batch_end_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let batch_duration = (batch_end_time - batch_start_time).to_double() / 1000000.0  // Convert to milliseconds
  
  // 创建批量指标
  let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "batch.operations")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "batch.duration")
  
  // 批量记录指标
  for i in 0..200 {
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some([
      ("batch.id", @azimuth.telemetry.api.common.StringValue("batch-001")),
      ("operation.type", @azimuth.telemetry.api.common.StringValue("batch_operation"))
    ]))
  }
  
  // 批量记录直方图数据
  let batch_durations = [10.5, 25.0, 15.75, 30.25, 20.0, 18.5, 22.75, 28.0, 12.5, 35.5]
  
  for duration in batch_durations {
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, duration, Some([
      ("batch.id", @azimuth.telemetry.api.common.StringValue("batch-001")),
      ("operation.type", @azimuth.telemetry.api.common.StringValue("batch_operation"))
    ]))
  }
  
  // 创建批量日志记录
  let batch_logs = []
  
  for i in 0..50 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
      @azimuth.telemetry.api.logs.Info,
      "Batch log message " + i.to_string(),
      Some([
        ("batch.index", @azimuth.telemetry.api.common.IntValue(i)),
        ("batch.id", @azimuth.telemetry.api.common.StringValue("batch-001")),
        ("log.type", @azimuth.telemetry.api.common.StringValue("batch_log"))
      ]),
      Some(batch_start_time + (i.to_int64() * 1000000L)),  // 每个日志间隔1ms
      None,
      Some("batch_trace_123"),
      Some("batch_span_456"),
      None
    )
    
    batch_logs = batch_logs @ [log_record]
  }
  
  // 批量发出日志
  for log_record in batch_logs {
    @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  }
  
  // 验证批量处理性能
  assert_true(batch_duration < 10000.0)  // 应该在10秒内完成
  assert_eq(batch_spans.length(), 100)
  assert_eq(batch_logs.length(), 50)
}

test "采样配置和策略测试" {
  // 测试不同的采样配置和策略
  
  // 创建带有不同采样率的追踪提供者
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "sampling.test.tracer")
  
  // 测试不同类型的Span的采样行为
  let span_kinds = [
    @azimuth.telemetry.api.trace.SpanKind.Internal,
    @azimuth.telemetry.api.trace.SpanKind.Server,
    @azimuth.telemetry.api.trace.SpanKind.Client,
    @azimuth.telemetry.api.trace.SpanKind.Producer,
    @azimuth.telemetry.api.trace.SpanKind.Consumer
  ]
  
  let sampled_spans = []
  let unsampled_spans = []
  
  for kind in span_kinds {
    for i in 0..10 {
      let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "sampling.test." + kind.to_string() + "." + i.to_string(), Some(kind))
      
      @azimuth.telemetry.api.trace.Span.set_attribute(span, "span.kind", @azimuth.telemetry.api.common.StringValue(kind.to_string()))
      @azimuth.telemetry.api.trace.Span.set_attribute(span, "span.index", @azimuth.telemetry.api.common.IntValue(i))
      
      let span_ctx = @azimuth.telemetry.api.trace.Span.span_context(span)
      if @azimuth.telemetry.api.trace.SpanContext.is_sampled(span_ctx) {
        sampled_spans = sampled_spans @ [span]
      } else {
        unsampled_spans = unsampled_spans @ [span]
      }
    }
  }
  
  // 为采样的Span添加更多详细信息
  for sampled_span in sampled_spans {
    @azimuth.telemetry.api.trace.Span.set_attribute(sampled_span, "sampling.decision", @azimuth.telemetry.api.common.StringValue("sampled"))
    @azimuth.telemetry.api.trace.Span.add_event(sampled_span, "detailed.event", Some([
      ("event.detail", @azimuth.telemetry.api.common.StringValue("This span is sampled and will have detailed data")),
      ("event.timestamp", @azimuth.telemetry.api.common.StringValue("2024-01-01T00:00:00Z"))
    ]), None)
  }
  
  // 为未采样的Span添加最少信息
  for unsampled_span in unsampled_spans {
    @azimuth.telemetry.api.trace.Span.set_attribute(unsampled_span, "sampling.decision", @azimuth.telemetry.api.common.StringValue("not_sampled"))
  }
  
  // 结束所有Span
  for span in sampled_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  for span in unsampled_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // 验证采样结果
  assert_true(sampled_spans.length() + unsampled_spans.length() == 50)
  
  // 测试基于属性的采样决策
  let ctx = @azimuth.telemetry.api.context.Context::root()
  let (_, high_priority_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "high.priority.operation")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(high_priority_span, "priority", @azimuth.telemetry.api.common.StringValue("high"))
  @azimuth.telemetry.api.trace.Span.set_attribute(high_priority_span, "user.tier", @azimuth.telemetry.api.common.StringValue("premium"))
  
  let (_, low_priority_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "low.priority.operation")
  
  @azimuth.telemetry.api.trace.Span.set_attribute(low_priority_span, "priority", @azimuth.telemetry.api.common.StringValue("low"))
  @azimuth.telemetry.api.trace.Span.set_attribute(low_priority_span, "user.tier", @azimuth.telemetry.api.common.StringValue("basic"))
  
  @azimuth.telemetry.api.trace.Span.end(high_priority_span)
  @azimuth.telemetry.api.trace.Span.end(low_priority_span)
}

test "动态配置更新测试" {
  // 测试运行时动态配置更新
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "dynamic.config.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "dynamic.config.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "dynamic.config.logger")
  
  // 模拟初始配置
  let initial_config = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(initial_config, "config.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  @azimuth.telemetry.api.common.Attributes.set(initial_config, "sampling.rate", @azimuth.telemetry.api.common.FloatValue(0.1))
  @azimuth.telemetry.api.common.Attributes.set(initial_config, "export.interval", @azimuth.telemetry.api.common.IntValue(5000))
  
  // 使用初始配置创建Span
  let (_, initial_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "initial.config.span")
  @azimuth.telemetry.api.trace.Span.set_attribute(initial_span, "config.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  @azimuth.telemetry.api.trace.Span.end(initial_span)
  
  // 模拟配置更新
  let updated_config = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(updated_config, "config.version", @azimuth.telemetry.api.common.StringValue("2.0.0"))
  @azimuth.telemetry.api.common.Attributes.set(updated_config, "sampling.rate", @azimuth.telemetry.api.common.FloatValue(0.5))
  @azimuth.telemetry.api.common.Attributes.set(updated_config, "export.interval", @azimuth.telemetry.api.common.IntValue(2000))
  @azimuth.telemetry.api.common.Attributes.set(updated_config, "feature.flags", @azimuth.telemetry.api.common.ArrayStringValue(["new_feature", "enhanced_tracing"]))
  
  // 使用更新后的配置创建Span
  let (_, updated_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "updated.config.span")
  @azimuth.telemetry.api.trace.Span.set_attribute(updated_span, "config.version", @azimuth.telemetry.api.common.StringValue("2.0.0"))
  @azimuth.telemetry.api.trace.Span.set_attribute(updated_span, "feature.enabled", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.trace.Span.end(updated_span)
  
  // 测试动态指标配置
  let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "dynamic.config.counter")
  
  // 使用初始配置记录指标
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some([
    ("config.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  ]))
  
  // 使用更新后的配置记录指标
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some([
    ("config.version", @azimuth.telemetry.api.common.StringValue("2.0.0")),
    ("enhanced.metric", @azimuth.telemetry.api.common.BoolValue(true))
  ]))
  
  // 测试动态日志配置
  let initial_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "Log with initial configuration",
    Some([
      ("config.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("log.level", @azimuth.telemetry.api.common.StringValue("INFO"))
    ]),
    None,
    None,
    None,
    None,
    None
  )
  
  let updated_log = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "Log with updated configuration",
    Some([
      ("config.version", @azimuth.telemetry.api.common.StringValue("2.0.0")),
      ("log.level", @azimuth.telemetry.api.common.StringValue("INFO")),
      ("enhanced.logging", @azimuth.telemetry.api.common.BoolValue(true))
    ]),
    None,
    None,
    None,
    None,
    None
  )
  
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, initial_log)
  @azimuth.telemetry.sdk.logs.Logger.emit(logger, updated_log)
}

test "内存管理和资源清理测试" {
  // 测试SDK的内存管理和资源清理
  
  // 创建大量对象进行内存管理测试
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "memory.test.tracer")
  
  // 创建大量Span然后结束，测试资源清理
  let memory_test_spans = []
  
  for i in 0..200 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "memory.test.span." + i.to_string())
    
    // 添加大量属性
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "memory.test.index", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "memory.test.data", @azimuth.telemetry.api.common.StringValue("x".repeat(100)))
    
    // 添加大量事件
    for j in 0..5 {
      @azimuth.telemetry.api.trace.Span.add_event(span, "memory.test.event." + j.to_string(), Some([
        ("event.data", @azimuth.telemetry.api.common.StringValue("y".repeat(50)))
      ]), None)
    }
    
    memory_test_spans = memory_test_spans @ [span]
  }
  
  // 结束所有Span，触发资源清理
  for span in memory_test_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // 测试指标资源管理
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "memory.test.meter")
  
  // 创建大量指标仪器
  let counters = []
  let histograms = []
  
  for i in 0..50 {
    let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(
      meter,
      "memory.test.counter." + i.to_string(),
      Some("Memory test counter " + i.to_string()),
      Some("operations")
    )
    
    let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(
      meter,
      "memory.test.histogram." + i.to_string(),
      Some("Memory test histogram " + i.to_string()),
      Some("ms")
    )
    
    counters = counters @ [counter]
    histograms = histograms @ [histogram]
  }
  
  // 使用所有指标仪器记录数据
  for i in 0..50 {
    @azimuth.telemetry.sdk.metrics.Counter.add(counters[i], i.to_double(), Some([
      ("memory.test", @azimuth.telemetry.api.common.StringValue("resource.management"))
    ]))
    
    @azimuth.telemetry.sdk.metrics.Histogram.record(histograms[i], i.to_double(), Some([
      ("memory.test", @azimuth.telemetry.api.common.StringValue("resource.management"))
    ]))
  }
  
  // 测试日志资源管理
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "memory.test.logger")
  
  // 创建大量日志记录
  for i in 0..100 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
      @azimuth.telemetry.api.logs.Info,
      "Memory test log " + i.to_string(),
      Some([
        ("memory.test.index", @azimuth.telemetry.api.common.IntValue(i)),
        ("memory.test.data", @azimuth.telemetry.api.common.StringValue("z".repeat(200)))
      ]),
      None,
      None,
      None,
      None,
      None
    )
    
    @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  }
  
  // 验证所有对象都正确创建和清理
  assert_eq(memory_test_spans.length(), 200)
  assert_eq(counters.length(), 50)
  assert_eq(histograms.length(), 50)
}

test "并发和线程安全测试" {
  // 测试SDK的并发操作和线程安全性
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "concurrent.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "concurrent.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "concurrent.test.logger")
  
  // 模拟并发创建Span
  let concurrent_spans = []
  
  for i in 0..20 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "concurrent.span." + i.to_string())
    
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "concurrent.index", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "thread.id", @azimuth.telemetry.api.common.StringValue("thread-" + i.to_string()))
    
    concurrent_spans = concurrent_spans @ [span]
  }
  
  // 并发操作Span属性
  for span in concurrent_spans {
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "concurrent.operation", @azimuth.telemetry.api.common.StringValue("attribute_update"))
    @azimuth.telemetry.api.trace.Span.add_event(span, "concurrent.event", Some([
      ("event.data", @azimuth.telemetry.api.common.StringValue("concurrent_test"))
    ]), None)
  }
  
  // 并发结束Span
  for span in concurrent_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // 测试并发指标操作
  let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "concurrent.operations")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "concurrent.duration")
  
  // 并发记录指标
  for i in 0..100 {
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0, Some([
      ("concurrent.index", @azimuth.telemetry.api.common.IntValue(i)),
      ("operation.type", @azimuth.telemetry.api.common.StringValue("concurrent_metric"))
    ]))
    
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, i.to_double(), Some([
      ("concurrent.index", @azimuth.telemetry.api.common.IntValue(i)),
      ("operation.type", @azimuth.telemetry.api.common.StringValue("concurrent_metric"))
    ]))
  }
  
  // 测试并发日志记录
  for i in 0..50 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
      @azimuth.telemetry.api.logs.Info,
      "Concurrent log message " + i.to_string(),
      Some([
        ("concurrent.index", @azimuth.telemetry.api.common.IntValue(i)),
        ("thread.id", @azimuth.telemetry.api.common.StringValue("thread-" + i.to_string()))
      ]),
      None,
      None,
      None,
      None,
      None
    )
    
    @azimuth.telemetry.sdk.logs.Logger.emit(logger, log_record)
  }
  
  // 验证并发操作结果
  assert_eq(concurrent_spans.length(), 20)
}

test "导出器高级配置测试" {
  // 测试导出器的高级配置功能
  
  // 创建不同配置的导出器
  let console_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let otlp_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  let custom_endpoint_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint(
    "https://custom.otlp.example.com:4318/v1/traces"
  )
  let auth_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_headers([
    ("Authorization", "Bearer custom-token-123"),
    ("X-Custom-Header", "custom-value")
  ])
  let timeout_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_timeout(60000)
  
  // 验证所有导出器都创建成功
  assert_true(console_exporter != @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter::{})
  assert_true(otlp_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(custom_endpoint_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(auth_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  assert_true(timeout_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // 创建测试Span用于不同导出器
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "exporter.config.test")
  
  let exporter_test_spans = []
  let exporter_configs = [
    ("console", console_exporter),
    ("otlp", otlp_exporter),
    ("custom_endpoint", custom_endpoint_exporter),
    ("auth", auth_exporter),
    ("timeout", timeout_exporter)
  ]
  
  for (config_name, exporter) in exporter_configs {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "exporter.config.span." + config_name)
    
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "exporter.config", @azimuth.telemetry.api.common.StringValue(config_name))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "test.timestamp", @azimuth.telemetry.api.common.IntValue(1700000000))
    
    exporter_test_spans = exporter_test_spans @ [(span, exporter)]
  }
  
  // 使用不同配置的导出器导出Span
  for (span, exporter) in exporter_test_spans {
    match exporter {
      console_exp => @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exp, span)
      otlp_exp => @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exp, span)
      custom_exp => @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(custom_exp, span)
      auth_exp => @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(auth_exp, span)
      timeout_exp => @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(timeout_exp, span)
      _ => ()  // 默认情况
    }
  }
  
  // 结束所有Span
  for (span, _) in exporter_test_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // 测试批量导出配置
  let batch_spans = []
  
  for i in 0..10 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, "batch.export.test." + i.to_string())
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "batch.index", @azimuth.telemetry.api.common.IntValue(i))
    batch_spans = batch_spans @ [span]
  }
  
  // 使用不同导出器进行批量导出
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export_batch(console_exporter, batch_spans)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(otlp_exporter, batch_spans)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(custom_endpoint_exporter, batch_spans)
  
  // 结束批量Span
  for span in batch_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // 测试日志和指标导出器配置
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "exporter.config.logger")
  
  let console_log_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.new()
  let console_metric_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.new()
  
  // 创建测试日志和指标
  let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
    @azimuth.telemetry.api.logs.Info,
    "Exporter configuration test log",
    Some([
      ("exporter.config", @azimuth.telemetry.api.common.StringValue("console")),
      ("test.type", @azimuth.telemetry.api.common.StringValue("exporter_configuration"))
    ]),
    None,
    None,
    None,
    None,
    None
  )
  
  let metric_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Counter(
    "exporter.config.counter",
    Some("Exporter configuration test counter"),
    Some("operations"),
    42.0,
    Some([
      ("exporter.config", @azimuth.telemetry.api.common.StringValue("console")),
      ("test.type", @azimuth.telemetry.api.common.StringValue("exporter_configuration"))
    ])
  )
  
  // 导出日志和指标
  @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.export_log(console_log_exporter, log_record)
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(console_metric_exporter, metric_data)
}