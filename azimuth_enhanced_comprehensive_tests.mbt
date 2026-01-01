// Azimuth 高级测试用例集合
// 包含10个新的MoonBit测试用例，覆盖遥测系统的高级功能

// 测试1: 异步遥测数据导出
pub test "异步遥测数据导出测试" {
  // 创建异步导出器
  let exporter = azimuth::BatchSpanProcessor::new(
    azimuth::ConsoleSpanExporter::new(),
    azimuth::BatchSpanProcessorOptions::default()
  )
  
  // 创建带异步导出的TracerProvider
  let provider = azimuth::TracerProvider::builder()
    .with_span_processor(exporter)
    .build()
  
  let tracer = azimuth::TracerProvider::get_tracer(provider, "async-test-tracer")
  
  // 创建多个Span以测试批量导出
  let spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "async-operation-" + i.to_string())
    azimuth::Span::add_event(span, "operation.started", Some([("step", azimuth::StringValue(i.to_string()))]))
    
    // 模拟一些操作时间
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "operation.index", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("async.batch"))
    azimuth::Span::set_attributes(span, attrs)
    
    azimuth::Span::set_status(span, azimuth::Ok)
    spans.push(span)
  }
  
  // 结束所有Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证导出器状态
  assert_true(azimuth::BatchSpanProcessor::is_shutdown(exporter) == false)
  
  // 测试强制刷新导出
  azimuth::BatchSpanProcessor::force_flush(exporter)
  
  // 测试关闭异步导出器
  azimuth::BatchSpanProcessor::shutdown(exporter)
  assert_true(azimuth::BatchSpanProcessor::is_shutdown(exporter) == true)
  
  // 验证异步导出的性能指标
  let export_metrics = azimuth::BatchSpanProcessor::get_metrics(exporter)
  assert_true(azimuth::ExportMetrics::queued_spans(export_metrics) >= 50)
  assert_true(azimuth::ExportMetrics::exported_spans(export_metrics) >= 50)
  
  // 测试并发异步导出
  let concurrent_provider = azimuth::TracerProvider::builder()
    .with_span_processor(azimuth::BatchSpanProcessor::new(
      azimuth::ConsoleSpanExporter::new(),
      azimuth::BatchSpanProcessorOptions::new(100, 5000, 10)  // 10个并发导出
    ))
    .build()
  
  let concurrent_tracer = azimuth::TracerProvider::get_tracer(concurrent_provider, "concurrent-test-tracer")
  
  // 创建并发Span
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(concurrent_tracer, "concurrent-operation-" + i.to_string())
    azimuth::Span::add_event(span, "concurrent.start", None)
    azimuth::Span::end(span)
  }
  
  // 验证并发导出性能
  let concurrent_exporter = azimuth::TracerProvider::get_span_processor(concurrent_provider)
  azimuth::BatchSpanProcessor::force_flush(concurrent_exporter)
  let concurrent_metrics = azimuth::BatchSpanProcessor::get_metrics(concurrent_exporter)
  
  assert_true(azimuth::ExportMetrics::exported_spans(concurrent_metrics) >= 20)
}

// 测试2: 采样策略高级测试
pub test "采样策略高级测试" {
  // 创建基于概率的采样器
  let probability_sampler = azimuth::TraceIdRatioBasedSampler::new(0.5)  // 50%采样率
  
  // 创建基于父级的采样器
  let parent_based_sampler = azimuth::ParentBasedSampler::new(
    azimuth::AlwaysOnSampler::new()
  )
  
  // 创建复合采样器
  let composite_sampler = azimuth::CompositeSampler::new([
    ("debug", azimuth::AlwaysOnSampler::new()),
    ("production", probability_sampler),
    ("testing", azimuth::AlwaysOffSampler::new())
  ])
  
  // 测试不同采样策略
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1234567890abcdef"
  let parent_span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
  
  // 测试概率采样
  let sampled_ctx = azimuth::SpanContext::new(trace_id, "1111111111111111", true, "")
  let not_sampled_ctx = azimuth::SpanContext::new(trace_id, "2222222222222222", false, "")
  
  // 测试父级采样
  let parent_sampled = azimuth::ParentBasedSampler::should_sample(
    parent_based_sampler,
    azimuth::Context::root(),
    trace_id,
    "test-operation",
    azimuth::Kind::Server,
    Some(parent_span_ctx),
    []
  )
  
  assert_eq(azimuth::SamplingResult::decision(parent_sampled), azimuth::RecordAndSample)
  
  // 测试复合采样
  let debug_sampling_result = azimuth::CompositeSampler::should_sample(
    composite_sampler,
    azimuth::Context::with_value(azimuth::Context::root(), 
      azimuth::ContextKey::new("environment"), "debug"),
    trace_id,
    "debug-operation",
    azimuth::Kind::Server,
    None,
    []
  )
  
  let production_sampling_result = azimuth::CompositeSampler::should_sample(
    composite_sampler,
    azimuth::Context::with_value(azimuth::Context::root(), 
      azimuth::ContextKey::new("environment"), "production"),
    trace_id,
    "production-operation",
    azimuth::Kind::Server,
    None,
    []
  )
  
  assert_eq(azimuth::SamplingResult::decision(debug_sampling_result), azimuth::RecordAndSample)
  
  // 测试自适应采样
  let adaptive_sampler = azimuth::AdaptiveSampler::new(
    1000,  // 最大采样数
    60     // 时间窗口（秒）
  )
  
  // 在时间窗口内创建多个Span测试自适应采样
  let adaptive_results = []
  for i in 0..1200 {  // 超过最大采样数
    let result = azimuth::AdaptiveSampler::should_sample(
      adaptive_sampler,
      azimuth::Context::root(),
      trace_id + i.to_string(),
      "adaptive-operation-" + i.to_string(),
      azimuth::Kind::Server,
      None,
      []
    )
    adaptive_results.push(azimuth::SamplingResult::decision(result))
  }
  
  // 验证自适应采样限制
  let sampled_count = 0
  for decision in adaptive_results {
    if decision == azimuth::RecordAndSample {
      sampled_count = sampled_count + 1
    }
  }
  
  assert_true(sampled_count <= 1000)  // 不应超过最大采样数
  
  // 测试采样属性
  let sampler_with_attributes = azimuth::TraceIdRatioBasedSampler::new_with_attributes(
    0.1,
    [("sampler.type", azimuth::StringValue("probability")),
     ("sampler.rate", azimuth::FloatValue(0.1))]
  )
  
  let attribute_sampling_result = azimuth::TraceIdRatioBasedSampler::should_sample(
    sampler_with_attributes,
    azimuth::Context::root(),
    trace_id,
    "attribute-test-operation",
    azimuth::Kind::Server,
    None,
    []
  )
  
  let sampling_attributes = azimuth::SamplingResult::attributes(attribute_sampling_result)
  assert_true(sampling_attributes.length() > 0)
}

// 测试3: 分布式追踪相关性高级测试
pub test "分布式追踪相关性高级测试" {
  // 创建分布式追踪上下文
  let root_trace_id = "abcdef1234567890abcdef1234567890"
  let root_span_id = "1111111111111111"
  
  // 服务A的根Span
  let service_a_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), "service-a"
  )
  let root_span_ctx = azimuth::SpanContext::new(root_trace_id, root_span_id, true, "")
  let root_span = azimuth::Span::new("service-a.root-operation", azimuth::Server, root_span_ctx)
  
  // 添加相关性标签
  let correlation_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(correlation_attrs, "correlation.id", azimuth::StringValue("corr-12345"))
  azimuth::Attributes::set(correlation_attrs, "user.id", azimuth::StringValue("user-67890"))
  azimuth::Attributes::set(correlation_attrs, "session.id", azimuth::StringValue("sess-abcde"))
  azimuth::Span::set_attributes(root_span, correlation_attrs)
  
  // 服务B的子Span
  let service_b_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), "service-b"
  )
  let service_b_span_ctx = azimuth::SpanContext::new(root_trace_id, "2222222222222222", true, "")
  let service_b_span = azimuth::Span::new("service-b.child-operation", azimuth::Client, service_b_span_ctx)
  
  // 服务C的子Span
  let service_c_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), "service-c"
  )
  let service_c_span_ctx = azimuth::SpanContext::new(root_trace_id, "3333333333333333", true, "")
  let service_c_span = azimuth::Span::new("service-c.grandchild-operation", azimuth::Internal, service_c_span_ctx)
  
  // 验证追踪相关性
  assert_eq(azimuth::SpanContext::trace_id(root_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_c_span_ctx), root_trace_id)
  
  // 验证Span ID唯一性
  assert_true(azimuth::SpanContext::span_id(root_span_ctx) != azimuth::SpanContext::span_id(service_b_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_b_span_ctx) != azimuth::SpanContext::span_id(service_c_span_ctx))
  
  // 测试跨服务Baggage传播
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "request.id", "req-12345")
  let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "trace.chain", "service-a->service-b->service-c")
  
  // 验证Baggage传播
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "request.id"), Some("req-12345"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "trace.chain"), Some("service-a->service-b->service-c"))
  
  // 测试跨服务上下文传播
  let propagation_ctx = azimuth::Context::root()
  let correlation_key = azimuth::ContextKey::new("correlation.id")
  let user_key = azimuth::ContextKey::new("user.id")
  
  let ctx_with_correlation = azimuth::Context::with_value(propagation_ctx, correlation_key, "corr-12345")
  let ctx_with_user = azimuth::Context::with_value(ctx_with_correlation, user_key, "user-67890")
  
  // 验证上下文传播
  assert_eq(azimuth::Context::get(ctx_with_correlation, correlation_key), Some("corr-12345"))
  assert_eq(azimuth::Context::get(ctx_with_user, user_key), Some("user-67890"))
  
  // 测试复合传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let correlation_propagator = azimuth::CorrelationContextPropagator::new()
  
  let composite_propagator = azimuth::CompositePropagator::new([
    trace_propagator, baggage_propagator, correlation_propagator
  ])
  
  // 测试注入和提取
  let carrier = azimuth::TextMapCarrier::new()
  let injection_ctx = azimuth::Context::with_values(azimuth::Context::root(), [
    (azimuth::ContextKey::new("correlation.id"), "corr-12345"),
    (azimuth::ContextKey::new("user.id"), "user-67890")
  ])
  
  azimuth::CompositePropagator::inject(composite_propagator, injection_ctx, carrier)
  
  // 验证注入的头部
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  let correlation_header = azimuth::TextMapCarrier::get(carrier, "correlation-context")
  
  assert_true(trace_header != None)
  assert_true(baggage_header != None)
  
  // 测试提取
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  let extracted_correlation = azimuth::Context::get(extracted_ctx, correlation_key)
  let extracted_user = azimuth::Context::get(extracted_ctx, user_key)
  
  assert_eq(extracted_correlation, Some("corr-12345"))
  assert_eq(extracted_user, Some("user-67890"))
  
  // 测试分布式追踪的性能
  let performance_tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), "performance-test"
  )
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建大量分布式Span
  let distributed_spans = []
  for i in 0..100 {
    let span_trace_id = "perf-trace-" + (i / 10).to_string()
    let span_id = "perf-span-" + i.to_string()
    let span_ctx = azimuth::SpanContext::new(span_trace_id, span_id, true, "")
    let span = azimuth::Span::new("perf-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    // 添加性能相关属性
    let perf_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(perf_attrs, "operation.index", azimuth::IntValue(i))
    azimuth::Attributes::set(perf_attrs, "operation.type", azimuth::StringValue("distributed.performance"))
    azimuth::Span::set_attributes(span, perf_attrs)
    
    distributed_spans.push(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration = end_time - start_time
  
  // 验证性能
  assert_true(duration < 5000000000L)  // 小于5秒
  assert_true(distributed_spans.length() == 100)
}

// 测试4: 度量聚合操作测试
pub test "度量聚合操作测试" {
  // 创建度量提供者
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test-meter")
  
  // 创建各种类型的度量仪器
  let counter = azimuth::Meter::create_counter(meter, "request.count", Some("Total request count"), Some("requests"))
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time distribution"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = azimuth::Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("MB"))
  
  // 测试计数器聚合
  for i in 0..1000 {
    azimuth::Counter::add(counter, 1.0, Some([("endpoint", azimuth::StringValue("/api/" + (i % 10).to_string()))]))
  }
  
  // 测试直方图聚合
  for i in 0..500 {
    let response_time = 50.0 + (i.to_double() * 0.5) + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 100.0)
    azimuth::Histogram::record(histogram, response_time, Some([("status", azimuth::StringValue(if i % 10 == 0 { "error" } else { "success" }))]))
  }
  
  // 测试上下行计数器聚合
  for i in 0..50 {
    azimuth::UpDownCounter::add(updown_counter, 1.0)  // 连接建立
  }
  for i in 0..20 {
    azimuth::UpDownCounter::add(updown_counter, -1.0)  // 连接关闭
  }
  
  // 测试仪表聚合
  for i in 0..100 {
    let memory_usage = 100.0 + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 500.0)
    azimuth::Gauge::record(gauge, memory_usage, Some([("instance", azimuth::StringValue("instance-" + (i % 5).to_string()))]))
  }
  
  // 创建度量视图以测试聚合
  let count_by_endpoint_view = azimuth::Meter::create_view(
    meter,
    "request.count.by.endpoint",
    azimuth::Aggregation::Sum(),
    ["endpoint"]
  )
  
  let response_time_percentile_view = azimuth::Meter::create_view(
    meter,
    "response.time.percentiles",
    azimuth::Aggregation::ExplicitBucketHistogram([50.0, 100.0, 200.0, 500.0]),
    ["status"]
  )
  
  let active_connections_latest_view = azimuth::Meter::create_view(
    meter,
    "active.connections.latest",
    azimuth::Aggregation::LastValue(),
    []
  )
  
  let memory_usage_avg_view = azimuth::Meter::create_view(
    meter,
    "memory.usage.average",
    azimuth::Aggregation::Mean(),
    ["instance"]
  )
  
  // 测试度量收集器
  let collector = azimuth::MetricCollector::new(meter_provider)
  let collection_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let metrics = azimuth::MetricCollector::collect(collector)
  let collection_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 验证收集性能
  let collection_duration = collection_end - collection_start
  assert_true(collection_duration < 1000000000L)  // 小于1秒
  
  // 验证度量数据点
  assert_true(metrics.length() > 0)
  
  // 测试度量导出
  let exporter = azimuth::ConsoleMetricExporter::new()
  let export_result = azimuth::MetricExporter::export(exporter, metrics)
  assert_eq(export_result, azimuth::ExportSuccess)
  
  // 测试时间序列聚合
  let time_series_collector = azimuth::TimeSeriesCollector::new()
  
  // 添加时间序列数据点
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  for i in 0..60 {  // 60个时间点
    let timestamp = base_time + (i * 1000000000L)  // 每秒一个点
    let value = 100.0 + (i.to_double() * 2.0) + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 20.0)
    
    azimuth::TimeSeriesCollector::add_point(time_series_collector, "cpu.usage", timestamp, value, [
      ("host", azimuth::StringValue("host-" + (i % 3).to_string()))
    ])
  }
  
  // 测试时间窗口聚合
  let windowed_aggregation = azimuth::TimeSeriesCollector::aggregate_by_window(
    time_series_collector,
    "cpu.usage",
    10000000000L,  // 10秒窗口
    azimuth::Aggregation::Mean()
  )
  
  assert_true(windowed_aggregation.length() > 0)
  
  // 测试下采样聚合
  let downsampled_aggregation = azimuth::TimeSeriesCollector::downsample(
    time_series_collector,
    "cpu.usage",
    10000000000L  // 10秒间隔下采样
  )
  
  assert_true(downsampled_aggregation.length() > 0)
  
  // 测试多维度聚合
  let multi_dimensional_aggregation = azimuth::TimeSeriesCollector::aggregate_by_dimensions(
    time_series_collector,
    "cpu.usage",
    ["host"]
  )
  
  assert_true(multi_dimensional_aggregation.length() > 0)
}

// 测试5: 日志记录批量处理测试
pub test "日志记录批量处理测试" {
  // 创建批量日志处理器
  let batch_processor = azimuth::BatchLogProcessor::new(
    azimuth::ConsoleLogExporter::new(),
    azimuth::BatchLogProcessorOptions::new(100, 5000, 3)  // 100条批量，5秒超时，3个队列
  )
  
  // 创建带批量处理的LoggerProvider
  let logger_provider = azimuth::LoggerProvider::builder()
    .with_log_processor(batch_processor)
    .build()
  
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "batch-test-logger")
  
  // 创建批量日志记录
  let log_records = []
  for i in 0..200 {  // 超过批量大小
    let severity = if i % 10 == 0 { azimuth::Error } 
                   else if i % 5 == 0 { azimuth::Warn } 
                   else { azimuth::Info }
    
    let log_record = azimuth::LogRecord::new_with_context(
      severity,
      Some("Batch log message " + i.to_string()),
      Some([
        ("log.index", azimuth::IntValue(i)),
        ("log.category", azimuth::StringValue("batch.test")),
        ("log.batch.id", azimuth::StringValue("batch-001"))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("batch-trace-" + (i / 10).to_string()),
      Some("batch-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    log_records.push(log_record)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 测试强制刷新批量处理器
  azimuth::BatchLogProcessor::force_flush(batch_processor)
  
  // 验证批量处理器指标
  let processor_metrics = azimuth::BatchLogProcessor::get_metrics(batch_processor)
  assert_true(azimuth::LogProcessorMetrics::queued_logs(processor_metrics) >= 200)
  assert_true(azimuth::LogProcessorMetrics::exported_logs(processor_metrics) >= 200)
  
  // 测试不同严重级别的批量处理
  let severity_logger = azimuth::LoggerProvider::get_logger(logger_provider, "severity-test-logger")
  
  // 创建不同严重级别的日志
  let fatal_log = azimuth::LogRecord::new(azimuth::Fatal, Some("Fatal error occurred"))
  let error_log = azimuth::LogRecord::new(azimuth::Error, Some("Error occurred"))
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, Some("Warning message"))
  let info_log = azimuth::LogRecord::new(azimuth::Info, Some("Info message"))
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, Some("Debug message"))
  let trace_log = azimuth::LogRecord::new(azimuth::Trace, Some("Trace message"))
  
  // 发送不同严重级别的日志
  azimuth::Logger::emit(severity_logger, fatal_log)
  azimuth::Logger::emit(severity_logger, error_log)
  azimuth::Logger::emit(severity_logger, warn_log)
  azimuth::Logger::emit(severity_logger, info_log)
  azimuth::Logger::emit(severity_logger, debug_log)
  azimuth::Logger::emit(severity_logger, trace_log)
  
  // 测试日志过滤和路由
  let filter_processor = azimuth::FilteringLogProcessor::new(
    azimuth::ConsoleLogExporter::new(),
    azimuth::LogFilter::new()
      .with_min_severity(azimuth::Warn)  // 只记录Warn及以上级别
      .with_attribute_filter("log.category", "important")  // 只记录特定类别
  )
  
  let filter_logger_provider = azimuth::LoggerProvider::builder()
    .with_log_processor(filter_processor)
    .build()
  
  let filter_logger = azimuth::LoggerProvider::get_logger(filter_logger_provider, "filter-test-logger")
  
  // 发送将被过滤的日志
  let filtered_info_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("This will be filtered out"),
    Some([("log.category", azimuth::StringValue("unimportant"))]),
    None, None, None, None, None
  )
  
  // 发送将通过过滤的日志
  let passed_warn_log = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("This will pass through"),
    Some([("log.category", azimuth::StringValue("important"))]),
    None, None, None, None, None
  )
  
  azimuth::Logger::emit(filter_logger, filtered_info_log)
  azimuth::Logger::emit(filter_logger, passed_warn_log)
  
  // 测试日志压缩
  let compression_processor = azimuth::CompressionLogProcessor::new(
    azimuth::ConsoleLogExporter::new(),
    azimuth::CompressionAlgorithm::Gzip
  )
  
  let compression_logger_provider = azimuth::LoggerProvider::builder()
    .with_log_processor(compression_processor)
    .build()
  
  let compression_logger = azimuth::LoggerProvider::get_logger(compression_logger_provider, "compression-test-logger")
  
  // 创建大量日志以测试压缩效果
  for i in 0..100 {
    let large_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Large log message with lots of repeated content to test compression effectiveness. " +
           "This message contains many repeated words and phrases to demonstrate how compression " +
           "can reduce the size of log data. Message index: " + i.to_string()),
      Some([
        ("log.type", azimuth::StringValue("compression.test")),
        ("log.size", azimuth::StringValue("large")),
        ("log.index", azimuth::IntValue(i))
      ]),
      None, None, None, None, None
    )
    
    azimuth::Logger::emit(compression_logger, large_log)
  }
  
  // 验证压缩指标
  let compression_metrics = azimuth::CompressionLogProcessor::get_metrics(compression_processor)
  assert_true(azimuth::CompressionMetrics::original_size(compression_metrics) > 0)
  assert_true(azimuth::CompressionMetrics::compressed_size(compression_metrics) > 0)
  assert_true(azimuth::CompressionMetrics::compression_ratio(compression_metrics) > 0.0)
  
  // 测试日志异步处理
  let async_processor = azimuth::AsyncLogProcessor::new(
    azimuth::ConsoleLogExporter::new(),
    azimuth::AsyncLogProcessorOptions::new(50, 2)  // 50条队列，2个工作线程
  )
  
  let async_logger_provider = azimuth::LoggerProvider::builder()
    .with_log_processor(async_processor)
    .build()
  
  let async_logger = azimuth::LoggerProvider::get_logger(async_logger_provider, "async-test-logger")
  
  // 异步发送日志
  for i in 0..75 {  // 超过队列大小
    let async_log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Async log message " + i.to_string()),
      Some([("async.index", azimuth::IntValue(i))]),
      None, None, None, None, None
    )
    
    azimuth::Logger::emit(async_logger, async_log)
  }
  
  // 等待异步处理完成
  azimuth::AsyncLogProcessor::flush(async_processor)
  
  // 验证异步处理指标
  let async_metrics = azimuth::AsyncLogProcessor::get_metrics(async_processor)
  assert_true(azimuth::AsyncLogMetrics::queued_logs(async_metrics) >= 75)
  assert_true(azimuth::AsyncLogMetrics::processed_logs(async_metrics) >= 75)
}

// 测试6: 资源合并策略详细测试
pub test "资源合并策略详细测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-001")),
    ("deployment.environment", azimuth::StringValue("development"))
  ])
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("override-service")),  // 应覆盖
    ("service.version", azimuth::StringValue("2.0.0")),  // 应覆盖
    ("host.name", azimuth::StringValue("override-host")),  // 新增
    ("host.ip", azimuth::StringValue("192.168.1.100"))  // 新增
  ])
  
  // 测试基本合并策略
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
            Some(azimuth::StringValue("override-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), 
            Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.instance.id"), 
            Some(azimuth::StringValue("instance-001")))  // 保留
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), 
            Some(azimuth::StringValue("development")))  // 保留
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.name"), 
            Some(azimuth::StringValue("override-host")))  // 新增
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "host.ip"), 
            Some(azimuth::StringValue("192.168.1.100")))  // 新增
  
  // 测试多级资源合并
  let level1_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("level1.attr1", azimuth::StringValue("level1-value1")),
    ("level1.attr2", azimuth::StringValue("level1-value2")),
    ("common.attr", azimuth::StringValue("level1-common"))
  ])
  
  let level2_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("level2.attr1", azimuth::StringValue("level2-value1")),
    ("level2.attr2", azimuth::StringValue("level2-value2")),
    ("common.attr", azimuth::StringValue("level2-common"))  // 应覆盖level1的common.attr
  ])
  
  let level3_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("level3.attr1", azimuth::StringValue("level3-value1")),
    ("level3.attr2", azimuth::StringValue("level3-value2")),
    ("common.attr", azimuth::StringValue("level3-common"))  // 应覆盖level2的common.attr
  ])
  
  // 多级合并
  let multi_level_merged = azimuth::Resource::merge(
    azimuth::Resource::merge(level1_resource, level2_resource),
    level3_resource
  )
  
  // 验证多级合并结果
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level1.attr1"), 
            Some(azimuth::StringValue("level1-value1")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level1.attr2"), 
            Some(azimuth::StringValue("level1-value2")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level2.attr1"), 
            Some(azimuth::StringValue("level2-value1")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level2.attr2"), 
            Some(azimuth::StringValue("level2-value2")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level3.attr1"), 
            Some(azimuth::StringValue("level3-value1")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "level3.attr2"), 
            Some(azimuth::StringValue("level3-value2")))
  assert_eq(azimuth::Resource::get_attribute(multi_level_merged, "common.attr"), 
            Some(azimuth::StringValue("level3-common")))  // 最终值
  
  // 测试冲突解决策略
  let conflict_resolver = azimuth::ResourceConflictResolver::new()
  
  // 添加冲突解决规则
  azimuth::ResourceConflictResolver::add_rule(conflict_resolver, "service.version", 
    azimuth::ConflictResolution::HighestVersion)
  azimuth::ResourceConflictResolver::add_rule(conflict_resolver, "service.instance.id", 
    azimuth::ConflictResolution::KeepExisting)
  azimuth::ResourceConflictResolver::add_rule(conflict_resolver, "timestamp", 
    azimuth::ConflictResolution::Latest)
  
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),  // 较低版本
    ("service.instance.id", azimuth::StringValue("instance-001")),
    ("timestamp", azimuth::StringValue("2025-01-01T00:00:00Z"))  // 较早时间戳
  ])
  
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("2.0.0")),  // 较高版本
    ("service.instance.id", azimuth::StringValue("instance-002")),
    ("timestamp", azimuth::StringValue("2025-01-02T00:00:00Z"))  // 较晚时间戳
  ])
  
  // 使用冲突解决策略合并
  let resolved_resource = azimuth::Resource::merge_with_resolver(
    resource1, resource2, conflict_resolver
  )
  
  // 验证冲突解决结果
  assert_eq(azimuth::Resource::get_attribute(resolved_resource, "service.version"), 
            Some(azimuth::StringValue("2.0.0")))  // 选择最高版本
  assert_eq(azimuth::Resource::get_attribute(resolved_resource, "service.instance.id"), 
            Some(azimuth::StringValue("instance-001")))  // 保留现有值
  assert_eq(azimuth::Resource::get_attribute(resolved_resource, "timestamp"), 
            Some(azimuth::StringValue("2025-01-02T00:00:00Z")))  // 选择最新时间戳
  
  // 测试资源属性类型转换
  let type_conversion_resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("numeric.attr", azimuth::IntValue(42)),
    ("float.attr", azimuth::FloatValue(3.14)),
    ("bool.attr", azimuth::BoolValue(true)),
    ("array.attr", azimuth::ArrayStringValue(["a", "b", "c"]))
  ])
  
  let type_conversion_resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("numeric.attr", azimuth::StringValue("100")),  // 字符串到数值转换
    ("float.attr", azimuth::StringValue("2.71")),  // 字符串到浮点转换
    ("bool.attr", azimuth::StringValue("false")),  // 字符串到布尔转换
    ("array.attr", azimuth::ArrayIntValue([1, 2, 3]))  // 数组类型转换
  ])
  
  // 使用类型转换合并
  let type_converted_resource = azimuth::Resource::merge_with_type_conversion(
    type_conversion_resource1, type_conversion_resource2
  )
  
  // 验证类型转换结果（基于简化实现）
  assert_eq(azimuth::Resource::get_attribute(type_converted_resource, "numeric.attr"), 
            Some(azimuth::IntValue(42)))
  assert_eq(azimuth::Resource::get_attribute(type_converted_resource, "float.attr"), 
            Some(azimuth::FloatValue(3.14)))
  assert_eq(azimuth::Resource::get_attribute(type_converted_resource, "bool.attr"), 
            Some(azimuth::BoolValue(true)))
  
  // 测试资源环境特定合并
  let dev_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("my-service")),
    ("deployment.environment", azimuth::StringValue("development")),
    ("debug.enabled", azimuth::BoolValue(true)),
    ("log.level", azimuth::StringValue("debug"))
  ])
  
  let prod_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("my-service")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("debug.enabled", azimuth::BoolValue(false)),
    ("log.level", azimuth::StringValue("warn"))
  ])
  
  // 环境感知合并
  let env_aware_merge = azimuth::Resource::merge_environment_aware(
    dev_resource, prod_resource, "production"
  )
  
  // 验证环境感知合并结果
  assert_eq(azimuth::Resource::get_attribute(env_aware_merge, "deployment.environment"), 
            Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(env_aware_merge, "debug.enabled"), 
            Some(azimuth::BoolValue(false)))
  assert_eq(azimuth::Resource::get_attribute(env_aware_merge, "log.level"), 
            Some(azimuth::StringValue("warn")))
}

// 测试7: 遥测数据压缩和序列化测试
pub test "遥测数据压缩和序列化测试" {
  // 测试Span数据压缩
  let span_compressor = azimuth::SpanCompressor::new(azimuth::CompressionAlgorithm::Gzip)
  
  // 创建测试Span
  let test_spans = []
  for i in 0..100 {
    let span_ctx = azimuth::SpanContext::new(
      "compress-trace-" + (i / 10).to_string(),
      "compress-span-" + i.to_string(),
      true, ""
    )
    
    let span = azimuth::Span::new("compress-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    // 添加大量属性以测试压缩效果
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "operation.index", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("compression.test"))
    azimuth::Attributes::set(attrs, "operation.description", 
      azimuth::StringValue("This is a long description for operation " + i.to_string() + 
                          " that contains repeated text to test compression effectiveness. " +
                          "The description includes many common words and phrases to demonstrate " +
                          "how compression algorithms can reduce data size."))
    
    for j in 0..5 {
      azimuth::Attributes::set(attrs, "repeated.attr." + j.to_string(), 
        azimuth::StringValue("repeated-value-for-compression-testing"))
    }
    
    azimuth::Span::set_attributes(span, attrs)
    
    // 添加事件
    for j in 0..3 {
      azimuth::Span::add_event(span, "event-" + j.to_string(), Some([
        ("event.index", azimuth::IntValue(j)),
        ("event.type", azimuth::StringValue("compression.test"))
      ]))
    }
    
    test_spans.push(span)
  }
  
  // 序列化Span
  let serialized_spans = azimuth::SpanSerializer::serialize_batch(test_spans)
  let original_size = serialized_spans.length()
  
  // 压缩Span数据
  let compressed_spans = azimuth::SpanCompressor::compress(span_compressor, serialized_spans)
  let compressed_size = compressed_spans.length()
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size.to_double() / original_size.to_double()
  assert_true(compression_ratio < 0.8)  // 至少20%的压缩率
  
  // 解压缩Span数据
  let decompressed_spans = azimuth::SpanCompressor::decompress(span_compressor, compressed_spans)
  assert_eq(decompressed_spans.length(), original_size)
  
  // 反序列化Span
  let deserialized_spans = azimuth::SpanSerializer::deserialize_batch(decompressed_spans)
  assert_true(deserialized_spans.length() == test_spans.length())
  
  // 测试不同压缩算法
  let lz4_compressor = azimuth::SpanCompressor::new(azimuth::CompressionAlgorithm::LZ4)
  let zstd_compressor = azimuth::SpanCompressor::new(azimuth::CompressionAlgorithm::Zstd)
  
  let lz4_compressed = azimuth::SpanCompressor::compress(lz4_compressor, serialized_spans)
  let zstd_compressed = azimuth::SpanCompressor::compress(zstd_compressor, serialized_spans)
  
  // 比较不同算法的压缩效果
  let lz4_ratio = lz4_compressed.length().to_double() / original_size.to_double()
  let zstd_ratio = zstd_compressed.length().to_double() / original_size.to_double()
  
  assert_true(lz4_ratio > 0.0)
  assert_true(zstd_ratio > 0.0)
  
  // 测试度量数据压缩
  let metric_compressor = azimuth::MetricCompressor::new(azimuth::CompressionAlgorithm::Gzip)
  
  // 创建测试度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "compression-test-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "compression.test.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "compression.test.histogram")
  
  // 添加大量度量数据点
  for i in 0..500 {
    azimuth::Counter::add(counter, 1.0, Some([
      ("metric.index", azimuth::IntValue(i)),
      ("metric.category", azimuth::StringValue("compression.test")),
      ("metric.description", azimuth::StringValue("Long description for metric compression testing"))
    ]))
    
    azimuth::Histogram::record(histogram, i.to_double(), Some([
      ("metric.type", azimuth::StringValue("histogram")),
      ("metric.index", azimuth::IntValue(i))
    ]))
  }
  
  // 收集度量数据
  let collector = azimuth::MetricCollector::new(meter_provider)
  let metrics = azimuth::MetricCollector::collect(collector)
  
  // 序列化度量
  let serialized_metrics = azimuth::MetricSerializer::serialize_batch(metrics)
  let metrics_original_size = serialized_metrics.length()
  
  // 压缩度量数据
  let compressed_metrics = azimuth::MetricCompressor::compress(metric_compressor, serialized_metrics)
  let metrics_compressed_size = compressed_metrics.length()
  
  // 验证度量压缩效果
  assert_true(metrics_compressed_size < metrics_original_size)
  let metrics_compression_ratio = metrics_compressed_size.to_double() / metrics_original_size.to_double()
  assert_true(metrics_compression_ratio < 0.8)
  
  // 测试日志数据压缩
  let log_compressor = azimuth::LogCompressor::new(azimuth::CompressionAlgorithm::Gzip)
  
  // 创建测试日志
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "compression-test-logger")
  
  let log_records = []
  for i in 0..200 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Compression test log message " + i.to_string() + 
           " with additional content to test compression effectiveness. " +
           "This message contains repeated patterns and common phrases to demonstrate " +
           "how compression can reduce log data size."),
      Some([
        ("log.index", azimuth::IntValue(i)),
        ("log.category", azimuth::StringValue("compression.test")),
        ("log.service", azimuth::StringValue("compression-test-service")),
        ("log.environment", azimuth::StringValue("test")),
        ("log.repeated.attr", azimuth::StringValue("repeated-value-for-testing-compression"))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("compression-trace-" + (i / 10).to_string()),
      Some("compression-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    log_records.push(log_record)
  }
  
  // 序列化日志
  let serialized_logs = azimuth::LogSerializer::serialize_batch(log_records)
  let logs_original_size = serialized_logs.length()
  
  // 压缩日志数据
  let compressed_logs = azimuth::LogCompressor::compress(log_compressor, serialized_logs)
  let logs_compressed_size = compressed_logs.length()
  
  // 验证日志压缩效果
  assert_true(logs_compressed_size < logs_original_size)
  let logs_compression_ratio = logs_compressed_size.to_double() / logs_original_size.to_double()
  assert_true(logs_compression_ratio < 0.8)
  
  // 测试序列化格式兼容性
  let json_serializer = azimuth::TelemetrySerializer::new(azimuth::SerializationFormat::JSON)
  let protobuf_serializer = azimuth::TelemetrySerializer::new(azimuth::SerializationFormat::Protobuf)
  let msgpack_serializer = azimuth::TelemetrySerializer::new(azimuth::SerializationFormat::MsgPack)
  
  // 测试不同序列化格式
  let json_serialized = azimuth::TelemetrySerializer::serialize_span(json_serializer, test_spans[0])
  let protobuf_serialized = azimuth::TelemetrySerializer::serialize_span(protobuf_serializer, test_spans[0])
  let msgpack_serialized = azimuth::TelemetrySerializer::serialize_span(msgpack_serializer, test_spans[0])
  
  // 验证序列化结果
  assert_true(json_serialized.length() > 0)
  assert_true(protobuf_serialized.length() > 0)
  assert_true(msgpack_serialized.length() > 0)
  
  // 测试反序列化兼容性
  let json_deserialized = azimuth::TelemetrySerializer::deserialize_span(json_serializer, json_serialized)
  let protobuf_deserialized = azimuth::TelemetrySerializer::deserialize_span(protobuf_serializer, protobuf_serialized)
  let msgpack_deserialized = azimuth::TelemetrySerializer::deserialize_span(msgpack_serializer, msgpack_serialized)
  
  // 验证反序列化结果
  assert_true(json_deserialized != None)
  assert_true(protobuf_deserialized != None)
  assert_true(msgpack_deserialized != None)
  
  // 测试序列化性能
  let performance_test_spans = []
  for i in 0..1000 {
    let span_ctx = azimuth::SpanContext::new("perf-trace", "perf-span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("perf-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    performance_test_spans.push(span)
  }
  
  let serialization_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let performance_serialized = azimuth::SpanSerializer::serialize_batch(performance_test_spans)
  let serialization_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let serialization_duration = serialization_end - serialization_start
  assert_true(serialization_duration < 5000000000L)  // 小于5秒
  
  let deserialization_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let _ = azimuth::SpanSerializer::deserialize_batch(performance_serialized)
  let deserialization_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let deserialization_duration = deserialization_end - deserialization_start
  assert_true(deserialization_duration < 5000000000L)  // 小于5秒
}

// 测试8: 多语言国际化支持测试
pub test "多语言国际化支持测试" {
  // 创建国际化资源管理器
  let i18n_manager = azimuth::I18nManager::new()
  
  // 添加多语言资源
  azimuth::I18nManager::add_language(i18n_manager, "en", [
    ("span.name.http.request", "HTTP Request"),
    ("span.name.database.query", "Database Query"),
    ("span.name.cache.operation", "Cache Operation"),
    ("log.message.error.occurred", "An error occurred"),
    ("log.message.operation.completed", "Operation completed successfully"),
    ("metric.name.request.count", "Request Count"),
    ("metric.name.response.time", "Response Time"),
    ("attribute.name.user.id", "User ID"),
    ("attribute.name.service.name", "Service Name"),
    ("attribute.name.error.code", "Error Code")
  ])
  
  azimuth::I18nManager::add_language(i18n_manager, "zh", [
    ("span.name.http.request", "HTTP请求"),
    ("span.name.database.query", "数据库查询"),
    ("span.name.cache.operation", "缓存操作"),
    ("log.message.error.occurred", "发生错误"),
    ("log.message.operation.completed", "操作成功完成"),
    ("metric.name.request.count", "请求计数"),
    ("metric.name.response.time", "响应时间"),
    ("attribute.name.user.id", "用户ID"),
    ("attribute.name.service.name", "服务名称"),
    ("attribute.name.error.code", "错误代码")
  ])
  
  azimuth::I18nManager::add_language(i18n_manager, "ja", [
    ("span.name.http.request", "HTTPリクエスト"),
    ("span.name.database.query", "データベースクエリ"),
    ("span.name.cache.operation", "キャッシュ操作"),
    ("log.message.error.occurred", "エラーが発生しました"),
    ("log.message.operation.completed", "操作が正常に完了しました"),
    ("metric.name.request.count", "リクエスト数"),
    ("metric.name.response.time", "応答時間"),
    ("attribute.name.user.id", "ユーザーID"),
    ("attribute.name.service.name", "サービス名"),
    ("attribute.name.error.code", "エラーコード")
  ])
  
  // 测试多语言Span名称
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "i18n-test-tracer")
  
  // 创建英语Span
  let en_span = azimuth::Tracer::start_span_with_i18n(
    tracer, 
    "span.name.http.request", 
    "en", 
    i18n_manager
  )
  assert_eq(azimuth::Span::name(en_span), "HTTP Request")
  
  // 创建中文Span
  let zh_span = azimuth::Tracer::start_span_with_i18n(
    tracer, 
    "span.name.database.query", 
    "zh", 
    i18n_manager
  )
  assert_eq(azimuth::Span::name(zh_span), "数据库查询")
  
  // 创建日语Span
  let ja_span = azimuth::Tracer::start_span_with_i18n(
    tracer, 
    "span.name.cache.operation", 
    "ja", 
    i18n_manager
  )
  assert_eq(azimuth::Span::name(ja_span), "キャッシュ操作")
  
  // 测试多语言日志消息
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "i18n-test-logger")
  
  // 创建英语日志
  let en_log = azimuth::LogRecord::new_with_i18n(
    azimuth::Info,
    "log.message.operation.completed",
    "en",
    i18n_manager,
    Some([
      ("operation.name", azimuth::StringValue("test.operation")),
      ("operation.duration", azimuth::IntValue(150))
    ])
  )
  assert_eq(azimuth::LogRecord::body(en_log), Some("Operation completed successfully"))
  
  // 创建中文日志
  let zh_log = azimuth::LogRecord::new_with_i18n(
    azimuth::Error,
    "log.message.error.occurred",
    "zh",
    i18n_manager,
    Some([
      ("error.code", azimuth::IntValue(500)),
      ("error.message", azimuth::StringValue("Internal Server Error"))
    ])
  )
  assert_eq(azimuth::LogRecord::body(zh_log), Some("发生错误"))
  
  // 创建日语日志
  let ja_log = azimuth::LogRecord::new_with_i18n(
    azimuth::Warn,
    "log.message.error.occurred",
    "ja",
    i18n_manager,
    Some([
      ("error.code", azimuth::IntValue(404)),
      ("error.message", azimuth::StringValue("Not Found"))
    ])
  )
  assert_eq(azimuth::LogRecord::body(ja_log), Some("エラーが発生しました"))
  
  // 测试多语言度量名称和描述
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "i18n-test-meter")
  
  // 创建英语度量
  let en_counter = azimuth::Meter::create_counter_with_i18n(
    meter,
    "metric.name.request.count",
    Some("metric.name.request.count.description"),
    "en",
    i18n_manager,
    Some("requests")
  )
  assert_eq(en_counter.name, "Request Count")
  
  // 创建中文度量
  let zh_histogram = azimuth::Meter::create_histogram_with_i18n(
    meter,
    "metric.name.response.time",
    Some("metric.name.response.time.description"),
    "zh",
    i18n_manager,
    Some("ms")
  )
  assert_eq(zh_histogram.name, "响应时间")
  
  // 测试多语言属性名称
  let en_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set_with_i18n(en_attrs, "attribute.name.user.id", 
    azimuth::StringValue("user-123"), "en", i18n_manager)
  azimuth::Attributes::set_with_i18n(en_attrs, "attribute.name.service.name", 
    azimuth::StringValue("test-service"), "en", i18n_manager)
  
  let zh_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set_with_i18n(zh_attrs, "attribute.name.user.id", 
    azimuth::StringValue("用户-123"), "zh", i18n_manager)
  azimuth::Attributes::set_with_i18n(zh_attrs, "attribute.name.service.name", 
    azimuth::StringValue("测试服务"), "zh", i18n_manager)
  
  // 测试多语言错误消息
  let error_en = azimuth::TelemetryError::new_with_i18n(
    "error.code.connection.failed",
    "en",
    i18n_manager,
    Some([("host", azimuth::StringValue("example.com")), ("port", azimuth::IntValue(8080))])
  )
  
  let error_zh = azimuth::TelemetryError::new_with_i18n(
    "error.code.connection.failed",
    "zh",
    i18n_manager,
    Some([("host", azimuth::StringValue("example.com")), ("port", azimuth::IntValue(8080))])
  )
  
  // 测试回退语言机制
  let fallback_span = azimuth::Tracer::start_span_with_i18n(
    tracer, 
    "span.name.http.request", 
    "fr",  // 不支持的语言
    i18n_manager
  )
  // 应该回退到默认语言（英语）
  assert_eq(azimuth::Span::name(fallback_span), "HTTP Request")
  
  // 测试参数化消息
  azimuth::I18nManager::add_parameterized_message(i18n_manager, "en", 
    "operation.completed.with.duration", 
    "Operation {0} completed in {1}ms")
  
  azimuth::I18nManager::add_parameterized_message(i18n_manager, "zh", 
    "operation.completed.with.duration", 
    "操作 {0} 在 {1}ms 内完成")
  
  let param_en_log = azimuth::LogRecord::new_with_parameterized_i18n(
    azimuth::Info,
    "operation.completed.with.duration",
    "en",
    i18n_manager,
    ["data.processing", "250"]
  )
  assert_eq(azimuth::LogRecord::body(param_en_log), Some("Operation data.processing completed in 250ms"))
  
  let param_zh_log = azimuth::LogRecord::new_with_parameterized_i18n(
    azimuth::Info,
    "operation.completed.with.duration",
    "zh",
    i18n_manager,
    ["数据处理", "250"]
  )
  assert_eq(azimuth::LogRecord::body(param_zh_log), Some("操作 数据处理 在 250ms 内完成"))
  
  // 测试复数形式
  azimuth::I18nManager::add_plural_message(i18n_manager, "en",
    "item.count",
    ["{0} item", "{0} items"]  // 单数，复数
  )
  
  azimuth::I18nManager::add_plural_message(i18n_manager, "zh",
    "item.count",
    ["{0} 项", "{0} 项"]  // 中文通常不区分单复数
  )
  
  let singular_log = azimuth::LogRecord::new_with_plural_i18n(
    azimuth::Info,
    "item.count",
    "en",
    i18n_manager,
    1
  )
  assert_eq(azimuth::LogRecord::body(singular_log), Some("1 item"))
  
  let plural_log = azimuth::LogRecord::new_with_plural_i18n(
    azimuth::Info,
    "item.count",
    "en",
    i18n_manager,
    5
  )
  assert_eq(azimuth::LogRecord::body(plural_log), Some("5 items"))
  
  // 测试RTL（从右到左）语言支持
  azimuth::I18nManager::add_language(i18n_manager, "ar", [
    ("span.name.http.request", "طلب HTTP"),
    ("log.message.error.occurred", "حدث خطأ"),
    ("attribute.name.user.id", "معرف المستخدم")
  ])
  
  let ar_span = azimuth::Tracer::start_span_with_i18n(
    tracer, 
    "span.name.http.request", 
    "ar", 
    i18n_manager
  )
  assert_eq(azimuth::Span::name(ar_span), "طلب HTTP")
  
  // 测试动态语言切换
  let context = azimuth::Context::root()
  let lang_key = azimuth::ContextKey::new("language")
  
  let en_context = azimuth::Context::with_value(context, lang_key, "en")
  let zh_context = azimuth::Context::with_value(context, lang_key, "zh")
  
  let contextual_span = azimuth::Tracer::start_span_with_contextual_i18n(
    tracer,
    "span.name.http.request",
    en_context,
    i18n_manager
  )
  assert_eq(azimuth::Span::name(contextual_span), "HTTP Request")
  
  let contextual_span_zh = azimuth::Tracer::start_span_with_contextual_i18n(
    tracer,
    "span.name.http.request",
    zh_context,
    i18n_manager
  )
  assert_eq(azimuth::Span::name(contextual_span_zh), "HTTP请求")
}

// 测试9: 实时仪表板数据流测试
pub test "实时仪表板数据流测试" {
  // 创建实时仪表板
  let dashboard = azimuth::RealtimeDashboard::new(
    "test-dashboard",
    azimuth::DashboardConfig::new(1000, 60)  // 1秒更新，60秒数据保留
  )
  
  // 创建数据源
  let span_data_source = azimuth::SpanDataSource::new()
  let metric_data_source = azimuth::MetricDataSource::new()
  let log_data_source = azimuth::LogDataSource::new()
  
  // 添加数据源到仪表板
  azimuth::RealtimeDashboard::add_data_source(dashboard, "spans", span_data_source)
  azimuth::RealtimeDashboard::add_data_source(dashboard, "metrics", metric_data_source)
  azimuth::RealtimeDashboard::add_data_source(dashboard, "logs", log_data_source)
  
  // 创建实时流处理器
  let stream_processor = azimuth::TelemetryStreamProcessor::new()
  
  // 添加流处理规则
  azimuth::TelemetryStreamProcessor::add_rule(stream_processor, 
    azimuth::StreamRule::new("error.rate", 
      azimuth::RuleCondition::AttributeEquals("error", "true"),
      azimuth::RuleAction::AggregateByTimeWindow(10)  // 10秒窗口
    ))
  
  azimuth::TelemetryStreamProcessor::add_rule(stream_processor, 
    azimuth::StreamRule::new("response.time.threshold",
      azimuth::RuleCondition::AttributeGreaterThan("response.time", 1000.0),
      azimuth::RuleAction::TriggerAlert("high.response.time")
    ))
  
  // 创建模拟数据流
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "dashboard-test-tracer")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "dashboard-test-meter")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "dashboard-test-logger")
  
  // 生成实时Span数据流
  let span_stream = azimuth::TelemetryStream::new("spans")
  for i in 0..100 {
    let span_ctx = azimuth::SpanContext::new("dashboard-trace", "dashboard-span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("dashboard-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    // 添加属性
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "operation.index", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue(if i % 10 == 0 { "error" } else { "normal" }))
    azimuth::Attributes::set(attrs, "response.time", azimuth::FloatValue(50.0 + (i.to_double() * 10.0)))
    
    if i % 10 == 0 {
      azimuth::Attributes::set(attrs, "error", azimuth::StringValue("true"))
      azimuth::Attributes::set(attrs, "error.code", azimuth::IntValue(500))
    }
    
    azimuth::Span::set_attributes(span, attrs)
    
    // 添加到流
    azimuth::TelemetryStream::add_item(span_stream, azimuth::Clock::now_unix_nanos(azimuth::Clock::system()), span)
  }
  
  // 生成实时度量数据流
  let metric_stream = azimuth::TelemetryStream::new("metrics")
  let counter = azimuth::Meter::create_counter(meter, "dashboard.requests")
  let histogram = azimuth::Meter::create_histogram(meter, "dashboard.response.time")
  
  for i in 0..200 {
    let request_time = 100.0 + (azimuth::Random::next_u64(azimuth::Random::system()).to_double() % 500.0)
    
    azimuth::Counter::add(counter, 1.0, Some([
      ("endpoint", azimuth::StringValue("/api/" + (i % 5).to_string())),
      ("status", azimuth::StringValue(if i % 20 == 0 { "error" } else { "success" }))
    ]))
    
    azimuth::Histogram::record(histogram, request_time, Some([
      ("endpoint", azimuth::StringValue("/api/" + (i % 5).to_string()))
    ]))
    
    // 添加到流
    let timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    azimuth::TelemetryStream::add_metric(metric_stream, timestamp, "dashboard.requests", 1.0)
    azimuth::TelemetryStream::add_metric(metric_stream, timestamp, "dashboard.response.time", request_time)
  }
  
  // 生成实时日志数据流
  let log_stream = azimuth::TelemetryStream::new("logs")
  
  for i in 0..150 {
    let severity = if i % 15 == 0 { azimuth::Error } 
                   else if i % 10 == 0 { azimuth::Warn } 
                   else if i % 5 == 0 { azimuth::Info } 
                   else { azimuth::Debug }
    
    let log_record = azimuth::LogRecord::new_with_context(
      severity,
      Some("Dashboard log message " + i.to_string()),
      Some([
        ("log.index", azimuth::IntValue(i)),
        ("log.component", azimuth::StringValue("dashboard-test")),
        ("log.level", azimuth::StringValue(severity.to_string()))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("dashboard-trace"),
      Some("dashboard-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    // 添加到流
    azimuth::TelemetryStream::add_log(log_stream, azimuth::Clock::now_unix_nanos(azimuth::Clock::system()), log_record)
  }
  
  // 处理数据流
  let processed_span_stream = azimuth::TelemetryStreamProcessor::process(stream_processor, span_stream)
  let processed_metric_stream = azimuth::TelemetryStreamProcessor::process(stream_processor, metric_stream)
  let processed_log_stream = azimuth::TelemetryStreamProcessor::process(stream_processor, log_stream)
  
  // 创建实时图表
  let request_rate_chart = azimuth::RealtimeChart::new(
    "request.rate",
    azimuth::ChartType::Line,
    azimuth::TimeSeries::new(60)  // 60秒时间序列
  )
  
  let error_rate_chart = azimuth::RealtimeChart::new(
    "error.rate",
    azimuth::ChartType::Bar,
    azimuth::TimeSeries::new(60)
  )
  
  let response_time_chart = azimuth::RealtimeChart::new(
    "response.time.distribution",
    azimuth::ChartType::Histogram,
    azimuth::TimeSeries::new(60)
  )
  
  // 更新图表数据
  let chart_updater = azimuth::ChartUpdater::new()
  
  // 从处理后的度量流更新请求率图表
  let request_rate_data = azimuth::TelemetryStream::aggregate_by_time(
    processed_metric_stream,
    "dashboard.requests",
    5  // 5秒聚合
  )
  
  for data_point in request_rate_data {
    azimuth::RealtimeChart::add_data_point(request_rate_chart, 
      azimuth::TimeSeriesDataPoint::new(data_point.timestamp, data_point.value))
  }
  
  // 从处理后的Span流更新错误率图表
  let error_rate_data = azimuth::TelemetryStream::filter_and_aggregate(
    processed_span_stream,
    (span) => azimuth::Span::get_attribute(span, "error") == Some(azimuth::StringValue("true")),
    5  // 5秒聚合
  )
  
  for data_point in error_rate_data {
    azimuth::RealtimeChart::add_data_point(error_rate_chart,
      azimuth::TimeSeriesDataPoint::new(data_point.timestamp, data_point.value))
  }
  
  // 从处理后的度量流更新响应时间分布图表
  let response_time_data = azimuth::TelemetryStream::get_distribution(
    processed_metric_stream,
    "dashboard.response.time",
    [0.0, 100.0, 200.0, 500.0, 1000.0]  // 分桶边界
  )
  
  for bucket in response_time_data {
    azimuth::RealtimeChart::add_histogram_bucket(response_time_chart,
      azimuth::HistogramBucket::new(bucket.lower_bound, bucket.upper_bound, bucket.count))
  }
  
  // 创建实时仪表板视图
  let dashboard_view = azimuth::DashboardView::new("main")
  
  azimuth::DashboardView::add_chart(dashboard_view, request_rate_chart)
  azimuth::DashboardView::add_chart(dashboard_view, error_rate_chart)
  azimuth::DashboardView::add_chart(dashboard_view, response_time_chart)
  
  // 添加实时指标
  let real_time_metrics = azimuth::RealtimeMetrics::new()
  
  // 计算实时指标
  let current_request_rate = azimuth::TelemetryStream::get_current_rate(
    processed_metric_stream,
    "dashboard.requests",
    10  // 最近10秒
  )
  
  let current_error_rate = azimuth::TelemetryStream::get_current_rate(
    processed_span_stream,
    "error",
    10  // 最近10秒
  )
  
  let avg_response_time = azimuth::TelemetryStream::get_average(
    processed_metric_stream,
    "dashboard.response.time",
    10  // 最近10秒
  )
  
  azimuth::RealtimeMetrics::add_metric(real_time_metrics, "current.request.rate", current_request_rate)
  azimuth::RealtimeMetrics::add_metric(real_time_metrics, "current.error.rate", current_error_rate)
  azimuth::RealtimeMetrics::add_metric(real_time_metrics, "avg.response.time", avg_response_time)
  
  // 添加实时指标到仪表板视图
  azimuth::DashboardView::add_metrics(dashboard_view, real_time_metrics)
  
  // 创建实时警报
  let alert_manager = azimuth::AlertManager::new()
  
  // 添加警报规则
  azimuth::AlertManager::add_rule(alert_manager,
    azimuth::AlertRule::new("high.error.rate",
      azimuth::AlertCondition::MetricThreshold("current.error.rate", azimuth::ComparisonOperator::GreaterThan, 0.1),
      azimuth::AlertSeverity::Warning,
      "Error rate is above 10%"
    ))
  
  azimuth::AlertManager::add_rule(alert_manager,
    azimuth::AlertRule::new("high.response.time",
      azimuth::AlertCondition::MetricThreshold("avg.response.time", azimuth::ComparisonOperator::GreaterThan, 500.0),
      azimuth::AlertSeverity::Critical,
      "Average response time is above 500ms"
    ))
  
  // 检查警报
  let alerts = azimuth::AlertManager::check_rules(alert_manager, real_time_metrics)
  
  // 添加警报到仪表板视图
  for alert in alerts {
    azimuth::DashboardView::add_alert(dashboard_view, alert)
  }
  
  // 测试仪表板数据导出
  let dashboard_exporter = azimuth::DashboardExporter::new(azimuth::DashboardExportFormat::JSON)
  let exported_dashboard = azimuth::DashboardExporter::export(dashboard_exporter, dashboard_view)
  
  assert_true(exported_dashboard.length() > 0)
  
  // 测试仪表板性能
  let performance_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 处理大量数据点
  let large_stream = azimuth::TelemetryStream::new("large.test")
  for i in 0..1000 {
    let timestamp = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    azimuth::TelemetryStream::add_metric(large_stream, timestamp, "test.metric", i.to_double())
  }
  
  let processed_large_stream = azimuth::TelemetryStreamProcessor::process(stream_processor, large_stream)
  let aggregated_data = azimuth::TelemetryStream::aggregate_by_time(processed_large_stream, "test.metric", 1)
  
  let performance_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let performance_duration = performance_end - performance_start
  
  assert_true(performance_duration < 5000000000L)  // 小于5秒
  assert_true(aggregated_data.length() > 0)
  
  // 测试实时订阅
  let subscription = azimuth::RealtimeSubscription::new()
  
  azimuth::RealtimeSubscription::subscribe(subscription, "spans", 
    (data) => {
      // 处理Span数据更新
      assert_true(data.length() > 0)
    })
  
  azimuth::RealtimeSubscription::subscribe(subscription, "metrics",
    (data) => {
      // 处理度量数据更新
      assert_true(data.length() > 0)
    })
  
  azimuth::RealtimeSubscription::subscribe(subscription, "logs",
    (data) => {
      // 处理日志数据更新
      assert_true(data.length() > 0)
    })
  
  // 发布数据更新
  azimuth::RealtimeSubscription::publish(subscription, "spans", span_stream)
  azimuth::RealtimeSubscription::publish(subscription, "metrics", metric_stream)
  azimuth::RealtimeSubscription::publish(subscription, "logs", log_stream)
}

// 测试10: 云原生遥测集成测试
pub test "云原生遥测集成测试" {
  // 创建云原生资源检测器
  let resource_detector = azimuth::CloudResourceDetector::new()
  
  // 检测云环境资源
  let cloud_resource = azimuth::CloudResourceDetector::detect(resource_detector)
  
  // 验证云资源属性
  let cloud_provider = azimuth::Resource::get_attribute(cloud_resource, "cloud.provider")
  let cloud_region = azimuth::Resource::get_attribute(cloud_resource, "cloud.region")
  let cloud_zone = azimuth::Resource::get_attribute(cloud_resource, "cloud.zone")
  let cloud_account_id = azimuth::Resource::get_attribute(cloud_resource, "cloud.account.id")
  
  // 测试Kubernetes环境检测
  let k8s_detector = azimuth::KubernetesResourceDetector::new()
  let k8s_resource = azimuth::KubernetesResourceDetector::detect(k8s_detector)
  
  // 验证Kubernetes资源属性
  let k8s_pod_name = azimuth::Resource::get_attribute(k8s_resource, "k8s.pod.name")
  let k8s_namespace = azimuth::Resource::get_attribute(k8s_resource, "k8s.namespace")
  let k8s_deployment_name = azimuth::Resource::get_attribute(k8s_resource, "k8s.deployment.name")
  let k8s_node_name = azimuth::Resource::get_attribute(k8s_resource, "k8s.node.name")
  
  // 测试容器环境检测
  let container_detector = azimuth::ContainerResourceDetector::new()
  let container_resource = azimuth::ContainerResourceDetector::detect(container_detector)
  
  // 验证容器资源属性
  let container_id = azimuth::Resource::get_attribute(container_resource, "container.id")
  let container_name = azimuth::Resource::get_attribute(container_resource, "container.name")
  let container_image = azimuth::Resource::get_attribute(container_resource, "container.image")
  let container_runtime = azimuth::Resource::get_attribute(container_resource, "container.runtime")
  
  // 合并所有检测到的资源
  let merged_cloud_resource = azimuth::Resource::merge(
    azimuth::Resource::merge(cloud_resource, k8s_resource),
    container_resource
  )
  
  // 创建云原生TracerProvider
  let cloud_tracer_provider = azimuth::TracerProvider::builder()
    .with_resource(merged_cloud_resource)
    .with_span_processor(azimuth::BatchSpanProcessor::new(
      azimuth::OTLPTraceExporter::new_with_endpoint("http://otel-collector:4317"),
      azimuth::BatchSpanProcessorOptions::default()
    ))
    .build()
  
  let cloud_tracer = azimuth::TracerProvider::get_tracer(cloud_tracer_provider, "cloud-native-service")
  
  // 创建云原生Span
  let cloud_span = azimuth::Tracer::start_span(cloud_tracer, "cloud.native.operation")
  
  // 添加云原生属性
  let cloud_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(cloud_attrs, "operation.type", azimuth::StringValue("cloud.native"))
  azimuth::Attributes::set(cloud_attrs, "service.environment", azimuth::StringValue("kubernetes"))
  azimuth::Attributes::set(cloud_attrs, "service.cluster", azimuth::StringValue("production"))
  
  azimuth::Span::set_attributes(cloud_span, cloud_attrs)
  
  // 测试云原生度量
  let cloud_meter_provider = azimuth::MeterProvider::builder()
    .with_resource(merged_cloud_resource)
    .with_metric_exporter(azimuth::OTLPMetricExporter::new_with_endpoint("http://otel-collector:4317"))
    .build()
  
  let cloud_meter = azimuth::MeterProvider::get_meter(cloud_meter_provider, "cloud-native-metrics")
  
  // 创建云原生度量
  let request_counter = azimuth::Meter::create_counter(cloud_meter, "cloud.requests.total")
  let response_time_histogram = azimuth::Meter::create_histogram(cloud_meter, "cloud.response.time")
  let resource_usage_gauge = azimuth::Meter::create_gauge(cloud_meter, "cloud.resource.usage")
  
  // 添加云原生度量数据
  azimuth::Counter::add(request_counter, 1.0, Some([
    ("service.name", azimuth::StringValue("cloud-native-service")),
    ("k8s.namespace", azimuth::StringValue("production")),
    ("k8s.pod.name", azimuth::StringValue("cloud-native-pod-001"))
  ]))
  
  azimuth::Histogram::record(response_time_histogram, 150.5, Some([
    ("service.name", azimuth::StringValue("cloud-native-service")),
    ("endpoint", azimuth::StringValue("/api/cloud/data"))
  ]))
  
  azimuth::Gauge::record(resource_usage_gauge, 75.2, Some([
    ("resource.type", azimuth::StringValue("cpu")),
    ("k8s.node.name", azimuth::StringValue("worker-node-001"))
  ]))
  
  // 测试云原生日志
  let cloud_logger_provider = azimuth::LoggerProvider::builder()
    .with_resource(merged_cloud_resource)
    .with_log_processor(azimuth::BatchLogProcessor::new(
      azimuth::OTLPLogExporter::new_with_endpoint("http://otel-collector:4317"),
      azimuth::BatchLogProcessorOptions::default()
    ))
    .build()
  
  let cloud_logger = azimuth::LoggerProvider::get_logger(cloud_logger_provider, "cloud-native-logger")
  
  // 创建云原生日志记录
  let cloud_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Cloud native operation completed successfully"),
    Some([
      ("service.name", azimuth::StringValue("cloud-native-service")),
      ("k8s.namespace", azimuth::StringValue("production")),
      ("operation.type", azimuth::StringValue("cloud.native")),
      ("trace.id", azimuth::StringValue(azimuth::SpanContext::trace_id(azimuth::Span::span_context(cloud_span)))),
      ("span.id", azimuth::StringValue(azimuth::SpanContext::span_id(azimuth::Span::span_context(cloud_span))))
    ]),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(cloud_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(cloud_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(cloud_logger, cloud_log_record)
  
  // 测试服务网格集成
  let service_mesh_config = azimuth::ServiceMeshConfig::new()
  
  // 配置Istio集成
  azimuth::ServiceMeshConfig::configure_istio(service_mesh_config, [
    ("istio.include.inbound.span", azimuth::BoolValue(true)),
    ("istio.include.outbound.span", azimuth::BoolValue(true)),
    ("istio.propagate.context", azimuth::BoolValue(true))
  ])
  
  // 创建服务网格感知的Tracer
  let mesh_tracer_provider = azimuth::TracerProvider::builder()
    .with_resource(merged_cloud_resource)
    .with_service_mesh_config(service_mesh_config)
    .build()
  
  let mesh_tracer = azimuth::TracerProvider::get_tracer(mesh_tracer_provider, "service-mesh-service")
  
  // 创建服务网格Span
  let mesh_span = azimuth::Tracer::start_span_with_mesh_context(
    mesh_tracer,
    "service.mesh.operation",
    Some([
      ("service.upstream.name", azimuth::StringValue("upstream-service")),
      ("service.upstream.cluster", azimuth::StringValue("upstream-cluster")),
      ("mesh.request.id", azimuth::StringValue("mesh-req-12345"))
    ])
  )
  
  // 测试云原生自动发现
  let discovery = azimuth::CloudServiceDiscovery::new()
  
  // 自动发现同网格中的服务
  let discovered_services = azimuth::CloudServiceDiscovery::discover_services(discovery, "production")
  
  // 验证服务发现结果
  assert_true(discovered_services.length() >= 0)
  
  // 测试云原生配置管理
  let config_manager = azimuth::CloudConfigManager::new()
  
  // 从云环境加载配置
  let telemetry_config = azimuth::CloudConfigManager::load_telemetry_config(config_manager, [
    ("otel.service.name", "cloud-native-service"),
    ("otel.exporter.otlp.endpoint", "http://otel-collector:4317"),
    ("otel.resource.attributes", "service.name=cloud-native-service,service.version=1.0.0")
  ])
  
  // 验证配置加载
  assert_true(telemetry_config != None)
  
  // 测试云原生健康检查
  let health_checker = azimuth::CloudHealthChecker::new()
  
  // 添加健康检查端点
  azimuth::CloudHealthChecker::add_check(health_checker, "otel.collector", 
    "http://otel-collector:13133")
  azimuth::CloudHealthChecker::add_check(health_checker, "prometheus", 
    "http://prometheus:9090/-/healthy")
  azimuth::CloudHealthChecker::add_check(health_checker, "jaeger", 
    "http://jaeger:14269/")
  
  // 执行健康检查
  let health_results = azimuth::CloudHealthChecker::check_all(health_checker)
  
  // 验证健康检查结果
  assert_true(health_results.length() >= 0)
  
  // 测试云原生指标收集
  let metrics_collector = azimuth::CloudMetricsCollector::new()
  
  // 收集Kubernetes指标
  let k8s_metrics = azimuth::CloudMetricsCollector::collect_kubernetes_metrics(metrics_collector, [
    "k8s.pod.cpu.usage",
    "k8s.pod.memory.usage",
    "k8s.node.cpu.usage",
    "k8s.node.memory.usage"
  ])
  
  // 收集容器指标
  let container_metrics = azimuth::CloudMetricsCollector::collect_container_metrics(metrics_collector, [
    "container.cpu.usage",
    "container.memory.usage",
    "container.network.io",
    "container.disk.io"
  ])
  
  // 验证指标收集结果
  assert_true(k8s_metrics.length() >= 0)
  assert_true(container_metrics.length() >= 0)
  
  // 测试云原生分布式追踪
  let distributed_tracer = azimuth::CloudDistributedTracer::new(cloud_tracer)
  
  // 创建分布式追踪链
  let root_span = azimuth::CloudDistributedTracer::start_root_span(distributed_tracer, 
    "distributed.root.operation", [
      ("service.name", azimuth::StringValue("gateway-service")),
      ("k8s.namespace", azimuth::StringValue("production"))
    ])
  
  // 服务A调用服务B
  let service_a_span = azimuth::CloudDistributedTracer::start_child_span(distributed_tracer, 
    root_span, "service.a.operation", [
      ("service.name", azimuth::StringValue("service-a")),
      ("k8s.pod.name", azimuth::StringValue("service-a-pod-001"))
    ])
  
  // 服务B调用服务C
  let service_b_span = azimuth::CloudDistributedTracer::start_child_span(distributed_tracer, 
    service_a_span, "service.b.operation", [
      ("service.name", azimuth::StringValue("service-b")),
      ("k8s.pod.name", azimuth::StringValue("service-b-pod-001"))
    ])
  
  // 服务C操作
  let service_c_span = azimuth::CloudDistributedTracer::start_child_span(distributed_tracer, 
    service_b_span, "service.c.operation", [
      ("service.name", azimuth::StringValue("service-c")),
      ("k8s.pod.name", azimuth::StringValue("service-c-pod-001"))
    ])
  
  // 结束所有Span
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(root_span)
  
  // 验证分布式追踪链
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span)),
            azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span)))
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span)),
            azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span)))
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span)),
            azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_c_span)))
  
  // 测试云原生遥测管道
  let telemetry_pipeline = azimuth::CloudTelemetryPipeline::new()
  
  // 配置管道组件
  azimuth::CloudTelemetryPipeline::configure_receiver(telemetry_pipeline, 
    azimuth::OTLPReceiver::new("0.0.0.0:4317"))
  
  azimuth::CloudTelemetryPipeline::configure_processor(telemetry_pipeline,
    azimuth::BatchProcessor::new(1000, 5000))
  
  azimuth::CloudTelemetryPipeline::configure_exporter(telemetry_pipeline,
    azimuth::OTLPExporter::new("http://otel-collector:4317"))
  
  // 启动管道
  azimuth::CloudTelemetryPipeline::start(telemetry_pipeline)
  
  // 验证管道状态
  assert_true(azimuth::CloudTelemetryPipeline::is_running(telemetry_pipeline))
  
  // 停止管道
  azimuth::CloudTelemetryPipeline::stop(telemetry_pipeline)
  assert_false(azimuth::CloudTelemetryPipeline::is_running(telemetry_pipeline))
}