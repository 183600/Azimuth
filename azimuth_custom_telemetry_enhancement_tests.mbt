// Azimuth 自定义遥测增强测试用例
// 专注于遥测系统的高级功能和增强特性

// 测试1: 自定义指标聚合器
test "自定义指标聚合器测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "custom.aggregator")
  
  // 创建自定义聚合器
  let aggregator = CustomAggregator::new("percentile", [50.0, 90.0, 95.0, 99.0])
  
  // 创建指标并记录数据
  let response_time = Meter::create_histogram_with_aggregator(meter, "http.response.time", aggregator)
  
  // 记录不同响应时间
  Histogram::record(response_time, 0.050)
  Histogram::record(response_time, 0.075)
  Histogram::record(response_time, 0.100)
  Histogram::record(response_time, 0.200)
  Histogram::record(response_time, 0.500)
  
  // 获取聚合结果
  let percentiles = CustomAggregator::get_percentiles(aggregator)
  let p50 = percentiles[0]
  let p99 = percentiles[3]
  
  // 验证聚合结果
  assert_true(p50 > 0.050 && p50 < 0.100)
  assert_true(p99 > 0.200 && p99 <= 0.500)
}

// 测试2: 分布式追踪链路分析
test "分布式追踪链路分析测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "chain.analyzer")
  
  // 创建追踪链路
  let api_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::set_attribute(api_span, "http.method", StringValue("POST"))
  Span::set_attribute(api_span, "api.version", StringValue("v2"))
  
  let auth_span = Tracer::start_span_with_parent(tracer, "auth.service", api_span)
  Span::set_attribute(auth_span, "auth.method", StringValue("jwt"))
  Span::set_attribute(auth_span, "auth.duration", IntValue(25))
  Span::end(auth_span)
  
  let service_span = Tracer::start_span_with_parent(tracer, "business.logic", api_span)
  Span::set_attribute(service_span, "service.name", StringValue("payment.processor"))
  Span::set_attribute(service_span, "operation.type", StringValue("transaction"))
  
  let db_span = Tracer::start_span_with_parent(tracer, "database.operation", service_span)
  Span::set_attribute(db_span, "db.query", StringValue("INSERT INTO transactions"))
  Span::set_attribute(db_span, "db.duration", IntValue(120))
  Span::end(db_span)
  
  Span::end(service_span)
  Span::end(api_span)
  
  // 分析链路
  let chain_analyzer = TraceChainAnalyzer::new()
  let analysis = TraceChainAnalyzer::analyze(chain_analyzer, api_span)
  
  // 验证链路分析结果
  assert_eq(TraceChainAnalysis::span_count(analysis), 4)
  assert_eq(TraceChainAnalysis::total_duration(analysis), 145)
  assert_true(TraceChainAnalysis::critical_path_contains(analysis, "database.operation"))
}

// 测试3: 智能日志级别过滤
test "智能日志级别过滤测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "smart.filter")
  
  // 配置智能过滤器
  let filter = SmartLogFilter::new()
  SmartLogFilter::set_level(filter, Warn)
  SmartLogFilter::add_pattern(filter, "security.*", Error)
  SmartLogFilter::add_pattern(filter, "performance.*", Info)
  SmartLogFilter::add_exception_pattern(filter, "database.connection", Debug)
  
  // 创建不同级别的日志
  let debug_log = Logger::create_log_record(logger)
  LogRecord::set_severity(debug_log, Debug)
  LogRecord::set_body(debug_log, "Debug information")
  LogRecord::set_attribute(debug_log, "log.category", StringValue("general"))
  
  let security_log = Logger::create_log_record(logger)
  LogRecord::set_severity(security_log, Warn)
  LogRecord::set_body(security_log, "Security warning")
  LogRecord::set_attribute(security_log, "log.category", StringValue("security.breach"))
  
  let perf_log = Logger::create_log_record(logger)
  LogRecord::set_severity(perf_log, Info)
  LogRecord::set_body(perf_log, "Performance metrics")
  LogRecord::set_attribute(perf_log, "log.category", StringValue("performance.metrics"))
  
  let db_log = Logger::create_log_record(logger)
  LogRecord::set_severity(db_log, Debug)
  LogRecord::set_body(db_log, "Database connection established")
  LogRecord::set_attribute(db_log, "log.category", StringValue("database.connection"))
  
  // 应用过滤器
  let filtered_logs = SmartLogFilter::apply(filter, [debug_log, security_log, perf_log, db_log])
  
  // 验证过滤结果
  assert_eq(filtered_logs.length(), 3) // debug_log被过滤，security_log提升到Error，perf_log和db_log通过
}

// 测试4: 资源使用监控和阈值告警
test "资源使用监控和阈值告警测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource.monitor")
  
  // 创建资源监控指标
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage")
  let disk_io = Meter::create_counter(meter, "system.disk.io.operations")
  let network_bytes = Meter::create_counter(meter, "system.network.bytes")
  
  // 设置告警阈值
  let alert_manager = AlertManager::new()
  AlertManager::set_threshold(alert_manager, "system.cpu.usage", 80.0, GreaterThan)
  AlertManager::set_threshold(alert_manager, "system.memory.usage", 90.0, GreaterThan)
  AlertManager::set_rate_threshold(alert_manager, "system.disk.io.operations", 1000.0, PerSecond)
  
  // 模拟资源使用情况
  Gauge::set(cpu_usage, 85.5)  // 超过阈值
  Gauge::set(memory_usage, 75.2)  // 未超过阈值
  Counter::add(disk_io, 1200.0)  // 超过速率阈值
  Counter::add(network_bytes, 50000.0)
  
  // 检查告警
  let alerts = AlertManager::check_thresholds(alert_manager)
  
  // 验证告警结果
  assert_eq(alerts.length(), 2)  // CPU和磁盘IO告警
  assert_true(Alert::contains_metric(alerts[0], "system.cpu.usage"))
  assert_true(Alert::contains_metric(alerts[1], "system.disk.io.operations"))
}

// 测试5: 采样策略和自适应采样
test "采样策略和自适应采样测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "adaptive.sampler")
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  AdaptiveSampler::set_base_sampling_rate(adaptive_sampler, 0.1)  // 基础采样率10%
  AdaptiveSampler::set_error_sampling_rate(adaptive_sampler, 1.0)  // 错误采样率100%
  AdaptiveSampler::set_latency_threshold(adaptive_sampler, 1.0)  // 延迟阈值1秒
  
  // 创建正常请求span
  let normal_span = Tracer::start_span_with_sampler(tracer, "normal.request", adaptive_sampler)
  Span::set_attribute(normal_span, "request.duration", FloatValue(0.5))
  Span::set_status(normal_span, Ok)
  Span::end(normal_span)
  
  // 创建错误请求span
  let error_span = Tracer::start_span_with_sampler(tracer, "error.request", adaptive_sampler)
  Span::set_attribute(error_span, "request.duration", FloatValue(0.3))
  Span::set_status(error_span, Error)
  Span::end(error_span)
  
  // 创建慢请求span
  let slow_span = Tracer::start_span_with_sampler(tracer, "slow.request", adaptive_sampler)
  Span::set_attribute(slow_span, "request.duration", FloatValue(1.5))
  Span::set_status(slow_span, Ok)
  Span::end(slow_span)
  
  // 获取采样统计
  let stats = AdaptiveSampler::get_sampling_stats(adaptive_sampler)
  
  // 验证采样结果
  assert_eq(SamplingStats::total_spans(stats), 3)
  assert_eq(SamplingStats::sampled_spans(stats), 2)  // 错误和慢请求被采样
  assert_true(SamplingStats::is_sampled(stats, error_span))
  assert_true(SamplingStats::is_sampled(stats, slow_span))
}

// 测试6: 批量数据处理和压缩
test "批量数据处理和压缩测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch.processor")
  
  // 创建批量处理器
  let batch_processor = BatchProcessor::new()
  BatchProcessor::set_batch_size(batch_processor, 100)
  BatchProcessor::set_flush_interval(batch_processor, 5000)  // 5秒
  BatchProcessor::enable_compression(batch_processor, Gzip)
  
  // 创建指标
  let request_counter = Meter::create_counter(meter, "batch.requests")
  let latency_histogram = Meter::create_histogram(meter, "batch.latency")
  
  // 批量记录数据
  for i in 0..=150 {
    Counter::add(request_counter, 1.0)
    Histogram::record(latency, 0.001 + (i as Float) * 0.00001)
  }
  
  // 触发批量处理
  let batch_result = BatchProcessor::flush(batch_processor)
  
  // 验证批量处理结果
  assert_eq(BatchResult::processed_items(batch_result), 151)
  assert_true(BatchResult::compression_ratio(batch_result) > 1.0)
  assert_eq(BatchResult::batch_count(batch_result), 2)  // 分成两个批次
}

// 测试7: 时间窗口统计和趋势分析
test "时间窗口统计和趋势分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time.window.analyzer")
  
  // 创建时间窗口分析器
  let window_analyzer = TimeWindowAnalyzer::new()
  TimeWindowAnalyzer::add_window(window_analyzer, "1m", 60)  // 1分钟窗口
  TimeWindowAnalyzer::add_window(window_analyzer, "5m", 300)  // 5分钟窗口
  TimeWindowAnalyzer::add_window(window_analyzer, "1h", 3600)  // 1小时窗口
  
  // 创建指标
  let throughput = Meter::create_counter(meter, "requests.throughput")
  let error_rate = Meter::create_counter(meter, "errors.count")
  
  // 模拟时间序列数据
  let current_time = Time::now()
  for i in 0..=120 {
    let timestamp = current_time - (i * 60)  // 每分钟一个数据点
    Counter::add(throughput, 100.0 + (i % 20) as Float)
    
    if i % 10 == 0 {
      Counter::add(error_rate, 1.0)
    }
    
    TimeWindowAnalyzer::add_data_point(window_analyzer, timestamp, throughput, error_rate)
  }
  
  // 获取趋势分析
  let trend_1m = TimeWindowAnalyzer::get_trend(window_analyzer, "1m")
  let trend_5m = TimeWindowAnalyzer::get_trend(window_analyzer, "5m")
  
  // 验证趋势分析结果
  assert_true(TrendAnalysis::slope(trend_1m) != 0.0)
  assert_true(TrendAnalysis::volatility(trend_5m) >= 0.0)
  assert_eq(TrendAnalysis::data_points(trend_1m), 60)
}

// 测试8: 异常追踪和根因分析
test "异常追踪和根因分析测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "exception.tracker")
  
  // 创建异常追踪器
  let exception_tracker = ExceptionTracker::new()
  ExceptionTracker::enable_stack_trace(exception_tracker, true)
  ExceptionTracker::enable_causal_chain(exception_tracker, true)
  
  // 创建异常场景
  let root_span = Tracer::start_span(tracer, "api.request")
  
  let service_span = Tracer::start_span_with_parent(tracer, "service.call", root_span)
  let db_span = Tracer::start_span_with_parent(tracer, "database.query", service_span)
  
  // 记录异常
  Span::record_exception(db_span, "ConnectionTimeoutException", "Database connection timed out")
  Span::set_status(db_span, Error)
  Span::end(db_span)
  
  // 记录重试异常
  let retry_span = Tracer::start_span_with_parent(tracer, "database.retry", service_span)
  Span::record_exception(retry_span, "DeadlockException", "Database deadlock detected")
  Span::set_status(retry_span, Error)
  Span::end(retry_span)
  
  Span::end(service_span)
  Span::end(root_span)
  
  // 进行根因分析
  let root_cause = ExceptionTracker::analyze_root_cause(exception_tracker, root_span)
  
  // 验证根因分析结果
  assert_eq(RootCauseAnalysis::primary_exception(root_cause), "ConnectionTimeoutException")
  assert_eq(RootCauseAnalysis::exception_chain_length(root_cause), 2)
  assert_true(RootCauseAnalysis::contains_component(root_cause, "database"))
}

// 测试9: 性能基准测试和回归检测
test "性能基准测试和回归检测测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance.benchmark")
  
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new()
  PerformanceBenchmark::set_baseline(benchmark, "api.v1.endpoint", 0.100)  // 100ms基线
  PerformanceBenchmark::set_baseline(benchmark, "api.v2.endpoint", 0.050)  // 50ms基线
  PerformanceBenchmark::set_regression_threshold(benchmark, 0.2)  // 20%回归阈值
  
  // 创建性能指标
  let v1_latency = Meter::create_histogram(meter, "api.v1.latency")
  let v2_latency = Meter::create_histogram(meter, "api.v2.latency")
  
  // 模拟性能测试数据
  for i in 0..=100 {
    // v1 API性能略有下降
    Histogram::record(v1_latency, 0.110 + (i % 10) as Float * 0.001)
    
    // v2 API性能保持稳定
    Histogram::record(v2_latency, 0.045 + (i % 5) as Float * 0.001)
  }
  
  // 进行基准测试分析
  let v1_analysis = PerformanceBenchmark::analyze(benchmark, "api.v1.endpoint", v1_latency)
  let v2_analysis = PerformanceBenchmark::analyze(benchmark, "api.v2.endpoint", v2_latency)
  
  // 验证基准测试结果
  assert_true(BenchmarkAnalysis::has_regression(v1_analysis))  // v1有性能回归
  assert_false(BenchmarkAnalysis::has_regression(v2_analysis))  // v2无性能回归
  assert_true(BenchmarkAnalysis::performance_change(v1_analysis) > 0.2)
}

// 测试10: 多格式数据导出和转换
test "多格式数据导出和转换测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.exporter")
  
  // 创建测试span
  let test_span = Tracer::start_span(tracer, "export.test")
  Span::set_attribute(test_span, "service.name", StringValue("test.service"))
  Span::set_attribute(test_span, "operation.name", StringValue("test.operation"))
  Span::set_attribute(test_span, "duration", IntValue(150))
  Span::set_status(test_span, Ok)
  Span::end(test_span)
  
  // 创建多格式导出器
  let exporter = MultiFormatExporter::new()
  
  // 导出为JSON格式
  let json_export = MultiFormatExporter::export_as(exporter, test_span, "json")
  
  // 导出为Protobuf格式
  let protobuf_export = MultiFormatExporter::export_as(exporter, test_span, "protobuf")
  
  // 导出为CSV格式
  let csv_export = MultiFormatExporter::export_as(exporter, test_span, "csv")
  
  // 验证导出结果
  assert_true(String::contains(json_export, "test.service"))
  assert_true(String::contains(json_export, "test.operation"))
  assert_true(Protobuf::is_valid(protobuf_export))
  assert_true(String::contains(csv_export, "test.service,test.operation"))
  
  // 测试格式转换
  let json_to_protobuf = MultiFormatExporter::convert(exporter, json_export, "json", "protobuf")
  let protobuf_to_csv = MultiFormatExporter::convert(exporter, protobuf_export, "protobuf", "csv")
  
  // 验证转换结果
  assert_true(Protobuf::is_valid(json_to_protobuf))
  assert_true(String::contains(protobuf_to_csv, "test.service"))
}