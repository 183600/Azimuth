// Azimuth 遥测数据流管道测试用例
// 专注于遥测数据的流式处理和管道传输功能

// 测试1: 基础数据流管道构建
test "基础数据流管道构建测试" {
  let pipeline = TelemetryPipeline::new("基础遥测管道")
  
  // 创建数据源
  let metric_source = MetricDataSource::new("application.metrics")
  let trace_source = TraceDataSource::new("application.traces")
  let log_source = LogDataSource::new("application.logs")
  
  // 创建处理器
  let metric_processor = MetricProcessor::new()
  let trace_processor = TraceProcessor::new()
  let log_processor = LogProcessor::new()
  
  // 创建输出目标
  let prometheus_sink = PrometheusSink::new("http://prometheus:9090")
  let jaeger_sink = JaegerSink::new("http://jaeger:14268")
  let elasticsearch_sink = ElasticsearchSink::new("http://elasticsearch:9200")
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, metric_source)
  TelemetryPipeline::add_source(pipeline, trace_source)
  TelemetryPipeline::add_source(pipeline, log_source)
  
  TelemetryPipeline::add_processor(pipeline, metric_processor)
  TelemetryPipeline::add_processor(pipeline, trace_processor)
  TelemetryPipeline::add_processor(pipeline, log_processor)
  
  TelemetryPipeline::add_sink(pipeline, prometheus_sink)
  TelemetryPipeline::add_sink(pipeline, jaeger_sink)
  TelemetryPipeline::add_sink(pipeline, elasticsearch_sink)
  
  // 配置数据流路由
  TelemetryPipeline::route(pipeline, metric_source, metric_processor, prometheus_sink)
  TelemetryPipeline::route(pipeline, trace_source, trace_processor, jaeger_sink)
  TelemetryPipeline::route(pipeline, log_source, log_processor, elasticsearch_sink)
  
  // 验证管道配置
  assert_eq(TelemetryPipeline::get_source_count(pipeline), 3)
  assert_eq(TelemetryPipeline::get_processor_count(pipeline), 3)
  assert_eq(TelemetryPipeline::get_sink_count(pipeline), 3)
  assert_eq(TelemetryPipeline::get_route_count(pipeline), 3)
}

// 测试2: 数据流转换和增强
test "数据流转换和增强测试" {
  let pipeline = TelemetryPipeline::new("数据增强管道")
  let source = MetricDataSource::new("raw.metrics")
  let enrichment_processor = EnrichmentProcessor::new()
  let sink = MemorySink::new()
  
  // 配置增强规则
  let geo_enrichment = GeoEnrichmentRule::new("client.ip")
  let service_enrichment = ServiceEnrichmentRule::new("service.name")
  let timestamp_enrichment = TimestampEnrichmentRule::new()
  
  EnrichmentProcessor::add_rule(enrichment_processor, geo_enrichment)
  EnrichmentProcessor::add_rule(enrichment_processor, service_enrichment)
  EnrichmentProcessor::add_rule(enrichment_processor, timestamp_enrichment)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, enrichment_processor)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, enrichment_processor, sink)
  
  // 创建测试数据
  let raw_metric = Metric::new("http.request.duration", 0.125)
  Metric::add_attribute(raw_metric, "client.ip", "192.168.1.100")
  Metric::add_attribute(raw_metric, "service.name", "api.service")
  
  // 处理数据
  TelemetryPipeline::process(pipeline, raw_metric)
  
  // 验证增强结果
  let enriched_metrics = MemorySink::get_metrics(sink)
  assert_eq(enriched_metrics.length(), 1)
  
  let enriched_metric = enriched_metrics[0]
  assert_true(Metric::has_attribute(enriched_metric, "client.geo.country"))
  assert_true(Metric::has_attribute(enriched_metric, "service.version"))
  assert_true(Metric::has_attribute(enriched_metric, "processing.timestamp"))
}

// 测试3: 数据流过滤和采样
test "数据流过滤和采样测试" {
  let pipeline = TelemetryPipeline::new("过滤采样管道")
  let source = TraceDataSource::new("all.traces")
  let filter_processor = FilterProcessor::new()
  let sampling_processor = SamplingProcessor::new()
  let sink = MemorySink::new()
  
  // 配置过滤规则
  let error_filter = ErrorFilter::new()  // 只允许错误追踪
  let high_latency_filter = HighLatencyFilter::new(1000)  // 只允许高延迟追踪
  
  FilterProcessor::add_filter(filter_processor, error_filter)
  FilterProcessor::add_filter(filter_processor, high_latency_filter)
  
  // 配置采样策略
  let probabilistic_sampler = ProbabilisticSampler::new(0.1)  // 10%采样率
  let deterministic_sampler = DeterministicSampler::new("service.name")
  
  SamplingProcessor::set_primary_sampler(sampling_processor, probabilistic_sampler)
  SamplingProcessor::set_fallback_sampler(sampling_processor, deterministic_sampler)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, filter_processor)
  TelemetryPipeline::add_processor(pipeline, sampling_processor)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, filter_processor, sampling_processor)
  TelemetryPipeline::route(pipeline, sampling_processor, sink)
  
  // 创建测试数据
  let normal_trace = Trace::new("trace-1", "normal.operation", Ok, 500)
  let error_trace = Trace::new("trace-2", "error.operation", Error, 200)
  let slow_trace = Trace::new("trace-3", "slow.operation", Ok, 1500)
  
  // 处理数据
  TelemetryPipeline::process(pipeline, normal_trace)
  TelemetryPipeline::process(pipeline, error_trace)
  TelemetryPipeline::process(pipeline, slow_trace)
  
  // 验证过滤和采样结果
  let filtered_traces = MemorySink::get_traces(sink)
  
  // 错误和高延迟追踪应该通过过滤器
  assert_true(filtered_traces.any(fn(trace) { trace.trace_id == "trace-2" }))
  assert_true(filtered_traces.any(fn(trace) { trace.trace_id == "trace-3" }))
  
  // 正常追踪应该被过滤掉
  assert_false(filtered_traces.any(fn(trace) { trace.trace_id == "trace-1" }))
}

// 测试4: 数据流聚合和统计
test "数据流聚合和统计测试" {
  let pipeline = TelemetryPipeline::new("聚合统计管道")
  let source = MetricDataSource::new("raw.metrics")
  let aggregation_processor = AggregationProcessor::new()
  let sink = MemorySink::new()
  
  // 配置聚合规则
  let rate_aggregation = RateAggregation::new("http.requests.total", "1m")
  let avg_aggregation = AvgAggregation::new("http.request.duration", "5m")
  let percentile_aggregation = PercentileAggregation::new("http.request.duration", "5m", [50.0, 95.0, 99.0])
  
  AggregationProcessor::add_aggregation(aggregation_processor, rate_aggregation)
  AggregationProcessor::add_aggregation(aggregation_processor, avg_aggregation)
  AggregationProcessor::add_aggregation(aggregation_processor, percentile_aggregation)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, aggregation_processor)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, aggregation_processor, sink)
  
  // 创建测试数据
  for i in 0..=100 {
    let request_metric = Metric::new("http.requests.total", 1.0)
    Metric::add_attribute(request_metric, "endpoint", "/api/users")
    Metric::add_attribute(request_metric, "method", "GET")
    
    let duration_metric = Metric::new("http.request.duration", (50 + i % 200).to_float())
    Metric::add_attribute(duration_metric, "endpoint", "/api/users")
    Metric::add_attribute(duration_metric, "method", "GET")
    
    TelemetryPipeline::process(pipeline, request_metric)
    TelemetryPipeline::process(pipeline, duration_metric)
  }
  
  // 验证聚合结果
  let aggregated_metrics = MemorySink::get_metrics(sink)
  
  // 检查速率聚合
  let rate_metric = aggregated_metrics.find(fn(m) { m.name == "http.requests.total.rate" })
  assert_true(rate_metric.is_some())
  assert_true(rate_metric.unwrap().value > 0.0)
  
  // 检查平均值聚合
  let avg_metric = aggregated_metrics.find(fn(m) { m.name == "http.request.duration.avg" })
  assert_true(avg_metric.is_some())
  assert_true(avg_metric.unwrap().value >= 50.0 && avg_metric.unwrap().value <= 250.0)
  
  // 检查百分位数聚合
  let p95_metric = aggregated_metrics.find(fn(m) { m.name == "http.request.duration.p95" })
  assert_true(p95_metric.is_some())
}

// 测试5: 数据流错误处理和恢复
test "数据流错误处理和恢复测试" {
  let pipeline = TelemetryPipeline::new("容错管道")
  let source = LogDataSource::new("application.logs")
  let processor = LogProcessor::new()
  let primary_sink = FailingSink::new("primary", 0.3)  // 30%失败率
  let fallback_sink = MemorySink::new()
  
  // 配置错误处理策略
  let retry_policy = RetryPolicy::exponential_backoff(3, 100, 2000)  // 最多重试3次
  let circuit_breaker = CircuitBreaker::new(5, 60000)  // 5次失败后断路，60秒后重试
  
  FailingSink::set_retry_policy(primary_sink, retry_policy)
  FailingSink::set_circuit_breaker(primary_sink, circuit_breaker)
  
  // 配置故障转移
  let failover_strategy = FailoverStrategy::new()
  FailoverStrategy::add_sink(failover_strategy, primary_sink)
  FailoverStrategy::add_sink(failover_strategy, fallback_sink)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, processor)
  TelemetryPipeline::add_failover_strategy(pipeline, failover_strategy)
  
  // 创建测试数据
  for i in 0..=20 {
    let log_entry = LogEntry::new(Info, "Test log message " + i.to_string(), "test.service")
    TelemetryPipeline::process(pipeline, log_entry)
  }
  
  // 验证错误处理
  let primary_metrics = FailingSink::get_success_count(primary_sink)
  let fallback_logs = MemorySink::get_logs(fallback_sink)
  
  // 主接收器应该成功处理一些数据
  assert_true(primary_metrics > 0)
  
  // 故障转移接收器应该处理失败的数据
  assert_true(fallback_logs.length() > 0)
  
  // 总数据量应该等于输入数据量
  assert_eq(primary_metrics + fallback_logs.length(), 21)
}

// 测试6: 数据流背压控制
test "数据流背压控制测试" {
  let pipeline = TelemetryPipeline::new("背压控制管道")
  let source = MetricDataSource::new("high.volume.metrics")
  let slow_processor = SlowProcessor::new(50)  // 50ms处理延迟
  let fast_sink = MemorySink::new()
  
  // 配置背压策略
  let backpressure_strategy = BackpressureStrategy::buffer_with_drop(1000, 0.1)  // 缓冲1000条，丢弃10%
  
  TelemetryPipeline::set_backpressure_strategy(pipeline, backpressure_strategy)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, slow_processor)
  TelemetryPipeline::add_sink(pipeline, fast_sink)
  
  TelemetryPipeline::route(pipeline, source, slow_processor, fast_sink)
  
  // 快速发送大量数据
  for i in 0..=2000 {
    let metric = Metric::new("test.metric", i.to_float())
    TelemetryPipeline::process(pipeline, metric)
  }
  
  // 等待处理完成
  TelemetryPipeline::wait_for_completion(pipeline, 5000)
  
  // 验证背压控制效果
  let processed_metrics = MemorySink::get_metrics(fast_sink)
  let buffer_stats = TelemetryPipeline::get_buffer_stats(pipeline)
  
  // 处理的指标数量应该小于等于缓冲区大小
  assert_true(processed_metrics.length() <= 1000)
  
  // 缓冲区应该有丢弃记录
  assert_true(buffer_stats.dropped_count > 0)
  
  // 丢弃率应该接近配置的10%
  let actual_drop_rate = buffer_stats.dropped_count.to_float() / 2001.0
  assert_true(actual_drop_rate >= 0.05 && actual_drop_rate <= 0.15)
}

// 测试7: 数据流监控和指标
test "数据流监控和指标测试" {
  let pipeline = TelemetryPipeline::new("监控管道")
  let source = TraceDataSource::new("monitored.traces")
  let processor = TraceProcessor::new()
  let sink = MemorySink::new()
  let monitor = PipelineMonitor::new()
  
  // 启用监控
  TelemetryPipeline::enable_monitoring(pipeline, monitor)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, processor)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, processor, sink)
  
  // 处理数据
  for i in 0..=100 {
    let trace = Trace::new("trace-" + i.to_string(), "test.operation", Ok, 100 + i)
    TelemetryPipeline::process(pipeline, trace)
  }
  
  // 获取监控指标
  let monitoring_metrics = PipelineMonitor::get_metrics(monitor)
  
  // 验证监控指标
  assert_eq(monitoring_metrics.processed_count, 101)
  assert_true(monitoring_metrics.total_processing_time_ms > 0)
  assert_true(monitoring_metrics.average_processing_time_ms > 0)
  assert_eq(monitoring_metrics.error_count, 0)
  assert_true(monitoring_metrics.throughput_per_second > 0)
  
  // 验证组件级监控
  let source_metrics = PipelineMonitor::get_source_metrics(monitor, source)
  let processor_metrics = PipelineMonitor::get_processor_metrics(monitor, processor)
  let sink_metrics = PipelineMonitor::get_sink_metrics(monitor, sink)
  
  assert_eq(source_metrics.received_count, 101)
  assert_eq(processor_metrics.processed_count, 101)
  assert_eq(sink_metrics.written_count, 101)
}

// 测试8: 数据流动态配置
test "数据流动态配置测试" {
  let pipeline = TelemetryPipeline::new("动态配置管道")
  let source = MetricDataSource::new("configurable.metrics")
  let processor = MetricProcessor::new()
  let sink = MemorySink::new()
  let config_manager = PipelineConfigManager::new()
  
  // 初始配置
  let initial_config = PipelineConfig::new()
  PipelineConfig::set_batch_size(initial_config, 100)
  PipelineConfig::set_flush_interval(initial_config, 5000)
  PipelineConfig::set_compression_enabled(initial_config, false)
  
  TelemetryPipeline::apply_config(pipeline, initial_config)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, processor)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, processor, sink)
  
  // 验证初始配置
  assert_eq(TelemetryPipeline::get_batch_size(pipeline), 100)
  assert_eq(TelemetryPipeline::get_flush_interval(pipeline), 5000)
  assert_false(TelemetryPipeline::is_compression_enabled(pipeline))
  
  // 动态更新配置
  let updated_config = PipelineConfig::new()
  PipelineConfig::set_batch_size(updated_config, 200)
  PipelineConfig::set_flush_interval(updated_config, 2000)
  PipelineConfig::set_compression_enabled(updated_config, true)
  
  PipelineConfigManager::update_config(config_manager, pipeline, updated_config)
  
  // 验证更新后的配置
  assert_eq(TelemetryPipeline::get_batch_size(pipeline), 200)
  assert_eq(TelemetryPipeline::get_flush_interval(pipeline), 2000)
  assert_true(TelemetryPipeline::is_compression_enabled(pipeline))
  
  // 处理数据验证配置生效
  for i in 0..=150 {
    let metric = Metric::new("test.metric", i.to_float())
    TelemetryPipeline::process(pipeline, metric)
  }
  
  // 验证批处理行为
  let batch_stats = TelemetryPipeline::get_batch_stats(pipeline)
  assert_eq(batch_stats.batch_size, 200)
  assert_true(batch_stats.compression_enabled)
}

// 测试9: 数据流多路复用
test "数据流多路复用测试" {
  let pipeline = TelemetryPipeline::new("多路复用管道")
  let source = MetricDataSource::new("multiplexed.metrics")
  let processor = MetricProcessor::new()
  let sink1 = MemorySink::new()
  let sink2 = MemorySink::new()
  let sink3 = MemorySink::new()
  
  // 配置多路复用规则
  let multiplexer = MetricMultiplexer::new()
  
  // 基于指标名称的路由
  MetricMultiplexer::add_route(multiplexer, "http.*", [sink1])
  MetricMultiplexer::add_route(multiplexer, "db.*", [sink2])
  MetricMultiplexer::add_route(multiplexer, "cache.*", [sink3])
  
  // 基于属性的路由
  MetricMultiplexer::add_attribute_route(multiplexer, "service", "api", [sink1])
  MetricMultiplexer::add_attribute_route(multiplexer, "service", "database", [sink2])
  MetricMultiplexer::add_attribute_route(multiplexer, "service", "cache", [sink3])
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, processor)
  TelemetryPipeline::add_multiplexer(pipeline, multiplexer)
  
  TelemetryPipeline::route(pipeline, source, processor, multiplexer)
  
  // 创建测试数据
  let http_metric = Metric::new("http.requests.total", 100.0)
  Metric::add_attribute(http_metric, "service", "api")
  
  let db_metric = Metric::new("db.connections.active", 25.0)
  Metric::add_attribute(db_metric, "service", "database")
  
  let cache_metric = Metric::new("cache.hits.total", 1000.0)
  Metric::add_attribute(cache_metric, "service", "cache")
  
  // 处理数据
  TelemetryPipeline::process(pipeline, http_metric)
  TelemetryPipeline::process(pipeline, db_metric)
  TelemetryPipeline::process(pipeline, cache_metric)
  
  // 验证多路复用结果
  let sink1_metrics = MemorySink::get_metrics(sink1)
  let sink2_metrics = MemorySink::get_metrics(sink2)
  let sink3_metrics = MemorySink::get_metrics(sink3)
  
  assert_eq(sink1_metrics.length(), 1)
  assert_eq(sink2_metrics.length(), 1)
  assert_eq(sink3_metrics.length(), 1)
  
  assert_eq(sink1_metrics[0].name, "http.requests.total")
  assert_eq(sink2_metrics[0].name, "db.connections.active")
  assert_eq(sink3_metrics[0].name, "cache.hits.total")
}

// 测试10: 数据流序列化和反序列化
test "数据流序列化和反序列化测试" {
  let pipeline = TelemetryPipeline::new("序列化管道")
  let source = TraceDataSource::new("serialized.traces")
  let serializer = JsonSerializer::new()
  let deserializer = JsonDeserializer::new()
  let sink = MemorySink::new()
  
  // 配置序列化选项
  let serialization_options = SerializationOptions::new()
  SerializationOptions::set_pretty_print(serialization_options, false)
  SerializationOptions::set_include_metadata(serialization_options, true)
  SerializationOptions::set_compression(serialization_options, Gzip)
  
  JsonSerializer::set_options(serializer, serialization_options)
  
  // 构建管道
  TelemetryPipeline::add_source(pipeline, source)
  TelemetryPipeline::add_processor(pipeline, serializer)
  TelemetryPipeline::add_processor(pipeline, deserializer)
  TelemetryPipeline::add_sink(pipeline, sink)
  
  TelemetryPipeline::route(pipeline, source, serializer, deserializer)
  TelemetryPipeline::route(pipeline, deserializer, sink)
  
  // 创建测试数据
  let original_trace = Trace::new("trace-12345", "api.request", Ok, 250)
  Trace::add_attribute(original_trace, "http.method", "GET")
  Trace::add_attribute(original_trace, "http.url", "https://api.example.com/users")
  Trace::add_attribute(original_trace, "http.status_code", 200)
  Trace::add_span(original_trace, Span::new("span-1", "http.handler", Client))
  Trace::add_span(original_trace, Span::new("span-2", "database.query", Server))
  
  // 处理数据
  TelemetryPipeline::process(pipeline, original_trace)
  
  // 验证序列化和反序列化结果
  let deserialized_traces = MemorySink::get_traces(sink)
  assert_eq(deserialized_traces.length(), 1)
  
  let deserialized_trace = deserialized_traces[0]
  
  // 验证基本属性
  assert_eq(deserialized_trace.trace_id, original_trace.trace_id)
  assert_eq(deserialized_trace.operation_name, original_trace.operation_name)
  assert_eq(deserialized_trace.status, original_trace.status)
  assert_eq(deserialized_trace.duration_ms, original_trace.duration_ms)
  
  // 验证属性
  let original_method = Trace::get_attribute(original_trace, "http.method")
  let deserialized_method = Trace::get_attribute(deserialized_trace, "http.method")
  assert_eq(original_method, deserialized_method)
  
  // 验证span
  assert_eq(deserialized_trace.spans.length(), original_trace.spans.length())
}