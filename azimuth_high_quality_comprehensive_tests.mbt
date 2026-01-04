// Azimuth 高质量综合测试用例
// 专注于遥测系统的高级功能和复杂场景

// 测试1: 分布式追踪的高级特性
test "分布式追踪高级特性测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "advanced.distributed.tracer")
  
  // 创建复杂的追踪场景
  let root_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::set_attribute(root_span, "http.method", StringValue("POST"))
  Span::set_attribute(root_span, "http.target", StringValue("/api/v1/process"))
  Span::set_attribute(root_span, "user.id", StringValue("user-789"))
  
  // 创建子服务追踪
  let auth_context = Span::context(root_span)
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", auth_context)
  Span::set_attribute(auth_span, "auth.token.type", StringValue("jwt"))
  Span::set_attribute(auth_span, "auth.issuer", StringValue("identity.service"))
  Span::set_status(auth_span, Ok)
  Span::end(auth_span)
  
  // 创建数据库追踪链
  let db_context = Span::context(auth_span)
  let db_span = Tracer::start_span_with_context(tracer, "database.transaction", db_context)
  Span::set_attribute(db_span, "db.system", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("INSERT INTO audit_log (...) VALUES (...)"))
  Span::set_attribute(db_span, "db.rows_affected", IntValue(1))
  
  // 创建嵌套的子操作
  let index_scan = Tracer::start_span_with_context(tracer, "database.index_scan", Span::context(db_span))
  Span::set_attribute(index_scan, "db.index.name", StringValue("idx_user_created_at"))
  Span::set_attribute(index_scan, "db.index.hits", IntValue(15))
  Span::set_attribute(index_scan, "db.index.duration_ms", FloatValue(2.5))
  Span::end(index_scan)
  
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  // 创建缓存操作追踪
  let cache_span = Tracer::start_span_with_context(tracer, "cache.operation", auth_context)
  Span::set_attribute(cache_span, "cache.system", StringValue("redis"))
  Span::set_attribute(cache_span, "cache.operation", StringValue("set"))
  Span::set_attribute(cache_span, "cache.key", StringValue("user:789:profile"))
  Span::set_attribute(cache_span, "cache.ttl_seconds", IntValue(3600))
  Span::set_status(cache_span, Ok)
  Span::end(cache_span)
  
  // 完成根span
  Span::set_attribute(root_span, "response.status_code", IntValue(201))
  Span::set_attribute(root_span, "response.size_bytes", IntValue(1024))
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 验证追踪链完整性
  assert_eq(Span::status(root_span), Ok)
  assert_eq(Span::status(auth_span), Ok)
  assert_eq(Span::status(db_span), Ok)
  assert_eq(Span::status(cache_span), Ok)
}

// 测试2: 遥测数据的聚合和分析
test "遥测数据聚合和分析测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "telemetry.aggregator")
  
  // 创建多维度指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let latency_histogram = Meter::create_histogram(meter, "http.request.duration")
  let error_rate = Meter::create_gauge(meter, "http.error.rate")
  
  // 模拟不同服务的指标数据
  let services = ["auth.service", "user.service", "payment.service", "notification.service"]
  let endpoints = ["/login", "/profile", "/charge", "/send"]
  
  // 为每个服务和端点生成指标
  for i in 0..services.length() {
    for j in 0..endpoints.length() {
      let service_attrs = Attributes::new()
      Attributes::set(service_attrs, "service.name", StringValue(services[i]))
      Attributes::set(service_attrs, "endpoint", StringValue(endpoints[j]))
      
      // 生成请求计数
      let request_count = 100.0 + (i * 50.0) + (j * 25.0)
      Counter::add_with_attributes(request_counter, request_count, service_attrs)
      
      // 生成延迟数据
      for k in 0..10 {
        let latency = 0.05 + (i * 0.02) + (j * 0.01) + (k * 0.001)
        Histogram::record_with_attributes(latency_histogram, latency, service_attrs)
      }
      
      // 生成错误率
      let error_percentage = 0.01 + (i * 0.005) + (j * 0.002)
      Gauge::set_with_attributes(error_rate, error_percentage, service_attrs)
    }
  }
  
  // 聚合计算总请求量
  let total_requests = Counter::value(request_counter)
  assert_true(total_requests > 1000.0)
  
  // 计算平均延迟
  let avg_latency = Histogram::mean(latency_histogram)
  assert_true(avg_latency > 0.05 and avg_latency < 0.2)
  
  // 创建自定义聚合指标
  let throughput_calculator = fn(counter: Counter, time_window_seconds: Int) {
    let total = Counter::value(counter)
    total / time_window_seconds.to_float()
  }
  
  let throughput = throughput_calculator(request_counter, 60)
  assert_true(throughput > 10.0)
  
  // 创建百分位计算器
  let percentile_calculator = fn(histogram: Histogram, percentile: Float) {
    // 模拟百分位计算
    let mean = Histogram::mean(histogram)
    mean * (1.0 + percentile * 0.5)
  }
  
  let p99_latency = percentile_calculator(latency_histogram, 0.99)
  let p95_latency = percentile_calculator(latency_histogram, 0.95)
  
  assert_true(p99_latency > p95_latency)
  assert_true(p95_latency > avg_latency)
}

// 测试3: 自适应采样策略
test "自适应采样策略测试" {
  let sampler_config = AdaptiveSamplerConfig::new()
  
  // 设置采样参数
  AdaptiveSamplerConfig::set_base_sampling_rate(sampler_config, 0.1)
  AdaptiveSamplerConfig::set_max_sampling_rate(sampler_config, 0.5)
  AdaptiveSamplerConfig::set_min_sampling_rate(sampler_config, 0.01)
  AdaptiveSamplerConfig::set_adjustment_threshold(sampler_config, 100)
  AdaptiveSamplerConfig::set_adjustment_factor(sampler_config, 1.5)
  
  let sampler = AdaptiveSampler::new(sampler_config)
  
  // 模拟低流量场景
  let low_traffic_ctx = SamplingContext::new()
  SamplingContext::set_current_qps(low_traffic_ctx, 50)
  SamplingContext::set_error_rate(low_traffic_ctx, 0.02)
  
  let low_decision = Sampler::should_sample(sampler, low_traffic_ctx)
  assert_eq(SamplingDecision::decision(low_decision), Sample)
  assert_true(SamplingDecision::sampling_rate(low_decision) >= 0.1)
  
  // 模拟高流量场景
  let high_traffic_ctx = SamplingContext::new()
  SamplingContext::set_current_qps(high_traffic_ctx, 1000)
  SamplingContext::set_error_rate(high_traffic_ctx, 0.05)
  
  let high_decision = Sampler::should_sample(sampler, high_traffic_ctx)
  assert_eq(SamplingDecision::decision(high_decision), Sample)
  assert_true(SamplingDecision::sampling_rate(high_decision) <= 0.1)
  
  // 模拟高错误率场景
  let high_error_ctx = SamplingContext::new()
  SamplingContext::set_current_qps(high_error_ctx, 500)
  SamplingContext::set_error_rate(high_error_ctx, 0.15)
  
  let error_decision = Sampler::should_sample(sampler, high_error_ctx)
  assert_eq(SamplingDecision::decision(error_decision), Sample)
  assert_true(SamplingDecision::sampling_rate(error_decision) > 0.1)
  
  // 测试采样决策的一致性
  let consistent_ctx = SamplingContext::new()
  SamplingContext::set_trace_id(consistent_ctx, "trace-consistency-test")
  
  let decision1 = Sampler::should_sample(sampler, consistent_ctx)
  let decision2 = Sampler::should_sample(sampler, consistent_ctx)
  
  assert_eq(SamplingDecision::decision(decision1), SamplingDecision::decision(decision2))
  assert_eq(SamplingDecision::sampling_rate(decision1), SamplingDecision::sampling_rate(decision2))
  
  // 测试采样属性
  let decision_attrs = SamplingDecision::attributes(low_decision)
  assert_true(Attributes::contains(decision_attrs, "sampler.type"))
  assert_true(Attributes::contains(decision_attrs, "sampler.rate"))
}

// 测试4: 异常检测和恢复
test "异常检测和恢复测试" {
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置异常检测参数
  AnomalyDetector::set_sensitivity(anomaly_detector, 0.8)
  AnomalyDetector::set_window_size(anomaly_detector, 100)
  AnomalyDetector::set_threshold_multiplier(anomaly_detector, 2.5)
  
  // 生成正常指标数据
  let normal_metrics = []
  for i in 0..50 {
    let value = 100.0 + (Random::float() * 20.0) - 10.0  // 90-110范围
    normal_metrics = normal_metrics.push(value)
  }
  
  // 训练异常检测模型
  AnomalyDetector::train(anomaly_detector, normal_metrics)
  let baseline_stats = AnomalyDetector::get_baseline_stats(anomaly_detector)
  
  assert_true(Stats::mean(baseline_stats) > 95.0 and Stats::mean(baseline_stats) < 105.0)
  assert_true(Stats::stddev(baseline_stats) > 5.0 and Stats::stddev(baseline_stats) < 15.0)
  
  // 测试正常数据检测
  let normal_test_data = [98.5, 102.3, 99.7, 101.2, 97.8]
  for value in normal_test_data {
    let result = AnomalyDetector::detect(anomaly_detector, value)
    assert_eq(AnomalyResult::is_anomaly(result), false)
  }
  
  // 测试异常数据检测
  let anomaly_test_data = [150.0, 25.0, 200.0, 10.0, 175.0]
  for value in anomaly_test_data {
    let result = AnomalyDetector::detect(anomaly_detector, value)
    assert_eq(AnomalyResult::is_anomaly(result), true)
    assert_true(AnomalyResult::confidence(result) > 0.8)
  }
  
  // 测试异常恢复策略
  let recovery_strategy = RecoveryStrategy::new()
  RecoveryStrategy::add_action(recovery_strategy, "scale_up", 0.9)
  RecoveryStrategy::add_action(recovery_strategy, "circuit_breaker", 0.7)
  RecoveryStrategy::add_action(recovery_strategy, "fallback_cache", 0.5)
  
  let high_severity_anomaly = AnomalyResult::new(true, 0.95, "latency.spike")
  let recovery_actions = RecoveryStrategy::get_actions(recovery_strategy, high_severity_anomaly)
  
  assert_true(recovery_actions.contains("scale_up"))
  assert_true(recovery_actions.contains("circuit_breaker"))
  
  // 测试恢复执行
  let recovery_executor = RecoveryExecutor::new()
  let recovery_result = RecoveryExecutor::execute(recovery_executor, recovery_actions)
  
  assert_eq(RecoveryResult::status(recovery_result), Success)
  assert_true(RecoveryResult::executed_actions(recovery_result).length() > 0)
}

// 测试5: 性能优化和资源管理
test "性能优化和资源管理测试" {
  let resource_manager = ResourceManager::new()
  
  // 配置资源限制
  ResourceManager::set_memory_limit(resource_manager, 1024 * 1024 * 1024)  // 1GB
  ResourceManager::set_cpu_limit(resource_manager, 80.0)  // 80%
  ResourceManager::set_connection_limit(resource_manager, 1000)
  ResourceManager::set_buffer_size(resource_manager, 64 * 1024)  // 64KB
  
  // 创建资源池
  let buffer_pool = ResourceManager::create_buffer_pool(resource_manager, 100)
  let connection_pool = ResourceManager::create_connection_pool(resource_manager, 50)
  
  // 测试缓冲区池
  let buffers = []
  for i in 0..50 {
    let buffer = BufferPool::acquire(buffer_pool)
    assert_false(Buffer::is_null(buffer))
    assert_eq(Buffer::size(buffer), 64 * 1024)
    buffers = buffers.push(buffer)
  }
  
  // 释放缓冲区
  for buffer in buffers {
    BufferPool::release(buffer_pool, buffer)
  }
  
  // 验证缓冲区池状态
  let pool_stats = BufferPool::get_stats(buffer_pool)
  assert_eq(BufferPoolStats::available(pool_stats), 50)
  assert_eq(BufferPoolStats::acquired(pool_stats), 0)
  
  // 测试连接池
  let connections = []
  for i in 0..25 {
    let connection = ConnectionPool::acquire(connection_pool)
    assert_false(Connection::is_closed(connection))
    connections = connections.push(connection)
  }
  
  // 模拟连接使用
  for connection in connections {
    Connection::execute_query(connection, "SELECT 1")
    ConnectionPool::release(connection_pool, connection)
  }
  
  // 测试性能优化器
  let optimizer = PerformanceOptimizer::new()
  
  // 配置优化策略
  PerformanceOptimizer::enable_batching(optimizer, true)
  PerformanceOptimizer::set_batch_size(optimizer, 100)
  PerformanceOptimizer::enable_compression(optimizer, true)
  PerformanceOptimizer::set_compression_level(optimizer, 6)
  PerformanceOptimizer::enable_caching(optimizer, true)
  PerformanceOptimizer::set_cache_size(optimizer, 1000)
  
  // 测试批量处理优化
  let batch_processor = BatchProcessor::new(100, 5000)  // batch_size, timeout_ms
  
  let items = []
  for i in 0..150 {
    items = items.push("item-" + i.to_string())
  }
  
  let processed_batches = BatchProcessor::process(batch_processor, items, fn(batch) {
    batch.length()  // 模拟处理
  })
  
  assert_eq(processed_batches, 2)  // 150 items / 100 batch_size = 2 batches
  
  // 测试内存优化
  let memory_optimizer = MemoryOptimizer::new()
  MemoryOptimizer::enable_gc_tuning(memory_optimizer, true)
  MemoryOptimizer::set_gc_threshold(memory_optimizer, 0.8)
  
  let memory_stats = MemoryOptimizer::get_stats(memory_optimizer)
  assert_true(MemoryStats::heap_usage(memory_stats) < ResourceManager::get_memory_limit(resource_manager))
}

// 测试6: 数据质量和完整性验证
test "数据质量和完整性验证测试" {
  let data_validator = DataQualityValidator::new()
  
  // 配置验证规则
  DataQualityValidator::add_rule(data_validator, "trace_id", RuleType::Required, RuleOperator::Equals, 32)
  DataQualityValidator::add_rule(data_validator, "span_name", RuleType::Required, RuleOperator::MinLength, 1)
  DataQualityValidator::add_rule(data_validator, "timestamp", RuleType::Required, RuleOperator::Range, [0, 2147483647])
  DataQualityValidator::add_rule(data_validator, "duration", RuleType::Optional, RuleOperator::Min, 0)
  
  // 创建有效测试数据
  let valid_span_data = {
    trace_id: "123456789012345678901234567890ab",
    span_name: "valid_operation",
    timestamp: 1640995200,
    duration: Some(150),
    status: "ok",
    attributes: [("service.name", "test.service")]
  }
  
  let valid_result = DataQualityValidator::validate(data_validator, valid_span_data)
  assert_eq(ValidationResult::is_valid(valid_result), true)
  assert_eq(ValidationResult::errors(valid_result).length(), 0)
  
  // 创建无效测试数据
  let invalid_span_data = {
    trace_id: "short",
    span_name: "",
    timestamp: -1,
    duration: Some(-10),
    status: "ok",
    attributes: [("service.name", "test.service")]
  }
  
  let invalid_result = DataQualityValidator::validate(data_validator, invalid_span_data)
  assert_eq(ValidationResult::is_valid(invalid_result), false)
  assert_true(ValidationResult::errors(invalid_result).length() >= 3)
  
  // 测试数据完整性检查
  let integrity_checker = DataIntegrityChecker::new()
  
  // 创建数据集
  let dataset = [
    {id: "1", trace_id: "trace-a", span_id: "span-1", parent_span_id: None},
    {id: "2", trace_id: "trace-a", span_id: "span-2", parent_span_id: Some("span-1")},
    {id: "3", trace_id: "trace-b", span_id: "span-3", parent_span_id: None},
    {id: "4", trace_id: "trace-a", span_id: "span-4", parent_span_id: Some("span-2")},
    {id: "5", trace_id: "trace-c", span_id: "span-5", parent_span_id: None}
  ]
  
  let integrity_result = DataIntegrityChecker::check(integrity_checker, dataset)
  
  // 验证父子关系完整性
  assert_eq(IntegrityResult::orphaned_spans(integrity_result).length(), 0)
  assert_eq(IntegrityResult::circular_references(integrity_result).length(), 0)
  
  // 测试数据一致性
  let consistency_checker = DataConsistencyChecker::new()
  
  // 创建时间序列数据
  let time_series_data = [
    {timestamp: 1000, value: 10.0, metric: "cpu.usage"},
    {timestamp: 2000, value: 15.0, metric: "cpu.usage"},
    {timestamp: 3000, value: 12.0, metric: "cpu.usage"},
    {timestamp: 4000, value: 20.0, metric: "cpu.usage"},
    {timestamp: 5000, value: 18.0, metric: "cpu.usage"}
  ]
  
  let consistency_result = DataConsistencyChecker::check_temporal_consistency(
    consistency_checker, 
    time_series_data
  )
  
  assert_eq(ConsistencyResult::is_consistent(consistency_result), true)
  assert_eq(ConsistencyResult::anomalies(consistency_result).length(), 0)
  
  // 测试数据修复
  let data_repairer = DataRepairer::new()
  DataRepairer::enable_auto_repair(data_repairer, true)
  DataRepairer::set_repair_strategy(data_repairer, RepairStrategy::Interpolation)
  
  let damaged_data = {
    trace_id: "123456789012345678901234567890ab",
    span_name: "repaired_operation",
    timestamp: 1640995200,
    duration: None,  // 缺失数据
    status: "ok",
    attributes: [("service.name", "test.service")]
  }
  
  let repaired_data = DataRepairer::repair(data_repairer, damaged_data)
  assert_true(Record::has_field(repaired_data, "duration"))
  assert_true(Record::get_field(repaired_data, "duration") != None)
}

// 测试7: 跨服务上下文传播
test "跨服务上下文传播测试" {
  let propagator_factory = PropagatorFactory::new()
  
  // 创建复合传播器
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let custom_propagator = CustomPropagator::new("x-custom-context")
  
  let composite_propagator = CompositePropagator::new([
    trace_propagator,
    baggage_propagator,
    custom_propagator
  ])
  
  // 创建源上下文
  let source_context = Context::root()
  
  // 添加追踪上下文
  let trace_context = SpanContext::new(
    "trace-1234567890123456", 
    "span-abcdef1234567890", 
    true, 
    "key1=value1,key2=value2"
  )
  let ctx_with_trace = Context::with_span_context(source_context, trace_context)
  
  // 添加行李项
  let user_id_key = ContextKey::new("user.id")
  let request_id_key = ContextKey::new("request.id")
  let tenant_id_key = ContextKey::new("tenant.id")
  
  let ctx_with_baggage = Context::with_value(ctx_with_trace, user_id_key, "user-12345")
  let ctx_with_baggage2 = Context::with_value(ctx_with_baggage, request_id_key, "req-abcdef")
  let ctx_with_baggage3 = Context::with_value(ctx_with_baggage2, tenant_id_key, "tenant-789")
  
  // 添加自定义上下文
  let custom_key = ContextKey::new("custom.auth.token")
  let final_context = Context::with_value(ctx_with_baggage3, custom_key, "token-secret-123")
  
  // 创建载体
  let http_headers = TextMapCarrier::new()
  let metadata = TextMapCarrier::new()
  
  // 注入上下文到HTTP头
  CompositePropagator::inject(composite_propagator, final_context, http_headers)
  
  // 验证HTTP头
  assert_true(TextMapCarrier::contains(http_headers, "traceparent"))
  assert_true(TextMapCarrier::contains(http_headers, "baggage"))
  assert_true(TextMapCarrier::contains(http_headers, "x-custom-context"))
  
  // 注入上下文到元数据
  CompositePropagator::inject(composite_propagator, final_context, metadata)
  
  // 模拟跨服务调用 - 服务A到服务B
  let service_b_context = CompositePropagator::extract(composite_propagator, http_headers)
  
  // 验证追踪上下文传播
  let extracted_trace = Context::get_span_context(service_b_context)
  assert_eq(SpanContext::trace_id(extracted_trace), "trace-1234567890123456")
  assert_eq(SpanContext::span_id(extracted_trace), "span-abcdef1234567890")
  
  // 验证行李项传播
  let extracted_user_id = Context::get(service_b_context, user_id_key)
  let extracted_request_id = Context::get(service_b_context, request_id_key)
  let extracted_tenant_id = Context::get(service_b_context, tenant_id_key)
  
  assert_eq(extracted_user_id, Some("user-12345"))
  assert_eq(extracted_request_id, Some("req-abcdef"))
  assert_eq(extracted_tenant_id, Some("tenant-789"))
  
  // 验证自定义上下文传播
  let extracted_custom = Context::get(service_b_context, custom_key)
  assert_eq(extracted_custom, Some("token-secret-123"))
  
  // 模拟服务B到服务C的传播
  let service_c_carrier = TextMapCarrier::new()
  
  // 服务B添加新的上下文
  let service_name_key = ContextKey::new("service.name")
  let service_b_enhanced = Context::with_value(service_b_context, service_name_key, "service-b")
  
  // 传播到服务C
  CompositePropagator::inject(composite_propagator, service_b_enhanced, service_c_carrier)
  let service_c_context = CompositePropagator::extract(composite_propagator, service_c_carrier)
  
  // 验证上下文累积
  let service_c_user_id = Context::get(service_c_context, user_id_key)
  let service_c_service_name = Context::get(service_c_context, service_name_key)
  
  assert_eq(service_c_user_id, Some("user-12345"))  // 原有上下文保留
  assert_eq(service_c_service_name, Some("service-b"))  // 新上下文添加
  
  // 测试上下文隔离
  let isolated_context = Context::root()
  let isolated_with_user = Context::with_value(isolated_context, user_id_key, "isolated-user")
  
  assert_ne(Context::get(isolated_with_user, user_id_key), Context::get(service_c_context, user_id_key))
}

// 测试8: 实时监控和告警
test "实时监控和告警测试" {
  let monitoring_system = MonitoringSystem::new()
  
  // 配置监控指标
  let cpu_monitor = MonitoringSystem::add_metric_monitor(monitoring_system, "system.cpu.usage")
  let memory_monitor = MonitoringSystem::add_metric_monitor(monitoring_system, "system.memory.usage")
  let error_rate_monitor = MonitoringSystem::add_metric_monitor(monitoring_system, "http.error.rate")
  let latency_monitor = MonitoringSystem::add_metric_monitor(monitoring_system, "http.request.duration")
  
  // 配置告警规则
  let alert_manager = AlertManager::new()
  
  // CPU使用率告警
  AlertManager::add_rule(alert_manager, "cpu.high", AlertRule::new()
    .with_metric("system.cpu.usage")
    .with_condition(Condition::GreaterThan(80.0))
    .with_duration(300)  // 5分钟
    .with_severity(AlertSeverity::Warning)
    .with_message("CPU使用率过高: {{value}}%"))
  
  // 内存使用率告警
  AlertManager::add_rule(alert_manager, "memory.high", AlertRule::new()
    .with_metric("system.memory.usage")
    .with_condition(Condition::GreaterThan(90.0))
    .with_duration(180)  // 3分钟
    .with_severity(AlertSeverity::Critical)
    .with_message("内存使用率过高: {{value}}%"))
  
  // 错误率告警
  AlertManager::add_rule(alert_manager, "error.rate.high", AlertRule::new()
    .with_metric("http.error.rate")
    .with_condition(Condition::GreaterThan(0.05))
    .with_duration(120)  // 2分钟
    .with_severity(AlertSeverity::Warning)
    .with_message("错误率过高: {{value}}%"))
  
  // 延迟告警
  AlertManager::add_rule(alert_manager, "latency.high", AlertRule::new()
    .with_metric("http.request.duration")
    .with_condition(Condition::PercentileGreaterThan(0.95, 1.0))
    .with_duration(300)  // 5分钟
    .with_severity(AlertSeverity::Warning)
    .with_message("P95延迟过高: {{value}}s"))
  
  // 模拟正常指标数据
  let normal_metrics = [
    {timestamp: 1000, cpu: 45.2, memory: 60.5, error_rate: 0.01, latency_p95: 0.25},
    {timestamp: 2000, cpu: 52.8, memory: 65.3, error_rate: 0.015, latency_p95: 0.3},
    {timestamp: 3000, cpu: 48.1, memory: 62.7, error_rate: 0.008, latency_p95: 0.22},
    {timestamp: 4000, cpu: 55.6, memory: 68.9, error_rate: 0.012, latency_p95: 0.28},
    {timestamp: 5000, cpu: 50.3, memory: 64.2, error_rate: 0.009, latency_p95: 0.24}
  ]
  
  for metric in normal_metrics {
    MonitoringSystem::record_metric(monitoring_system, "system.cpu.usage", metric.cpu, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "system.memory.usage", metric.memory, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.error.rate", metric.error_rate, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.request.duration", metric.latency_p95, metric.timestamp)
    
    let alerts = AlertManager::evaluate(alert_manager, monitoring_system)
    assert_eq(alerts.length(), 0)  // 正常情况下不应有告警
  }
  
  // 模拟异常指标数据 - CPU使用率过高
  let high_cpu_metrics = [
    {timestamp: 6000, cpu: 85.2, memory: 70.5, error_rate: 0.02, latency_p95: 0.35},
    {timestamp: 7000, cpu: 88.7, memory: 72.3, error_rate: 0.025, latency_p95: 0.4},
    {timestamp: 8000, cpu: 90.1, memory: 75.8, error_rate: 0.03, latency_p95: 0.45},
    {timestamp: 9000, cpu: 87.5, memory: 73.2, error_rate: 0.022, latency_p95: 0.38},
    {timestamp: 10000, cpu: 91.3, memory: 76.9, error_rate: 0.028, latency_p95: 0.42}
  ]
  
  for metric in high_cpu_metrics {
    MonitoringSystem::record_metric(monitoring_system, "system.cpu.usage", metric.cpu, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "system.memory.usage", metric.memory, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.error.rate", metric.error_rate, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.request.duration", metric.latency_p95, metric.timestamp)
  }
  
  // 评估告警
  let alerts = AlertManager::evaluate(alert_manager, monitoring_system)
  assert_true(alerts.length() >= 1)  // 应该有CPU告警
  
  let cpu_alert = alerts.find(fn(alert) { Alert::rule_id(alert) == "cpu.high" })
  assert_true(cpu_alert != None)
  assert_eq(Alert::severity(cpu_alert.unwrap()), AlertSeverity::Warning)
  
  // 测试告警通知
  let notification_manager = NotificationManager::new()
  
  // 配置通知渠道
  NotificationManager::add_channel(notification_manager, "email", EmailChannel::new("alerts@example.com"))
  NotificationManager::add_channel(notification_manager, "slack", SlackChannel::new("#alerts"))
  NotificationManager::add_channel(notification_manager, "pagerduty", PagerDutyChannel::new("integration-key"))
  
  // 发送告警通知
  for alert in alerts {
    let notification_result = NotificationManager::send(notification_manager, alert)
    assert_eq(NotificationResult::status(notification_result), Sent)
  }
  
  // 测试告警抑制
  let alert_suppressor = AlertSuppressor::new()
  AlertSuppressor::add_suppression_rule(alert_suppressor, "maintenance.window", 
    SuppressionRule::new()
      .with_time_window("22:00-06:00")  // 夜间维护窗口
      .with_severity_filter([AlertSeverity::Warning])
      .with_reason("维护窗口期间抑制警告级别告警"))
  
  let suppressed_alerts = AlertSuppressor::suppress(alert_suppressor, alerts)
  assert_true(suppressed_alerts.length() <= alerts.length())
  
  // 测试告警恢复
  let recovery_metrics = [
    {timestamp: 11000, cpu: 65.2, memory: 68.5, error_rate: 0.015, latency_p95: 0.3},
    {timestamp: 12000, cpu: 58.7, memory: 65.3, error_rate: 0.012, latency_p95: 0.28},
    {timestamp: 13000, cpu: 52.1, memory: 62.8, error_rate: 0.008, latency_p95: 0.25}
  ]
  
  for metric in recovery_metrics {
    MonitoringSystem::record_metric(monitoring_system, "system.cpu.usage", metric.cpu, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "system.memory.usage", metric.memory, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.error.rate", metric.error_rate, metric.timestamp)
    MonitoringSystem::record_metric(monitoring_system, "http.request.duration", metric.latency_p95, metric.timestamp)
  }
  
  let recovery_alerts = AlertManager::evaluate(alert_manager, monitoring_system)
  assert_true(recovery_alerts.length() < alerts.length())  // 告警应该减少或恢复
}

// 测试9: 配置管理和动态更新
test "配置管理和动态更新测试" {
  let config_manager = ConfigManager::new()
  
  // 设置基础配置
  ConfigManager::set(config_manager, "telemetry.service.name", "azimuth-collector")
  ConfigManager::set(config_manager, "telemetry.service.version", "2.1.0")
  ConfigManager::set(config_manager, "telemetry.exporter.type", "otlp")
  ConfigManager::set(config_manager, "telemetry.exporter.endpoint", "https://otel-collector.example.com:4317")
  ConfigManager::set(config_manager, "telemetry.exporter.protocol", "grpc")
  
  // 配置采样
  ConfigManager::set(config_manager, "sampler.type", "adaptive")
  ConfigManager::set(config_manager, "sampler.base_rate", 0.1)
  ConfigManager::set(config_manager, "sampler.max_rate", 0.5)
  ConfigManager::set(config_manager, "sampler.adjustment_threshold", 100)
  
  // 配置批处理
  ConfigManager::set(config_manager, "batch.processor.enabled", true)
  ConfigManager::set(config_manager, "batch.processor.max_queue_size", 2048)
  ConfigManager::set(config_manager, "batch.processor.max_export_batch_size", 512)
  ConfigManager::set(config_manager, "batch.processor.export_timeout_ms", 30000)
  ConfigManager::set(config_manager, "batch.processor.schedule_delay_ms", 5000)
  
  // 配置资源
  ConfigManager::set(config_manager, "resource.attributes.service.name", "azimuth-service")
  ConfigManager::set(config_manager, "resource.attributes.service.namespace", "production")
  ConfigManager::set(config_manager, "resource.attributes.service.instance.id", "instance-12345")
  ConfigManager::set(config_manager, "resource.attributes.deployment.environment", "prod")
  
  // 验证初始配置
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.name"), Some("azimuth-collector"))
  assert_eq(ConfigManager::get(config_manager, "sampler.type"), Some("adaptive"))
  assert_eq(ConfigManager::get(config_manager, "batch.processor.enabled"), Some("true"))
  
  // 测试配置验证
  let config_validator = ConfigValidator::new()
  
  // 添加验证规则
  ConfigValidator::add_rule(config_validator, "sampler.base_rate", ValidationRule::new()
    .with_type(ValueType::Float)
    .with_range(0.0, 1.0)
    .with_required(true))
  
  ConfigValidator::add_rule(config_validator, "batch.processor.max_queue_size", ValidationRule::new()
    .with_type(ValueType::Integer)
    .with_min(1)
    .with_max(10000)
    .with_required(true))
  
  ConfigValidator::add_rule(config_validator, "telemetry.exporter.endpoint", ValidationRule::new()
    .with_type(ValueType::URL)
    .with_required(true))
  
  // 验证配置
  let validation_result = ConfigValidator::validate(config_validator, config_manager)
  assert_eq(ValidationResult::is_valid(validation_result), true)
  assert_eq(ValidationResult::errors(validation_result).length(), 0)
  
  // 测试动态配置更新
  let config_watcher = ConfigWatcher::new()
  ConfigWatcher::watch_key(config_watcher, "sampler.base_rate")
  ConfigWatcher::watch_key(config_watcher, "batch.processor.max_export_batch_size")
  ConfigWatcher::watch_key(config_watcher, "telemetry.exporter.endpoint")
  
  // 添加变更监听器
  let change_log = []
  ConfigWatcher::add_listener(config_watcher, fn(key, old_value, new_value) {
    change_log = change_log.push({key, old_value, new_value})
  })
  
  // 动态更新配置
  ConfigManager::set(config_manager, "sampler.base_rate", 0.2)
  ConfigManager::set(config_manager, "batch.processor.max_export_batch_size", 1024)
  ConfigManager::set(config_manager, "telemetry.exporter.endpoint", "https://new-collector.example.com:4317")
  
  // 验证配置变更通知
  assert_true(change_log.length() >= 3)
  
  let sampler_change = change_log.find(fn(change) { change.key == "sampler.base_rate" })
  assert_true(sampler_change != None)
  assert_eq(sampler_change.unwrap().old_value, "0.1")
  assert_eq(sampler_change.unwrap().new_value, "0.2")
  
  // 测试配置热重载
  let config_reloader = ConfigReloader::new()
  ConfigReloader::set_auto_reload(config_reloader, true)
  ConfigReloader::set_reload_interval(config_reloader, 30000)  // 30秒
  
  // 模拟配置文件变更
  let updated_config = {
    "telemetry": {
      "service": {
        "name": "azimuth-collector-updated",
        "version": "2.1.1"
      },
      "exporter": {
        "type": "otlp",
        "endpoint": "https://updated-collector.example.com:4317",
        "protocol": "grpc"
      }
    },
    "sampler": {
      "type": "adaptive",
      "base_rate": 0.15,
      "max_rate": 0.6
    }
  }
  
  let reload_result = ConfigReloader::reload_from_object(config_reloader, config_manager, updated_config)
  assert_eq(ReloadResult::status(reload_result), Success)
  assert_true(ReloadResult::updated_keys(reload_result).length() > 0)
  
  // 验证更新后的配置
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.name"), Some("azimuth-collector-updated"))
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.version"), Some("2.1.1"))
  assert_eq(ConfigManager::get(config_manager, "sampler.base_rate"), Some("0.15"))
  
  // 测试配置回滚
  let config_history = ConfigHistory::new()
  ConfigHistory::save_snapshot(config_history, config_manager)
  
  // 进行更多配置变更
  ConfigManager::set(config_manager, "sampler.base_rate", 0.8)  // 无效值
  ConfigManager::set(config_manager, "telemetry.exporter.endpoint", "invalid-url")
  
  // 验证配置失败
  let invalid_validation_result = ConfigValidator::validate(config_validator, config_manager)
  assert_eq(ValidationResult::is_valid(invalid_validation_result), false)
  assert_true(ValidationResult::errors(invalid_validation_result).length() >= 2)
  
  // 回滚到上一个有效配置
  let rollback_result = ConfigHistory::rollback(config_history, config_manager, 1)
  assert_eq(RollbackResult::status(rollback_result), Success)
  
  // 验证回滚后的配置
  assert_eq(ConfigManager::get(config_manager, "telemetry.service.name"), Some("azimuth-collector-updated"))
  assert_eq(ConfigManager::get(config_manager, "sampler.base_rate"), Some("0.15"))
  
  let final_validation_result = ConfigValidator::validate(config_validator, config_manager)
  assert_eq(ValidationResult::is_valid(final_validation_result), true)
}

// 测试10: 安全性和多租户隔离
test "安全性和多租户隔离测试" {
  let security_manager = SecurityManager::new()
  
  // 配置安全策略
  SecurityManager::set_encryption_policy(security_manager, EncryptionPolicy::AES256)
  SecurityManager::set_access_control_policy(security_manager, AccessControlPolicy::RoleBased)
  SecurityManager::set_data_retention_policy(security_manager, DataRetentionPolicy::ThirtyDays)
  
  // 创建租户管理器
  let tenant_manager = TenantManager::new()
  
  // 创建租户
  let tenant_a = TenantManager::create_tenant(tenant_manager, {
    id: "tenant-a-123",
    name: "Enterprise Customer A",
    plan: "enterprise",
    quota: {
      max_spans_per_day: 10000000,
      max_metrics_per_day: 5000000,
      max_storage_gb: 100,
      max_data_retention_days: 90
    },
    isolation_level: "strict",
    encryption_key_id: "key-tenant-a-123"
  })
  
  let tenant_b = TenantManager::create_tenant(tenant_manager, {
    id: "tenant-b-456",
    name: "Standard Customer B",
    plan: "standard",
    quota: {
      max_spans_per_day: 1000000,
      max_metrics_per_day: 500000,
      max_storage_gb: 10,
      max_data_retention_days: 30
    },
    isolation_level: "standard",
    encryption_key_id: "key-tenant-b-456"
  })
  
  // 验证租户创建
  assert_eq(Tenant::id(tenant_a), "tenant-a-123")
  assert_eq(Tenant::plan(tenant_a), "enterprise")
  assert_eq(Tenant::quota(tenant_a).max_spans_per_day, 10000000)
  
  assert_eq(Tenant::id(tenant_b), "tenant-b-456")
  assert_eq(Tenant::plan(tenant_b), "standard")
  assert_eq(Tenant::quota(tenant_b).max_spans_per_day, 1000000)
  
  // 创建租户上下文
  let tenant_a_context = TenantContext::new(tenant_a)
  let tenant_b_context = TenantContext::new(tenant_b)
  
  // 测试数据隔离
  let data_isolator = DataIsolator::new()
  
  // 租户A的数据
  let tenant_a_span = {
    trace_id: "trace-tenant-a-123",
    span_id: "span-tenant-a-456",
    tenant_id: "tenant-a-123",
    service_name: "payment-service",
    operation_name: "process_payment",
    user_id: "user-tenant-a-789"
  }
  
  // 租户B的数据
  let tenant_b_span = {
    trace_id: "trace-tenant-b-123",
    span_id: "span-tenant-b-456",
    tenant_id: "tenant-b-456",
    service_name: "user-service",
    operation_name: "get_profile",
    user_id: "user-tenant-b-789"
  }
  
  // 验证数据隔离
  assert_true(DataIsolator::can_access(data_isolator, tenant_a_context, tenant_a_span))
  assert_false(DataIsolator::can_access(data_isolator, tenant_a_context, tenant_b_span))
  assert_true(DataIsolator::can_access(data_isolator, tenant_b_context, tenant_b_span))
  assert_false(DataIsolator::can_access(data_isolator, tenant_b_context, tenant_a_span))
  
  // 测试数据加密
  let encryption_service = EncryptionService::new()
  
  // 为租户A加密数据
  let tenant_a_encrypted = EncryptionService::encrypt_for_tenant(
    encryption_service, 
    tenant_a_span, 
    "tenant-a-123"
  )
  
  // 为租户B加密数据
  let tenant_b_encrypted = EncryptionService::encrypt_for_tenant(
    encryption_service, 
    tenant_b_span, 
    "tenant-b-456"
  )
  
  // 验证加密
  assert_true(EncryptedData::is_encrypted(tenant_a_encrypted))
  assert_true(EncryptedData::is_encrypted(tenant_b_encrypted))
  assert_ne(EncryptedData::cipher_text(tenant_a_encrypted), EncryptedData::cipher_text(tenant_b_encrypted))
  
  // 验证解密
  let tenant_a_decrypted = EncryptionService::decrypt_for_tenant(
    encryption_service, 
    tenant_a_encrypted, 
    "tenant-a-123"
  )
  
  let tenant_b_decrypted = EncryptionService::decrypt_for_tenant(
    encryption_service, 
    tenant_b_encrypted, 
    "tenant-b-456"
  )
  
  assert_eq(DecryptedData::tenant_id(tenant_a_decrypted), "tenant-a-123")
  assert_eq(DecryptedData::tenant_id(tenant_b_decrypted), "tenant-b-456")
  
  // 测试跨租户解密失败
  let cross_tenant_decrypt = EncryptionService::decrypt_for_tenant(
    encryption_service, 
    tenant_a_encrypted, 
    "tenant-b-456"  // 错误的租户ID
  )
  
  assert_eq(DecryptionResult::status(cross_tenant_decrypt), Error)
  assert_eq(DecryptionResult::error_code(cross_tenant_decrypt), "INVALID_TENANT")
  
  // 测试访问控制
  let access_controller = AccessController::new()
  
  // 创建角色和权限
  AccessController::create_role(access_controller, "tenant_admin", [
    "data.read",
    "data.write", 
    "data.delete",
    "config.read",
    "config.write"
  ])
  
  AccessController::create_role(access_controller, "tenant_user", [
    "data.read",
    "data.write"
  ])
  
  AccessController::create_role(access_controller, "tenant_viewer", [
    "data.read"
  ])
  
  // 分配用户角色
  AccessController::assign_role(access_controller, "user-tenant-a-001", "tenant-a-123", "tenant_admin")
  AccessController::assign_role(access_controller, "user-tenant-a-002", "tenant-a-123", "tenant_user")
  AccessController::assign_role(access_controller, "user-tenant-b-001", "tenant-b-456", "tenant_viewer")
  
  // 验证权限检查
  let admin_access = AccessController::check_permission(
    access_controller, 
    "user-tenant-a-001", 
    "tenant-a-123", 
    "data.delete"
  )
  assert_eq(AccessResult::allowed(admin_access), true)
  
  let user_access = AccessController::check_permission(
    access_controller, 
    "user-tenant-a-002", 
    "tenant-a-123", 
    "data.delete"
  )
  assert_eq(AccessResult::allowed(user_access), false)
  
  let viewer_access = AccessController::check_permission(
    access_controller, 
    "user-tenant-b-001", 
    "tenant-b-456", 
    "data.write"
  )
  assert_eq(AccessResult::allowed(viewer_access), false)
  
  // 测试配额限制
  let quota_enforcer = QuotaEnforcer::new()
  
  // 设置租户配额
  QuotaEnforcer::set_tenant_quota(quota_enforcer, "tenant-a-123", {
    spans_per_day: 10000000,
    metrics_per_day: 5000000,
    storage_gb: 100
  })
  
  QuotaEnforcer::set_tenant_quota(quota_enforcer, "tenant-b-456", {
    spans_per_day: 1000000,
    metrics_per_day: 500000,
    storage_gb: 10
  })
  
  // 模拟配额使用
  let tenant_a_usage = {
    spans_today: 8500000,
    metrics_today: 4200000,
    storage_used_gb: 85
  }
  
  let tenant_b_usage = {
    spans_today: 950000,
    metrics_today: 480000,
    storage_used_gb: 9.5
  }
  
  // 检查配额
  let tenant_a_quota_check = QuotaEnforcer::check_quota(quota_enforcer, "tenant-a-123", tenant_a_usage)
  assert_eq(QuotaCheckResult::within_limits(tenant_a_quota_check), true)
  
  let tenant_b_quota_check = QuotaEnforcer::check_quota(quota_enforcer, "tenant-b-456", tenant_b_usage)
  assert_eq(QuotaCheckResult::within_limits(tenant_b_quota_check), true)
  
  // 模拟超限情况
  let tenant_a_over_usage = {
    spans_today: 10500000,  // 超出限制
    metrics_today: 4200000,
    storage_used_gb: 85
  }
  
  let tenant_a_over_check = QuotaEnforcer::check_quota(quota_enforcer, "tenant-a-123", tenant_a_over_usage)
  assert_eq(QuotaCheckResult::within_limits(tenant_a_over_check), false)
  assert_true(QuotaCheckResult::exceeded_limits(tenant_a_over_check).contains("spans_per_day"))
  
  // 测试审计日志
  let audit_logger = AuditLogger::new()
  
  // 记录审计事件
  AuditLogger::log_access_attempt(audit_logger, {
    tenant_id: "tenant-a-123",
    user_id: "user-tenant-a-001",
    resource: "span-data",
    action: "read",
    result: "success",
    timestamp: 1640995200,
    ip_address: "192.168.1.100",
    user_agent: "azimuth-collector/2.1.0"
  })
  
  AuditLogger::log_config_change(audit_logger, {
    tenant_id: "tenant-a-123",
    user_id: "user-tenant-a-001",
    config_key: "sampler.base_rate",
    old_value: "0.1",
    new_value: "0.2",
    timestamp: 1640995300
  })
  
  AuditLogger::log_security_event(audit_logger, {
    tenant_id: "tenant-b-456",
    user_id: "user-tenant-b-001",
    event_type: "unauthorized_access_attempt",
    resource: "admin-config",
    result: "denied",
    timestamp: 1640995400,
    details: "User attempted to access admin configuration without sufficient privileges"
  })
  
  // 查询审计日志
  let tenant_a_audit_logs = AuditLogger::query_by_tenant(audit_logger, "tenant-a-123")
  assert_eq(tenant_a_audit_logs.length(), 2)
  
  let tenant_b_audit_logs = AuditLogger::query_by_tenant(audit_logger, "tenant-b-456")
  assert_eq(tenant_b_audit_logs.length(), 1)
  
  let security_events = AuditLogger::query_by_event_type(audit_logger, "security_event")
  assert_eq(security_events.length(), 1)
  assert_eq(AuditEvent::event_type(security_events[0]), "unauthorized_access_attempt")
}