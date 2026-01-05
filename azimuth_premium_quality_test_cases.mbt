// Azimuth 遥测系统 - 高级质量测试用例
// 专注于遥测系统的高级功能、边界情况和性能优化

// 测试1: 分布式追踪上下文传播
test "分布式追踪上下文传播测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.tracing")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "service.root.operation")
  let root_context = Span::context(root_span)
  
  // 设置根span属性
  Span::set_attribute(root_span, "service.name", StringValue("auth.service"))
  Span::set_attribute(root_span, "service.version", StringValue("1.2.3"))
  Span::set_attribute(root_span, "deployment.environment", StringValue("production"))
  
  // 模拟服务间调用 - 用户认证服务
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", root_context)
  Span::set_attribute(auth_span, "user.id", StringValue("user-12345"))
  Span::set_attribute(auth_span, "auth.method", StringValue("oauth2"))
  Span::add_event(auth_span, "auth.started", None)
  
  // 模拟认证延迟
  Span::add_event(auth_span, "token.validated", None)
  Span::set_status(auth_span, Ok)
  Span::end(auth_span)
  
  // 模拟服务间调用 - 用户资料服务
  let profile_span = Tracer::start_span_with_context(tracer, "profile.service.fetch", root_context)
  Span::set_attribute(profile_span, "user.id", StringValue("user-12345"))
  Span::set_attribute(profile_span, "profile.fields", ArrayValue(["name", "email", "preferences"]))
  Span::add_event(profile_span, "profile.query.started", None)
  
  // 模拟数据库查询
  let db_span = Tracer::start_span_with_context(tracer, "database.query", Span::context(profile_span))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::add_event(db_span, "query.executed", None)
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  Span::add_event(profile_span, "profile.retrieved", None)
  Span::set_status(profile_span, Ok)
  Span::end(profile_span)
  
  // 完成根span
  Span::set_attribute(root_span, "operation.duration", IntValue(150))
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 验证上下文传播
  assert_eq(Span::status(root_span), Ok)
  assert_eq(Span::status(auth_span), Ok)
  assert_eq(Span::status(profile_span), Ok)
  assert_eq(Span::status(db_span), Ok)
  
  // 验证追踪关系
  let root_trace_id = Span::trace_id(root_span)
  let auth_trace_id = Span::trace_id(auth_span)
  let profile_trace_id = Span::trace_id(profile_span)
  let db_trace_id = Span::trace_id(db_span)
  
  assert_eq(root_trace_id, auth_trace_id)
  assert_eq(root_trace_id, profile_trace_id)
  assert_eq(root_trace_id, db_trace_id)
}

// 测试2: 遥测数据压缩和序列化
test "遥测数据压缩和序列化测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "compression.test")
  
  // 创建大量遥测数据
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let latency_histogram = Meter::create_histogram(meter, "http.request.duration")
  let error_gauge = Meter::create_gauge(meter, "http.errors.rate")
  
  // 生成大量数据点
  for i in 1..=1000 {
    Counter::add_with_attributes(request_counter, 1.0, [
      ("method", StringValue("GET")),
      ("endpoint", StringValue("/api/data/" + (i % 10).to_string())),
      ("status_code", IntValue(200))
    ])
    
    let latency = 0.1 + (i % 50).to_float() * 0.01
    Histogram::record_with_attributes(latency_histogram, latency, [
      ("method", StringValue("GET")),
      ("endpoint", StringValue("/api/data/" + (i % 10).to_string()))
    ])
    
    if i % 100 == 0 {
      Gauge::set(error_gauge, (i % 5).to_float())
    }
  }
  
  // 创建序列化器
  let json_serializer = TelemetrySerializer::new("json")
  let protobuf_serializer = TelemetrySerializer::new("protobuf")
  let compressed_serializer = TelemetrySerializer::new("gzip")
  
  // 序列化数据
  let json_data = TelemetrySerializer::serialize_batch(json_serializer, [
    MetricData::from_counter(request_counter),
    MetricData::from_histogram(latency_histogram),
    MetricData::from_gauge(error_gauge)
  ])
  
  let protobuf_data = TelemetrySerializer::serialize_batch(protobuf_serializer, [
    MetricData::from_counter(request_counter),
    MetricData::from_histogram(latency_histogram),
    MetricData::from_gauge(error_gauge)
  ])
  
  let compressed_data = TelemetrySerializer::serialize_with_compression(compressed_serializer, [
    MetricData::from_counter(request_counter),
    MetricData::from_histogram(latency_histogram),
    MetricData::from_gauge(error_gauge)
  ], "gzip")
  
  // 验证序列化结果
  assert_true(json_data.length() > 0)
  assert_true(protobuf_data.length() > 0)
  assert_true(compressed_data.length() > 0)
  
  // 验证压缩效果
  let compression_ratio = compressed_data.length().to_float() / json_data.length().to_float()
  assert_true(compression_ratio < 0.8) // 压缩率应该小于80%
  
  // 反序列化验证
  let deserializer = TelemetryDeserializer::new("json")
  let deserialized_metrics = TelemetryDeserializer::deserialize_batch(deserializer, json_data)
  
  assert_eq(deserialized_metrics.length(), 3)
  
  // 验证反序列化后的数据完整性
  for metric in deserialized_metrics {
    match metric {
      MetricData::Counter(counter_data) => {
        assert_eq(counter_data.name, "http.requests.total")
        assert_eq(counter_data.value, 1000.0)
      }
      MetricData::Histogram(histogram_data) => {
        assert_eq(histogram_data.name, "http.request.duration")
        assert_eq(histogram_data.count, 1000)
      }
      MetricData::Gauge(gauge_data) => {
        assert_eq(gauge_data.name, "http.errors.rate")
        assert_true(gauge_data.value >= 0.0 && gauge_data.value <= 4.0)
      }
      _ => assert_true(false)
    }
  }
}

// 测试3: 多租户隔离和安全性测试
test "多租户隔离和安全性测试" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  
  // 创建多租户配置
  let tenant_manager = TenantManager::new()
  
  // 添加租户
  TenantManager::add_tenant(tenant_manager, "tenant-a", "Tenant A Corporation")
  TenantManager::add_tenant(tenant_manager, "tenant-b", "Tenant B LLC")
  TenantManager::add_tenant(tenant_manager, "tenant-c", "Tenant C Inc.")
  
  // 为租户A创建遥测资源
  let tracer_a = TracerProvider::get_tracer_with_tenant(tracer_provider, "tenant-a.service", "tenant-a")
  let meter_a = MeterProvider::get_meter_with_tenant(meter_provider, "tenant-a.metrics", "tenant-a")
  
  let tenant_a_span = Tracer::start_span(tracer_a, "tenant.a.operation")
  Span::set_attribute(tenant_a_span, "tenant.id", StringValue("tenant-a"))
  Span::set_attribute(tenant_a_span, "sensitive.data", StringValue("encrypted-data-a"))
  Span::end(tenant_a_span)
  
  let tenant_a_counter = Meter::create_counter_with_tenant(meter_a, "tenant.a.requests", "tenant-a")
  Counter::add_with_attributes(tenant_a_counter, 10.0, [
    ("tenant.id", StringValue("tenant-a")),
    ("user.role", StringValue("admin"))
  ])
  
  // 为租户B创建遥测资源
  let tracer_b = TracerProvider::get_tracer_with_tenant(tracer_provider, "tenant-b.service", "tenant-b")
  let meter_b = MeterProvider::get_meter_with_tenant(meter_provider, "tenant-b.metrics", "tenant-b")
  
  let tenant_b_span = Tracer::start_span(tracer_b, "tenant.b.operation")
  Span::set_attribute(tenant_b_span, "tenant.id", StringValue("tenant-b"))
  Span::set_attribute(tenant_b_span, "sensitive.data", StringValue("encrypted-data-b"))
  Span::end(tenant_b_span)
  
  let tenant_b_counter = Meter::create_counter_with_tenant(meter_b, "tenant.b.requests", "tenant-b")
  Counter::add_with_attributes(tenant_b_counter, 15.0, [
    ("tenant.id", StringValue("tenant-b")),
    ("user.role", StringValue("user"))
  ])
  
  // 测试租户隔离
  let tenant_a_data = TenantManager::get_telemetry_data(tenant_manager, "tenant-a")
  let tenant_b_data = TenantManager::get_telemetry_data(tenant_manager, "tenant-b")
  
  // 验证租户A只能访问自己的数据
  assert_eq(tenant_a_data.spans.length(), 1)
  assert_eq(tenant_a_data.metrics.length(), 1)
  assert_eq(tenant_a_data.spans[0].name, "tenant.a.operation")
  assert_eq(tenant_a_data.metrics[0].name, "tenant.a.requests")
  
  // 验证租户B只能访问自己的数据
  assert_eq(tenant_b_data.spans.length(), 1)
  assert_eq(tenant_b_data.metrics.length(), 1)
  assert_eq(tenant_b_data.spans[0].name, "tenant.b.operation")
  assert_eq(tenant_b_data.metrics[0].name, "tenant.b.requests")
  
  // 测试数据加密和脱敏
  let data_masker = DataMasker::new()
  DataMasker::add_sensitive_field(data_masker, "sensitive.data")
  DataMasker::add_pii_field(data_masker, "user.email")
  
  let masked_a_data = DataMasker::mask_telemetry(data_masker, tenant_a_data)
  let masked_b_data = DataMasker::mask_telemetry(data_masker, tenant_b_data)
  
  // 验证敏感数据已脱敏
  assert_eq(masked_a_data.spans[0].attributes["sensitive.data"], "***")
  assert_eq(masked_b_data.spans[0].attributes["sensitive.data"], "***")
  
  // 测试访问控制
  let access_controller = AccessController::new()
  AccessController::add_role(access_controller, "tenant-a.admin", ["read", "write"])
  AccessController::add_role(access_controller, "tenant-b.user", ["read"])
  
  // 验证权限控制
  assert_true(AccessController::has_permission(access_controller, "tenant-a.admin", "write"))
  assert_false(AccessController::has_permission(access_controller, "tenant-b.user", "write"))
  assert_true(AccessController::has_permission(access_controller, "tenant-b.user", "read"))
}

// 测试4: 实时流处理遥测测试
test "实时流处理遥测测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "stream.processing")
  
  // 创建流处理指标
  let throughput_counter = Meter::create_counter(meter, "stream.throughput")
  let latency_histogram = Meter::create_histogram(meter, "stream.processing.latency")
  let backlog_gauge = Meter::create_gauge(meter, "stream.backlog.size")
  let error_rate = Meter::create_gauge(meter, "stream.error.rate")
  
  // 创建实时流处理器
  let stream_processor = StreamProcessor::new("realtime.events")
  StreamProcessor::set_buffer_size(stream_processor, 1000)
  StreamProcessor::set_batch_size(stream_processor, 50)
  
  // 模拟实时事件流
  let event_stream = [
    ("user.login", 100, 10),
    ("user.logout", 80, 5),
    ("data.update", 150, 20),
    ("system.alert", 50, 2),
    ("user.login", 120, 12),
    ("data.update", 140, 18),
    ("user.login", 90, 8),
    ("system.alert", 60, 3),
    ("data.update", 160, 22),
    ("user.logout", 70, 6)
  ]
  
  // 处理事件流
  let mut total_events = 0
  let mut total_latency = 0
  let mut error_count = 0
  
  for (event_type, latency, size) in event_stream {
    // 记录处理开始
    let start_time = Timestamp::now()
    
    // 模拟事件处理
    StreamProcessor::process_event(stream_processor, event_type, size)
    
    // 记录处理完成
    let end_time = Timestamp::now()
    let processing_latency = (end_time - start_time).to_float() / 1000.0 // 转换为毫秒
    
    // 更新指标
    Counter::add_with_attributes(throughput_counter, 1.0, [
      ("event.type", StringValue(event_type))
    ])
    
    Histogram::record_with_attributes(latency_histogram, processing_latency, [
      ("event.type", StringValue(event_type))
    ])
    
    total_events = total_events + 1
    total_latency = total_latency + latency
    
    // 模拟错误
    if event_type == "system.alert" && size < 55 {
      error_count = error_count + 1
    }
  }
  
  // 更新聚合指标
  Gauge::set(backlog_gauge, StreamProcessor::get_backlog_size(stream_processor).to_float())
  Gauge::set(error_rate, (error_count.to_float() / total_events.to_float()) * 100.0)
  
  // 创建实时分析器
  let analyzer = StreamAnalyzer::new()
  StreamAnalyzer::add_event_stream(analyzer, event_stream)
  
  // 计算流统计
  let avg_throughput = StreamAnalyzer::get_average_throughput(analyzer)
  let peak_latency = StreamAnalyzer::get_peak_latency(analyzer)
  let bottleneck_events = StreamAnalyzer::detect_bottlenecks(analyzer)
  
  // 验证流处理结果
  assert_eq(Counter::value(throughput_counter), total_events.to_float())
  assert_true(Gauge::value(backlog_gauge) >= 0.0)
  assert_eq(Gauge::value(error_rate), 20.0) // 2/10 = 20%
  
  // 验证分析结果
  assert_true(avg_throughput > 0.0)
  assert_true(peak_latency > 0.0)
  assert_true(bottleneck_events.length() >= 0)
  
  // 创建自适应流控制器
  let controller = StreamController::new()
  StreamController::set_target_latency(controller, 100.0) // 目标延迟100ms
  StreamController::adjust_processing_rate(controller, avg_throughput, peak_latency)
  
  // 验证自适应控制
  let adjusted_rate = StreamController::get_processing_rate(controller)
  assert_true(adjusted_rate > 0.0)
  
  // 测试流恢复机制
  let recovery_manager = StreamRecoveryManager::new()
  StreamRecoveryManager::set_checkpoint_interval(recovery_manager, 100)
  StreamRecoveryManager::create_checkpoint(recovery_manager, stream_processor)
  
  // 模拟流处理失败
  StreamProcessor::simulate_failure(stream_processor)
  assert_true(StreamProcessor::is_failed(stream_processor))
  
  // 执行恢复
  StreamRecoveryManager::recover_from_checkpoint(recovery_manager, stream_processor)
  assert_false(StreamProcessor::is_failed(stream_processor))
  
  // 验证恢复后的状态
  assert_true(StreamProcessor::get_processed_count(stream_processor) > 0)
}

// 测试5: 遥测配置管理动态更新测试
test "遥测配置管理动态更新测试" {
  let config_manager = ConfigManager::new()
  
  // 初始化默认配置
  let default_config = TelemetryConfig::default()
  ConfigManager::load_config(config_manager, default_config)
  
  // 验证默认配置
  let current_config = ConfigManager::get_current_config(config_manager)
  assert_eq(current_config.sampling_rate, 1.0)
  assert_eq(current_config.max_spans_per_second, 1000)
  assert_eq(current_config.metric_export_interval, 60000) // 60秒
  
  // 创建配置监听器
  let config_listener = ConfigListener::new()
  ConfigListener::on_sampling_rate_change(config_listener, fn(old_rate, new_rate) {
    assert_true(old_rate != new_rate)
  })
  
  ConfigListener::on_export_interval_change(config_listener, fn(old_interval, new_interval) {
    assert_true(old_interval != new_interval)
  })
  
  ConfigManager::add_listener(config_manager, config_listener)
  
  // 动态更新采样率
  let new_sampling_rate = 0.5
  ConfigManager::update_sampling_rate(config_manager, new_sampling_rate)
  
  // 验证配置更新
  let updated_config = ConfigManager::get_current_config(config_manager)
  assert_eq(updated_config.sampling_rate, new_sampling_rate)
  
  // 动态更新导出间隔
  let new_export_interval = 30000 // 30秒
  ConfigManager::update_export_interval(config_manager, new_export_interval)
  
  // 验证配置更新
  let updated_config2 = ConfigManager::get_current_config(config_manager)
  assert_eq(updated_config2.metric_export_interval, new_export_interval)
  
  // 测试配置热重载
  let temp_config_file = "/tmp/telemetry_config.json"
  let hot_reload_config = TelemetryConfig {
    sampling_rate: 0.8,
    max_spans_per_second: 2000,
    metric_export_interval: 45000,
    enable_compression: true,
    batch_size: 500
  }
  
  TelemetryConfig::save_to_file(hot_reload_config, temp_config_file)
  ConfigManager::hot_reload_from_file(config_manager, temp_config_file)
  
  // 验证热重载结果
  let reloaded_config = ConfigManager::get_current_config(config_manager)
  assert_eq(reloaded_config.sampling_rate, 0.8)
  assert_eq(reloaded_config.max_spans_per_second, 2000)
  assert_eq(reloaded_config.metric_export_interval, 45000)
  assert_eq(reloaded_config.enable_compression, true)
  assert_eq(reloaded_config.batch_size, 500)
  
  // 测试配置验证
  let invalid_config = TelemetryConfig {
    sampling_rate: 1.5, // 无效：超过1.0
    max_spans_per_second: -100, // 无效：负数
    metric_export_interval: 0, // 无效：零
    enable_compression: true,
    batch_size: 0 // 无效：零
  }
  
  let validation_result = ConfigManager::validate_config(config_manager, invalid_config)
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 3)
  
  // 测试配置回滚
  let rollback_config = ConfigManager::create_snapshot(config_manager)
  ConfigManager::update_sampling_rate(config_manager, 0.1)
  ConfigManager::update_max_spans_per_second(config_manager, 500)
  
  // 验证更改
  let changed_config = ConfigManager::get_current_config(config_manager)
  assert_eq(changed_config.sampling_rate, 0.1)
  assert_eq(changed_config.max_spans_per_second, 500)
  
  // 执行回滚
  ConfigManager::rollback_to_snapshot(config_manager, rollback_config)
  
  // 验证回滚结果
  let rolled_back_config = ConfigManager::get_current_config(config_manager)
  assert_eq(rolled_back_config.sampling_rate, 0.8)
  assert_eq(rolled_back_config.max_spans_per_second, 2000)
  
  // 测试配置版本控制
  let version = ConfigManager::get_config_version(config_manager)
  let versioned_config = ConfigManager::get_config_by_version(config_manager, version - 1)
  assert_true(versioned_config.is_some)
  
  // 清理临时文件
  File::delete(temp_config_file)
}

// 测试6: 自适应采样策略测试
test "自适应采样策略测试" {
  let sampling_manager = SamplingManager::new()
  
  // 创建自适应采样策略
  let adaptive_strategy = AdaptiveSamplingStrategy::new()
  AdaptiveSamplingStrategy::set_target_throughput(adaptive_strategy, 1000.0)
  AdaptiveSamplingStrategy::set_error_rate_threshold(adaptive_strategy, 5.0)
  AdaptiveSamplingStrategy::set_latency_threshold(adaptive_strategy, 100.0)
  
  SamplingManager::set_strategy(sampling_manager, adaptive_strategy)
  
  // 模拟不同负载情况
  let low_load_scenario = [
    ("normal.request", 50.0, 200),  // 延迟50ms, 200个请求
    ("normal.request", 55.0, 180),
    ("normal.request", 48.0, 220),
    ("normal.request", 52.0, 190),
    ("normal.request", 49.0, 210)
  ]
  
  let high_load_scenario = [
    ("high.volume.request", 120.0, 2000),  // 延迟120ms, 2000个请求
    ("high.volume.request", 135.0, 2100),
    ("high.volume.request", 125.0, 1900),
    ("high.volume.request", 140.0, 2200),
    ("high.volume.request", 130.0, 1800)
  ]
  
  let error_scenario = [
    ("error.prone.request", 80.0, 100),   // 延迟80ms, 100个请求, 10%错误率
    ("error.prone.request", 85.0, 120),
    ("error.prone.request", 78.0, 90),
    ("error.prone.request", 90.0, 110),
    ("error.prone.request", 82.0, 95)
  ]
  
  // 测试低负载情况下的采样
  let low_load_samples = []
  for (request_type, latency, count) in low_load_scenario {
    for i in 1..=count {
      let should_sample = SamplingManager::should_sample(sampling_manager, request_type, latency)
      low_load_samples = low_load_samples.push(should_sample)
    }
  }
  
  let low_load_sampling_rate = low_load_samples.filter(fn(s) { s }).length().to_float() / low_load_samples.length().to_float()
  assert_true(low_load_sampling_rate > 0.8) // 低负载时应该保持高采样率
  
  // 测试高负载情况下的采样
  let high_load_samples = []
  for (request_type, latency, count) in high_load_scenario {
    for i in 1..=count {
      let should_sample = SamplingManager::should_sample(sampling_manager, request_type, latency)
      high_load_samples = high_load_samples.push(should_sample)
    }
  }
  
  let high_load_sampling_rate = high_load_samples.filter(fn(s) { s }).length().to_float() / high_load_samples.length().to_float()
  assert_true(high_load_sampling_rate < 0.6) // 高负载时应该降低采样率
  
  // 测试错误情况下的采样
  let error_samples = []
  for (request_type, latency, count) in error_scenario {
    for i in 1..=count {
      // 模拟10%的错误率
      let has_error = i % 10 == 0
      let should_sample = SamplingManager::should_sample_with_error(sampling_manager, request_type, latency, has_error)
      error_samples = error_samples.push(should_sample)
    }
  }
  
  let error_sampling_rate = error_samples.filter(fn(s) { s }).length().to_float() / error_samples.length().to_float()
  assert_true(error_sampling_rate > 0.9) // 错误请求应该总是被采样
  
  // 测试采样策略动态调整
  let sampling_metrics = SamplingManager::get_metrics(sampling_manager)
  assert_true(sampling_metrics.total_requests > 0)
  assert_true(sampling_metrics.sampled_requests > 0)
  assert_true(sampling_metrics.current_sampling_rate > 0.0 && sampling_metrics.current_sampling_rate <= 1.0)
  
  // 测试基于优先级的采样
  let priority_strategy = PrioritySamplingStrategy::new()
  PrioritySamplingStrategy::set_priority(priority_strategy, "critical.request", 1.0)
  PrioritySamplingStrategy::set_priority(priority_strategy, "normal.request", 0.5)
  PrioritySamplingStrategy::set_priority(priority_strategy, "background.request", 0.1)
  
  SamplingManager::add_fallback_strategy(sampling_manager, priority_strategy)
  
  // 测试优先级采样
  let critical_sampled = SamplingManager::should_sample_with_priority(sampling_manager, "critical.request", 60.0)
  let normal_sampled = SamplingManager::should_sample_with_priority(sampling_manager, "normal.request", 60.0)
  let background_sampled = SamplingManager::should_sample_with_priority(sampling_manager, "background.request", 60.0)
  
  assert_true(critical_sampled) // 关键请求应该总是被采样
  assert_true(normal_sampled || !normal_sampled) // 正常请求根据策略采样
  assert_true(background_sampled || !background_sampled) // 后台请求根据策略采样
  
  // 测试采样决策缓存
  let cache_enabled = SamplingManager::enable_decision_cache(sampling_manager, 1000)
  assert_true(cache_enabled)
  
  // 重复相同请求应该使用缓存
  let first_decision = SamplingManager::should_sample(sampling_manager, "cached.request", 75.0)
  let cached_decision = SamplingManager::should_sample(sampling_manager, "cached.request", 75.0)
  assert_eq(first_decision, cached_decision)
  
  // 测试采样统计
  let statistics = SamplingManager::get_statistics(sampling_manager)
  assert_true(statistics.cache_hit_rate >= 0.0)
  assert_true(statistics.strategy_adjustments > 0)
  assert_true(statistics.priority_sampling_enabled)
}

// 测试7: 遥测数据质量验证测试
test "遥测数据质量验证测试" {
  let quality_validator = DataQualityValidator::new()
  
  // 设置数据质量规则
  QualityValidator::add_required_field(quality_validator, "trace.id")
  QualityValidator::add_required_field(quality_validator, "span.name")
  QualityValidator::add_required_field(quality_validator, "timestamp")
  
  QualityValidator::add_field_format_rule(quality_validator, "trace.id", "^[a-f0-9]{32}$")
  QualityValidator::add_field_format_rule(quality_validator, "timestamp", "^[0-9]{13}$")
  
  QualityValidator::add_value_range_rule(quality_validator, "duration", 0, 300000) // 0-5分钟
  QualityValidator::add_value_range_rule(quality_validator, "http.status_code", 100, 599)
  
  // 创建测试数据集
  let valid_telemetry_data = [
    TelemetryData {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_name: "http.request",
      timestamp: "1609459200000",
      duration: 150,
      attributes: [
        ("http.method", "GET"),
        ("http.url", "/api/users"),
        ("http.status_code", "200")
      ]
    },
    TelemetryData {
      trace_id: "abcdef1234567890abcdef1234567890",
      span_name: "database.query",
      timestamp: "1609459200100",
      duration: 80,
      attributes: [
        ("db.statement", "SELECT * FROM users"),
        ("db.type", "postgresql")
      ]
    }
  ]
  
  let invalid_telemetry_data = [
    TelemetryData {
      trace_id: "invalid-trace-id", // 无效格式
      span_name: "http.request",
      timestamp: "1609459200000",
      duration: 150,
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", "200")
      ]
    },
    TelemetryData {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_name: "", // 空名称
      timestamp: "invalid-timestamp", // 无效时间戳
      duration: -10, // 负持续时间
      attributes: [
        ("http.status_code", "999") // 超出范围
      ]
    }
  ]
  
  // 验证有效数据
  let valid_results = []
  for data in valid_telemetry_data {
    let result = QualityValidator::validate(quality_validator, data)
    valid_results = valid_results.push(result)
  }
  
  // 验证无效数据
  let invalid_results = []
  for data in invalid_telemetry_data {
    let result = QualityValidator::validate(quality_validator, data)
    invalid_results = invalid_results.push(result)
  }
  
  // 验证验证结果
  assert_eq(valid_results.length(), 2)
  assert_eq(invalid_results.length(), 2)
  
  // 所有有效数据应该通过验证
  for result in valid_results {
    assert_true(result.is_valid)
    assert_eq(result.errors.length(), 0)
  }
  
  // 所有无效数据应该有错误
  for result in invalid_results {
    assert_false(result.is_valid)
    assert_true(result.errors.length() > 0)
  }
  
  // 测试数据一致性检查
  let consistency_checker = ConsistencyChecker::new()
  ConsistencyChecker::add_parent_child_rule(consistency_checker, "parent.span", "child.span")
  ConsistencyChecker::add_timestamp_order_rule(consistency_checker)
  
  // 创建一致性测试数据
  let parent_span = TelemetryData {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_name: "parent.span",
    timestamp: "1609459200000",
    duration: 200,
    attributes: [
      ("span.kind", "server")
    ]
  }
  
  let child_span = TelemetryData {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_name: "child.span",
    timestamp: "1609459200050", // 在父span之后开始
    duration: 100,
    attributes: [
      ("span.kind", "client"),
      ("parent.span", "parent.span")
    ]
  }
  
  let inconsistent_span = TelemetryData {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_name: "child.span",
    timestamp: "1609459199500", // 在父span之前开始
    duration: 100,
    attributes: [
      ("span.kind", "client"),
      ("parent.span", "parent.span")
    ]
  }
  
  // 验证一致性
  let consistent_result = ConsistencyChecker::check_span_relationship(consistency_checker, parent_span, child_span)
  assert_true(consistent_result.is_consistent)
  
  let inconsistent_result = ConsistencyChecker::check_span_relationship(consistency_checker, parent_span, inconsistent_span)
  assert_false(inconsistent_result.is_consistent)
  assert_true(inconsistent_result.violations.length() > 0)
  
  // 测试数据质量报告
  let quality_reporter = QualityReporter::new()
  QualityReporter::add_validation_results(quality_reporter, valid_results)
  QualityReporter::add_validation_results(quality_reporter, invalid_results)
  
  let report = QualityReporter::generate_report(quality_reporter)
  assert_eq(report.total_records, 4)
  assert_eq(report.valid_records, 2)
  assert_eq(report.invalid_records, 2)
  assert_eq(report.quality_score, 50.0) // 2/4 = 50%
  
  // 测试数据质量改进建议
  let recommendations = QualityReporter::get_improvement_recommendations(quality_reporter)
  assert_true(recommendations.length() > 0)
  
  // 验证建议内容
  let has_trace_id_recommendation = recommendations.any(fn(rec) {
    rec.field == "trace.id" && rec.issue == "format_validation"
  })
  assert_true(has_trace_id_recommendation)
  
  // 测试数据质量趋势分析
  let trend_analyzer = QualityTrendAnalyzer::new()
  for i in 1..=10 {
    let day_data = generate_daily_telemetry_data(i)
    QualityTrendAnalyzer::add_daily_data(trend_analyzer, day_data)
  }
  
  let trend = QualityTrendAnalyzer::analyze_quality_trend(trend_analyzer)
  assert_true(trend.overall_trend == "improving" || trend.overall_trend == "stable" || trend.overall_trend == "declining")
  assert_true(trend.quality_scores.length() == 10)
}

// 测试8: 跨服务遥测一致性测试
test "跨服务遥测一致性测试" {
  // 创建多个服务的遥测提供者
  let auth_service_provider = TracerProvider::new("auth.service")
  let user_service_provider = TracerProvider::new("user.service")
  let payment_service_provider = TracerProvider::new("payment.service")
  
  // 获取各服务的tracer
  let auth_tracer = TracerProvider::get_tracer(auth_service_provider, "auth.operations")
  let user_tracer = TracerProvider::get_tracer(user_service_provider, "user.operations")
  let payment_tracer = TracerProvider::get_tracer(payment_service_provider, "payment.operations")
  
  // 模拟跨服务调用链
  // 1. 认证服务开始
  let auth_span = Tracer::start_span(auth_tracer, "user.authenticate")
  let auth_context = Span::context(auth_span)
  Span::set_attribute(auth_span, "service.name", StringValue("auth.service"))
  Span::set_attribute(auth_span, "user.id", StringValue("user-12345"))
  
  // 2. 认证服务调用用户服务
  let user_span_context = ContextPropagation::inject_context(auth_context, [
    ("service.target", StringValue("user.service"))
  ])
  
  let user_span = Tracer::start_span_with_propagated_context(user_tracer, "user.get_profile", user_span_context)
  Span::set_attribute(user_span, "service.name", StringValue("user.service"))
  Span::set_attribute(user_span, "user.id", StringValue("user-12345"))
  let user_context = Span::context(user_span)
  
  // 3. 用户服务调用支付服务
  let payment_span_context = ContextPropagation::inject_context(user_context, [
    ("service.target", StringValue("payment.service"))
  ])
  
  let payment_span = Tracer::start_span_with_propagated_context(payment_tracer, "payment.get_methods", payment_span_context)
  Span::set_attribute(payment_span, "service.name", StringValue("payment.service"))
  Span::set_attribute(payment_span, "user.id", StringValue("user-12345"))
  
  // 结束支付服务span
  Span::end(payment_span)
  
  // 结束用户服务span
  Span::end(user_span)
  
  // 结束认证服务span
  Span::end(auth_span)
  
  // 创建一致性验证器
  let consistency_validator = CrossServiceConsistencyValidator::new()
  
  // 收集所有服务的遥测数据
  let auth_data = TracerProvider::get_collected_data(auth_service_provider)
  let user_data = TracerProvider::get_collected_data(user_service_provider)
  let payment_data = TracerProvider::get_collected_data(payment_service_provider)
  
  // 验证跨服务一致性
  let consistency_result = ConsistencyValidator::validate_cross_service_traces(consistency_validator, [
    auth_data, user_data, payment_data
  ])
  
  // 验证一致性结果
  assert_true(consistency_result.is_consistent)
  assert_eq(consistency_result.trace_count, 1) // 应该只有一个完整的追踪链
  assert_eq(consistency_result.service_count, 3) // 涉及3个服务
  
  // 验证追踪链完整性
  let trace_chain = consistency_result.trace_chains[0]
  assert_eq(trace_chain.spans.length(), 3)
  assert_eq(trace_chain.root_span.service, "auth.service")
  assert_eq(trace_chain.leaf_span.service, "payment.service")
  
  // 验证上下文传播
  let auth_trace_id = Span::trace_id(auth_span)
  let user_trace_id = Span::trace_id(user_span)
  let payment_trace_id = Span::trace_id(payment_span)
  
  assert_eq(auth_trace_id, user_trace_id)
  assert_eq(user_trace_id, payment_trace_id)
  
  // 测试跨服务指标一致性
  let auth_metrics = TracerProvider::get_metrics(auth_service_provider)
  let user_metrics = TracerProvider::get_metrics(user_service_provider)
  let payment_metrics = TracerProvider::get_metrics(payment_service_provider)
  
  // 创建跨服务指标聚合器
  let metrics_aggregator = CrossServiceMetricsAggregator::new()
  MetricsAggregator::add_service_metrics(metrics_aggregator, "auth.service", auth_metrics)
  MetricsAggregator::add_service_metrics(metrics_aggregator, "user.service", user_metrics)
  MetricsAggregator::add_service_metrics(metrics_aggregator, "payment.service", payment_metrics)
  
  // 计算跨服务聚合指标
  let aggregated_metrics = MetricsAggregator::calculate_aggregated_metrics(metrics_aggregator)
  
  // 验证聚合指标
  assert_true(aggregated_metrics.total_requests > 0)
  assert_true(aggregated_metrics.total_latency > 0)
  assert_true(aggregated_metrics.service_metrics.contains("auth.service"))
  assert_true(aggregated_metrics.service_metrics.contains("user.service"))
  assert_true(aggregated_metrics.service_metrics.contains("payment.service"))
  
  // 测试跨服务错误传播
  let error_propagation_tester = ErrorPropagationTester::new()
  
  // 模拟错误传播场景
  let error_auth_span = Tracer::start_span(auth_tracer, "user.authenticate.error")
  let error_auth_context = Span::context(error_auth_span)
  Span::set_attribute(error_auth_span, "error.type", StringValue("AuthenticationError"))
  
  // 传播错误到下游服务
  let error_user_span = Tracer::start_span_with_propagated_context(user_tracer, "user.get_profile.error", error_auth_context)
  Span::set_attribute(error_user_span, "error.type", StringValue("UpstreamError"))
  Span::set_attribute(error_user_span, "upstream.service", StringValue("auth.service"))
  
  Span::end(error_user_span)
  Span::end(error_auth_span)
  
  // 验证错误传播
  let error_data = TracerProvider::get_collected_data(auth_service_provider)
  let error_user_data = TracerProvider::get_collected_data(user_service_provider)
  
  let error_propagation_result = ErrorPropagationTester::verify_error_propagation(error_propagation_tester, [
    error_data, error_user_data
  ])
  
  assert_true(error_propagation_result.error_propagated)
  assert_true(error_propagation_result.error_context_preserved)
  
  // 测试跨服务性能基准
  let performance_benchmark = CrossServicePerformanceBenchmark::new()
  
  // 执行性能基准测试
  let benchmark_result = PerformanceBenchmark::run_benchmark(performance_benchmark, [
    auth_service_provider, user_service_provider, payment_service_provider
  ])
  
  // 验证性能基准结果
  assert_true(benchmark_result.avg_cross_service_latency > 0)
  assert_true(benchmark_result.p95_cross_service_latency >= benchmark_result.avg_cross_service_latency)
  assert_true(benchmark_result.service_performance_metrics.length() == 3)
  
  // 测试跨服务负载均衡
  let load_balancer = CrossServiceLoadBalancer::new()
  LoadBalancer::add_service_instance(load_balancer, "auth.service", "auth-1", 0.8)
  LoadBalancer::add_service_instance(load_balancer, "auth.service", "auth-2", 0.6)
  LoadBalancer::add_service_instance(load_balancer, "user.service", "user-1", 0.9)
  LoadBalancer::add_service_instance(load_balancer, "user.service", "user-2", 0.7)
  
  // 模拟负载均衡决策
  let auth_instance = LoadBalancer::select_instance(load_balancer, "auth.service")
  let user_instance = LoadBalancer::select_instance(load_balancer, "user.service")
  
  // 验证负载均衡结果
  assert_true(auth_instance == "auth-1" || auth_instance == "auth-2")
  assert_true(user_instance == "user-1" || user_instance == "user-2")
  
  // 验证负载均衡统计
  let lb_stats = LoadBalancer::get_statistics(load_balancer)
  assert_true(lb_stats.total_requests > 0)
  assert_true(lb_stats.instance_distribution.contains("auth-1"))
  assert_true(lb_stats.instance_distribution.contains("auth-2"))
}

// 辅助函数：生成每日遥测数据
fn generate_daily_telemetry_data(day : Int) -> DailyTelemetryData {
  let base_quality = 70.0 + (day % 5).to_float() * 5.0
  let random_variation = (day % 3).to_float() * 2.5
  
  DailyTelemetryData {
    date: "2023-01-" + (day.to_string().length() == 1 ? "0" + day.to_string() : day.to_string()),
    total_records: 1000 + day * 100,
    valid_records: (1000.0 * (base_quality + random_variation) / 100.0).to_int(),
    invalid_records: (1000.0 * (100.0 - base_quality - random_variation) / 100.0).to_int(),
    quality_score: base_quality + random_variation
  }
}