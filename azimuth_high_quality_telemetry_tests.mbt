// Azimuth 高质量遥测测试用例
// 测试Azimuth遥测系统的高级功能和边界情况

// 测试1: 时间序列数据处理
test "时间序列数据处理和聚合" {
  // 创建时间序列数据收集器
  let collector = TimeSeriesCollector::new()
  
  // 添加时间序列数据点
  let timestamp1 = 1735689600000000000L  // 2025-01-02 10:00:00 UTC
  let timestamp2 = 1735689660000000000L  // 2025-01-02 10:01:00 UTC
  let timestamp3 = 1735689720000000000L  // 2025-01-02 10:02:00 UTC
  
  TimeSeriesCollector::add_point(collector, "cpu.usage", timestamp1, 45.2)
  TimeSeriesCollector::add_point(collector, "cpu.usage", timestamp2, 52.8)
  TimeSeriesCollector::add_point(collector, "cpu.usage", timestamp3, 48.1)
  
  TimeSeriesCollector::add_point(collector, "memory.usage", timestamp1, 68.5)
  TimeSeriesCollector::add_point(collector, "memory.usage", timestamp2, 72.3)
  TimeSeriesCollector::add_point(collector, "memory.usage", timestamp3, 70.7)
  
  // 测试数据聚合
  let cpu_avg = TimeSeriesCollector::average(collector, "cpu.usage", timestamp1, timestamp3)
  let memory_max = TimeSeriesCollector::maximum(collector, "memory.usage", timestamp1, timestamp3)
  let cpu_min = TimeSeriesCollector::minimum(collector, "cpu.usage", timestamp1, timestamp3)
  
  match cpu_avg {
    Some(value) => assert_true(value > 48.0 && value < 49.0)
    None => assert_true(false)
  }
  
  match memory_max {
    Some(value) => assert_true(value >= 72.0 && value <= 73.0)
    None => assert_true(false)
  }
  
  match cpu_min {
    Some(value) => assert_true(value >= 45.0 && value <= 46.0)
    None => assert_true(false)
  }
  
  // 测试时间范围查询
  let cpu_points = TimeSeriesCollector::get_points_in_range(collector, "cpu.usage", timestamp1, timestamp2)
  assert_eq(cpu_points.length(), 2)
  
  // 测试数据点插值
  let interpolated = TimeSeriesCollector::interpolate(collector, "cpu.usage", 1735689630000000000L)
  match interpolated {
    Some(value) => assert_true(value > 48.0 && value < 51.0)
    None => assert_true(false)
  }
}

// 测试2: 分布式追踪
test "分布式追踪链和上下文传播" {
  // 创建分布式追踪器
  let tracer = DistributedTracer::new("payment.service")
  
  // 创建根span
  let root_span = DistributedTracer::start_span(tracer, "payment.process")
  Span::set_attribute(root_span, "payment.amount", 150.75)
  Span::set_attribute(root_span, "payment.currency", "USD")
  
  // 创建子span - 验证
  let validation_span = DistributedTracer::start_child_span(tracer, "payment.validate", root_span)
  Span::set_attribute(validation_span, "validation.method", "card")
  Span::add_event(validation_span, "validation.started", [])
  Span::set_status(validation_span, Ok, None)
  Span::end(validation_span)
  
  // 创建子span - 处理
  let processing_span = DistributedTracer::start_child_span(tracer, "payment.processing", root_span)
  Span::set_attribute(processing_span, "processor.id", "proc-001")
  
  // 嵌套子span - 网关调用
  let gateway_span = DistributedTracer::start_child_span(tracer, "gateway.call", processing_span)
  Span::set_attribute(gateway_span, "gateway.endpoint", "https://api.payment.com/charge")
  Span::set_attribute(gateway_span, "gateway.timeout", 5000)
  Span::end(gateway_span)
  
  Span::end(processing_span)
  
  // 创建子span - 通知
  let notification_span = DistributedTracer::start_child_span(tracer, "payment.notification", root_span)
  Span::set_attribute(notification_span, "notification.channel", "email")
  Span::set_attribute(notification_span, "notification.recipient", "user@example.com")
  Span::end(notification_span)
  
  // 验证追踪链
  let trace_id = Span::trace_id(root_span)
  let validation_trace_id = Span::trace_id(validation_span)
  let processing_trace_id = Span::trace_id(processing_span)
  let gateway_trace_id = Span::trace_id(gateway_span)
  let notification_trace_id = Span::trace_id(notification_span)
  
  assert_eq(trace_id, validation_trace_id)
  assert_eq(trace_id, processing_trace_id)
  assert_eq(trace_id, gateway_trace_id)
  assert_eq(trace_id, notification_trace_id)
  
  // 验证父子关系
  let root_span_id = Span::span_id(root_span)
  let validation_parent_id = Span::parent_span_id(validation_span)
  let processing_parent_id = Span::parent_span_id(processing_span)
  let gateway_parent_id = Span::parent_span_id(gateway_span)
  let notification_parent_id = Span::parent_span_id(notification_span)
  
  assert_eq(root_span_id, validation_parent_id)
  assert_eq(root_span_id, processing_parent_id)
  assert_eq(Span::span_id(processing_span), gateway_parent_id)
  assert_eq(root_span_id, notification_parent_id)
  
  Span::end(root_span)
}

// 测试3: 性能监控
test "性能监控和资源使用追踪" {
  // 创建性能监控器
  let monitor = PerformanceMonitor::new()
  
  // 开始性能监控
  PerformanceMonitor::start_monitoring(monitor)
  
  // 模拟工作负载
  let start_time = PerformanceMonitor::current_timestamp(monitor)
  
  // 模拟CPU密集型操作
  let mut result = 0
  for i in 1..=100000 {
    result = result + i * i
  }
  assert_true(result > 0)
  
  let end_time = PerformanceMonitor::current_timestamp(monitor)
  let operation_duration = end_time - start_time
  
  // 记录性能指标
  PerformanceMonitor::record_operation_time(monitor, "calculation.heavy", operation_duration)
  PerformanceMonitor::record_memory_usage(monitor, "calculation.memory", 1024 * 1024)  // 1MB
  PerformanceMonitor::record_cpu_usage(monitor, "calculation.cpu", 85.5)
  
  // 创建多个并发操作性能测试
  let operations = ["database.query", "api.request", "file.read", "cache.lookup"]
  
  for op in operations {
    let op_start = PerformanceMonitor::current_timestamp(monitor)
    
    // 模拟不同类型的操作延迟
    let delay = match op {
      "database.query" => 150,
      "api.request" => 80,
      "file.read" => 25,
      "cache.lookup" => 5,
      _ => 10
    }
    
    // 模拟操作延迟
    let mut counter = 0
    for i in 1..=delay * 1000 {
      counter = counter + 1
    }
    
    let op_end = PerformanceMonitor::current_timestamp(monitor)
    let op_duration = op_end - op_start
    
    PerformanceMonitor::record_operation_time(monitor, op, op_duration)
    PerformanceMonitor::record_operation_success(monitor, op, true)
  }
  
  // 获取性能统计
  let avg_calc_time = PerformanceMonitor::average_operation_time(monitor, "calculation.heavy")
  let max_memory = PerformanceMonitor::max_memory_usage(monitor, "calculation.memory")
  let avg_cpu = PerformanceMonitor::average_cpu_usage(monitor, "calculation.cpu")
  
  match avg_calc_time {
    Some(time) => assert_true(time > 0)
    None => assert_true(false)
  }
  
  match max_memory {
    Some(memory) => assert_eq(memory, 1024 * 1024)
    None => assert_true(false)
  }
  
  match avg_cpu {
    Some(cpu) => assert_eq(cpu, 85.5)
    None => assert_true(false)
  }
  
  // 测试性能阈值检查
  let is_slow = PerformanceMonitor::is_operation_slow(monitor, "database.query", 100)
  let is_fast = PerformanceMonitor::is_operation_slow(monitor, "cache.lookup", 100)
  
  assert_true(is_slow)
  assert_false(is_fast)
  
  PerformanceMonitor::stop_monitoring(monitor)
}

// 测试4: 数据采样策略
test "数据采样策略和概率采样" {
  // 创建采样管理器
  let sampler = SamplingManager::new()
  
  // 测试固定概率采样
  let fixed_sampler = SamplingManager::create_fixed_sampler(sampler, 0.1)  // 10%采样率
  
  let sampled_count = 0
  let total_count = 1000
  
  for i in 1..=total_count {
    let should_sample = SamplingManager::should_sample(fixed_sampler, "trace.id." + i.to_string())
    if should_sample {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率在合理范围内 (8% - 12%)
  let actual_rate = sampled_count.to_float() / total_count.to_float()
  assert_true(actual_rate >= 0.08 && actual_rate <= 0.12)
  
  // 测试基于属性的采样
  let attribute_sampler = SamplingManager::create_attribute_sampler(sampler, [
    ("service.name", "critical.service", 1.0),  // 总是采样
    ("service.name", "normal.service", 0.1),   // 10%采样
    ("error.level", "error", 1.0),             // 错误总是采样
    ("error.level", "warning", 0.5)            // 警告50%采样
  ])
  
  // 测试关键服务总是被采样
  let critical_sampled = SamplingManager::should_sample_with_attributes(
    attribute_sampler, 
    "trace.critical", 
    [("service.name", "critical.service")]
  )
  assert_true(critical_sampled)
  
  // 测试错误总是被采样
  let error_sampled = SamplingManager::should_sample_with_attributes(
    attribute_sampler, 
    "trace.error", 
    [("error.level", "error")]
  )
  assert_true(error_sampled)
  
  // 测试自适应采样
  let adaptive_sampler = SamplingManager::create_adaptive_sampler(sampler, 100)  // 目标100个采样/秒
  
  // 模拟高流量
  let high_traffic_sampled = 0
  for i in 1..=1000 {
    if SamplingManager::should_sample(adaptive_sampler, "high.traffic." + i.to_string()) {
      high_traffic_sampled = high_traffic_sampled + 1
    }
  }
  
  // 高流量时采样率应该降低
  assert_true(high_traffic_sampled < 200)
  
  // 测试采样决策缓存
  let cached_decision = SamplingManager::get_cached_decision(fixed_sampler, "cached.trace.id")
  match cached_decision {
    None => {
      // 第一次采样决策
      let decision = SamplingManager::should_sample(fixed_sampler, "cached.trace.id")
      let cached_decision2 = SamplingManager::get_cached_decision(fixed_sampler, "cached.trace.id")
      match cached_decision2 {
        Some(cached) => assert_eq(cached, decision)
        None => assert_true(false)
      }
    }
    Some(_) => assert_true(true)
  }
}

// 测试5: 配置管理
test "动态配置管理和热更新" {
  // 创建配置管理器
  let config_manager = ConfigurationManager::new()
  
  // 设置初始配置
  ConfigurationManager::set_value(config_manager, "telemetry.enabled", true)
  ConfigurationManager::set_value(config_manager, "telemetry.sampling.rate", 0.1)
  ConfigurationManager::set_value(config_manager, "telemetry.batch.size", 100)
  ConfigurationManager::set_value(config_manager, "telemetry.export.interval", 5000)
  
  // 验证配置值
  let telemetry_enabled = ConfigurationManager::get_bool(config_manager, "telemetry.enabled")
  let sampling_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling.rate")
  let batch_size = ConfigurationManager::get_int(config_manager, "telemetry.batch.size")
  let export_interval = ConfigurationManager::get_int(config_manager, "telemetry.export.interval")
  
  assert_eq(telemetry_enabled, Some(true))
  assert_eq(sampling_rate, Some(0.1))
  assert_eq(batch_size, Some(100))
  assert_eq(export_interval, Some(5000))
  
  // 测试默认值
  let missing_bool = ConfigurationManager::get_bool_with_default(config_manager, "missing.bool", false)
  let missing_float = ConfigurationManager::get_float_with_default(config_manager, "missing.float", 0.5)
  let missing_int = ConfigurationManager::get_int_with_default(config_manager, "missing.int", 42)
  let missing_string = ConfigurationManager::get_string_with_default(config_manager, "missing.string", "default")
  
  assert_eq(missing_bool, false)
  assert_eq(missing_float, 0.5)
  assert_eq(missing_int, 42)
  assert_eq(missing_string, "default")
  
  // 测试配置变更监听
  let change_received = ref false
  ConfigurationManager::add_change_listener(config_manager, "telemetry.sampling.rate", fn(old_val, new_val) {
    change_received := true
    assert_eq(old_val, "0.1")
    assert_eq(new_val, "0.2")
  })
  
  // 更新配置
  ConfigurationManager::set_value(config_manager, "telemetry.sampling.rate", 0.2)
  assert_true(change_received)
  
  // 验证更新后的值
  let updated_rate = ConfigurationManager::get_float(config_manager, "telemetry.sampling.rate")
  assert_eq(updated_rate, Some(0.2))
  
  // 测试配置组和命名空间
  ConfigurationManager::set_value_with_namespace(config_manager, "service", "database", "connection.pool.size", 10)
  ConfigurationManager::set_value_with_namespace(config_manager, "service", "cache", "connection.pool.size", 20)
  
  let db_pool_size = ConfigurationManager::get_int_with_namespace(config_manager, "service", "database", "connection.pool.size")
  let cache_pool_size = ConfigurationManager::get_int_with_namespace(config_manager, "service", "cache", "connection.pool.size")
  
  assert_eq(db_pool_size, Some(10))
  assert_eq(cache_pool_size, Some(20))
  
  // 测试配置验证
  let validation_result = ConfigurationManager::validate_value(config_manager, "telemetry.sampling.rate", 1.5)
  assert_false(validation_result.is_valid)
  assert_eq(validation_result.error_message, Some("Sampling rate must be between 0.0 and 1.0"))
  
  let valid_result = ConfigurationManager::validate_value(config_manager, "telemetry.sampling.rate", 0.8)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.error_message, None)
  
  // 测试配置导出和导入
  let exported_config = ConfigurationManager::export_config(config_manager)
  assert_true(exported_config.contains("telemetry.enabled"))
  assert_true(exported_config.contains("telemetry.sampling.rate"))
  
  let new_config_manager = ConfigurationManager::new()
  ConfigurationManager::import_config(new_config_manager, exported_config)
  
  let imported_enabled = ConfigurationManager::get_bool(new_config_manager, "telemetry.enabled")
  let imported_rate = ConfigurationManager::get_float(new_config_manager, "telemetry.sampling.rate")
  
  assert_eq(imported_enabled, Some(true))
  assert_eq(imported_rate, Some(0.2))
}

// 测试6: 数据序列化
test "遥测数据序列化和格式转换" {
  // 创建序列化管理器
  let serializer = SerializationManager::new()
  
  // 创建测试span
  let span_data = SpanData::new(
    "test.operation",
    "trace-123",
    "span-456",
    "parent-789",
    1735689600000000000L,
    1735689605000000000L
  )
  
  SpanData::add_attribute(span_data, "service.name", "test.service")
  SpanData::add_attribute(span_data, "operation.type", "test")
  SpanData::add_attribute(span_data, "user.id", "user-123")
  
  SpanData::add_event(span_data, "operation.started", 1735689601000000000L, [("status", "starting")])
  SpanData::add_event(span_data, "operation.completed", 1735689604000000000L, [("status", "success")])
  
  // 测试JSON序列化
  let json_data = SerializationManager::to_json(serializer, span_data)
  assert_true(json_data.contains("test.operation"))
  assert_true(json_data.contains("trace-123"))
  assert_true(json_data.contains("test.service"))
  
  // 测试JSON反序列化
  let deserialized_span = SerializationManager::span_from_json(serializer, json_data)
  assert_eq(SpanData::name(deserialized_span), "test.operation")
  assert_eq(SpanData::trace_id(deserialized_span), "trace-123")
  assert_eq(SpanData::span_id(deserialized_span), "span-456")
  
  // 测试二进制序列化
  let binary_data = SerializationManager::to_binary(serializer, span_data)
  assert_true(binary_data.length() > 0)
  
  // 测试二进制反序列化
  let binary_deserialized = SerializationManager::span_from_binary(serializer, binary_data)
  assert_eq(SpanData::name(binary_deserialized), "test.operation")
  assert_eq(SpanData::trace_id(binary_deserialized), "trace-123")
  
  // 创建测试度量数据
  let metric_data = MetricData::new_counter("requests.total", 150.0)
  MetricData::add_attribute(metric_data, "method", "GET")
  MetricData::add_attribute(metric_data, "status", "200")
  MetricData::add_attribute(metric_data, "endpoint", "/api/users")
  
  // 测试度量数据序列化
  let metric_json = SerializationManager::metric_to_json(serializer, metric_data)
  assert_true(metric_json.contains("requests.total"))
  assert_true(metric_json.contains("150.0"))
  assert_true(metric_json.contains("GET"))
  
  // 测试度量数据反序列化
  let deserialized_metric = SerializationManager::metric_from_json(serializer, metric_json)
  assert_eq(MetricData::name(deserialized_metric), "requests.total")
  assert_eq(MetricData::value(deserialized_metric), 150.0)
  
  // 创建测试日志数据
  let log_data = LogData::new(1735689602000000000L, Error, "Database connection failed")
  LogData::add_attribute(log_data, "service.name", "auth.service")
  LogData::add_attribute(log_data, "error.code", "DB_CONN_001")
  LogData::add_attribute(log_data, "retry.count", 3)
  
  // 测试日志数据序列化
  let log_json = SerializationManager::log_to_json(serializer, log_data)
  assert_true(log_json.contains("Database connection failed"))
  assert_true(log_json.contains("auth.service"))
  assert_true(log_json.contains("DB_CONN_001"))
  
  // 测试日志数据反序列化
  let deserialized_log = SerializationManager::log_from_json(serializer, log_json)
  assert_eq(LogData::severity(deserialized_log), Error)
  assert_eq(LogData::body(deserialized_log), "Database connection failed")
  
  // 测试批量序列化
  let batch_data = BatchData::new()
  BatchData::add_span(batch_data, span_data)
  BatchData::add_metric(batch_data, metric_data)
  BatchData::add_log(batch_data, log_data)
  
  let batch_json = SerializationManager::batch_to_json(serializer, batch_data)
  assert_true(batch_json.contains("test.operation"))
  assert_true(batch_json.contains("requests.total"))
  assert_true(batch_json.contains("Database connection failed"))
  
  // 测试压缩序列化
  let compressed_data = SerializationManager::to_compressed_json(serializer, batch_data)
  assert_true(compressed_data.length() < batch_json.length())
  
  // 测试解压缩和反序列化
  let decompressed_batch = SerializationManager::batch_from_compressed_json(serializer, compressed_data)
  assert_eq(BatchData::span_count(decompressed_batch), 1)
  assert_eq(BatchData::metric_count(decompressed_batch), 1)
  assert_eq(BatchData::log_count(decompressed_batch), 1)
}

// 测试7: 缓存机制
test "遥测数据缓存和内存管理" {
  // 创建缓存管理器
  let cache = CacheManager::new(1000)  // 最大1000项
  
  // 测试基本缓存操作
  let span_key = "span:trace-123:span-456"
  let span_data = SpanData::new("test.operation", "trace-123", "span-456", "", 0L, 0L)
  
  CacheManager::put(cache, span_key, span_data)
  
  let cached_span = CacheManager::get(cache, span_key)
  match cached_span {
    Some(span) => {
      assert_eq(SpanData::name(span), "test.operation")
      assert_eq(SpanData::trace_id(span), "trace-123")
    }
    None => assert_true(false)
  }
  
  // 测试缓存不存在的键
  let missing = CacheManager::get(cache, "nonexistent:key")
  assert_eq(missing, None)
  
  // 测试缓存更新
  let updated_span = SpanData::new("updated.operation", "trace-123", "span-456", "", 0L, 0L)
  CacheManager::put(cache, span_key, updated_span)
  
  let cached_updated = CacheManager::get(cache, span_key)
  match cached_updated {
    Some(span) => assert_eq(SpanData::name(span), "updated.operation")
    None => assert_true(false)
  }
  
  // 测试缓存删除
  CacheManager::remove(cache, span_key)
  let after_removal = CacheManager::get(cache, span_key)
  assert_eq(after_removal, None)
  
  // 测试缓存大小限制
  for i in 1..=1200 {
    let key = "span:trace-" + i.to_string() + ":span-" + i.to_string()
    let data = SpanData::new("operation." + i.to_string(), "trace-" + i.to_string(), "span-" + i.to_string(), "", 0L, 0L)
    CacheManager::put(cache, key, data)
  }
  
  // 缓存大小应该不超过最大限制
  assert_true(CacheManager::size(cache) <= 1000)
  
  // 测试LRU淘汰策略
  let first_key = "span:trace-1:span-1"
  let first_span = CacheManager::get(cache, first_key)
  assert_eq(first_span, None)  // 应该已经被淘汰
  
  // 测试TTL过期
  let ttl_cache = CacheManager::new_with_ttl(100, 1000)  // 100项，1秒TTL
  
  let ttl_key = "span:ttl:test"
  let ttl_data = SpanData::new("ttl.operation", "ttl-trace", "ttl-span", "", 0L, 0L)
  CacheManager::put(ttl_cache, ttl_key, ttl_data)
  
  let immediate_get = CacheManager::get(ttl_cache, ttl_key)
  assert_ne(immediate_get, None)  // 应该能立即获取
  
  // 模拟时间流逝（在实际实现中需要时间控制）
  // 这里仅演示API使用
  
  // 测试缓存统计
  let stats = CacheManager::get_stats(cache)
  assert_true(stats.hits >= 0)
  assert_true(stats.misses >= 0)
  assert_true(stats.evictions >= 0)
  
  // 测试缓存预热
  let warmup_data = [
    ("span:warmup:1", SpanData::new("warmup.op1", "trace-1", "span-1", "", 0L, 0L)),
    ("span:warmup:2", SpanData::new("warmup.op2", "trace-2", "span-2", "", 0L, 0L)),
    ("span:warmup:3", SpanData::new("warmup.op3", "trace-3", "span-3", "", 0L, 0L))
  ]
  
  CacheManager::warmup(cache, warmup_data)
  
  for (key, data) in warmup_data {
    let cached = CacheManager::get(cache, key)
    match cached {
      Some(_) => assert_true(true)
      None => assert_true(false)
    }
  }
  
  // 测试缓存清空
  CacheManager::clear(cache)
  assert_eq(CacheManager::size(cache), 0)
  
  let after_clear = CacheManager::get(cache, "span:warmup:1")
  assert_eq(after_clear, None)
}

// 测试8: 异常恢复
test "异常恢复和故障容错机制" {
  // 创建恢复管理器
  let recovery = RecoveryManager::new()
  
  // 测试重试机制
  let mut attempt_count = 0
  let operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  }
  
  let result = RecoveryManager::execute_with_retry(recovery, operation, 3, 100)
  match result {
    Ok(value) => assert_eq(value, "Success after retries")
    Err(_) => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
  
  // 测试重试次数耗尽
  attempt_count = 0
  let always_fail_operation = fn() {
    attempt_count = attempt_count + 1
    Error("Persistent failure")
  }
  
  let failed_result = RecoveryManager::execute_with_retry(recovery, always_fail_operation, 2, 50)
  match failed_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Persistent failure")
  }
  
  assert_eq(attempt_count, 2)
  
  // 测试断路器模式
  let circuit_breaker = RecoveryManager::create_circuit_breaker(recovery, "test.service", 3, 5000)
  
  // 初始状态应该是关闭的
  assert_eq(RecoveryManager::circuit_state(circuit_breaker), "closed")
  
  // 模拟失败调用
  for i in 1..=4 {
    let circuit_result = RecoveryManager::execute_through_circuit(circuit_breaker, fn() {
      Error("Service unavailable")
    })
    match circuit_result {
      Ok(_) => assert_true(false)
      Err(_) => assert_true(true)
    }
  }
  
  // 达到失败阈值后，断路器应该打开
  assert_eq(RecoveryManager::circuit_state(circuit_breaker), "open")
  
  // 断路器打开时，调用应该立即失败
  let open_circuit_result = RecoveryManager::execute_through_circuit(circuit_breaker, fn() {
    Ok("Should not be called")
  })
  match open_circuit_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Circuit breaker is open")
  }
  
  // 测试降级机制
  let fallback_result = RecoveryManager::execute_with_fallback(
    fn() { Error("Primary service failed") },
    fn() { Ok("Fallback response") }
  )
  match fallback_result {
    Ok(value) => assert_eq(value, "Fallback response")
    Err(_) => assert_true(false)
  }
  
  // 测试超时处理
  let slow_operation = fn() {
    // 模拟慢操作
    let mut counter = 0
    for i in 1..=1000000 {
      counter = counter + 1
    }
    Ok("Slow operation completed")
  }
  
  let timeout_result = RecoveryManager::execute_with_timeout(recovery, slow_operation, 100)
  match timeout_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Operation timed out")
  }
  
  // 测试批量操作的异常恢复
  let batch_operations = [
    fn() { Ok("Operation 1 success") },
    fn() { Error("Operation 2 failed") },
    fn() { Ok("Operation 3 success") },
    fn() { Error("Operation 4 failed") },
    fn() { Ok("Operation 5 success") }
  ]
  
  let batch_results = RecoveryManager::execute_batch_with_recovery(recovery, batch_operations, 2)
  assert_eq(batch_results.length(), 5)
  assert_eq(batch_results[0], Ok("Operation 1 success"))
  assert_eq(batch_results[1], Err("Operation 2 failed"))
  assert_eq(batch_results[2], Ok("Operation 3 success"))
  assert_eq(batch_results[3], Err("Operation 4 failed"))
  assert_eq(batch_results[4], Ok("Operation 5 success"))
  
  // 测试健康检查
  let healthy_service = fn() { Ok(true) }
  let unhealthy_service = fn() { Error("Service down") }
  
  let health_check1 = RecoveryManager::health_check(recovery, healthy_service)
  let health_check2 = RecoveryManager::health_check(recovery, unhealthy_service)
  
  assert_true(health_check1)
  assert_false(health_check2)
}

// 测试9: 资源管理
test "资源管理和内存优化" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 测试资源池
  let connection_pool = ResourceManager::create_pool(resource_manager, "database.connections", 10, fn() {
    DatabaseConnection::new("localhost", 5432, "testdb")
  })
  
  // 从池中获取连接
  let conn1 = ResourceManager::acquire_resource(connection_pool)
  let conn2 = ResourceManager::acquire_resource(connection_pool)
  let conn3 = ResourceManager::acquire_resource(connection_pool)
  
  assert_ne(conn1, None)
  assert_ne(conn2, None)
  assert_ne(conn3, None)
  
  // 验证池状态
  let pool_stats = ResourceManager::pool_stats(connection_pool)
  assert_eq(pool_stats.active, 3)
  assert_eq(pool_stats.available, 7)
  
  // 释放资源回池
  ResourceManager::release_resource(connection_pool, conn1)
  ResourceManager::release_resource(connection_pool, conn2)
  
  let after_release_stats = ResourceManager::pool_stats(connection_pool)
  assert_eq(after_release_stats.active, 1)
  assert_eq(after_release_stats.available, 9)
  
  // 测试内存使用监控
  let memory_monitor = ResourceManager::create_memory_monitor(resource_manager)
  
  let initial_memory = ResourceManager::get_memory_usage(memory_monitor)
  
  // 分配一些内存
  let large_array = []
  for i in 1..=10000 {
    large_array = large_array.push(i * i)
  }
  
  let after_allocation = ResourceManager::get_memory_usage(memory_monitor)
  assert_true(after_allocation > initial_memory)
  
  // 清理数组
  let large_array = []
  
  let after_cleanup = ResourceManager::get_memory_usage(memory_monitor)
  assert_true(after_cleanup <= after_allocation)
  
  // 测试垃圾回收
  let gc_result = ResourceManager::force_gc(resource_manager)
  assert_true(gc_result.success)
  assert_true(gc_result.collected_bytes > 0)
  
  // 测试资源限制
  let limited_pool = ResourceManager::create_limited_pool(resource_manager, "limited.resources", 2, fn() {
    ExpensiveResource::new()
  })
  
  let res1 = ResourceManager::acquire_resource(limited_pool)
  let res2 = ResourceManager::acquire_resource(limited_pool)
  let res3 = ResourceManager::acquire_resource(limited_pool)  // 应该失败
  
  assert_ne(res1, None)
  assert_ne(res2, None)
  assert_eq(res3, None)
  
  // 测试资源自动清理
  let auto_cleanup_manager = ResourceManager::create_auto_cleanup_manager(resource_manager, 1000)
  
  let cleanup_resource = ResourceManager::register_for_cleanup(auto_cleanup_manager, fn() {
    // 清理逻辑
    true
  })
  
  assert_ne(cleanup_resource, None)
  
  // 测试资源泄漏检测
  let leak_detector = ResourceManager::create_leak_detector(resource_manager)
  
  // 模拟资源泄漏
  let leaked_resource = ResourceManager::acquire_resource(connection_pool)
  // 故意不释放
  
  let leak_report = ResourceManager::detect_leaks(leak_detector)
  assert_true(leak_report.leaks_detected > 0)
  assert_true(leak_report.leaked_resources.contains("database.connections"))
  
  // 测试资源使用统计
  let resource_stats = ResourceManager::get_resource_stats(resource_manager)
  assert_true(resource_stats.total_pools > 0)
  assert_true(resource_stats.total_resources > 0)
  assert_true(resource_stats.memory_usage > 0)
  
  // 测试资源预热
  ResourceManager::warmup_pool(connection_pool, 5)
  let warmup_stats = ResourceManager::pool_stats(connection_pool)
  assert_true(warmup_stats.available >= 5)
  
  // 清理资源
  ResourceManager::release_resource(connection_pool, conn3)
  ResourceManager::release_resource(connection_pool, leaked_resource)
}

// 测试10: 数据完整性
test "数据完整性和一致性验证" {
  // 创建完整性验证器
  let integrity_validator = IntegrityValidator::new()
  
  // 创建测试数据
  let trace_data = TraceData::new("trace-123", "service.test")
  TraceData::add_span(trace_data, SpanData::new("span-1", "trace-123", "span-1", "", 1000L, 2000L))
  TraceData::add_span(trace_data, SpanData::new("span-2", "trace-123", "span-2", "span-1", 1500L, 2500L))
  TraceData::add_span(trace_data, SpanData::new("span-3", "trace-123", "span-3", "span-1", 1600L, 2300L))
  
  // 测试基本完整性验证
  let validation_result = IntegrityValidator::validate_trace(integrity_validator, trace_data)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试时间顺序验证
  let out_of_order_trace = TraceData::new("trace-456", "service.test")
  TraceData::add_span(out_of_order_trace, SpanData::new("span-1", "trace-456", "span-1", "", 2000L, 3000L))
  TraceData::add_span(out_of_order_trace, SpanData::new("span-2", "trace-456", "span-2", "span-1", 1500L, 2500L))  // 开始时间早于父span
  
  let time_validation = IntegrityValidator::validate_trace(integrity_validator, out_of_order_trace)
  assert_false(time_validation.is_valid)
  assert_true(time_validation.errors.length() > 0)
  
  // 测试父子关系验证
  let invalid_parent_trace = TraceData::new("trace-789", "service.test")
  TraceData::add_span(invalid_parent_trace, SpanData::new("span-1", "trace-789", "span-1", "", 1000L, 2000L))
  TraceData::add_span(invalid_parent_trace, SpanData::new("span-2", "trace-789", "span-2", "nonexistent-parent", 1500L, 2500L))
  
  let parent_validation = IntegrityValidator::validate_trace(integrity_validator, invalid_parent_trace)
  assert_false(parent_validation.is_valid)
  assert_true(parent_validation.errors.contains("Invalid parent reference"))
  
  // 测试数据校验和
  let checksum = IntegrityValidator::calculate_checksum(integrity_validator, trace_data)
  assert_true(checksum.length() > 0)
  
  // 修改数据后校验和应该变化
  TraceData::add_span(trace_data, SpanData::new("span-4", "trace-123", "span-4", "", 3000L, 4000L))
  let modified_checksum = IntegrityValidator::calculate_checksum(integrity_validator, trace_data)
  assert_ne(checksum, modified_checksum)
  
  // 测试数据一致性验证
  let metric_data = MetricData::new_gauge("cpu.usage", 75.5)
  MetricData::add_attribute(metric_data, "host.name", "server-01")
  MetricData::add_attribute(metric_data, "region", "us-west-2")
  
  let metric_validation = IntegrityValidator::validate_metric(integrity_validator, metric_data)
  assert_true(metric_validation.is_valid)
  
  // 测试无效度量值
  let invalid_metric = MetricData::new_gauge("invalid.metric", -10.0)  // 负的CPU使用率
  MetricData::add_attribute(invalid_metric, "metric.type", "cpu.usage")
  
  let invalid_metric_validation = IntegrityValidator::validate_metric(integrity_validator, invalid_metric)
  assert_false(invalid_metric_validation.is_valid)
  
  // 测试日志完整性
  let log_data = LogData::new(1735689600000000000L, Info, "Test log message")
  LogData::add_attribute(log_data, "service.name", "test.service")
  LogData::add_attribute(log_data, "trace.id", "trace-123")
  
  let log_validation = IntegrityValidator::validate_log(integrity_validator, log_data)
  assert_true(log_validation.is_valid)
  
  // 测试缺少必要字段的日志
  let incomplete_log = LogData::new(0L, Info, "")  // 缺少时间戳和消息
  let incomplete_validation = IntegrityValidator::validate_log(integrity_validator, incomplete_log)
  assert_false(incomplete_validation.is_valid)
  
  // 测试批量数据完整性
  let batch_data = BatchData::new()
  BatchData::add_trace(batch_data, trace_data)
  BatchData::add_metric(batch_data, metric_data)
  BatchData::add_log(batch_data, log_data)
  
  let batch_validation = IntegrityValidator::validate_batch(integrity_validator, batch_data)
  assert_true(batch_validation.is_valid)
  
  // 添加无效数据到批次
  BatchData::add_metric(batch_data, invalid_metric)
  let invalid_batch_validation = IntegrityValidator::validate_batch(integrity_validator, batch_data)
  assert_false(invalid_batch_validation.is_valid)
  
  // 测试数据修复
  let repaired_batch = IntegrityValidator::repair_batch(integrity_validator, batch_data)
  let repair_validation = IntegrityValidator::validate_batch(integrity_validator, repaired_batch)
  assert_true(repair_validation.is_valid)
  
  // 测试数据去重
  let duplicate_trace = TraceData::new("trace-123", "service.test")  // 相同的trace ID
  TraceData::add_span(duplicate_trace, SpanData::new("span-1", "trace-123", "span-1", "", 1000L, 2000L))
  
  let deduplication_result = IntegrityValidator::deduplicate_traces(integrity_validator, [trace_data, duplicate_trace])
  assert_eq(deduplication_result.length(), 1)  // 应该只保留一个
  
  // 测试数据版本控制
  let versioned_data = IntegrityValidator::version_data(integrity_validator, trace_data, "1.0.0")
  let version = IntegrityValidator::get_data_version(integrity_validator, versioned_data)
  assert_eq(version, "1.0.0")
  
  // 测试数据迁移
  let migrated_data = IntegrityValidator::migrate_data(integrity_validator, versioned_data, "1.0.0", "2.0.0")
  let migrated_version = IntegrityValidator::get_data_version(integrity_validator, migrated_data)
  assert_eq(migrated_version, "2.0.0")
  
  // 验证迁移后的数据完整性
  let migration_validation = IntegrityValidator::validate_trace(integrity_validator, migrated_data)
  assert_true(migration_validation.is_valid)
}