// Azimuth Enhanced Integration Test Suite
// 增强集成测试套件 - 包含高级集成和边界测试用例

// Test 1: 遥测数据聚合测试
test "telemetry data aggregation" {
  // 创建多个遥测数据点
  let data_points = [
    @azimuth.TelemetryPoint {
      metric_name : "response.time",
      value : 120.5,
      timestamp : @azimuth.current_timestamp(),
      attributes : [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.TelemetryPoint {
      metric_name : "response.time",
      value : 85.3,
      timestamp : @azimuth.current_timestamp(),
      attributes : [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.TelemetryPoint {
      metric_name : "response.time",
      value : 200.1,
      timestamp : @azimuth.current_timestamp(),
      attributes : [("endpoint", "/api/orders"), ("method", "POST")]
    }
  ]
  
  // 测试数据聚合
  let aggregator = @azimuth.Aggregator.new()
  let aggregated = @azimuth.aggregate_data(aggregator, data_points)
  
  // 验证聚合结果
  assert_eq(aggregated.count, 3)
  assert_true(aggregated.average > 80.0)
  assert_true(aggregated.average < 201.0)
  assert_eq(aggregated.min, 85.3)
  assert_eq(aggregated.max, 200.1)
  
  // 测试按属性分组聚合
  let grouped = @azimuth.aggregate_by_attributes(aggregator, data_points, ["endpoint", "method"])
  assert_eq(grouped.length(), 2)
  
  // 验证特定分组的聚合结果
  let users_group = grouped.find(fn(group) { 
    group.attributes.contains(("endpoint", "/api/users")) 
  })
  match users_group {
    Some(group) => {
      assert_eq(group.count, 2)
      assert_eq(group.average, (120.5 + 85.3) / 2.0)
    }
    None => assert_true(false)
  }
}

// Test 2: 分布式追踪链路测试
test "distributed tracing chain" {
  // 创建根span
  let tracer = @azimuth.Tracer.new("distributed.test")
  let root_span = @azimuth.Tracer.start_span(tracer, "root.operation")
  
  // 设置根span属性
  @azimuth.Span.set_attribute(root_span, "service.name", "api-gateway")
  @azimuth.Span.set_attribute(root_span, "operation.type", "request")
  
  // 创建子span
  let child_span1 = @azimuth.Tracer.start_span_with_parent(tracer, "service.call", root_span)
  @azimuth.Span.set_attribute(child_span1, "service.name", "user-service")
  @azimuth.Span.set_attribute(child_span1, "endpoint", "/validate")
  
  let child_span2 = @azimuth.Tracer.start_span_with_parent(tracer, "database.query", root_span)
  @azimuth.Span.set_attribute(child_span2, "service.name", "database")
  @azimuth.Span.set_attribute(child_span2, "query.type", "SELECT")
  
  // 创建嵌套子span
  let nested_span = @azimuth.Tracer.start_span_with_parent(tracer, "cache.check", child_span1)
  @azimuth.Span.set_attribute(nested_span, "cache.hit", false)
  
  // 验证span关系
  let root_ctx = @azimuth.Span.context(root_span)
  let child1_ctx = @azimuth.Span.context(child_span1)
  let child2_ctx = @azimuth.Span.context(child_span2)
  let nested_ctx = @azimuth.Span.context(nested_span)
  
  assert_eq(@azimuth.SpanContext.trace_id(root_ctx), @azimuth.SpanContext.trace_id(child1_ctx))
  assert_eq(@azimuth.SpanContext.trace_id(root_ctx), @azimuth.SpanContext.trace_id(child2_ctx))
  assert_eq(@azimuth.SpanContext.trace_id(root_ctx), @azimuth.SpanContext.trace_id(nested_ctx))
  
  assert_eq(@azimuth.SpanContext.parent_span_id(child1_ctx), @azimuth.SpanContext.span_id(root_ctx))
  assert_eq(@azimuth.SpanContext.parent_span_id(child2_ctx), @azimuth.SpanContext.span_id(root_ctx))
  assert_eq(@azimuth.SpanContext.parent_span_id(nested_ctx), @azimuth.SpanContext.span_id(child_span1))
  
  // 结束所有span（按正确顺序）
  @azimuth.Span.end(nested_span)
  @azimuth.Span.end(child_span1)
  @azimuth.Span.end(child_span2)
  @azimuth.Span.end(root_span)
  
  // 验证追踪链完整性
  let trace_chain = @azimuth.get_trace_chain(root_ctx)
  assert_eq(trace_chain.length(), 4)
  assert_true(trace_chain.any(fn(span) { @azimuth.Span.name(span) == "root.operation" }))
  assert_true(trace_chain.any(fn(span) { @azimuth.Span.name(span) == "service.call" }))
  assert_true(trace_chain.any(fn(span) { @azimuth.Span.name(span) == "database.query" }))
  assert_true(trace_chain.any(fn(span) { @azimuth.Span.name(span) == "cache.check" }))
}

// Test 3: 自定义度量指标测试
test "custom metrics and gauges" {
  // 创建度量提供者
  let meter = @azimuth.Meter.new("custom.metrics.test")
  
  // 创建自定义计数器
  let request_counter = @azimuth.Meter.create_counter(meter, "custom.requests.total")
  @azimuth.Counter.add(request_counter, 1.0, [("method", "GET"), ("status", "200")])
  @azimuth.Counter.add(request_counter, 1.0, [("method", "POST"), ("status", "201")])
  @azimuth.Counter.add(request_counter, 1.0, [("method", "GET"), ("status", "404")])
  
  // 创建自定义仪表盘
  let memory_gauge = @azimuth.Meter.create_gauge(meter, "custom.memory.usage")
  @azimuth.Gauge.set(memory_gauge, 1024.5, [("component", "cache")])
  @azimuth.Gauge.set(memory_gauge, 2048.7, [("component", "database")])
  @azimuth.Gauge.set(memory_gauge, 512.3, [("component", "queue")])
  
  // 创建自定义直方图
  let latency_histogram = @azimuth.Meter.create_histogram(meter, "custom.latency.distribution")
  @azimuth.Histogram.record(latency_histogram, 45.2, [("service", "auth")])
  @azimuth.Histogram.record(latency_histogram, 123.7, [("service", "auth")])
  @azimuth.Histogram.record(latency_histogram, 78.9, [("service", "payment")])
  @azimuth.Histogram.record(latency_histogram, 256.1, [("service", "payment")])
  
  // 验证度量指标
  let metrics_snapshot = @azimuth.Meter.get_metrics_snapshot(meter)
  
  // 验证计数器
  let counter_metrics = metrics_snapshot.filter(fn(m) { m.name == "custom.requests.total" })
  assert_eq(counter_metrics.length(), 1)
  let counter_data = counter_metrics[0]
  assert_eq(counter_data.total_count, 3.0)
  
  // 验证仪表盘
  let gauge_metrics = metrics_snapshot.filter(fn(m) { m.name == "custom.memory.usage" })
  assert_eq(gauge_metrics.length(), 1)
  let gauge_data = gauge_metrics[0]
  assert_eq(gauge_data.measurements.length(), 3)
  
  // 验证直方图
  let histogram_metrics = metrics_snapshot.filter(fn(m) { m.name == "custom.latency.distribution" })
  assert_eq(histogram_metrics.length(), 1)
  let histogram_data = histogram_metrics[0]
  assert_eq(histogram_data.sample_count, 4)
  assert_true(histogram_data.sum > 300.0)
}

// Test 4: 遥测数据导出测试
test "telemetry data export" {
  // 创建导出器配置
  let exporter_config = @azimuth.ExporterConfig {
    endpoint : "https://telemetry.example.com/api/v1/traces",
    headers : [
      ("Authorization", "Bearer test-token-123"),
      ("Content-Type", "application/json")
    ],
    batch_size : 100,
    timeout_ms : 5000,
    retry_count : 3
  }
  
  // 创建导出器
  let exporter = @azimuth.Exporter.new(exporter_config)
  
  // 创建测试遥测数据
  let test_span = @azimuth.TestSpan.new("export.test.operation")
  @azimuth.TestSpan.set_attribute(test_span, "test.type", "export")
  @azimuth.TestSpan.set_attribute(test_span, "export.batch", "1")
  
  let test_metric = @azimuth.TestMetric.new("export.test.metric", 42.5)
  @azimuth.TestMetric.set_attribute(test_metric, "metric.type", "gauge")
  
  let test_log = @azimuth.TestLog.new("Export test log entry", @azimuth.LogLevel.Info)
  @azimuth.TestLog.set_attribute(test_log, "log.category", "export")
  
  // 测试批量导出
  let export_batch = @azimuth.ExportBatch.new()
  @azimuth.ExportBatch.add_span(export_batch, test_span)
  @azimuth.ExportBatch.add_metric(export_batch, test_metric)
  @azimuth.ExportBatch.add_log(export_batch, test_log)
  
  // 执行导出
  let export_result = @azimuth.Exporter.export(exporter, export_batch)
  
  // 验证导出结果
  assert_true(export_result.success)
  assert_eq(export_result.exported_spans, 1)
  assert_eq(export_result.exported_metrics, 1)
  assert_eq(export_result.exported_logs, 1)
  assert_true(export_result.duration_ms < 1000)
  
  // 测试导出失败重试机制
  let failing_exporter = @azimuth.Exporter.new(@azimuth.ExporterConfig {
    endpoint : "https://invalid-endpoint.example.com/api/v1/traces",
    headers : [],
    batch_size : 100,
    timeout_ms : 1000,
    retry_count : 2
  })
  
  let retry_result = @azimuth.Exporter.export_with_retry(failing_exporter, export_batch)
  assert_false(retry_result.success)
  assert_eq(retry_result.attempts, 2)
  assert_true(retry_result.last_error.contains("connection"))
}

// Test 5: 配置热更新测试
test "configuration hot reload" {
  // 创建初始配置
  let initial_config = @azimuth.TelemetryConfig {
    service_name : "azimuth-service",
    service_version : "1.0.0",
    enabled : true,
    sampling_rate : 1.0,
    batch_size : 100,
    export_interval_ms : 5000,
    attributes : [
      ("environment", "production"),
      ("region", "us-west-2")
    ]
  }
  
  // 创建配置管理器
  let config_manager = @azimuth.ConfigManager.new(initial_config)
  
  // 验证初始配置
  let current_config = @azimuth.ConfigManager.get_config(config_manager)
  assert_eq(current_config.service_name, "azimuth-service")
  assert_eq(current_config.sampling_rate, 1.0)
  assert_eq(current_config.batch_size, 100)
  
  // 测试配置更新
  let updated_config = @azimuth.ConfigManager.update_config(config_manager, "sampling_rate", 0.5)
  match updated_config {
    Some(config) => {
      assert_eq(config.sampling_rate, 0.5)
      assert_eq(config.service_name, "azimuth-service") // 其他配置保持不变
    }
    None => assert_true(false)
  }
  
  // 测试批量配置更新
  let config_updates = [
    ("service_version", "1.1.0"),
    ("batch_size", 200),
    ("export_interval_ms", 10000)
  ]
  
  let batch_updated = @azimuth.ConfigManager.batch_update_config(config_manager, config_updates)
  assert_true(batch_updated)
  
  // 验证批量更新结果
  let final_config = @azimuth.ConfigManager.get_config(config_manager)
  assert_eq(final_config.service_version, "1.1.0")
  assert_eq(final_config.batch_size, 200)
  assert_eq(final_config.export_interval_ms, 10000)
  assert_eq(final_config.sampling_rate, 0.5) // 之前的更新保持
  
  // 测试配置验证
  let invalid_updates = [
    ("sampling_rate", 1.5), // 无效：超过1.0
    ("batch_size", 0)      // 无效：不能为0
  ]
  
  let invalid_result = @azimuth.ConfigManager.batch_update_config(config_manager, invalid_updates)
  assert_false(invalid_result)
  
  // 验证配置未被无效更新修改
  let unchanged_config = @azimuth.ConfigManager.get_config(config_manager)
  assert_eq(unchanged_config.sampling_rate, 0.5)
  assert_eq(unchanged_config.batch_size, 200)
  
  // 测试配置持久化和重载
  let persist_result = @azimuth.ConfigManager.persist_config(config_manager, "/tmp/azimuth_config.json")
  assert_true(persist_result)
  
  let new_config_manager = @azimuth.ConfigManager.new_empty()
  let load_result = @azimuth.ConfigManager.load_config(new_config_manager, "/tmp/azimuth_config.json")
  assert_true(load_result)
  
  let loaded_config = @azimuth.ConfigManager.get_config(new_config_manager)
  assert_eq(loaded_config.service_name, "azimuth-service")
  assert_eq(loaded_config.sampling_rate, 0.5)
  assert_eq(loaded_config.batch_size, 200)
}

// Test 6: 性能监控和阈值测试
test "performance monitoring and thresholds" {
  // 创建性能监控器
  let monitor = @azimuth.PerformanceMonitor.new()
  
  // 设置性能阈值
  let thresholds = @azimuth.PerformanceThresholds {
    cpu_usage_percent : 80.0,
    memory_usage_percent : 85.0,
    response_time_ms : 1000.0,
    error_rate_percent : 5.0,
    throughput_per_second : 100.0
  }
  
  @azimuth.PerformanceMonitor.set_thresholds(monitor, thresholds)
  
  // 模拟性能指标
  let performance_metrics = [
    @azimuth.PerformanceMetric {
      name : "cpu_usage",
      value : 75.5,
      unit : "percent",
      timestamp : @azimuth.current_timestamp()
    },
    @azimuth.PerformanceMetric {
      name : "memory_usage",
      value : 90.2,
      unit : "percent",
      timestamp : @azimuth.current_timestamp()
    },
    @azimuth.PerformanceMetric {
      name : "response_time",
      value : 1200.0,
      unit : "ms",
      timestamp : @azimuth.current_timestamp()
    },
    @azimuth.PerformanceMetric {
      name : "error_rate",
      value : 3.5,
      unit : "percent",
      timestamp : @azimuth.current_timestamp()
    },
    @azimuth.PerformanceMetric {
      name : "throughput",
      value : 150.0,
      unit : "req/s",
      timestamp : @azimuth.current_timestamp()
    }
  ]
  
  // 记录性能指标
  for metric in performance_metrics {
    @azimuth.PerformanceMonitor.record_metric(monitor, metric)
  }
  
  // 检查阈值违规
  let violations = @azimuth.PerformanceMonitor.check_thresholds(monitor)
  
  // 验证检测结果
  assert_eq(violations.length(), 2) // 内存使用率和响应时间超过阈值
  
  let memory_violation = violations.find(fn(v) { v.metric_name == "memory_usage" })
  match memory_violation {
    Some(violation) => {
      assert_eq(violation.current_value, 90.2)
      assert_eq(violation.threshold_value, 85.0)
      assert_true(violation.severity == @azimuth.Severity.Warning)
    }
    None => assert_true(false)
  }
  
  let response_time_violation = violations.find(fn(v) { v.metric_name == "response_time" })
  match response_time_violation {
    Some(violation) => {
      assert_eq(violation.current_value, 1200.0)
      assert_eq(violation.threshold_value, 1000.0)
      assert_true(violation.severity == @azimuth.Severity.Critical)
    }
    None => assert_true(false)
  }
  
  // 测试性能趋势分析
  let trend_analysis = @azimuth.PerformanceMonitor.analyze_trends(monitor, "response_time", 10)
  assert_true(trend_analysis.trend_direction == @azimuth.TrendDirection.Increasing)
  assert_true(trend_analysis.confidence > 0.7)
  
  // 测试自动阈值调整
  let adjusted_thresholds = @azimuth.PerformanceMonitor.auto_adjust_thresholds(monitor, 7) // 7天历史数据
  assert_true(adjusted_thresholds.response_time_ms > 1000.0) // 基于历史数据调整
}

// Test 7: 数据采样策略测试
test "data sampling strategies" {
  // 创建采样管理器
  let sampling_manager = @azimuth.SamplingManager.new()
  
  // 测试固定比例采样
  let fixed_sampler = @azimuth.FixedRateSampler.new(0.1) // 10%采样率
  
  let sampled_count = 0
  let total_count = 1000
  
  for i = 0; i < total_count; i = i + 1 {
    let trace_id = "trace-" + i.to_string()
    if @azimuth.Sampler.should_sample(fixed_sampler, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率接近目标值（允许合理误差）
  let actual_rate = sampled_count.to_float() / total_count.to_float()
  assert_true(actual_rate > 0.05)
  assert_true(actual_rate < 0.15)
  
  // 测试自适应采样
  let adaptive_sampler = @azimuth.AdaptiveSampler.new(100) // 目标100个采样/秒
  
  let adaptive_sampled = 0
  let current_time = @azimuth.current_timestamp()
  
  // 模拟不同时间段的请求
  for i = 0; i < 500; i = i + 1 {
    let trace_id = "adaptive-trace-" + i.to_string()
    let timestamp = current_time + (i * 10) // 每10ms一个请求
    
    if @azimuth.AdaptiveSampler.should_sample_at_time(adaptive_sampler, trace_id, timestamp) {
      adaptive_sampled = adaptive_sampled + 1
    }
  }
  
  // 验证自适应采样
  assert_true(adaptive_sampled <= 100) // 不应超过目标采样数
  
  // 测试基于属性的采样
  let attribute_sampler = @azimuth.AttributeBasedSampler.new([
    ("error", true, 1.0),        // 错误追踪100%采样
    ("slow_query", true, 0.8),   // 慢查询80%采样
    ("endpoint", "/api/critical", 1.0) // 关键端点100%采样
  ])
  
  // 测试错误追踪采样
  let error_trace_attrs = [("error", true), ("endpoint", "/api/users")]
  assert_true(@azimuth.AttributeBasedSampler.should_sample_with_attributes(
    attribute_sampler, 
    "error-trace", 
    error_trace_attrs
  ))
  
  // 测试慢查询采样
  let slow_query_attrs = [("slow_query", true), ("query_time", "5000ms")]
  let slow_query_sampled = 0
  for i = 0; i < 100; i = i + 1 {
    if @azimuth.AttributeBasedSampler.should_sample_with_attributes(
      attribute_sampler, 
      "slow-query-" + i.to_string(), 
      slow_query_attrs
    ) {
      slow_query_sampled = slow_query_sampled + 1
    }
  }
  
  // 验证慢查询采样率接近80%
  let slow_query_rate = slow_query_sampled.to_float() / 100.0
  assert_true(slow_query_rate > 0.7)
  assert_true(slow_query_rate < 0.9)
  
  // 测试组合采样策略
  let composite_sampler = @azimuth.CompositeSampler.new([
    fixed_sampler,
    adaptive_sampler,
    attribute_sampler
  ])
  
  let composite_sampled = 0
  for i = 0; i < 100; i = i + 1 {
    let trace_id = "composite-trace-" + i.to_string()
    if @azimuth.CompositeSampler.should_sample(composite_sampler, trace_id) {
      composite_sampled = composite_sampled + 1
    }
  }
  
  // 验证组合采样策略工作
  assert_true(composite_sampled > 0)
  assert_true(composite_sampled <= 100)
}

// Test 8: 遥测数据缓存测试
test "telemetry data caching" {
  // 创建缓存管理器
  let cache_manager = @azimuth.CacheManager.new(@azimuth.CacheConfig {
    max_size : 1000,
    ttl_ms : 60000, // 1分钟TTL
    eviction_policy : @azimuth.EvictionPolicy.LRU
  })
  
  // 测试基本的缓存操作
  let trace_id = "test-trace-12345"
  let span_data = @azimuth.SpanData {
    trace_id : trace_id,
    span_id : "span-123",
    operation_name : "cached.operation",
    start_time : @azimuth.current_timestamp(),
    attributes : [("cached", "true")]
  }
  
  // 测试缓存写入
  let put_result = @azimuth.CacheManager.put(cache_manager, trace_id, span_data)
  assert_true(put_result)
  
  // 测试缓存读取
  let cached_data = @azimuth.CacheManager.get(cache_manager, trace_id)
  match cached_data {
    Some(data) => {
      assert_eq(data.trace_id, trace_id)
      assert_eq(data.operation_name, "cached.operation")
      assert_true(data.attributes.contains(("cached", "true")))
    }
    None => assert_true(false)
  }
  
  // 测试缓存不存在的键
  let non_existent = @azimuth.CacheManager.get(cache_manager, "non-existent-trace")
  assert_eq(non_existent, None)
  
  // 测试缓存更新
  let updated_span = @azimuth.SpanData {
    trace_id : trace_id,
    span_id : "span-123",
    operation_name : "cached.operation.updated",
    start_time : @azimuth.current_timestamp(),
    attributes : [("cached", "true"), ("updated", "true")]
  }
  
  let update_result = @azimuth.CacheManager.put(cache_manager, trace_id, updated_span)
  assert_true(update_result)
  
  let updated_data = @azimuth.CacheManager.get(cache_manager, trace_id)
  match updated_data {
    Some(data) => {
      assert_eq(data.operation_name, "cached.operation.updated")
      assert_true(data.attributes.contains(("updated", "true")))
    }
    None => assert_true(false)
  }
  
  // 测试批量缓存操作
  let batch_data = []
  for i = 0; i < 100; i = i + 1 {
    let batch_span = @azimuth.SpanData {
      trace_id : "batch-trace-" + i.to_string(),
      span_id : "span-" + i.to_string(),
      operation_name : "batch.operation",
      start_time : @azimuth.current_timestamp(),
      attributes : [("batch", "true"), ("index", i.to_string())]
    }
    batch_data = batch_data.push((batch_span.trace_id, batch_span))
  }
  
  let batch_put_result = @azimuth.CacheManager.put_batch(cache_manager, batch_data)
  assert_true(batch_put_result)
  
  // 验证批量缓存
  let batch_get_keys = batch_data.map(fn(item) { item.0 })
  let batch_get_result = @azimuth.CacheManager.get_batch(cache_manager, batch_get_keys)
  assert_eq(batch_get_result.length(), 100)
  
  // 测试缓存大小限制
  let cache_stats = @azimuth.CacheManager.get_stats(cache_manager)
  assert_eq(cache_stats.size, 101) // 1个初始项 + 100个批量项
  
  // 添加更多项以测试驱逐策略
  for i = 100; i < 1200; i = i + 1 {
    let overflow_span = @azimuth.SpanData {
      trace_id : "overflow-trace-" + i.to_string(),
      span_id : "span-" + i.to_string(),
      operation_name : "overflow.operation",
      start_time : @azimuth.current_timestamp(),
      attributes : []
    }
    @azimuth.CacheManager.put(cache_manager, overflow_span.trace_id, overflow_span)
  }
  
  // 验证缓存大小不超过最大值
  let final_stats = @azimuth.CacheManager.get_stats(cache_manager)
  assert_true(final_stats.size <= 1000)
  
  // 测试TTL过期
  let ttl_span = @azimuth.SpanData {
    trace_id : "ttl-test-trace",
    span_id : "ttl-span",
    operation_name : "ttl.operation",
    start_time : @azimuth.current_timestamp(),
    attributes : []
  }
  
  @azimuth.CacheManager.put(cache_manager, ttl_span.trace_id, ttl_span)
  
  // 模拟时间流逝（通过创建新的缓存管理器，设置很短的TTL）
  let short_ttl_cache = @azimuth.CacheManager.new(@azimuth.CacheConfig {
    max_size : 100,
    ttl_ms : 1, // 1ms TTL
    eviction_policy : @azimuth.EvictionPolicy.LRU
  })
  
  @azimuth.CacheManager.put(short_ttl_cache, "short-ttl-test", ttl_span)
  
  // 等待TTL过期（在实际实现中可能需要时间模拟）
  let expired_data = @azimuth.CacheManager.get(short_ttl_cache, "short-ttl-test")
  // 在实际实现中，这里应该返回None，因为TTL已过期
}

// Test 9: 错误恢复和容错测试
test "error recovery and fault tolerance" {
  // 创建容错管理器
  let fault_manager = @azimuth.FaultManager.new(@azimuth.FaultConfig {
    max_retries : 3,
    retry_delay_ms : 1000,
    circuit_breaker_threshold : 5,
    circuit_breaker_timeout_ms : 10000,
    fallback_enabled : true
  })
  
  // 测试重试机制
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      @azimuth.OperationResult.Error("Temporary failure")
    } else {
      @azimuth.OperationResult.Success("Operation completed")
    }
  }
  
  let retry_result = @azimuth.FaultManager.execute_with_retry(fault_manager, flaky_operation)
  match retry_result {
    @azimuth.OperationResult.Success(message) => {
      assert_eq(message, "Operation completed")
      assert_eq(attempt_count, 3)
    }
    @azimuth.OperationResult.Error(_) => assert_true(false)
  }
  
  // 测试熔断器
  let mut failure_count = 0
  let failing_operation = fn() {
    failure_count = failure_count + 1
    @azimuth.OperationResult.Error("Persistent failure")
  }
  
  // 触发熔断器
  for i = 0; i < 6; i = i + 1 {
    @azimuth.FaultManager.execute_with_circuit_breaker(fault_manager, "failing.service", failing_operation)
  }
  
  // 验证熔断器已打开
  let circuit_state = @azimuth.FaultManager.get_circuit_state(fault_manager, "failing.service")
  assert_eq(circuit_state, @azimuth.CircuitState.Open)
  
  // 测试熔断器打开时的快速失败
  let fast_failure_result = @azimuth.FaultManager.execute_with_circuit_breaker(
    fault_manager, 
    "failing.service", 
    fn() { @azimuth.OperationResult.Success("Should not execute") }
  )
  match fast_failure_result {
    @azimuth.OperationResult.Error(message) => {
      assert_true(message.contains("Circuit breaker is open"))
    }
    _ => assert_true(false)
  }
  
  // 测试降级机制
  let primary_operation = fn() {
    @azimuth.OperationResult.Error("Primary service unavailable")
  }
  
  let fallback_operation = fn() {
    @azimuth.OperationResult.Success("Fallback response")
  }
  
  let fallback_result = @azimuth.FaultManager.execute_with_fallback(
    fault_manager, 
    primary_operation, 
    fallback_operation
  )
  match fallback_result {
    @azimuth.OperationResult.Success(message) => {
      assert_eq(message, "Fallback response")
    }
    _ => assert_true(false)
  }
  
  // 测试超时处理
  let slow_operation = fn() {
    // 模拟慢操作
    @azimuth.Thread.sleep(5000) // 5秒
    @azimuth.OperationResult.Success("Slow operation completed")
  }
  
  let timeout_result = @azimuth.FaultManager.execute_with_timeout(
    fault_manager, 
    slow_operation, 
    1000 // 1秒超时
  )
  match timeout_result {
    @azimuth.OperationResult.Error(message) => {
      assert_true(message.contains("timeout"))
    }
    _ => assert_true(false)
  }
  
  // 测试错误聚合和分析
  let error_samples = [
    @azimuth.ErrorSample {
      error_type : "ConnectionError",
      service : "database",
      timestamp : @azimuth.current_timestamp(),
      context : [("query", "SELECT * FROM users")]
    },
    @azimuth.ErrorSample {
      error_type : "TimeoutError",
      service : "api-gateway",
      timestamp : @azimuth.current_timestamp(),
      context : [("endpoint", "/api/users")]
    },
    @azimuth.ErrorSample {
      error_type : "ConnectionError",
      service : "database",
      timestamp : @azimuth.current_timestamp(),
      context : [("query", "SELECT * FROM orders")]
    }
  ]
  
  let error_analysis = @azimuth.FaultManager.analyze_errors(fault_manager, error_samples)
  
  // 验证错误分析结果
  assert_eq(error_analysis.total_errors, 3)
  assert_eq(error_analysis.unique_error_types, 2)
  assert_eq(error_analysis.most_common_error_type, "ConnectionError")
  
  let db_errors = error_analysis.by_service.get("database")
  match db_errors {
    Some(count) => assert_eq(count, 2)
    None => assert_true(false)
  }
}

// Test 10: 资源使用优化测试
test "resource usage optimization" {
  // 创建资源监控器
  let resource_monitor = @azimuth.ResourceMonitor.new()
  
  // 测试内存使用优化
  let memory_before = @azimuth.ResourceMonitor.get_memory_usage(resource_monitor)
  
  // 创建大量遥测数据
  let large_dataset = []
  for i = 0; i < 10000; i = i + 1 {
    let telemetry_data = @azimuth.TelemetryData {
      trace_id : "trace-" + i.to_string(),
      span_id : "span-" + i.to_string(),
      operation_name : "memory.test.operation",
      attributes : [
        ("index", i.to_string()),
        ("data", "large data string " + i.to_string()),
        ("timestamp", @azimuth.current_timestamp().to_string())
      ]
    }
    large_dataset = large_dataset.push(telemetry_data)
  }
  
  let memory_after_creation = @azimuth.ResourceMonitor.get_memory_usage(resource_monitor)
  assert_true(memory_after_creation > memory_before)
  
  // 测试内存优化策略
  let optimized_dataset = @azimuth.ResourceOptimizer.optimize_memory_usage(large_dataset)
  let memory_after_optimization = @azimuth.ResourceMonitor.get_memory_usage(resource_monitor)
  
  // 验证内存使用减少
  assert_true(memory_after_optimization < memory_after_creation)
  assert_eq(optimized_dataset.length(), large_dataset.length()) // 数据数量不变
  
  // 测试CPU使用优化
  let cpu_start = @azimuth.ResourceMonitor.get_cpu_usage(resource_monitor)
  
  // 执行CPU密集型操作
  let cpu_intensive_result = @azimuth.ResourceOptimizer.optimize_cpu_processing(large_dataset, fn(data) {
    // 模拟CPU密集型转换
    data.map(fn(item) { 
      @azimuth.TelemetryData {
        trace_id : item.trace_id,
        span_id : item.span_id,
        operation_name : item.operation_name.to_uppercase(),
        attributes : item.attributes
      }
    })
  })
  
  let cpu_end = @azimuth.ResourceMonitor.get_cpu_usage(resource_monitor)
  
  // 验证CPU优化结果
  assert_eq(cpu_intensive_result.length(), large_dataset.length())
  assert_true(cpu_intensive_result[0].operation_name.contains("MEMORY.TEST.OPERATION"))
  
  // 测试I/O优化
  let io_start_time = @azimuth.current_timestamp()
  
  // 执行批量I/O操作
  let batch_export_data = large_dataset.take(1000) // 取前1000项
  let io_optimized_result = @azimuth.ResourceOptimizer.optimize_io_operations(batch_export_data, fn(batch) {
    // 模拟批量I/O操作
    @azimuth.BatchOperationResult {
      success : true,
      processed_count : batch.length(),
      duration_ms : @azimuth.current_timestamp() - io_start_time
    }
  })
  
  let io_end_time = @azimuth.current_timestamp()
  let io_duration = io_end_time - io_start_time
  
  // 验证I/O优化结果
  assert_true(io_optimized_result.success)
  assert_eq(io_optimized_result.processed_count, 1000)
  assert_true(io_optimized_result.duration_ms < io_duration)
  
  // 测试资源池管理
  let resource_pool = @azimuth.ResourcePool.new(@azimuth.PoolConfig {
    initial_size : 5,
    max_size : 20,
    growth_factor : 2,
    shrink_threshold : 0.25
  })
  
  // 测试资源获取和释放
  let resources = []
  for i = 0; i < 10; i = i + 1 {
    let resource = @azimuth.ResourcePool.acquire(resource_pool)
    resources = resources.push(resource)
  }
  
  // 验证资源池扩展
  let pool_stats = @azimuth.ResourcePool.get_stats(resource_pool)
  assert_true(pool_stats.current_size >= 10)
  assert_true(pool_stats.active_resources == 10)
  
  // 释放资源
  for resource in resources {
    @azimuth.ResourcePool.release(resource_pool, resource)
  }
  
  // 验证资源释放
  let release_stats = @azimuth.ResourcePool.get_stats(resource_pool)
  assert_true(release_stats.active_resources == 0)
  
  // 测试垃圾回收优化
  let gc_before = @azimuth.ResourceMonitor.get_gc_stats(resource_monitor)
  
  // 创建临时对象
  let temporary_objects = []
  for i = 0; i < 5000; i = i + 1 {
    temporary_objects = temporary_objects.push("temp-object-" + i.to_string())
  }
  
  // 清理引用
  let _ = []
  
  // 执行优化垃圾回收
  let gc_result = @azimuth.ResourceOptimizer.optimize_gc(resource_monitor)
  assert_true(gc_result.success)
  
  let gc_after = @azimuth.ResourceMonitor.get_gc_stats(resource_monitor)
  assert_true(gc_after.collections > gc_before.collections)
  
  // 测试资源使用报告
  let resource_report = @azimuth.ResourceMonitor.generate_report(resource_monitor)
  
  // 验证报告内容
  assert_true(resource_report.timestamp > 0)
  assert_true(resource_report.memory_usage_mb > 0)
  assert_true(resource_report.cpu_usage_percent >= 0.0)
  assert_true(resource_report.cpu_usage_percent <= 100.0)
  assert_true(resource_report.gc_collections >= 0)
  assert_true(resource_report.resource_pools.length() > 0)
}