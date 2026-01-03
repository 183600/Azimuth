// Azimuth 综合高质量遥测测试套件
// 涵盖遥测系统核心功能和高级特性的全面测试

// 测试1: 遥测数据聚合与分析
test "遥测数据聚合与分析测试" {
  // 创建聚合管理器
  let aggregator = @azimuth.aggregation.Manager.create({
    "window.size": 60,  // 60秒窗口
    "aggregation.interval": 5,  // 5秒聚合间隔
    "retention.period": 3600  // 1小时保留期
  })
  
  // 验证聚合器创建成功
  assert_true(@azimuth.aggregation.Manager.is_valid(aggregator))
  
  // 创建多种类型的度量
  let counter = @azimuth.metrics.Counter.create("request.count")
    .set_description("请求总数")
    .set_unit("count")
  
  let histogram = @azimuth.metrics.Histogram.create("response.time")
    .set_description("响应时间分布")
    .set_unit("ms")
    .set_buckets([1.0, 5.0, 10.0, 50.0, 100.0, 500.0])
  
  let gauge = @azimuth.metrics.Gauge.create("memory.usage")
    .set_description("内存使用量")
    .set_unit("bytes")
  
  // 模拟数据输入
  for i = 0; i < 1000; i = i + 1 {
    counter.record(1, {"endpoint": "/api/v1/data", "status": "200"})
    histogram.record(@azimuth.math.random() * 1000.0, {"endpoint": "/api/v1/data"})
    gauge.set(@azimuth.math.random() * 1024.0 * 1024.0 * 100.0, {"host": "server-1"})
  }
  
  // 执行聚合
  let aggregation_result = aggregator.aggregate({
    "counters": [counter],
    "histograms": [histogram],
    "gauges": [gauge]
  })
  
  // 验证聚合结果
  assert_true(aggregation_result.success)
  assert_eq(aggregation_result.total_metrics, 3)
  assert_true(aggregation_result.aggregated_data["request.count"].sum > 0)
  assert_true(aggregation_result.aggregated_data["response.time"].count > 0)
  assert_true(aggregation_result.aggregated_data["memory.usage"].current_value > 0)
  
  // 验证时间窗口聚合
  let windowed_result = aggregator.get_windowed_aggregations(60)  // 最近60秒
  assert_true(windowed_result.length() > 0)
  assert_true(windowed_result[0].window_start > 0)
  assert_true(windowed_result[0].window_end > windowed_result[0].window_start)
}

// 测试2: 分布式追踪完整性验证
test "分布式追踪完整性验证测试" {
  // 创建分布式追踪管理器
  let tracer = @azimuth.tracer.DistributedTracer.create({
    "service.name": "azimuth-test-service",
    "service.version": "1.0.0",
    "sampling.probability": 1.0  // 100%采样以确保测试完整性
  })
  
  // 验证追踪器创建成功
  assert_true(@azimuth.tracer.DistributedTracer.is_valid(tracer))
  
  // 创建根Span
  let root_span = tracer.create_span("root-operation")
    .set_attribute("operation.type", "root")
    .set_attribute("user.id", "test-user-123")
    .start()
  
  // 验证根Span创建成功
  assert_true(@azimuth.tracer.Span.is_valid(root_span))
  assert_true(root_span.trace_id.length() > 0)
  assert_true(root_span.span_id.length() > 0)
  assert_eq(root_span.parent_span_id, "")
  
  // 创建子Span
  let child_span1 = tracer.create_span("child-operation-1")
    .set_parent(root_span)
    .set_attribute("operation.type", "child")
    .start()
  
  let child_span2 = tracer.create_span("child-operation-2")
    .set_parent(root_span)
    .set_attribute("operation.type", "child")
    .start()
  
  // 验证子Span的父子关系
  assert_eq(child_span1.trace_id, root_span.trace_id)
  assert_eq(child_span1.parent_span_id, root_span.span_id)
  assert_eq(child_span2.trace_id, root_span.trace_id)
  assert_eq(child_span2.parent_span_id, root_span.span_id)
  assert_ne(child_span1.span_id, child_span2.span_id)
  
  // 创建孙Span
  let grandchild_span = tracer.create_span("grandchild-operation")
    .set_parent(child_span1)
    .set_attribute("operation.type", "grandchild")
    .start()
  
  // 验证孙Span的层次关系
  assert_eq(grandchild_span.trace_id, root_span.trace_id)
  assert_eq(grandchild_span.parent_span_id, child_span1.span_id)
  
  // 添加事件和链接
  root_span.add_event("operation.started", {"timestamp": @azimuth.time.now()})
  child_span1.add_event("database.query.executed", {"query": "SELECT * FROM users"})
  child_span2.add_event("cache.miss", {"key": "user:123"})
  
  // 创建Span链接
  let linked_span = tracer.create_span("linked-operation")
    .add_link(root_span, "related-operation")
    .start()
  
  // 结束所有Span
  grandchild_span.end()
  child_span2.end()
  child_span1.end()
  linked_span.end()
  root_span.end()
  
  // 验证追踪完整性
  let trace_tree = tracer.get_trace_tree(root_span.trace_id)
  assert_true(trace_tree.is_complete)
  assert_eq(trace_tree.root_span.span_id, root_span.span_id)
  assert_eq(trace_tree.spans.length(), 5)  // 根 + 2个子 + 1个孙 + 1个链接
  
  // 验证Span层次结构
  let root_children = trace_tree.get_children(root_span.span_id)
  assert_eq(root_children.length(), 2)
  
  let child1_children = trace_tree.get_children(child_span1.span_id)
  assert_eq(child1_children.length(), 1)
  assert_eq(child1_children[0].span_id, grandchild_span.span_id)
}

// 测试3: 性能监控与资源使用优化
test "性能监控与资源使用优化测试" {
  // 创建性能监控器
  let profiler = @azimuth.performance.Profiler.create({
    "sampling.interval": 10,  // 10ms采样间隔
    "max.samples": 10000,     // 最大样本数
    "cpu.monitoring": true,
    "memory.monitoring": true,
    "gc.monitoring": true
  })
  
  // 验证性能监控器创建成功
  assert_true(@azimuth.performance.Profiler.is_valid(profiler))
  
  // 开始性能监控
  profiler.start_monitoring()
  
  // 模拟CPU密集型操作
  let start_time = @azimuth.time.now()
  let mut result = 0
  for i = 0; i < 1000000; i = i + 1 {
    result = result + @azimuth.math.sqrt(i.to_float())
  }
  let end_time = @azimuth.time.now()
  
  // 模拟内存分配操作
  let large_arrays = []
  for i = 0; i < 100; i = i + 1 {
    let array = []
    for j = 0; j < 1000; j = j + 1 {
      array = array.push(@azimuth.math.random())
    }
    large_arrays = large_arrays.push(array)
  }
  
  // 停止性能监控
  profiler.stop_monitoring()
  
  // 获取性能报告
  let performance_report = profiler.get_performance_report()
  
  // 验证性能报告
  assert_true(performance_report.monitoring_duration > 0)
  assert_true(performance_report.cpu_samples.length() > 0)
  assert_true(performance_report.memory_samples.length() > 0)
  assert_true(performance_report.gc_events.length() >= 0)
  
  // 验证CPU使用率
  let avg_cpu_usage = performance_report.get_average_cpu_usage()
  assert_true(avg_cpu_usage >= 0.0)
  assert_true(avg_cpu_usage <= 100.0)
  
  // 验证内存使用情况
  let peak_memory = performance_report.get_peak_memory_usage()
  assert_true(peak_memory > 0)
  
  let memory_growth = performance_report.get_memory_growth()
  assert_true(memory_growth >= 0)
  
  // 验证操作耗时
  let operation_duration = end_time - start_time
  assert_true(operation_duration > 0)
  
  // 获取性能建议
  let recommendations = profiler.get_performance_recommendations()
  assert_true(recommendations.length() >= 0)
  
  // 验证资源使用优化建议
  if avg_cpu_usage > 80.0 {
    assert_true(recommendations.any(fn(r) { r.contains("CPU") }))
  }
  
  if memory_growth > 100 * 1024 * 1024 {  // 100MB
    assert_true(recommendations.any(fn(r) { r.contains("memory") }))
  }
}

// 测试4: 错误处理与自动恢复机制
test "错误处理与自动恢复机制测试" {
  // 创建错误处理管理器
  let error_manager = @azimuth.error.ErrorManager.create({
    "max.retry.attempts": 3,
    "retry.backoff.strategy": "exponential",
    "circuit.breaker.threshold": 5,
    "recovery.timeout": 5000  // 5秒
  })
  
  // 验证错误管理器创建成功
  assert_true(@azimuth.error.ErrorManager.is_valid(error_manager))
  
  // 创建遥测客户端
  let telemetry_client = @azimuth.client.TelemetryClient.create({
    "endpoint": "https://telemetry.example.com/api/v1/traces",
    "timeout": 5000,
    "retry.enabled": true,
    "circuit.breaker.enabled": true
  })
  
  // 验证客户端创建成功
  assert_true(@azimuth.client.TelemetryClient.is_valid(telemetry_client))
  
  // 模拟连接错误
  let mock_error = @azimuth.error.NetworkError.create("Connection refused")
  
  // 测试重试机制
  let retry_result = error_manager.execute_with_retry(fn() {
    telemetry_client.send_batch([])  // 发送空批次
  }, mock_error)
  
  // 验证重试结果
  assert_false(retry_result.success)
  assert_eq(retry_result.attempts, 3)  // 应该尝试3次
  assert_true(retry_result.total_duration > 0)
  
  // 测试熔断器
  for i = 0; i < 6; i = i + 1 {  // 超过阈值(5)的失败次数
    error_manager.record_failure("telemetry.send")
  }
  
  // 验证熔断器状态
  assert_true(error_manager.is_circuit_open("telemetry.send"))
  
  // 测试熔断器阻止操作
  let blocked_result = error_manager.execute_with_circuit_breaker("telemetry.send", fn() {
    telemetry_client.send_batch([])
  })
  
  // 验证操作被阻止
  assert_false(blocked_result.success)
  assert_eq(blocked_result.error.type, "CircuitBreakerOpen")
  
  // 测试自动恢复
  let recovery_start = @azimuth.time.now()
  
  // 等待恢复超时
  @azimuth.time.sleep(6000)  // 6秒，超过5秒恢复超时
  
  // 模拟成功操作
  let recovery_result = error_manager.execute_with_circuit_breaker("telemetry.send", fn() {
    @azimuth.result.Ok("success")  // 模拟成功
  })
  
  // 验证恢复结果
  assert_true(recovery_result.success)
  assert_false(error_manager.is_circuit_open("telemetry.send"))  // 熔断器应关闭
  
  // 测试错误分类和统计
  let error_stats = error_manager.get_error_statistics()
  assert_true(error_stats.total_errors > 0)
  assert_true(error_stats.error_types.contains("NetworkError"))
  assert_true(error_stats.recovery_attempts > 0)
  assert_true(error_stats.successful_recoveries > 0)
}

// 测试5: 数据完整性与一致性验证
test "数据完整性与一致性验证测试" {
  // 创建数据完整性验证器
  let integrity_validator = @azimuth.integrity.Validator.create({
    "checksum.algorithm": "SHA-256",
    "compression.enabled": true,
    "encryption.enabled": false,
    "validation.level": "strict"
  })
  
  // 验证完整性验证器创建成功
  assert_true(@azimuth.integrity.Validator.is_valid(integrity_validator))
  
  // 创建测试数据集
  let test_data = []
  for i = 0; i < 1000; i = i + 1 {
    let data_point = {
      "timestamp": @azimuth.time.now() - i * 1000,  // 1秒间隔
      "trace_id": @azimuth.trace.generate_id(),
      "span_id": @azimuth.trace.generate_span_id(),
      "operation_name": "operation-" + @azimuth.string.from_int(i % 10),
      "duration": @azimuth.math.random() * 1000.0,
      "status": ["success", "error", "timeout"][i % 3],
      "attributes": {
        "service.name": "test-service",
        "host": "server-" + @azimuth.string.from_int(i % 5),
        "user.id": "user-" + @azimuth.string.from_int(i % 100)
      }
    }
    test_data = test_data.push(data_point)
  }
  
  // 计算原始数据校验和
  let original_checksum = integrity_validator.calculate_checksum(test_data)
  assert_true(original_checksum.length() > 0)
  
  // 序列化数据
  let serialized_data = @azimuth.serialization.serialize(test_data, "json")
  assert_true(serialized_data.length() > 0)
  
  // 压缩数据
  let compressed_data = @azimuth.compression.compress(serialized_data, "gzip")
  assert_true(compressed_data.length() > 0)
  assert_true(compressed_data.length() < serialized_data.length())  // 压缩后应更小
  
  // 解压缩数据
  let decompressed_data = @azimuth.compression.decompress(compressed_data, "gzip")
  assert_eq(decompressed_data.length(), serialized_data.length())
  
  // 反序列化数据
  let deserialized_data = @azimuth.serialization.deserialize(decompressed_data, "json")
  assert_eq(deserialized_data.length(), test_data.length())
  
  // 验证数据完整性
  let restored_checksum = integrity_validator.calculate_checksum(deserialized_data)
  assert_eq(restored_checksum, original_checksum)  // 校验和应匹配
  
  // 验证数据一致性
  for i = 0; i < test_data.length(); i = i + 1 {
    let original = test_data[i]
    let restored = deserialized_data[i]
    
    assert_eq(original.timestamp, restored.timestamp)
    assert_eq(original.trace_id, restored.trace_id)
    assert_eq(original.span_id, restored.span_id)
    assert_eq(original.operation_name, restored.operation_name)
    assert_eq(original.status, restored.status)
    assert_eq(original.attributes["service.name"], restored.attributes["service.name"])
    assert_eq(original.attributes["host"], restored.attributes["host"])
    assert_eq(original.attributes["user.id"], restored.attributes["user.id"])
  }
  
  // 测试数据损坏检测
  let corrupted_data = serialized_data.substring(0, serialized_data.length() / 2)
  let corruption_result = integrity_validator.validate_data(corrupted_data)
  assert_false(corruption_result.valid)
  assert_eq(corruption_result.error.type, "DataCorruption")
  
  // 测试数据修复
  let repair_result = integrity_validator.attempt_repair(corrupted_data, original_checksum)
  if repair_result.success {
    let repaired_checksum = integrity_validator.calculate_checksum(repair_result.data)
    assert_eq(repaired_checksum, original_checksum)
  }
}

// 测试6: 自定义度量与高级分析
test "自定义度量与高级分析测试" {
  // 创建自定义度量注册表
  let registry = @azimuth.metrics.Registry.create({
    "default.aggregation": "temporal",
    "default.temporality": "cumulative",
    "exemplars.enabled": true
  })
  
  // 验证注册表创建成功
  assert_true(@azimuth.metrics.Registry.is_valid(registry))
  
  // 创建自定义计数器
  let custom_counter = registry.create_counter("custom.operations", {
    "description": "自定义操作计数",
    "unit": "operations",
    "attributes": ["operation.type", "service.component"]
  })
  
  // 创建自定义直方图
  let custom_histogram = registry.create_histogram("custom.duration", {
    "description": "自定义操作持续时间",
    "unit": "ms",
    "buckets": [0.1, 0.5, 1.0, 2.5, 5.0, 10.0, 25.0, 50.0, 100.0],
    "attributes": ["operation.type", "service.component"]
  })
  
  // 创建自定义仪表
  let custom_gauge = registry.create_gauge("custom.queue.size", {
    "description": "自定义队列大小",
    "unit": "items",
    "attributes": ["queue.name", "service.component"]
  })
  
  // 记录自定义度量数据
  let operation_types = ["read", "write", "delete", "update"]
  let components = ["database", "cache", "queue", "auth"]
  
  for i = 0; i < 1000; i = i + 1 {
    let op_type = operation_types[i % operation_types.length()]
    let component = components[i % components.length()]
    
    // 记录计数器
    custom_counter.record(1, {
      "operation.type": op_type,
      "service.component": component
    })
    
    // 记录直方图
    custom_histogram.record(@azimuth.math.random() * 100.0, {
      "operation.type": op_type,
      "service.component": component
    })
    
    // 记录仪表
    custom_gauge.set(@azimuth.math.random_int(1000), {
      "queue.name": "queue-" + @azimuth.string.from_int(i % 5),
      "service.component": component
    })
  }
  
  // 获取所有度量
  let all_metrics = registry.collect_all()
  assert_true(all_metrics.length() >= 3)  // 至少有3种类型的度量
  
  // 执行高级分析
  let analyzer = @azimuth.analysis.Analyzer.create({
    "percentiles": [50.0, 90.0, 95.0, 99.0],
    "correlation.analysis": true,
    "trend.analysis": true,
    "anomaly.detection": true
  })
  
  // 验证分析器创建成功
  assert_true(@azimuth.analysis.Analyzer.is_valid(analyzer))
  
  // 执行分析
  let analysis_result = analyzer.analyze(all_metrics)
  
  // 验证分析结果
  assert_true(analysis_result.success)
  assert_true(analysis_result.summary.total_metrics > 0)
  assert_true(analysis_result.summary.total_data_points > 0)
  
  // 验证百分位数分析
  let duration_analysis = analysis_result.get_metric_analysis("custom.duration")
  assert_true(duration_analysis.percentiles.contains_key("p50"))
  assert_true(duration_analysis.percentiles.contains_key("p90"))
  assert_true(duration_analysis.percentiles.contains_key("p95"))
  assert_true(duration_analysis.percentiles.contains_key("p99"))
  
  // 验证趋势分析
  let counter_trend = analysis_result.get_trend_analysis("custom.operations")
  assert_true(counter_trend.trend_direction == "up" || 
              counter_trend.trend_direction == "down" || 
              counter_trend.trend_direction == "stable")
  assert_true(counter_trend.confidence >= 0.0)
  assert_true(counter_trend.confidence <= 1.0)
  
  // 验证异常检测
  let anomalies = analysis_result.get_anomalies()
  assert_true(anomalies.length() >= 0)
  
  if anomalies.length() > 0 {
    let first_anomaly = anomalies[0]
    assert_true(first_anomaly.metric_name.length() > 0)
    assert_true(first_anomaly.anomaly_score > 0.0)
    assert_true(first_anomaly.timestamp > 0)
  }
  
  // 验证相关性分析
  let correlations = analysis_result.get_correlations()
  assert_true(correlations.length() >= 0)
  
  if correlations.length() > 0 {
    let first_correlation = correlations[0]
    assert_true(first_correlation.metric_a.length() > 0)
    assert_true(first_correlation.metric_b.length() > 0)
    assert_true(first_correlation.correlation_coefficient >= -1.0)
    assert_true(first_correlation.correlation_coefficient <= 1.0)
  }
}

// 测试7: 资源监控与自适应调整
test "资源监控与自适应调整测试" {
  // 创建资源监控器
  let resource_monitor = @azimuth.resource.Monitor.create({
    "monitoring.interval": 100,  // 100ms监控间隔
    "cpu.threshold": 80.0,       // CPU使用率阈值
    "memory.threshold": 90.0,    // 内存使用率阈值
    "disk.threshold": 85.0,      // 磁盘使用率阈值
    "network.threshold": 1000000  // 网络使用阈值(bytes/s)
  })
  
  // 验证资源监控器创建成功
  assert_true(@azimuth.resource.Monitor.is_valid(resource_monitor))
  
  // 创建自适应调整管理器
  let adaptive_manager = @azimuth.adaptive.Manager.create({
    "adjustment.strategy": "gradual",
    "max.adjustment.step": 0.2,  // 最大调整步长20%
    "stabilization.period": 30000,  // 30秒稳定期
    "resource.priorities": ["cpu", "memory", "disk", "network"]
  })
  
  // 验证自适应管理器创建成功
  assert_true(@azimuth.adaptive.Manager.is_valid(adaptive_manager))
  
  // 启动资源监控
  resource_monitor.start_monitoring()
  
  // 模拟高CPU负载
  let cpu_load_generator = @azimuth.resource.CPULoadGenerator.create({
    "intensity": 0.9,  // 90% CPU使用率
    "duration": 5000   // 5秒
  })
  
  cpu_load_generator.start()
  
  // 等待监控器检测到高负载
  @azimuth.time.sleep(1000)  // 1秒
  
  // 获取当前资源状态
  let resource_state = resource_monitor.get_current_state()
  assert_true(resource_state.cpu_usage > 0.0)
  assert_true(resource_state.memory_usage > 0.0)
  assert_true(resource_state.timestamp > 0)
  
  // 创建遥测配置
  let telemetry_config = {
    "sampling.probability": 1.0,
    "batch.size": 1000,
    "flush.interval": 5000,
    "compression.enabled": true,
    "retry.max.attempts": 3
  }
  
  // 触发自适应调整
  let adjustment_result = adaptive_manager.adjust_configuration(telemetry_config, resource_state)
  
  // 验证调整结果
  assert_true(adjustment_result.adjusted)
  assert_true(adjustment_result.new_config != telemetry_config)
  assert_true(adjustment_result.adjustments.length() > 0)
  
  // 验证具体调整项
  let sampling_adjusted = adjustment_result.adjustments.any(fn(adj) {
    adj.parameter == "sampling.probability" && adj.old_value != adj.new_value
  })
  let batch_size_adjusted = adjustment_result.adjustments.any(fn(adj) {
    adj.parameter == "batch.size" && adj.old_value != adj.new_value
  })
  
  assert_true(sampling_adjusted || batch_size_adjusted)
  
  // 验证调整方向正确（高负载时应降低资源消耗）
  if resource_state.cpu_usage > 80.0 {
    let new_sampling = adjustment_result.new_config["sampling.probability"]
    let old_sampling = telemetry_config["sampling.probability"]
    assert_true(new_sampling <= old_sampling)  // 采样率应降低
  }
  
  // 停止CPU负载生成器
  cpu_load_generator.stop()
  
  // 模拟低负载状态
  @azimuth.time.sleep(2000)  // 等待2秒让CPU使用率下降
  
  // 获取低负载状态
  let low_load_state = resource_monitor.get_current_state()
  
  // 触发反向调整
  let readjustment_result = adaptive_manager.adjust_configuration(
    adjustment_result.new_config, 
    low_load_state
  )
  
  // 验证反向调整
  if low_load_state.cpu_usage < 50.0 {
    let new_sampling = readjustment_result.new_config["sampling.probability"]
    let prev_sampling = adjustment_result.new_config["sampling.probability"]
    assert_true(new_sampling >= prev_sampling)  // 采样率应恢复
  }
  
  // 停止资源监控
  resource_monitor.stop_monitoring()
  
  // 获取监控历史
  let monitoring_history = resource_monitor.get_monitoring_history()
  assert_true(monitoring_history.length() > 0)
  
  // 验证历史数据完整性
  let first_sample = monitoring_history[0]
  assert_true(first_sample.cpu_usage >= 0.0)
  assert_true(first_sample.memory_usage >= 0.0)
  assert_true(first_sample.timestamp > 0)
  
  // 验证调整历史
  let adjustment_history = adaptive_manager.get_adjustment_history()
  assert_true(adjustment_history.length() > 0)
  
  let first_adjustment = adjustment_history[0]
  assert_true(first_adjustment.timestamp > 0)
  assert_true(first_adjustment.triggering_state.cpu_usage >= 0.0)
  assert_true(first_adjustment.adjustments.length() > 0)
}

// 测试8: 智能采样策略与数据保留
test "智能采样策略与数据保留测试" {
  // 创建智能采样管理器
  let sampling_manager = @azimuth.sampling.SmartSamplingManager.create({
    "default.probability": 0.1,  // 默认10%采样率
    "max.sample.rate": 1000,     // 每秒最大采样数
    "adaptive.sampling": true,   // 启用自适应采样
    "error.based.sampling": true, // 启用基于错误的采样
    "latency.based.sampling": true // 启用基于延迟的采样
  })
  
  // 验证采样管理器创建成功
  assert_true(@azimuth.sampling.SmartSamplingManager.is_valid(sampling_manager))
  
  // 创建数据保留策略管理器
  let retention_manager = @azimuth.retention.Manager.create({
    "default.retention.days": 30,    // 默认保留30天
    "high.priority.retention.days": 90,  // 高优先级保留90天
    "low.priority.retention.days": 7,    // 低优先级保留7天
    "compression.before.retention": true
  })
  
  // 验证保留管理器创建成功
  assert_true(@azimuth.retention.Manager.is_valid(retention_manager))
  
  // 模拟不同优先级的遥测数据
  let telemetry_data = []
  let current_time = @azimuth.time.now()
  
  // 生成正常优先级数据
  for i = 0; i < 1000; i = i + 1 {
    let data_point = {
      "timestamp": current_time - i * 1000,
      "trace_id": @azimuth.trace.generate_id(),
      "span_id": @azimuth.trace.generate_span_id(),
      "priority": "normal",
      "duration": @azimuth.math.random() * 1000.0,
      "error": false,
      "attributes": {
        "service.name": "test-service",
        "operation.name": "operation-" + @azimuth.string.from_int(i % 10)
      }
    }
    telemetry_data = telemetry_data.push(data_point)
  }
  
  // 生成高优先级数据（错误和慢请求）
  for i = 0; i < 100; i = i + 1 {
    let data_point = {
      "timestamp": current_time - i * 1000,
      "trace_id": @azimuth.trace.generate_id(),
      "span_id": @azimuth.trace.generate_span_id(),
      "priority": "high",
      "duration": @azimuth.math.random() * 1000.0 + 1000.0,  // >1秒
      "error": i % 10 < 3,  // 30%错误率
      "attributes": {
        "service.name": "critical-service",
        "operation.name": "critical-operation"
      }
    }
    telemetry_data = telemetry_data.push(data_point)
  }
  
  // 生成低优先级数据（健康检查等）
  for i = 0; i < 500; i = i + 1 {
    let data_point = {
      "timestamp": current_time - i * 1000,
      "trace_id": @azimuth.trace.generate_id(),
      "span_id": @azimuth.trace.generate_span_id(),
      "priority": "low",
      "duration": @azimuth.math.random() * 10.0 + 5.0,  // 5-15ms
      "error": false,
      "attributes": {
        "service.name": "health-check",
        "operation.name": "ping"
      }
    }
    telemetry_data = telemetry_data.push(data_point)
  }
  
  // 应用智能采样
  let sampling_result = sampling_manager.apply_sampling(telemetry_data)
  
  // 验证采样结果
  assert_true(sampling_result.sampled_data.length() < telemetry_data.length())
  assert_true(sampling_result.sampling_rate > 0.0)
  assert_true(sampling_result.sampling_rate <= 1.0)
  
  // 验证高优先级数据采样率更高
  let high_priority_original = telemetry_data.filter(fn(d) { d.priority == "high" })
  let high_priority_sampled = sampling_result.sampled_data.filter(fn(d) { d.priority == "high" })
  let high_priority_rate = high_priority_sampled.length().to_float() / high_priority_original.length().to_float()
  
  let normal_priority_original = telemetry_data.filter(fn(d) { d.priority == "normal" })
  let normal_priority_sampled = sampling_result.sampled_data.filter(fn(d) { d.priority == "normal" })
  let normal_priority_rate = normal_priority_sampled.length().to_float() / normal_priority_original.length().to_float()
  
  let low_priority_original = telemetry_data.filter(fn(d) { d.priority == "low" })
  let low_priority_sampled = sampling_result.sampled_data.filter(fn(d) { d.priority == "low" })
  let low_priority_rate = low_priority_sampled.length().to_float() / low_priority_original.length().to_float()
  
  assert_true(high_priority_rate >= normal_priority_rate)
  assert_true(normal_priority_rate >= low_priority_rate)
  
  // 验证错误和慢请求的采样偏好
  let error_data = high_priority_original.filter(fn(d) { d.error })
  let error_sampled = high_priority_sampled.filter(fn(d) { d.error })
  let error_rate = error_sampled.length().to_float() / error_data.length().to_float()
  
  let slow_data = high_priority_original.filter(fn(d) { d.duration > 1500.0 })
  let slow_sampled = high_priority_sampled.filter(fn(d) { d.duration > 1500.0 })
  let slow_rate = slow_sampled.length().to_float() / slow_data.length().to_float()
  
  assert_true(error_rate >= high_priority_rate)
  assert_true(slow_rate >= high_priority_rate)
  
  // 应用数据保留策略
  let retention_result = retention_manager.apply_retention(sampling_result.sampled_data)
  
  // 验证保留结果
  assert_true(retention_result.retained_data.length() <= sampling_result.sampled_data.length())
  assert_true(retention_result.expired_data.length() >= 0)
  assert_true(retention_result.compressed_data.length() >= 0)
  
  // 验证不同优先级数据的保留期限
  let high_priority_retained = retention_result.retained_data.filter(fn(d) { d.priority == "high" })
  let normal_priority_retained = retention_result.retained_data.filter(fn(d) { d.priority == "normal" })
  let low_priority_retained = retention_result.retained_data.filter(fn(d) { d.priority == "low" })
  
  // 验证保留统计
  let retention_stats = retention_result.get_retention_statistics()
  assert_true(retention_stats.total_original > 0)
  assert_true(retention_stats.total_retained >= 0)
  assert_true(retention_stats.total_expired >= 0)
  assert_true(retention_stats.total_compressed >= 0)
  assert_true(retention_stats.compression_ratio >= 0.0)
  
  // 验证存储空间节省
  if retention_result.compressed_data.length() > 0 {
    let original_size = retention_result.calculate_data_size(sampling_result.sampled_data)
    let compressed_size = retention_result.calculate_data_size(retention_result.compressed_data)
    assert_true(compressed_size < original_size)
  }
}

// 测试9: 配置管理与动态更新
test "配置管理与动态更新测试" {
  // 创建配置管理器
  let config_manager = @azimuth.config.Manager.create({
    "config.source": "file",
    "config.file.path": "/etc/azimuth/telemetry.json",
    "watch.for.changes": true,
    "validation.enabled": true,
    "schema.version": "1.0"
  })
  
  // 验证配置管理器创建成功
  assert_true(@azimuth.config.Manager.is_valid(config_manager))
  
  // 创建初始配置
  let initial_config = {
    "telemetry": {
      "service": {
        "name": "azimuth-test-service",
        "version": "1.0.0",
        "environment": "test"
      },
      "tracing": {
        "sampling.probability": 0.1,
        "batch.size": 1000,
        "flush.interval": 5000,
        "exporter": "otlp"
      },
      "metrics": {
        "enabled": true,
        "export.interval": 10000,
        "histograms.buckets": [1.0, 5.0, 10.0, 50.0, 100.0],
        "exemplars.enabled": true
      },
      "logging": {
        "level": "info",
        "format": "json",
        "output": "stdout"
      }
    },
    "resources": {
      "cpu.limit": 80.0,
      "memory.limit": 1024.0,
      "disk.limit": 10240.0
    }
  }
  
  // 加载初始配置
  let load_result = config_manager.load_config(initial_config)
  
  // 验证配置加载
  assert_true(load_result.success)
  assert_true(config_manager.is_config_loaded())
  
  // 验证配置值
  assert_eq(config_manager.get("telemetry.service.name"), "azimuth-test-service")
  assert_eq(config_manager.get("telemetry.tracing.sampling.probability"), 0.1)
  assert_true(config_manager.get("telemetry.metrics.enabled"))
  assert_eq(config_manager.get("telemetry.logging.level"), "info")
  assert_eq(config_manager.get("resources.cpu.limit"), 80.0)
  
  // 创建配置更新
  let config_updates = {
    "telemetry.tracing.sampling.probability": 0.2,
    "telemetry.tracing.batch.size": 2000,
    "telemetry.metrics.export.interval": 15000,
    "telemetry.logging.level": "debug",
    "resources.memory.limit": 2048.0
  }
  
  // 应用配置更新
  let update_result = config_manager.update_config(config_updates)
  
  // 验证配置更新
  assert_true(update_result.success)
  assert_eq(update_result.updated_keys.length(), 5)
  
  // 验证新配置值
  assert_eq(config_manager.get("telemetry.tracing.sampling.probability"), 0.2)
  assert_eq(config_manager.get("telemetry.tracing.batch.size"), 2000)
  assert_eq(config_manager.get("telemetry.metrics.export.interval"), 15000)
  assert_eq(config_manager.get("telemetry.logging.level"), "debug")
  assert_eq(config_manager.get("resources.memory.limit"), 2048.0)
  
  // 验证未变更的配置值
  assert_eq(config_manager.get("telemetry.service.name"), "azimuth-test-service")
  assert_true(config_manager.get("telemetry.metrics.enabled"))
  assert_eq(config_manager.get("resources.cpu.limit"), 80.0)
  
  // 测试配置验证
  let invalid_config = {
    "telemetry.tracing.sampling.probability": 1.5,  // 无效：>1.0
    "telemetry.tracing.batch.size": -100,           // 无效：负数
    "resources.cpu.limit": "invalid"                // 无效：非数字
  }
  
  let validation_result = config_manager.validate_config(invalid_config)
  
  // 验证配置验证结果
  assert_false(validation_result.valid)
  assert_true(validation_result.errors.length() >= 3)
  assert_true(validation_result.errors.any(fn(err) { err.contains("sampling.probability") }))
  assert_true(validation_result.errors.any(fn(err) { err.contains("batch.size") }))
  assert_true(validation_result.errors.any(fn(err) { err.contains("cpu.limit") }))
  
  // 测试配置回滚
  let rollback_result = config_manager.rollback_config()
  
  // 验证配置回滚
  assert_true(rollback_result.success)
  assert_eq(config_manager.get("telemetry.tracing.sampling.probability"), 0.1)  // 回滚到初始值
  assert_eq(config_manager.get("telemetry.tracing.batch.size"), 1000)
  assert_eq(config_manager.get("telemetry.metrics.export.interval"), 10000)
  assert_eq(config_manager.get("telemetry.logging.level"), "info")
  assert_eq(config_manager.get("resources.memory.limit"), 1024.0)
  
  // 测试配置导出
  let export_result = config_manager.export_config()
  
  // 验证配置导出
  assert_true(export_result.success)
  assert_true(export_result.config_data.length() > 0)
  
  // 验证导出的配置格式
  let exported_config = @azimuth.json.parse(export_result.config_data)
  assert_eq(exported_config["telemetry"]["service"]["name"], "azimuth-test-service")
  assert_eq(exported_config["telemetry"]["tracing"]["sampling.probability"], 0.1)
  
  // 测试配置变更历史
  let config_history = config_manager.get_config_history()
  assert_true(config_history.length() >= 2)  // 初始加载 + 更新 + 回滚
  
  // 验证历史记录
  let latest_change = config_history[0]
  assert_true(latest_change.timestamp > 0)
  assert_eq(latest_change.operation, "rollback")
  assert_true(latest_change.changed_keys.length() > 0)
  
  let update_change = config_history[1]
  assert_eq(update_change.operation, "update")
  assert_eq(update_change.changed_keys.length(), 5)
}

// 测试10: 多格式数据导出与集成
test "多格式数据导出与集成测试" {
  // 创建多格式导出管理器
  let export_manager = @azimuth.export.MultiFormatManager.create({
    "supported.formats": ["json", "protobuf", "avro", "csv", "parquet"],
    "default.format": "json",
    "compression.enabled": true,
    "chunking.enabled": true,
    "chunk.size": 10000
  })
  
  // 验证导出管理器创建成功
  assert_true(@azimuth.export.MultiFormatManager.is_valid(export_manager))
  
  // 创建集成连接器
  let integration_connector = @azimuth.integration.Connector.create({
    "backends": {
      "prometheus": {
        "endpoint": "http://prometheus:9090/api/v1/write",
        "format": "prometheus",
        "timeout": 5000
      },
      "jaeger": {
        "endpoint": "http://jaeger:14268/api/traces",
        "format": "jaeger",
        "timeout": 10000
      },
      "elasticsearch": {
        "endpoint": "http://elasticsearch:9200",
        "index.pattern": "azimuth-telemetry-{date}",
        "format": "json",
        "timeout": 5000
      },
      "kafka": {
        "brokers": ["kafka:9092"],
        "topic": "azimuth-telemetry",
        "format": "avro",
        "timeout": 3000
      }
    }
  })
  
  // 验证集成连接器创建成功
  assert_true(@azimuth.integration.Connector.is_valid(integration_connector))
  
  // 创建测试数据集
  let test_data = []
  let current_time = @azimuth.time.now()
  
  // 生成追踪数据
  for i = 0; i < 100; i = i + 1 {
    let span = {
      "trace_id": @azimuth.trace.generate_id(),
      "span_id": @azimuth.trace.generate_span_id(),
      "parent_span_id": if i > 0 { @azimuth.trace.generate_span_id() } else { "" },
      "operation_name": "operation-" + @azimuth.string.from_int(i % 10),
      "start_time": current_time - i * 1000,
      "end_time": current_time - i * 1000 + @azimuth.math.random() * 1000.0,
      "status": ["ok", "error", "timeout"][i % 3],
      "attributes": {
        "service.name": "test-service",
        "service.version": "1.0.0",
        "host": "server-" + @azimuth.string.from_int(i % 3),
        "user.id": "user-" + @azimuth.string.from_int(i % 20)
      },
      "events": [
        {
          "name": "event-" + @azimuth.string.from_int(i % 5),
          "timestamp": current_time - i * 1000 + 100,
          "attributes": {
            "event.type": "internal",
            "event.data": "sample-data-" + @azimuth.string.from_int(i)
          }
        }
      ]
    }
    test_data = test_data.push(span)
  }
  
  // 生成度量数据
  for i = 0; i < 200; i = i + 1 {
    let metric = {
      "name": "metric-" + @azimuth.string.from_int(i % 5),
      "type": ["counter", "gauge", "histogram"][i % 3],
      "value": @azimuth.math.random() * 1000.0,
      "timestamp": current_time - i * 500,
      "attributes": {
        "service.name": "test-service",
        "metric.type": "performance",
        "unit": ["ms", "bytes", "count"][i % 3]
      }
    }
    test_data = test_data.push(metric)
  }
  
  // 测试JSON格式导出
  let json_export_result = export_manager.export(test_data, "json")
  
  // 验证JSON导出
  assert_true(json_export_result.success)
  assert_true(json_export_result.data.length() > 0)
  
  let json_parsed = @azimuth.json.parse(json_export_result.data)
  assert_true(json_parsed.length() == test_data.length())
  
  // 测试Protobuf格式导出
  let protobuf_export_result = export_manager.export(test_data, "protobuf")
  
  // 验证Protobuf导出
  assert_true(protobuf_export_result.success)
  assert_true(protobuf_export_result.data.length() > 0)
  assert_true(protobuf_export_result.data.length() < json_export_result.data.length())  // Protobuf应更紧凑
  
  // 测试CSV格式导出
  let csv_export_result = export_manager.export(test_data, "csv")
  
  // 验证CSV导出
  assert_true(csv_export_result.success)
  assert_true(csv_export_result.data.length() > 0)
  assert_true(csv_export_result.data.contains("trace_id"))  // 应包含列标题
  
  // 测试分块导出
  let chunked_export_result = export_manager.export_chunked(test_data, "json", 50)
  
  // 验证分块导出
  assert_true(chunked_export_result.success)
  assert_eq(chunked_export_result.chunks.length(), 6)  // 300条数据，每块50条，应为6块
  assert_true(chunked_export_result.chunks[0].data.length() > 0)
  assert_true(chunked_export_result.chunks[5].data.length() <= 50)
  
  // 测试压缩导出
  let compressed_export_result = export_manager.export_compressed(test_data, "json", "gzip")
  
  // 验证压缩导出
  assert_true(compressed_export_result.success)
  assert_true(compressed_export_result.data.length() > 0)
  assert_true(compressed_export_result.data.length() < json_export_result.data.length())  // 压缩后应更小
  
  // 测试Prometheus集成
  let prometheus_export = integration_connector.prepare_for_backend(test_data, "prometheus")
  
  // 验证Prometheus格式
  assert_true(prometheus_export.success)
  assert_true(prometheus_export.data.length() > 0)
  assert_true(prometheus_export.data.contains("# TYPE"))  // Prometheus指标格式
  
  // 测试Jaeger集成
  let jaeger_export = integration_connector.prepare_for_backend(test_data, "jaeger")
  
  // 验证Jaeger格式
  assert_true(jaeger_export.success)
  assert_true(jaeger_export.data.length() > 0)
  
  let jaeger_parsed = @azimuth.json.parse(jaeger_export.data)
  assert_true(jaeger_parsed.contains_key("data"))
  assert_true(jaeger_parsed["data"].length() > 0)
  
  // 测试Elasticsearch集成
  let elasticsearch_export = integration_connector.prepare_for_backend(test_data, "elasticsearch")
  
  // 验证Elasticsearch格式
  assert_true(elasticsearch_export.success)
  assert_true(elasticsearch_export.data.length() > 0)
  
  // 测试Kafka集成
  let kafka_export = integration_connector.prepare_for_backend(test_data, "kafka")
  
  // 验证Kafka格式
  assert_true(kafka_export.success)
  assert_true(kafka_export.data.length() > 0)
  
  // 测试批量导出到多个后端
  let multi_backend_export = integration_connector.export_to_multiple_backends(test_data, ["prometheus", "jaeger", "elasticsearch"])
  
  // 验证多后端导出
  assert_true(multi_backend_export.success)
  assert_eq(multi_backend_export.results.length(), 3)
  assert_true(multi_backend_export.results.all(fn(r) { r.success }))
  
  // 测试导出统计
  let export_stats = export_manager.get_export_statistics()
  
  // 验证导出统计
  assert_true(export_stats.total_exports > 0)
  assert_true(export_stats.successful_exports > 0)
  assert_true(export_stats.failed_exports >= 0)
  assert_true(export_stats.average_export_size > 0)
  assert_true(export_stats.compression_ratio > 0.0)
  
  // 测试集成统计
  let integration_stats = integration_connector.get_integration_statistics()
  
  // 验证集成统计
  assert_true(integration_stats.total_integrations > 0)
  assert_true(integration_stats.successful_integrations > 0)
  assert_true(integration_stats.failed_integrations >= 0)
  assert_true(integration_stats.backend_stats.contains_key("prometheus"))
  assert_true(integration_stats.backend_stats.contains_key("jaeger"))
  assert_true(integration_stats.backend_stats.contains_key("elasticsearch"))
  assert_true(integration_stats.backend_stats.contains_key("kafka"))
}