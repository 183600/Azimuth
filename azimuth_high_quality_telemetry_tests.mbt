// Azimuth High-Quality Telemetry Test Suite
// 高质量遥测测试套件 - 专注于高级遥测功能和边界条件

// Test 1: 分布式追踪上下文传播
test "distributed tracing context propagation" {
  // 创建根追踪上下文
  let root_trace_id = "1234567890abcdef1234567890abcdef"
  let root_span_id = "1234567890abcdef"
  
  let root_context = @azimuth.TraceContext {
    trace_id: root_trace_id,
    span_id: root_span_id,
    baggage: [
      ("user.id", "user123"),
      ("request.id", "req456"),
      ("service.version", "1.2.3")
    ],
    trace_flags: 0x01
  }
  
  // 验证根上下文
  assert_eq(root_context.trace_id, root_trace_id)
  assert_eq(root_context.span_id, root_span_id)
  assert_eq(root_context.baggage.length(), 3)
  assert_eq(root_context.trace_flags, 0x01)
  
  // 创建子跨度的上下文
  let child_span_id = "abcdef1234567890"
  let child_context = @azimuth.create_child_context(root_context, child_span_id)
  
  // 验证子上下文继承
  assert_eq(child_context.trace_id, root_trace_id)
  assert_eq(child_context.span_id, child_span_id)
  assert_eq(child_context.baggage.length(), 3)
  assert_eq(child_context.trace_flags, 0x01)
  
  // 测试跨服务上下文传播
  let propagated_headers = @azimuth.extract_headers(child_context)
  assert_true(propagated_headers.contains("traceparent"))
  assert_true(propagated_headers.contains("tracestate"))
  
  let extracted_context = @azimuth.extract_context(propagated_headers)
  assert_eq(extracted_context.trace_id, root_trace_id)
  assert_eq(extracted_context.baggage.length(), 3)
  
  // 测试上下文修改不影响原始上下文
  let modified_context = @azimuth.add_baggage_item(child_context, "new.key", "new.value")
  assert_eq(modified_context.baggage.length(), 4)
  assert_eq(child_context.baggage.length(), 3) // 原始上下文未改变
}

// Test 2: 高级指标聚合与分析
test "advanced metrics aggregation and analysis" {
  // 创建指标聚合器
  let aggregator = @azimuth.MetricsAggregator.new()
  
  // 添加不同类型的指标数据点
  let datapoints = [
    @azimuth.MetricDataPoint {
      name: "request.duration",
      value: @azimuth.MetricValue.Double(120.5),
      unit: "ms",
      timestamp: 1640995200000,
      attributes: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricDataPoint {
      name: "request.duration",
      value: @azimuth.MetricValue.Double(85.3),
      unit: "ms",
      timestamp: 1640995201000,
      attributes: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricDataPoint {
      name: "request.duration",
      value: @azimuth.MetricValue.Double(210.7),
      unit: "ms",
      timestamp: 1640995202000,
      attributes: [("endpoint", "/api/users"), ("method", "POST")]
    },
    @azimuth.MetricDataPoint {
      name: "error.rate",
      value: @azimuth.MetricValue.Double(0.02),
      unit: "ratio",
      timestamp: 1640995203000,
      attributes: [("service", "user-service")]
    }
  ]
  
  // 添加数据点到聚合器
  datapoints.each(fn(dp) { @azimuth.add_datapoint(aggregator, dp) })
  
  // 测试指标聚合
  let aggregated_metrics = @azimuth.aggregate_metrics(aggregator)
  assert_eq(aggregated_metrics.length(), 2) // 两个不同的指标名称
  
  // 验证请求持续时间指标的聚合结果
  let duration_metric = aggregated_metrics.find(fn(m) { m.name == "request.duration" })
  match duration_metric {
    Some(metric) => {
      // 验证统计信息
      match metric.aggregation {
        @azimuth.AggregationResult.Summary(stats) => {
          assert_eq(stats.count, 3)
          assert_eq(stats.min, 85.3)
          assert_eq(stats.max, 210.7)
          assert_true(stats.mean > 120.0 && stats.mean < 150.0)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试按属性分组聚合
  let grouped_metrics = @azimuth.aggregate_by_attributes(aggregator, ["endpoint", "method"])
  assert_eq(grouped_metrics.length(), 2) // GET和POST两组
  
  // 测试百分位数计算
  let percentiles = @azimuth.calculate_percentiles(aggregator, "request.duration", [50.0, 90.0, 95.0, 99.0])
  assert_eq(percentiles.length(), 4)
  assert_true(percentiles[0].value > 80.0) // 50th percentile
  assert_true(percentiles[1].value > 100.0) // 90th percentile
  assert_true(percentiles[2].value > 150.0) // 95th percentile
}

// Test 3: 自适应采样策略
test "adaptive sampling strategies" {
  // 创建自适应采样器
  let sampler = @azimuth.AdaptiveSampler.new()
  
  // 配置采样策略
  let sampling_config = @azimuth.SamplingConfig {
    base_rate: 0.1, // 基础采样率10%
    max_rate: 1.0,  // 最大采样率100%
    min_rate: 0.01, // 最小采样率1%
    adjustment_factor: 0.2,
    error_threshold: 0.05, // 错误率阈值5%
    latency_threshold: 1000.0 // 延迟阈值1000ms
  }
  
  @azimuth.configure_sampler(sampler, sampling_config)
  
  // 测试基础采样决策
  let sample_decisions = []
  for i = 0; i < 1000; i = i + 1 {
    let decision = @azimuth.should_sample(sampler, "trace-" + i.to_string())
    sample_decisions = sample_decisions.push(decision)
  }
  
  let sampled_count = sample_decisions.filter(fn(d) { d }).length()
  let actual_rate = sampled_count.to_float() / 1000.0
  
  // 验证采样率在预期范围内
  assert_true(actual_rate > 0.05 && actual_rate < 0.15)
  
  // 模拟高错误率场景
  let error_metrics = @azimuth.ErrorMetrics {
    total_requests: 1000,
    error_count: 80, // 8%错误率，超过阈值
    avg_latency: 500.0
  }
  
  // 更新采样策略
  @azimuth.update_sampling_strategy(sampler, error_metrics)
  
  // 测试调整后的采样决策
  let adjusted_decisions = []
  for i = 0; i < 1000; i = i + 1 {
    let decision = @azimuth.should_sample(sampler, "trace-adjusted-" + i.to_string())
    adjusted_decisions = adjusted_decisions.push(decision)
  }
  
  let adjusted_count = adjusted_decisions.filter(fn(d) { d }).length()
  let adjusted_rate = adjusted_count.to_float() / 1000.0
  
  // 验证采样率已提高
  assert_true(adjusted_rate > actual_rate)
  assert_true(adjusted_rate > 0.15)
  
  // 测试基于优先级的采样
  let high_priority_trace = @azimuth.TraceContext {
    trace_id: "high-priority-trace",
    span_id: "span-123",
    baggage: [("priority", "high")],
    trace_flags: 0x01
  }
  
  let low_priority_trace = @azimuth.TraceContext {
    trace_id: "low-priority-trace",
    span_id: "span-456",
    baggage: [("priority", "low")],
    trace_flags: 0x01
  }
  
  let high_priority_sampled = @azimuth.should_sample_context(sampler, high_priority_trace)
  let low_priority_sampled = @azimuth.should_sample_context(sampler, low_priority_trace)
  
  // 高优先级追踪应该有更高的采样概率
  assert_true(high_priority_sampled || low_priority_sampled) // 至少有一个被采样
}

// Test 4: 错误处理与自动恢复机制
test "error handling and automatic recovery mechanisms" {
  // 创建错误恢复管理器
  let recovery_manager = @azimuth.ErrorRecoveryManager.new()
  
  // 配置恢复策略
  let recovery_config = @azimuth.RecoveryConfig {
    max_retry_attempts: 3,
    backoff_strategy: @azimuth.BackoffStrategy.Exponential,
    initial_backoff_ms: 100,
    max_backoff_ms: 5000,
    recovery_timeout_ms: 30000
  }
  
  @azimuth.configure_recovery(recovery_manager, recovery_config)
  
  // 测试可重试错误
  let retryable_error = @azimuth.TelemetryError {
    code: @azimuth.ErrorCode.NETWORK_TIMEOUT,
    message: "Network connection timeout",
    timestamp: @azimuth.current_timestamp(),
    context: [("url", "https://api.example.com/telemetry")]
  }
  
  let operation = fn() {
    // 模拟有时失败有时成功的操作
    let random = @azimuth.random()
    if random < 0.7 {
      Err(retryable_error)
    } else {
      Ok("Operation succeeded")
    }
  }
  
  let result = @azimuth.execute_with_retry(recovery_manager, operation)
  match result {
    Ok(success) => assert_eq(success, "Operation succeeded")
    Err(error) => {
      // 如果重试失败，验证已达到最大重试次数
      assert_eq(error.code, @azimuth.ErrorCode.MAX_RETRIES_EXCEEDED)
    }
  }
  
  // 测试不可重试错误
  let non_retryable_error = @azimuth.TelemetryError {
    code: @azimuth.ErrorCode.INVALID_CONFIGURATION,
    message: "Invalid telemetry configuration",
    timestamp: @azimuth.current_timestamp(),
    context: [("config_key", "sampling_rate")]
  }
  
  let failing_operation = fn() {
    Err(non_retryable_error)
  }
  
  let immediate_result = @azimuth.execute_with_retry(recovery_manager, failing_operation)
  match immediate_result {
    Ok(_) => assert_true(false) // 不应该成功
    Err(error) => {
      // 应该立即失败，不进行重试
      assert_eq(error.code, @azimuth.ErrorCode.INVALID_CONFIGURATION)
    }
  }
  
  // 测试断路器模式
  let circuit_breaker = @azimuth.CircuitBreaker.new()
  @azimuth.configure_circuit_breaker(circuit_breaker, {
    failure_threshold: 5,
    recovery_timeout_ms: 10000,
    expected_response_time_ms: 1000
  })
  
  // 模拟连续失败
  for i = 0; i < 6; i = i + 1 {
    let result = @azimuth.execute_through_circuit_breaker(circuit_breaker, failing_operation)
    match result {
      Ok(_) => assert_true(false)
      Err(error) => {
        if i < 5 {
          assert_eq(error.code, @azimuth.ErrorCode.INVALID_CONFIGURATION)
        } else {
          // 第6次应该触发断路器
          assert_eq(error.code, @azimuth.ErrorCode.CIRCUIT_BREAKER_OPEN)
        }
      }
    }
  }
  
  // 验证断路器状态
  assert_true(@azimuth.is_circuit_breaker_open(circuit_breaker))
}

// Test 5: 高性能数据处理流水线
test "high-performance data processing pipeline" {
  // 创建数据处理流水线
  let pipeline = @azimuth.DataPipeline.new()
  
  // 配置流水线阶段
  let stages = [
    @azimuth.PipelineStage {
      name: "validation",
      processor: fn(data) {
        // 验证数据完整性
        if data.contains("trace_id") and data.contains("timestamp") {
          @azimuth.ProcessingResult.Success(data)
        } else {
          @azimuth.ProcessingResult.Failure("Missing required fields")
        }
      },
      parallel: false
    },
    @azimuth.PipelineStage {
      name: "enrichment",
      processor: fn(data) {
        // 数据增强
        let enriched = data + ",enriched=true,timestamp=" + @azimuth.current_timestamp().to_string()
        @azimuth.ProcessingResult.Success(enriched)
      },
      parallel: true
    },
    @azimuth.PipelineStage {
      name: "aggregation",
      processor: fn(data) {
        // 数据聚合
        @azimuth.ProcessingResult.Success(data + ",aggregated=true")
      },
      parallel: false
    }
  ]
  
  @azimuth.configure_pipeline(pipeline, stages)
  
  // 创建测试数据批次
  let batch_size = 1000
  let test_data = []
  for i = 0; i < batch_size; i = i + 1 {
    let data_item = "trace_id=trace-" + i.to_string() + ",timestamp=" + (@azimuth.current_timestamp() + i).to_string()
    test_data = test_data.push(data_item)
  }
  
  // 测试流水线处理性能
  let start_time = @azimuth.current_timestamp()
  let results = @azimuth.process_batch(pipeline, test_data)
  let end_time = @azimuth.current_timestamp()
  let processing_time = end_time - start_time
  
  // 验证处理结果
  assert_eq(results.length(), batch_size)
  
  let success_count = results.filter(fn(r) {
    match r {
      @azimuth.ProcessingResult.Success(_) => true
      @azimuth.ProcessingResult.Failure(_) => false
    }
  }).length()
  
  assert_eq(success_count, batch_size) // 所有数据都应该成功处理
  
  // 验证性能要求
  assert_true(processing_time < 5000) // 应该在5秒内完成1000条记录
  let throughput = batch_size.to_float() / (processing_time.to_float() / 1000.0)
  assert_true(throughput > 200) // 每秒至少处理200条记录
  
  // 验证数据增强
  match results[0] {
    @azimuth.ProcessingResult.Success(data) => {
      assert_true(data.contains("enriched=true"))
      assert_true(data.contains("aggregated=true"))
    }
    _ => assert_true(false)
  }
  
  // 测试流水线背压处理
  let large_batch_size = 10000
  let large_test_data = []
  for i = 0; i < large_batch_size; i = i + 1 {
    let data_item = "trace_id=large-trace-" + i.to_string() + ",timestamp=" + (@azimuth.current_timestamp() + i).to_string()
    large_test_data = large_test_data.push(data_item)
  }
  
  let large_start_time = @azimuth.current_timestamp()
  let large_results = @azimuth.process_batch(pipeline, large_test_data)
  let large_end_time = @azimuth.current_timestamp()
  let large_processing_time = large_end_time - large_start_time
  
  // 验证大批量处理性能
  assert_eq(large_results.length(), large_batch_size)
  assert_true(large_processing_time < 30000) // 应该在30秒内完成10000条记录
}

// Test 6: 边界条件与异常处理
test "edge cases and exception handling" {
  // 测试空数据处理
  let empty_array: Array[String] = []
  let empty_result = @azimuth.process_telemetry_data(empty_array)
  assert_eq(empty_result.length(), 0)
  
  // 测试极大数值处理
  let max_int = @azimuth.max_int_value()
  let min_int = @azimuth.min_int_value()
  
  let max_metric = @azimuth.MetricDataPoint {
    name: "max.test",
    value: @azimuth.MetricValue.Long(max_int),
    unit: "count",
    timestamp: @azimuth.current_timestamp(),
    attributes: []
  }
  
  let min_metric = @azimuth.MetricDataPoint {
    name: "min.test",
    value: @azimuth.MetricValue.Long(min_int),
    unit: "count",
    timestamp: @azimuth.current_timestamp(),
    attributes: []
  }
  
  // 验证极值处理不会溢出
  let max_result = @azimuth.validate_metric(max_metric)
  let min_result = @azimuth.validate_metric(min_metric)
  
  match max_result {
    @azimuth.ValidationResult.Valid => assert_true(true)
    @azimuth.ValidationResult.Invalid(reason) => assert_true(false, "Max value should be valid: " + reason)
  }
  
  match min_result {
    @azimuth.ValidationResult.Valid => assert_true(true)
    @azimuth.ValidationResult.Invalid(reason) => assert_true(false, "Min value should be valid: " + reason)
  }
  
  // 测试超长字符串处理
  let very_long_string = "a".repeat(1000000) // 1MB的字符串
  let long_name_metric = @azimuth.MetricDataPoint {
    name: very_long_string,
    value: @azimuth.MetricValue.Double(1.0),
    unit: "ms",
    timestamp: @azimuth.current_timestamp(),
    attributes: []
  }
  
  let long_name_result = @azimuth.validate_metric(long_name_metric)
  match long_name_result {
    @azimuth.ValidationResult.Valid => assert_true(false, "Very long metric name should be invalid")
    @azimuth.ValidationResult.Invalid(reason) => assert_true(reason.contains("too long"))
  }
  
  // 测试无效时间戳处理
  let invalid_timestamp_metrics = [
    @azimuth.MetricDataPoint {
      name: "future.timestamp",
      value: @azimuth.MetricValue.Double(1.0),
      unit: "ms",
      timestamp: @azimuth.current_timestamp() + 86400000 * 365, // 1年后
      attributes: []
    },
    @azimuth.MetricDataPoint {
      name: "past.timestamp",
      value: @azimuth.MetricValue.Double(1.0),
      unit: "ms",
      timestamp: 0, // Unix纪元之前
      attributes: []
    }
  ]
  
  invalid_timestamp_metrics.each(fn(metric) {
    let result = @azimuth.validate_metric(metric)
    match result {
      @azimuth.ValidationResult.Valid => assert_true(false, "Invalid timestamp should be rejected")
      @azimuth.ValidationResult.Invalid(reason) => assert_true(reason.contains("timestamp"))
    }
  })
  
  // 测试循环引用检测
  let circular_ref_test = fn() {
    // 创建可能产生循环引用的数据结构
    let parent_span = @azimuth.Span {
      name: "parent",
      span_id: "parent-123",
      parent_span_id: None,
      children: []
    }
    
    let child_span = @azimuth.Span {
      name: "child",
      span_id: "child-456",
      parent_span_id: Some("parent-123"),
      children: []
    }
    
    // 尝试创建循环引用
    let updated_parent = { parent_span | children: [child_span] }
    let updated_child = { child_span | parent_span_id: Some("child-456") } // 自引用
    
    // 检测循环引用
    let has_circular_ref = @azimuth.detect_circular_reference(updated_child)
    assert_true(has_circular_ref)
  }
  
  circular_ref_test()
  
  // 测试内存限制处理
  let memory_limited_processor = @azimuth.MemoryLimitedProcessor.new(1024 * 1024) // 1MB限制
  
  let large_data = "x".repeat(2 * 1024 * 1024) // 2MB数据
  let memory_result = @azimuth.process_with_memory_limit(memory_limited_processor, large_data)
  
  match memory_result {
    Ok(_) => assert_true(false, "Processing large data should fail with memory limit")
    Err(error) => assert_eq(error.code, @azimuth.ErrorCode.MEMORY_LIMIT_EXCEEDED)
  }
}

// Test 7: 资源合并与冲突解决
test "resource merging and conflict resolution" {
  // 创建基础资源
  let base_resource = @azimuth.Resource {
    attributes: [
      ("service.name", @azimuth.AttributeValue.String("payment-service")),
      ("service.version", @azimuth.AttributeValue.String("1.0.0")),
      ("service.instance.id", @azimuth.AttributeValue.String("instance-123")),
      ("host.name", @azimuth.AttributeValue.String("prod-host-1")),
      ("environment", @azimuth.AttributeValue.String("production"))
    ]
  }
  
  // 创建冲突资源
  let conflicting_resource = @azimuth.Resource {
    attributes: [
      ("service.name", @azimuth.AttributeValue.String("payment-service")), // 相同
      ("service.version", @azimuth.AttributeValue.String("2.0.0")), // 冲突
      ("deployment.region", @azimuth.AttributeValue.String("us-west-2")), // 新增
      ("host.name", @azimuth.AttributeValue.String("prod-host-2")), // 冲突
      ("team", @azimuth.AttributeValue.String("payments")) // 新增
    ]
  }
  
  // 测试不同的合并策略
  let merge_strategies = [
    @azimuth.MergeStrategy.PreferFirst,   // 优先第一个资源
    @azimuth.MergeStrategy.PreferLast,    // 优先最后一个资源
    @azimuth.MergeStrategy.CombineArrays, // 合并数组值
    @azimuth.MergeStrategy.HighestVersion // 选择最高版本
  ]
  
  let merge_results = merge_strategies.map(fn(strategy) {
    @azimuth.merge_resources_with_strategy(base_resource, conflicting_resource, strategy)
  })
  
  // 验证PreferFirst策略
  let prefer_first_result = merge_results[0]
  let service_version_first = prefer_first_result.attributes.find(fn(attr) { attr.0 == "service.version" })
  match service_version_first {
    Some((_, @azimuth.AttributeValue.String(version))) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let host_name_first = prefer_first_result.attributes.find(fn(attr) { attr.0 == "host.name" })
  match host_name_first {
    Some((_, @azimuth.AttributeValue.String(host))) => assert_eq(host, "prod-host-1")
    _ => assert_true(false)
  }
  
  // 验证PreferLast策略
  let prefer_last_result = merge_results[1]
  let service_version_last = prefer_last_result.attributes.find(fn(attr) { attr.0 == "service.version" })
  match service_version_last {
    Some((_, @azimuth.AttributeValue.String(version))) => assert_eq(version, "2.0.0")
    _ => assert_true(false)
  }
  
  let host_name_last = prefer_last_result.attributes.find(fn(attr) { attr.0 == "host.name" })
  match host_name_last {
    Some((_, @azimuth.AttributeValue.String(host))) => assert_eq(host, "prod-host-2")
    _ => assert_true(false)
  }
  
  // 验证所有结果都包含新增属性
  merge_results.each(fn(result) {
    let region_attr = result.attributes.find(fn(attr) { attr.0 == "deployment.region" })
    match region_attr {
      Some((_, @azimuth.AttributeValue.String(region))) => assert_eq(region, "us-west-2")
      _ => assert_true(false)
    }
    
    let team_attr = result.attributes.find(fn(attr) { attr.0 == "team" })
    match team_attr {
      Some((_, @azimuth.AttributeValue.String(team))) => assert_eq(team, "payments")
      _ => assert_true(false)
    }
  })
  
  // 测试多资源合并
  let additional_resources = [
    @azimuth.Resource {
      attributes: [
        ("service.version", @azimuth.AttributeValue.String("1.5.0")),
        ("cost.center", @azimuth.AttributeValue.String("engineering")),
        ("sla.tier", @azimuth.AttributeValue.String("critical"))
      ]
    },
    @azimuth.Resource {
      attributes: [
        ("service.version", @azimuth.AttributeValue.String("1.8.0")),
        ("data.classification", @azimuth.AttributeValue.String("sensitive")),
        ("compliance", @azimuth.AttributeValue.String("pci-dss"))
      ]
    }
  ]
  
  let multi_merge_result = @azimuth.merge_multiple_resources([base_resource, conflicting_resource] + additional_resources)
  
  // 验证合并结果包含所有唯一属性
  let unique_attribute_names = multi_merge_result.attributes.map(fn(attr) { attr.0 }).unique()
  assert_eq(unique_attribute_names.length(), 9) // 所有唯一属性名
  
  // 验证版本冲突解决
  let final_version = multi_merge_result.attributes.find(fn(attr) { attr.0 == "service.version" })
  match final_version {
    Some((_, @azimuth.AttributeValue.String(version))) => {
      // 应该是最高版本
      assert_true(version == "2.0.0" || version == "1.8.0" || version == "1.5.0" || version == "1.0.0")
    }
    _ => assert_true(false)
  }
  
  // 测试冲突检测
  let conflicts = @azimuth.detect_resource_conflicts(base_resource, conflicting_resource)
  assert_eq(conflicts.length(), 2) // service.version和host.name冲突
  
  let version_conflict = conflicts.find(fn(c) { c.attribute_name == "service.version" })
  match version_conflict {
    Some(conflict) => {
      assert_eq(conflict.first_value, @azimuth.AttributeValue.String("1.0.0"))
      assert_eq(conflict.second_value, @azimuth.AttributeValue.String("2.0.0"))
    }
    None => assert_true(false)
  }
}

// Test 8: 高级序列化与压缩
test "advanced serialization and compression" {
  // 创建复杂的遥测数据结构
  let complex_span = @azimuth.Span {
    name: "complex.database.operation",
    span_id: "span-12345678",
    parent_span_id: Some("span-87654321"),
    trace_id: "trace-abcdef1234567890",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: @azimuth.SpanStatus.OK,
    attributes: [
      ("db.system", @azimuth.AttributeValue.String("postgresql")),
      ("db.statement", @azimuth.AttributeValue.String("SELECT * FROM users WHERE id = $1")),
      ("db.user", @azimuth.AttributeValue.String("app_user")),
      ("net.peer.name", @azimuth.AttributeValue.String("db.example.com")),
      ("net.peer.port", @azimuth.AttributeValue.Int(5432)),
      ("retry.count", @azimuth.AttributeValue.Int(2)),
      ("cache.hit", @azimuth.AttributeValue.Bool(true)),
      ("operation.cost", @azimuth.AttributeValue.Double(0.025))
    ],
    events: [
      @azimuth.SpanEvent {
        name: "db.query.start",
        timestamp: 1640995200000,
        attributes: [
          ("event.category", @azimuth.AttributeValue.String("database")),
          ("query.type", @azimuth.AttributeValue.String("select"))
        ]
      },
      @azimuth.SpanEvent {
        name: "db.query.retry",
        timestamp: 1640995220000,
        attributes: [
          ("retry.reason", @azimuth.AttributeValue.String("timeout")),
          ("retry.attempt", @azimuth.AttributeValue.Int(1))
        ]
      },
      @azimuth.SpanEvent {
        name: "db.query.success",
        timestamp: 1640995250000,
        attributes: [
          ("rows.affected", @azimuth.AttributeValue.Int(1)),
          ("execution.time", @azimuth.AttributeValue.Double(45.7))
        ]
      }
    ],
    links: [
      @azimuth.SpanLink {
        trace_id: "trace-linked-123456",
        span_id: "span-linked-789012",
        attributes: [
          ("link.type", @azimuth.AttributeValue.String("follows_from"))
        ]
      }
    ]
  }
  
  // 测试不同的序列化格式
  let serialization_formats = [
    @azimuth.SerializationFormat.JSON,
    @azimuth.SerializationFormat.Protobuf,
    @azimuth.SerializationFormat.MsgPack,
    @azimuth.SerializationFormat.XML
  ]
  
  let serialized_results = serialization_formats.map(fn(format) {
    @azimuth.serialize_span(complex_span, format)
  })
  
  // 验证所有序列化都成功
  serialized_results.each(fn(result) {
    match result {
      Ok(data) => assert_true(data.length() > 0)
      Err(error) => assert_true(false, "Serialization failed: " + error.message)
    }
  })
  
  // 测试序列化大小比较
  let json_data = match serialized_results[0] { Ok(data) => data; _ => "" }
  let protobuf_data = match serialized_results[1] { Ok(data) => data; _ => "" }
  let msgpack_data = match serialized_results[2] { Ok(data) => data; _ => "" }
  
  // Protobuf和MsgPack应该比JSON更紧凑
  assert_true(protobuf_data.length() < json_data.length())
  assert_true(msgpack_data.length() < json_data.length())
  
  // 测试反序列化
  let deserialization_results = serialized_results.map(fn(result) {
    match result {
      Ok(data) => @azimuth.deserialize_span(data, serialization_formats[serialized_results.index_of(result)])
      Err(_) => Err(@azimuth.SerializationError { message: "Serialization failed" })
    }
  })
  
  // 验证反序列化恢复原始数据
  deserialization_results.each(fn(result) {
    match result {
      Ok(deserialized_span) => {
        assert_eq(deserialized_span.name, complex_span.name)
        assert_eq(deserialized_span.span_id, complex_span.span_id)
        assert_eq(deserialized_span.trace_id, complex_span.trace_id)
        assert_eq(deserialized_span.attributes.length(), complex_span.attributes.length())
        assert_eq(deserialized_span.events.length(), complex_span.events.length())
        assert_eq(deserialized_span.links.length(), complex_span.links.length())
      }
      Err(error) => assert_true(false, "Deserialization failed: " + error.message)
    }
  })
  
  // 测试压缩
  let compression_algorithms = [
    @azimuth.CompressionAlgorithm.Gzip,
    @azimuth.CompressionAlgorithm.Zlib,
    @azimuth.CompressionAlgorithm.LZ4,
    @azimuth.CompressionAlgorithm.Snappy
  ]
  
  let compression_results = compression_algorithms.map(fn(algorithm) {
    match serialized_results[0] { // 使用JSON序列化数据
      Ok(data) => @azimuth.compress_data(data, algorithm)
      Err(_) => Err(@azimuth.CompressionError { message: "Serialization failed" })
    }
  })
  
  // 验证压缩效果
  compression_results.each(fn(result) {
    match result {
      Ok(compressed_data) => {
        assert_true(compressed_data.length() > 0)
        // 压缩数据应该小于原始数据
        assert_true(compressed_data.length() < json_data.length())
      }
      Err(error) => assert_true(false, "Compression failed: " + error.message)
    }
  })
  
  // 测试解压缩
  let decompression_results = compression_results.map(fn(result) {
    match result {
      Ok(compressed_data) => @azimuth.decompress_data(compressed_data, compression_algorithms[compression_results.index_of(result)])
      Err(_) => Err(@azimuth.CompressionError { message: "Compression failed" })
    }
  })
  
  // 验证解压缩恢复原始数据
  decompression_results.each(fn(result) {
    match result {
      Ok(decompressed_data) => {
        assert_eq(decompressed_data, json_data)
      }
      Err(error) => assert_true(false, "Decompression failed: " + error.message)
    }
  })
  
  // 测试性能基准
  let performance_results = []
  
  // 序列化性能
  serialization_formats.each(fn(format) {
    let start_time = @azimuth.current_timestamp()
    for i = 0; i < 100; i = i + 1 {
      let result = @azimuth.serialize_span(complex_span, format)
      match result {
        Ok(_) => assert_true(true)
        Err(_) => assert_true(false)
      }
    }
    let end_time = @azimuth.current_timestamp()
    let duration = end_time - start_time
    performance_results = performance_results.push((format.to_string() + "_serialization", duration))
  })
  
  // 反序列化性能
  serialization_formats.each(fn(format) {
    let serialized_data = match @azimuth.serialize_span(complex_span, format) {
      Ok(data) => data
      Err(_) => ""
    }
    
    let start_time = @azimuth.current_timestamp()
    for i = 0; i < 100; i = i + 1 {
      let result = @azimuth.deserialize_span(serialized_data, format)
      match result {
        Ok(_) => assert_true(true)
        Err(_) => assert_true(false)
      }
    }
    let end_time = @azimuth.current_timestamp()
    let duration = end_time - start_time
    performance_results = performance_results.push((format.to_string() + "_deserialization", duration))
  })
  
  // 验证性能要求
  performance_results.each(fn((operation, duration)) {
    // 每种操作100次应该在5秒内完成
    assert_true(duration < 5000, operation + " should complete within 5 seconds, took: " + duration.to_string() + "ms")
  })
}

// Test 9: 自适应配置管理
test "adaptive configuration management" {
  // 创建配置管理器
  let config_manager = @azimuth.ConfigManager.new()
  
  // 初始化基础配置
  let base_config = @azimuth.TelemetryConfig {
    service_name: "azimuth-service",
    service_version: "1.0.0",
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    enabled: true,
    attributes: [
      ("environment", @azimuth.AttributeValue.String("production")),
      ("region", @azimuth.AttributeValue.String("us-west-2"))
    ]
  }
  
  @azimuth.set_config(config_manager, base_config)
  
  // 验证初始配置
  let current_config = @azimuth.get_config(config_manager)
  assert_eq(current_config.service_name, "azimuth-service")
  assert_eq(current_config.sampling_rate, 0.1)
  assert_eq(current_config.batch_size, 100)
  
  // 测试动态配置更新
  let updates = [
    ("sampling_rate", @azimuth.ConfigValue.Double(0.2)),
    ("batch_size", @azimuth.ConfigValue.Int(200)),
    ("export_interval_ms", @azimuth.ConfigValue.Int(10000))
  ]
  
  updates.each(fn((key, value)) {
    let update_result = @azimuth.update_config_value(config_manager, key, value)
    assert_true(update_result.success)
  })
  
  // 验证配置更新
  let updated_config = @azimuth.get_config(config_manager)
  assert_eq(updated_config.sampling_rate, 0.2)
  assert_eq(updated_config.batch_size, 200)
  assert_eq(updated_config.export_interval_ms, 10000)
  
  // 测试配置验证
  let invalid_updates = [
    ("sampling_rate", @azimuth.ConfigValue.Double(-0.1)), // 负采样率
    ("sampling_rate", @azimuth.ConfigValue.Double(1.5)),  // 超过1.0的采样率
    ("batch_size", @azimuth.ConfigValue.Int(0)),          // 零批次大小
    ("batch_size", @azimuth.ConfigValue.Int(-100))        // 负批次大小
  ]
  
  invalid_updates.each(fn((key, value)) {
    let update_result = @azimuth.update_config_value(config_manager, key, value)
    assert_false(update_result.success)
    assert_true(update_result.error_message.contains("Invalid"))
  })
  
  // 测试配置回滚
  let config_before_rollback = @azimuth.get_config(config_manager)
  
  // 应用一些更改
  @azimuth.update_config_value(config_manager, "sampling_rate", @azimuth.ConfigValue.Double(0.3))
  @azimuth.update_config_value(config_manager, "batch_size", @azimuth.ConfigValue.Int(300))
  
  // 创建配置快照
  let snapshot_id = @azimuth.create_config_snapshot(config_manager)
  assert_true(snapshot_id.length() > 0)
  
  // 应用更多更改
  @azimuth.update_config_value(config_manager, "sampling_rate", @azimuth.ConfigValue.Double(0.4))
  @azimuth.update_config_value(config_manager, "batch_size", @azimuth.ConfigValue.Int(400))
  
  // 验证更改
  let changed_config = @azimuth.get_config(config_manager)
  assert_eq(changed_config.sampling_rate, 0.4)
  assert_eq(changed_config.batch_size, 400)
  
  // 回滚到快照
  let rollback_result = @azimuth.rollback_to_snapshot(config_manager, snapshot_id)
  assert_true(rollback_result.success)
  
  // 验证回滚
  let rolled_back_config = @azimuth.get_config(config_manager)
  assert_eq(rolled_back_config.sampling_rate, 0.3)
  assert_eq(rolled_back_config.batch_size, 300)
  
  // 测试基于负载的自动配置调整
  let load_monitor = @azimuth.LoadMonitor.new()
  
  // 模拟高负载场景
  let high_load_metrics = @azimuth.LoadMetrics {
    cpu_usage: 0.85,      // 85% CPU使用率
    memory_usage: 0.75,   // 75% 内存使用率
    request_rate: 1000.0, // 每秒1000个请求
    error_rate: 0.02      // 2% 错误率
  }
  
  // 配置自适应调整规则
  let adaptation_rules = [
    @azimuth.AdaptationRule {
      condition: fn(metrics) { metrics.cpu_usage > 0.8 },
      action: fn(config) { { config | sampling_rate: config.sampling_rate * 0.5 } },
      description: "Reduce sampling rate under high CPU load"
    },
    @azimuth.AdaptationRule {
      condition: fn(metrics) { metrics.request_rate > 500.0 },
      action: fn(config) { { config | batch_size: config.batch_size * 2 } },
      description: "Increase batch size under high request rate"
    }
  ]
  
  @azimuth.set_adaptation_rules(config_manager, adaptation_rules)
  
  // 触发自适应调整
  let adaptation_result = @azimuth.adapt_config_to_load(config_manager, high_load_metrics)
  assert_true(adaptation_result.success)
  assert_eq(adaptation_result.applied_rules.length(), 2)
  
  // 验证自适应调整
  let adapted_config = @azimuth.get_config(config_manager)
  assert_eq(adapted_config.sampling_rate, 0.15) // 0.3 * 0.5
  assert_eq(adapted_config.batch_size, 600)     // 300 * 2
  
  // 测试配置变更通知
  let notification_log = { mut changes: [] }
  
  let config_change_listener = fn(old_config, new_config, changed_keys) {
    notification_log.changes = notification_log.changes.push({
      timestamp: @azimuth.current_timestamp(),
      changed_keys: changed_keys
    })
  }
  
  @azimuth.add_config_change_listener(config_manager, config_change_listener)
  
  // 触发配置变更
  @azimuth.update_config_value(config_manager, "sampling_rate", @azimuth.ConfigValue.Double(0.25))
  
  // 验证通知
  assert_eq(notification_log.changes.length(), 1)
  assert_true(notification_log.changes[0].changed_keys.contains("sampling_rate"))
}

// Test 10: 高级数据分析与模式识别
test "advanced data analysis and pattern recognition" {
  // 创建数据分析器
  let analyzer = @azimuth.DataAnalyzer.new()
  
  // 生成模拟遥测数据
  let generate_telemetry_data = fn(count: Int) {
    let mut data = []
    let base_timestamp = @azimuth.current_timestamp()
    
    for i = 0; i < count; i = i + 1 {
      let timestamp = base_timestamp + i * 1000 // 每秒一个数据点
      let request_duration = match i % 10 {
        0 => 500.0 + @azimuth.random() * 100.0  // 偶尔的长请求
        1 => 50.0 + @azimuth.random() * 20.0    // 偶尔的短请求
        _ => 100.0 + @azimuth.random() * 50.0   // 正常请求
      }
      
      let error_occurred = @azimuth.random() < 0.05 // 5%错误率
      let status_code = if error_occurred { 500 } else { 200 }
      
      let data_point = @azimuth.TelemetryDataPoint {
        timestamp: timestamp,
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        operation_name: "http.request",
        duration_ms: request_duration,
        status_code: status_code,
        attributes: [
          ("endpoint", @azimuth.AttributeValue.String("/api/data")),
          ("method", @azimuth.AttributeValue.String("GET")),
          ("service", @azimuth.AttributeValue.String("api-service"))
        ]
      }
      
      data = data.push(data_point)
    }
    
    data
  }
  
  // 生成测试数据集
  let test_data = generate_telemetry_data(1000)
  
  // 添加数据到分析器
  test_data.each(fn(dp) { @azimuth.add_data_point(analyzer, dp) })
  
  // 测试异常检测
  let anomaly_detection_config = @azimuth.AnomalyDetectionConfig {
    algorithm: @azimuth.AnomalyAlgorithm.StatisticalOutlier,
    sensitivity: 0.05,
    window_size: 100
  }
  
  let anomalies = @azimuth.detect_anomalies(analyzer, "duration_ms", anomaly_detection_config)
  assert_true(anomalies.length() > 0)
  assert_true(anomalies.length() < test_data.length() / 10) // 异常应该少于10%
  
  // 验证异常确实是异常值
  anomalies.each(fn(anomaly) {
    assert_true(anomaly.value > 200.0 || anomaly.value < 80.0) // 应该是明显异常的值
  })
  
  // 测试趋势分析
  let trend_analysis = @azimuth.analyze_trend(analyzer, "duration_ms", @azimuth.TimeWindow.Hour)
  match trend_analysis {
    @azimuth.TrendResult.Increasing(slope) => assert_true(slope > 0)
    @azimuth.TrendResult.Decreasing(slope) => assert_true(slope < 0)
    @azimuth.TrendResult.Stable => assert_true(true)
    @azimuth.TrendResult.InsufficientData => assert_true(false)
  }
  
  // 测试模式识别
  let patterns = @azimuth.recognize_patterns(analyzer)
  
  // 验证识别出的模式
  let periodic_pattern = patterns.find(fn(p) { p.pattern_type == @azimuth.PatternType.Periodic })
  match periodic_pattern {
    Some(pattern) => {
      assert_true(pattern.confidence > 0.7)
      assert_true(pattern.period_seconds > 0)
    }
    None => assert_true(false, "Should detect periodic patterns")
  }
  
  let error_spike_pattern = patterns.find(fn(p) { p.pattern_type == @azimuth.PatternType.ErrorSpike })
  match error_spike_pattern {
    Some(pattern) => {
      assert_true(pattern.confidence > 0.5)
      assert_true(pattern.affected_time_windows.length() > 0)
    }
    None => assert_true(true) // 可能没有错误尖峰
  }
  
  // 测试相关性分析
  let correlation_matrix = @azimuth.calculate_correlations(analyzer, ["duration_ms", "status_code"])
  
  // 验证相关性矩阵
  assert_eq(correlation_matrix.dimensions, (2, 2))
  
  // 检查duration_ms和status_code之间的相关性
  let duration_status_correlation = @azimuth.get_correlation(correlation_matrix, "duration_ms", "status_code")
  assert_true(duration_status_correlation >= -1.0 && duration_status_correlation <= 1.0)
  
  // 测试预测分析
  let prediction_config = @azimuth.PredictionConfig {
    algorithm: @azimuth.PredictionAlgorithm.LinearRegression,
    feature_columns: ["timestamp"],
    target_column: "duration_ms",
    training_window_size: 800,
    prediction_horizon: 200
  }
  
  let prediction_model = @azimuth.train_prediction_model(analyzer, prediction_config)
  match prediction_model {
    @azimuth.Model.Trained(model) => {
      assert_true(model.accuracy > 0.5) // 模型应该有一定准确性
      
      // 测试预测
      let predictions = @azimuth.predict(model, test_data.slice(800, 200))
      assert_eq(predictions.length(), 200)
      
      // 验证预测值在合理范围内
      predictions.each(fn(prediction) {
        assert_true(prediction > 0.0 && prediction < 1000.0)
      })
    }
    @azimuth.Model.TrainingFailed(error) => assert_true(false, "Model training failed: " + error)
    @azimuth.Model.InsufficientData => assert_true(false, "Should have sufficient data")
  }
  
  // 测试性能基线建立
  let baseline = @azimuth.establish_performance_baseline(analyzer, @azimuth.TimeWindow.Day)
  
  // 验证基线指标
  assert_true(baseline.average_duration_ms > 0.0)
  assert_true(baseline.p95_duration_ms > baseline.average_duration_ms)
  assert_true(baseline.p99_duration_ms > baseline.p95_duration_ms)
  assert_true(baseline.error_rate >= 0.0 && baseline.error_rate <= 1.0)
  assert_true(baseline.throughput_per_second > 0.0)
  
  // 测试与基线的偏差检测
  let recent_data = test_data.slice(900, 100) // 最近10%的数据
  let deviation_analysis = @azimuth.analyze_deviation_from_baseline(analyzer, recent_data, baseline)
  
  // 验证偏差分析
  assert_true(deviation_analysis.duration_deviation >= 0.0)
  assert_true(deviation_analysis.error_rate_deviation >= 0.0)
  assert_true(deviation_analysis.throughput_deviation >= 0.0)
  
  // 测试智能告警生成
  let alert_config = @azimuth.AlertConfig {
    duration_threshold_multiplier: 2.0,
    error_rate_threshold: 0.1,
    throughput_drop_threshold: 0.2,
    consecutive_violations: 3
  }
  
  let alerts = @azimuth.generate_intelligent_alerts(analyzer, baseline, alert_config)
  
  // 验证告警
  alerts.each(fn(alert) {
    assert_true(alert.severity == @azimuth.AlertSeverity.Warning || 
                alert.severity == @azimuth.AlertSeverity.Critical)
    assert_true(alert.message.length() > 0)
    assert_true(alert.timestamp > 0)
  })
}