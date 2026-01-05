// Azimuth Premium Quality Test Suite
// 高质量测试用例，专注于核心功能和边界条件

// 测试1: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建遥测数据管理器
  let telemetry_manager = TelemetryManager::new()
  
  // 配置数据保留策略
  TelemetryManager::set_retention_policy(telemetry_manager, {
    span_retention_days: 7,      // Span数据保留7天
    metric_retention_days: 30,   // 指标数据保留30天
    log_retention_days: 14,      // 日志数据保留14天
    max_storage_size: 1073741824 // 最大1GB存储
  })
  
  // 创建测试span数据
  let test_span = Span::new("test.operation", Server, SpanContext::new("trace-123", "span-456", true, ""))
  Span::set_attribute(test_span, "service.name", StringValue("test.service"))
  Span::set_attribute(test_span, "operation.type", StringValue("database.query"))
  
  // 记录span开始时间
  let start_time = Time::now()
  Span::set_start_time(test_span, start_time)
  
  // 模拟操作执行
  Thread::sleep(100) // 模拟100ms操作
  
  // 记录span结束时间
  let end_time = Time::now()
  Span::set_end_time(test_span, end_time)
  
  // 计算持续时间
  let duration = Span::get_duration(test_span)
  assert_true(duration >= 90 && duration <= 110) // 允许10ms误差
  
  // 添加到管理器
  TelemetryManager::add_span(telemetry_manager, test_span)
  
  // 验证数据存储
  let stored_spans = TelemetryManager::get_spans_by_trace(telemetry_manager, "trace-123")
  assert_eq(stored_spans.length(), 1)
  assert_eq(stored_spans[0].span_id, "span-456")
  
  // 测试数据过期
  let old_timestamp = Time::now() - (8 * 24 * 60 * 60 * 1000) // 8天前
  let old_span = Span::new("old.operation", Server, SpanContext::new("trace-old", "span-old", true, ""))
  Span::set_start_time(old_span, old_timestamp)
  Span::set_end_time(old_span, old_timestamp + 1000)
  
  TelemetryManager::add_span(telemetry_manager, old_span)
  
  // 执行数据清理
  let cleanup_result = TelemetryManager::cleanup_expired_data(telemetry_manager)
  
  // 验证清理结果
  assert_true(cleanup_result.expired_spans_count >= 1)
  
  // 验证新数据仍然存在
  let remaining_spans = TelemetryManager::get_spans_by_trace(telemetry_manager, "trace-123")
  assert_eq(remaining_spans.length(), 1)
  
  // 验证旧数据已清理
  let old_spans = TelemetryManager::get_spans_by_trace(telemetry_manager, "trace-old")
  assert_eq(old_spans.length(), 0)
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播测试" {
  // 创建追踪上下文管理器
  let context_manager = TraceContextManager::new()
  
  // 创建根追踪上下文
  let root_context = TraceContextManager::create_root_context(context_manager, "service-a")
  assert_true(root_context.trace_id.length() > 0)
  assert_true(root_context.span_id.length() > 0)
  assert_true(root_context.is_sampled)
  
  // 创建子上下文
  let child_context_1 = TraceContextManager::create_child_context(context_manager, root_context, "service-b")
  assert_eq(child_context_1.trace_id, root_context.trace_id)
  assert_true(child_context_1.span_id != root_context.span_id)
  assert_eq(child_context_1.parent_span_id, Some(root_context.span_id))
  
  let child_context_2 = TraceContextManager::create_child_context(context_manager, child_context_1, "service-c")
  assert_eq(child_context_2.trace_id, root_context.trace_id)
  assert_true(child_context_2.span_id != child_context_1.span_id)
  assert_eq(child_context_2.parent_span_id, Some(child_context_1.span_id))
  
  // 测试上下文序列化
  let serialized_context = TraceContextManager::serialize_context(context_manager, child_context_2)
  assert_true(serialized_context.length() > 0)
  
  // 测试上下文反序列化
  let deserialized_context = TraceContextManager::deserialize_context(context_manager, serialized_context)
  assert_eq(deserialized_context.trace_id, child_context_2.trace_id)
  assert_eq(deserialized_context.span_id, child_context_2.span_id)
  assert_eq(deserialized_context.parent_span_id, child_context_2.parent_span_id)
  
  // 测试跨进程传播
  let carrier = TraceContextManager::create_carrier(context_manager)
  TraceContextManager::inject_context(context_manager, child_context_2, carrier)
  
  let extracted_context = TraceContextManager::extract_context(context_manager, carrier)
  assert_eq(extracted_context.trace_id, child_context_2.trace_id)
  assert_eq(extracted_context.span_id, child_context_2.span_id)
  
  // 测试baggage项传播
  TraceContextManager::set_baggage_item(context_manager, child_context_2, "user.id", "user-12345")
  TraceContextManager::set_baggage_item(context_manager, child_context_2, "request.id", "req-67890")
  
  let propagated_context = TraceContextManager::propagate_context(context_manager, child_context_2, carrier)
  let baggage_items = TraceContextManager::get_baggage_items(propagated_context)
  
  assert_eq(baggage_items.length(), 2)
  assert_true(baggage_items.contains(("user.id", "user-12345")))
  assert_true(baggage_items.contains(("request.id", "req-67890")))
}

// 测试3: 遥测指标聚合和计算
test "遥测指标聚合和计算测试" {
  // 创建指标管理器
  let metric_manager = MetricManager::new()
  
  // 创建计数器指标
  let request_counter = MetricManager::create_counter(metric_manager, "http.requests.total", "Total HTTP requests")
  MetricManager::add_label(request_counter, ("method", "GET"))
  MetricManager::add_label(request_counter, ("status", "200"))
  
  // 记录指标值
  for i in 0..=100 {
    MetricManager::increment(request_counter, 1.0)
  }
  
  // 创建另一个计数器
  let error_counter = MetricManager::create_counter(metric_manager, "http.errors.total", "Total HTTP errors")
  MetricManager::add_label(error_counter, ("method", "POST"))
  MetricManager::add_label(error_counter, ("status", "500"))
  
  for i in 0..=10 {
    MetricManager::increment(error_counter, 1.0)
  }
  
  // 创建直方图指标
  let latency_histogram = MetricManager::create_histogram(metric_manager, "http.request.duration", "HTTP request duration")
  MetricManager::add_label(latency_histogram, ("endpoint", "/api/users"))
  
  // 记录延迟值
  let latency_values = [10.0, 25.0, 50.0, 75.0, 100.0, 150.0, 200.0, 300.0, 500.0, 1000.0]
  for i in 0..=50 {
    let latency = latency_values[i % latency_values.length()]
    MetricManager::record(latency_histogram, latency)
  }
  
  // 创建仪表盘指标
  let active_connections = MetricManager::create_gauge(metric_manager, "db.connections.active", "Active database connections")
  
  // 设置仪表盘值
  MetricManager::set(active_connections, 25.0)
  
  // 模拟连接数变化
  for i in 0..=10 {
    let new_value = 25.0 + (i % 10) - 5.0 // 在20-30之间波动
    MetricManager::set(active_connections, new_value)
  }
  
  // 执行指标聚合
  let aggregation_result = MetricManager::aggregate_metrics(metric_manager, {
    time_range: TimeRange::last_hour(),
    aggregation_interval: 60000 // 1分钟
  })
  
  // 验证聚合结果
  assert_true(aggregation_result.metrics.length() >= 3)
  
  // 检查计数器聚合
  let request_metric = aggregation_result.metrics.find(fn(m) { m.name == "http.requests.total" })
  assert_true(request_metric != None)
  
  match request_metric {
    Some(metric) => {
      assert_eq(metric.aggregated_value, 101.0)
      assert_true(metric.labels.contains(("method", "GET")))
      assert_true(metric.labels.contains(("status", "200")))
    }
    None => assert_true(false)
  }
  
  // 检查错误计数器聚合
  let error_metric = aggregation_result.metrics.find(fn(m) { m.name == "http.errors.total" })
  assert_true(error_metric != None)
  
  match error_metric {
    Some(metric) => {
      assert_eq(metric.aggregated_value, 11.0)
    }
    None => assert_true(false)
  }
  
  // 检查直方图聚合
  let latency_metric = aggregation_result.metrics.find(fn(m) { m.name == "http.request.duration" })
  assert_true(latency_metric != None)
  
  match latency_metric {
    Some(metric) => {
      assert_true(metric.histogram_values.contains_key("count"))
      assert_true(metric.histogram_values.contains_key("sum"))
      assert_true(metric.histogram_values.contains_key("min"))
      assert_true(metric.histogram_values.contains_key("max"))
      
      // 验证百分位数
      assert_true(metric.histogram_values.contains_key("p50"))
      assert_true(metric.histogram_values.contains_key("p95"))
      assert_true(metric.histogram_values.contains_key("p99"))
    }
    None => assert_true(false)
  }
  
  // 检查仪表盘聚合
  let connection_metric = aggregation_result.metrics.find(fn(m) { m.name == "db.connections.active" })
  assert_true(connection_metric != None)
  
  match connection_metric {
    Some(metric) => {
      assert_true(metric.gauge_value >= 20.0)
      assert_true(metric.gauge_value <= 30.0)
    }
    None => assert_true(false)
  }
  
  // 测试指标计算
  let calculated_metrics = MetricManager::calculate_derived_metrics(metric_manager, aggregation_result)
  
  // 验证错误率计算
  let error_rate = calculated_metrics.find(fn(m) { m.name == "http.error_rate" })
  assert_true(error_rate != None)
  
  match error_rate {
    Some(metric) => {
      let expected_rate = 11.0 / (101.0 + 11.0)
      assert_true(metric.calculated_value >= expected_rate - 0.001)
      assert_true(metric.calculated_value <= expected_rate + 0.001)
    }
    None => assert_true(false)
  }
}

// 测试4: 遥测数据质量验证
test "遥测数据质量验证测试" {
  // 创建数据质量验证器
  let quality_validator = DataQualityValidator::new()
  
  // 配置验证规则
  QualityValidator::add_rule(quality_validator, {
    name: "span_duration_validation",
    type: "range_check",
    field: "duration",
    min_value: 0,
    max_value: 3600000, // 最大1小时
    severity: "error"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "trace_id_format_validation",
    type: "regex_check",
    field: "trace_id",
    pattern: "^[a-f0-9]{32}$|^[a-zA-Z0-9-]{10,}$", // 十六进制或通用格式
    severity: "error"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "service_name_validation",
    type: "not_empty",
    field: "service_name",
    severity: "warning"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "timestamp_consistency_validation",
    type: "consistency_check",
    fields: ["start_time", "end_time"],
    rule: "end_time >= start_time",
    severity: "error"
  })
  
  // 创建有效测试数据
  let valid_span = {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "span-123456",
    parent_span_id: Some("parent-123456"),
    operation_name: "valid.operation",
    service_name: "valid.service",
    start_time: 1640995200000,
    end_time: 1640995200500,
    duration: 500,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.status_code", "200")
    ]
  }
  
  // 验证有效数据
  let valid_result = QualityValidator::validate(quality_validator, valid_span)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  assert_eq(valid_result.warnings.length(), 0)
  
  // 创建无效测试数据 - 负持续时间
  let invalid_duration_span = { valid_span | 
    duration: -100,
    end_time: 1640995200000 - 100
  }
  
  let invalid_duration_result = QualityValidator::validate(quality_validator, invalid_duration_span)
  assert_false(invalid_duration_result.is_valid)
  assert_true(invalid_duration_result.errors.length() > 0)
  
  let duration_error = invalid_duration_result.errors.find(fn(e) { e.rule == "span_duration_validation" })
  assert_true(duration_error != None)
  
  // 创建无效测试数据 - 错误的trace_id格式
  let invalid_trace_id_span = { valid_span | 
    trace_id: "invalid-trace-id!"
  }
  
  let invalid_trace_id_result = QualityValidator::validate(quality_validator, invalid_trace_id_span)
  assert_false(invalid_trace_id_result.is_valid)
  assert_true(invalid_trace_id_result.errors.length() > 0)
  
  let trace_id_error = invalid_trace_id_result.errors.find(fn(e) { e.rule == "trace_id_format_validation" })
  assert_true(trace_id_error != None)
  
  // 创建警告测试数据 - 空服务名
  let warning_span = { valid_span | 
    service_name: ""
  }
  
  let warning_result = QualityValidator::validate(quality_validator, warning_span)
  assert_false(warning_result.is_valid) // 有警告也视为无效
  assert_eq(warning_result.errors.length(), 0)
  assert_true(warning_result.warnings.length() > 0)
  
  let service_name_warning = warning_result.warnings.find(fn(w) { w.rule == "service_name_validation" })
  assert_true(service_name_warning != None)
  
  // 创建时间不一致测试数据
  let inconsistent_time_span = { valid_span | 
    end_time: 1640995200000 - 500 // 结束时间早于开始时间
  }
  
  let inconsistent_time_result = QualityValidator::validate(quality_validator, inconsistent_time_span)
  assert_false(inconsistent_time_result.is_valid)
  assert_true(inconsistent_time_result.errors.length() > 0)
  
  let time_error = inconsistent_time_result.errors.find(fn(e) { e.rule == "timestamp_consistency_validation" })
  assert_true(time_error != None)
  
  // 批量验证测试
  let test_spans = [
    valid_span,
    invalid_duration_span,
    invalid_trace_id_span,
    warning_span,
    inconsistent_time_span
  ]
  
  let batch_result = QualityValidator::validate_batch(quality_validator, test_spans)
  
  // 验证批量结果
  assert_eq(batch_result.total_count, 5)
  assert_eq(batch_result.valid_count, 1)
  assert_eq(batch_result.invalid_count, 4)
  assert_true(batch_result.error_summary.length() > 0)
  assert_true(batch_result.warning_summary.length() > 0)
  
  // 测试数据质量报告生成
  let quality_report = QualityValidator::generate_quality_report(quality_validator, batch_result)
  
  // 验证报告内容
  assert_true(quality_report.summary.length() > 0)
  assert_true(quality_report.error_details.length() > 0)
  assert_true(quality_report.warning_details.length() > 0)
  assert_true(quality_report.recommendations.length() > 0)
  
  // 验证报告包含具体规则违反情况
  let has_duration_error = quality_report.error_details.any(fn(d) { 
    d.rule == "span_duration_validation" 
  })
  assert_true(has_duration_error)
  
  let has_trace_id_error = quality_report.error_details.any(fn(d) { 
    d.rule == "trace_id_format_validation" 
  })
  assert_true(has_trace_id_error)
  
  let has_service_name_warning = quality_report.warning_details.any(fn(d) { 
    d.rule == "service_name_validation" 
  })
  assert_true(has_service_name_warning)
}