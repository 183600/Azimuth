// Azimuth 新综合遥测测试
// 包含10个不同的遥测系统测试用例

// 测试1: 遥测数据聚合测试
test "遥测数据聚合功能" {
  // 创建遥测数据收集器
  let collector = TelemetryCollector::new()
  
  // 添加不同类型的遥测数据
  let metric1 = Metric::new("cpu_usage", 75.5, "percentage")
  let metric2 = Metric::new("memory_usage", 62.3, "percentage")
  let metric3 = Metric::new("disk_io", 1024.0, "bytes_per_second")
  
  TelemetryCollector::add_metric(collector, metric1)
  TelemetryCollector::add_metric(collector, metric2)
  TelemetryCollector::add_metric(collector, metric3)
  
  // 添加事件
  let event = Event::new("service_restart", Map::from([("service", "web-api"), ("reason", "maintenance")]))
  TelemetryCollector::add_event(collector, event)
  
  // 添加日志
  let log = Log::new("info", "User authentication successful", Map::from([("user_id", "12345")]))
  TelemetryCollector::add_log(collector, log)
  
  // 执行数据聚合
  let aggregated_data = TelemetryCollector::aggregate(collector, 60000) // 1分钟窗口
  
  // 验证聚合结果
  assert_true(AggregatedData::metric_count(aggregated_data) >= 3)
  assert_true(AggregatedData::event_count(aggregated_data) >= 1)
  assert_true(AggregatedData::log_count(aggregated_data) >= 1)
  
  // 验证指标聚合计算
  let cpu_avg = AggregatedData::get_metric_average(aggregated_data, "cpu_usage")
  match cpu_avg {
    Some(value) => assert_eq(value, 75.5)
    None => assert_true(false)
  }
  
  // 验证时间窗口
  assert_eq(AggregatedData::time_window(aggregated_data), 60000)
  
  // 验证聚合时间戳
  assert_true(AggregatedData::aggregation_timestamp(aggregated_data) > 0)
}

// 测试2: 分布式追踪上下文传播测试
test "分布式追踪上下文传播" {
  // 创建追踪上下文
  let trace_context = TraceContext::new("trace-12345", "span-67890", "parent-abcde")
  
  // 验证上下文属性
  assert_eq(TraceContext::trace_id(trace_context), "trace-12345")
  assert_eq(TraceContext::span_id(trace_context), "span-67890")
  assert_eq(TraceContext::parent_span_id(trace_context), "parent-abcde")
  
  // 创建子span
  let child_span = TraceContext::create_child_span(trace_context, "child-span")
  
  // 验证子span继承父span的trace_id
  assert_eq(TraceContext::trace_id(child_span), "trace-12345")
  assert_neq(TraceContext::span_id(child_span), "span-67890")
  assert_eq(TraceContext::parent_span_id(child_span), "span-67890")
  
  // 添加baggage项
  TraceContext::add_baggage_item(trace_context, "user_id", "12345")
  TraceContext::add_baggage_item(trace_context, "session_id", "session-abc")
  
  // 验证baggage传播到子span
  let child_baggage = TraceContext::get_baggage_items(child_span)
  assert_true(child_baggage.contains(("user_id", "12345")))
  assert_true(child_baggage.contains(("session_id", "session-abc")))
  
  // 序列化上下文
  let serialized_context = TraceContext::serialize(trace_context)
  assert_true(serialized_context.length() > 0)
  
  // 反序列化上下文
  let deserialized_context = TraceContext::deserialize(serialized_context)
  match deserialized_context {
    Some(context) => {
      assert_eq(TraceContext::trace_id(context), "trace-12345")
      assert_eq(TraceContext::span_id(context), "span-67890")
    }
    None => assert_true(false)
  }
  
  // 测试跨进程传播
  let propagation_headers = TraceContext::inject_to_headers(trace_context)
  assert_true(propagation_headers.contains_key("traceparent"))
  assert_true(propagation_headers.contains_key("tracestate"))
  
  // 从headers提取上下文
  let extracted_context = TraceContext::extract_from_headers(propagation_headers)
  match extracted_context {
    Some(context) => {
      assert_eq(TraceContext::trace_id(context), "trace-12345")
      assert_eq(TraceContext::span_id(context), "span-67890")
    }
    None => assert_true(false)
  }
}

// 测试3: 性能指标收集测试
test "性能指标收集功能" {
  // 创建性能监控器
  let performance_monitor = PerformanceMonitor::new()
  
  // 启动性能监控
  PerformanceMonitor::start_monitoring(performance_monitor)
  
  // 模拟工作负载
  simulate_workload(100) // 模拟100ms的工作
  
  // 记录性能指标
  let metrics = PerformanceMonitor::collect_metrics(performance_monitor)
  
  // 验证CPU使用率指标
  let cpu_usage = PerformanceMetrics::cpu_usage(metrics)
  assert_true(cpu_usage >= 0.0)
  assert_true(cpu_usage <= 100.0)
  
  // 验证内存使用指标
  let memory_usage = PerformanceMetrics::memory_usage(metrics)
  assert_true(memory_usage > 0.0)
  
  // 验证响应时间指标
  let response_time = PerformanceMetrics::response_time(metrics)
  assert_true(response_time >= 100) // 至少100ms
  
  // 验证吞吐量指标
  let throughput = PerformanceMetrics::throughput(metrics)
  assert_true(throughput > 0.0)
  
  // 验证错误率指标
  let error_rate = PerformanceMetrics::error_rate(metrics)
  assert_true(error_rate >= 0.0)
  assert_true(error_rate <= 1.0)
  
  // 创建性能基准测试
  let benchmark = PerformanceBenchmark::new("api_endpoint")
  
  // 运行基准测试
  let benchmark_result = PerformanceBenchmark::run(benchmark, fn() {
    simulate_workload(50) // 模拟50ms的工作
  }, 10) // 运行10次
  
  // 验证基准测试结果
  assert_eq(BenchmarkResult::iteration_count(benchmark_result), 10)
  assert_true(BenchmarkResult::average_time(benchmark_result) >= 50)
  assert_true(BenchmarkResult::min_time(benchmark_result) >= 50)
  assert_true(BenchmarkResult::max_time(benchmark_result) >= 50)
  
  // 验证百分位数
  let p95 = BenchmarkResult::percentile(benchmark_result, 95)
  assert_true(p95 >= 50)
  
  let p99 = BenchmarkResult::percentile(benchmark_result, 99)
  assert_true(p99 >= 50)
}

// 测试4: 错误追踪和报告测试
test "错误追踪和报告功能" {
  // 创建错误追踪器
  let error_tracker = ErrorTracker::new()
  
  // 模拟错误
  let error1 = Error::new("database_connection", "Connection timeout", 5001)
  let error2 = Error::new("api_call", "Rate limit exceeded", 429)
  let error3 = Error::new("validation", "Invalid input parameter", 400)
  
  // 记录错误
  ErrorTracker::record_error(error_tracker, error1)
  ErrorTracker::record_error(error_tracker, error2)
  ErrorTracker::record_error(error_tracker, error3)
  
  // 验证错误计数
  assert_eq(ErrorTracker::error_count(error_tracker), 3)
  
  // 验证错误分类
  let db_errors = ErrorTracker::get_errors_by_type(error_tracker, "database_connection")
  assert_eq(db_errors.length(), 1)
  
  let api_errors = ErrorTracker::get_errors_by_type(error_tracker, "api_call")
  assert_eq(api_errors.length(), 1)
  
  // 验证错误率计算
  let total_operations = 100
  let error_rate = ErrorTracker::calculate_error_rate(error_tracker, total_operations)
  assert_eq(error_rate, 0.03) // 3/100
  
  // 创建错误报告
  let error_report = ErrorTracker::generate_report(error_tracker, 3600) // 1小时窗口
  
  // 验证错误报告内容
  assert_true(ErrorReport::total_errors(error_report) >= 3)
  assert_true(ErrorReport::unique_error_types(error_report) >= 3)
  assert_true(ErrorReport::time_range(error_report) == 3600)
  
  // 验证错误趋势分析
  let error_trend = ErrorReport::get_error_trend(error_report)
  assert_true(ErrorTrend::data_points(error_trend) > 0)
  
  // 验证错误严重性分析
  let severity_analysis = ErrorReport::get_severity_analysis(error_report)
  assert_true(SeverityAnalysis::critical_count(severity_analysis) >= 0)
  assert_true(SeverityAnalysis::warning_count(severity_analysis) >= 0)
  assert_true(SeverityAnalysis::info_count(severity_analysis) >= 0)
  
  // 测试错误恢复追踪
  let recovery_tracker = ErrorRecoveryTracker::new()
  
  // 记录错误恢复
  ErrorRecoveryTracker::record_recovery_attempt(recovery_tracker, error1)
  ErrorRecoveryTracker::record_recovery_success(recovery_tracker, error1)
  
  ErrorRecoveryTracker::record_recovery_attempt(recovery_tracker, error2)
  ErrorRecoveryTracker::record_recovery_failure(recovery_tracker, error2)
  
  // 验证恢复统计
  let recovery_stats = ErrorRecoveryTracker::get_statistics(recovery_tracker)
  assert_eq(RecoveryStats::total_attempts(recovery_stats), 2)
  assert_eq(RecoveryStats::successful_recoveries(recovery_stats), 1)
  assert_eq(RecoveryStats::failed_recoveries(recovery_stats), 1)
  assert_eq(RecoveryStats::success_rate(recovery_stats), 0.5)
}

// 测试5: 资源使用监控测试
test "资源使用监控功能" {
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new()
  
  // 启动资源监控
  ResourceMonitor::start_monitoring(resource_monitor)
  
  // 等待监控数据收集
  wait_for_monitoring_data(1000) // 等待1秒
  
  // 获取资源使用情况
  let resource_usage = ResourceMonitor::get_current_usage(resource_monitor)
  
  // 验证CPU使用情况
  let cpu_usage = ResourceUsage::cpu_percentage(resource_usage)
  assert_true(cpu_usage >= 0.0)
  assert_true(cpu_usage <= 100.0)
  
  // 验证内存使用情况
  let memory_usage = ResourceUsage::memory_percentage(resource_usage)
  assert_true(memory_usage >= 0.0)
  assert_true(memory_usage <= 100.0)
  
  let memory_total = ResourceUsage::memory_total(resource_usage)
  let memory_used = ResourceUsage::memory_used(resource_usage)
  assert_true(memory_used <= memory_total)
  
  // 验证磁盘使用情况
  let disk_usage = ResourceUsage::disk_percentage(resource_usage)
  assert_true(disk_usage >= 0.0)
  assert_true(disk_usage <= 100.0)
  
  let disk_total = ResourceUsage::disk_total(resource_usage)
  let disk_used = ResourceUsage::disk_used(resource_usage)
  assert_true(disk_used <= disk_total)
  
  // 验证网络使用情况
  let network_in = ResourceUsage::network_bytes_in(resource_usage)
  let network_out = ResourceUsage::network_bytes_out(resource_usage)
  assert_true(network_in >= 0)
  assert_true(network_out >= 0)
  
  // 设置资源阈值
  let thresholds = ResourceThresholds::new()
  ResourceThresholds::set_cpu_threshold(thresholds, 80.0) // 80%
  ResourceThresholds::set_memory_threshold(thresholds, 85.0) // 85%
  ResourceThresholds::set_disk_threshold(thresholds, 90.0) // 90%
  
  // 检查资源阈值违规
  let violations = ResourceMonitor::check_threshold_violations(resource_monitor, thresholds)
  
  // 验证违规检测
  for violation in violations {
    assert_true(ResourceViolation::resource_type(violation).is_one_of(["cpu", "memory", "disk"]))
    assert_true(ResourceViolation::current_value(violation) > ResourceViolation::threshold_value(violation))
    assert_true(ResourceViolation::timestamp(violation) > 0)
  }
  
  // 测试资源使用趋势
  let usage_history = ResourceMonitor::get_usage_history(resource_monitor, 3600) // 1小时历史
  assert_true(UsageHistory::data_points(usage_history) > 0)
  
  // 验证趋势分析
  let cpu_trend = UsageHistory::get_trend(usage_history, "cpu")
  assert_true(Trend::direction(cpu_trend).is_one_of(["increasing", "decreasing", "stable"]))
  
  // 测试资源预测
  let prediction = ResourceMonitor::predict_usage(resource_monitor, "cpu", 3600) // 预测1小时后
  assert_true(ResourcePrediction::predicted_value(prediction) >= 0.0)
  assert_true(ResourcePrediction::confidence_interval(prediction).length() == 2)
}

// 测试6: 跨服务遥测一致性测试
test "跨服务遥测一致性" {
  // 创建服务遥测管理器
  let service_telemetry = ServiceTelemetryManager::new()
  
  // 注册服务
  let service1 = "user-service"
  let service2 = "order-service"
  let service3 = "payment-service"
  
  ServiceTelemetryManager::register_service(service_telemetry, service1)
  ServiceTelemetryManager::register_service(service_telemetry, service2)
  ServiceTelemetryManager::register_service(service_telemetry, service3)
  
  // 为每个服务添加遥测数据
  let trace_id = "trace-consistency-test"
  
  // 服务1遥测数据
  let span1 = Span::new("authenticate_user", trace_id, "span-1")
  Span::add_attribute(span1, "service", service1)
  Span::add_attribute(span1, "user_id", "12345")
  Span::set_duration(span1, 150) // 150ms
  ServiceTelemetryManager::add_span(service_telemetry, service1, span1)
  
  // 服务2遥测数据
  let span2 = Span::new("create_order", trace_id, "span-2")
  Span::add_attribute(span2, "service", service2)
  Span::add_attribute(span2, "order_id", "order-67890")
  Span::set_duration(span2, 200) // 200ms
  ServiceTelemetryManager::add_span(service_telemetry, service2, span2)
  
  // 服务3遥测数据
  let span3 = Span::new("process_payment", trace_id, "span-3")
  Span::add_attribute(span3, "service", service3)
  Span::add_attribute(span3, "payment_id", "payment-abc")
  Span::set_duration(span3, 300) // 300ms
  ServiceTelemetryManager::add_span(service_telemetry, service3, span3)
  
  // 验证跨服务追踪一致性
  let trace = ServiceTelemetryManager::get_trace_by_id(service_telemetry, trace_id)
  match trace {
    Some(t) => {
      // 验证trace包含所有服务的span
      assert_eq(Trace::span_count(t), 3)
      
      // 验证所有span属于同一trace
      for span in Trace::spans(t) {
        assert_eq(Span::trace_id(span), trace_id)
      }
      
      // 验证span时间顺序
      let spans = Trace::spans(t)
      let sorted_spans = spans.sort_by(fn(a, b) { Span::start_time(a) - Span::start_time(b) })
      assert_eq(sorted_spans.length(), 3)
    }
    None => assert_true(false)
  }
  
  // 测试服务间遥测同步
  let sync_result = ServiceTelemetryManager::synchronize_telemetry(service_telemetry, [service1, service2, service3])
  assert_true(SyncResult::is_successful(sync_result))
  assert_eq(SyncResult::synced_services(sync_result), 3)
  
  // 验证同步后的数据一致性
  let consistency_report = ServiceTelemetryManager::check_consistency(service_telemetry, trace_id)
  assert_true(ConsistencyReport::is_consistent(consistency_report))
  assert_eq(ConsistencyReport::inconsistency_count(consistency_report), 0)
  
  // 测试服务间属性传播
  let common_attributes = ServiceTelemetryManager::get_common_attributes(service_telemetry, trace_id)
  assert_true(common_attributes.contains(("trace_id", trace_id)))
  
  // 测试服务性能对比
  let performance_comparison = ServiceTelemetryManager::compare_service_performance(
    service_telemetry,
    [service1, service2, service3],
    3600 // 1小时窗口
  )
  
  // 验证性能对比结果
  assert_eq(PerformanceComparison::service_count(performance_comparison), 3)
  assert_true(PerformanceComparison::has_metrics_for_service(performance_comparison, service1))
  assert_true(PerformanceComparison::has_metrics_for_service(performance_comparison, service2))
  assert_true(PerformanceComparison::has_metrics_for_service(performance_comparison, service3))
}

// 测试7: 实时数据流处理测试
test "实时数据流处理功能" {
  // 创建数据流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置数据源
  let data_source = DataSource::new("telemetry_events")
  DataSource::set_event_type(data_source, "metric")
  DataSource::set_batch_size(data_source, 100)
  DataSource::set_batch_timeout(data_source, 1000) // 1秒
  
  // 配置处理管道
  let pipeline = StreamPipeline::new()
  
  // 添加过滤阶段
  StreamPipeline::add_filter(pipeline, fn(event) {
    event.get_attribute("environment") == Some("production")
  })
  
  // 添加转换阶段
  StreamPipeline::add_transform(pipeline, fn(event) {
    let value = event.get_metric_value("response_time")
    match value {
      Some(v) => {
        event.set_metric_value("response_time_ms", v * 1000) // 转换为毫秒
        event
      }
      None => event
    }
  })
  
  // 添加聚合阶段
  StreamPipeline::add_aggregation(pipeline, fn(events) {
    let total_response_time = events.reduce(fn(acc, event) {
      let value = event.get_metric_value("response_time_ms")
      match value {
        Some(v) => acc + v
        None => acc
      }
    }, 0.0)
    
    let avg_response_time = total_response_time / events.length().to_float()
    
    let aggregated_event = Event::new("aggregated_metrics")
    aggregated_event.set_metric_value("avg_response_time_ms", avg_response_time)
    aggregated_event.set_attribute("event_count", events.length().to_string())
    
    [aggregated_event]
  })
  
  // 配置输出
  let output_sink = OutputSink::new("metrics_dashboard")
  StreamPipeline::set_output(pipeline, output_sink)
  
  // 启动流处理
  StreamProcessor::start(stream_processor, data_source, pipeline)
  
  // 模拟数据流
  let test_events = []
  for i in 1..=50 {
    let event = Event::new("api_call")
    event.set_attribute("service", "user-api")
    event.set_attribute("environment", "production")
    event.set_metric_value("response_time", i.to_float() / 10.0) // 0.1到5.0秒
    test_events.push(event)
  }
  
  // 添加一些非生产环境事件（应该被过滤掉）
  for i in 1..=10 {
    let event = Event::new("api_call")
    event.set_attribute("service", "user-api")
    event.set_attribute("environment", "staging")
    event.set_metric_value("response_time", i.to_float() / 10.0)
    test_events.push(event)
  }
  
  // 处理事件
  let processed_events = StreamProcessor::process_batch(stream_processor, test_events)
  
  // 验证过滤结果
  assert_eq(processed_events.length(), 1) // 聚合后只有一个事件
  
  // 验证聚合结果
  let aggregated_event = processed_events[0]
  assert_eq(aggregated_event.get_type(), "aggregated_metrics")
  
  let avg_response_time = aggregated_event.get_metric_value("avg_response_time_ms")
  match avg_response_time {
    Some(value) => {
      // 验证平均值计算正确
      let expected_avg = (1..=50).reduce(fn(acc, i) { acc + i.to_float() }, 0.0) / 50.0 / 10.0 * 1000.0
      assert_eq(value, expected_avg)
    }
    None => assert_true(false)
  }
  
  let event_count = aggregated_event.get_attribute("event_count")
  match event_count {
    Some(count) => assert_eq(count, "50") // 只有生产环境的50个事件被处理
    None => assert_true(false)
  }
  
  // 验证输出
  let output_events = OutputSink::get_events(output_sink)
  assert_eq(output_events.length(), 1)
  
  // 测试实时性能指标
  let performance_metrics = StreamProcessor::get_performance_metrics(stream_processor)
  assert_true(PerformanceMetrics::events_processed(performance_metrics) >= 50)
  assert_true(PerformanceMetrics::processing_rate(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::average_latency(performance_metrics) >= 0.0)
}

// 测试8: 遥测数据序列化测试
test "遥测数据序列化功能" {
  // 创建测试遥测数据
  let telemetry_data = TelemetryData::new()
  
  // 添加指标
  TelemetryData::add_metric(telemetry_data, "cpu_usage", 75.5)
  TelemetryData::add_metric(telemetry_data, "memory_usage", 62.3)
  TelemetryData::add_metric(telemetry_data, "disk_io", 1024.0)
  
  // 添加属性
  TelemetryData::add_attribute(telemetry_data, "service_name", "web-api")
  TelemetryData::add_attribute(telemetry_data, "instance_id", "instance-123")
  TelemetryData::add_attribute(telemetry_data, "environment", "production")
  TelemetryData::add_attribute(telemetry_data, "version", "1.2.3")
  
  // 添加事件
  let event = Event::new("deployment")
  event.set_attribute("old_version", "1.2.2")
  event.set_attribute("new_version", "1.2.3")
  event.set_timestamp(1640995200)
  TelemetryData::add_event(telemetry_data, event)
  
  // 测试JSON序列化
  let json_serializer = JsonSerializer::new()
  let json_data = TelemetrySerializer::serialize(json_serializer, telemetry_data)
  
  // 验证JSON数据
  assert_true(json_data.length() > 0)
  assert_true(json_data.contains("\"cpu_usage\""))
  assert_true(json_data.contains("\"service_name\""))
  
  // 测试JSON反序列化
  let deserialized_data = TelemetrySerializer::deserialize(json_serializer, json_data)
  match deserialized_data {
    Ok(data) => {
      // 验证指标
      let cpu_usage = TelemetryData::get_metric(data, "cpu_usage")
      match cpu_usage {
        Some(value) => assert_eq(value, 75.5)
        None => assert_true(false)
      }
      
      // 验证属性
      let service_name = TelemetryData::get_attribute(data, "service_name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "web-api")
        None => assert_true(false)
      }
      
      // 验证事件
      let events = TelemetryData::get_events(data)
      assert_eq(events.length(), 1)
      assert_eq(events[0].get_type(), "deployment")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试二进制序列化
  let binary_serializer = BinarySerializer::new()
  let binary_data = TelemetrySerializer::serialize(binary_serializer, telemetry_data)
  
  // 验证二进制数据
  assert_true(binary_data.length() > 0)
  
  // 测试二进制反序列化
  let binary_deserialized = TelemetrySerializer::deserialize(binary_serializer, binary_data)
  match binary_deserialized {
    Ok(data) => {
      // 验证数据完整性
      assert_eq(TelemetryData::metric_count(data), 3)
      assert_eq(TelemetryData::attribute_count(data), 4)
      assert_eq(TelemetryData::event_count(data), 1)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试压缩序列化
  let compressed_serializer = CompressedSerializer::new()
  let compressed_data = TelemetrySerializer::serialize(compressed_serializer, telemetry_data)
  
  // 验证压缩效果
  assert_true(compressed_data.length() < json_data.length())
  
  // 测试压缩反序列化
  let compressed_deserialized = TelemetrySerializer::deserialize(compressed_serializer, compressed_data)
  match compressed_deserialized {
    Ok(data) => {
      // 验证数据完整性
      let cpu_usage = TelemetryData::get_metric(data, "cpu_usage")
      match cpu_usage {
        Some(value) => assert_eq(value, 75.5)
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试序列化性能
  let serialization_performance = SerializationPerformance::benchmark(
    telemetry_data,
    [json_serializer, binary_serializer, compressed_serializer]
  )
  
  // 验证性能结果
  assert_true(SerializationPerformance::has_result_for(serialization_performance, "json"))
  assert_true(SerializationPerformance::has_result_for(serialization_performance, "binary"))
  assert_true(SerializationPerformance::has_result_for(serialization_performance, "compressed"))
  
  let json_performance = SerializationPerformance::get_result(serialization_performance, "json")
  assert_true(SerializationResult::serialization_time(json_performance) >= 0)
  assert_true(SerializationResult::deserialization_time(json_performance) >= 0)
  assert_true(SerializationResult::serialized_size(json_performance) > 0)
}

// 测试9: 自定义指标测试
test "自定义指标功能" {
  // 创建自定义指标管理器
  let custom_metrics = CustomMetricsManager::new()
  
  // 注册自定义指标
  let counter_metric = CustomMetricsManager::register_counter(
    custom_metrics,
    "user_logins",
    "Total number of user logins",
    ["service", "region"]
  )
  
  let gauge_metric = CustomMetricsManager::register_gauge(
    custom_metrics,
    "active_connections",
    "Current number of active connections",
    ["service", "instance"]
  )
  
  let histogram_metric = CustomMetricsManager::register_histogram(
    custom_metrics,
    "request_duration",
    "Request duration in seconds",
    [0.1, 0.5, 1.0, 2.0, 5.0],
    ["endpoint", "method"]
  )
  
  // 验证指标注册
  assert_true(CustomMetricsManager::is_registered(custom_metrics, "user_logins"))
  assert_true(CustomMetricsManager::is_registered(custom_metrics, "active_connections"))
  assert_true(CustomMetricsManager::is_registered(custom_metrics, "request_duration"))
  
  // 记录计数器指标
  CounterMetric::increment(counter_metric, [("service", "auth-api"), ("region", "us-west")])
  CounterMetric::increment(counter_metric, [("service", "auth-api"), ("region", "us-west")])
  CounterMetric::increment(counter_metric, [("service", "auth-api"), ("region", "eu-east")])
  
  // 验证计数器值
  let counter_value1 = CounterMetric::get_value(counter_metric, [("service", "auth-api"), ("region", "us-west")])
  assert_eq(counter_value1, 2)
  
  let counter_value2 = CounterMetric::get_value(counter_metric, [("service", "auth-api"), ("region", "eu-east")])
  assert_eq(counter_value2, 1)
  
  // 记录仪表指标
  GaugeMetric::set(gauge_metric, 25, [("service", "web-api"), ("instance", "instance-1")])
  GaugeMetric::set(gauge_metric, 30, [("service", "web-api"), ("instance", "instance-2")])
  
  // 验证仪表值
  let gauge_value1 = GaugeMetric::get_value(gauge_metric, [("service", "web-api"), ("instance", "instance-1")])
  assert_eq(gauge_value1, 25)
  
  let gauge_value2 = GaugeMetric::get_value(gauge_metric, [("service", "web-api"), ("instance", "instance-2")])
  assert_eq(gauge_value2, 30)
  
  // 记录直方图指标
  HistogramMetric::observe(histogram_metric, 0.3, [("endpoint", "/api/users"), ("method", "GET")])
  HistogramMetric::observe(histogram_metric, 0.8, [("endpoint", "/api/users"), ("method", "GET")])
  HistogramMetric::observe(histogram_metric, 1.5, [("endpoint", "/api/users"), ("method", "GET")])
  HistogramMetric::observe(histogram_metric, 3.2, [("endpoint", "/api/users"), ("method", "GET")])
  
  // 验证直方图统计
  let histogram_stats = HistogramMetric::get_stats(
    histogram_metric,
    [("endpoint", "/api/users"), ("method", "GET")]
  )
  
  assert_eq(HistogramStats::count(histogram_stats), 4)
  assert_eq(HistogramStats::sum(histogram_stats), 5.8)
  
  // 验证分位数
  let p50 = HistogramStats::percentile(histogram_stats, 0.5)
  assert_true(p50 >= 0.3 && p50 <= 1.5)
  
  let p95 = HistogramStats::percentile(histogram_stats, 0.95)
  assert_true(p95 >= 1.5 && p95 <= 3.2)
  
  // 测试指标聚合
  let aggregated_metrics = CustomMetricsManager::aggregate_by_label(
    custom_metrics,
    "user_logins",
    "service"
  )
  
  // 验证聚合结果
  assert_eq(aggregated_metrics.length(), 1) // 只有auth-api服务
  assert_eq(aggregated_metrics[("service", "auth-api")], 3) // 2 + 1
  
  // 测试指标导出
  let exporter = PrometheusExporter::new()
  let exported_data = CustomMetricsManager::export(custom_metrics, exporter)
  
  // 验证导出数据
  assert_true(exported_data.contains("user_logins"))
  assert_true(exported_data.contains("active_connections"))
  assert_true(exported_data.contains("request_duration"))
  
  // 测试指标重置
  CustomMetricsManager::reset_metric(custom_metrics, "user_logins")
  
  let reset_counter_value = CounterMetric::get_value(
    counter_metric,
    [("service", "auth-api"), ("region", "us-west")]
  )
  assert_eq(reset_counter_value, 0)
}

// 测试10: 遥测系统恢复能力测试
test "遥测系统恢复能力" {
  // 创建遥测系统
  let telemetry_system = TelemetrySystem::new()
  
  // 配置系统组件
  let data_collector = DataCollector::new()
  let data_processor = DataProcessor::new()
  let data_storage = DataStorage::new()
  let alert_manager = AlertManager::new()
  
  TelemetrySystem::set_collector(telemetry_system, data_collector)
  TelemetrySystem::set_processor(telemetry_system, data_processor)
  TelemetrySystem::set_storage(telemetry_system, data_storage)
  TelemetrySystem::set_alert_manager(telemetry_system, alert_manager)
  
  // 启动系统
  TelemetrySystem::start(telemetry_system)
  
  // 验证系统状态
  assert_eq(TelemetrySystem::get_status(telemetry_system), "running")
  
  // 模拟正常操作
  let normal_data = generate_test_telemetry_data(100)
  let process_result = TelemetrySystem::process_data(telemetry_system, normal_data)
  assert_true(ProcessResult::is_successful(process_result))
  assert_eq(ProcessResult::processed_count(process_result), 100)
  
  // 模拟存储故障
  TelemetrySystem::simulate_component_failure(telemetry_system, "storage")
  
  // 验证故障检测
  assert_eq(TelemetrySystem::get_status(telemetry_system), "degraded")
  assert_true(TelemetrySystem::has_failed_component(telemetry_system, "storage"))
  
  // 验证故障警报
  let alerts = AlertManager::get_active_alerts(alert_manager)
  assert_true(alerts.length() > 0)
  
  let storage_alert = alerts.find(fn(alert) {
    Alert::component(alert) == "storage" && Alert::severity(alert) == "critical"
  })
  assert_true(storage_alert.is_some())
  
  // 验证系统继续运行（降级模式）
  let degraded_data = generate_test_telemetry_data(50)
  let degraded_result = TelemetrySystem::process_data(telemetry_system, degraded_data)
  assert_true(ProcessResult::is_successful(degraded_result)) // 在降级模式下仍应能处理数据
  
  // 模拟故障恢复
  TelemetrySystem::simulate_component_recovery(telemetry_system, "storage")
  
  // 验证恢复过程
  assert_eq(TelemetrySystem::get_status(telemetry_system), "recovering")
  
  // 等待恢复完成
  wait_for_recovery(2000) // 等待2秒
  
  // 验证系统完全恢复
  assert_eq(TelemetrySystem::get_status(telemetry_system), "running")
  assert_false(TelemetrySystem::has_failed_component(telemetry_system, "storage"))
  
  // 验证恢复警报
  let recovery_alerts = AlertManager::get_active_alerts(alert_manager)
  let recovery_notification = recovery_alerts.find(fn(alert) {
    Alert::type(alert) == "recovery" && Alert::component(alert) == "storage"
  })
  assert_true(recovery_notification.is_some())
  
  // 测试系统恢复后的数据处理
  let recovery_data = generate_test_telemetry_data(75)
  let recovery_result = TelemetrySystem::process_data(telemetry_system, recovery_data)
  assert_true(ProcessResult::is_successful(recovery_result))
  assert_eq(ProcessResult::processed_count(recovery_result), 75)
  
  // 测试系统韧性统计
  let resilience_stats = TelemetrySystem::get_resilience_statistics(telemetry_system)
  
  // 验证韧性统计
  assert_true(ResilienceStats::total_failures(resilience_stats) >= 1)
  assert_true(ResilienceStats::total_recoveries(resilience_stats) >= 1)
  assert_true(ResilienceStats::average_recovery_time(resilience_stats) > 0)
  assert_true(ResilienceStats::uptime_percentage(resilience_stats) > 90.0)
  
  // 测试系统健康检查
  let health_check = TelemetrySystem::perform_health_check(telemetry_system)
  
  // 验证健康检查结果
  assert_true(HealthCheck::overall_status(health_check) == "healthy")
  assert_true(HealthCheck::component_status(health_check, "collector") == "healthy")
  assert_true(HealthCheck::component_status(health_check, "processor") == "healthy")
  assert_true(HealthCheck::component_status(health_check, "storage") == "healthy")
  assert_true(HealthCheck::component_status(health_check, "alert_manager") == "healthy")
  
  // 测试故障恢复策略
  let recovery_strategy = RecoveryStrategy::new("storage")
  RecoveryStrategy::set_max_retry_attempts(recovery_strategy, 3)
  RecoveryStrategy::set_retry_delay(recovery_strategy, 1000) // 1秒
  RecoveryStrategy::set_circuit_breaker_threshold(recovery_strategy, 5)
  
  TelemetrySystem::set_recovery_strategy(telemetry_system, "storage", recovery_strategy)
  
  // 验证策略配置
  let configured_strategy = TelemetrySystem::get_recovery_strategy(telemetry_system, "storage")
  assert_eq(RecoveryStrategy::max_retry_attempts(configured_strategy), 3)
  assert_eq(RecoveryStrategy::retry_delay(configured_strategy), 1000)
  assert_eq(RecoveryStrategy::circuit_breaker_threshold(configured_strategy), 5)
}

// 辅助函数定义

// 模拟工作负载
fn simulate_workload(duration_ms: Int) -> Unit {
  let start_time = get_current_time_ms()
  while get_current_time_ms() - start_time < duration_ms {
    // 模拟一些计算工作
    let x = 1.0
    for i in 1..=1000 {
      x = x * 1.000001
    }
  }
}

// 获取当前时间（毫秒）
fn get_current_time_ms() -> Int {
  // 在实际实现中，这里会返回系统时间
  1640995200000
}

// 等待监控数据收集
fn wait_for_monitoring_data(duration_ms: Int) -> Unit {
  let start_time = get_current_time_ms()
  while get_current_time_ms() - start_time < duration_ms {
    // 等待
  }
}

// 生成测试遥测数据
fn generate_test_telemetry_data(count: Int) -> Array[TelemetryEvent] {
  let events = []
  for i in 1..=count {
    let event = TelemetryEvent::new("test_event")
    event.set_attribute("event_id", i.to_string())
    event.set_metric_value("value", i.to_float())
    events.push(event)
  }
  events
}

// 等待恢复完成
fn wait_for_recovery(duration_ms: Int) -> Unit {
  let start_time = get_current_time_ms()
  while get_current_time_ms() - start_time < duration_ms {
    // 等待
  }
}