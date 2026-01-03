// Azimuth 高级测试用例集
// 专注于遥测系统的高级功能、性能优化和复杂场景

// 测试1: 遥测数据处理和转换管道
test "遥测数据处理和转换管道测试" {
  // 创建数据管道
  let pipeline = TelemetryPipeline::new()
  
  // 配置数据源
  let metrics_source = DataSource::metrics("prometheus", "http://prometheus:9090/api/v1/query")
  let traces_source = DataSource::traces("jaeger", "http://jaeger:14268/api/traces")
  let logs_source = DataSource::logs("fluentd", "http://fluentd:24224")
  
  // 添加数据转换器
  let normalizer = DataTransformer::normalizer()
  let aggregator = DataTransformer::aggregator(5000) // 5秒窗口
  let filter = DataTransformer::filter("service.name != 'health-check'")
  
  TelemetryPipeline::add_source(pipeline, metrics_source)
  TelemetryPipeline::add_source(pipeline, traces_source)
  TelemetryPipeline::add_source(pipeline, logs_source)
  
  TelemetryPipeline::add_transformer(pipeline, normalizer)
  TelemetryPipeline::add_transformer(pipeline, filter)
  TelemetryPipeline::add_transformer(pipeline, aggregator)
  
  // 配置输出目标
  let output = DataSink::elasticsearch("http://elasticsearch:9200")
  TelemetryPipeline::add_sink(pipeline, output)
  
  // 启动管道
  TelemetryPipeline::start(pipeline)
  
  // 验证管道状态
  assert_eq(TelemetryPipeline::status(pipeline), Running)
  assert_eq(TelemetryPipeline::source_count(pipeline), 3)
  assert_eq(TelemetryPipeline::transformer_count(pipeline), 3)
  
  // 处理测试数据
  let test_metric = Metric::counter("test.requests", 100.0, ["service:test"])
  let processed_data = TelemetryPipeline::process(pipeline, test_metric)
  
  // 验证处理结果
  assert_true(ProcessedData::is_valid(processed_data))
  assert_eq(ProcessedData::get_attribute(processed_data, "pipeline.processed"), Some(BoolValue(true)))
}

// 测试2: 分布式追踪的高级场景
test "分布式追踪高级场景测试" {
  let tracer_provider = TracerProvider::new_with_config(TracerConfig {
    sampling_rate: 0.1,
    max_attributes_per_span: 128,
    max_events_per_span: 128,
    max_links_per_span: 128
  })
  let tracer = TracerProvider::get_tracer(tracer_provider, "advanced.tracer")
  
  // 创建复杂的分布式追踪场景
  let root_span = Tracer::start_span(tracer, "user.request")
  Span::set_attribute(root_span, "user.id", StringValue("user-12345"))
  Span::set_attribute(root_span, "request.id", StringValue("req-abc123"))
  
  // 模拟微服务调用链
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service", Span::context(root_span))
  Span::set_attribute(auth_span, "auth.method", StringValue("jwt"))
  Span::set_attribute(auth_span, "auth.result", StringValue("success"))
  Span::end(auth_span)
  
  let user_service_span = Tracer::start_span_with_context(tracer, "user.service", Span::context(root_span))
  
  // 并行调用多个下游服务
  let order_service_span = Tracer::start_span_with_context(tracer, "order.service", Span::context(user_service_span))
  Span::set_attribute(order_service_span, "order.count", IntValue(5))
  Span::set_attribute(order_service_span, "order.total", FloatValue(125.50))
  
  let payment_service_span = Tracer::start_span_with_context(tracer, "payment.service", Span::context(user_service_span))
  Span::set_attribute(payment_service_span, "payment.method", StringValue("credit_card"))
  Span::set_attribute(payment_service_span, "payment.amount", FloatValue(125.50))
  
  // 模拟异步操作
  let notification_span = Tracer::start_span_with_context(tracer, "notification.service", Span::context(root_span))
  Span::set_attribute(notification_span, "notification.type", StringValue("email"))
  Span::set_attribute(notification_span, "notification.status", StringValue("queued"))
  
  // 完成所有span
  Span::end(order_service_span)
  Span::end(payment_service_span)
  Span::end(notification_span)
  Span::end(user_service_span)
  Span::end(root_span)
  
  // 验证追踪数据
  let trace_id = Span::trace_id(root_span)
  let spans = TracerProvider::get_spans_by_trace_id(tracer_provider, trace_id)
  
  assert_eq(spans.length(), 6) // root + auth + user + order + payment + notification
  assert_true(Span::all_have_attribute(spans, "service.name"))
  assert_true(Span::all_have_trace_id(spans, trace_id))
}

// 测试3: 性能优化和资源管理
test "性能优化和资源管理测试" {
  // 创建资源池管理器
  let resource_pool = ResourcePool::new(100) // 最大100个资源
  
  // 创建性能监控器
  let performance_monitor = PerformanceMonitor::new()
  PerformanceMonitor::enable_cpu_monitoring(performance_monitor, 1000) // 每秒采样
  PerformanceMonitor::enable_memory_monitoring(performance_monitor, 1000)
  PerformanceMonitor::enable_gc_monitoring(performance_monitor)
  
  // 创建批量处理器
  let batch_processor = BatchProcessor::new(1000, 5000) // 1000条记录或5秒超时
  BatchProcessor::enable_compression(batch_processor, Gzip)
  BatchProcessor::enable_parallel_processing(batch_processor, 4) // 4个并行处理器
  
  // 模拟高负载场景
  let start_time = Time::now()
  let mut processed_count = 0
  
  for i in 0..=10000 {
    // 从资源池获取资源
    let resource = ResourcePool::acquire(resource_pool)
    
    // 处理数据
    let telemetry_data = TelemetryData::new("test.metric", FloatValue(i.to_float()))
    BatchProcessor::add(batch_processor, telemetry_data)
    processed_count = processed_count + 1
    
    // 释放资源
    ResourcePool::release(resource_pool, resource)
    
    // 每1000次迭代检查性能指标
    if i % 1000 == 0 {
      let cpu_usage = PerformanceMonitor::get_cpu_usage(performance_monitor)
      let memory_usage = PerformanceMonitor::get_memory_usage(performance_monitor)
      
      // 验证性能在可接受范围内
      assert_true(cpu_usage < 80.0, "CPU使用率过高: \(cpu_usage)%")
      assert_true(memory_usage < 70.0, "内存使用率过高: \(memory_usage)%")
    }
  }
  
  // 强制处理剩余数据
  BatchProcessor::flush(batch_processor)
  
  let end_time = Time::now()
  let duration = end_time - start_time
  let throughput = processed_count.to_float() / duration.to_seconds()
  
  // 验证性能指标
  assert_true(throughput > 1000.0, "吞吐量过低: \(throughput) ops/sec")
  assert_eq(ResourcePool::available_count(resource_pool), 100)
  assert_true(BatchProcessor::is_empty(batch_processor))
}

// 测试4: 错误处理和恢复机制
test "错误处理和恢复机制测试" {
  // 创建错误处理策略
  let retry_policy = RetryPolicy::exponential_backoff(3, 1000, 30000) // 最多3次重试，初始延迟1秒，最大30秒
  let circuit_breaker = CircuitBreaker::new(5, 30000, 60000) // 5次失败后断路，30秒后半开，60秒后关闭
  
  // 创建错误处理器
  let error_handler = ErrorHandler::new()
  ErrorHandler::add_policy(error_handler, retry_policy)
  ErrorHandler::add_policy(error_handler, circuit_breaker)
  
  // 创建遥测处理器
  let telemetry_processor = TelemetryProcessor::new_with_error_handler(error_handler)
  
  // 测试瞬时错误恢复
  let transient_error_data = TelemetryData::new("test.metric", IntValue(42))
  let transient_result = TelemetryProcessor::process_with_simulation(
    telemetry_processor, 
    transient_error_data, 
    ErrorSimulation::transient(2) // 模拟前2次失败
  )
  
  // 验证瞬时错误恢复
  assert_eq(transient_result.status, Success)
  assert_eq(transient_result.retry_count, 2)
  
  // 测试持久性错误处理
  let persistent_error_data = TelemetryData::new("test.metric", IntValue(84))
  let persistent_result = TelemetryProcessor::process_with_simulation(
    telemetry_processor, 
    persistent_error_data, 
    ErrorSimulation::persistent() // 总是失败
  )
  
  // 验证持久性错误处理
  assert_eq(persistent_result.status, Failed)
  assert_eq(persistent_result.retry_count, 3)
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // 测试断路器恢复
  Time::sleep(35000) // 等待超过半开时间
  
  let recovery_data = TelemetryData::new("test.metric", IntValue(126))
  let recovery_result = TelemetryProcessor::process(telemetry_processor, recovery_data)
  
  // 验证断路器恢复
  assert_eq(recovery_result.status, Success)
  assert_false(CircuitBreaker::is_open(circuit_breaker))
}

// 测试5: 数据安全和隐私保护
test "数据安全和隐私保护测试" {
  // 创建数据脱敏器
  let data_masker = DataMasker::new()
  DataMasker::add_pattern(data_masker, "email", RegexPattern("\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"))
  DataMasker::add_pattern(data_masker, "phone", RegexPattern("\b\d{3}-\d{3}-\d{4}\b"))
  DataMasker::add_pattern(data_masker, "ssn", RegexPattern("\b\d{3}-\d{2}-\d{4}\b"))
  DataMasker::add_pattern(data_masker, "credit_card", RegexPattern("\b\d{4}[\s-]?\d{4}[\s-]?\d{4}[\s-]?\d{4}\b"))
  
  // 创建加密器
  let encryption_config = EncryptionConfig {
    algorithm: AES256_GCM,
    key_rotation_interval: 86400, // 24小时
    key_derivation: PBKDF2
  }
  let encryptor = Encryptor::new(encryption_config)
  
  // 创建访问控制器
  let access_controller = AccessController::new()
  AccessController::add_role(access_controller, "admin", ["read", "write", "delete"])
  AccessController::add_role(access_controller, "analyst", ["read"])
  AccessController::add_role(access_controller, "viewer", ["read:aggregated"])
  
  // 测试数据脱敏
  let sensitive_data = TelemetryData::new("user.profile", StringValue("John Doe, john.doe@example.com, 555-123-4567"))
  let masked_data = DataMasker::mask(data_masker, sensitive_data)
  
  // 验证脱敏结果
  assert_true(String::contains(masked_data.value.to_string(), "***@***.com"))
  assert_true(String::contains(masked_data.value.to_string(), "***-***-****"))
  assert_false(String::contains(masked_data.value.to_string(), "john.doe@example.com"))
  assert_false(String::contains(masked_data.value.to_string(), "555-123-4567"))
  
  // 测试数据加密
  let encrypted_data = Encryptor::encrypt(encryptor, masked_data)
  let decrypted_data = Encryptor::decrypt(encryptor, encrypted_data)
  
  // 验证加密解密
  assert_eq(masked_data.value, decrypted_data.value)
  assert_ne(encrypted_data.value, masked_data.value)
  
  // 测试访问控制
  let admin_context = SecurityContext::new("user-123", ["admin"])
  let analyst_context = SecurityContext::new("user-456", ["analyst"])
  let viewer_context = SecurityContext::new("user-789", ["viewer"])
  
  // 验证访问权限
  assert_true(AccessController::can_access(access_controller, admin_context, "read"))
  assert_true(AccessController::can_access(access_controller, admin_context, "write"))
  assert_true(AccessController::can_access(access_controller, analyst_context, "read"))
  assert_false(AccessController::can_access(access_controller, analyst_context, "write"))
  assert_true(AccessController::can_access(access_controller, viewer_context, "read:aggregated"))
  assert_false(AccessController::can_access(access_controller, viewer_context, "read"))
}

// 测试6: 多平台兼容性
test "多平台兼容性测试" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  let current_platform = PlatformDetector::detect(platform_detector)
  
  // 创建平台适配器
  let adapter = PlatformAdapter::new(current_platform)
  
  // 测试不同平台的功能
  match current_platform {
    Platform::Linux => {
      // Linux特定功能测试
      let cgroup_metrics = PlatformAdapter::get_cgroup_metrics(adapter)
      assert_true(cgroup_metrics.cpu_limit > 0.0)
      assert_true(cgroup_metrics.memory_limit > 0)
      
      let proc_metrics = PlatformAdapter::get_proc_metrics(adapter)
      assert_true(proc_metrics.process_count > 0)
      assert_true(proc_metrics.load_average.length() == 3)
    }
    Platform::Windows => {
      // Windows特定功能测试
      let perf_counters = PlatformAdapter::get_performance_counters(adapter)
      assert_true(perf_counters.cpu_usage >= 0.0)
      assert_true(perf_counters.memory_usage >= 0.0)
      
      let registry_metrics = PlatformAdapter::get_registry_metrics(adapter)
      assert_true(registry_metrics.handle_count > 0)
    }
    Platform::MacOS => {
      // macOS特定功能测试
      let system_info = PlatformAdapter::get_system_info(adapter)
      assert_true(String::length(system_info.hardware_model) > 0)
      assert_true(String::length(system_info.os_version) > 0)
      
      let power_metrics = PlatformAdapter::get_power_metrics(adapter)
      assert_true(power_metrics.battery_level >= 0.0)
      assert_true(power_metrics.battery_level <= 100.0)
    }
    Platform::WebAssembly => {
      // WebAssembly特定功能测试
      let memory_info = PlatformAdapter::get_memory_info(adapter)
      assert_true(memory_info.initial_pages > 0)
      assert_true(memory_info.maximum_pages >= memory_info.initial_pages)
      
      let browser_info = PlatformAdapter::get_browser_info(adapter)
      assert_true(String::length(browser_info.user_agent) > 0)
    }
  }
  
  // 测试跨平台功能
  let file_system = PlatformAdapter::get_file_system(adapter)
  assert_true(file_system.available_space > 0)
  assert_true(file_system.total_space >= file_system.available_space)
  
  let network_info = PlatformAdapter::get_network_info(adapter)
  assert_true(network_info.interfaces.length() > 0)
  
  // 测试平台特定的遥测收集
  let telemetry_collector = PlatformAdapter::create_telemetry_collector(adapter)
  let platform_metrics = TelemetryCollector::collect_platform_metrics(telemetry_collector)
  
  assert_true(platform_metrics.length() > 0)
  assert_true(Metric::has_attribute(platform_metrics[0], "platform"))
}

// 测试7: 实时流处理
test "实时流处理测试" {
  // 创建流处理器
  let stream_processor = StreamProcessor::new(1000) // 1000ms窗口
  
  // 配置流处理拓扑
  let stream_topology = StreamTopology::new()
  
  // 添加数据源
  let kafka_source = StreamSource::kafka("telemetry-topic", "localhost:9092")
  StreamTopology::add_source(stream_topology, kafka_source)
  
  // 添加处理节点
  let filter_node = StreamNode::filter("service.name != 'health-check'")
  let aggregation_node = StreamNode::time_window_aggregation(5000, ["service.name", "endpoint"])
  let alert_node = StreamNode::alert_condition("error_rate > 0.05", "high_error_rate")
  
  StreamTopology::add_node(stream_topology, filter_node)
  StreamTopology::add_node(stream_topology, aggregation_node)
  StreamTopology::add_node(stream_topology, alert_node)
  
  // 添加输出
  let alert_sink = StreamSink::webhook("http://alertmanager:9093/api/v1/alerts")
  StreamTopology::add_sink(stream_topology, alert_sink)
  
  // 构建和启动流处理
  let stream = StreamProcessor::build(stream_processor, stream_topology)
  StreamProcessor::start(stream)
  
  // 模拟实时数据流
  let test_events = [
    StreamEvent::metric("response_time", 0.125, ["service:api", "endpoint:/users"]),
    StreamEvent::metric("response_time", 0.250, ["service:api", "endpoint:/users"]),
    StreamEvent::metric("error_rate", 0.02, ["service:api", "endpoint:/users"]),
    StreamEvent::metric("response_time", 0.075, ["service:db", "endpoint:query"]),
    StreamEvent::metric("error_rate", 0.08, ["service:api", "endpoint:/users"]), // 触发告警
    StreamEvent::metric("response_time", 0.180, ["service:cache", "endpoint:get"])
  ]
  
  // 处理事件
  let mut processed_events = []
  for event in test_events {
    let result = StreamProcessor::process_event(stream, event)
    if result.processed {
      processed_events = processed_events.push(result.event)
    }
  }
  
  // 验证处理结果
  assert_eq(processed_events.length(), 5) // 过滤掉health-check事件
  
  // 等待聚合窗口完成
  Time::sleep(6000)
  
  // 检查聚合结果
  let aggregated_metrics = StreamProcessor::get_aggregated_metrics(stream)
  assert_true(aggregated_metrics.length() > 0)
  
  // 检查告警
  let alerts = StreamProcessor::get_alerts(stream)
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].alert_type, "high_error_rate")
  assert_eq(alerts[0].severity, "warning")
  
  // 停止流处理
  StreamProcessor::stop(stream)
  assert_eq(StreamProcessor::status(stream), Stopped)
}

// 测试8: 时间序列数据分析
test "时间序列数据分析测试" {
  // 创建时间序列数据库
  let tsdb = TimeSeriesDB::new_with_config(TimeSeriesConfig {
    retention_period: 86400 * 7, // 7天
    compression: LZ4,
    shard_count: 12
  })
  
  // 生成测试数据
  let now = Time::now()
  let mut test_data = []
  
  for i in 0..=1000 {
    let timestamp = now + Duration::seconds(i.to_int())
    let value = 50.0 + 10.0 * Float::sin(i.to_float() * 0.1) + Float::random() * 5.0
    let data_point = TimeSeriesPoint::new(timestamp, value, ["metric:cpu_usage", "host:web-01"])
    test_data = test_data.push(data_point)
  }
  
  // 批量写入数据
  TimeSeriesDB::write_batch(tsdb, test_data)
  
  // 测试查询功能
  let query_start = now
  let query_end = now + Duration::seconds(1000)
  
  // 简单范围查询
  let range_result = TimeSeriesDB::query_range(tsdb, "metric:cpu_usage", query_start, query_end)
  assert_eq(range_result.points.length(), 1001)
  
  // 聚合查询
  let avg_query = TimeSeriesQuery::aggregation("metric:cpu_usage", query_start, query_end, Avg, 60)
  let avg_result = TimeSeriesDB::execute_query(tsdb, avg_query)
  assert_true(avg_result.points.length() > 0)
  assert_true(avg_result.points.length() < 1001) // 聚合后点数减少
  
  // 下采样查询
  let downsample_query = TimeSeriesQuery::downsample("metric:cpu_usage", query_start, query_end, 300)
  let downsample_result = TimeSeriesDB::execute_query(tsdb, downsample_query)
  assert_true(downsample_result.points.length() <= 4) // 1000秒/300秒 ≈ 3.33，向上取整为4
  
  // 测试分析功能
  let analyzer = TimeSeriesAnalyzer::new()
  
  // 趋势分析
  let trend_analysis = TimeSeriesAnalyzer::analyze_trend(analyzer, range_result.points)
  assert_true(trend_analysis.trend_direction != None)
  assert_true(trend_analysis.correlation_coefficient >= -1.0)
  assert_true(trend_analysis.correlation_coefficient <= 1.0)
  
  // 异常检测
  let anomaly_detection = TimeSeriesAnalyzer::detect_anomalies(analyzer, range_result.points, ZScore, 2.0)
  assert_true(anomaly_detection.anomalies.length() >= 0)
  
  // 季节性分析
  let seasonality_analysis = TimeSeriesAnalyzer::detect_seasonality(analyzer, range_result.points)
  assert_true(seasonality_analysis.has_seasonality != None)
  
  // 预测
  let forecast_result = TimeSeriesAnalyzer::forecast(analyzer, range_result.points, 100)
  assert_eq(forecast_result.forecast_points.length(), 100)
  assert_true(forecast_result.confidence_interval.low.length() == 100)
  assert_true(forecast_result.confidence_interval.high.length() == 100)
  
  // 测试压缩和存储效率
  let storage_stats = TimeSeriesDB::get_storage_stats(tsdb)
  assert_true(storage_stats.compression_ratio > 1.0)
  assert_true(storage_stats.disk_usage > 0)
}

// 测试9: 可视化和仪表板
test "可视化和仪表板测试" {
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new()
  
  // 创建仪表板
  let dashboard = Dashboard::new("System Overview", "系统总览仪表板")
  
  // 添加图表组件
  let line_chart = Chart::line("CPU Usage", "CPU使用率趋势")
  Chart::add_metric(line_chart, "system.cpu.usage", ["host:web-01"])
  Chart::set_time_range(line_chart, TimeRange::last_1h())
  Chart::add_annotation(line_chart, Annotation::new("Deployment", Time::now() - Duration::minutes(30), "部署新版本"))
  Dashboard::add_chart(dashboard, line_chart)
  
  let bar_chart = Chart::bar("Request Count", "请求计数")
  Chart::add_metric(bar_chart, "http.requests.total", ["service:api"])
  Chart::set_grouping(bar_chart, ["endpoint"])
  Chart::set_sort_order(bar_chart, Descending)
  Dashboard::add_chart(dashboard, bar_chart)
  
  let pie_chart = Chart::pie("Error Distribution", "错误分布")
  Chart::add_metric(pie_chart, "http.errors.total", ["service:api"])
  Chart::set_grouping(pie_chart, ["error.type"])
  Dashboard::add_chart(dashboard, pie_chart)
  
  let heatmap = Chart::heatmap("Response Time Heatmap", "响应时间热力图")
  Chart::add_metric(heatmap, "http.request.duration", ["service:api"])
  Chart::set_x_axis(heatmap, "endpoint")
  Chart::set_y_axis(heatmap, "status_code")
  Dashboard::add_chart(dashboard, heatmap)
  
  // 添加指标卡片
  let metric_card = MetricCard::new("Current Users", "当前用户数")
  MetricCard::set_metric(metric_card, "user.active.count")
  MetricCard::set_unit(metric_card, "users")
  MetricCard::set_thresholds(metric_card, [
    Threshold::new(0, 100, Green),
    Threshold::new(100, 500, Yellow),
    Threshold::new(500, 1000000, Red)
  ])
  Dashboard::add_metric_card(dashboard, metric_card)
  
  // 添加表格
  let table = Table::new("Top Services", "服务排行")
  Table::add_column(table, TableColumn::new("Service", "service.name"))
  Table::add_column(table, TableColumn::new("Request Count", "http.requests.total"))
  Table::add_column(table, TableColumn::new("Error Rate", "http.errors.rate"))
  Table::add_column(table, TableColumn::new("Avg Response Time", "http.request.duration.avg"))
  Table::set_sorting(table, "http.requests.total", Descending)
  Table::set_limit(table, 10)
  Dashboard::add_table(dashboard, table)
  
  // 保存仪表板
  DashboardManager::save_dashboard(dashboard_manager, dashboard)
  
  // 测试仪表板渲染
  let rendered_dashboard = DashboardManager::render_dashboard(dashboard_manager, dashboard.id)
  assert_true(RenderedDashboard::has_chart(rendered_dashboard, "CPU Usage"))
  assert_true(RenderedDashboard::has_chart(rendered_dashboard, "Request Count"))
  assert_true(RenderedDashboard::has_metric_card(rendered_dashboard, "Current Users"))
  assert_true(RenderedDashboard::has_table(rendered_dashboard, "Top Services"))
  
  // 测试仪表板查询
  let dashboard_queries = DashboardManager::generate_queries(dashboard_manager, dashboard.id)
  assert_true(dashboard_queries.length() > 0)
  
  // 执行查询并验证结果
  let query_results = []
  for query in dashboard_queries {
    let result = QueryEngine::execute(query)
    query_results = query_results.push(result)
  }
  
  // 验证查询结果
  for result in query_results {
    assert_true(QueryResult::is_success(result))
    assert_true(QueryResult::data_count(result) > 0)
  }
  
  // 测试仪表板分享
  let share_token = DashboardManager::generate_share_token(dashboard_manager, dashboard.id, 3600) // 1小时有效期
  assert_true(String::length(share_token) > 0)
  
  let shared_dashboard = DashboardManager::get_shared_dashboard(dashboard_manager, share_token)
  assert_eq(shared_dashboard.title, dashboard.title)
  assert_eq(shared_dashboard.charts.length(), dashboard.charts.length())
}

// 测试10: 系统集成和端到端测试
test "系统集成和端到端测试" {
  // 创建完整的遥测系统
  let telemetry_system = TelemetrySystem::new_with_config(SystemConfig {
    metrics_enabled: true,
    tracing_enabled: true,
    logging_enabled: true,
    profiling_enabled: true
  })
  
  // 配置数据收集器
  let metrics_collector = MetricsCollector::new(Prometheus)
  let tracing_collector = TracingCollector::new(Jaeger)
  let logging_collector = LoggingCollector::new(Fluentd)
  
  TelemetrySystem::add_collector(telemetry_system, metrics_collector)
  TelemetrySystem::add_collector(telemetry_system, tracing_collector)
  TelemetrySystem::add_collector(telemetry_system, logging_collector)
  
  // 配置数据处理器
  let batch_processor = BatchProcessor::new(100, 5000)
  let filter_processor = FilterProcessor::new()
  let enrichment_processor = EnrichmentProcessor::new()
  
  FilterProcessor::add_rule(filter_processor, FilterRule::exclude("health.check"))
  EnrichmentProcessor::add_enricher(enrichment_processor, Enricher::hostname())
  EnrichmentProcessor::add_enricher(enrichment_processor, Enricher::environment())
  
  TelemetrySystem::add_processor(telemetry_system, batch_processor)
  TelemetrySystem::add_processor(telemetry_system, filter_processor)
  TelemetrySystem::add_processor(telemetry_system, enrichment_processor)
  
  // 配置数据导出器
  let prometheus_exporter = Exporter::prometheus(9090)
  let jaeger_exporter = Exporter::jaeger("http://jaeger:14268/api/traces")
  let elasticsearch_exporter = Exporter::elasticsearch("http://elasticsearch:9200")
  
  TelemetrySystem::add_exporter(telemetry_system, prometheus_exporter)
  TelemetrySystem::add_exporter(telemetry_system, jaeger_exporter)
  TelemetrySystem::add_exporter(telemetry_system, elasticsearch_exporter)
  
  // 启动系统
  TelemetrySystem::start(telemetry_system)
  
  // 模拟完整的应用场景
  let app = TestApplication::new("test.app")
  
  // 启动应用追踪
  let app_span = TelemetrySystem::start_span(telemetry_system, "app.startup")
  Span::set_attribute(app_span, "app.version", StringValue("1.0.0"))
  
  // 模拟用户请求
  for i in 0..=100 {
    let request_span = TelemetrySystem::start_span_with_context(telemetry_system, "user.request", Span::context(app_span))
    Span::set_attribute(request_span, "user.id", StringValue("user-\(i)"))
    Span::set_attribute(request_span, "request.method", StringValue("GET"))
    Span::set_attribute(request_span, "request.path", StringValue("/api/users"))
    
    // 记录指标
    TelemetrySystem::record_counter(telemetry_system, "http.requests.total", 1.0, ["method:GET", "path:/api/users"])
    TelemetrySystem::record_histogram(telemetry_system, "http.request.duration", 0.05 + Float::random() * 0.1, ["method:GET", "path:/api/users"])
    
    // 模拟数据库查询
    if i % 10 == 0 {
      let db_span = TelemetrySystem::start_span_with_context(telemetry_system, "database.query", Span::context(request_span))
      Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = $1"))
      Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
      
      TelemetrySystem::record_counter(telemetry_system, "db.queries.total", 1.0, ["type:select"])
      TelemetrySystem::record_histogram(telemetry_system, "db.query.duration", 0.01 + Float::random() * 0.02, ["type:select"])
      
      // 模拟偶发错误
      if i % 30 == 0 {
        Span::set_status(db_span, Error)
        Span::add_event(db_span, "database.error", None)
        TelemetrySystem::record_counter(telemetry_system, "db.errors.total", 1.0, ["type:timeout"])
      } else {
        Span::set_status(db_span, Ok)
      }
      
      Span::end(db_span)
    }
    
    // 记录日志
    TelemetrySystem::log_info(telemetry_system, "Request processed", [
      ("user.id", StringValue("user-\(i)")),
      ("request.method", StringValue("GET")),
      ("request.path", StringValue("/api/users")),
      ("response.status", IntValue(200))
    ])
    
    // 模拟偶发错误
    if i % 25 == 0 {
      Span::set_status(request_span, Error)
      Span::set_attribute(request_span, "error.type", StringValue("timeout"))
      TelemetrySystem::record_counter(telemetry_system, "http.errors.total", 1.0, ["method:GET", "path:/api/users", "error:timeout"])
    } else {
      Span::set_status(request_span, Ok)
    }
    
    Span::end(request_span)
  }
  
  Span::end(app_span)
  
  // 等待数据处理完成
  Time::sleep(10000)
  
  // 验证系统状态
  assert_eq(TelemetrySystem::status(telemetry_system), Running)
  
  // 验证指标
  let metrics = TelemetrySystem::get_metrics(telemetry_system)
  assert_true(Metrics::has_counter(metrics, "http.requests.total"))
  assert_true(Metrics::has_histogram(metrics, "http.request.duration"))
  assert_true(Metrics::has_counter(metrics, "db.queries.total"))
  assert_true(Metrics::has_counter(metrics, "db.errors.total"))
  assert_true(Metrics::has_counter(metrics, "http.errors.total"))
  
  // 验证追踪
  let traces = TelemetrySystem::get_traces(telemetry_system)
  assert_true(traces.length() > 0)
  
  let root_trace = Traces::find_by_name(traces, "app.startup")
  assert_true(Trace::has_span(root_trace, "user.request"))
  assert_true(Trace::has_span(root_trace, "database.query"))
  
  // 验证日志
  let logs = TelemetrySystem::get_logs(telemetry_system)
  assert_true(logs.length() > 0)
  
  let info_logs = Logs::filter_by_severity(logs, Info)
  assert_true(info_logs.length() > 0)
  
  // 验证导出器
  assert_true(Exporter::is_healthy(prometheus_exporter))
  assert_true(Exporter::is_healthy(jaeger_exporter))
  assert_true(Exporter::is_healthy(elasticsearch_exporter))
  
  // 验证端到端数据流
  let prometheus_metrics = Exporter::get_metrics(prometheus_exporter)
  assert_true(String::contains(prometheus_metrics, "http_requests_total"))
  assert_true(String::contains(prometheus_metrics, "http_request_duration_seconds"))
  
  let jaeger_traces = Exporter::get_traces(jaeger_exporter)
  assert_true(jaeger_traces.length() > 0)
  
  let elasticsearch_logs = Exporter::get_logs(elasticsearch_exporter)
  assert_true(elasticsearch_logs.length() > 0)
  
  // 停止系统
  TelemetrySystem::stop(telemetry_system)
  assert_eq(TelemetrySystem::status(telemetry_system), Stopped)
}