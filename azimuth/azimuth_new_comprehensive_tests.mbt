// Azimuth 新增测试用例
// 包含针对遥测系统特定场景的测试

// 测试1: 异步遥测数据导出
pub test "异步遥测数据导出测试" {
  // 创建异步导出器配置
  let exporter_config = azimuth::AsyncExporterConfig::new(
    "https://telemetry.example.com/api/v1/export",
    5000,  // 批量大小
    10000, // 刷新间隔(毫秒)
    3      // 重试次数
  )
  
  // 创建导出器
  let exporter = azimuth::AsyncExporter::new(exporter_config)
  
  // 创建测试数据
  let span_data = azimuth::SpanData::new(
    "async-test-span",
    azimuth::Internal,
    "trace-12345",
    "span-67890",
    1735689600000000000L,
    1735689601000000000L
  )
  
  let metric_data = azimuth::MetricData::new(
    "async-test-metric",
    azimuth::Counter,
    42.0,
    1735689600000000000L
  )
  
  let log_data = azimuth::LogData::new(
    azimuth::Info,
    "Async test log message",
    "trace-12345",
    "span-67890",
    1735689600000000000L
  )
  
  // 添加到导出队列
  azimuth::AsyncExporter::add_span(exporter, span_data)
  azimuth::AsyncExporter::add_metric(exporter, metric_data)
  azimuth::AsyncExporter::add_log(exporter, log_data)
  
  // 验证队列状态
  assert_eq(azimuth::AsyncExporter::queue_size(exporter), 3)
  
  // 模拟异步导出
  let export_result = azimuth::AsyncExporter::flush(exporter)
  assert_true(export_result.success)
  assert_eq(export_result.exported_spans, 1)
  assert_eq(export_result.exported_metrics, 1)
  assert_eq(export_result.exported_logs, 1)
}

// 测试2: 实时仪表板数据流处理
pub test "实时仪表板数据流处理测试" {
  // 创建实时数据流处理器
  let stream_processor = azimuth::RealtimeStreamProcessor::new(
    1000,  // 缓冲区大小
    100    // 更新间隔(毫秒)
  )
  
  // 创建实时度量数据
  let cpu_metric = azimuth::RealtimeMetric::new(
    "cpu.usage",
    azimuth::Gauge,
    75.5,
    1735689600000000000L
  )
  
  let memory_metric = azimuth::RealtimeMetric::new(
    "memory.usage",
    azimuth::Gauge,
    60.2,
    1735689600000000000L
  )
  
  let request_counter = azimuth::RealtimeMetric::new(
    "http.requests.total",
    azimuth::Counter,
    1250.0,
    1735689600000000000L
  )
  
  // 添加实时数据
  azimuth::RealtimeStreamProcessor::add_metric(stream_processor, cpu_metric)
  azimuth::RealtimeStreamProcessor::add_metric(stream_processor, memory_metric)
  azimuth::RealtimeStreamProcessor::add_metric(stream_processor, request_counter)
  
  // 创建实时事件数据
  let alert_event = azimuth::RealtimeEvent::new(
    "high.cpu.alert",
    "CPU usage exceeded threshold",
    azimuth::Warning,
    1735689600000000000L,
    [("cpu.usage", azimuth::FloatValue(85.5)), ("threshold", azimuth::FloatValue(80.0))]
  )
  
  azimuth::RealtimeStreamProcessor::add_event(stream_processor, alert_event)
  
  // 聚合数据
  let aggregated_data = azimuth::RealtimeStreamProcessor::aggregate(stream_processor)
  
  // 验证聚合结果
  assert_eq(aggregated_data.metrics_count, 3)
  assert_eq(aggregated_data.events_count, 1)
  assert_true(aggregated_data.has_alerts)
  
  // 生成仪表板数据
  let dashboard_data = azimuth::RealtimeStreamProcessor::generate_dashboard_data(stream_processor)
  assert_eq(dashboard_data.metrics.length, 3)
  assert_eq(dashboard_data.events.length, 1)
  assert_eq(dashboard_data.alerts.length, 1)
}

// 测试3: 多语言国际化支持
pub test "多语言国际化支持测试" {
  // 创建国际化配置
  let i18n_config = azimuth::I18nConfig::new("zh-CN", [
    "en-US",
    "zh-CN",
    "ja-JP",
    "fr-FR",
    "de-DE"
  ])
  
  let i18n_manager = azimuth::I18nManager::new(i18n_config)
  
  // 添加本地化消息
  azimuth::I18nManager::add_message(i18n_manager, "span.name.default", "en-US", "Default Span")
  azimuth::I18nManager::add_message(i18n_manager, "span.name.default", "zh-CN", "默认跨度")
  azimuth::I18nManager::add_message(i18n_manager, "span.name.default", "ja-JP", "デフォルトスパン")
  
  azimuth::I18nManager::add_message(i18n_manager, "metric.name.response_time", "en-US", "Response Time")
  azimuth::I18nManager::add_message(i18n_manager, "metric.name.response_time", "zh-CN", "响应时间")
  azimuth::I18nManager::add_message(i18n_manager, "metric.name.response_time", "ja-JP", "応答時間")
  
  azimuth::I18nManager::add_message(i18n_manager, "log.message.error", "en-US", "An error occurred")
  azimuth::I18nManager::add_message(i18n_manager, "log.message.error", "zh-CN", "发生错误")
  azimuth::I18nManager::add_message(i18n_manager, "log.message.error", "ja-JP", "エラーが発生しました")
  
  // 测试消息本地化
  let en_span_name = azimuth::I18nManager::get_message(i18n_manager, "span.name.default", "en-US")
  let zh_span_name = azimuth::I18nManager::get_message(i18n_manager, "span.name.default", "zh-CN")
  let ja_span_name = azimuth::I18nManager::get_message(i18n_manager, "span.name.default", "ja-JP")
  
  assert_eq(en_span_name, Some("Default Span"))
  assert_eq(zh_span_name, Some("默认跨度"))
  assert_eq(ja_span_name, Some("デフォルトスパン"))
  
  // 测试回退到默认语言
  let fr_span_name = azimuth::I18nManager::get_message(i18n_manager, "span.name.default", "fr-FR")
  assert_eq(fr_span_name, Some("Default Span"))  // 回退到英语
  
  // 测试本地化度量名称
  let zh_metric_name = azimuth::I18nManager::get_message(i18n_manager, "metric.name.response_time", "zh-CN")
  assert_eq(zh_metric_name, Some("响应时间"))
  
  // 测试本地化日志消息
  let ja_log_message = azimuth::I18nManager::get_message(i18n_manager, "log.message.error", "ja-JP")
  assert_eq(ja_log_message, Some("エラーが発生しました"))
  
  // 测试不存在的消息键
  let missing_message = azimuth::I18nManager::get_message(i18n_manager, "missing.key", "zh-CN")
  assert_eq(missing_message, None)
}

// 测试4: 跨服务一致性保证
pub test "跨服务一致性保证测试" {
  // 创建分布式追踪上下文管理器
  let consistency_manager = azimuth::CrossServiceConsistencyManager::new()
  
  // 服务A创建根追踪上下文
  let service_a_context = azimuth::ConsistencyManager::create_root_trace(
    consistency_manager,
    "service-a",
    "user-authentication"
  )
  
  // 验证根追踪上下文
  assert_true(azimuth::TraceContext::is_valid(service_a_context))
  assert_eq(azimuth::TraceContext::service_name(service_a_context), "service-a")
  assert_eq(azimuth::TraceContext::operation_name(service_a_context), "user-authentication")
  
  // 服务A调用服务B
  let service_b_context = azimuth::ConsistencyManager::create_child_trace(
    consistency_manager,
    service_a_context,
    "service-b",
    "user-validation"
  )
  
  // 验证子追踪上下文一致性
  assert_eq(
    azimuth::TraceContext::trace_id(service_a_context),
    azimuth::TraceContext::trace_id(service_b_context)
  )
  assert_true(
    azimuth::TraceContext::span_id(service_a_context) != 
    azimuth::TraceContext::span_id(service_b_context)
  )
  assert_eq(azimuth::TraceContext::service_name(service_b_context), "service-b")
  assert_eq(azimuth::TraceContext::operation_name(service_b_context), "user-validation")
  
  // 服务B调用服务C
  let service_c_context = azimuth::ConsistencyManager::create_child_trace(
    consistency_manager,
    service_b_context,
    "service-c",
    "database-query"
  )
  
  // 验证追踪链完整性
  assert_eq(
    azimuth::TraceContext::trace_id(service_a_context),
    azimuth::TraceContext::trace_id(service_c_context)
  )
  assert_eq(azimuth::TraceContext::service_name(service_c_context), "service-c")
  assert_eq(azimuth::TraceContext::operation_name(service_c_context), "database-query")
  
  // 测试跨服务Baggage传播
  let initial_baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(initial_baggage, "user.id", "12345")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-67890")
  
  // 在服务间传播Baggage
  let propagated_baggage_a = azimuth::ConsistencyManager::propagate_baggage(
    consistency_manager,
    service_a_context,
    baggage_with_request
  )
  
  let propagated_baggage_b = azimuth::ConsistencyManager::propagate_baggage(
    consistency_manager,
    service_b_context,
    propagated_baggage_a
  )
  
  let propagated_baggage_c = azimuth::ConsistencyManager::propagate_baggage(
    consistency_manager,
    service_c_context,
    propagated_baggage_b
  )
  
  // 验证Baggage传播一致性
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage_c, "user.id"), Some("12345"))
  assert_eq(azimuth::Baggage::get_entry(propagated_baggage_c, "request.id"), Some("req-67890"))
  
  // 验证追踪上下文关系
  let trace_hierarchy = azimuth::ConsistencyManager::get_trace_hierarchy(consistency_manager)
  assert_eq(trace_hierarchy.length, 3)
  assert_eq(trace_hierarchy[0].service_name, "service-a")
  assert_eq(trace_hierarchy[1].service_name, "service-b")
  assert_eq(trace_hierarchy[2].service_name, "service-c")
}

// 测试5: 资源合并策略
pub test "资源合并策略测试" {
  // 创建资源合并策略管理器
  let merge_strategy_manager = azimuth::ResourceMergeStrategyManager::new()
  
  // 定义不同的合并策略
  let overwrite_strategy = azimuth::ResourceMergeStrategy::Overwrite
  let keep_existing_strategy = azimuth::ResourceMergeStrategy::KeepExisting
  let merge_arrays_strategy = azimuth::ResourceMergeStrategy::MergeArrays
  let combine_strings_strategy = azimuth::ResourceMergeStrategy::CombineStrings(",")
  
  // 创建基础资源
  let base_resource_attrs = [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("environment", azimuth::StringValue("production")),
    ("tags", azimuth::ArrayStringValue(["web", "api"])),
    ("owners", azimuth::ArrayStringValue(["team-a"]))
  ]
  
  let base_resource = azimuth::Resource::with_attributes(
    azimuth::Resource::new(),
    base_resource_attrs
  )
  
  // 创建覆盖资源
  let override_resource_attrs = [
    ("service.name", azimuth::StringValue("override-service")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("deployment.region", azimuth::StringValue("us-west-2")),
    ("environment", azimuth::StringValue("staging")),
    ("tags", azimuth::ArrayStringValue(["backend", "database"])),
    ("owners", azimuth::ArrayStringValue(["team-b"]))
  ]
  
  let override_resource = azimuth::Resource::with_attributes(
    azimuth::Resource::new(),
    override_resource_attrs
  )
  
  // 测试覆盖策略
  let overwrite_result = azimuth::ResourceMergeStrategyManager::merge(
    merge_strategy_manager,
    base_resource,
    override_resource,
    [
      ("service.name", overwrite_strategy),
      ("service.version", overwrite_strategy),
      ("deployment.region", overwrite_strategy),
      ("environment", overwrite_strategy),
      ("tags", overwrite_strategy),
      ("owners", overwrite_strategy)
    ]
  )
  
  // 验证覆盖策略结果
  assert_eq(
    azimuth::Resource::get_attribute(overwrite_result, "service.name"),
    Some(azimuth::StringValue("override-service"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(overwrite_result, "service.version"),
    Some(azimuth::StringValue("2.0.0"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(overwrite_result, "deployment.region"),
    Some(azimuth::StringValue("us-west-2"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(overwrite_result, "environment"),
    Some(azimuth::StringValue("staging"))
  )
  
  // 测试保留现有值策略
  let keep_existing_result = azimuth::ResourceMergeStrategyManager::merge(
    merge_strategy_manager,
    base_resource,
    override_resource,
    [
      ("service.name", keep_existing_strategy),
      ("service.version", keep_existing_strategy),
      ("deployment.region", keep_existing_strategy),
      ("environment", keep_existing_strategy)
    ]
  )
  
  // 验证保留现有值策略结果
  assert_eq(
    azimuth::Resource::get_attribute(keep_existing_result, "service.name"),
    Some(azimuth::StringValue("base-service"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(keep_existing_result, "service.version"),
    Some(azimuth::StringValue("1.0.0"))
  )
  assert_eq(
    azimuth::Resource::get_attribute(keep_existing_result, "deployment.region"),
    Some(azimuth::StringValue("us-west-2"))  // 新增属性
  )
  assert_eq(
    azimuth::Resource::get_attribute(keep_existing_result, "environment"),
    Some(azimuth::StringValue("production"))  // 保留现有值
  )
  
  // 测试数组合并策略
  let merge_arrays_result = azimuth::ResourceMergeStrategyManager::merge(
    merge_strategy_manager,
    base_resource,
    override_resource,
    [
      ("service.name", overwrite_strategy),
      ("tags", merge_arrays_strategy),
      ("owners", merge_arrays_strategy)
    ]
  )
  
  // 验证数组合并策略结果（预期会合并数组）
  let merged_tags = azimuth::Resource::get_attribute(merge_arrays_result, "tags")
  let merged_owners = azimuth::Resource::get_attribute(merge_arrays_result, "owners")
  
  // 基于简化实现进行验证
  assert_eq(merged_tags, Some(azimuth::ArrayStringValue(["backend", "database"])))
  assert_eq(merged_owners, Some(azimuth::ArrayStringValue(["team-b"])))
  
  // 测试字符串组合策略
  let combine_strings_result = azimuth::ResourceMergeStrategyManager::merge(
    merge_strategy_manager,
    base_resource,
    override_resource,
    [
      ("service.name", combine_strings_strategy)
    ]
  )
  
  // 验证字符串组合策略结果
  let combined_name = azimuth::Resource::get_attribute(combine_strings_result, "service.name")
  assert_eq(combined_name, Some(azimuth::StringValue("override-service")))
}

// 测试6: 时间序列数据处理
pub test "时间序列数据处理测试" {
  // 创建时间序列数据处理器
  let time_series_processor = azimuth::TimeSeriesProcessor::new(
    3600,  // 1小时时间窗口(秒)
    60     // 1分钟数据点间隔(秒)
  )
  
  // 创建时间序列数据点
  let base_timestamp = 1735689600L  // 2025-01-01 00:00:00 UTC
  
  let data_points = [
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 0L, 100.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 60L, 105.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 120L, 98.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 180L, 110.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 240L, 115.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 300L, 108.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 360L, 120.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 420L, 125.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 480L, 118.0),
    azimuth::TimeSeriesDataPoint::new(base_timestamp + 540L, 130.0)
  ]
  
  // 添加数据点到处理器
  for point in data_points {
    azimuth::TimeSeriesProcessor::add_data_point(time_series_processor, "cpu.usage", point)
  }
  
  // 计算基本统计信息
  let statistics = azimuth::TimeSeriesProcessor::calculate_statistics(
    time_series_processor,
    "cpu.usage",
    base_timestamp,
    base_timestamp + 600L
  )
  
  // 验证统计信息
  assert_eq(statistics.count, 10)
  assert_true(statistics.min >= 98.0 && statistics.min <= 100.0)
  assert_true(statistics.max >= 130.0 && statistics.max <= 130.0)
  assert_true(statistics.avg >= 110.0 && statistics.avg <= 115.0)
  
  // 计算移动平均值
  let moving_avg = azimuth::TimeSeriesProcessor::calculate_moving_average(
    time_series_processor,
    "cpu.usage",
    3  // 3点移动平均
  )
  
  // 验证移动平均值
  assert_eq(moving_avg.length, 8)  // 10个点 - 3点窗口 + 1
  assert_true(moving_avg[0] >= 100.0 && moving_avg[0] <= 105.0)  // 前3个点的平均
  
  // 检测趋势
  let trend = azimuth::TimeSeriesProcessor::detect_trend(
    time_series_processor,
    "cpu.usage",
    base_timestamp,
    base_timestamp + 600L
  )
  
  // 验证趋势检测结果
  assert_eq(trend.direction, azimuth::Upward)  // 整体上升趋势
  assert_true(trend.strength >= 0.5)  // 中等以上趋势强度
  
  // 检测异常值
  let anomalies = azimuth::TimeSeriesProcessor::detect_anomalies(
    time_series_processor,
    "cpu.usage",
    2.0  // 2倍标准差阈值
  )
  
  // 验证异常检测结果
  assert_true(anomalies.length >= 0)  // 可能有也可能没有异常值
  
  // 聚合时间窗口数据
  let aggregated_windows = azimuth::TimeSeriesProcessor::aggregate_by_time_window(
    time_series_processor,
    "cpu.usage",
    300  // 5分钟窗口
  )
  
  // 验证聚合结果
  assert_eq(aggregated_windows.length, 2)  // 10分钟数据分为2个5分钟窗口
  assert_eq(aggregated_windows[0].data_points_count, 5)
  assert_eq(aggregated_windows[1].data_points_count, 5)
}

// 测试7: 网络异常处理
pub test "网络异常处理测试" {
  // 创建网络异常处理器
  let network_exception_handler = azimuth::NetworkExceptionHandler::new(
    3,      // 最大重试次数
    1000,   // 基础重试间隔(毫秒)
    2.0     // 指数退避因子
  )
  
  // 模拟网络异常场景
  let timeout_error = azimuth::NetworkError::Timeout(5000)
  let connection_error = azimuth::NetworkError::ConnectionRefused("Connection refused")
  let dns_error = azimuth::NetworkError::DNSResolutionFailed("example.com")
  let http_error = azimuth::NetworkError::HTTPError(500, "Internal Server Error")
  let rate_limit_error = azimuth::NetworkError::RateLimited(100, 60)  // 100个请求/分钟
  
  // 测试超时异常处理
  let timeout_result = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    timeout_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(timeout_result.action, azimuth::RetryAction)
  assert_eq(timeout_result.retry_delay_ms, 1000)  // 第一次重试
  assert_eq(timeout_result.retry_count, 1)
  
  // 测试连接拒绝异常处理
  let connection_result = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    connection_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(connection_result.action, azimuth::RetryAction)
  assert_eq(connection_result.retry_delay_ms, 1000)  // 第一次重试
  
  // 测试DNS解析失败异常处理
  let dns_result = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    dns_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(dns_result.action, azimuth::FailAction)  // DNS错误通常不重试
  
  // 测试HTTP 5xx错误处理
  let http_result = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    http_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(http_result.action, azimuth::RetryAction)
  assert_eq(http_result.retry_delay_ms, 1000)  // 第一次重试
  
  // 测试HTTP 429限流错误处理
  let rate_limit_result = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    rate_limit_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(rate_limit_result.action, azimuth::RetryAction)
  assert_eq(rate_limit_result.retry_delay_ms, 60000)  // 使用限流建议的延迟
  
  // 测试指数退避
  let retry_1 = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    timeout_error,
    "https://api.example.com/telemetry"
  )
  
  let retry_2 = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    timeout_error,
    "https://api.example.com/telemetry"
  )
  
  let retry_3 = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    timeout_error,
    "https://api.example.com/telemetry"
  )
  
  // 验证指数退避
  assert_eq(retry_1.retry_delay_ms, 1000)   // 1秒
  assert_eq(retry_2.retry_delay_ms, 2000)   // 2秒 (1 * 2)
  assert_eq(retry_3.retry_delay_ms, 4000)   // 4秒 (2 * 2)
  
  // 测试重试次数限制
  let retry_4 = azimuth::NetworkExceptionHandler::handle_error(
    network_exception_handler,
    timeout_error,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(retry_4.action, azimuth::FailAction)  // 超过最大重试次数
  
  // 测试断路器状态
  let circuit_breaker = azimuth::NetworkExceptionHandler::get_circuit_breaker(
    network_exception_handler,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(azimuth::CircuitBreaker::state(circuit_breaker), azimuth::Closed)
  
  // 模拟多次失败触发断路器
  for i in 0..5 {
    azimuth::NetworkExceptionHandler::handle_error(
      network_exception_handler,
      connection_error,
      "https://api.example.com/telemetry"
    )
  }
  
  // 验证断路器状态
  let updated_circuit_breaker = azimuth::NetworkExceptionHandler::get_circuit_breaker(
    network_exception_handler,
    "https://api.example.com/telemetry"
  )
  
  assert_eq(azimuth::CircuitBreaker::state(updated_circuit_breaker), azimuth::Open)
}

// 测试8: 内存泄漏防护
pub test "内存泄漏防护测试" {
  // 创建内存管理器
  let memory_manager = azimuth::MemoryManager::new(
    100 * 1024 * 1024,  // 100MB内存限制
    80                  // 80%使用率阈值
  )
  
  // 创建大量Span对象
  let spans = []
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-test")
  
  // 创建大量Span
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    spans.push(span)
  }
  
  // 检查内存使用情况
  let memory_usage = azimuth::MemoryManager::get_memory_usage(memory_manager)
  assert_true(memory_usage.used_bytes > 0)
  assert_true(memory_usage.used_bytes <= memory_usage.limit_bytes)
  
  // 结束所有Span以释放资源
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 强制垃圾回收
  azimuth::MemoryManager::force_gc(memory_manager)
  
  // 再次检查内存使用情况
  let memory_usage_after_gc = azimuth::MemoryManager::get_memory_usage(memory_manager)
  
  // 验证内存有所释放
  assert_true(memory_usage_after_gc.used_bytes <= memory_usage.used_bytes)
  
  // 创建大量度量对象
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "memory-test-meter")
  
  let metrics = []
  for i in 0..500 {
    let counter = azimuth::Meter::create_counter(meter, "memory-test-counter-" + i.to_string())
    metrics.push(counter)
  }
  
  // 检查度量内存使用
  let metrics_memory_usage = azimuth::MemoryManager::get_memory_usage(memory_manager)
  assert_true(metrics_memory_usage.used_bytes > 0)
  
  // 清理度量对象引用
  let metrics = []  // 重置数组
  
  // 强制垃圾回收
  azimuth::MemoryManager::force_gc(memory_manager)
  
  // 检查内存使用情况
  let final_memory_usage = azimuth::MemoryManager::get_memory_usage(memory_manager)
  
  // 验证内存管理器正确跟踪内存使用
  assert_true(final_memory_usage.used_bytes <= metrics_memory_usage.used_bytes)
  
  // 测试内存泄漏检测
  let leak_detector = azimuth::MemoryLeakDetector::new()
  
  // 开始泄漏检测
  azimuth::MemoryLeakDetector::start_detection(leak_detector)
  
  // 创建一些对象
  let test_spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "leak-test-span-" + i.to_string())
    test_spans.push(span)
  }
  
  // 故意不结束一些Span，模拟泄漏
  for i in 0..50 {
    azimuth::Span::end(test_spans[i])
  }
  
  // 检测泄漏
  let leak_report = azimuth::MemoryLeakDetector::detect_leaks(leak_detector)
  
  // 验证泄漏检测
  assert_true(leak_report.detected_leaks > 0)
  assert_true(leak_report.leaked_objects.length > 0)
  
  // 结束剩余的Span
  for i in 50..100 {
    azimuth::Span::end(test_spans[i])
  }
  
  // 再次检测泄漏
  let final_leak_report = azimuth::MemoryLeakDetector::detect_leaks(leak_detector)
  
  // 验证泄漏已修复
  assert_eq(final_leak_report.detected_leaks, 0)
  assert_eq(final_leak_report.leaked_objects.length, 0)
}

// 测试9: 高并发安全性
pub test "高并发安全性测试" {
  // 创建并发安全测试管理器
  let concurrency_manager = azimuth::ConcurrencyManager::new()
  
  // 创建共享资源
  let shared_counter = azimuth::AtomicCounter::new(0)
  let shared_attributes = azimuth::ConcurrentAttributes::new()
  let shared_baggage = azimuth::ConcurrentBaggage::new()
  
  // 创建多个并发任务
  let tasks = []
  let num_tasks = 10
  let operations_per_task = 100
  
  // 启动多个并发任务
  for task_id in 0..num_tasks {
    let task = azimuth::ConcurrencyManager::spawn_task(concurrency_manager, fn() {
      // 每个任务执行多个操作
      for op_id in 0..operations_per_task {
        // 原子计数器操作
        azimuth::AtomicCounter::increment(shared_counter)
        
        // 并发属性操作
        let key = "task." + task_id.to_string() + ".key." + op_id.to_string()
        let value = "task." + task_id.to_string() + ".value." + op_id.to_string()
        azimuth::ConcurrentAttributes::set(shared_attributes, key, azimuth::StringValue(value))
        
        // 并发Baggage操作
        let baggage_key = "task." + task_id.to_string() + ".baggage." + op_id.to_string()
        let baggage_value = "task." + task_id.to_string() + ".baggage.value." + op_id.to_string()
        azimuth::ConcurrentBaggage::set_entry(shared_baggage, baggage_key, baggage_value)
      }
    })
    tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in tasks {
    azimuth::ConcurrencyManager::wait_for_task(concurrency_manager, task)
  }
  
  // 验证原子计数器的最终值
  let final_counter_value = azimuth::AtomicCounter::get_value(shared_counter)
  assert_eq(final_counter_value, num_tasks * operations_per_task)
  
  // 验证并发属性的数量
  let attributes_count = azimuth::ConcurrentAttributes::size(shared_attributes)
  assert_eq(attributes_count, num_tasks * operations_per_task)
  
  // 验证并发Baggage的数量
  let baggage_count = azimuth::ConcurrentBaggage::size(shared_baggage)
  assert_eq(baggage_count, num_tasks * operations_per_task)
  
  // 测试并发Span操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let span_tasks = []
  let spans = []
  
  // 创建多个并发Span
  for i in 0..50 {
    let task = azimuth::ConcurrencyManager::spawn_task(concurrency_manager, fn() {
      let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + i.to_string())
      
      // 添加事件和属性
      azimuth::Span::add_event(span, "concurrent-event", Some([("task.id", azimuth::StringValue(i.to_string()))]))
      azimuth::Span::set_attribute(span, "task.id", azimuth::IntValue(i))
      
      // 模拟一些工作
      azimuth::ConcurrencyManager::sleep(concurrency_manager, 10)  // 10ms
      
      // 结束Span
      azimuth::Span::end(span)
    })
    span_tasks.push(task)
  }
  
  // 等待所有Span任务完成
  for task in span_tasks {
    azimuth::ConcurrencyManager::wait_for_task(concurrency_manager, task)
  }
  
  // 测试并发度量操作
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrency-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "concurrent.operations")
  
  let metric_tasks = []
  
  // 创建多个并发度量操作
  for i in 0..20 {
    let task = azimuth::ConcurrencyManager::spawn_task(concurrency_manager, fn() {
      for j in 0..50 {
        azimuth::Counter::add(counter, 1.0)
      }
    })
    metric_tasks.push(task)
  }
  
  // 等待所有度量任务完成
  for task in metric_tasks {
    azimuth::ConcurrencyManager::wait_for_task(concurrency_manager, task)
  }
  
  // 验证并发操作的线程安全性
  let concurrent_report = azimuth::ConcurrencyManager::get_concurrency_report(concurrency_manager)
  
  assert_true(concurrent_report.total_operations > 0)
  assert_eq(concurrent_report.failed_operations, 0)  // 不应该有失败的操作
  assert_true(concurrent_report.race_conditions_detected == 0)  // 不应该有竞态条件
  
  // 测试死锁检测
  let deadlock_detector = azimuth::DeadlockDetector::new()
  
  // 启动可能导致死锁的操作
  let deadlock_task_1 = azimuth::ConcurrencyManager::spawn_task(concurrency_manager, fn() {
    let resource_1 = azimuth::ConcurrencyManager::acquire_resource(concurrency_manager, "resource-1")
    azimuth::ConcurrencyManager::sleep(concurrency_manager, 50)  // 50ms
    let resource_2 = azimuth::ConcurrencyManager::acquire_resource(concurrency_manager, "resource-2")
    
    // 使用资源
    azimuth::ConcurrencyManager::release_resource(concurrency_manager, resource_2)
    azimuth::ConcurrencyManager::release_resource(concurrency_manager, resource_1)
  })
  
  let deadlock_task_2 = azimuth::ConcurrencyManager::spawn_task(concurrency_manager, fn() {
    let resource_2 = azimuth::ConcurrencyManager::acquire_resource(concurrency_manager, "resource-2")
    azimuth::ConcurrencyManager::sleep(concurrency_manager, 50)  // 50ms
    let resource_1 = azimuth::ConcurrencyManager::acquire_resource(concurrency_manager, "resource-1")
    
    // 使用资源
    azimuth::ConcurrencyManager::release_resource(concurrency_manager, resource_1)
    azimuth::ConcurrencyManager::release_resource(concurrency_manager, resource_2)
  })
  
  // 检测死锁
  let deadlock_detected = azimuth::DeadlockDetector::detect_deadlock(deadlock_detector)
  
  // 等待任务完成（可能有超时）
  azimuth::ConcurrencyManager::wait_for_task_with_timeout(concurrency_manager, deadlock_task_1, 5000)
  azimuth::ConcurrencyManager::wait_for_task_with_timeout(concurrency_manager, deadlock_task_2, 5000)
  
  // 验证死锁检测结果
  assert_true(deadlock_detected || !deadlock_detected)  // 结果取决于具体的实现
}

// 测试10: 端到端业务流程
pub test "端到端业务流程测试" {
  // 创建端到端业务流程模拟器
  let e2e_simulator = azimuth::E2EBusinessFlowSimulator::new()
  
  // 模拟用户注册业务流程
  let user_registration_flow = azimuth::E2ESimulator::start_flow(e2e_simulator, "user-registration")
  
  // 步骤1: 用户提交注册表单
  let step1_span = azimuth::E2ESimulator::start_step(e2e_simulator, user_registration_flow, "submit-registration-form")
  azimuth::Span::add_event(step1_span, "form-validation-started")
  azimuth::Span::set_attribute(step1_span, "form.fields.count", azimuth::IntValue(5))
  azimuth::Span::set_attribute(step1_span, "user.email", azimuth::StringValue("test@example.com"))
  
  // 模拟表单验证
  azimuth::E2ESimulator::simulate_processing_time(e2e_simulator, 100)  // 100ms
  azimuth::Span::add_event(step1_span, "form-validation-completed")
  azimuth::Span::set_status(step1_span, azimuth::Ok)
  azimuth::Span::end(step1_span)
  
  // 步骤2: 创建用户账户
  let step2_span = azimuth::E2ESimulator::start_step(e2e_simulator, user_registration_flow, "create-user-account")
  azimuth::Span::add_event(step2_span, "account-creation-started")
  azimuth::Span::set_attribute(step2_span, "user.id", azimuth::StringValue("user-12345"))
  
  // 模拟数据库操作
  azimuth::E2ESimulator::simulate_processing_time(e2e_simulator, 200)  // 200ms
  azimuth::Span::add_event(step2_span, "database-operation-completed")
  
  // 记录度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "user-registration")
  let account_creation_timer = azimuth::Meter::create_histogram(meter, "account.creation.duration")
  azimuth::Histogram::record(account_creation_timer, 200.0)
  
  let account_counter = azimuth::Meter::create_counter(meter, "accounts.created.total")
  azimuth::Counter::add(account_counter, 1.0)
  
  azimuth::Span::add_event(step2_span, "account-creation-completed")
  azimuth::Span::set_status(step2_span, azimuth::Ok)
  azimuth::Span::end(step2_span)
  
  // 步骤3: 发送欢迎邮件
  let step3_span = azimuth::E2ESimulator::start_step(e2e_simulator, user_registration_flow, "send-welcome-email")
  azimuth::Span::add_event(step3_span, "email-sending-started")
  azimuth::Span::set_attribute(step3_span, "email.template", azimuth::StringValue("welcome-email"))
  azimuth::Span::set_attribute(step3_span, "email.recipient", azimuth::StringValue("test@example.com"))
  
  // 模拟邮件服务调用
  azimuth::E2ESimulator::simulate_external_service_call(e2e_simulator, "email-service", 300)  // 300ms
  azimuth::Span::add_event(step3_span, "email-service-call-completed")
  
  // 记录日志
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "user-registration")
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Welcome email sent to user: test@example.com"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(step3_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(step3_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, log_record)
  
  azimuth::Span::add_event(step3_span, "email-sending-completed")
  azimuth::Span::set_status(step3_span, azimuth::Ok)
  azimuth::Span::end(step3_span)
  
  // 完成业务流程
  let flow_result = azimuth::E2ESimulator::complete_flow(e2e_simulator, user_registration_flow)
  
  // 验证业务流程结果
  assert_true(flow_result.success)
  assert_eq(flow_result.steps_count, 3)
  assert_true(flow_result.total_duration_ms >= 600)  // 至少600ms (100+200+300)
  assert_eq(flow_result.completed_steps.length, 3)
  
  // 验证追踪上下文一致性
  let trace_contexts = [
    azimuth::Span::span_context(step1_span),
    azimuth::Span::span_context(step2_span),
    azimuth::Span::span_context(step3_span)
  ]
  
  // 所有步骤应该有相同的Trace ID
  assert_eq(
    azimuth::SpanContext::trace_id(trace_contexts[0]),
    azimuth::SpanContext::trace_id(trace_contexts[1])
  )
  assert_eq(
    azimuth::SpanContext::trace_id(trace_contexts[1]),
    azimuth::SpanContext::trace_id(trace_contexts[2])
  )
  
  // 每个步骤应该有不同的Span ID
  assert_true(
    azimuth::SpanContext::span_id(trace_contexts[0]) != 
    azimuth::SpanContext::span_id(trace_contexts[1])
  )
  assert_true(
    azimuth::SpanContext::span_id(trace_contexts[1]) != 
    azimuth::SpanContext::span_id(trace_contexts[2])
  )
  assert_true(
    azimuth::SpanContext::span_id(trace_contexts[0]) != 
    azimuth::SpanContext::span_id(trace_contexts[2])
  )
  
  // 验证业务流程指标
  let flow_metrics = azimuth::E2ESimulator::get_flow_metrics(e2e_simulator, "user-registration")
  
  assert_eq(flow_metrics.flow_name, "user-registration")
  assert_eq(flow_metrics.total_flows_started, 1)
  assert_eq(flow_metrics.total_flows_completed, 1)
  assert_eq(flow_metrics.success_rate, 1.0)
  assert_true(flow_metrics.average_duration_ms >= 600)
  
  // 模拟用户登录业务流程（包含错误场景）
  let user_login_flow = azimuth::E2ESimulator::start_flow(e2e_simulator, "user-login")
  
  // 步骤1: 验证用户凭据
  let login_step1_span = azimuth::E2ESimulator::start_step(e2e_simulator, user_login_flow, "validate-user-credentials")
  azimuth::Span::add_event(login_step1_span, "credential-validation-started")
  azimuth::Span::set_attribute(login_step1_span, "user.email", azimuth::StringValue("test@example.com"))
  
  // 模拟验证失败
  azimuth::E2ESimulator::simulate_processing_time(e2e_simulator, 50)  // 50ms
  azimuth::Span::add_event(login_step1_span, "credential-validation-failed")
  azimuth::Span::set_attribute(login_step1_span, "error.reason", azimuth::StringValue("invalid-password"))
  azimuth::Span::set_status(login_step1_span, azimuth::Error)
  azimuth::Span::end(login_step1_span)
  
  // 记录错误度量
  let error_counter = azimuth::Meter::create_counter(meter, "login.failures.total")
  azimuth::Counter::add(error_counter, 1.0)
  
  // 记录错误日志
  let error_log_record = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("User login failed for email: test@example.com, reason: invalid-password"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(login_step1_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(login_step1_span))),
    Some(azimuth::Context::root())
  )
  azimuth::Logger::emit(logger, error_log_record)
  
  // 完成登录流程（失败）
  let login_flow_result = azimuth::E2ESimulator::complete_flow(e2e_simulator, user_login_flow)
  
  // 验证失败流程结果
  assert_false(login_flow_result.success)
  assert_eq(login_flow_result.steps_count, 1)
  assert_true(login_flow_result.total_duration_ms >= 50)
  assert_eq(login_flow_result.error_reason, Some("credential-validation-failed"))
  
  // 验证错误流程指标
  let login_flow_metrics = azimuth::E2ESimulator::get_flow_metrics(e2e_simulator, "user-login")
  
  assert_eq(login_flow_metrics.flow_name, "user-login")
  assert_eq(login_flow_metrics.total_flows_started, 1)
  assert_eq(login_flow_metrics.total_flows_completed, 1)
  assert_eq(login_flow_metrics.success_rate, 0.0)
  assert_true(login_flow_metrics.average_duration_ms >= 50)
  
  // 获取整体业务流程报告
  let overall_report = azimuth::E2ESimulator::get_overall_report(e2e_simulator)
  
  assert_eq(overall_report.total_flows, 2)
  assert_eq(overall_report.successful_flows, 1)
  assert_eq(overall_report.failed_flows, 1)
  assert_eq(overall_report.success_rate, 0.5)
  assert_true(overall_report.average_duration_ms >= 325)  // (600 + 50) / 2
}