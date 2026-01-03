// Azimuth Telemetry System - High Quality Dynamic Configuration Tests
// 配置动态更新测试

test "遥测系统配置动态更新" {
  // 创建配置管理器
  let config_manager = azimuth::ConfigurationManager::new()
  
  // 设置初始配置
  let initial_config = azimuth::TelemetryConfig::new()
  azimuth::TelemetryConfig::set_sampling_rate(initial_config, 0.1) // 10% 采样率
  azimuth::TelemetryConfig::set_batch_size(initial_config, 100)
  azimuth::TelemetryConfig::set_export_interval(initial_config, 5000) // 5秒
  azimuth::TelemetryConfig::set_max_spans(initial_config, 1000)
  azimuth::TelemetryConfig::set_max_attributes(initial_config, 128)
  
  // 应用初始配置
  azimuth::ConfigurationManager::apply_config(config_manager, initial_config)
  
  // 验证初始配置
  assert_eq(azimuth::ConfigurationManager::get_sampling_rate(config_manager), 0.1)
  assert_eq(azimuth::ConfigurationManager::get_batch_size(config_manager), 100)
  assert_eq(azimuth::ConfigurationManager::get_export_interval(config_manager), 5000)
  assert_eq(azimuth::ConfigurationManager::get_max_spans(config_manager), 1000)
  assert_eq(azimuth::ConfigurationManager::get_max_attributes(config_manager), 128)
  
  // 创建遥测处理器
  let telemetry_processor = azimuth::TelemetryProcessor::new()
  azimuth::TelemetryProcessor::set_configuration_manager(telemetry_processor, config_manager)
  
  // 验证处理器使用初始配置
  assert_eq(azimuth::TelemetryProcessor::get_sampling_rate(telemetry_processor), 0.1)
  assert_eq(azimuth::TelemetryProcessor::get_batch_size(telemetry_processor), 100)
  assert_eq(azimuth::TelemetryProcessor::get_export_interval(telemetry_processor), 5000)
  
  // 动态更新配置
  let updated_config = azimuth::TelemetryConfig::new()
  azimuth::TelemetryConfig::set_sampling_rate(updated_config, 0.5) // 50% 采样率
  azimuth::TelemetryConfig::set_batch_size(updated_config, 200)
  azimuth::TelemetryConfig::set_export_interval(updated_config, 10000) // 10秒
  azimuth::TelemetryConfig::set_max_spans(updated_config, 2000)
  azimuth::TelemetryConfig::set_max_attributes(updated_config, 256)
  
  // 应用更新的配置
  azimuth::ConfigurationManager::apply_config(config_manager, updated_config)
  
  // 验证配置已更新
  assert_eq(azimuth::ConfigurationManager::get_sampling_rate(config_manager), 0.5)
  assert_eq(azimuth::ConfigurationManager::get_batch_size(config_manager), 200)
  assert_eq(azimuth::ConfigurationManager::get_export_interval(config_manager), 10000)
  assert_eq(azimuth::ConfigurationManager::get_max_spans(config_manager), 2000)
  assert_eq(azimuth::ConfigurationManager::get_max_attributes(config_manager), 256)
  
  // 验证处理器已自动应用新配置
  assert_eq(azimuth::TelemetryProcessor::get_sampling_rate(telemetry_processor), 0.5)
  assert_eq(azimuth::TelemetryProcessor::get_batch_size(telemetry_processor), 200)
  assert_eq(azimuth::TelemetryProcessor::get_export_interval(telemetry_processor), 10000)
}

test "采样策略动态更新" {
  // 创建采样策略管理器
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 设置初始采样策略
  let initial_sampling_config = azimuth::SamplingConfig::new()
  azimuth::SamplingConfig::set_strategy(initial_sampling_config, azimuth::SamplingStrategy::Probabilistic)
  azimuth::SamplingConfig::set_probability(initial_sampling_config, 0.1) // 10% 概率
  azimuth::SamplingConfig::set_parent_based(initial_sampling_config, false)
  
  // 应用初始采样策略
  azimuth::SamplingManager::apply_config(sampling_manager, initial_sampling_config)
  
  // 创建采样器
  let sampler = azimuth::Sampler::new()
  azimuth::Sampler::set_sampling_manager(sampler, sampling_manager)
  
  // 验证初始采样策略
  assert_eq(azimuth::SamplingManager::get_strategy(sampling_manager), azimuth::SamplingStrategy::Probabilistic)
  assert_eq(azimuth::SamplingManager::get_probability(sampling_manager), 0.1)
  assert_false(azimuth::SamplingManager::is_parent_based(sampling_manager))
  
  // 测试初始采样行为
  let mut sampled_count = 0
  let total_samples = 1000
  
  for i in 0..total_samples {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_context = azimuth::SpanContext::new(trace_id, span_id, true, "")
    
    let should_sample = azimuth::Sampler::should_sample(sampler, span_context)
    if should_sample {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率接近初始概率
  let actual_sampling_rate = sampled_count as Float / total_samples as Float
  assert_true(actual_sampling_rate > 0.05 && actual_sampling_rate < 0.15) // 在10%附近
  
  // 动态更新采样策略
  let updated_sampling_config = azimuth::SamplingConfig::new()
  azimuth::SamplingConfig::set_strategy(updated_sampling_config, azimuth::SamplingStrategy::ParentBased)
  azimuth::SamplingConfig::set_probability(updated_sampling_config, 0.5) // 50% 概率
  azimuth::SamplingConfig::set_parent_based(updated_sampling_config, true)
  
  // 应用更新的采样策略
  azimuth::SamplingManager::apply_config(sampling_manager, updated_sampling_config)
  
  // 验证采样策略已更新
  assert_eq(azimuth::SamplingManager::get_strategy(sampling_manager), azimuth::SamplingStrategy::ParentBased)
  assert_eq(azimuth::SamplingManager::get_probability(sampling_manager), 0.5)
  assert_true(azimuth::SamplingManager::is_parent_based(sampling_manager))
  
  // 验证采样器已自动应用新策略
  assert_eq(azimuth::Sampler::get_strategy(sampler), azimuth::SamplingStrategy::ParentBased)
  assert_eq(azimuth::Sampler::get_probability(sampler), 0.5)
  
  // 测试更新后的采样行为
  sampled_count = 0
  
  for i in 0..total_samples {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_context = azimuth::SpanContext::new(trace_id, span_id, true, "")
    
    let should_sample = azimuth::Sampler::should_sample(sampler, span_context)
    if should_sample {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率接近更新后的概率
  let updated_sampling_rate = sampled_count as Float / total_samples as Float
  assert_true(updated_sampling_rate > 0.45 && updated_sampling_rate < 0.55) // 在50%附近
}

test "导出器配置动态更新" {
  // 创建导出器管理器
  let exporter_manager = azimuth::ExporterManager::new()
  
  // 设置初始导出器配置
  let initial_exporter_config = azimuth::ExporterConfig::new()
  azimuth::ExporterConfig::set_type(initial_exporter_config, azimuth::ExporterType::Http)
  azimuth::ExporterConfig::set_endpoint(initial_exporter_config, "https://api.example.com/telemetry")
  azimuth::ExporterConfig::set_timeout(initial_exporter_config, 30000) // 30秒
  azimuth::ExporterConfig::set_retry_count(initial_exporter_config, 3)
  azimuth::ExporterConfig::set_batch_size(initial_exporter_config, 100)
  azimuth::ExporterConfig::set_compression(initial_exporter_config, false)
  
  // 应用初始导出器配置
  azimuth::ExporterManager::apply_config(exporter_manager, initial_exporter_config)
  
  // 创建导出器
  let exporter = azimuth::Exporter::new()
  azimuth::Exporter::set_exporter_manager(exporter, exporter_manager)
  
  // 验证初始导出器配置
  assert_eq(azimuth::ExporterManager::get_type(exporter_manager), azimuth::ExporterType::Http)
  assert_eq(azimuth::ExporterManager::get_endpoint(exporter_manager), "https://api.example.com/telemetry")
  assert_eq(azimuth::ExporterManager::get_timeout(exporter_manager), 30000)
  assert_eq(azimuth::ExporterManager::get_retry_count(exporter_manager), 3)
  assert_eq(azimuth::ExporterManager::get_batch_size(exporter_manager), 100)
  assert_false(azimuth::ExporterManager::is_compression_enabled(exporter_manager))
  
  // 验证导出器使用初始配置
  assert_eq(azimuth::Exporter::get_type(exporter), azimuth::ExporterType::Http)
  assert_eq(azimuth::Exporter::get_endpoint(exporter), "https://api.example.com/telemetry")
  assert_eq(azimuth::Exporter::get_timeout(exporter), 30000)
  
  // 动态更新导出器配置
  let updated_exporter_config = azimuth::ExporterConfig::new()
  azimuth::ExporterConfig::set_type(updated_exporter_config, azimuth::ExporterType::Grpc)
  azimuth::ExporterConfig::set_endpoint(updated_exporter_config, "grpc://api.example.com:4317")
  azimuth::ExporterConfig::set_timeout(updated_exporter_config, 60000) // 60秒
  azimuth::ExporterConfig::set_retry_count(updated_exporter_config, 5)
  azimuth::ExporterConfig::set_batch_size(updated_exporter_config, 200)
  azimuth::ExporterConfig::set_compression(updated_exporter_config, true)
  
  // 应用更新的导出器配置
  azimuth::ExporterManager::apply_config(exporter_manager, updated_exporter_config)
  
  // 验证导出器配置已更新
  assert_eq(azimuth::ExporterManager::get_type(exporter_manager), azimuth::ExporterType::Grpc)
  assert_eq(azimuth::ExporterManager::get_endpoint(exporter_manager), "grpc://api.example.com:4317")
  assert_eq(azimuth::ExporterManager::get_timeout(exporter_manager), 60000)
  assert_eq(azimuth::ExporterManager::get_retry_count(exporter_manager), 5)
  assert_eq(azimuth::ExporterManager::get_batch_size(exporter_manager), 200)
  assert_true(azimuth::ExporterManager::is_compression_enabled(exporter_manager))
  
  // 验证导出器已自动应用新配置
  assert_eq(azimuth::Exporter::get_type(exporter), azimuth::ExporterType::Grpc)
  assert_eq(azimuth::Exporter::get_endpoint(exporter), "grpc://api.example.com:4317")
  assert_eq(azimuth::Exporter::get_timeout(exporter), 60000)
  
  // 测试导出器重新连接
  let reconnect_success = azimuth::Exporter::reconnect(exporter)
  assert_true(reconnect_success)
  
  // 验证导出器状态
  assert_eq(azimuth::Exporter::get_status(exporter), azimuth::ExporterStatus::Connected)
}

test "度量配置动态更新" {
  // 创建度量管理器
  let metrics_manager = azimuth::MetricsManager::new()
  
  // 设置初始度量配置
  let initial_metrics_config = azimuth::MetricsConfig::new()
  azimuth::MetricsConfig::set_export_interval(initial_metrics_config, 30000) // 30秒
  azimuth::MetricsConfig::set_aggregation_temporality(initial_metrics_config, azimuth::AggregationTemporality::Cumulative)
  azimuth::MetricsConfig::set_memory_limit(initial_metrics_config, 100 * 1024 * 1024) // 100MB
  azimuth::MetricsConfig::set_max_metrics(initial_metrics_config, 10000)
  azimuth::MetricsConfig::set_histogram_boundaries(initial_metrics_config, [10.0, 100.0, 1000.0])
  
  // 应用初始度量配置
  azimuth::MetricsManager::apply_config(metrics_manager, initial_metrics_config)
  
  // 创建度量提供器
  let meter_provider = azimuth::MeterProvider::new()
  azimuth::MeterProvider::set_metrics_manager(meter_provider, metrics_manager)
  
  // 验证初始度量配置
  assert_eq(azimuth::MetricsManager::get_export_interval(metrics_manager), 30000)
  assert_eq(azimuth::MetricsManager::get_aggregation_temporality(metrics_manager), azimuth::AggregationTemporality::Cumulative)
  assert_eq(azimuth::MetricsManager::get_memory_limit(metrics_manager), 100 * 1024 * 1024)
  assert_eq(azimuth::MetricsManager::get_max_metrics(metrics_manager), 10000)
  
  let boundaries = azimuth::MetricsManager::get_histogram_boundaries(metrics_manager)
  assert_eq(boundaries.length(), 3)
  assert_eq(boundaries[0], 10.0)
  assert_eq(boundaries[1], 100.0)
  assert_eq(boundaries[2], 1000.0)
  
  // 验证度量提供器使用初始配置
  assert_eq(azimuth::MeterProvider::get_export_interval(meter_provider), 30000)
  assert_eq(azimuth::MeterProvider::get_aggregation_temporality(meter_provider), azimuth::AggregationTemporality::Cumulative)
  
  // 创建度量
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "test_meter")
  let counter = azimuth::Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // 记录一些度量
  azimuth::Counter::add(counter, 1.0)
  azimuth::Counter::add(counter, 2.0)
  azimuth::Histogram::record(histogram, 5.0)
  azimuth::Histogram::record(histogram, 50.0)
  azimuth::Histogram::record(histogram, 500.0)
  
  // 动态更新度量配置
  let updated_metrics_config = azimuth::MetricsConfig::new()
  azimuth::MetricsConfig::set_export_interval(updated_metrics_config, 60000) // 60秒
  azimuth::MetricsConfig::set_aggregation_temporality(updated_metrics_config, azimuth::AggregationTemporality::Delta)
  azimuth::MetricsConfig::set_memory_limit(updated_metrics_config, 200 * 1024 * 1024) // 200MB
  azimuth::MetricsConfig::set_max_metrics(updated_metrics_config, 20000)
  azimuth::MetricsConfig::set_histogram_boundaries(updated_metrics_config, [5.0, 50.0, 500.0, 5000.0])
  
  // 应用更新的度量配置
  azimuth::MetricsManager::apply_config(metrics_manager, updated_metrics_config)
  
  // 验证度量配置已更新
  assert_eq(azimuth::MetricsManager::get_export_interval(metrics_manager), 60000)
  assert_eq(azimuth::MetricsManager::get_aggregation_temporality(metrics_manager), azimuth::AggregationTemporality::Delta)
  assert_eq(azimuth::MetricsManager::get_memory_limit(metrics_manager), 200 * 1024 * 1024)
  assert_eq(azimuth::MetricsManager::get_max_metrics(metrics_manager), 20000)
  
  let updated_boundaries = azimuth::MetricsManager::get_histogram_boundaries(metrics_manager)
  assert_eq(updated_boundaries.length(), 4)
  assert_eq(updated_boundaries[0], 5.0)
  assert_eq(updated_boundaries[1], 50.0)
  assert_eq(updated_boundaries[2], 500.0)
  assert_eq(updated_boundaries[3], 5000.0)
  
  // 验证度量提供器已自动应用新配置
  assert_eq(azimuth::MeterProvider::get_export_interval(meter_provider), 60000)
  assert_eq(azimuth::MeterProvider::get_aggregation_temporality(meter_provider), azimuth::AggregationTemporality::Delta)
  
  // 记录更多度量以测试新配置
  azimuth::Counter::add(counter, 3.0)
  azimuth::Histogram::record(histogram, 15.0)
  azimuth::Histogram::record(histogram, 150.0)
  azimuth::Histogram::record(histogram, 1500.0)
  azimuth::Histogram::record(histogram, 15000.0)
  
  // 验证度量统计
  let counter_value = azimuth::Counter::get_value(counter)
  assert_eq(counter_value, 6.0) // 1 + 2 + 3
  
  let histogram_stats = azimuth::Histogram::get_stats(histogram)
  assert_eq(azimuth::HistogramStats::count(histogram_stats), 7) // 3 + 4
  assert_eq(azimuth::HistogramStats::sum(histogram_stats), 5.0 + 50.0 + 500.0 + 15.0 + 150.0 + 1500.0 + 15000.0)
}

test "日志配置动态更新" {
  // 创建日志管理器
  let logging_manager = azimuth::LoggingManager::new()
  
  // 设置初始日志配置
  let initial_logging_config = azimuth::LoggingConfig::new()
  azimuth::LoggingConfig::set_min_severity(initial_logging_config, azimuth::Info)
  azimuth::LoggingConfig::set_include_timestamp(initial_logging_config, true)
  azimuth::LoggingConfig::set_include_thread_id(initial_logging_config, false)
  azimuth::LoggingConfig::set_include_process_id(initial_logging_config, true)
  azimuth::LoggingConfig::set_max_log_size(initial_logging_config, 1024 * 1024) // 1MB
  azimuth::LoggingConfig::set_max_log_files(initial_logging_config, 10)
  
  // 应用初始日志配置
  azimuth::LoggingManager::apply_config(logging_manager, initial_logging_config)
  
  // 创建日志提供器
  let logger_provider = azimuth::LoggerProvider::new()
  azimuth::LoggerProvider::set_logging_manager(logger_provider, logging_manager)
  
  // 验证初始日志配置
  assert_eq(azimuth::LoggingManager::get_min_severity(logging_manager), azimuth::Info)
  assert_true(azimuth::LoggingManager::is_timestamp_included(logging_manager))
  assert_false(azimuth::LoggingManager::is_thread_id_included(logging_manager))
  assert_true(azimuth::LoggingManager::is_process_id_included(logging_manager))
  assert_eq(azimuth::LoggingManager::get_max_log_size(logging_manager), 1024 * 1024)
  assert_eq(azimuth::LoggingManager::get_max_log_files(logging_manager), 10)
  
  // 验证日志提供器使用初始配置
  assert_eq(azimuth::LoggerProvider::get_min_severity(logger_provider), azimuth::Info)
  assert_true(azimuth::LoggerProvider::is_timestamp_included(logger_provider))
  assert_false(azimuth::LoggerProvider::is_thread_id_included(logger_provider))
  
  // 创建日志器
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "test_logger")
  
  // 记录不同级别的日志
  let debug_log = azimuth::LogRecord::new(azimuth::Debug, "Debug message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Info message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Warning message")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Error message")
  
  azimuth::Logger::emit(logger, debug_log) // 应该被过滤掉
  azimuth::Logger::emit(logger, info_log) // 应该被记录
  azimuth::Logger::emit(logger, warn_log) // 应该被记录
  azimuth::Logger::emit(logger, error_log) // 应该被记录
  
  // 验证日志记录
  let logged_records = azimuth::LoggerProvider::get_logged_records(logger_provider)
  assert_eq(logged_records.length(), 3) // Debug被过滤掉
  
  // 动态更新日志配置
  let updated_logging_config = azimuth::LoggingConfig::new()
  azimuth::LoggingConfig::set_min_severity(updated_logging_config, azimuth::Debug)
  azimuth::LoggingConfig::set_include_timestamp(updated_logging_config, false)
  azimuth::LoggingConfig::set_include_thread_id(updated_logging_config, true)
  azimuth::LoggingConfig::set_include_process_id(updated_logging_config, false)
  azimuth::LoggingConfig::set_max_log_size(updated_logging_config, 2 * 1024 * 1024) // 2MB
  azimuth::LoggingConfig::set_max_log_files(updated_logging_config, 20)
  
  // 应用更新的日志配置
  azimuth::LoggingManager::apply_config(logging_manager, updated_logging_config)
  
  // 验证日志配置已更新
  assert_eq(azimuth::LoggingManager::get_min_severity(logging_manager), azimuth::Debug)
  assert_false(azimuth::LoggingManager::is_timestamp_included(logging_manager))
  assert_true(azimuth::LoggingManager::is_thread_id_included(logging_manager))
  assert_false(azimuth::LoggingManager::is_process_id_included(logging_manager))
  assert_eq(azimuth::LoggingManager::get_max_log_size(logging_manager), 2 * 1024 * 1024)
  assert_eq(azimuth::LoggingManager::get_max_log_files(logging_manager), 20)
  
  // 验证日志提供器已自动应用新配置
  assert_eq(azimuth::LoggerProvider::get_min_severity(logger_provider), azimuth::Debug)
  assert_false(azimuth::LoggerProvider::is_timestamp_included(logger_provider))
  assert_true(azimuth::LoggerProvider::is_thread_id_included(logger_provider))
  
  // 记录更多日志以测试新配置
  let new_debug_log = azimuth::LogRecord::new(azimuth::Debug, "New debug message")
  let new_info_log = azimuth::LogRecord::new(azimuth::Info, "New info message")
  
  azimuth::Logger::emit(logger, new_debug_log) // 现在应该被记录
  azimuth::Logger::emit(logger, new_info_log) // 应该被记录
  
  // 验证所有日志记录
  let all_logged_records = azimuth::LoggerProvider::get_logged_records(logger_provider)
  assert_eq(all_logged_records.length(), 5) // 包括之前和新的日志
  
  // 验证最新的日志记录
  let latest_records = all_logged_records.slice(3, 5)
  assert_eq(latest_records.length(), 2)
  
  match azimuth::LogRecord::severity_number(latest_records[0]) {
    azimuth::Debug => assert_true(true)
    _ => assert_true(false)
  }
  
  match azimuth::LogRecord::severity_number(latest_records[1]) {
    azimuth::Info => assert_true(true)
    _ => assert_true(false)
  }
}

test "资源限制配置动态更新" {
  // 创建资源限制管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 设置初始资源限制配置
  let initial_resource_config = azimuth::ResourceConfig::new()
  azimuth::ResourceConfig::set_max_cpu_usage(initial_resource_config, 80.0) // 80%
  azimuth::ResourceConfig::set_max_memory_usage(initial_resource_config, 512 * 1024 * 1024) // 512MB
  azimuth::ResourceConfig::set_max_network_bandwidth(initial_resource_config, 10 * 1024 * 1024) // 10MB/s
  azimuth::ResourceConfig::set_max_disk_io(initial_resource_config, 50 * 1024 * 1024) // 50MB/s
  azimuth::ResourceConfig::set_monitoring_interval(initial_resource_config, 5000) // 5秒
  azimuth::ResourceConfig::set_throttling_enabled(initial_resource_config, true)
  
  // 应用初始资源限制配置
  azimuth::ResourceManager::apply_config(resource_manager, initial_resource_config)
  
  // 创建资源监控器
  let resource_monitor = azimuth::ResourceMonitor::new()
  azimuth::ResourceMonitor::set_resource_manager(resource_monitor, resource_manager)
  
  // 验证初始资源限制配置
  assert_eq(azimuth::ResourceManager::get_max_cpu_usage(resource_manager), 80.0)
  assert_eq(azimuth::ResourceManager::get_max_memory_usage(resource_manager), 512 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_max_network_bandwidth(resource_manager), 10 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_max_disk_io(resource_manager), 50 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_monitoring_interval(resource_manager), 5000)
  assert_true(azimuth::ResourceManager::is_throttling_enabled(resource_manager))
  
  // 验证资源监控器使用初始配置
  assert_eq(azimuth::ResourceMonitor::get_max_cpu_usage(resource_monitor), 80.0)
  assert_eq(azimuth::ResourceMonitor::get_max_memory_usage(resource_monitor), 512 * 1024 * 1024)
  assert_true(azimuth::ResourceMonitor::is_throttling_enabled(resource_monitor))
  
  // 启动资源监控
  azimuth::ResourceMonitor::start_monitoring(resource_monitor)
  
  // 模拟资源使用
  azimuth::ResourceMonitor::simulate_cpu_usage(resource_monitor, 70.0) // 低于限制
  azimuth::ResourceMonitor::simulate_memory_usage(resource_monitor, 400 * 1024 * 1024) // 低于限制
  
  // 验证资源状态
  assert_eq(azimuth::ResourceMonitor::get_cpu_usage(resource_monitor), 70.0)
  assert_eq(azimuth::ResourceMonitor::get_memory_usage(resource_monitor), 400 * 1024 * 1024)
  assert_eq(azimuth::ResourceMonitor::get_status(resource_monitor), azimuth::ResourceStatus::Normal)
  
  // 动态更新资源限制配置
  let updated_resource_config = azimuth::ResourceConfig::new()
  azimuth::ResourceConfig::set_max_cpu_usage(updated_resource_config, 60.0) // 60%
  azimuth::ResourceConfig::set_max_memory_usage(updated_resource_config, 256 * 1024 * 1024) // 256MB
  azimuth::ResourceConfig::set_max_network_bandwidth(updated_resource_config, 5 * 1024 * 1024) // 5MB/s
  azimuth::ResourceConfig::set_max_disk_io(updated_resource_config, 25 * 1024 * 1024) // 25MB/s
  azimuth::ResourceConfig::set_monitoring_interval(updated_resource_config, 2000) // 2秒
  azimuth::ResourceConfig::set_throttling_enabled(updated_resource_config, true)
  
  // 应用更新的资源限制配置
  azimuth::ResourceManager::apply_config(resource_manager, updated_resource_config)
  
  // 验证资源限制配置已更新
  assert_eq(azimuth::ResourceManager::get_max_cpu_usage(resource_manager), 60.0)
  assert_eq(azimuth::ResourceManager::get_max_memory_usage(resource_manager), 256 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_max_network_bandwidth(resource_manager), 5 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_max_disk_io(resource_manager), 25 * 1024 * 1024)
  assert_eq(azimuth::ResourceManager::get_monitoring_interval(resource_manager), 2000)
  assert_true(azimuth::ResourceManager::is_throttling_enabled(resource_manager))
  
  // 验证资源监控器已自动应用新配置
  assert_eq(azimuth::ResourceMonitor::get_max_cpu_usage(resource_monitor), 60.0)
  assert_eq(azimuth::ResourceMonitor::get_max_memory_usage(resource_monitor), 256 * 1024 * 1024)
  
  // 模拟资源使用（现在可能超过限制）
  azimuth::ResourceMonitor::simulate_cpu_usage(resource_monitor, 70.0) // 超过新的60%限制
  azimuth::ResourceMonitor::simulate_memory_usage(resource_monitor, 300 * 1024 * 1024) // 超过新的256MB限制
  
  // 验证资源状态变化
  assert_eq(azimuth::ResourceMonitor::get_cpu_usage(resource_monitor), 70.0)
  assert_eq(azimuth::ResourceMonitor::get_memory_usage(resource_monitor), 300 * 1024 * 1024)
  assert_eq(azimuth::ResourceMonitor::get_status(resource_monitor), azimuth::ResourceStatus::Throttled)
  
  // 验证资源限制事件
  let resource_events = azimuth::ResourceMonitor::get_resource_events(resource_monitor)
  assert_true(resource_events.length() > 0)
  
  // 检查是否有CPU限制事件
  let mut cpu_limit_events = 0
  let mut memory_limit_events = 0
  
  for event in resource_events {
    match azimuth::ResourceEvent::event_type(event) {
      azimuth::ResourceEventType::CPULimitExceeded => cpu_limit_events = cpu_limit_events + 1
      azimuth::ResourceEventType::MemoryLimitExceeded => memory_limit_events = memory_limit_events + 1
      _ => ()
    }
  }
  
  assert_true(cpu_limit_events > 0)
  assert_true(memory_limit_events > 0)
  
  // 停止资源监控
  azimuth::ResourceMonitor::stop_monitoring(resource_monitor)
  
  // 验证监控器状态
  assert_eq(azimuth::ResourceMonitor::get_status(resource_monitor), azimuth::ResourceStatus::Stopped)
}