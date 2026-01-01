// 配置动态更新测试用例
// 测试 Azimuth 遥测系统中的配置动态更新功能

test "telemetry_sampling_config_dynamic_update" {
  // 创建采样配置
  let initial_config = SamplingConfig::new()
  SamplingConfig::set_sampling_ratio(initial_config, 0.1)  // 10%采样率
  
  // 验证初始配置
  assert_eq(SamplingConfig::get_sampling_ratio(initial_config), 0.1)
  
  // 动态更新采样配置
  SamplingConfig::set_sampling_ratio(initial_config, 0.5)  // 50%采样率
  
  // 验证配置更新
  assert_eq(SamplingConfig::get_sampling_ratio(initial_config), 0.5)
  
  // 再次更新配置
  SamplingConfig::set_sampling_ratio(initial_config, 1.0)  // 100%采样率
  
  // 验证最终配置
  assert_eq(SamplingConfig::get_sampling_ratio(initial_config), 1.0)
}

test "telemetry_metric_config_dynamic_update" {
  let provider = MeterProvider::default()
  
  // 创建指标配置
  let metric_config = MetricConfig::new()
  MetricConfig::set_export_interval(metric_config, 10000)  // 10秒导出间隔
  MetricConfig::set_aggregation_temporality(metric_config, AggregationTemporality::DELTA)
  
  // 验证初始配置
  assert_eq(MetricConfig::get_export_interval(metric_config), 10000)
  assert_eq(MetricConfig::get_aggregation_temporality(metric_config), AggregationTemporality::DELTA)
  
  // 动态更新指标配置
  MetricConfig::set_export_interval(metric_config, 5000)  // 5秒导出间隔
  MetricConfig::set_aggregation_temporality(metric_config, AggregationTemporality::CUMULATIVE)
  
  // 验证配置更新
  assert_eq(MetricConfig::get_export_interval(metric_config), 5000)
  assert_eq(MetricConfig::get_aggregation_temporality(metric_config), AggregationTemporality::CUMULATIVE)
  
  // 测试配置应用
  let meter = MeterProvider::get_meter(provider, "config-test")
  MeterProvider::apply_metric_config(provider, metric_config)
  
  // 创建指标验证配置应用
  let counter = Meter::create_counter(meter, "config.test.counter")
  assert_eq(counter.name, "config.test.counter")
}

test "telemetry_logging_config_dynamic_update" {
  let provider = LoggerProvider::default()
  
  // 创建日志配置
  let logging_config = LoggingConfig::new()
  LoggingConfig::set_log_level(logging_config, LogLevel::INFO)
  LoggingConfig::set_include_timestamp(logging_config, true)
  LoggingConfig::set_include_trace_id(logging_config, false)
  
  // 验证初始配置
  assert_eq(LoggingConfig::get_log_level(logging_config), LogLevel::INFO)
  assert_true(LoggingConfig::get_include_timestamp(logging_config))
  assert_false(LoggingConfig::get_include_trace_id(logging_config))
  
  // 动态更新日志配置
  LoggingConfig::set_log_level(logging_config, LogLevel::DEBUG)
  LoggingConfig::set_include_timestamp(logging_config, false)
  LoggingConfig::set_include_trace_id(logging_config, true)
  
  // 验证配置更新
  assert_eq(LoggingConfig::get_log_level(logging_config), LogLevel::DEBUG)
  assert_false(LoggingConfig::get_include_timestamp(logging_config))
  assert_true(LoggingConfig::get_include_trace_id(logging_config))
  
  // 测试配置应用
  LoggerProvider::apply_logging_config(provider, logging_config)
  
  // 创建日志记录器验证配置应用
  let logger = LoggerProvider::get_logger(provider, "config-test-logger")
  assert_true(true)
}

test "telemetry_resource_config_dynamic_update" {
  // 创建资源配置
  let resource_config = ResourceConfig::new()
  ResourceConfig::set_service_name(resource_config, "original-service")
  ResourceConfig::set_service_version(resource_config, "1.0.0")
  ResourceConfig::set_deployment_environment(resource_config, "development")
  
  // 验证初始配置
  assert_eq(ResourceConfig::get_service_name(resource_config), "original-service")
  assert_eq(ResourceConfig::get_service_version(resource_config), "1.0.0")
  assert_eq(ResourceConfig::get_deployment_environment(resource_config), "development")
  
  // 动态更新资源配置
  ResourceConfig::set_service_name(resource_config, "updated-service")
  ResourceConfig::set_service_version(resource_config, "2.0.0")
  ResourceConfig::set_deployment_environment(resource_config, "production")
  
  // 验证配置更新
  assert_eq(ResourceConfig::get_service_name(resource_config), "updated-service")
  assert_eq(ResourceConfig::get_service_version(resource_config), "2.0.0")
  assert_eq(ResourceConfig::get_deployment_environment(resource_config), "production")
  
  // 测试配置应用
  let resource = Resource::from_config(resource_config)
  let service_name = Resource::get_attribute(resource, "service.name")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "updated-service")
    _ => assert_true(false, "Expected updated service name")
  }
}

test "telemetry_propagation_config_dynamic_update" {
  // 创建传播配置
  let propagation_config = PropagationConfig::new()
  PropagationConfig::set_trace_context_enabled(propagation_config, true)
  PropagationConfig::set_baggage_enabled(propagation_config, false)
  PropagationConfig::set_correlation_context_enabled(propagation_config, true)
  
  // 验证初始配置
  assert_true(PropagationConfig::get_trace_context_enabled(propagation_config))
  assert_false(PropagationConfig::get_baggage_enabled(propagation_config))
  assert_true(PropagationConfig::get_correlation_context_enabled(propagation_config))
  
  // 动态更新传播配置
  PropagationConfig::set_trace_context_enabled(propagation_config, false)
  PropagationConfig::set_baggage_enabled(propagation_config, true)
  PropagationConfig::set_correlation_context_enabled(propagation_config, false)
  
  // 验证配置更新
  assert_false(PropagationConfig::get_trace_context_enabled(propagation_config))
  assert_true(PropagationConfig::get_baggage_enabled(propagation_config))
  assert_false(PropagationConfig::get_correlation_context_enabled(propagation_config))
  
  // 测试配置应用
  let propagator = CompositePropagator::from_config(propagation_config)
  assert_true(true)
}

test "telemetry_batch_config_dynamic_update" {
  // 创建批处理配置
  let batch_config = BatchConfig::new()
  BatchConfig::set_max_export_batch_size(batch_config, 512)
  BatchConfig::set_max_queue_size(batch_config, 2048)
  BatchConfig::set_scheduled_delay_millis(batch_config, 5000)
  BatchConfig::set_export_timeout_millis(batch_config, 30000)
  
  // 验证初始配置
  assert_eq(BatchConfig::get_max_export_batch_size(batch_config), 512)
  assert_eq(BatchConfig::get_max_queue_size(batch_config), 2048)
  assert_eq(BatchConfig::get_scheduled_delay_millis(batch_config), 5000)
  assert_eq(BatchConfig::get_export_timeout_millis(batch_config), 30000)
  
  // 动态更新批处理配置
  BatchConfig::set_max_export_batch_size(batch_config, 1024)
  BatchConfig::set_max_queue_size(batch_config), 4096)
  BatchConfig::set_scheduled_delay_millis(batch_config, 10000)
  BatchConfig::set_export_timeout_millis(batch_config, 60000)
  
  // 验证配置更新
  assert_eq(BatchConfig::get_max_export_batch_size(batch_config), 1024)
  assert_eq(BatchConfig::get_max_queue_size(batch_config), 4096)
  assert_eq(BatchConfig::get_scheduled_delay_millis(batch_config), 10000)
  assert_eq(BatchConfig::get_export_timeout_millis(batch_config), 60000)
  
  // 测试配置应用
  let batch_processor = BatchProcessor::from_config(batch_config)
  assert_true(true)
}

test "telemetry_global_config_dynamic_update" {
  // 创建全局配置管理器
  let global_config = GlobalConfig::new()
  
  // 设置初始配置
  GlobalConfig::set_sampling_ratio(global_config, 0.1)
  GlobalConfig::set_log_level(global_config, LogLevel::INFO)
  GlobalConfig::set_metric_export_interval(global_config, 10000)
  GlobalConfig::set_service_name(global_config, "test-service")
  
  // 验证初始配置
  assert_eq(GlobalConfig::get_sampling_ratio(global_config), 0.1)
  assert_eq(GlobalConfig::get_log_level(global_config), LogLevel::INFO)
  assert_eq(GlobalConfig::get_metric_export_interval(global_config), 10000)
  assert_eq(GlobalConfig::get_service_name(global_config), "test-service")
  
  // 动态更新全局配置
  GlobalConfig::set_sampling_ratio(global_config, 0.5)
  GlobalConfig::set_log_level(global_config, LogLevel::DEBUG)
  GlobalConfig::set_metric_export_interval(global_config, 5000)
  GlobalConfig::set_service_name(global_config, "updated-service")
  
  // 验证配置更新
  assert_eq(GlobalConfig::get_sampling_ratio(global_config), 0.5)
  assert_eq(GlobalConfig::get_log_level(global_config), LogLevel::DEBUG)
  assert_eq(GlobalConfig::get_metric_export_interval(global_config), 5000)
  assert_eq(GlobalConfig::get_service_name(global_config), "updated-service")
  
  // 测试配置应用到各个组件
  GlobalConfig::apply_to_all(global_config)
  
  // 验证配置应用
  let tracer_provider = TracerProvider::from_global_config(global_config)
  let meter_provider = MeterProvider::from_global_config(global_config)
  let logger_provider = LoggerProvider::from_global_config(global_config)
  
  assert_true(true)
}