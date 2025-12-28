// 动态配置管理测试用例
// 测试Azimuth遥测系统的动态配置更新、热重载和配置验证功能

test "runtime_tracer_configuration_update" {
  // 测试运行时Tracer配置更新
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "config-test")
  
  // 初始配置
  let initial_config = TracerConfig::new(
    Some("initial-service"),
    Some("1.0.0"),
    Some(true),    // sampling_enabled
    Some(1000),    // max_attributes_per_span
    Some(128),     // max_events_per_span
    Some(32)       // max_links_per_span
  )
  
  // 应用初始配置
  TracerProvider::apply_config(tracer_provider, initial_config)
  
  let span = Tracer::start_span(tracer, "config-test-span-1")
  Span::set_attribute(span, "service.name", StringValue("initial-service"))
  Span::end(span)
  
  // 动态更新配置
  let updated_config = TracerConfig::new(
    Some("updated-service"),
    Some("2.0.0"),
    Some(false),   // sampling_disabled
    Some(2000),    // increased max_attributes
    Some(256),     // increased max_events
    Some(64)       // increased max_links
  )
  
  // 验证配置更新
  let config_update_success = TracerProvider::update_config(tracer_provider, updated_config)
  assert_true(config_update_success)
  
  // 使用新配置创建Span
  let span2 = Tracer::start_span(tracer, "config-test-span-2")
  Span::set_attribute(span2, "service.name", StringValue("updated-service"))
  Span::set_attribute(span2, "service.version", StringValue("2.0.0"))
  
  // 测试新的限制
  for i = 0; i < 1500; i = i + 1 {
    Span::set_attribute(span2, "attr." + i.to_string(), IntValue(i))
  }
  
  for i = 0; i < 200; i = i + 1 {
    Span::add_event(span2, "event-" + i.to_string(), Some([("event_index", IntValue(i))]))
  }
  
  Span::end(span2)
  
  // 验证配置生效
  assert_eq(updated_config.service_name, Some("updated-service"))
  assert_eq(updated_config.service_version, Some("2.0.0"))
  assert_eq(updated_config.sampling_enabled, Some(false))
}

test "metrics_configuration_hot_reload" {
  // 测试度量配置热重载
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics-config-test")
  
  // 初始度量配置
  let initial_metrics_config = MetricsConfig::new(
    Some(5000),        // aggregation_interval_ms
    Some(100),         // max_cardinality
    Some(true),        // exemplars_enabled
    Some(1000)         // max_time_series
  )
  
  MeterProvider::apply_metrics_config(meter_provider, initial_metrics_config)
  
  // 创建度量工具
  let counter = Meter::create_counter(meter, "hot_reload_counter")
  let histogram = Meter::create_histogram(meter, "hot_reload_histogram")
  
  // 生成初始数据
  for i = 0; i < 50; i = i + 1 {
    Counter::add_with_attributes(counter, i.to_double(), [("dimension", StringValue("value-" + i.to_string()))])
    Histogram::record_with_attributes(histogram, i.to_double(), [("dimension", StringValue("value-" + i.to_string()))])
  }
  
  // 热重载配置
  let updated_metrics_config = MetricsConfig::new(
    Some(2000),        // reduced aggregation_interval
    Some(200),         // increased max_cardinality
    Some(false),       // exemplars_disabled
    Some(2000)         // increased max_time_series
  )
  
  let hot_reload_success = MeterProvider::hot_reload_metrics_config(meter_provider, updated_metrics_config)
  assert_true(hot_reload_success)
  
  // 验证新配置生效
  for i = 50; i < 150; i = i + 1 {
    Counter::add_with_attributes(counter, i.to_double(), [("dimension", StringValue("value-" + i.to_string()))])
    Histogram::record_with_attributes(histogram, i.to_double(), [("dimension", StringValue("value-" + i.to_string()))])
  }
  
  // 验证配置更新
  assert_eq(updated_metrics_config.aggregation_interval_ms, Some(2000))
  assert_eq(updated_metrics_config.max_cardinality, Some(200))
  assert_eq(updated_metrics_config.exemplars_enabled, Some(false))
  assert_eq(updated_metrics_config.max_time_series, Some(2000))
}

test "logger_configuration_dynamic_update" {
  // 测试Logger配置动态更新
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "logger-config-test")
  
  // 初始Logger配置
  let initial_logger_config = LoggerConfig::new(
    Some(Info),       // min_log_level
    Some(true),       // include_timestamp
    Some(true),       // include_resource
    Some(true),       // include_span_context
    Some(1000)        // max_log_record_size
  )
  
  LoggerProvider::apply_logger_config(logger_provider, initial_logger_config)
  
  // 发送初始日志
  let log_record1 = LogRecord::new(Info, "Initial config log message")
  Logger::emit(logger, log_record1)
  
  // 动态更新Logger配置
  let updated_logger_config = LoggerConfig::new(
    Some(Debug),      // lower log level
    Some(false),      // exclude_timestamp
    Some(false),      // exclude_resource
    Some(false),      // exclude_span_context
    Some(2000)        // increased max size
  )
  
  let config_update_success = LoggerProvider::update_logger_config(logger_provider, updated_logger_config)
  assert_true(config_update_success)
  
  // 发送更新后的日志
  let log_record2 = LogRecord::new(Debug, "Updated config log message")
  Logger::emit(logger, log_record2)
  
  let log_record3 = LogRecord::new(Warn, "Warning message with lower config")
  Logger::emit(logger, log_record3)
  
  // 验证配置更新
  assert_eq(updated_logger_config.min_log_level, Some(Debug))
  assert_eq(updated_logger_config.include_timestamp, Some(false))
  assert_eq(updated_logger_config.include_resource, Some(false))
  assert_eq(updated_logger_config.include_span_context, Some(false))
  assert_eq(updated_logger_config.max_log_record_size, Some(2000))
}

test "resource_configuration_validation" {
  // 测试资源配置验证
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  // 测试有效资源配置
  let valid_resource_config = ResourceConfig::new(
    Some([("service.name", StringValue("validation-test-service"))]),
    Some([("environment", StringValue("test"))]),
    Some([("version", StringValue("1.0.0"))])
  )
  
  let validation_result = ResourceConfig::validate(valid_resource_config)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length, 0)
  
  // 应用有效配置
  TracerProvider::apply_resource_config(tracer_provider, valid_resource_config)
  MeterProvider::apply_resource_config(meter_provider, valid_resource_config)
  LoggerProvider::apply_resource_config(logger_provider, valid_resource_config)
  
  // 测试无效资源配置
  let invalid_resource_config = ResourceConfig::new(
    Some([("", StringValue("empty-key"))]),  // 空键名
    Some([("service.name", StringValue(""))]),  // 空值
    Some([("key.with.invalid.chars!", StringValue("value"))])  // 无效字符
  )
  
  let invalid_validation_result = ResourceConfig::validate(invalid_resource_config)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length > 0)
  
  // 验证错误信息
  let has_empty_key_error = invalid_validation_result.errors.any(fn(error) {
    error.contains("empty key")
  })
  let has_empty_value_error = invalid_validation_result.errors.any(fn(error) {
    error.contains("empty value")
  })
  let has_invalid_chars_error = invalid_validation_result.errors.any(fn(error) {
    error.contains("invalid characters")
  })
  
  assert_true(has_empty_key_error)
  assert_true(has_empty_value_error)
  assert_true(has_invalid_chars_error)
}

test "exporter_configuration_fallback" {
  // 测试导出器配置回退机制
  let tracer_provider = TracerProvider::default()
  
  // 主导出器配置
  let primary_exporter_config = ExporterConfig::new(
    "http://primary-exporter:4317",
    Some("otlp"),
    Some(5000),
    Some(3)
  )
  
  // 备用导出器配置
  let fallback_exporter_config = ExporterConfig::new(
    "http://fallback-exporter:4317",
    Some("otlp"),
    Some(10000),
    Some(5)
  )
  
  // 配置导出器链
  let exporter_chain = ExporterChain::new()
  ExporterChain::add_primary(exporter_chain, primary_exporter_config)
  ExporterChain::add_fallback(exporter_chain, fallback_exporter_config)
  
  TracerProvider::set_exporter_chain(tracer_provider, exporter_chain)
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "fallback-test")
  
  // 测试主导出器工作正常的情况
  let span1 = Tracer::start_span(tracer, "primary-exporter-test")
  Span::set_attribute(span1, "exporter.test", StringValue("primary"))
  Span::end(span1)
  
  // 模拟主导出器失败，测试回退到备用导出器
  let primary_exporter_healthy = false
  if not primary_exporter_healthy {
    ExporterChain::mark_primary_unhealthy(exporter_chain)
    
    let span2 = Tracer::start_span(tracer, "fallback-exporter-test")
    Span::set_attribute(span2, "exporter.test", StringValue("fallback"))
    Span::end(span2)
    
    // 验证使用了备用导出器
    assert_true(ExporterChain::is_using_fallback(exporter_chain))
  }
  
  // 测试主导出器恢复
  let primary_exporter_recovered = true
  if primary_exporter_recovered {
    ExporterChain::mark_primary_healthy(exporter_chain)
    
    let span3 = Tracer::start_span(tracer, "recovered-primary-test")
    Span::set_attribute(span3, "exporter.test", StringValue("recovered-primary"))
    Span::end(span3)
    
    // 验证恢复使用主导出器
    assert_false(ExporterChain::is_using_fallback(exporter_chain))
  }
}

test "configuration_persistence_and_recovery" {
  // 测试配置持久化和恢复
  let config_manager = ConfigurationManager::new()
  
  // 创建完整的系统配置
  let system_config = SystemConfig::new(
    TracerConfig::new(Some("persist-test"), Some("1.0.0"), Some(true), None, None, None),
    MetricsConfig::new(Some(5000), Some(100), Some(true), Some(1000)),
    LoggerConfig::new(Some(Info), Some(true), Some(true), Some(true), Some(1000)),
    ResourceConfig::new(
      Some([("service.name", StringValue("persist-test-service"))]),
      Some([("environment", StringValue("test"))]),
      None
    )
  )
  
  // 持久化配置
  let persist_success = ConfigurationManager::persist(config_manager, system_config, "test-config.json")
  assert_true(persist_success)
  
  // 验证配置文件存在
  let config_file_exists = file_exists("test-config.json")
  assert_true(config_file_exists)
  
  // 修改当前配置
  let modified_config = SystemConfig::new(
    TracerConfig::new(Some("modified-test"), Some("2.0.0"), Some(false), None, None, None),
    MetricsConfig::new(Some(3000), Some(200), Some(false), Some(2000)),
    LoggerConfig::new(Some(Debug), Some(false), Some(false), Some(false), Some(2000)),
    ResourceConfig::new(
      Some([("service.name", StringValue("modified-test-service"))]),
      None,
      None
    )
  )
  
  // 从持久化配置恢复
  let recovered_config = ConfigurationManager::load(config_manager, "test-config.json")
  assert_true(recovered_config.is_some)
  
  let loaded_config = recovered_config.unwrap()
  
  // 验证恢复的配置与原始配置一致
  assert_eq(loaded_config.tracer_config.service_name, system_config.tracer_config.service_name)
  assert_eq(loaded_config.metrics_config.aggregation_interval_ms, system_config.metrics_config.aggregation_interval_ms)
  assert_eq(loaded_config.logger_config.min_log_level, system_config.logger_config.min_log_level)
  
  // 测试配置版本管理
  let version_persist_success = ConfigurationManager::persist_with_version(config_manager, system_config, "test-config-v2.json", "2.0.0")
  assert_true(version_persist_success)
  
  let versioned_config = ConfigurationManager::load_with_version(config_manager, "test-config-v2.json", "2.0.0")
  assert_true(versioned_config.is_some)
  
  // 清理测试文件
  delete_file("test-config.json")
  delete_file("test-config-v2.json")
}