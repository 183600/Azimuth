// Azimuth Dynamic Configuration Management Test Suite
// 测试遥测系统的配置管理动态更新功能

test "采样率动态更新" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置动态采样率更新
  let sampling_config = DynamicSamplingConfig {
    initial_sampling_rate: 0.1,
    min_sampling_rate: 0.01,
    max_sampling_rate: 1.0,
    update_strategy: "gradual",
    update_interval: 1000, // 1秒
    validation_enabled: true
  }
  
  DynamicConfig::configure_sampling(telemetry_provider, sampling_config)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "dynamic.sampling.test")
  
  // 测试初始采样率
  let initial_sampled_count = 0
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "initial.sampling.test")
    if SpanContext::sampled(Span::span_context(span)) {
      initial_sampled_count = initial_sampled_count + 1
    }
    Span::end(span)
  }
  
  let initial_sampling_rate = initial_sampled_count.to_double() / 1000.0
  assert_true(initial_sampling_rate >= 0.05 && initial_sampling_rate <= 0.15) // 允许误差范围
  
  // 动态更新采样率
  DynamicConfig::update_sampling_rate(telemetry_provider, 0.5)
  
  // 等待配置更新生效
  TimeSimulator::advance(1000)
  
  // 测试更新后的采样率
  let updated_sampled_count = 0
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "updated.sampling.test")
    if SpanContext::sampled(Span::span_context(span)) {
      updated_sampled_count = updated_sampled_count + 1
    }
    Span::end(span)
  }
  
  let updated_sampling_rate = updated_sampled_count.to_double() / 1000.0
  assert_true(updated_sampling_rate >= 0.45 && updated_sampling_rate <= 0.55) // 允许误差范围
  
  // 验证采样率确实发生了变化
  assert_true(updated_sampling_rate > initial_sampling_rate * 3) // 至少增加了3倍
  
  assert_true(true)
}

test "导出器配置动态更新" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置初始导出器
  let initial_exporter_config = ExporterConfig {
    endpoint: "http://initial.example.com:4318",
    protocol: "http",
    timeout: 5000,
    batch_size: 100,
    retry_attempts: 3
  }
  
  DynamicConfig::configure_exporter(telemetry_provider, initial_exporter_config)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "dynamic.exporter.test")
  
  // 创建span并导出
  let span = Tracer::start_span(tracer, "exporter.config.test")
  Span::set_attribute(span, "test.phase", "initial")
  Span::end(span)
  
  // 获取当前导出器配置
  let current_config = DynamicConfig::get_exporter_config(telemetry_provider)
  assert_eq(current_config.endpoint, "http://initial.example.com:4318")
  assert_eq(current_config.timeout, 5000)
  
  // 动态更新导出器配置
  let updated_exporter_config = ExporterConfig {
    endpoint: "http://updated.example.com:4318",
    protocol: "http",
    timeout: 10000, // 增加超时时间
    batch_size: 200, // 增加批处理大小
    retry_attempts: 5  // 增加重试次数
  }
  
  DynamicConfig::update_exporter_config(telemetry_provider, updated_exporter_config)
  
  // 等待配置更新生效
  TimeSimulator::advance(1000)
  
  // 验证配置已更新
  let updated_config = DynamicConfig::get_exporter_config(telemetry_provider)
  assert_eq(updated_config.endpoint, "http://updated.example.com:4318")
  assert_eq(updated_config.timeout, 10000)
  assert_eq(updated_config.batch_size, 200)
  assert_eq(updated_config.retry_attempts, 5)
  
  // 使用新配置创建span并导出
  let span2 = Tracer::start_span(tracer, "exporter.config.test")
  Span::set_attribute(span2, "test.phase", "updated")
  Span::end(span2)
  
  // 验证新配置生效（通过检查导出器行为）
  let export_success = DynamicConfig::test_exporter_config(telemetry_provider)
  assert_true(export_success)
  
  assert_true(true)
}

test "度量配置动态更新" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置初始度量设置
  let initial_metrics_config = MetricsConfig {
    aggregation_interval: 10000,  // 10秒
    histogram_buckets: [10.0, 50.0, 100.0, 500.0],
    max_time_series: 10000,
    max_active_metrics: 1000,
    export_interval: 60000        // 1分钟
  }
  
  DynamicConfig::configure_metrics(telemetry_provider, initial_metrics_config)
  
  // 创建meter
  let meter = TelemetryProvider::get_meter(telemetry_provider, "dynamic.metrics.test")
  
  // 创建直方图
  let histogram = Meter::create_histogram(meter, "dynamic.histogram")
  
  // 记录一些值
  for i = 0; i < 100; i = i + 1 {
    Histogram::record(histogram, i.to_double() * 5.0)
  }
  
  // 获取当前度量配置
  let current_config = DynamicConfig::get_metrics_config(telemetry_provider)
  assert_eq(current_config.aggregation_interval, 10000)
  assert_eq(current_config.histogram_buckets, [10.0, 50.0, 100.0, 500.0])
  
  // 动态更新度量配置
  let updated_metrics_config = MetricsConfig {
    aggregation_interval: 5000,   // 减少到5秒
    histogram_buckets: [5.0, 25.0, 100.0, 250.0, 1000.0], // 更改桶边界
    max_time_series: 20000,       // 增加时间序列限制
    max_active_metrics: 2000,     // 增加活动度量限制
    export_interval: 30000        // 减少到30秒
  }
  
  DynamicConfig::update_metrics_config(telemetry_provider, updated_metrics_config)
  
  // 等待配置更新生效
  TimeSimulator::advance(1000)
  
  // 验证配置已更新
  let updated_config = DynamicConfig::get_metrics_config(telemetry_provider)
  assert_eq(updated_config.aggregation_interval, 5000)
  assert_eq(updated_config.histogram_buckets, [5.0, 25.0, 100.0, 250.0, 1000.0])
  assert_eq(updated_config.max_time_series, 20000)
  assert_eq(updated_config.max_active_metrics, 2000)
  assert_eq(updated_config.export_interval, 30000)
  
  // 使用新配置记录更多值
  for i = 0; i < 100; i = i + 1 {
    Histogram::record(histogram, i.to_double() * 10.0)
  }
  
  // 验证新配置生效（通过检查聚合行为）
  let aggregation_result = DynamicConfig::test_metrics_aggregation(telemetry_provider)
  assert_true(aggregation_result)
  
  assert_true(true)
}

test "资源属性动态更新" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置初始资源属性
  let initial_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("dynamic.service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("deployment.environment", AttributeValue::StringValue("development")),
      ("host.name", AttributeValue::StringValue("initial-host"))
    ]
  }
  
  DynamicConfig::configure_resource(telemetry_provider, initial_resource)
  
  // 创建tracer
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "dynamic.resource.test")
  
  // 创建span并验证资源属性
  let span = Tracer::start_span(tracer, "resource.test")
  let resource = Span::get_resource(span)
  
  let service_name = Resource::get_attribute(resource, "service.name")
  let service_version = Resource::get_attribute(resource, "service.version")
  let environment = Resource::get_attribute(resource, "deployment.environment")
  
  match service_name {
    Some(AttributeValue::StringValue(name)) => assert_eq(name, "dynamic.service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match environment {
    Some(AttributeValue::StringValue(env)) => assert_eq(env, "development")
    _ => assert_true(false)
  }
  
  Span::end(span)
  
  // 动态更新资源属性
  let updated_attributes = [
    ("service.name", AttributeValue::StringValue("dynamic.service")),
    ("service.version", AttributeValue::StringValue("1.1.0")), // 更新版本
    ("deployment.environment", AttributeValue::StringValue("production")), // 更新环境
    ("host.name", AttributeValue::StringValue("updated-host")), // 更新主机名
    ("region", AttributeValue::StringValue("us-west-2")) // 新增属性
  ]
  
  DynamicConfig::update_resource_attributes(telemetry_provider, updated_attributes)
  
  // 等待配置更新生效
  TimeSimulator::advance(1000)
  
  // 创建新span并验证更新的资源属性
  let span2 = Tracer::start_span(tracer, "resource.updated.test")
  let updated_resource = Span::get_resource(span2)
  
  let updated_service_version = Resource::get_attribute(updated_resource, "service.version")
  let updated_environment = Resource::get_attribute(updated_resource, "deployment.environment")
  let updated_host_name = Resource::get_attribute(updated_resource, "host.name")
  let region = Resource::get_attribute(updated_resource, "region")
  
  match updated_service_version {
    Some(AttributeValue::StringValue(version)) => assert_eq(version, "1.1.0")
    _ => assert_true(false)
  }
  
  match updated_environment {
    Some(AttributeValue::StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
  
  match updated_host_name {
    Some(AttributeValue::StringValue(host)) => assert_eq(host, "updated-host")
    _ => assert_true(false)
  }
  
  match region {
    Some(AttributeValue::StringValue(reg)) => assert_eq(reg, "us-west-2")
    _ => assert_true(false)
  }
  
  Span::end(span2)
  
  assert_true(true)
}

test "日志配置动态更新" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置初始日志设置
  let initial_logging_config = LoggingConfig {
    default_level: "INFO",
    output_format: "json",
    include_timestamp: true,
    include_level: true,
    include_resource: true,
    max_log_size: 1024,
    buffer_size: 1000
  }
  
  DynamicConfig::configure_logging(telemetry_provider, initial_logging_config)
  
  // 创建logger
  let logger = TelemetryProvider::get_logger(telemetry_provider, "dynamic.logging.test")
  
  // 记录日志
  Logger::emit_log(logger, "INFO", "Initial config test log", [])
  Logger::emit_log(logger, "DEBUG", "This should be filtered out", [])
  Logger::emit_log(logger, "WARN", "Warning log", [])
  
  // 获取当前日志配置
  let current_config = DynamicConfig::get_logging_config(telemetry_provider)
  assert_eq(current_config.default_level, "INFO")
  assert_eq(current_config.output_format, "json")
  assert_true(current_config.include_timestamp)
  
  // 动态更新日志配置
  let updated_logging_config = LoggingConfig {
    default_level: "DEBUG",       // 降低日志级别
    output_format: "text",        // 更改输出格式
    include_timestamp: false,     // 不包含时间戳
    include_level: true,
    include_resource: false,      // 不包含资源信息
    max_log_size: 2048,           // 增加最大日志大小
    buffer_size: 2000             // 增加缓冲区大小
  }
  
  DynamicConfig::update_logging_config(telemetry_provider, updated_logging_config)
  
  // 等待配置更新生效
  TimeSimulator::advance(1000)
  
  // 验证配置已更新
  let updated_config = DynamicConfig::get_logging_config(telemetry_provider)
  assert_eq(updated_config.default_level, "DEBUG")
  assert_eq(updated_config.output_format, "text")
  assert_false(updated_config.include_timestamp)
  assert_false(updated_config.include_resource)
  
  // 使用新配置记录日志
  Logger::emit_log(logger, "INFO", "Updated config test log", [])
  Logger::emit_log(logger, "DEBUG", "This should now be included", [])
  Logger::emit_log(logger, "ERROR", "Error log", [])
  
  // 验证新配置生效（通过检查日志输出）
  let log_output = DynamicConfig::get_recent_log_output(telemetry_provider)
  assert_true(log_output.contains("This should now be included"))
  assert_false(log_output.contains("\"timestamp\"")) // 文本格式不应包含JSON时间戳字段
  
  assert_true(true)
}

test "配置变更通知和回滚" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置变更通知
  let notification_config = NotificationConfig {
    enable_notifications: true,
    notification_handlers: ["email", "webhook"],
    notification_threshold: "warning",
    rollback_on_failure: true,
    max_rollback_attempts: 3
  }
  
  DynamicConfig::configure_notifications(telemetry_provider, notification_config)
  
  // 创建配置变更监听器
  let config_changes = []
  let change_listener = fn(config_change) {
    config_changes.push(config_change)
  }
  
  DynamicConfig::add_change_listener(telemetry_provider, change_listener)
  
  // 创建初始配置
  let initial_config = {
    "sampling.rate": 0.1,
    "exporter.timeout": 5000,
    "metrics.aggregation.interval": 10000
  }
  
  DynamicConfig::apply_config(telemetry_provider, initial_config)
  
  // 验证初始配置变更通知
  assert_eq(config_changes.length(), 1)
  let first_change = config_changes[0]
  assert_eq(first_change.type, "applied")
  assert_eq(first_change.config["sampling.rate"], "0.1")
  
  // 应用有效配置更新
  let valid_update = {
    "sampling.rate": 0.3,
    "exporter.timeout": 10000,
    "metrics.aggregation.interval": 5000
  }
  
  let update_result = DynamicConfig::apply_config(telemetry_provider, valid_update)
  
  // 验证更新成功
  assert_true(update_result.success)
  
  // 验证配置变更通知
  assert_eq(config_changes.length(), 2)
  let second_change = config_changes[1]
  assert_eq(second_change.type, "updated")
  assert_eq(second_change.config["sampling.rate"], "0.3")
  
  // 应用无效配置更新
  let invalid_update = {
    "sampling.rate": "invalid", // 无效值
    "exporter.timeout": -1,     // 负值
    "metrics.aggregation.interval": 0 // 零值
  }
  
  let invalid_result = DynamicConfig::apply_config(telemetry_provider, invalid_update)
  
  // 验证更新失败
  assert_false(invalid_result.success)
  
  // 验证配置变更通知
  assert_eq(config_changes.length(), 3)
  let third_change = config_changes[2]
  assert_eq(third_change.type, "failed")
  assert_true(third_change.errors.length() > 0)
  
  // 验证自动回滚
  assert_true(third_change.rollback_triggered)
  
  // 验证配置已回滚到上一个有效状态
  let current_config = DynamicConfig::get_current_config(telemetry_provider)
  let sampling_rate = DynamicConfig::get_config_value(current_config, "sampling.rate")
  match sampling_rate {
    Some(value) => {
      let rate = value.parse_double()
      match rate {
        Some(r) => assert_eq(r, 0.3) // 应该回滚到0.3
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证回滚通知
  assert_eq(config_changes.length(), 4)
  let fourth_change = config_changes[3]
  assert_eq(fourth_change.type, "rolled_back")
  
  assert_true(true)
}

test "基于条件的自动配置调整" {
  // 创建支持动态配置的遥测提供者
  let telemetry_provider = TelemetryProvider::with_dynamic_config()
  
  // 配置自动调整规则
  let auto_adjustment_rules = [
    // 高负载时降低采样率
    AutoAdjustmentRule {
      name: "high_load_sampling_reduction",
      condition: "cpu_usage > 0.8",
      action: "set_sampling_rate(0.05)",
      priority: 1
    },
    // 错误率高时增加日志级别
    AutoAdjustmentRule {
      name: "high_error_rate_logging",
      condition: "error_rate > 0.1",
      action: "set_log_level(DEBUG)",
      priority: 2
    },
    // 内存使用高时增加批处理大小
    AutoAdjustmentRule {
      name: "high_memory_batch_increase",
      condition: "memory_usage > 0.85",
      action: "set_batch_size(500)",
      priority: 3
    }
  ]
  
  DynamicConfig::configure_auto_adjustment(telemetry_provider, auto_adjustment_rules)
  
  // 创建初始配置
  let initial_config = {
    "sampling.rate": 0.2,
    "log.level": "INFO",
    "batch.size": 100
  }
  
  DynamicConfig::apply_config(telemetry_provider, initial_config)
  
  // 创建tracer和logger
  let tracer = TelemetryProvider::get_tracer(telemetry_provider, "auto.adjustment.test")
  let logger = TelemetryProvider::get_logger(telemetry_provider, "auto.adjustment.test")
  
  // 模拟高CPU使用率
  DynamicConfig::simulate_system_condition(telemetry_provider, "cpu_usage", 0.85)
  
  // 触发自动调整
  DynamicConfig::evaluate_adjustment_rules(telemetry_provider)
  
  // 验证采样率已降低
  let config_after_cpu = DynamicConfig::get_current_config(telemetry_provider)
  let sampling_rate_after_cpu = DynamicConfig::get_config_value(config_after_cpu, "sampling.rate")
  match sampling_rate_after_cpu {
    Some(value) => {
      let rate = value.parse_double()
      match rate {
        Some(r) => assert_eq(r, 0.05) // 应该降低到0.05
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 模拟高错误率
  DynamicConfig::simulate_system_condition(telemetry_provider, "error_rate", 0.15)
  
  // 触发自动调整
  DynamicConfig::evaluate_adjustment_rules(telemetry_provider)
  
  // 验证日志级别已调整
  let config_after_error = DynamicConfig::get_current_config(telemetry_provider)
  let log_level_after_error = DynamicConfig::get_config_value(config_after_error, "log.level")
  match log_level_after_error {
    Some(level) => assert_eq(level, "DEBUG") // 应该调整为DEBUG
    None => assert_true(false)
  }
  
  // 模拟高内存使用
  DynamicConfig::simulate_system_condition(telemetry_provider, "memory_usage", 0.9)
  
  // 触发自动调整
  DynamicConfig::evaluate_adjustment_rules(telemetry_provider)
  
  // 验证批处理大小已增加
  let config_after_memory = DynamicConfig::get_current_config(telemetry_provider)
  let batch_size_after_memory = DynamicConfig::get_config_value(config_after_memory, "batch.size")
  match batch_size_after_memory {
    Some(value) => {
      let size = value.parse_int()
      match size {
        Some(s) => assert_eq(s, 500) // 应该增加到500
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试自动调整后的行为
  let span = Tracer::start_span(tracer, "auto.adjusted.operation")
  Span::set_attribute(span, "test.type", "auto_adjustment")
  Span::end(span)
  
  Logger::emit_log(logger, "DEBUG", "Auto-adjusted log message", [])
  
  // 验证自动调整生效
  let adjustment_effective = DynamicConfig::verify_adjustment_effectiveness(telemetry_provider)
  assert_true(adjustment_effective)
  
  assert_true(true)
}